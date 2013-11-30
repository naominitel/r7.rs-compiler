module Serialize
(
    writeProgram
) where

-- This module containts functions related to serialization (or assembling)
-- It allows to transform an internal-representation of an ASM program into
-- its bytecode equivalent, with some modifications.
-- The layout of a bytecode binary file is:
--      * Header: contains informations about the file. Detailed below.
--      * Symbol table: contains all symbols used in the program to avoid
--        duplicates in the binary file
--      * Program: compiled instructions, with the following modifications:
--        - labels are removed ;
--        - labels are replaced with their adress inside file
--        - symbols are replaced with their index in the symbol table
--
-- The resulting file is represented by a ByteString

import Data.Binary.Put
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Char
import Data.Set
import Data.Word
import System.IO

import Bytecode
import Library
import Module

-- computes the length (bytes) of the serialized version of a single instruction

serializedLength :: Instr -> Word32
serializedLength (Label _)            = 0
serializedLength (Return)             = 1
serializedLength (Pop)                = 1
serializedLength (Call _)             = 2
serializedLength (TCall _)            = 2
serializedLength (Push (TUnit))       = 2
serializedLength (Push (TBool _))     = 3
serializedLength (Push (TFunc _ _ _)) = 8
serializedLength (Push _)             = 10
serializedLength (Alloc _)            = 9
serializedLength (Fetch _)            = 9
serializedLength (Store _)            = 9
serializedLength _                    = 5

-- associates a label number to its adress inside the program

type Labels = [(Word32, Word32)]

-- find all the label in the program

getLabels :: [Instr] -> Word32 -> Labels
getLabels [] _ = []
getLabels ((Label i):rest) offset = (i, offset) : getLabels rest offset
getLabels (instr:rest) off = getLabels rest $ off + serializedLength instr

-- return the offset of a label

labelIndex :: Word32 -> Labels -> Word32
labelIndex i [] = i -- FIXME: should be an error
labelIndex i ((l, off):rest) 
    | i == l    = off 
    | otherwise = labelIndex i rest 

-- list all symbols used in the program to avoid duplicates in the binary file

type Symbols = Set String

-- find all the symbols used in the program

getSymbols :: [Instr] -> Symbols
getSymbols [] = Data.Set.empty
getSymbols ((Push (TSym s)):rest) = insert s $ getSymbols rest
getSymbols (_:rest) = getSymbols rest

-- find the index of a symbol in the symbol table

symbolIndex :: String -> Symbols -> Word64
symbolIndex s set = 
    let aux l i = case l of
            [] -> 0 -- FIXME: should be an error
            (sym:r) -> (if s == sym then i else aux r $ i + 1)
    in aux (elems set) 0

-- serialize a value (literal constant)

serializeValue :: Type -> Labels -> Symbols -> Put

serializeValue (TBool b) _ _ = do
    putWord8    _BOOL
    putWord8    (if b then 1 else 0)

serializeValue (TInt i) _ _ = do
    putWord8    _INT
    putWord64be i

serializeValue (TFunc l a v) lbls _ = do
    putWord8    _FUN
    putWord32be $ labelIndex l lbls
    putWord8    a
    putWord8    (if v then 1 else 0)

serializeValue (TPrim p) _ _ = do
    putWord8    _PRIM
    putWord64be p

serializeValue (TSym s) _ syms = do
    putWord8    _SYM
    putWord64be $ symbolIndex s syms

serializeValue (TUnit) _ _ = do
    putWord8    _UNIT

-- serialize an ASM instruction into bytecode

serializeInstr :: Instr -> Labels -> Symbols -> Put

serializeInstr (Call i) _ _ = do
    putWord8    _CALL
    putWord8    i

serializeInstr (TCall i) _ _ = do
    putWord8    _TCALL
    putWord8    i

serializeInstr (Branch l) lbls _ = do
    putWord8    _BRANCH
    putWord32be $ labelIndex l lbls

serializeInstr (Fetch i) _ _ = do
    putWord8    _FETCH
    putWord64be i

serializeInstr (Jump i) lbl _ = do
    putWord8    _JUMP
    putWord32be $ labelIndex i lbl

serializeInstr (Label _) _ _ = return ()
 
serializeInstr (Pop) _ _ = do
    putWord8    _POP

serializeInstr (Push i) lbls syms = do
    putWord8    _PUSH
    serializeValue i lbls syms

serializeInstr (Alloc i) _ _ = do
    putWord8    _ALLOC
    putWord64be i

serializeInstr (Store i) _ _ = do
    putWord8    _STORE
    putWord64be i

serializeInstr (Return) _ _ = do
    putWord8    _RETURN

-- serialize an assembly program into a bytecode representation

serialize :: [Instr] -> Labels -> Symbols -> [BS.ByteString]
serialize [] _ _ = []
serialize (instr:rest) lbls symbols = 
    let bs = runPut $ serializeInstr instr lbls symbols
    in (BS.concat $ BL.toChunks $ bs) : serialize rest lbls symbols

-- creates the header to be written at the beginning of the file
-- the header follow the following structure: 
--      * Magic: 0x2A 0x2A 0x2A
--      * Version number of the file format
--      * 28 bits reserved for future format extension
--      * offset of symbol section, 64 bits big-endian
--      * offset of imports section, 64 bits big-endian
--      * offset of exports section, 64 bits big-endian
--      * offset of the program section, 64 bits big-endian

createHeader :: Word64 -> Word64 -> Word64 -> Word64 -> Put
createHeader sym_off imp_off exp_off prog_off = do
    putWord8    0x2A
    putWord8    0x2A
    putWord8    0x2A
    putWord8    0x01
    putWord32be 0xFFFFFFFF
    putWord64be 0xFFFFFFFFFFFFFFFF
    putWord64be 0xFFFFFFFFFFFFFFFF
    putWord64be 0xFFFFFFFFFFFFFFFF
    putWord64be sym_off
    putWord64be imp_off
    putWord64be exp_off
    putWord64be prog_off

-- creates the symbol table to be written at the beginning of the file 
-- the symbol table follows the following form for each symbol: 
--      * Size of the symbol string, 32 bits little-endian
--      * Symbol string, 8-bit ASCII characters

-- converts an Haskell (UTF-8) string into an ASCII string

toAscii :: String -> [Word8]
toAscii []    = []
toAscii (c:s) = (fromIntegral (ord c) :: Word8) : toAscii s

-- creates the imports offset

putLibName :: LibName -> Put
putLibName name = do
    let len = fromIntegral (Prelude.length name) :: Word64
    putWord64be len
    let aux = \part ->
            case part of
                [] -> return ()
                str : t -> do
                    let astr = BS.pack $ toAscii str
                    let slen = fromIntegral (BS.length astr) :: Word64
                    putWord64be slen
                    putByteString astr
                    aux t
    aux name

createImportSection :: [LibName] -> Put
createImportSection imps = do
    let len = fromIntegral (Prelude.length imps) :: Word64
    putWord64be len
    let aux = \lst ->
            case lst of
                [] -> return ()
                h : t -> do
                    putLibName h
                    aux t
    aux imps

createExportSection :: Word64 -> Put
createExportSection _ = putWord64be 0

-- creates the symbol table

createSymbolTable :: Symbols -> Put
createSymbolTable set =
    let aux l = case l of
            [] -> return ()
            (s:r) -> do
                let astr = BS.pack $ toAscii s
                putWord64be (fromIntegral (BS.length astr) :: Word64)
                putByteString astr
                aux r
    in do
        let its = (elems set)
        putWord64be (fromIntegral (Prelude.length its) :: Word64)
        aux its

-- write an assembly program to file:
--      * Find all the symbols
--      * Create the symbol table and read its length
--      * Finds all the symbols and their offsets
--      * Serialize the program and reads its size
--      * Creates the header
--      * Writes to file

-- align an offset on 0x10-boundaries

alignOffset :: Word64 -> Word64
alignOffset off = off - (off `mod` 0x10) + 0x10

writeProgram :: CompiledModule -> Handle -> IO ()
writeProgram (Mod exps imps prog) h = do
    let symbols   = getSymbols prog

    let sym_off   = 0x40 -- header_size
    let sym_table = BS.concat $ BL.toChunks $ runPut $ createSymbolTable symbols
    let symt_len  = (fromIntegral (BS.length sym_table) :: Word64)

    let imp_off   = alignOffset $ sym_off + symt_len
    let imports   = BS.concat $ BL.toChunks $ runPut $ createImportSection imps
    let imp_len   = (fromIntegral (BS.length imports) :: Word64)

    let exp_off   = alignOffset $ imp_off + imp_len
    let exports   = BS.concat $ BL.toChunks $ runPut $ createExportSection exps
    let exp_len   = (fromIntegral (BS.length exports) :: Word64)

    let text_off  = alignOffset $ exp_off + exp_len
    let labels    = getLabels prog 0
    let program   = BS.concat $ serialize prog labels symbols
    let prog_size = (fromIntegral (BS.length program) :: Word64)

    let hdr       = createHeader sym_off imp_off exp_off text_off
    let header    = BS.concat $ BL.toChunks $ runPut hdr

    BS.hPutStr h header

    hSeek h AbsoluteSeek $ fromIntegral sym_off
    BS.hPutStr h sym_table

    hSeek h AbsoluteSeek $ fromIntegral imp_off
    BS.hPutStr h imports

    hSeek h AbsoluteSeek $ fromIntegral exp_off
    BS.hPutStr h exports

    hSeek h AbsoluteSeek $ fromIntegral text_off
    BS.hPutStr h $ BS.concat $ BL.toChunks $ runPut $ putWord64be prog_size
    BS.hPutStr h program
