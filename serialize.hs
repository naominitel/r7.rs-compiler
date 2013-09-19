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

-- computes the length (bytes) of the serialized version of a single instruction

serializedLength :: Instr -> Word32
serializedLength (Label _) = 0
serializedLength (Return)  = 1
serializedLength (Pop)     = 1
serializedLength (Push _)  = 6
serializedLength _         = 5

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

symbolIndex :: String -> Symbols -> Word32
symbolIndex s set = 
    let aux l i = case l of
            [] -> 0 -- FIXME: should be an error
            (sym:r) -> (if s == sym then i else aux r $ i + 1)
    in aux (elems set) 0

-- serialize a value (literal constant)

serializeValue :: Type -> Labels -> Symbols -> Put

serializeValue (TBool b) _ _ = do
    putWord8    _BOOL
    putWord32le (if b then 1 else 0)

serializeValue (TInt i) _ _ = do
    putWord8    _INT
    putWord32le i

serializeValue (TFunc l) lbls _ = do
    putWord8    _FUN
    putWord32le $ labelIndex l lbls

serializeValue (TPrim p) _ _ = do
    putWord8    _PRIM
    putWord32le p

serializeValue (TSym s) _ syms = do
    putWord8    _SYM
    putWord32le $ symbolIndex s syms

serializeValue (TUnit) _ _ = do
    putWord8    _UNIT

-- serialize an ASM instruction into bytecode

serializeInstr :: Instr -> Labels -> Symbols -> Put

serializeInstr (Call i) _ _ = do
    putWord8    _CALL
    putWord32le i

serializeInstr (Branch l) lbls _ = do
    putWord8    _BRANCH
    putWord32le $ labelIndex l lbls 

serializeInstr (Fetch i) _ _ = do
    putWord8    _FETCH
    putWord32le i

serializeInstr (Jump i) lbl _ = do
    putWord8    _JUMP
    putWord32le $ labelIndex i lbl

serializeInstr (Label _) _ _ = return ()
 
serializeInstr (Pop) _ _ = do
    putWord8    _POP

serializeInstr (Push i) lbls syms = do
    putWord8    _PUSH
    serializeValue i lbls syms

serializeInstr (Alloc i) _ _ = do
    putWord8    _ALLOC
    putWord32le i

serializeInstr (Store i) _ _ = do
    putWord8    _STORE
    putWord32le i

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
--      * size of the symbol section, 32 bits little-endian
--      * size of the program section, 32 bits little-endian

createHeader :: Word32 -> Word32 -> Put
createHeader sym_size prog_size = do
    putWord8    0x2A
    putWord8    0x2A
    putWord8    0x2A
    putWord32le sym_size
    putWord32le prog_size -- $ programLength i

-- creates the symbol table to be written at the beginning of the file 
-- the symbol table follows the following form for each symbol: 
--      * Size of the symbol string, 32 bits little-endian
--      * Symbol string, 8-bit ASCII characters

-- converts an Haskell (UTF-8) string into an ASCII string

toAscii :: String -> [Word8]
toAscii []    = []
toAscii (c:s) = (fromIntegral (ord c) :: Word8) : toAscii s

-- creates the symbol table

createSymbolTable :: Symbols -> Put
createSymbolTable set =
    let aux l = case l of
            [] -> return ()
            (s:r) -> do
                let astr = BS.pack $ toAscii s
                putWord32le (fromIntegral (BS.length astr) :: Word32)
                putByteString astr
                aux r
    in aux (elems set)

-- write an assembly program to file:
--      * Find all the symbols
--      * Create the symbol table and read its length
--      * Finds all the symbols and their offsets
--      * Serialize the program and reads its size
--      * Creates the header
--      * Writes to file

writeProgram :: [Instr] -> Handle -> IO ()
writeProgram prog h = do
    let symbols   = getSymbols prog
    let sym_table = BS.concat $ BL.toChunks $ runPut $ createSymbolTable symbols
    let symt_size = (fromIntegral (BS.length sym_table) :: Word32)
    let labels    = getLabels prog symt_size
    let program   = BS.concat $ serialize prog labels symbols
    let prog_size = (fromIntegral (BS.length program) :: Word32)
    let header    = BL.toChunks $ runPut $ createHeader symt_size prog_size
    BS.hPutStr h $ BS.concat $ (header ++ [sym_table] ++ [program])
