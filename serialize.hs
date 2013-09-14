module Serialize
(
    writeProgram
) where

import Data.Binary.Put
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import Data.Word
import System.IO

import AST
import Bytecode
import Compiler
import Lexer
import Parser

-- computes the length of the serialized version of an instruction

serializedLength :: Instr -> Word32
serializedLength (Label _) = 0
serializedLength (Return)  = 1
serializedLength (Pop)     = 1
serializedLength (Push _)  = 6
serializedLength _         = 5

-- computes the length of the whole program

programLength :: [Instr] -> Word32
programLength [] = 0
programLength (instr:rest) = (serializedLength instr) + (programLength rest)

-- associates a label number to its adress inside the program

type Labels = [(Word32, Word32)]

labelIndexes :: [Instr] -> Word32 -> Labels
labelIndexes [] _ = []
labelIndexes ((Label i):rest) offset = (i, offset) : labelIndexes rest offset
labelIndexes (instr:rest) off = labelIndexes rest $ off + serializedLength instr

labelIndex :: Word32 -> Labels -> Word32
labelIndex i [] = i -- FIXME: should be an error
labelIndex i ((l, off):rest) 
    | i == l    = off 
    | otherwise = labelIndex i rest 

-- serialize a value

serializeValue :: Type -> Labels -> Put

serializeValue (TBool b) _ = do
    putWord8    _BOOL
    putWord32le (if b then 1 else 0)

serializeValue (TInt i) _ = do
    putWord8    _INT
    putWord32le i

serializeValue (TFunc l) lbls = do
    putWord8    _FUN
    putWord32le $ labelIndex l lbls

serializeValue (TPrim p) _ = do
    putWord8    _PRIM
    putWord32le p

-- serialize an ASM instruction into bytecode

serializeInstr :: Instr -> Labels -> Put

serializeInstr (Call i) _ = do
    putWord8    _CALL
    putWord32le i

serializeInstr (Branch l) lbls = do
    putWord8    _BRANCH
    putWord32le $ labelIndex l lbls 

serializeInstr (Fetch i) _ = do
    putWord8    _FETCH
    putWord32le i

serializeInstr (Jump i) lbl = do
    putWord8    _JUMP
    putWord32le $ labelIndex i lbl

serializeInstr (Label _) _ = return ()
 
serializeInstr (Pop) _ = do
    putWord8    _POP

serializeInstr (Push i) lbls = do
    putWord8    _PUSH
    serializeValue i lbls

serializeInstr (Return) _ = do
    putWord8    _RETURN

-- serialize an assembly program into a bytecode representation

serialize :: [Instr] -> Labels -> [BS.ByteString]
serialize [] _ = []
serialize (instr:rest) lbls = 
    let bs = runPut $ serializeInstr instr lbls
    in (BS.concat $ BL.toChunks $ bs) : serialize rest lbls

-- return the magic number to be writter at the beginning of the program

serializeMagic :: [Instr] -> Put
serializeMagic i = do
    putWord8    0x2A
    putWord8    0x2A
    putWord8    0x2A
    putWord32le $ programLength i

-- write an assembly program to file

writeProgram :: [Instr] -> Handle -> IO ()
writeProgram i h = do
    let lbls = labelIndexes i 0
    let magic = BS.concat $ BL.toChunks $ runPut $ serializeMagic i
    BS.hPutStr h $ BS.concat $ (magic : serialize i lbls)
