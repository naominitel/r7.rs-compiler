module Bytecode
(
    Instr(Call, TCall, Fetch, Jump, Label, Pop, Push, Return, Branch, Alloc, Store),
    Type(TBool, TInt, TFunc, TPrim, TSym, TUnit),
    _BOOL,  
    _INT,   
    _FUN,
    _PRIM, 
    _SYM,
    _UNIT,
    _CALL,  
    _FETCH, 
    _JUMP,
    _POP,  
    _PUSH,  
    _RETURN,
    _BRANCH,
    _ALLOC,
    _STORE,
    _TCALL
) where

import Data.ByteString
import Data.Word

-- This module declares data types for handling assembly instructions and their
-- bytecode representation

-- Type: represent the values in the assembly language.

data Type 
    = TBool Bool
    | TInt Word64
    | TFunc Word32 Word8 Bool
    | TPrim Word64
    | TSym String
    | TUnit

-- Instr: the instructions of the assembly language, with their operands

data Instr 
    = Alloc Word64
    | Call Word8
    | TCall Word8
    | Fetch Word64
    | Jump Word32
    | Label Word32
    | Pop
    | Push Type
    | Branch Word32
    | Return
    | Store Word64

-- bytecode repsentation of types and instructions (opcodes)

_UNIT   = 0x00 :: Word8
_BOOL   = 0x01 :: Word8
_INT    = 0x02 :: Word8
_SYM    = 0x05 :: Word8
_FUN    = 0x08 :: Word8
_PRIM   = 0x09 :: Word8
_PUSH   = 0x01 :: Word8
_POP    = 0x03 :: Word8
_JUMP   = 0x04 :: Word8
_CALL   = 0x06 :: Word8
_RETURN = 0x07 :: Word8
_FETCH  = 0x08 :: Word8
_BRANCH = 0x09 :: Word8
_STORE  = 0x0A :: Word8
_ALLOC  = 0x0C :: Word8
_TCALL  = 0x0D :: Word8

-- dump asm instructions

instance Show Type where
    show (TBool b)     = "BOOL "   ++ show b
    show (TInt i)      = "INT "    ++ show i
    show (TFunc i a v) = "FUNC "   ++ show (i, a, v)
    show (TPrim i)     = "PRIM "   ++ show i
    show (TSym i)      = "SYMBOL " ++ i
    show (TUnit)       = "UNIT"

instance Show Instr where
    show (Branch i) = "BRANCH " ++ show i
    show (Call i)   = "CALL "   ++ show i 
    show (TCall i)  = "TCALL "  ++ show i
    show (Fetch i)  = "FETCH "  ++ show i
    show (Jump i)   = "JUMP "   ++ show i
    show (Label i)  = "LABEL "  ++ show i
    show Pop        = "POP " 
    show (Alloc i)  = "ALLOC "  ++ show i
    show (Store i)  = "STORE "  ++ show i
    show (Push i)   = "PUSH "   ++ show i
    show Return     = "RETURN"
