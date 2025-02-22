module Aarch64 where

import Control.Exception (assert)
import qualified Data.Bits as Bits
import qualified Data.ByteString.Builder as B
import qualified Data.Finite as F
import Data.Int (Int32)

data Armv8Reg = Xn (F.Finite 30) | SP | XZR
  deriving (Eq, Show)

encodeReg :: (Num a) => Armv8Reg -> a
encodeReg (Xn n) = fromIntegral n
encodeReg SP = 31
encodeReg XZR = 31

data Armv8CondCode
  = EQ
  | NE
  | HS
  | LO
  | MI
  | PL
  | VS
  | VC
  | HI
  | LS
  | GE
  | LT
  | GT
  | LE
  | AL
  | NV
  deriving (Eq, Show)

assembleCondCode :: (Num a) => Armv8CondCode -> a
assembleCondCode Aarch64.EQ = 0b0000
assembleCondCode NE = 0b0001
assembleCondCode HS = 0b0010
assembleCondCode LO = 0b0011
assembleCondCode MI = 0b0100
assembleCondCode PL = 0b0101
assembleCondCode VS = 0b0110
assembleCondCode VC = 0b0111
assembleCondCode HI = 0b1000
assembleCondCode LS = 0b1001
assembleCondCode GE = 0b1010
assembleCondCode Aarch64.LT = 0b1011
assembleCondCode Aarch64.GT = 0b1100
assembleCondCode LE = 0b1101
assembleCondCode AL = 0b1110
assembleCondCode NV = 0b1111

type Armv8Addr = Integer

type Armv8Label = Integer

data Armv8Inst
  = BCond Armv8CondCode Int32
  | B Int32
  | LDRi Armv8Reg Armv8Reg Armv8Addr -- Load Immediate Unsigned Offset
  | STRi Armv8Reg Armv8Reg Armv8Addr
  | ADDSi Armv8Reg Armv8Reg Integer
  | MOV Armv8Reg Armv8Reg
  deriving (Eq, Show)

assemble :: Armv8Inst -> B.Builder
assemble (BCond cond addr) =
  assert (addr < (2 ^ 19))
    . B.word32LE
    $ Bits.shiftL 0b01010100 24 + Bits.shiftL (fromIntegral addr) 5 + assembleCondCode cond
assemble (B offset) =
  assert (offset < (2 ^ 26))
    . B.word32LE
    $ Bits.shiftL 0b000101 26 + fromIntegral offset
assemble (LDRi reg_target reg_addr imm_offset) =
  assert (imm_offset < (2 ^ 12))
    . assert (reg_addr /= XZR)
    . assert (reg_target /= XZR)
    . B.word32LE
    $ Bits.shiftL 0b11_111_0_01_01 22
      + Bits.shiftL (fromIntegral imm_offset) 10
      + Bits.shiftL (encodeReg reg_addr) 5
      + encodeReg reg_target
assemble (STRi reg_src reg_addr imm_offset) =
  assert (imm_offset < (2 ^ 12))
    . assert (reg_addr /= XZR)
    . assert (reg_src /= XZR)
    . B.word32LE
    $ Bits.shiftL 0b11_111_0_01_00 22
      + Bits.shiftL (fromIntegral imm_offset) 10
      + Bits.shiftL (encodeReg reg_addr) 5
      + encodeReg reg_src
assemble (ADDSi reg_dst reg_src imm_offset) =
  assert (imm_offset < (2 ^ 12))
    . assert (reg_src /= XZR)
    . assert (reg_dst /= XZR)
    . B.word32LE
    $ Bits.shiftL 0b1_0_0_100010_0 22
      + Bits.shiftL (fromIntegral imm_offset) 10
      + Bits.shiftL (encodeReg reg_src) 5
      + encodeReg reg_dst
assemble (MOV reg_dst reg_src) =
  assert (reg_src /= XZR && reg_src /= SP)
    . assert (reg_dst /= XZR && reg_dst /= SP)
    . B.word32LE
    $ Bits.shiftL 0b1_01_01010_00_0 21
      + Bits.shiftL (encodeReg reg_src) 16
      + Bits.shiftL 0b000_000 10
      + Bits.shiftL (encodeReg XZR) 5
      + encodeReg reg_dst
