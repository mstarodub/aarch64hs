module Elf where

import Control.Exception (assert)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32, Int64)
import Data.Word (Word16, Word32, Word64, Word8)
import qualified System.IO as IO
import qualified Test.Hspec as H

import qualified Aarch64

data ElfHeader
  = Header

data Arch = X86_64 | AArch64

type Elf64_Half = Word16

type Elf64_Word = Word32

type Elf64_SWord = Int32

type Elf64_XWord = Word64

type Elf64_SXWord = Int64

type Elf64_Addr = Word64

type Elf64_Off = Word64

type Elf64_Section = Word16

type Elf64_Versym = Word16

elfArch :: Arch -> Elf64_Half
elfArch X86_64 = 62
elfArch AArch64 = 183

data ElfType
  = ETNone
  | ETRel
  | ETExec
  | ETDyn
  | ETCore
  | ETProc Word8

elfType :: ElfType -> Elf64_Half
elfType ETNone = 0
elfType ETRel = 1
elfType ETExec = 2
elfType ETDyn = 3
elfType ETCore = 4
elfType (ETProc x) = 0xff00 + fromIntegral x

data PhdrType
  = PTNull
  | PTLoad
  | PTDynamic
  | PTInterp
  | PTNote
  | PTShlib
  | PTPhdr
  | PTProc Elf64_Word

phdrType :: PhdrType -> Elf64_Word
phdrType PTNull = 0
phdrType PTLoad = 1
phdrType PTDynamic = 2
phdrType PTInterp = 3
phdrType PTNote = 4
phdrType PTShlib = 5
phdrType PTPhdr = 6
phdrType (PTProc x) = assert (x <= 0x1000_000) $ 0x7000_0000 + x

phdrFlags :: (Bool, Bool, Bool) -> Elf64_Word
phdrFlags (r, w, x) = r' + w' + x'
  where
    r' = if r then 4 else 0
    w' = if w then 2 else 0
    x' = if x then 1 else 0

data Elf64_PHdr
  = Phdr
      PhdrType -- type
      (Bool, Bool, Bool) -- flags (Read, Write, Execute)
      Elf64_Off -- offset
      Elf64_Addr -- vaddr
      Elf64_Addr -- paddr
      Elf64_XWord -- filesz
      Elf64_XWord -- memsz
      Elf64_XWord -- align

elfPhdr :: Elf64_PHdr -> B.Builder
elfPhdr (Phdr ty flags offset vaddr paddr filesz memsz align) =
  assert (filesz `mod` align == memsz) $
    B.word32LE (phdrType ty)
      <> B.word32LE (phdrFlags flags)
      <> B.word64LE offset
      <> B.word64LE vaddr
      <> B.word64LE paddr
      <> B.word64LE filesz
      <> B.word64LE memsz
      <> B.word64LE align

data Elf64_SHdr
  = Shdr
      Elf64_Word -- name
      Elf64_Word -- type
      Elf64_XWord -- flags
      Elf64_Addr -- addr
      Elf64_Off -- offset
      Elf64_XWord -- size
      Elf64_Word -- link
      Elf64_Word -- info
      Elf64_XWord -- addralign
      Elf64_XWord -- entsize

elfShdr :: Elf64_SHdr -> B.Builder
elfShdr = undefined

data Class = Class32 | Class64

data Endian = Little | Big

elfData :: Endian -> Word8
elfData Little = 1
elfData Big = 2

elfClass :: Class -> Word8
elfClass Class32 = 1
elfClass Class64 = 2

elfIdent :: B.Builder
elfIdent =
  -- magic
  B.word8 0x7f
    <> B.string7 "ELF"
    -- class
    <> B.word8 (elfClass Class64)
    -- endian
    <> B.word8 (elfData Little)
    -- version
    <> B.word8 1
    -- padding
    <> mconcat (replicate 9 $ B.word8 0)

elfHeader :: Arch -> Elf64_Addr -> [Elf64_PHdr] -> [Elf64_SHdr] -> B.Builder
elfHeader arch entry phdrs shdrs =
  -- We assume the binary layout is the following:
  -- 0            phdrs_offset            shdr_offset
  -- [ elfheader  |         phdrs        |         shdrs        | ...
  -- i.e the elf header is put at the start of the binary, with the phdrs and
  -- shdrs respectively immediately following
  let
    ehdr_size :: Word16 = 64
    phdrs_offset = ehdr_size
    phdr_size :: Word16 = 56
    phdrs_size = phdr_size * fromIntegral (length phdrs)
    shdrs_offset = phdrs_offset + phdrs_size
    shdr_size = 64
  in
    elfIdent
      <> B.word16LE (elfType ETExec)
      <> B.word16LE (elfArch arch)
      <> B.word32LE 1 -- version (EV_CURRENT)
      <> B.word64LE entry
      <> B.word64LE (fromIntegral phdrs_offset)
      <> B.word64LE (fromIntegral shdrs_offset)
      <> B.word32LE 0 -- flags (there are none)
      <> B.word16LE ehdr_size -- sizeof elfHeader
      <> B.word16LE phdr_size -- sizeof elf_phdr
      <> B.word16LE (fromIntegral . length $ phdrs) -- # of phdrs
      <> B.word16LE shdr_size -- sizeof elf_shdr
      <> B.word16LE (fromIntegral . length $ shdrs) -- # of shdrs
      <> B.word16LE 0 -- shstrndx

test :: IO ()
test = H.hspec do
  let builderLength = LBS.length . B.toLazyByteString
  H.describe "elf" do
    H.it "ElfIdent size" $ do
      builderLength elfIdent `H.shouldBe` 16
    H.it "ElfHeader size" $ do
      builderLength (elfHeader AArch64 0 [] []) `H.shouldBe` 64

elf :: Arch -> Elf64_Addr -> [Elf64_PHdr] -> [Elf64_SHdr] -> B.Builder
elf arch entry phdrs shdrs =
  elfHeader arch entry phdrs shdrs
    <> mconcat (fmap elfPhdr phdrs)
    <> mconcat (fmap elfShdr shdrs)

writeElf :: IO ()
writeElf =
  let
    code =
      [ Aarch64.BCond Aarch64.AL 0
      , Aarch64.B 0
      , Aarch64.LDRi  (Aarch64.Xn 15) Aarch64.SP 0
      , Aarch64.STRi  (Aarch64.Xn 1) (Aarch64.Xn 5) 2
      , Aarch64.ADDSi (Aarch64.Xn 1) (Aarch64.Xn 5) 3
      , Aarch64.MOV   (Aarch64.Xn 3) (Aarch64.Xn 4)
      ]
    machine_code = foldMap Aarch64.assemble (code :: [Aarch64.Armv8Inst])
    machine_code_size = LBS.length . B.toLazyByteString $ machine_code
    phdr =
      Phdr
        PTLoad
        (True, False, True)
        4096 -- offset
        (2 * 1024 * 1024) -- vaddr
        (2 * 1024 * 1024) -- paddr
        (fromIntegral machine_code_size) -- filesz
        (fromIntegral machine_code_size) -- memsz
        4096 -- align
    e = elf AArch64 (2 * 1024 * 1024) [phdr] []
  in
    IO.withFile
      "arm.bin"
      IO.WriteMode
      \h -> do
        B.hPutBuilder h e
        IO.hSeek h IO.AbsoluteSeek 4096
        B.hPutBuilder h machine_code
