module PIC.Assemble
  (
  -- * Types
    ProgramRel  (..)
  , ProgramAbs  (..)
  , Instruction (..)
  , D           (..)
  , F
  , K
  , B
  -- * Linking Utilities
  , relative
  , absolute
  , link
  ) where


-- | A program with relative GOTO addresses.
data ProgramRel = ProgramRel [Instruction]

-- | A program with absolute GOTO addresses.
data ProgramAbs = ProgramAbs [Instruction]

instance Show ProgramAbs where
  show (ProgramAbs p) = unlines (map show p)

instance Show ProgramRel where
  show (ProgramRel p) = unlines (map show p)

-- | Destination; either working register (W) or the register file (F).
data D = W | F deriving (Show, Eq)

-- | Register address.
type F = Int

-- | Constant data.
type K = Int

-- | Bit selection.
type B = Int

-- | Supported PIC instructions.
data Instruction
  = ADDLW K
  | ADDWF F D
  | ANDLW K
  | ANDWF F D
  | ASSERT String F
--  | BCF F B
  | BSF F B
  | BTFSC F B
  | BTFSS F B
--  | CALL 
  | CLRF F
--  | CLRW
--  | CLRWDT 
  | COMF F D
--  | DECF F D
--  | DECFSZ F D
  | GOTO K
--  | INCF F D
  | INCFSZ F D
--  | IORLW K
  | IORWF F D
  | MOVLW K
  | MOVF F D
  | MOVWF F
  | NOP
--  | OPTION
--  | RETFIE
--  | RETLW
--  | RETURN
--  | RLF F D
--  | RRF F D
--  | SLEEP
--  | SUBLW K
  | SUBWF F D
--  | SWAPF F D
--  | TRIS
--  | XORLW K
  | XORWF F D
  deriving (Show, Eq)


-- Converts a 'ProgramAbs' to a 'ProgramRel'.
relative :: ProgramAbs -> ProgramRel
relative (ProgramAbs a) = ProgramRel $ swap (\ a g -> g - a) 0 a

-- Converts a 'ProgramRel' to a 'ProgramAbs'.
absolute :: ProgramRel -> ProgramAbs
absolute (ProgramRel a) = ProgramAbs $ swap (\ a g -> g + a) 0 a

swap :: (Int -> Int -> Int) -> Int -> [Instruction] -> [Instruction]
swap _ _ [] = []
swap f addr (GOTO a : instrs) = GOTO (f addr a) : swap f (addr + 1) instrs
swap f addr (instr  : instrs) = instr           : swap f (addr + 1) instrs

-- | Links two 'ProgramRel' together.
link :: ProgramRel -> ProgramRel -> ProgramRel
link (ProgramRel a) (ProgramRel b) = ProgramRel (a ++ b)

