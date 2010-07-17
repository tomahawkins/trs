-- | TRS to PIC compilation.
module PIC.Compiler
  ( compile
  ) where

import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Map as Map
import System.Exit

import PIC.Assemble
import PIC.Configuration
import TRS.Checker
import TRS.Types
import Utils


-- | Compiles a TRS 'System'.
compile :: System () -> IO ProgramAbs
compile system = do
  sys <- extract system
  case sys of
    Nothing -> putStrLn "ERROR: Design rule checks failed.  Compilation aborted." >> exitWith (ExitFailure 1)
    Just (rules, regs, asserts, exprs) -> do
      let (regMap, exprMap) = sysAllocate dW (dataMemoryDepth basicConfig) regs exprs
      assertInstrs' <- mapM (codeAssert dW exprMap) asserts
      let assertInstrs = concat assertInstrs'
      return $ absolute $ ProgramRel (codeInitRegs dW regMap regs ++ (if null rules then assertInstrs else []) ++ codeRules dW regMap exprMap assertInstrs rules)
  where
  dW = dataWidth basicConfig





-- Register and Expression Allocation

type Allocation = ([Bool], Map.Map Reg Address, Map.Map Expr Address)

sysAllocate :: Int -> Int -> [Reg] -> [Expr] -> (Reg -> Address, Expr -> Address)
sysAllocate dataWidth dataDepth regs exprs = (regAddr, exprAddr)
  where
  (externRegs,internRegs) = List.partition isExternReg regs
  isExternReg (Reg _ _ _ _ (Just _)) = True
  isExternReg (Reg _ _ _ _ Nothing)  = False
  alloc0 = (replicate 4 False ++ replicate (dataDepth - 4) True, Map.empty, Map.empty)
  alloc1 = foldl (flip (regAllocate dataWidth)) alloc0 (externRegs ++ internRegs)
  (_, regMap, exprMap) = foldl (exprAllocate dataWidth) alloc1 exprs
  regAddr :: Reg -> Address
  regAddr reg = regMap Map.! reg
  exprAddr expr = exprMap Map.! expr

regAllocate :: Int -> Reg -> Allocation -> Allocation
regAllocate dataWidth reg@(Reg _ name width _ (Just addr)) (avail,regMap,exprMap) = allocate
  where
    w = wordCount dataWidth width
    allocate :: Allocation
    allocate = if (length avail :: Int) >= fromIntegral addr + w && and (take w (drop addr avail))
                 then (take addr avail ++ replicate w False ++ drop (addr + w) avail, Map.insert reg addr regMap, exprMap)
                 else error $ "Data memory allocation failed.  External register address conflict: " ++ name ++ " " ++ show addr
regAllocate dataWidth reg@(Reg _ _ w _ Nothing) (avail,regMap,exprMap) = (avail', Map.insert reg addr regMap, exprMap)
  where
  (avail',addr) = softAllocate dataWidth w avail

exprAllocate :: Int -> Allocation -> Expr -> Allocation
exprAllocate _         (avail, regMap, exprMap) expr@(Val reg) = (avail, regMap, Map.insert expr (regMap Map.! reg) exprMap)
exprAllocate dataWidth (avail, regMap, exprMap) expr           = (avail', regMap, Map.insert expr addr exprMap)
  where 
  (avail',addr) = softAllocate dataWidth (width expr) avail

softAllocate :: Int -> Int -> [Bool] -> ([Bool],Address)
softAllocate dataWidth width avail = allocate (avail,0)
  where
  w = wordCount dataWidth width
  allocate :: ([Bool],Address) -> ([Bool],Address)
  allocate ([],_) = error "Data memory allocation failed.  Out of space."
  allocate (avail,addr) | length avail >= w && and (take w avail) = (replicate w False ++ drop w avail, addr)
  allocate (m:avail,addr)                                         = (m:avail',addr')
    where
    (avail',addr') = allocate (avail, addr + 1)

wordCount :: Int -> Int -> Int
wordCount _ w | w <= 0 = 0
wordCount dataWidth w  = 1 + wordCount dataWidth (w - dataWidth)




-- Code Generation

-- Code register initialization.
codeInitRegs :: Int -> (Reg -> Address) -> [Reg] -> [Instruction]
codeInitRegs dataWidth regAddr regs = concatMap initReg regs
  where
  initReg :: Reg -> [Instruction]
  initReg (Reg _ _ _ Nothing _) = []
  initReg reg@(Reg _ _ width (Just value) _) = constant dataWidth width value (regAddr reg)


-- Code for writing a constant value to memory.
constant :: Int -> Int -> Integer -> Address -> [Instruction]
constant dataWidth width value addr = instrs words value addr
    where
    words = wordCount dataWidth width
    shift = power 2 (toInteger dataWidth)
    instrs :: Int -> Integer -> Address -> [Instruction]
    instrs n _ _ | n <= 0 = []
    instrs n value addr = MOVLW ((fromIntegral $ value `mod` shift)) : MOVWF addr : instrs (n - 1) (fromIntegral $ value `div` shift) (addr + 1)

-- Code for calculating an expression.
codeExpr :: Int -> (Expr -> Address) -> Expr -> [Instruction]
codeExpr dataWidth exprAddr expr = case expr of
  Add a b -> add0 wid xAddr (exprAddr a) (exprAddr b)
    where
    add0 :: Int -> Int -> Int -> Int -> [Instruction]
    add0 width x a b | width == dataWidth = [MOVF a W, ADDWF b W, MOVWF x]
    add0 width x a b | width <  dataWidth = [MOVF a W, ADDWF b W, MOVWF x] ++ mask x
    add0 width x a b                      = [MOVF a W, ADDWF b W, MOVWF x] ++ addN (width - dataWidth) (x + 1) (a + 1) (b + 1)

    addN :: Int -> Int -> Int -> Int -> [Instruction]
    addN width x a b | width == dataWidth = [MOVF a W, BTFSC status carry, ADDLW 1, ADDWF b W, MOVWF x]
    addN width x a b | width <  dataWidth = [MOVF a W, BTFSC status carry, ADDLW 1, ADDWF b W, MOVWF x] ++ mask x
    addN width x a b                      = [MOVF a W, MOVWF x, MOVF b W, BTFSC status carry, INCFSZ b W, ADDWF x F] ++ addN (width - dataWidth) (x + 1) (a + 1) (b + 1)

  Sub a b -> sub0 wid xAddr (exprAddr a) (exprAddr b)
    where
    sub0 :: Int -> Int -> Int -> Int -> [Instruction]
    sub0 width x a b | width == dataWidth = [MOVF b W, SUBWF a W, MOVWF x]
    sub0 width x a b | width <  dataWidth = [MOVF b W, SUBWF a W, MOVWF x] ++ mask x
    sub0 width x a b                      = [MOVF b W, SUBWF a W, MOVWF x] ++ subN (width - dataWidth) (x + 1) (a + 1) (b + 1)

    subN :: Int -> Int -> Int -> Int -> [Instruction]
    subN width x a b | width == dataWidth = [MOVF b W, BTFSC status carry, ADDLW 1, SUBWF a W, MOVWF x]
    subN width x a b | width <  dataWidth = [MOVF b W, BTFSC status carry, ADDLW 1, SUBWF a W, MOVWF x] ++ mask x
    subN width x a b                      = [MOVF a W, MOVWF x, MOVF b W, BTFSC status carry, INCFSZ b W, SUBWF x F] ++ subN (width - dataWidth) (x + 1) (a + 1) (b + 1)

  Not    a     -> f wid xAddr (exprAddr a)
    where
    f :: Int -> Int -> Int -> [Instruction]
    f width x a | width == dataWidth = [COMF a W, MOVWF x]
    f width x a | width <  dataWidth = [COMF a W, MOVWF x] ++ mask x
    f width x a                      = [COMF a W, MOVWF x] ++ f (width - dataWidth) (x + 1) (a + 1)

  And    a b   -> f words xAddr (exprAddr a) (exprAddr b)
    where
    f :: Int -> Int -> Int -> Int -> [Instruction]
    f n _ _ _ | n <= 0 = []
    f n x a b = [MOVF a W, ANDWF b W, MOVWF x] ++ f (n - 1) (x + 1) (a + 1) (b + 1)

  Xor    a b   -> f words xAddr (exprAddr a) (exprAddr b)
    where
    f :: Int -> Int -> Int -> Int -> [Instruction]
    f n _ _ _ | n <= 0 = []
    f n x a b = [MOVF a W, XORWF b W, MOVWF x] ++ f (n - 1) (x + 1) (a + 1) (b + 1)

  Or     a b   -> f words xAddr (exprAddr a) (exprAddr b)
    where
    f :: Int -> Int -> Int -> Int -> [Instruction]
    f n _ _ _ | n <= 0 = []
    f n x a b = [MOVF a W, IORWF b W, MOVWF x] ++ f (n - 1) (x + 1) (a + 1) (b + 1)

  Select _ _ _ -> error "Bit selection not supported yet." --XXX
  Concat _ _   -> error "Concatenation not supported yet." --XXX

  Mux    c h l -> [BTFSS (exprAddr c) 0, GOTO $ length moveHigh + 1] ++ moveHigh ++ [BTFSC (exprAddr c) 0, GOTO $ length moveLow + 1] ++ moveLow
    where
    moveHigh = move words xAddr (exprAddr h)
    moveLow  = move words xAddr (exprAddr l)

  Eq     a b   -> [CLRF xAddr] ++ f words' (exprAddr a) (exprAddr b)
    where
    words' = wordCount dataWidth (width a)
    f :: Int -> Int -> Int -> [Instruction]
    f n _ _ | n <= 0 = [MOVF xAddr F, MOVLW 0, MOVWF xAddr, BTFSC status zero, BSF xAddr 0]
    f n a b          = [MOVF a W, XORWF b W, IORWF xAddr F] ++ f (n - 1) (a + 1) (b + 1)

  Lt     a b   -> [CLRF xAddr] ++ f (words' - 1) (exprAddr a + words' - 1) (exprAddr b + words' - 1)
    where
    words' = wordCount dataWidth (width a)
    f :: Int -> Int -> Int -> [Instruction]
    f word _ _ | word < 0 = []
    f word a b            = [MOVF b W, SUBWF a W, BTFSC status zero, GOTO 4, BTFSC status carry, BSF xAddr 0, GOTO (length rest + 1)] ++ rest
      where
      rest = f (word - 1) (a - 1) (b - 1)

  Const  w v   -> constant dataWidth w v xAddr

  Nondet _     -> error "Nondeterministic values not supported."

  Val    _     -> []

  where
  words = wordCount dataWidth (width expr)
  wid = width expr
  widthMSW = wid `mod` dataWidth
  xAddr = exprAddr expr
  status = 3
  carry  = 0
  zero   = 2

  mask :: Int -> [Instruction]
  mask a = [MOVF a W, ANDLW (power 2 widthMSW - 1), MOVWF a]


-- Codes the set of rules.
codeRules :: Int -> (Reg -> Address) -> (Expr -> Address) -> [Instruction] -> [Rule] -> [Instruction]
codeRules dataWidth regAddr exprAddr assertInstrs rules = ruleInstrs ++ [GOTO (0 - length ruleInstrs)]
  where
  ruleInstrs = concatMap codeRule rules
  codeRule :: Rule -> [Instruction]
  codeRule (Rule _ condition action) = assertInstrs ++ ruleInstrs
    where
    conditionInstrs = concatMap (codeExpr dataWidth exprAddr) (topological [condition])
    assigns = regsAssigned action
    (_,assignExprs) = unzip assigns
    actionExprInstrs = concatMap (codeExpr dataWidth exprAddr) (topological assignExprs)
    actionAssignInstrs = concatMap codeAssign assigns
    actionInstrs = actionExprInstrs ++ actionAssignInstrs
    ruleInstrs = conditionInstrs ++ [BTFSS (exprAddr condition) 0, GOTO (length actionInstrs + 1)] ++ actionInstrs

  codeAssign :: (Reg,Expr) -> [Instruction]
  codeAssign (reg,expr) = instrs
    where
    words = wordCount dataWidth (width reg)
    instrs = move words (regAddr reg) (exprAddr expr)

move :: Int -> Int -> Int -> [Instruction]
move w _ _ | w <= 0 = []
move w toAddr fromAddr = [MOVF fromAddr W, MOVWF toAddr] ++ move (w - 1) (toAddr + 1) (fromAddr + 1)

-- | Code assertions.
codeAssert :: Int -> (Expr -> Address) -> Assert -> IO [Instruction]
codeAssert dataWidth exprAddr (Assert name prop) = case prop of
  PropertyAlways (PropertyExpr expr) -> return $ testInstrs ++ [ASSERT name (exprAddr expr)]
    where
    testInstrs = concatMap (codeExpr dataWidth exprAddr) (topological [expr])
  PropertyAlways (PropertyImply (PropertyExpr cond) (PropertyExpr test)) -> return combInstrs
    where
    condTestInstrs = concatMap (codeExpr dataWidth exprAddr) (topological [cond,test])
    combInstrs = condTestInstrs ++ [BTFSC (exprAddr cond) 0, ASSERT name (exprAddr test)]

  _ -> do
    putStrLn $ "WARNING: Assertion " ++ name ++ " not supported in PIC compilation."
    return []




