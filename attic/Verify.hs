-- | A suite of tests to check the TRS compiler and PIC assembler.

module Verify ( verify ) where

--import Control.Monad.State hiding (when, join)
import System.Cmd
import System.Exit

import TRS
import qualified C

--type Suite = StateT ([String],Bool) IO ()

-- | Runs the test suite.
verify :: IO ()
verify = do
  --program <- PIC.compile coreAll
  --pass <- PIC.simulate "testsuite" Nothing program
  test8  <- runCTest C.Word8
  test16 <- runCTest C.Word16
  test32 <- runCTest C.Word32
  --test64 <- runCTest C.Word64
  putStrLn $ "Test Results : " ++ (if and [test8,test16,test32] then "PASS" else "FAIL")
  return ()
  where
  cmd = "gcc -Wall testsuite.c && chmod u+x a.out && a.out && rm -f testsuite.c a.out"

  runCTest :: C.Word -> IO Bool
  runCTest word = do
    --putStrLn $ "Running test with " ++ show word ++ " configuration."
    C.compile "testsuite" word coreAll
    result <- system cmd
    return (result == ExitSuccess)




{-
runAll :: String -> Suite -> IO ()
runAll name suite = do
  (_,r) <- execStateT suite ([name],True)
  putStrLn ("Test Results : " ++ (if r then "PASS" else "FAIL"))

-- | Runs a sub testsuite.
run :: String -> Suite -> Suite
run name suite' = do
  (names,r) <- get
  put (name:names,r)
  suite'
  (names,r) <- get
  put (tail names,r)

-- | Compiles and runs a 'System' and returns a pass or fail.
sim :: String -> Maybe Int -> System () -> Suite
sim name cycles system = do
  (names,r) <- get
  let name' = join (reverse (name:names)) "."
  lift (putStrLn $ "Running Test     : " ++ name')
  program <- lift (PIC.compile system)
  r' <- lift (PIC.simulate name' cycles program)
  lift (putStrLn "")
  put (names, r && r')
  -}

assertExpr :: String -> Expr -> System ()
assertExpr name e = assert name $ always $ fromExpr e

assertImply :: String -> Expr -> Expr -> System ()
assertImply name c e = assert name $ always (fromExpr c ?-> fromExpr e)

coverExpr :: String -> Expr -> System ()
coverExpr name e = assertImply name e true

--list :: System () -> Suite
--list system = do
--  program <- lift (PIC.compile system)
--  lift (print program)

coreAll :: System ()
coreAll = do
  scope "logic" coreLogic
  scope "arith" coreArith
  scope "comp"  coreComp
  scope "ctrl"  coreCtrl
  scope "rules" coreRules

coreLogic :: System ()
coreLogic = do
  scope "and" testAnd
  scope "or"  testOr
  scope "xor" testXor
  scope "inv" testInv
  where
  testAnd :: System ()
  testAnd = do
    assertExpr "and1" $ (constant 1 0 @& constant 1 0) @== constant 1 0
    assertExpr "and2" $ (constant 1 0 @& constant 1 1) @== constant 1 0
    assertExpr "and3" $ (constant 1 1 @& constant 1 0) @== constant 1 0
    assertExpr "and4" $ (constant 1 1 @& constant 1 1) @== constant 1 1
    assertExpr "and5" $ (constant 4 6 @& constant 4 3) @== constant 4 2
    assertExpr "and6" $ (constant 12 0xCCC @& constant 12 0x555) @== constant 12 0x444
  testOr :: System ()
  testOr = do
    assertExpr "or1" $ (constant 1 0 @| constant 1 0) @== constant 1 0
    assertExpr "or2" $ (constant 1 0 @| constant 1 1) @== constant 1 1
    assertExpr "or3" $ (constant 1 1 @| constant 1 0) @== constant 1 1
    assertExpr "or4" $ (constant 1 1 @| constant 1 1) @== constant 1 1
    assertExpr "or5" $ (constant 4 6 @| constant 4 3) @== constant 4 7
    assertExpr "or6" $ (constant 12 0xCCC @| constant 12 0x555) @== constant 12 0xDDD
  testXor :: System ()
  testXor = do
    assertExpr "xor1" $ (constant 1 0 @^ constant 1 0) @== constant 1 0
    assertExpr "xor2" $ (constant 1 0 @^ constant 1 1) @== constant 1 1
    assertExpr "xor3" $ (constant 1 1 @^ constant 1 0) @== constant 1 1
    assertExpr "xor4" $ (constant 1 1 @^ constant 1 1) @== constant 1 0
    assertExpr "xor5" $ (constant 4 6 @^ constant 4 3) @== constant 4 5
    assertExpr "xor6" $ (constant 12 0xCCC @^ constant 12 0x555) @== constant 12 0x999
  testInv :: System ()
  testInv = do
    assertExpr "inv1" $ inv (constant 1 0) @== constant 1 1
    assertExpr "inv2" $ inv (constant 1 1) @== constant 1 0
    assertExpr "inv3" $ inv (constant 2 1) @== constant 2 2
    assertExpr "inv4" $ inv (constant 12 0xF0F) @== constant 12 0x0F0
    assertExpr "inv5" $ inv (constant 24 0x000001) @== constant 24 0xFFFFFE
    assertExpr "inv6" $ inv (constant 8  0x01) @== constant 8 0xFE
    assertExpr "inv7" $ inv (constant 16 0x0001) @== constant 16 0xFFFE

coreArith :: System ()
coreArith = do
  scope "add" testAdd
  scope "sub" testSub
  where
  testAdd :: System ()
  testAdd = do
    assertExpr "add1"  $ (constant 2 0 @+ constant 2 0) @== constant 2 0
    assertExpr "add2"  $ (constant 2 0 @+ constant 2 1) @== constant 2 1
    assertExpr "add3"  $ (constant 2 1 @+ constant 2 0) @== constant 2 1
    assertExpr "add4"  $ (constant 2 1 @+ constant 2 1) @== constant 2 2
    assertExpr "add5"  $ (constant 2 2 @+ constant 2 0) @== constant 2 2
    assertExpr "add6"  $ (constant 2 2 @+ constant 2 1) @== constant 2 3
    assertExpr "add7"  $ (constant 2 2 @+ constant 2 2) @== constant 2 0
    assertExpr "add8"  $ (constant 9 255 @+ constant 9 1) @== constant 9 256
    assertExpr "add9"  $ (constant 24 0x0000FF @+ constant 24 0x00FF01) @== constant 24 0x010000
    assertExpr "add10" $ (constant 24 0x00FFFF @+ constant 24 0x000001) @== constant 24 0x010000
    assertExpr "add11" $ (constant 24 0x000002 @+ constant 24 0xFFFFFE) @== constant 24 0x000000
    assertExpr "add12" $ (constant 32 0x0000FF00 @+ constant 32 0x00FF0100) @== constant 32 0x01000000
    assertExpr "add13" $ (constant 32 0x00FFFF00 @+ constant 32 0x00000100) @== constant 32 0x01000000
    assertExpr "add14" $ (constant 32 0x00000200 @+ constant 32 0xFFFFFE00) @== constant 32 0x00000000
  testSub :: System ()
  testSub = do
    assertExpr "sub1"  $ (constant 2 0 @- constant 2 0) @== constant 2 0
    assertExpr "sub2"  $ (constant 2 0 @- constant 2 1) @== constant 2 3
    assertExpr "sub3"  $ (constant 2 1 @- constant 2 0) @== constant 2 1
    assertExpr "sub4"  $ (constant 2 1 @- constant 2 1) @== constant 2 0
    assertExpr "sub5"  $ (constant 2 2 @- constant 2 0) @== constant 2 2
    assertExpr "sub6"  $ (constant 2 2 @- constant 2 1) @== constant 2 1
    assertExpr "sub7"  $ (constant 2 1 @- constant 2 3) @== constant 2 2
    assertExpr "sub8"  $ (constant 9 256 @- constant 9 1) @== constant 9 255
    assertExpr "sub9"  $ (constant 32 0x00000000 @- constant 32 0x00000100) @== constant 32 0xFFFFFF00
    assertExpr "sub10" $ (constant 32 0x01000000 @- constant 32 0x00FF0100) @== constant 32 0x0000FF00

coreComp :: System ()
coreComp = do
  scope "lt" testLT
  where
  testLT :: System ()
  testLT = do
    assertExpr "lt0" $ inv (constant 2 0 @< constant 2 0)
    assertExpr "lt1" $     (constant 2 0 @< constant 2 1)
    assertExpr "lt2" $     (constant 2 0 @< constant 2 2)
    assertExpr "lt3" $     (constant 2 0 @< constant 2 3)
    assertExpr "lt4" $ inv (constant 2 1 @< constant 2 0)
    assertExpr "lt5" $ inv (constant 2 1 @< constant 2 1)
    assertExpr "lt6" $     (constant 2 1 @< constant 2 2)
    assertExpr "lt7" $     (constant 2 1 @< constant 2 3)
    assertExpr "lt8" $ inv (constant 2 2 @< constant 2 0)
    assertExpr "lt9" $ inv (constant 2 2 @< constant 2 1)
    assertExpr "ltA" $ inv (constant 2 2 @< constant 2 2)
    assertExpr "ltB" $     (constant 2 2 @< constant 2 3)
    assertExpr "ltC" $ inv (constant 2 3 @< constant 2 0)
    assertExpr "ltD" $ inv (constant 2 3 @< constant 2 1)
    assertExpr "ltE" $ inv (constant 2 3 @< constant 2 2)
    assertExpr "ltF" $ inv (constant 2 3 @< constant 2 3)
    assertExpr "lt64a" $ inv (constant 64 1 @< constant 64 0)
    assertExpr "lt64b" $ inv (constant 64 0 @< constant 64 0)
    assertExpr "lt64c" $     (constant 64 0 @< constant 64 1)

coreCtrl :: System ()
coreCtrl = do
  scope "mux" testMux
  where
    testMux :: System ()
    testMux = do
      assertExpr "mux1" $ mux true  (constant 8 0x12) (constant 8 0x34) @== constant 8 0x12
      assertExpr "mux2" $ mux false (constant 8 0x56) (constant 8 0x78) @== constant 8 0x78
      assertExpr "mux3" $ mux true  (constant 32 0x01234567) (constant 32 0x89ABCDEF) @== constant 32 0x01234567
      assertExpr "mux4" $ mux false (constant 32 0x76543210) (constant 32 0xFEDCBA98) @== constant 32 0xFEDCBA98

coreRules :: System ()
coreRules = do
  scope "counter" testCounter
  scope "mutualCounter" testMutualCounter
  where
    testCounter :: System ()
    testCounter = do
      counter <- reg "counter" 3 (Just 0)
      rule "counterIncrement" (counter <== counter @+ one (width counter))
      coverExpr "counter0" (counter @== constant 3 0)
      coverExpr "counter1" (counter @== constant 3 1)
      coverExpr "counter2" (counter @== constant 3 2)
      coverExpr "counter3" (counter @== constant 3 3)
      coverExpr "counter4" (counter @== constant 3 4)
      coverExpr "counter5" (counter @== constant 3 5)
      coverExpr "counter6" (counter @== constant 3 6)
      coverExpr "counter7" (counter @== constant 3 7)

    testMutualCounter :: System ()
    testMutualCounter = do
      ctrl <- reg "crtl" 1 (Just 0)
      counter <- reg "counter" 3 (Just 0)
      rule "counterIncrement0" (incr (inv ctrl) counter)
      rule "counterIncrement1" (incr      ctrl  counter)
      coverExpr "counter0" (counter @== constant 3 0)
      coverExpr "counter1" (counter @== constant 3 1)
      coverExpr "counter2" (counter @== constant 3 2)
      coverExpr "counter3" (counter @== constant 3 3)
      coverExpr "counter4" (counter @== constant 3 4)
      coverExpr "counter5" (counter @== constant 3 5)
      coverExpr "counter6" (counter @== constant 3 6)
      coverExpr "counter7" (counter @== constant 3 7)
      where
      incr :: Expr -> Expr -> Action ()
      incr c counter = do
        when c
        counter <== counter @+ one (width counter)


