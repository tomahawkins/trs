-- | TRS to C compilation.
module C.Compiler
  ( compile
  , Word (..)
  ) where

import Control.Monad hiding (join)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import System.Exit
import System.IO

import TRS.Checker
import TRS.Types
import Utils


-- | The Configuration of the generated C code and the target machine.
--   Word width of the target machine.
data Word = Word8 | Word16 | Word32 | Word64 deriving Show

{-
-- The structure of the generated C code.
data Structure
  = Callable     -- | No main function.  Intended to be called by another program.  Also produces a header file.
  | StandAlone   -- | A normal main function.  Initializes, then runs forever.
  | Verification -- | A main function for verification.  Initializes, then runs until all assertions have been checked.
  -}

wordWidth :: Word -> Int
wordWidth Word8  =  8
wordWidth Word16 = 16
wordWidth Word32 = 32
wordWidth Word64 = 64

wordCount :: Word -> Int -> Int
wordCount _ w | w <= 0 = 0
wordCount word w       = 1 + wordCount word (w - wordWidth word)

mask :: Word -> Int -> Integer
mask word width = power 2 (toInteger widthMSB) - 1 
  where
  widthMSB :: Int
  widthMSB = if width `mod` wordWidth word == 0 then wordWidth word else width `mod` wordWidth word

cType :: Word -> String
cType Word8  = "unsigned char"
cType Word16 = "unsigned short int"
cType Word32 = "unsigned long int"
cType Word64 = "unsigned long long int"


-- | Compiles a TRS 'System' to C.
compile :: String -> Word -> System () -> IO ()
compile name word system = do
  sys <- extract system
  case sys of
    Nothing -> putStrLn "ERROR: Design rule checks failed.  Compilation aborted." >> exitWith (ExitFailure 1)
    Just (SystemElab rules regs asserts exprs) -> do
      asserts' <- filterM isAssertSupported asserts  
      writeFile (name ++ ".c") (code word rules regs asserts' exprs)
  where
  isAssertSupported :: Assert -> IO Bool
  isAssertSupported (Assert name prop) = case prop of
    PropertyAlways (PropertyExpr _) -> return True
    PropertyAlways (PropertyImply (PropertyExpr _) (PropertyExpr _)) -> return True
    _ -> do
      putStrLn $ "WARNING: Assertion " ++ name ++ " is not supported in C."
      return False

code :: Word -> [Rule] -> [Reg] -> [Assert] -> [Expr] -> String
code word rules regs asserts exprs =
  declare word regs asserts assertId exprs exprId ++
  primitives word ++
  assertions word asserts assertId exprId ++
  initialize word regs asserts assertId ++
  calcRules word exprId rules ++
  main
  where
  exprId :: Expr -> Int
  exprId = (Map.!) (Map.fromList (zip exprs [0 .. length exprs - 1]))

  assertId :: Assert -> Int
  assertId = (Map.!) (Map.fromList (zip asserts [0 .. length asserts - 1]))

declare :: Word -> [Reg] -> [Assert] -> (Assert -> Int) -> [Expr] -> (Expr -> Int) -> String
declare word regs asserts assertId exprs exprId = unlines
  [ "#include <stdlib.h>"
  , "#include <stdio.h>"
  , ""
  , join (map declareReg regs) "\n"
  , join (map declareExpr exprs) "\n"
  , join (map declareAssert asserts) "\n"
  , "unsigned char assertions_pass;"
  , ""
  ]
  where
  declareReg (Reg uid name width _)  = cType word ++ " r" ++ show uid ++ "[" ++ show (wordCount word width) ++ "];  // Register " ++ name
  declareExpr expr = case expr of
    Val (Reg uid _ _ _) -> cType word ++ " * e" ++ show (exprId expr) ++ " = r" ++ show uid ++ ";"
    Const w v           -> cType word ++ " e" ++ show (exprId expr) ++ "[" ++ show (wordCount word w) ++ "] = {" ++ join (map show (constant word w v)) ", " ++ "};"
    _                   -> cType word ++ " e" ++ show (exprId expr) ++ "[" ++ show (wordCount word (width expr)) ++ "];"
  declareAssert assert = "unsigned char a" ++ show (assertId assert) ++ ";"

expression :: Word -> (Expr -> Int) -> [Expr] -> String
expression word exprId exprs = if null code then "" else code ++ "\n"
  where
  code = join (mapMaybe f (topological exprs)) "\n"
  f :: Expr -> Maybe String
  f x = case x of
    Add a b      -> Just $ prim "add" [xWords, xMask, var a, var b, var x]
    Sub a b      -> Just $ prim "sub" [xWords, xMask, var a, var b, var x]
    Not a        -> Just $ prim "not" [xWords, xMask, var a, var x]
    And a b      -> Just $ prim "and" [xWords, xMask, var a, var b, var x]
    Xor a b      -> Just $ prim "xor" [xWords, xMask, var a, var b, var x]
    Or  a b      -> Just $ prim "or"  [xWords, xMask, var a, var b, var x]
    Select _ _ _ -> error "Bit selection not supported yet." --XXX
    Concat _ _   -> error "Concatenation not supported yet." --XXX
    Mux    c h l -> Just $ prim "mux" [xWords, var c, var h, var l, var x]
    Eq     a b   -> Just $ prim "eq" [words a, var a, var b, var x]
    Lt     a b   -> Just $ prim "lt" [words a, var a, var b, var x]
    Const  _ _   -> Nothing
    Nondet _     -> error "Nondeterministic values not supported."
    Val    _     -> Nothing
    where
    var expr = "e" ++ show (exprId expr)
    words expr = show (wordCount word (width expr))
    xWords = words x
    xMask  = show (mask word (width x))

prim :: String -> [String] -> String
prim name args = "  trs_" ++ name ++ "(" ++ join args ", " ++ ");"




assertions :: Word -> [Assert] -> (Assert -> Int) -> (Expr -> Int) -> String
assertions word asserts assertId exprId = unlines
  [ "void check_assertions () {"
  , join (map assertion asserts) "\n"
  , "  if (1" ++ concatMap (\ assert -> " && a" ++ show (assertId assert)) asserts ++ ") {"
  , "    exit (! assertions_pass);"
  , "  }"
  , "}"
  , ""
  ]
  where
  assertion :: Assert -> String
  assertion assert@(Assert name (PropertyAlways (PropertyExpr check))) =
    expression word exprId [check] ++
    "  a" ++ show (assertId assert) ++ " = 1;\n" ++
    "  if (e" ++ show (exprId check) ++ "[0]) {\n" ++
    "    printf(\"Assertion passed: " ++ name ++ "\\n\");\n" ++
    "  }\n" ++
    "  else {\n" ++
    "    printf(\"ERROR: Assertion failed: " ++ name ++ "\\n\");\n" ++
    "    assertions_pass = 0;\n" ++
    "  }"
  assertion assert@(Assert name (PropertyAlways (PropertyImply (PropertyExpr cond) (PropertyExpr check)))) =
    expression word exprId [cond, check] ++
    "  if (e" ++ show (exprId cond) ++ "[0]) {\n" ++
    "    a" ++ show (assertId assert) ++ " = 1;\n" ++
    "    if (e" ++ show (exprId check) ++ "[0]) {\n" ++
    "      printf(\"Assertion passed: " ++ name ++ "\\n\");\n" ++
    "    }\n" ++
    "    else {\n" ++
    "      printf(\"ERROR: Assertion failed: " ++ name ++ "\\n\");\n" ++
    "      assertions_pass = 0;\n" ++
    "    }\n" ++
    "  }"
  assertion _ = error "Assertion not supported."

initialize :: Word -> [Reg] -> [Assert] -> (Assert -> Int) -> String
initialize word regs asserts assertId = unlines
  [ "void init () {"
  , join (map initReg  regs) "\n"
  , join (map initAssert asserts) "\n"
  , "  assertions_pass = 1;"
  , "  check_assertions ();"
  , "}"
  , ""
  ]
  where

  initReg (Reg uid _ w (Just v)) = constant' ("r" ++ show uid) w v
  initReg (Reg uid _ w Nothing ) = constant' ("r" ++ show uid) w 0  --XXX Add a note that nondet init values are set to 0.

  initAssert a = "  a" ++ show (assertId a) ++ " = 0;"

  constant' :: String -> Int -> Integer -> String
  constant' name w value = join (map (\ (i,v) -> "  " ++ name ++ "[" ++ show i ++ "] = " ++ show v ++ ";") (enum (constant word w value))) "\n"

constant :: Word -> Int -> Integer -> [Integer]
constant word w v = constant' (wordCount word w) v
  where
  constant' i _ | i <= 0 = []
  constant' i v          = (v `mod` toInteger shift) : constant' (i - 1) (v `div` toInteger shift)
  shift = power 2 (toInteger $ wordWidth word)

calcRules :: Word -> (Expr -> Int) -> [Rule] -> String
calcRules word exprId rules = unlines
  [ "void rules () {"
  , join (map (rule word exprId) rules) "\n"
  , "}"
  , ""
  ]

rule :: Word -> (Expr -> Int) -> Rule -> String
rule word exprId (Rule name condition assigns) =
  "  // Rule " ++ name ++ "\n" ++
  expression word exprId [condition] ++
  "  if (e" ++ show (exprId condition) ++ ") {\n" ++
  expression word exprId exprs ++
  concatMap assign assigns ++
  "  }\n" ++
  "  check_assertions ();\n"
  where
  (_,exprs) = unzip assigns
  assign (reg,expr) = "  " ++ prim "copy" [show (wordCount word (width expr)), "e" ++ show (exprId expr), "r" ++ show (uid reg)]    

main :: String
main = unlines
  [ "int main () {"
  , "  init ();"
  , "  while (1)"
  , "    rules ();"
  , "  return 0;"
  , "}"
  , ""
  ]


primitives :: Word -> String
primitives word = unlines
  [ "void trs_copy(int words, " ++ single ++ "* a, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  for (i = 0; i < words; i++)"
  , "    x[i] = a[i];"
  , "}"
  , ""
  , "void trs_mask(int words, " ++ single ++ " mask, " ++ single ++ "* x)"
  , "{"
  , "  x[words - 1] = x[words - 1] & mask;"
  , "}"
  , ""
  , "void trs_not(int words, " ++ single ++ " mask, " ++ single ++ "* a, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  for (i = 0; i < words; i++)"
  , "    x[i] = ~ a[i];"
  , "  trs_mask(words, mask, x);"
  , "}"
  , ""
  , "void trs_and(int words, " ++ single ++ " mask, " ++ single ++ "* a, " ++ single ++ "* b, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  for (i = 0; i < words; i++)"
  , "    x[i] = a[i] & b[i];"
  , "  trs_mask(words, mask, x);"
  , "}"
  , ""
  , "void trs_xor(int words, " ++ single ++ " mask, " ++ single ++ "* a, " ++ single ++ "* b, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  for (i = 0; i < words; i++)"
  , "    x[i] = a[i] ^ b[i];"
  , "  trs_mask(words, mask, x);"
  , "}"
  , ""
  , "void trs_or(int words, " ++ single ++ " mask, " ++ single ++ "* a, " ++ single ++ "* b, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  for (i = 0; i < words; i++)"
  , "    x[i] = a[i] | b[i];"
  , "  trs_mask(words, mask, x);"
  , "}"
  , ""
  , "void trs_concat_over(int words_a, int words_b, int shift_left, int shift_right, " ++ single ++ "* a, " ++ single ++ "* b, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  int j;"
  , "  for (i = 0; i < words_b; i++)"
  , "    x[i] = b[i];"
  , "  for (i = 0, j = words_b - 1; i < words_a; i++, j++) {"
  , "    x[j] = x[j] | (" ++ mask ++ " & (a[i] << shift_left));"
  , "    x[j + 1] = a[i] >> shift_right;"
  , "  }"
  , "}"
  , ""
  , "void trs_concat_under(int words_a, int words_b, int shift_left, int shift_right, " ++ single ++ "* a, " ++ single ++ "* b, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  int j;"
  , "  for (i = 0; i < words_b; i++)"
  , "    x[i] = b[i];"
  , "  for (i = 0, j = words_b - 1; i < words_a - 1; i++, j++) {"
  , "    x[j] = x[j] | (" ++ mask ++ " & (a[i] << shift_left));"
  , "    x[j + 1] = a[i] >> shift_right;"
  , "  }"
  , "  x[j] = x[j] | (" ++ mask ++ " & (a[i] << shift_left));"
  , "}"
  , ""
  , "void trs_concat_simple(int words_a, int words_b, " ++ single ++ "* a, " ++ single ++ "* b, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  int j;"
  , "  for (i = 0; i < words_b; i++)"
  , "    x[i] = b[i];"
  , "  for (i = 0, j = words_b; i < words_a; i++, j++)"
  , "    x[j] = a[i];"
  , "}"
  , ""
  , "void trs_select(int word, int bit, " ++ single ++ "* a, " ++ single ++ "* x)"
  , "{"
  , "  x[0] = 1 & (a[word] >> bit);"
  , "}"
  , ""
  , "void trs_eq(int words, " ++ single ++ "* a, " ++ single ++ "* b, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  x[0] = 1;"
  , "  for (i = 0; i < words; i++)"
  , "    x[0] = x[0] && a[i] == b[i];"
  , "}"
  , ""
  , "void trs_lt(int words, " ++ single ++ "* a, " ++ single ++ "* b, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  x[0] = 0;"
  , "  for (i = words - 1; i >= 0; i--) {"
  , "    if (a[i] < b[i]) {"
  , "      x[0] = 1;"
  , "      return;"
  , "    }"
  , "    else if (a[i] > b[i]) {"
  , "      return;"
  , "    }"
  , "  }"
  , "}"
  , ""
  , "void trs_add(int words, " ++ single ++ " mask, " ++ single ++ "* a, " ++ single ++ "* b, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  " ++ double ++ " tmp = 0;"
  , "  for (i = 0; i < words; i++) {"
  , "    tmp = (" ++ double ++ ") a[i] + (" ++ double ++ ") b[i] + tmp;"
  , "    x[i] = (" ++ single ++ ") (tmp & " ++ mask ++ ");"
  , "    tmp = (tmp >> " ++ width ++ ") & 1;"
  , "  }"
  , "  trs_mask(words, mask, x);"
  , "}"
  , ""
  , "void trs_sub(int words, " ++ single ++ " mask, " ++ single ++ "* a, " ++ single ++ "* b, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  " ++ double ++ " tmp = 0;"
  , "  for (i = 0; i < words; i++) {"
  , "    tmp = (" ++ double ++ " ) a[i] - (" ++ double ++ " ) b[i] - tmp;"
  , "    x[i] = (" ++ single ++ ") (tmp & " ++ mask ++ ");"
  , "    tmp = (tmp >> " ++ width ++ ") & 1;"
  , "  }"
  , "  trs_mask(words, mask, x);"
  , "}"
  , ""
  , "void trs_mul(int words, " ++ single ++ " mask, " ++ single ++ "* a, " ++ single ++ "* b, " ++ single ++ "* x)"
  , "{"
  , "  int i, ia, ib, ic;"
  , "  " ++ double ++ " tmp;"
  , "  for (i = 0; i < words; i++) x[i] = 0;"
  , "  for (i = 0; i < words; i++) {"
  , "    for (ia = i, ib = 0; ia >= 0; ia--, ib++) {"
  , "      tmp = (" ++ double ++ ") (a[ia]) * (" ++ double ++ ") (b[ib]);"
  , "      for (ic = i; ic < words; ic++) {"
  , "        tmp = tmp + (" ++ double ++ ") x[ic];"
  , "        x[ic] = (" ++ single ++ ") (tmp & " ++ mask ++ ");"
  , "        tmp = (tmp >> 8);"
  , "      }"
  , "    }"
  , "  }"
  , "  trs_mask(words, mask, x);"
  , "}"
  , ""
  , "void trs_mux(int words, " ++ single ++ "* select, " ++ single ++ "* on_1, " ++ single ++ "* on_0, " ++ single ++ "* x)"
  , "{"
  , "  int i;"
  , "  for (i = 0; i < words; i++)"
  , "    x[i] = select[0] ? on_1[i] : on_0[i];"
  , "}"
  , ""
  , "void trs_ff(int words, " ++ single ++ "* clk, " ++ single ++ "* q)"
  , "{"
  , "  int i;"
  , "  " ++ single ++ "* state_clk = & q[words];"
  , "  " ++ single ++ "* state_d   = & q[words + 1];"
  , "  if (clk[0] && ! state_clk[0]) "
  , "    for (i = 0; i < words; i++)"
  , "      q[i] = state_d[i];"
  , "  state_clk[0] = clk[0];"
  , "}"
  , ""
  , "void trs_ff_update(int words, " ++ single ++ "* data, " ++ single ++ "* q)"
  , "{"
  , "  int i;"
  , "  " ++ single ++ "* state_d   = & q[words + 1];"
  , "  for (i = 0; i < words; i++)"
  , "    state_d[i] = data[i];"
  , "}"
  , ""
  , "void trs_ffc(int words, " ++ single ++ "* clr, " ++ single ++ "* clk, " ++ single ++ "* q)"
  , "{"
  , "  int i;"
  , "  " ++ single ++ "* state_clr = & q[words];"
  , "  " ++ single ++ "* state_clk = & q[words + 1];"
  , "  " ++ single ++ "* state_d   = & q[words + 2];"
  , "  if (clr[0] && ! state_clr[0])"
  , "    for (i = 0; i < words; i++)"
  , "      q[i] = 0;"
  , "  else if (clk[0] && ! state_clk[0]) "
  , "    for (i = 0; i < words; i++)"
  , "      q[i] = state_d[i];"
  , "  state_clr[0] = clr[0];"
  , "  state_clk[0] = clk[0];"
  , "}"
  , ""
  , "void trs_ffc_update(int words, " ++ single ++ "* data, " ++ single ++ "* q)"
  , "{"
  , "  int i;"
  , "  " ++ single ++ "* state_d = & q[words + 2];"
  , "  for (i = 0; i < words; i++)"
  , "    state_d[i] = data[i];"
  , "}"
  , ""
  ]
  where
  single = cType word
  double = case word of
    Word8  -> "unsigned short int"
    Word16 -> "unsigned long int"
    Word32 -> "unsigned long long int"
    Word64 -> "unsigned long long long int"
  width = show (wordWidth word)
  mask = "0x" ++ replicate (wordWidth word `div` 4) 'F'

