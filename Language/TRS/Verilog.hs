-- | TRS Verilog code generation.
module Language.TRS.Verilog ( verilog ) where

import Control.Monad.State
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import System.Exit
import System.IO

import Language.TRS.Elaboration
import Language.TRS.Logic
import Language.TRS.Scheduling


type Writer = StateT (Handle, Int, Map.Map Bit String) IO

writeStr :: String -> Writer ()
writeStr s = do
  (h,_,_) <- get
  liftIO $ hPutStr h s

writeStrLn :: String -> Writer ()
writeStrLn s = do
  (h,_,_) <- get
  liftIO $ hPutStrLn h s

nextRef :: Bit -> Writer (Writer ())
nextRef s = do
  (h,i,m) <- get
  let name = "_" ++ show i
  put (h,i+1,Map.insert s name m)
  return $ writeStr name

lookupRef :: Bit -> Writer (Maybe (Writer ()))
lookupRef s = do
  (_,_,m) <- get
  case Map.lookup s m of
    Nothing -> return Nothing
    Just s' -> return $ Just $ writeStr s'

-- | Compiles a 'System' to a Verilog-PSL file for simulation, formal verification, and synthesis.
verilog :: String -> System () -> IO ()
verilog name system = do
  r <- elaborate system
  case r of
    Nothing -> putStrLn "ERROR: Design rule checks failed.  Verilog generation aborted." >> exitWith (ExitFailure 1)
    Just systemDB -> do
      r <- schedule systemDB
      case r of
        Nothing -> putStrLn "ERROR: Rule scheduling failed.  Verilog generation aborted." >> exitWith (ExitFailure 1)
        Just (doc, assigns, displays) -> do
          putStrLn $ "Starting Verilog netlist generation (" ++ name ++ ".v) ..."
          hFlush stdout
          h <- openFile (name ++ ".v") WriteMode
          hPutStrLn h $ comment $ "Final Rule Priority:\n\n" ++ doc
          execStateT (writeVerilog name systemDB assigns displays) (h,0,Map.empty)
          hClose h

comment :: String -> String
comment a = "//  " ++ concatMap comment' a
comment' '\n' = "\n//  "
comment' c = c:[]

writeVerilog :: String -> SystemDB -> [(Reg,(Signal,Signal))] -> [(Signal,String,[Signal],Bool)] -> Writer ()
writeVerilog name sys assigns displays = do
  writeStr $ "module " ++ name ++ "\n  ( reset\n  , clock\n"
  mapM_ (\ (Input  _ n _) -> writeStrLn $ "  , " ++ n) $ sysInputs  sys
  mapM_ (\ (Output _ n _) -> writeStrLn $ "  , " ++ n) $ sysOutputs sys
  writeStrLn "  );"
  writeStrLn "  input  reset;"
  writeStrLn "  input  clock;"
  mapM_ declareInput  $ sysInputs   sys
  mapM_ declareOutput $ sysOutputs  sys
  mapM_ declareReg    $ assigns
  mapM_ declareLabel  $ sysLabels   sys
  mapM_ modelLabel    $ sysLabels   sys
  mapM_ modelReg      $ assigns
  mapM_ modelDisplay  $ displays
  mapM_ modelOutput   $ sysOutputs  sys
  --mapM_ modelVerify   $ sysVerifys  sys
  writeStrLn "endmodule"

widthDecl :: Widthable a => a -> String
widthDecl a = if width a == 1 then "" else "[" ++ show (width a - 1) ++ ":0] "

declareInput :: Input -> Writer ()
declareInput i@(Input _ name _) = writeStrLn $ "  input  " ++ widthDecl i ++ name ++ ";"

declareOutput :: Output -> Writer ()
declareOutput o@(Output _ name _) = writeStrLn $ "  output " ++ widthDecl o ++ name ++ ";"

declareReg :: (Reg,(Signal,Signal)) -> Writer ()
declareReg (r@(Reg _ name _ _), _) = do
  writeStrLn $ "  reg    " ++ widthDecl r ++ name ++ ";"

-- | Returns an action that writes in the expression of a signal.
--   Writes out any dependent signals not already declared.
ref :: Bit -> Writer (Writer ())
ref s = case s of 
  Not a -> do
    a <- ref a
    return (writeStr "! (" >> a >> writeStr ")")
  Const v -> return $ writeStr ("1'b" ++ (if v then "1" else "0"))
  BitInput (Input _ n w)   m -> return (writeStr $ n ++ (if w == 1 then "" else "[" ++ show m ++ "]"))
  BitReg   (Reg   _ n w _) m -> return (writeStr $ n ++ (if w == 1 then "" else "[" ++ show m ++ "]"))
  BitLabel (Label _ n s)   m -> return (writeStr $ n ++ (if width s == 1 then "" else "[" ++ show m ++ "]"))
  s -> do
    l <- lookupRef s
    case l of
      Just r -> return r
      Nothing -> case s of
        And a b -> do
          a <- ref a
          b <- ref b
          r <- nextRef s
          writeStr "  wire   " >> r >> writeStr " = " >> a >> writeStr " && " >> b >> writeStrLn ";"
          return r
        Or a b -> do
          a <- ref a
          b <- ref b
          r <- nextRef s
          writeStr "  wire   " >> r >> writeStr " = " >> a >> writeStr " || " >> b >> writeStrLn ";"
          return r
        _ -> error "Verilog.ref: Unexpected Bit type."

refs :: Signal -> Writer (Writer ())
refs s = do
  bits <- mapM ref s
  return $ if length bits == 1 then head bits else writeStr "{" >> refs' bits >> writeStr "}"

refs' :: [Writer ()] -> Writer ()
refs' [] = return ()
refs' [a] = a
refs' (a:b) = a >> writeStr ", " >> refs' b
  
declareLabel :: Label -> Writer ()
declareLabel (Label _ name signal) = do
  writeStrLn $ "  wire   " ++ widthDecl signal ++ name ++ ";"

modelLabel :: Label -> Writer ()
modelLabel (Label _ name signal) = do
  s <- refs signal
  writeStr ("  assign " ++ name ++ " = ") >> s >> writeStrLn ";"

modelReg :: (Reg,(Signal,Signal)) -> Writer ()
modelReg (Reg _ name width value, (enable, s)) = do
  enable <- refs enable
  s      <- refs s
  writeStrLn $ "  always @ (posedge reset or posedge clock)"
  writeStrLn $ "    if (reset)"
  writeStrLn $ "      " ++ name ++ " <= " ++ show width ++ "'d" ++ show value ++ ";"
  writeStr     "    else if (" >> enable >> writeStrLn ")"
  writeStr    ("      " ++ name ++ " <= ") >> s >> writeStrLn ";"

modelDisplay :: (Signal,String,[Signal],Bool) -> Writer ()
modelDisplay (enable, string, signals, isFinish) = do
  enable <- refs enable
  signals <- mapM refs signals
  writeStrLn $ "  always @ (posedge clock)"
  writeStr     "    if (" >> enable >> writeStrLn ") begin"
  writeStr    ("      $display(\"" ++ string ++ "\", ") >> refs' signals >> writeStrLn ");"
  (if isFinish then writeStrLn "      $finish;" else return ())
  writeStrLn   "    end"

modelOutput :: Output -> Writer ()
modelOutput (Output _ name s) = do
  s <- refs s
  writeStr ("  assign " ++ name ++ " = ") >> s >> writeStrLn ";"

{-
modelVerify :: Verify -> Writer ()
modelVerify verify = do
  case verify of
    Assert name enable prop -> writeStr ("  // psl " ++ name ++ ": assert ") >> modelProperty prop >> writeStr " @ (posedge (clock || ! " >> do { e <- ref enable ; e } >> writeStrLn "));"
    Assume name enable prop -> writeStr ("  // psl " ++ name ++ ": assume ") >> modelProperty prop >> writeStr " @ (posedge (clock || ! " >> do { e <- ref enable ; e } >> writeStrLn "));"
    Cover  name enable seq  -> writeStr ("  // psl " ++ name ++ ": cover  ") >> modelSequence seq  >> writeStr " @ (posedge (clock || ! " >> do { e <- ref enable ; e } >> writeStrLn "));"

modelProperty :: Property -> Writer ()
modelProperty prop = case prop of
  PropertySignal signal  -> do { s <- ref signal ; s }
  PropertyNot  p         -> writeStr "(! " >> modelProperty p >> writeStr ")"
  PropertyAnd p1 p2      -> writeStr "(" >> modelProperty p1 >> writeStr " && " >> modelProperty p2 >> writeStr ")"
  PropertyImply p1 p2    -> writeStr "(" >> modelProperty p1 >> writeStr " -> " >> modelProperty p2 >> writeStr ")"
  PropertyAlways p       -> writeStr "(always " >> modelProperty p >> writeStr ")"
  PropertySequence s     -> writeStr "(" >> modelSequence s >> writeStr " !)"
  PropertySequenceWeak s -> modelSequence s
  PropertyIfSequence s p -> writeStr "(" >> modelSequence s >> writeStr " |-> " >> modelProperty p >> writeStr ")"

modelSequence :: Sequence -> Writer ()
modelSequence sequence = case sequence of
  SequenceSignal signal -> writeStr "{" >> do { s <- ref signal ; s } >> writeStr "}"
  SequenceAnd s1 s2     -> writeStr "{" >> modelSequence s1 >> writeStr " && " >> modelSequence s2 >> writeStr "}"
  SequenceConcat s1 s2  -> writeStr "{" >> modelSequence s1 >> writeStr " ; "  >> modelSequence s2 >> writeStr "}"
  SequenceInfuse s1 s2  -> writeStr "{" >> modelSequence s1 >> writeStr " : "  >> modelSequence s2 >> writeStr "}"
  SequenceAltern s1 s2  -> writeStr "{" >> modelSequence s1 >> writeStr " | "  >> modelSequence s2 >> writeStr "}"
  SequenceRepeat s      -> writeStr "{" >> modelSequence s  >> writeStr "[*]}"
  SequenceNull          -> writeStr "{[*0]}"
-}
