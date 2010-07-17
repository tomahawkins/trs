module Language.TRS.Elaboration
  (
  -- * System and Action Containers
    System
  , SystemDB   (..)
  , Action
  , ActionDB   (..)
  , Relation   (..)
  -- ** Container Utilities
  , buildSystem
  , emptySystemDB
  , buildAction
  -- * Type Aliases and Utilities
  , Signal
  , Rule       (..)
  , Execution  (..)
  , Output     (..)
  , UID
  , Width
  , Depth
  , Name
  , Widthable
  , width
  , elaborate
  ) where

import Control.Monad.State
import qualified Data.List as List
import System.IO

import Language.TRS.Logic

-- | Signal type.
type Signal = [Bit]

-- | Unique identifier.
type UID = Int

-- | A width value.
type Width = Int

-- | A depth value.
type Depth = Int

-- | A name or label.
type Name = String

class Widthable a where width :: a -> Width
instance Widthable ([] a) where width = length
instance Widthable Reg where width (Reg _ _ w _) = w
instance Widthable Input where width (Input _ _ w) = w
instance Widthable Output where width (Output _ _ s) = width s

-- | Top level output.
data Output = Output UID Name Signal


-- | The 'Rule' type represents a rule of an atomic state transition.
data Rule = Rule 
  { ruleUID :: UID
  , ruleName :: Name
  , ruleEnable :: Bit
  , ruleAction :: ActionDB
  }

instance Show Rule where show r = ruleName r ++ case actExecution $ ruleAction r of { Concurrent -> "" ; Sequential -> " (sequential)" }
instance Eq  Rule where (==) a b = ruleUID a == ruleUID b
instance Ord Rule where compare a b = compare (ruleUID a) (ruleUID b)

-- | Execution type for 'Rule's.
data Execution = Sequential | Concurrent


{-
-- | A 'Sequence' is similar to a PSL SERE.
data Sequence
  = SequenceSignal Signal
  | SequenceAnd    Sequence Sequence
  | SequenceConcat Sequence Sequence
  | SequenceInfuse Sequence Sequence
  | SequenceAltern Sequence Sequence
  | SequenceRepeat Sequence
  | SequenceNull
  deriving (Show, Eq)

-- | A temporal design 'Property'.
data Property
  = PropertySignal       Signal
  | PropertyNot          Property
  | PropertyAnd          Property Property
  | PropertyImply        Property Property
  | PropertyAlways       Property
  | PropertySequence     Sequence
  | PropertySequenceWeak Sequence
  | PropertyIfSequence   Sequence Property
  deriving (Show, Eq)

-- | Operator overloading for 'Signal's and 'Sequence's.
class Seq a where
  toSeq :: a -> Sequence

instance Seq Signal where
  toSeq s = SequenceSignal s

instance Seq Sequence where
  toSeq s = s

-- | Operator overloading for 'Signal's, 'Sequence's, and 'Property's.
class Prop a where
  toProp :: a -> Property

instance Prop Signal where
  toProp s = PropertySignal s

instance Prop Sequence where
  toProp s = PropertySequence s

instance Prop Property where
  toProp p = p

-- | A VerificationDirective.
data Verify
  = Assert String Signal Property
  | Assume String Signal Property
  | Cover  String Signal Sequence
  deriving (Eq)

instance Show Verify where
  show (Assert name _ prop) = name ++ ": assert " ++ show prop ++ ";"
  show (Assume name _ prop) = name ++ ": assume " ++ show prop ++ ";"
  show (Cover  name _ seq ) = name ++ ": cover "  ++ show seq  ++ ";"
-}

data SystemDB = SystemDB
  { sysNextId  :: Int
  , sysName    :: String
  , sysNames   :: [String]
  , sysEnable  :: Bit
  , sysRules   :: [Rule]
  , sysRegs    :: [Reg]
  , sysLabels  :: [Label]
  , sysInputs  :: [Input]
  , sysOutputs :: [Output]
  }

buildSystem :: System () -> SystemDB -> IO SystemDB
buildSystem system systemDB = execStateT system systemDB

emptySystemDB :: SystemDB
emptySystemDB = SystemDB
  { sysNextId  = 0
  , sysName    = ""
  , sysNames   = "reset" : "clock" : systemVerilogKeywords
  , sysEnable  = true
  , sysRules   = []
  , sysRegs    = []
  , sysLabels  = []
  , sysInputs  = []
  , sysOutputs = []
  }

data ActionDB = ActionDB
  { actEnable :: Bit
  , actAssigns :: [(Reg, Signal)]
  , actExecution :: Execution
  , actActiveWhenEnabled :: Bool
  , actRelations :: [Relation]
  , actDisplays  :: [(String, [Signal], Bool)]
  }

buildAction :: Action () -> System ActionDB
buildAction action = do
  execStateT action $ ActionDB
    { actEnable = true
    , actAssigns = []
    , actExecution = Concurrent
    , actActiveWhenEnabled = False
    , actRelations = []
    , actDisplays = []
    }

-- | The 'Action' container holds enabling conditions, conditional actions, register assignments, and simulation directives.
type Action = StateT ActionDB System

-- | The 'System' container holds top level IO, 'Reg', and 'Rule' definitions.
type System = StateT SystemDB IO

-- | A Relation is used for relative performance constraints between 'Action's.
data Relation = Higher UID | Lower UID deriving Eq

-- | Elabortes a 'System', returning a 'SystemDB'.
elaborate :: System () -> IO (Maybe SystemDB)
elaborate system = do
  putStrLn "Starting system elaboration..."
  hFlush stdout
  systemDB <- buildSystem system emptySystemDB
  let rules    = sysRules    systemDB
      -- verifys  = sysVerifys  systemDB
      outputs  = sysOutputs  systemDB
      names    = sysNames    systemDB
      names'   = List.nub names
      duplicateNames = foldl (flip List.delete) names names'
  ruleChecks <- mapM isRuleOk rules
  --verifyChecks <- mapM isVerifyOk verifys
  outputChecks <- mapM isOutputOk outputs
  (if null duplicateNames
    then return ()
    else do
      putStrLn "ERROR: The following names are duplicated in design:"
      mapM_ (putStrLn . ("  " ++)) $ List.nub duplicateNames)
  if not (null duplicateNames && and (ruleChecks {- ++ verifyChecks -} ++ outputChecks))
    then return Nothing
    else return $ Just systemDB

isRuleOk :: Rule -> IO Bool
isRuleOk r = do
  c1 <- checkAssignConflicts
  -- c2 <- checkConditionSignal
  -- c3 <- mapM (isSignalOk $ "Rule " ++ ruleName r) (ruleEnable r : assignSignals ++ displaySignals)
  c4 <- mapM (isAssignOk r) $ actAssigns $ ruleAction r
  return $ and (c1 : c4)

  where
  assigns = actAssigns $ ruleAction r
  --condition = ruleEnable r
  --displays = actDisplays $ ruleAction r
  --displaySignals = concatMap (\ (_,s,_) -> s) displays
  name = ruleName r
  (regs, _) = unzip assigns
  regs' = List.nub regs
  dup = foldl (flip List.delete) regs regs'

  checkAssignConflicts :: IO Bool
  checkAssignConflicts = do
    if length regs /= length regs'
      then do
        putStrLn $ "ERROR: Rule " ++ name ++ " contains multiple assignments to the same register(s):" ++ concatMap (\ (Reg _ n _ _) -> "  " ++ n) (List.nub dup)
        return False
      else return True

  {-
  checkConditionSignal :: IO Bool
  checkConditionSignal = do
    if width condition /= 1
      then do
        putStrLn $ "ERROR: Rule " ++ name ++ " condition expression does not have a width of 1."
        return False
      else return True
  -}

  isAssignOk :: Rule -> (Reg,Signal) -> IO Bool
  isAssignOk rule (r@(Reg _ name _ _),e) =
    if width r == width e
      then return True
      else do
        putStrLn $ "ERROR: Rule " ++ ruleName rule ++ " contains non-matching widths in assignment to register " ++ name ++ "."
        return False

{-
isVerifyOk :: Verify -> IO Bool
isVerifyOk verify = do
  c1 <- mapM check $ verifySignals verify
  return $ and c1
  where
  name = case verify of
           Assert n _ _ -> n
           Assume n _ _ -> n
           Cover  n _ _ -> n
  verifyType = case verify of
                 Assert _ _ _ -> "Assertion"
                 Assume _ _ _ -> "Assumption"
                 Cover  _ _ _ -> "Cover point"
  check :: Signal -> IO Bool
  check expr = do
    c1 <- isSignalOk (verifyType ++ " " ++ name) expr
    if width expr /= 1
      then do
        putStrLn $ "ERROR: " ++ verifyType ++ " " ++ name ++ " contains an expression that does not have a width of 1: " ++ show expr
        return False
      else return c1
-}

{-
isSignalOk :: String -> Signal -> IO Bool
isSignalOk r sig = do
  if isSignalValid sig then return True else do
    putStrLn $ "ERROR: " ++ r ++ " contains an invalid signal width: " ++ show sig
    return False
-}

isOutputOk :: Output -> IO Bool
isOutputOk (Output _ n s) = if width s > 0 then return True else do
    putStrLn $ "ERROR: Output \"" ++ n ++ "\" contains an invalid expression width."
    return False
  

{-
verifySignals :: Verify -> [Signal]
verifySignals verify = case verify of
  Assert _ enable prop -> enable : propertySignals prop
  Assume _ enable prop -> enable : propertySignals prop
  Cover  _ enable seq  -> enable : sequenceSignals seq

propertySignals :: Property -> [Signal]
propertySignals prop = case prop of
  PropertySignal expr    -> [expr]
  PropertyNot    p1      -> propertySignals p1
  PropertyAnd    p1 p2   -> propertySignals p1 ++ propertySignals p2
  PropertyImply  p1 p2   -> propertySignals p1 ++ propertySignals p2
  PropertyAlways p1      -> propertySignals p1
  PropertySequence     s -> sequenceSignals s
  PropertySequenceWeak s -> sequenceSignals s
  PropertyIfSequence s p -> sequenceSignals s ++ propertySignals p

sequenceSignals :: Sequence -> [Signal]
sequenceSignals sequence = case sequence of
  SequenceSignal expr  -> [expr]
  SequenceAnd    s1 s2 -> sequenceSignals s1 ++ sequenceSignals s2
  SequenceConcat s1 s2 -> sequenceSignals s1 ++ sequenceSignals s2
  SequenceInfuse s1 s2 -> sequenceSignals s1 ++ sequenceSignals s2
  SequenceAltern s1 s2 -> sequenceSignals s1 ++ sequenceSignals s2
  SequenceRepeat s1    -> sequenceSignals s1
  SequenceNull         -> []
-}

systemVerilogKeywords :: [String]
systemVerilogKeywords =
  [ "alias"
  , "always"
  , "always_comb"
  , "always_ff"
  , "always_latch"
  , "and"
  , "assert"
  , "assign"
  , "assume"
  , "automatic"
  , "before"
  , "begin"
  , "bind"
  , "bins"
  , "binsof"
  , "bit"
  , "break"
  , "buf"
  , "bufif0"
  , "bufif1"
  , "byte"
  , "case"
  , "casex"
  , "casez"
  , "cell"
  , "chandle"
  , "class"
  , "clocking"
  , "cmos"
  , "config"
  , "const"
  , "constraint"
  , "context"
  , "continue"
  , "cover"
  , "covergroup"
  , "coverpoint"
  , "cross"
  , "deassign"
  , "default"
  , "defparam"
  , "design"
  , "disable"
  , "dist"
  , "do"
  , "edge"
  , "else"
  , "end"
  , "endcase"
  , "endclass"
  , "endclocking"
  , "endconfig"
  , "endfunction"
  , "endgenerate"
  , "endgroup"
  , "endinterface"
  , "endmodule"
  , "endpackage"
  , "endprimitive"
  , "endprogram"
  , "endproperty"
  , "endspecify"
  , "endsequence"
  , "endtable"
  , "endtask"
  , "enum"
  , "event"
  , "expect"
  , "export"
  , "extends"
  , "extern"
  , "final"
  , "first_match"
  , "for"
  , "force"
  , "foreach"
  , "forever"
  , "fork"
  , "forkjoin"
  , "function"
  , "generate"
  , "genvar"
  , "highz0"
  , "highz1"
  , "if"
  , "iff"
  , "ifnone"
  , "ignore_bins"
  , "illegal_bins"
  , "import"
  , "incdir"
  , "include"
  , "initial"
  , "inout"
  , "input"
  , "inside"
  , "instance"
  , "int"
  , "integer"
  , "interface"
  , "intersect"
  , "join"
  , "join_any"
  , "join_none"
  , "large"
  , "liblist"
  , "library"
  , "local"
  , "localparam"
  , "logic"
  , "longint"
  , "macromodule"
  , "matches"
  , "medium"
  , "modport"
  , "module"
  , "nand"
  , "negedge"
  , "new"
  , "nmos"
  , "nor"
  , "noshowcancelled"
  , "not"
  , "notif0"
  , "notif1"
  , "null"
  , "or"
  , "output"
  , "package"
  , "packed"
  , "parameter"
  , "pmos"
  , "posedge"
  , "primitive"
  , "priority"
  , "program"
  , "property"
  , "protected"
  , "pull0"
  , "pull1"
  , "pulldown"
  , "pullup"
  , "pulsestyle_onevent"
  , "pulsestyle_ondetect"
  , "pure"
  , "rand"
  , "randc"
  , "randcase"
  , "randsequence"
  , "rcmos"
  , "real"
  , "realtime"
  , "ref"
  , "reg"
  , "release"
  , "repeat"
  , "return"
  , "rnmos"
  , "rpmos"
  , "rtran"
  , "rtranif0"
  , "rtranif1"
  , "scalared"
  , "sequence"
  , "shortint"
  , "shortreal"
  , "showcancelled"
  , "signed"
  , "small"
  , "solve"
  , "specify"
  , "specparam"
  , "static"
  , "string"
  , "strong0"
  , "strong1"
  , "struct"
  , "super"
  , "supply0"
  , "supply1"
  , "table"
  , "tagged"
  , "task"
  , "this"
  , "throughout"
  , "time"
  , "timeprecision"
  , "timeunit"
  , "tran"
  , "tranif0"
  , "tranif1"
  , "tri"
  , "tri0"
  , "tri1"
  , "triand"
  , "trior"
  , "trireg"
  , "type"
  , "typedef"
  , "union"
  , "unique"
  , "unsigned"
  , "use"
  , "var"
  , "vectored"
  , "virtual"
  , "void"
  , "wait"
  , "wait_order"
  , "wand"
  , "weak0"
  , "weak1"
  , "while"
  , "wildcard"
  , "wire"
  , "with"
  , "within"
  , "wor"
  , "xnor"
  , "xor"
  ]

