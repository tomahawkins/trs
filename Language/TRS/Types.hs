-- | TRS types.

module Language.TRS.Types
  (
  -- * System and Action Containers
    System
  , SystemDB   (..)
  , Action
  , ActionDB   (..)
  , Relation   (..)
  -- ** Container Utilities
  , getSystemDB
  , putSystemDB
  , evalSystem
  , buildAction
  -- * Overloading Type Classes for Signals, Sequences, and Properties
  , Sig        (..)
  , Seq        (..)
  , Prop       (..)
  -- * Rules
  , Rule       (..)
  , Execution  (..)
  -- * Signals, IO Ports, Registers, Sequences, and Properties
  , Signal     (..)
  , Reg        (..)
  , Sequence   (..)
  , Property   (..)
  -- * Verification Directives
  , Verify     (..)
  -- * Type Aliases and Utilities
  , UID
  , Width
  , Depth
  , Name
  , Widthable
  , width
  , uid
  , node
  , signal
  , insertSignal
  , insertSystemSignal
  , systemRTL
  ) where

import qualified Control.Monad.State as State

import qualified Language.TRS.RTL as RTL
import Language.TRS.Utils

type UID = Int

data Signal
  = Add    Signal Signal
  | Sub    Signal Signal
  | Not    Signal
  | And    Signal Signal
  | Xor    Signal Signal
  | Or     Signal Signal
  | Select Signal Int Int
  | Concat Signal Signal
  | Mux    Signal Signal Signal
  | Eq     Signal Signal
  | Lt     Signal Signal
  | Const  Int Integer
  | Node   Bool Int RTL.Node deriving (Eq, Ord)

instance Show Signal where
  show = showSignal 6 

showSignal :: Int -> Signal -> String
showSignal n _ | n <= 0 = "..."
showSignal n s = show (width s) ++ "'(" ++ showSignal' n s ++ ")"

showSignal' :: Int -> Signal -> String
showSignal' n _ | n <= 0 = "..."
showSignal' n s = case s of
  Add a b      -> showSignal (n-1) a ++ " + " ++ showSignal (n-1) b
  Sub a b      -> showSignal (n-1) a ++ " - " ++ showSignal (n-1) b
  Not a        -> "~ " ++ showSignal (n-1) a
  And a b      -> showSignal (n-1) a ++ " & " ++ showSignal (n-1) b
  Xor a b      -> showSignal (n-1) a ++ " ^ " ++ showSignal (n-1) b
  Or  a b      -> showSignal (n-1) a ++ " | " ++ showSignal (n-1) b
  Select a m l -> showSignal (n-1) a ++ "[" ++ show m ++ ":" ++ show l ++ "]"
  Concat a b   -> showSignal (n-1) a ++ " ++ " ++ showSignal (n-1) b
  Mux c a b    -> showSignal (n-1) c ++ " ? " ++ showSignal (n-1) a ++ " : " ++ showSignal (n-1) b
  Eq  a b      -> showSignal (n-1) a ++ " == " ++ showSignal (n-1) b
  Lt  a b      -> showSignal (n-1) a ++ " < " ++ showSignal (n-1) b
  Const _ v    -> show v
  Node _ _ n   -> show n


-- | A class of types that can be converted to and from a 'Signal'.
class Sig a where
  toSig   :: a -> Signal
  fromSig :: Signal -> a

instance Sig Signal where
  toSig = id
  fromSig = id

-- | A width value.
type Width = Int

-- | A depth value.
type Depth = Int

-- | A name or label.
type Name = String

class Widthable a where
  -- | The width of a 'Signal' or other bit vector similar type.
  width :: a -> Width

instance Widthable Signal where
  width s = case s of
    Add a _      -> width a
    Sub a _      -> width a
    Not a        -> width a
    And a _      -> width a
    Xor a _      -> width a
    Or  a _      -> width a
    Select _ m l -> m - l + 1
    Concat a b   -> width a + width b
    Mux _ b _    -> width b
    Eq  a _      -> width a
    Lt  a _      -> width a
    Const w _    -> w
    Node _ w _   -> w


-- | The 'Reg' type represents register state elements.
data Reg = Reg Bool Int RTL.Node deriving (Eq, Ord)

instance Show Reg where
  show (Reg _ _ n) = show n

instance Widthable Reg where
  width (Reg _ w _) = w


-- | The 'Rule' type represents a rule of an atomic state transition.
data Rule = Rule 
  { ruleUID :: UID
  , ruleName :: Name
  , ruleEnable :: RTL.Node
  , ruleAction :: ActionDB
  }

-- | Execution type for 'Rule's.
data Execution = Sequential | Concurrent

instance UId Rule where
  uid = ruleUID

instance Eq Rule where
  r1 == r2 = uid r1 == uid r2

instance Ord Rule where
  compare r1 r2 = compare (uid r1) (uid r2)

instance Show Rule where
  show = ruleName


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

data SystemDB = SystemDB
  { sysNextId  :: Int
  , sysName    :: String
  , sysNames   :: [String]
  , sysEnable  :: Signal
  , sysRules   :: [Rule]
  , sysRegs    :: [RTL.Node]
  , sysVerifys :: [Verify]
  , sysInputs  :: [RTL.Node]
  , sysOutputs :: [RTL.Node]
  , sysRTL     :: RTL.RTLDB
  }

initSystemDB :: SystemDB
initSystemDB = SystemDB
  { sysNextId  = 0
  , sysName    = ""
  , sysNames   = ["reset", "clock"] ++ systemVerilogKeywords
  , sysEnable  = Const 1 1
  , sysRules   = []
  , sysRegs    = []
  , sysVerifys = []
  , sysInputs  = []
  , sysOutputs = []
  , sysRTL     = RTL.empty
  }

type Action = State.StateT ActionDB System

-- | The 'System' container holds 'Signal', 'Reg', and 'Rule' definitions.
data System a = System (SystemDB -> (SystemDB,a))

-- The 'System' container holds 'Signal', 'Reg', and 'Rule' definitions.
instance Monad System where
  (System m) >>= k = System f
    where
    f db = k' db'
      where
      (db',a) = m db
      System k' = k a

  return a = System $ \ db -> (db,a)

-- | Gets 'SystemDB'.
getSystemDB :: System SystemDB
getSystemDB = System (\ systemDB -> (systemDB,systemDB))

-- | Puts 'SystemDB'.
putSystemDB :: SystemDB -> System ()
putSystemDB systemDB = System (\ _ -> (systemDB,()))

-- | Evaluate a top level 'System'.
evalSystem :: System () -> SystemDB
evalSystem (System f) = systemDB
  where
  (systemDB, ()) = f initSystemDB

-- | The 'Action' container holds enabling conditions and state transitions.
data ActionDB = ActionDB
  { actEnable :: RTL.Node
  , actAssigns :: [(RTL.Node, RTL.Node)]
  , actExecution :: Execution
  , actActiveWhenEnabled :: Bool
  , actRelations :: [Relation]
  , actDisplays  :: [(String, [RTL.Node], Bool)]
  }

buildAction :: Action () -> System ActionDB
buildAction action = do
  true <- insertSystemSignal Nothing $ Const 1 1
  State.execStateT action $ ActionDB
    { actEnable = node true
    , actAssigns = []
    , actExecution = Concurrent
    , actActiveWhenEnabled = False
    , actRelations = []
    , actDisplays = []
    }

-- | A Relation is used for relative performance constraints between 'Action's.
data Relation = Higher UID | Lower UID | Exclusive UID | Inclusive UID deriving (Show, Eq, Ord)

instance UId Relation where
  uid (Higher u) = u
  uid (Lower  u) = u
  uid (Exclusive u) = u
  uid (Inclusive u) = u

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

insertSignal :: Maybe Name -> Signal -> RTL.RTL Signal
insertSignal name s = case s of
  Add a b -> insertBinary (RTL.add name) a b
  Sub a b -> insertBinary (RTL.sub name) a b
  And a b -> insertBinary (RTL.and_ name) a b
  Xor a b -> insertBinary (RTL.xor name) a b
  Or  a b -> insertBinary (RTL.or_ name) a b
  Eq  a b -> insertBinary (RTL.eq  name) a b
  Lt  a b -> insertBinary (RTL.lt  name) a b
  Concat a b -> insertBinary (RTL.cat name) a b
  Not a -> do
    a <- insertSignal Nothing a
    x <- RTL.inv name (node a)
    signal x
  Select a m l -> do
    a <- insertSignal Nothing a
    x <- RTL.select name (node a) m l
    signal x
  Const w v -> RTL.constant name w v >>= signal
  Mux a b c -> do
    a <- insertSignal Nothing a
    b <- insertSignal Nothing b
    c <- insertSignal Nothing c
    x <- RTL.mux name (node a) (node b) (node c)
    signal x
  Node _ _ _ -> return s

insertBinary :: (RTL.Node -> RTL.Node -> RTL.RTL RTL.Node) -> Signal -> Signal -> RTL.RTL Signal
insertBinary f a b = do
  a <- insertSignal Nothing a
  b <- insertSignal Nothing b
  x <- f (node a) (node b)
  signal x
    
insertSystemSignal :: Maybe Name -> Signal -> System Signal
insertSystemSignal name s = do
  db <- getSystemDB
  let (s',rtl) = RTL.build (insertSignal name s) (sysRTL db)
  putSystemDB $ db { sysRTL = rtl }
  return s'

node :: Signal -> RTL.Node
node (Node _ _ n) = n
node _ = error "Types.node: expecting Node constructor."

signal :: RTL.Node -> RTL.RTL Signal
signal n = do
  (v,w) <- RTL.validWidth n
  return $ Node v w n

systemRTL :: RTL.RTL a -> System a
systemRTL update = do
  db <- getSystemDB
  let (a,rtl) = RTL.build update $ sysRTL db
  putSystemDB $ db { sysRTL = rtl }
  return a

