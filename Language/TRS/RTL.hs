module Language.TRS.RTL
  ( RTL
  , RTLDB
  , Operator (..)
  , Node
  , build
  , build_
  , empty
  , valid
  , width
  , regs
  , inputs
  , outputs
  , validWidth
  , constant
  , true
  , false
  , reg
  , add
  , sub
  , inv
  , and_
  , xor
  , or_
  , eq
  , lt
  , select
  , cat
  , mux
  , input
  , output
  , display
  ) where

import Control.Monad.State
import qualified Data.Graph.Inductive as Graph
import Data.Graph.Inductive hiding ( empty )

type Operand = Int
data Info = Info (Bool,Int) (Maybe String) Operator

data Operator
  = Add
  | Sub
  | Not
  | And
  | Xor
  | Or
  | Select Int Int
  | Concat
  | Mux
  | Eq
  | Lt
  | Const Integer
  | Reg Integer
  | Input
  | Output
  | Display String Bool

type RTL = State RTLDB

data RTLDB = RTLDB
  { rtlNextId :: Int
  , rtlGraph  :: Graph.Gr Info Operand
  , rtlInputs
  , rtlOutputs
  , rtlRegs
  , rtlDisplays :: [Node]
  }

build_ :: RTL a -> RTLDB -> RTLDB
build_ rtl db = execState rtl db

build :: RTL a -> RTLDB -> (a, RTLDB)
build rtl db = runState rtl db

empty :: RTLDB
empty = RTLDB
  { rtlNextId  = 0
  , rtlGraph   = Graph.empty
  , rtlInputs  = []
  , rtlOutputs = []
  , rtlRegs    = []
  , rtlDisplays = []
  }

valid :: Node -> RTL Bool
valid n = do
  (v,_) <- validWidth n
  return v

width :: Node -> RTL Int
width n = do
  (_,w) <- validWidth n
  return w

validWidth :: Node -> RTL (Bool,Int)
validWidth n = do
  db <- get
  case Graph.lab (rtlGraph db) n of
    Nothing -> return (False,0)
    Just (Info vw _ _) -> return vw

regs :: RTL [Node]
regs = do
  db <- get
  return $ rtlRegs db

inputs :: RTL [Node]
inputs = do
  db <- get
  return $ rtlInputs db

outputs :: RTL [Node]
outputs = do
  db <- get
  return $ rtlOutputs db

nextId :: RTL Int
nextId = do
  db <- get
  let i = rtlNextId db
  put $ db { rtlNextId = i + 1 }
  return i

constant :: Maybe String -> Int -> Integer -> RTL Node
constant name w v = do
  i <- nextId
  db <- get
  put $ db { rtlGraph = Graph.insNode (i, Info (w > 0, w) name (Const v)) $ rtlGraph db }
  return i

true :: RTL Node
true = constant Nothing 1 1

false :: RTL Node
false = constant Nothing 1 0

binaryRator :: Operator -> (Int -> Int -> Bool) -> (Int -> Int -> Int) -> Maybe String -> Node -> Node -> RTL Node
binaryRator rator guard width name a b = do
  i <- nextId
  (aV,aW) <- validWidth a
  (bV,bW) <- validWidth b
  let v = aV && bV && aW > 0 && bW > 0 && guard aW bW
      w = width aW bW
  db <- get
  put $ db { rtlGraph = Graph.insEdges [(a,i,0),(b,i,1)] $ Graph.insNode (i, Info (v,w) name rator) $ rtlGraph db }
  return i

add :: Maybe String -> Node -> Node -> RTL Node
add name a b = binaryRator Add (==) const name a b

sub :: Maybe String -> Node -> Node -> RTL Node
sub name a b = binaryRator Sub (==) const name a b

and_ :: Maybe String -> Node -> Node -> RTL Node
and_ name a b = binaryRator And (==) const name a b

xor :: Maybe String -> Node -> Node -> RTL Node
xor name a b = binaryRator Xor (==) const name a b

or_ :: Maybe String -> Node -> Node -> RTL Node
or_ name a b = binaryRator Or  (==) const name a b

eq :: Maybe String -> Node -> Node -> RTL Node
eq name a b = binaryRator Eq  (==) (\ _ _ -> 1) name a b

lt :: Maybe String -> Node -> Node -> RTL Node
lt name a b = binaryRator Lt  (==) (\ _ _ -> 1) name a b

mux :: Maybe String -> Node -> Node -> Node -> RTL Node
mux name a b c = do
  i <- nextId
  (aV,aW) <- validWidth a
  (bV,bW) <- validWidth b
  (cV,cW) <- validWidth c
  let v = aV && bV && cV && aW == 1 && bW == cW && bW > 0
      w = bW
  db <- get
  put $ db { rtlGraph = Graph.insEdges [(a,i,0),(b,i,1),(c,i,2)] $ Graph.insNode (i, Info (v,w) name Mux) $ rtlGraph db }
  return i

select :: Maybe String -> Node -> Int -> Int -> RTL Node
select name a m l = do
  i <- nextId
  (aV,aW) <- validWidth a
  let v = aV && aW > m && m >= l && l >= 0
      w = m - l + 1
  db <- get
  put $ db { rtlGraph = Graph.insEdge (a,i,0) $ Graph.insNode (i, Info (v,w) name (Select m l)) $ rtlGraph db }
  return i

cat :: Maybe String -> Node -> Node -> RTL Node
cat name a b = binaryRator Concat (\ _ _ -> True) (+) name a b

inv :: Maybe String -> Node -> RTL Node
inv name a = do
  i <- nextId
  (aV,aW) <- validWidth a
  db <- get
  put $ db { rtlGraph = Graph.insEdge (a,i,0) $ Graph.insNode (i, Info (aV,aW) name Not) $ rtlGraph db }
  return i

reg :: String -> Int -> Integer -> RTL Node
reg name w v = do
  i <- nextId
  enable <- false
  db <- get
  put $ db
    { rtlGraph = Graph.insEdges [(enable,i,0),(i,i,1)] $ Graph.insNode (i, Info (w > 0, w) (Just name) (Reg v)) $ rtlGraph db
    , rtlRegs  = i : rtlRegs db
    }
  return i

input :: String -> Int -> RTL Node
input n w = do
  i <- nextId
  db <- get
  put $ db
    { rtlGraph  = Graph.insNode (i, Info (w > 0, w) (Just n) Input) $ rtlGraph db
    , rtlInputs = i : rtlInputs db
    }
  return i

output :: String -> Int -> RTL ()
output n a = do
  i <- nextId
  (v,w) <- validWidth a
  db <- get
  put $ db
    { rtlGraph   = Graph.insEdge (a,i,0) $ Graph.insNode (i, Info (v,w) (Just n) Output) $ rtlGraph db
    , rtlOutputs = i : rtlOutputs db
    }

display :: Node -> String -> [Node] -> Bool -> RTL Node
display enable text args finish = do
  i <- nextId
  (enableV, enableW) <- validWidth enable
  argsV <- mapM valid args
  let v = enableV && enableW == 1 && and argsV 
      argEdges = map (\ (n,i') -> (n,i,i')) $ zip args [1..]
  db <- get
  put $ db
    { rtlGraph    = Graph.insEdges ((enable,i,0):argEdges) $ Graph.insNode (i, Info (v,0) Nothing (Display text finish)) $ rtlGraph db
    , rtlDisplays = i : rtlDisplays db
    }
  return i


