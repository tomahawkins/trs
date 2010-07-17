-- | An interface to the MiniSat SAT solver.

module Language.TRS.SAT
  ( Formula (Var, Const, Not, And, Or)
  , sat
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import System.Cmd
import System.IO
import System.Process

import Language.TRS.Utils

data Formula a
  = Var a
  | Const Bool
  | Label (Formula a)
  | Not (Formula a)
  | And (Formula a) (Formula a)
  | Or  (Formula a) (Formula a) deriving (Ord, Eq)

sat :: Ord a => Formula a -> IO (Maybe [a])
sat f = case cnf of
  Const True  -> return $ Just []
  Const False -> return Nothing
  _ -> do
    h <- openFile "minisat_input" WriteMode
    formatCNF h cnf
    hClose h
    (i,o,e,p) <- runInteractiveCommand "minisat2 minisat_input minisat_output"
    waitForProcess p
    hClose i
    hClose o
    hClose e
    r <- readFileNow "minisat_output"
    system "rm minisat_input minisat_output"
    return $ results r
  where
  cnf = toCNF f
  (vars, labels) = variables cnf
  varNum = Set.size vars
  varIds = Map.fromList $ zip (Set.toList vars) [1 .. varNum]
  idVars = Map.fromList $ zip [1 .. varNum] (Set.toList vars)
  labIds = Map.fromList $ zip (Set.toList labels) [varNum + 1 .. varNum + Set.size labels]

  results s = if l !! 0 == "UNSAT" then Nothing else Just $ mapMaybe f w
    where
    l = lines s
    w = init $ words $ l !! 1
    f n = if n' < 0 || n' > varNum then Nothing else Just $ idVars Map.! n'
      where
      n' :: Int
      n' = read n

  formatCNF h f = ands f
    where
    ands (And a b) = ands a >> ands b
    ands f = ors f >> hPutStrLn h " 0"

    ors (Or a b)        = ors a >> hPutStr h " " >> ors b
    ors (Var a)         = hPutStr h (show (varIds Map.! a))
    ors (Not (Var a))   = hPutStr h ("-" ++ show (varIds Map.! a))
    ors (Label a)       = hPutStr h (show (labIds Map.! a))
    ors (Not (Label a)) = hPutStr h ("-" ++ show (labIds Map.! a))
    ors _               = error $ "Formula is not in CNF."
 
-- | Variables of 'Formula'.
variables :: Ord a => Formula a -> (Set.Set a, Set.Set (Formula a))
variables f = case f of
  Const _ -> (Set.empty,Set.empty)
  Var a   -> (Set.singleton a, Set.empty)
  Label a -> (Set.empty, Set.singleton a)
  Not a   -> variables a
  And a b -> (Set.union a1 b1, Set.union a2 b2)
    where
    (a1,a2) = variables a
    (b1,b2) = variables b
  Or a b  -> (Set.union a1 b1, Set.union a2 b2)
    where
    (a1,a2) = variables a
    (b1,b2) = variables b

-- | Converts a 'Formula' to CNF.
toCNF :: Ord a => Formula a -> Formula a
toCNF f = if Set.null supportCNF then f' else And f' support
  where
  (f', supportCNF) = tseitin $ buryNot $ constProp f
  support = foldl1 And $ map (foldl1 Or . Set.toList) $ Set.toList supportCNF

-- | Labeled conversion to CNF.  (newFormula, cnfAccumulation)
tseitin :: Ord a => Formula a -> (Formula a, Set.Set (Set.Set (Formula a)))
tseitin f = case f of
  Const _       -> (f, Set.empty)
  Var _         -> (f, Set.empty)
  Label _       -> (f, Set.empty)
  Not (Var _)   -> (f, Set.empty)
  Not (Label _) -> (f, Set.empty)
  Not _         -> error "tseitin: constProp or buryNot was not applied before tseitin."
  And a b       -> (x, Set.union xSet (Set.union aSet bSet))
    where
    (a',aSet) = tseitin a
    (b',bSet) = tseitin b
    x = Label $ And a' b'
    xSet = Set.fromList
      [ Set.fromList [Not x, a']
      , Set.fromList [Not x, b']
      , Set.fromList [x, buryNot (Not a'), buryNot (Not b')]
      ]
  Or a b        -> (x, Set.union xSet (Set.union aSet bSet))
    where
    (a',aSet) = tseitin a
    (b',bSet) = tseitin b
    x = Label $ Or a' b'
    xSet = Set.fromList
      [ Set.fromList [x, buryNot (Not a')]
      , Set.fromList [x, buryNot (Not b')]
      , Set.fromList [Not x, a', b']
      ]


-- | Pushes negations down to variables.
buryNot :: Formula a -> Formula a
buryNot f = case f of
  Var i -> Var i
  Const a -> Const a
  Label a -> Label a
  Not (Var a) -> Not (Var a)
  Not (Const a) -> Not (Const a)
  Not (Label a) -> Not (Label a)
  Not (Not a) -> buryNot a
  And a b -> And (buryNot a) (buryNot b)
  Or  a b -> Or  (buryNot a) (buryNot b)
  Not (And a b) -> Or  (buryNot (Not a)) (buryNot (Not b))
  Not (Or  a b) -> And (buryNot (Not a)) (buryNot (Not b))

-- | Constant propagation.
constProp :: Formula a -> Formula a
constProp f = case f of
  Var a -> Var a
  Const a -> Const a
  Label a -> Label a
  Not a -> case constProp a of
    (Const a) -> Const $ not a
    a' -> Not a'
  And a b -> case (constProp a, constProp b) of
    (Const True, b') -> b'
    (Const False, _) -> Const False
    (a', Const True) -> a'
    (_, Const False) -> Const False
    (a',b') -> And a' b'
  Or a b -> case (constProp a, constProp b) of
    (Const True, _)   -> Const True
    (Const False, b') ->  b'
    (_,  Const True)  -> Const True
    (a', Const False) ->  a'
    (a',b') -> Or a' b'

