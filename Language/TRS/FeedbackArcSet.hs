-- | Heuristic optimizations for the Feedback Arc (Edge) Set problem.
--
--   Scheduling global, linear, fixed priority rules is equivalent to
--   minimizing the number of edges to remove to for a acyclic rule data dependency graph.

module Language.TRS.FeedbackArcSet
  ( optimize
  , Edge      (..)
  ) where

import qualified Data.Set as Set
import System.IO
import System.Random

-- | An 'Edge' connects an upstream and downstream vertix pair.
data Edge a
  = Soft a a   -- ^ A soft edge constraint may be removed from the scheduling graph.
  | Hard a a   -- ^ A hard edge constraint must not be removed from the graph.
  deriving (Show, Eq, Ord) 

-- | Optimizes the vertice order by minimizing the
--   number of soft edges removed to form an acyclic graph.
optimize :: (Show a, Ord a) => Set.Set a -> Set.Set (Edge a) -> IO (Either (Set.Set a) ([a],String))
optimize vertices edges' = do
  putStr "Attempting to satisfy hard constraints..."
  hFlush stdout
  if not $ Set.null remaining
    then do
      putStrLn "failed"
      return $ Left remaining
    else do
      putStrLn "success"
      finalOrder <- annealing hardEdges softEdges initOrder
      return $ Right (finalOrder, doc finalOrder softEdges)
  where
  edges = Set.filter isNotLoopback edges'

  isNotLoopback (Hard a b) | a == b = False
  isNotLoopback (Soft a b) | a == b = False
  isNotLoopback _ = True

  hardVertices = Set.fold addHard Set.empty edges
  addHard (Hard a b) s = Set.insert a $ Set.insert b s
  addHard _ s = s

  softVertices = vertices Set.\\ hardVertices

  hardEdges = Set.map extractVertices $ Set.filter isHard edges
  softEdges = Set.filter isNotRelatedToHardEdge $ Set.map extractVertices $ Set.filter (not . isHard) edges
  isHard (Hard _ _) = True
  isHard (Soft _ _) = False
  extractVertices (Hard a b) = (a,b)
  extractVertices (Soft a b) = (a,b)
  isNotRelatedToHardEdge (a,b) = not $ Set.member (a,b) hardEdges || Set.member (b,a) hardEdges

  hardRoots = Set.fold removeHard hardVertices hardEdges
  removeHard (_, a) s = Set.delete a s

  hardUpstreams v = Set.map fst $ Set.filter (\ (_,b) -> v == b) hardEdges  --XXX Make a Map to improve performance.

  (remaining, _, hardOrder) = topo (hardVertices Set.\\ hardRoots, hardRoots, [hardRoots])

  topo (remaining, computed, order) =
    if Set.null next
      then (remaining, computed, reverse order)
      else topo (remaining Set.\\ next, Set.union computed next, next : order)
    where
    next = Set.filter (\ v -> Set.isSubsetOf (hardUpstreams v) computed) remaining

  initOrder = concatMap Set.toList hardOrder ++ Set.toList softVertices  -- XXX

cost :: Ord a => Set.Set (a,a) -> [a] -> Int
cost softEdges vertices = c
  where
  (c,_) = foldl f (0,[]) vertices
  f (cost,higher) vertex = (foldl f' cost higher, vertex:higher)
    where
    f' cost vertexHigher | Set.member (vertex, vertexHigher) softEdges = cost + 1
    f' cost _ = cost

mutate :: Ord a => Set.Set (a,a) -> Int -> [a] -> Maybe [a]
mutate hardEdges index vertices = case splitAt index vertices of
  (a, b0:b1:b2) | not (Set.member (b0,b1) hardEdges) -> Just $ a ++ (b1:b0:b2)
  _ -> Nothing

annealing :: Ord a => Set.Set (a,a) -> Set.Set (a,a) -> [a] -> IO [a]
annealing hardEdges softEdges initOrder = do
  putStrLn "Starting vertex order optimization..."
  hFlush stdout
  putStrLn $ "  iteration 0 cost: " ++ show initCost
  hFlush stdout
  (order, cost) <- if initCost == 0 then return init else iterate 1 random init init
  putStrLn $ "  final cost: " ++ show cost
  hFlush stdout
  return order
  where
  random :: [Int]
  random = randomRs (0, length initOrder - 2) (mkStdGen 0)
  initCost = cost softEdges initOrder
  init = (initOrder,initCost)
  totalIterations = 20

  iterate count _ _ best | count >= totalIterations = return best
  iterate count index (currOrder, _) (bestOrder,bestCost) = do
    putStrLn $ "  iteration " ++ show count ++ " cost: " ++ show cost'
    hFlush stdout
    if cost' == 0
      then return (order', cost')
      else iterate (count + 1) (tail index) (order',cost') (if cost' < bestCost then (order', cost') else (bestOrder, bestCost))
    where
    order' = case mutate hardEdges (head index) currOrder of
      Nothing -> currOrder
      Just v  -> v
    cost' = cost softEdges order' 
    
doc :: Show a => [a] -> Set.Set (a,a) -> String
doc order _ = unlines names
  where
  names' = map show order
  maxLength = maximum $ map length names'
  names = map (\n -> n ++ replicate (maxLength - length n) ' ') names'

