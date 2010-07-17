-- | TRS rule scheduling.

module Language.TRS.Scheduling ( schedule ) where

import Control.Monad
import qualified Data.List as List
import Data.Maybe
import qualified Data.Set as Set
import System.IO

import Language.TRS.Elaboration
import Language.TRS.Language
import Language.TRS.Logic hiding (false)
import Language.TRS.FeedbackArcSet
import qualified Language.TRS.SAT as SAT

-- | Schedules the 'Rule's of a 'SystemDB' and returns the enabling and data control for each 'Reg' and each display.
schedule :: SystemDB -> IO (Maybe (String, [(Reg,(Signal,Signal))], [(Signal,String,[Signal],Bool)]))
schedule db = do
  putStrLn "Starting rule scheduling optimization..."
  hFlush stdout
  results <- optimizePriority $ initialSchedule $ sysRules db
  case results of
    Nothing -> return Nothing
    Just (schedule, doc) -> do
      let (assigns, displays) = buildRuleLogic (sysRegs db) schedule []
      return $ Just (doc, assigns, displays)

type Schedule = [(Rule,[Rule])]

-- | Rule scheduling (priority) optimization.
optimizePriority :: Schedule -> IO (Maybe (Schedule, String))
optimizePriority rules = do
  reducedSoftEdges <- reduceEdges allSoftEdges
  ruleOrder <- optimize (Set.fromList $ map fst rules) $ Set.unions
    [ reducedSoftEdges
    , priorityEdges
    , activeWhenEnabledEdges
    ]
  case ruleOrder of
    Left rules  -> do
      putStrLn "ERROR: Following rules form a circular constraint:"
      mapM_ (\ rule -> putStrLn $ "  " ++ ruleName rule) $ Set.toList rules
      return Nothing
    Right (rules, doc) -> return $ Just (map (groupDownstreams (allSoftEdges Set.\\ reducedSoftEdges)) rules, doc)
  where
  groupDownstreams :: Set.Set (Edge Rule) -> Rule -> (Rule,[Rule])
  groupDownstreams unneededEdges r = case lookup r rules of
    Just ds -> (r, filter (\ r' -> Set.notMember (Soft r r') unneededEdges) ds) where
    Nothing -> error "optimizePriority: Rule not found."

  ruleRelations :: [(Rule,Relation)]
  ruleRelations = concatMap (\ (r,_) -> zip (repeat r) (actRelations (ruleAction r))) rules

  allSoftEdges :: Set.Set (Edge Rule)
  allSoftEdges = Set.fromList $ concatMap (\ (from,rs) -> map (\ to -> Soft from to) rs) rules

  priorityEdges :: Set.Set (Edge Rule)
  priorityEdges = Set.fromList $ f ruleRelations
    where
    f :: [(Rule,Relation)] -> [Edge Rule]
    f [] = []
    f ((rule, relation):relations) = case relation of
      Higher i -> map (\ r -> Hard rule r) (fst $ unzip $ filter (\ (_,rel) -> rel == Lower  i) relations) ++ f relations
      Lower  i -> map (\ r -> Hard r rule) (fst $ unzip $ filter (\ (_,rel) -> rel == Higher i) relations) ++ f relations

  activeWhenEnabledEdges :: Set.Set (Edge Rule)
  activeWhenEnabledEdges = Set.fromList $ concatMap f rules
    where
    f :: (Rule,[Rule]) -> [Edge Rule]
    f (r,_) = if actActiveWhenEnabled (ruleAction r) then mapMaybe f' $ Set.toList allSoftEdges else []
      where
      f' :: Edge Rule -> Maybe (Edge Rule)
      f' (Soft a b) | b == r = Just (Hard r a)
      f' _ = Nothing

-- | Builds the enable and control logic for all 'Reg's and displays for a given 'Schedule'.
buildRuleLogic :: [Reg] -> Schedule -> [(Rule, Signal)] -> ([(Reg,(Signal,Signal))], [(Signal,String,[Signal],Bool)])
buildRuleLogic regs [] _ = (map (\ r -> (r, (false, value r))) regs, [])
buildRuleLogic regs ((rule, ds) : lower) higher = (regEnableData, activeDisplays)
  where
  -- Active (real enable) for rule.
  active :: Signal
  active = case exe of
    Concurrent -> foldl (\ c1 c2 -> c1 &. inv c2)           [ruleEnable rule]  $ mapMaybe (flip lookup higher) ds
    Sequential -> foldl (\ c1 c2 -> c1 &. inv c2) (redirect [ruleEnable rule]) $ mapMaybe (flip lookup higher) ds

  -- Regs, actives, and data signals.
  regEnableData = foldl updateRegs regAssigns $ actAssigns $ ruleAction rule

  -- Display actives, strings, and signals.
  activeDisplays :: [(Signal,String,[Signal],Bool)]
  activeDisplays = case exe of
    Concurrent -> map (\ (string, signals, finish) -> (active, string,              signals, finish)) displays ++ lowerDisplays
    Sequential -> map (\ (string, signals, finish) -> (active, string, map redirect signals, finish)) displays ++ lowerDisplays

  exe = actExecution $ ruleAction rule
  displays :: [(String,[Signal],Bool)]
  displays = actDisplays $ ruleAction rule

  -- Register actives and data for lower priority rules.
  regAssigns :: [(Reg,(Signal,Signal))]
  lowerDisplays :: [(Signal,String,[Signal],Bool)]
  (regAssigns, lowerDisplays) = buildRuleLogic regs lower $ case exe of
    Concurrent -> (rule,active) : higher
    Sequential -> higher   -- Sequential rules do not disable lower priority rules.

  -- Fetches the active and data from regs.
  activeData :: Reg -> (Signal,Signal)
  activeData reg = case lookup reg regAssigns of
    Nothing -> error "activeData: Register not found."
    Just ed -> ed

  -- Updates registers active and data signals.
  updateRegs :: [(Reg,(Signal,Signal))] -> (Reg,Signal) -> [(Reg,(Signal,Signal))]
  updateRegs regAssigns' (reg,signal) = case exe of
    Concurrent -> replace (reg, (e |. active, mux active           signal  d)) regAssigns'
    Sequential -> replace (reg, (e |. active, mux active (redirect signal) d)) regAssigns'
    where (e,d) = activeData reg

  -- Redirects signals from registers to the schedule datapath for sequential rules.
  redirect :: Signal -> Signal
  redirect = concatMap redirectBit

  redirectBit :: Bit -> Signal
  redirectBit s = case s of
    Not a        -> inv $ redirectBit a
    And a b      -> redirectBit a &. redirectBit b
    Or  a b      -> redirectBit a |. redirectBit b
    Const _      -> [s]
    BitInput _ _ -> [s]
    BitLabel (Label _ _ s) i -> select (redirect s) i i
    BitReg reg i -> case lookup reg regAssigns of
      Nothing -> error "redirect: Register not found."
      Just (_,d) -> select d i i

-- Replaces an element in an association list.
replace :: Eq a => (a,b) -> [(a,b)] -> [(a,b)]
replace (a,b) [] = [(a,b)]
replace (a,b) ((a',_):c) | a == a' = (a,b):c
replace (a,b) (h:c)                = h : replace (a,b) c


-- | Creates the initial rule priority 'Schedule'.
initialSchedule :: [Rule] -> Schedule
initialSchedule rules = map (downstreamRulesOfRule rules) rules

downstreamRulesOfRule :: [Rule] -> Rule -> (Rule,[Rule])
downstreamRulesOfRule rules rule = (rule, mapMaybe isDownstreamRule (upstreamRules rules))
  where
  isDownstreamRule :: ([Rule],Rule) -> Maybe Rule
  isDownstreamRule (rules, r) = if elem rule rules then Just r else Nothing

upstreamRules rules = map (upstreamRulesOfRule rules) rules

upstreamRulesOfRule :: [Rule] -> Rule -> ([Rule],Rule)
upstreamRulesOfRule rules rule = (upstreamRules, rule)
  where
  exe = actExecution $ ruleAction rule
  upstreamRules = case exe of { Sequential -> [] ; Concurrent -> filter isUpstreamRule rules }
  regs = upstreamRegsOfRule rule
  isUpstreamRule :: Rule -> Bool
  isUpstreamRule r | r == rule = False
  isUpstreamRule rule = not $ Set.null $ Set.intersection regs regs'
    where
    assigns = actAssigns $ ruleAction rule
    regs' = Set.fromList $ fst $ unzip assigns

upstreamRegsOfRule :: Rule -> Set.Set Reg
upstreamRegsOfRule rule = regs
  where
  assigns = actAssigns $ ruleAction rule
  displays = actDisplays $ ruleAction rule
  signals = (concat $ (\ (_,d,_) -> d) $ unzip3 displays) ++ ([ruleEnable rule] : snd (unzip assigns))
  regs = foldl (\ regs sig -> Set.union regs $ upstreamRegsOfSignal sig) Set.empty signals

upstreamRegsOfSignal :: Signal -> Set.Set Reg
upstreamRegsOfSignal sig = Set.unions $ map upstreamRegsOfBit sig

upstreamRegsOfBit :: Bit -> Set.Set Reg
upstreamRegsOfBit b = case b of
  Not a -> upstreamRegsOfBit a
  And a b -> Set.union (upstreamRegsOfBit a) (upstreamRegsOfBit b)
  Or  a b -> Set.union (upstreamRegsOfBit a) (upstreamRegsOfBit b)
  Const _ -> Set.empty
  BitInput _ _ -> Set.empty
  BitLabel (Label _ _ s) _ -> upstreamRegsOfSignal s
  BitReg r _ -> Set.singleton r

-- | Reduces number of soft 'Edge's in a graph by using SAT to search for mutually excusive rules.
reduceEdges :: Set.Set (Edge Rule) -> IO (Set.Set (Edge Rule))
reduceEdges edges = do
  putStrLn "Starting rule mutual exclusion analysis..."
  hFlush stdout
  inclusiveRules <- filterM compareRules $ Set.toList ruleRelations
  return $ Set.filter (realEdge $ Set.fromList inclusiveRules) edges
  where
  ruleRelations :: Set.Set (Rule,Rule)
  ruleRelations = Set.fold insertEdge Set.empty edges
  insertEdge :: Edge Rule -> Set.Set (Rule,Rule) -> Set.Set (Rule,Rule)
  insertEdge (Hard _ _) s = s
  insertEdge (Soft a b) s = Set.insert (min a b, max a b) s
  compareRules :: (Rule,Rule) -> IO Bool
  compareRules (a,b) = do
    putStr $ "  checking: " ++ ruleName a ++ " && " ++ ruleName b ++ " == "
    hFlush stdout
    result <- sat 20 (head ([ruleEnable a] &. [ruleEnable b])) -- SAT with maxDepth.
    case result of
      Yes     -> putStrLn "true"  >> hFlush stdout >> return True
      No      -> putStrLn "false" >> hFlush stdout >> return False
      Unknown -> putStrLn "???"   >> hFlush stdout >> return True

  realEdge :: Set.Set (Rule,Rule) -> Edge Rule -> Bool
  realEdge _ (Hard _ _) = True
  realEdge s (Soft a b) = Set.member (min a b, max a b) s

data YesNo = Yes | No | Unknown

-- | Pruning SAT procedure.
sat :: Int -> Bit -> IO YesNo
sat maxDepth s = sat' 1 $ formula 0 s
  where
  sat' :: Int -> SAT.Formula Bit -> IO YesNo
  sat' i f = if f == f' then return Yes else if i > maxDepth then return Unknown else do
    result <- SAT.sat f'
    case result of
      Nothing -> return No
      Just _  -> sat' (i+1) f'
    where
    f' = formula i s

formula :: Int -> Bit -> SAT.Formula Bit
formula d s | d <= 0 = SAT.Var s
formula d s = case s of
  Not a   -> SAT.Not (formula  d    a)
  And a b -> SAT.And (formula (d-1) a) (formula (d-1) b)
  Or  a b -> SAT.Or  (formula (d-1) a) (formula (d-1) b)
  Const v -> SAT.Const v
  BitLabel  (Label _ _ s) m -> formula d (head $ select s m m)
  BitInput  _ _ -> SAT.Var s
  BitReg    _ _ -> SAT.Var s

