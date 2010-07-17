module PIC.Simulator
  ( simulate
  ) where

import Data.Bits
import Data.IntMap as Map hiding ( null, filter )

import PIC.Assemble
import PIC.Configuration
import Utils

-- | Execution violations.
data Violation
  = NoMoreInstructions
  | InvalidGOTO
  | InvalidSkip
  deriving Show

-- | The state of the machine.
data State = State
  { completedCycles :: Int
  , prevInstrs
  , nextInstrs :: [Instruction]
  , regFile    :: Map.IntMap Int
  , regW       :: Int
  , remainingAssertions :: [(String,[Instruction])]
  , assertionViolations :: Bool
  }

instance Show State where
  show state = show (head (nextInstrs state)) ++
    "  W=" ++ show (regW state) ++
    "  Z=" ++ (if testBit (regFile state ! status) zero  then "1" else "0") ++
    "  C=" ++ (if testBit (regFile state ! status) carry then "1" else "0") ++
    "  " ++ show (regFile state) ++ "\n"

simulate :: String -> Maybe Int -> ProgramAbs -> IO Bool
simulate name cycles program = do
  result <- run name config cycles (initState dMD program)
  case result of
    Left NoMoreInstructions -> do
      putStrLn "ERROR: Program ran out of instructions to execute."
      return False
    Left InvalidGOTO -> do
      putStrLn $ "ERROR: Program encountered an invalid GOTO address."
      return False
    Left InvalidSkip -> do
      putStrLn $ "ERROR: Program encountered an invalid skip instruction."
      return False
    Right state ->
      case (completedCycles state, assertionViolations state, remainingAssertions state) of
        (_,False,[]) -> return True
        (_,True,[]) -> do
          putStrLn $ "ERROR: Simulation " ++ name ++ " failed with assertion violations."
          return False
        (_,False,remaining) -> do
          putStrLn $ "ERROR: Simulation " ++ name ++ " failed to check the following assertions:"
          mapM_ (\ (n,_) -> putStrLn $ "  " ++ n) remaining
          return False
        (_,True,remaining) -> do
          putStrLn $ "ERROR: Simulation " ++ name ++ " failed with assertion violations and failed to check the following assertions:"
          mapM_ (\ (n,_) -> putStrLn $ "  " ++ n) remaining
          return False
  where
  dMD = dataMemoryDepth config
  config = basicConfig

initState :: Int -> ProgramAbs -> State
initState regFileDepth program = State
  { completedCycles = 0
  , prevInstrs = []
  , nextInstrs = instrs
  , regFile = initRegFile regFileDepth
  , regW = 0
  , remainingAssertions = assertions instrs
  , assertionViolations = False
  }
  where
  ProgramRel instrs = relative program
  assertions :: [Instruction] -> [(String,[Instruction])]
  assertions [] = []
  assertions i@(ASSERT name _ : is) = (name,i) : assertions is
  assertions (_:is) = assertions is

initRegFile :: Int -> Map.IntMap Int
initRegFile depth = foldl (\ m i -> Map.insert i 0 m) Map.empty [0 .. depth - 1]

run :: String -> Configuration -> Maybe Int -> State -> IO (Either Violation State)
run _ _ (Just cycles) state | cycles <= 0                      = return $ Right state
run n config (Just cycles) state = do stepResult <- step n config state
                                      case stepResult of
                                        Left violation -> return (Left violation)
                                        Right s        -> run n config (Just $ cycles - 1) s
run _ _      Nothing state | null (remainingAssertions state) = return $ Right state
run n config Nothing state                                    = do stepResult <- step n config state
                                                                   case stepResult of
                                                                     Left violation -> return (Left violation)
                                                                     Right s        -> run n config Nothing s


step :: String -> Configuration -> State -> IO (Either Violation State)
step _ _ (State { nextInstrs = [] }) = return $ Left NoMoreInstructions
step name config state' = newState
  where
  state = state'
  newState = case current of
    ADDLW k          -> carryLW (w + k)
    ADDWF f d        -> carryWF f d (w +)
    ANDLW k          -> basicLW (w .&. k)
    ANDWF f d        -> basicWF f d (w .&.)
    ASSERT n f       -> if testBit (regFileMap ! f) 0
                          then do
                            putStrLn $ "Assertion Passed : " ++ name ++ "." ++ n
                            return $ Right $ nextState'
                          else do
                            putStrLn $ "ERROR: Assertion Failed : " ++ name ++ "." ++ n
                            return $ Right $ nextState' { assertionViolations = True }
      where
      nextState' = nextState { remainingAssertions = remAsserts }
      remAsserts = filter isNotAssert (remainingAssertions state)
      isNotAssert (_,i) = i /= (current : next)
  
    BSF f b          -> return $ Right $ nextState { regFile = adjust (\ v -> setBit v b) f regFileMap }
    BTFSC f b        -> if not bitSet && null next then return $ Left $ InvalidSkip else return $ Right $ nextState'
      where
      bitSet = testBit (regFileMap ! f) b
      nextState' = if bitSet then nextState else nextStateSkip
  
    BTFSS f b        -> if bitSet && null next then return $ Left $ InvalidSkip else return $ Right $ nextState'
      where
      bitSet = testBit (regFileMap ! f) b
      nextState' = if bitSet then nextStateSkip else nextState
  
    CLRF f           -> basicWF f F (\ _ -> 0)
    COMF f d         -> basicWF f d complement
    GOTO k | k == 0                       -> return $ Right $ nextState { prevInstrs = prev, nextInstrs = current : next }
    GOTO k | k < 0 && abs k > length prev -> return $ Left InvalidGOTO
    GOTO k | k > 0 && k - 1 > length next -> return $ Left InvalidGOTO
    GOTO k | k < 0                        -> return $ Right $ nextState { prevInstrs = drop (abs k) prev, nextInstrs = reverse (take (abs k) prev) ++ current : next }
    GOTO k                                -> return $ Right $ nextState { prevInstrs = reverse (take (k - 1) next) ++ current : prev, nextInstrs = drop (k - 1) next }
    INCFSZ f d       ->
      if v == 0 && null next then return $ Left $ InvalidSkip else return $ Right $ nextState2
      where
      v = mask ((regFileMap ! f) + 1)
      nextState1 = if v == 0 then nextStateSkip else nextState
      nextState2 = case d of
        W -> nextState1 { regW = v }
        F -> nextState2 { regFile = insert f v regFileMap }

    IORWF f d        -> basicWF f d (w .|.)
    MOVLW k          -> return $ Right $ nextState { regW = k }
    MOVF f d         -> basicWF f d id
    MOVWF f          -> return $ Right $ nextState { regFile = insert f w regFileMap }
    NOP              -> return $ Right $ nextState
    SUBWF f d        -> carryWF f d (\ r -> r - w) 
    XORWF f d        -> basicWF f d (xor w)
  
  prev = prevInstrs state
  (current : next) = nextInstrs state

  w = regW state

  regFileMap = regFile state

  nextState = state
    { completedCycles = completedCycles state + 1
    , prevInstrs      = current : prev
    , nextInstrs      = next
    }

  nextStateSkip = nextState { prevInstrs = head next : current : prev, nextInstrs = tail next }

  dW = dataWidth config

  mask i = i .&. (power 2 dW - 1)

  zeroStatus :: Int -> (Int,IntMap Int)
  zeroStatus v = (v',insert status ((if v' == 0 then setBit else clearBit) (regFileMap ! status) zero) regFileMap)
    where
    v' = mask v

  zeroCarryStatus :: Int -> (Int,IntMap Int)
  zeroCarryStatus v = (v', regFileMap'')
    where
    (v',regFileMap') = zeroStatus v
    regFileMap'' = insert status ((if testBit v dW then setBit else clearBit) (regFileMap' ! status) carry) regFileMap

  basicLW :: Int -> IO (Either Violation State)
  basicLW v = return $ Right $ nextState { regW = v', regFile = regFileMap' }
    where
    (v',regFileMap') = zeroStatus v

  basicWF :: Int -> D -> (Int -> Int) -> IO (Either Violation State)
  basicWF reg W f = return $ Right $ nextState { regW = v', regFile = regFileMap' }
    where
    (v',regFileMap') = zeroStatus (f (regFileMap ! reg))

  basicWF reg F f = return $ Right $ nextState { regFile = regFileMap'' }
    where
    (v',regFileMap') = zeroStatus (f (regFileMap ! reg))
    regFileMap'' = insert reg v' regFileMap'

  carryLW :: Int -> IO (Either Violation State)
  carryLW v = return $ Right $ nextState { regW = v', regFile = regFileMap' }
    where
    (v',regFileMap') = zeroCarryStatus v

  carryWF :: Int -> D -> (Int -> Int) -> IO (Either Violation State)
  carryWF reg W f = return $ Right $ nextState { regW = v', regFile = regFileMap' }
    where
    (v',regFileMap') = zeroCarryStatus (f (regFileMap ! reg))

  carryWF reg F f = return $ Right $ nextState { regFile = regFileMap'' }
    where
    (v',regFileMap') = zeroCarryStatus (f (regFileMap ! reg))
    regFileMap'' = insert reg v' regFileMap'


status = 3
carry  = 0
zero   = 2

