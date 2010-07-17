-- | The core TRS language.

module Language.TRS.Language
  (
  -- * Namespace Control
    scope
  -- * Rule Declarations and Operations
  , rule
  , rules
  -- * Action Directives
  , System
  , Action
  , when
  , doif
  , display
  , finish
  , sequenceActions
  , loopActions
  -- ** Performance Constraints
  , sequential
  , priority
  , alwaysActiveWhenEnabled
  -- * Top Level IO
  , input
  , inputs
  , output
  , outputs
  -- * State Element Declarations and Operations
  , Reg
  , reg
  , regs
  , (<==)
  , value
  , incrReg
  , decrReg
  -- * Combinational Signal expressions
  , Signal
  -- ** Signal Labeling
  , label
  , labels
  -- ** Constants Declarations
  , constant
  -- *** Common Constants
  , true
  , false
  , zero
  , one
  , ones
  -- ** Arithmetic Operations
  , (+.)
  , (-.)
  -- ** Signalwise Logical Operations
  , inv
  , (&.)
  , (^.)
  , (|.)
  , imply
  , equiv
  -- ** Signal Selection
  , select
  , msb
  , lsb
  , msbs
  , lsbs
  , bits
  -- ** Equality Operations
  , (==.)
  , (/=.)
  -- ** Relational Operations
  , (<.)
  , (>.)
  , (<=.)
  , (>=.)
  -- ** Multiplexing
  , mux
  {-
  -- * Verification Directives
  , assert
  , assume
  , cover
  -- * Sequences
  , ($$)
  , ($:)
  , ($|)
  , ($&)
  , ($&&)
  , within
  , star
  , plus
  {-
  , anyN
  , anyNtoM
  , anyNtoInf
  , repeatStar
  , repeatPlus
  , repeatN
  , repeatNtoM
  , repeatNtoInf
  -}
  -- * Properties
  , weak
  , notP
  , (&&&)
  , (|||)
  , (-->)
  , always
  , never
  , (|->)
  , (|=>)
  -}
  -- * Utilities
  , lift
  , liftIO
  , bitsRequired
  , tree
  , Width
  , Depth
  , width
  ) where

import Control.Monad.State hiding (when)
import Control.Monad.Trans

import Language.TRS.Elaboration
import Language.TRS.Logic hiding (true, false)

infixl 6 +., -.
--infixr 5 ++.
infix  4 ==., /=., <., <=., >., >=.
infixl 3 &., ^. -- , &&&, $&, $&&
infixl 2 |. -- , |||, $$, $:, $|
infixr 1 <== -- , |->, |=>, -->

-- | A 1-bit 1.
true :: Signal
true = constant 1 1

-- | A 1-bit 0.
false :: Signal
false = constant 1 0

-- | A value zero of a given width.
zero :: Int -> Signal
zero w = constant w 0

-- | A value one of a given width.
one :: Int -> Signal
one w = constant w 1

-- | A value with all bits 1.
ones :: Int -> Signal
ones w = constant w (-1)


-- | Creates a new namespace with an enable condition for all sub rules and verification directives.
--
-- > scope "newScope" enableCondition system
scope :: Name -> Signal -> System a -> System a
scope name enable system = do
  db <- get
  put $ db { sysName = sysName db ++ name ++ "_", sysEnable = head ([sysEnable db] &. enable) }
  a <- system
  db' <- get
  put $ db' { sysName = sysName db, sysEnable = sysEnable db }
  return a

-- | Labels a 'Signal' with a name.
label :: Name -> Signal -> System Signal
label name s = do
  name <- fullName name
  i <- nextId
  db <- get
  let l = Label i name s
  put $ db { sysLabels = l : sysLabels db }
  return $ map (BitLabel l) $ reverse [0 .. width s - 1]

-- | Labels a list of 'Signal's with a name.
labels :: Name -> [Signal] -> System [Signal]
labels name sigs = mapM (\ (i,s) -> label (name ++ show i) s) $ zip [0..] sigs

fullName :: Name -> System Name
fullName name = do
  db <- get
  let name' = sysName db ++ name
  put $ db { sysNames = name' : sysNames db }
  return name'

nextId :: System Int
nextId = do
  db <- get
  put $ db { sysNextId = sysNextId db + 1 }
  return $ sysNextId db

-- | Addition.
(+.) :: Signal -> Signal -> Signal
(+.) a b | width a /= width b = error "(+.): Signals do not have same width."
(+.) a b = x where (x,_) = fullAdd (bits a) (bits b)

fullAdd :: [Signal] -> [Signal] -> (Signal,Signal)
fullAdd (a:as) (b:bs) = ((a ^. b ^. c) ++ x, ((a ^. b) &. c) |. (a &. b)) where (x,c) = fullAdd as bs
fullAdd _ _ = ([],false)

-- | Subtraction.
(-.) :: Signal -> Signal -> Signal
(-.) a b | width a /= width b = error "(-.): Signals do not have same width."
(-.) a b = x where (x,_) = fullSub (bits a) (bits b)

fullSub :: [Signal] -> [Signal] -> (Signal,Signal)
fullSub (a:as) (b:bs) = ((a ^. b ^. c) ++ x, (inv (a ^. b) &. c) |. (inv a &. b)) where (x,c) = fullSub as bs
fullSub _ _ = ([],false)

-- | Signalwise NOT.
inv :: Signal -> Signal
inv = map not_

-- | Signalwise AND.
(&.) :: Signal -> Signal -> Signal
a &. b | width a /= width b = error "(&.): Signals do not have same width."
a &. b = map (\ (a,b) -> and_ a b) $ zip a b

-- | Signalwise OR.
(|.) :: Signal -> Signal -> Signal
a |. b | width a /= width b = error "(|.): Signals do not have same width."
a |. b = map (\ (a,b) -> or_ a b) $ zip a b

-- | Signalwise XOR.
(^.) :: Signal -> Signal -> Signal
(^.) a b = (a &. inv b) |. (inv a &. b)

-- | Implication.
imply :: Signal -> Signal -> Signal
imply a b = inv a |. b

-- | Equivalence.
equiv :: Signal -> Signal -> Signal
equiv a b = inv (a ^. b)

-- | Signal range select.
--
-- > bits bits msb lsb  -- Verilog equivalent: bits[msb:lsb]
select :: Signal -> Int -> Int -> Signal
select a msb lsb | msb >= width a || lsb < 0 = error "select: Invalid bit range."
select _ msb lsb | msb < lsb = []
select a msb lsb = take (msb - lsb + 1) $ drop (width a - msb - 1) a

-- | The most significant bit.
msb :: Signal -> Signal
msb a = [head a]

-- | The least significant bit.
lsb :: Signal -> Signal
lsb a = [last a]

-- | All bits except for the least significant bit.
msbs :: Signal -> Signal
msbs = init

-- | All bits except for the most significant bit.
lsbs :: Signal -> Signal
lsbs = tail

-- | All individual bits of a 'Signal'.
bits :: Signal -> [Signal]
bits [] = []
bits a = msb a : bits (lsbs a)


-- | A 2-input mux.
--
-- > mux ctrl onHigh onLow  -- Verilog equivalent:  ctrl ? onHigh : onLow
mux :: Signal -> Signal -> Signal -> Signal
mux c _ _ | width c /= 1 = error "mux: Control signal is not a single bit."
mux _ h l | width h /= width l = error "mux: Data do not have the same width."
mux c h l = concat $ map (\ (h,l) -> (c &. h) |. (inv c &. l)) $ zip (bits h) (bits l)


-- | Equality comparison.
(==.) :: Signal -> Signal -> Signal
(==.) a b | width a /= width b = error "(==.): Signals do not have same width."
(==.) a b = tree (&.) id $ map (\ (a,b) -> (a &. b) |. (inv (a |. b))) $ zip (bits a) (bits b)

-- | Inequality comparison.
(/=.) :: Signal -> Signal -> Signal
(/=.) a b = inv (a ==. b)

-- | Unsigned less-than comparison.
(<.) :: Signal -> Signal -> Signal
(<.) a b | width a /= width b = error "(<.): Signals do not have same width."
a <. b = c where (_,c) = fullSub (bits a) (bits b)

-- | Unsigned greater-than comparison.
(>.) :: Signal -> Signal -> Signal
a >. b = b <. a

-- | Unsigned less-than-or-equal comparison.
(<=.) :: Signal -> Signal -> Signal
a <=. b = inv (a >. b)

-- | Unsigned greater-than-or-equal comparison.
(>=.) :: Signal -> Signal -> Signal
a >=. b = inv (a <. b)


-- | General constant declaration.
--
-- > constant width value
constant :: Int -> Integer -> Signal
constant w v = reverse $ constant' w v

constant' w _ | w <= 0 = []
constant' w v = Const (mod v 2 == 1) : constant' (w - 1) (div v 2)

-- | Register declaration.
--
-- > reg "regName" width defaultValue
reg :: Name -> Width -> Integer -> System Reg
reg name width init = do
  if width <= 0
    then error "reg: Width can not be <= 0."
    else do
      name <- fullName name
      i <- nextId
      db <- get
      let r = Reg i name width init
      put $ db { sysRegs = r : sysRegs db }
      return r

-- | Declares a list of 'Reg's.
regs :: Name -> Width -> Depth -> Integer -> System [Reg]
regs name w d init = mapM (\ d -> reg (name ++ show d) w init) [0 .. d - 1]

-- | Increments a 'Reg'.
incrReg :: Reg -> Action ()
incrReg r = r <== value r +. one (width r)

-- | Decrements a 'Reg'.
decrReg :: Reg -> Action ()
decrReg r = r <== value r -. one (width r)


-- | Given a name and an 'Action', adds a transition rule to the 'System'.
--
-- > rule "ruleName" action
rule :: Name -> Action () -> System ()
rule name action = do
  act <- buildAction action
  db <- get
  enable <- label (name ++ "_enable") ([sysEnable db] &. [actEnable act])
  db <- get
  i <- nextId
  n <- fullName name
  let rule = Rule { ruleUID = i
                  , ruleName = n
                  , ruleEnable = head enable
                  , ruleAction = act }
  put $ db { sysRules = rule : sysRules db }

-- | Defines a rule for each action.
rules :: Name -> [Action ()] -> System ()
rules name actions = mapM_ (\ (a,n) -> rule (name ++ show n) a) $ zip actions [0..]

{-
-- | Asserts that a 'Property' must be true.
assert :: Prop a => Name -> a -> System ()
assert name property = do
  name' <- fullName name
  db <- get
  put $ db { sysVerifys = Assert name' (sysEnable db) (toProp property) : sysVerifys db }

-- | Assumes a 'Property' is true.
assume :: Prop a => Name -> a -> System ()
assume name property = do
  name' <- fullName name
  db <- get
  put $ db { sysVerifys = Assume name' (sysEnable db) (toProp property) : sysVerifys db }

-- | Declares a functional coverage 'Sequence'.
cover :: Seq a => Name -> a -> System ()
cover name s = do
  name' <- fullName name
  db <- get
  put $ db { sysVerifys = Cover name' (sysEnable db) (toSeq s) : sysVerifys db }

-}

-- | Declares a top level input 'Signal'.
input :: Name -> Width -> System Signal
input name width = do
  name <- fullName name
  i <- nextId
  db <- get
  let input = Input i name width
  put $ db { sysInputs = input : sysInputs db }
  return $ map (BitInput input) $ reverse [0 .. width - 1]

-- | Declares a list of top level inputs.
inputs :: Name -> Width -> Depth -> System [Signal]
inputs name w d = mapM (\ d -> input (name ++ show d) w) [0 .. d - 1]


-- | Declares a top level output.
output :: Name -> Signal -> System ()
output name value = do
  name <- fullName name
  i <- nextId
  db <- get
  put $ db { sysOutputs = Output i name value : sysOutputs db }

-- | Declares a list of top level outputs.
outputs :: Name -> [Signal] -> System ()
outputs name signals = mapM_ (\ (s,d) -> output (name ++ show d) s) $ zip signals [0..]

-- | Assigns an 'Signal' to a register 'Signal' to form an action.
(<==) :: Reg -> Signal -> Action ()
r <== s = do
  act <- get
  put $ act { actAssigns = (r,s) : actAssigns act }

-- | Returns the current value ('Signal) of a 'Reg'.
value :: Reg -> Signal
value r = map (BitReg r) $ reverse [0 .. width r - 1]

-- | Adds an enabling condition to an action.
when :: Signal -> Action ()
when [c] = do
  act <- get
  put $ act { actEnable = head ([actEnable act] &. [c]) }
when _ = error "when: Condition not a single bit."

-- | Adds a display action to print out signal values.  Follows Verilog's $display notation.
display :: String -> [Signal] -> Action ()
display string signals = do
  act <- get
  put $ act { actDisplays = (string,signals,False) : actDisplays act }

-- | Displays a string and signals, then stops the simulation.
finish :: String -> [Signal] -> Action ()
finish string signals = do
  act <- get
  put $ act { actDisplays = (string,signals,True) : actDisplays act }

-- | Sequences 'Action's in consecutive cycles.
sequenceActions :: Name -> [Action ()] -> System ()
sequenceActions name actions = do
  count <- reg (name ++ "SequenceCount") (bitsRequired $ length actions + 1) 0
  mapM_ (sequenceRule count) $ zip [0..] actions
  where
  sequenceRule :: Reg -> (Integer, Action ()) -> System ()
  sequenceRule count (n,a) = rule (name ++ show n) $ do
      when (value count ==. constant (width count) n)
      a
      incrReg count
  
-- | Loops through a list of 'Action's in consecutive cycles.  Goes back to first once last is finished.
loopActions :: Name -> [Action ()] -> System ()
loopActions name actions = do
  count <- reg (name ++ "LoopCount") (bitsRequired $ length actions) 0
  mapM_ (loopRule count) $ zip [0..] actions
  where
  loopRule :: Reg -> (Integer, Action ()) -> System ()
  loopRule count (n,a) = rule (name ++ show n) $ do
      when (value count ==. constant (width count) n)
      a
      if n == toInteger (length actions - 1)
        then count <== zero (width count)
        else incrReg count

-- | Declares the current 'Action' should be executed sequentially.
sequential :: Action ()
sequential = do
  act <- get
  put $ act { actExecution = Sequential }

-- | Used to define priority between rules.  Returns two 'Action's.  The first should be called in the higher priority rule.
--   The second should be called in the lower priority rule.
--
-- > (higherPriority, lowerPriority) <- priority
priority :: System (Action (), Action ())
priority = do
  i <- nextId
  let higher = do
        act <- get
        put $ act { actRelations = Higher i : actRelations act }
      lower  = do
        act <- get
        put $ act { actRelations = Lower  i : actRelations act }
  return (higher,lower)

{-
-- | Asserts the current 'Action' must always be enabled.
alwaysEnabled :: Action ()
alwaysEnabled = return () --XXX
-}

-- | Asserts the current 'Action' must always be active when enabled.
alwaysActiveWhenEnabled :: Action ()
alwaysActiveWhenEnabled = do
  act <- get
  put $ act { actActiveWhenEnabled = True }

{-
-- | Asserts the current 'Action' must always be enabled and active.
alwaysActive :: Action ()
alwaysActive = do
  alwaysEnabled
  alwaysActiveWhenEnabled
-}

-- | Conditional action.  Action will be performed in enabling condition is true.
doif :: Action () -> Action ()
doif action = do
  action --XXX

{-
-- | Concatenates two 'Sequence's.
($$) :: (Seq a, Seq b) => a -> b -> Sequence
s1 $$ s2  = SequenceConcat (toSeq s1) (toSeq s2)

-- | Infuses two 'Sequence's.
($:) :: (Seq a, Seq b) => a -> b -> Sequence
s1 $: s2  = SequenceInfuse (toSeq s1) (toSeq s2)

-- | ANDs two length matching 'Sequence's.
($&&) :: (Seq a, Seq b) => a -> b -> Sequence
s1 $&& s2 = SequenceAnd (toSeq s1) (toSeq s2)

-- | Sequence alternation.
($|) :: (Seq a, Seq b) => a -> b -> Sequence
s1 $| s2 = SequenceAltern (toSeq s1) (toSeq s2)

-- | ANDs two non length matching 'Sequence's.
($&) :: (Seq a, Seq b) => a -> b -> Sequence
s1 $& s2 = (s1 $&& (s2 $$ star)) $| ((s1 $$ star) $&& s2)

-- | Sequence repeated 0 or more cycles.
repeatStar :: Seq a => a -> Sequence
repeatStar s = SequenceRepeat (toSeq s)

-- | Sequence repeated 1 or more cycles.
repeatPlus :: Seq a => a -> Sequence
repeatPlus s = s $$ repeatStar s

-- | Anything, 0 or more cycles.
star :: Sequence
star = repeatStar true

-- | Anything, 1 or more cycles.
plus :: Sequence
plus = repeatPlus true

-- | One 'Sequence' within another.
within :: (Seq a, Seq b) => a -> b -> Sequence
within s1 s2 = (star $$ s1 $$ star) $&& s2


{-
  , anyN
  , anyNtoM
  , anyNtoInf
  , repeatN
  , repeatNtoM
  , repeatNtoInf
-}

-- | Converts a weak 'Sequence' to a 'Property'.
weak :: Seq a => a -> Property
weak s = PropertySequenceWeak (toSeq s)

-- | 'Property' is always true.
always :: Prop a => a -> Property
always p = PropertyAlways $ toProp p

-- | 'Property' is never true.
never :: Prop a => a -> Property
never p = always $ notP p

-- | 'Property' negation.
notP :: Prop a => a -> Property
notP p = PropertyNot $ toProp p

-- | 'Property' AND.
(&&&) :: (Prop a, Prop b) => a -> b -> Property
p1 &&& p2 = PropertyAnd (toProp p1) (toProp p2)

-- | 'Property' OR.
(|||) :: (Prop a, Prop b) => a -> b -> Property
p1 ||| p2 = notP (notP p1 &&& notP p2)

-- | 'Property' implication.
(-->) :: (Prop a, Prop b) => a -> b -> Property
p1 --> p2 = PropertyImply (toProp p1) (toProp p2)

-- | Suffix implication, overlapping.
(|->) :: (Seq a, Prop b) => a -> b -> Property
s |-> p = PropertyIfSequence (toSeq s) (toProp p)

-- | Suffix implication, non-overlapping.
(|=>) :: (Seq a, Prop b) => a -> b -> Property
s |=> p = (s $$ true) |-> p

-}



-- | Number of bits required to uniquely represent a number of elements.
bitsRequired :: Integral a => a -> Int
bitsRequired n = f $ n - 1
  where
  f :: Integral a => a -> Int
  f n | n <= 0 = 0
  f n          = 1 + f (div n 2)

-- | Creates a binary tree from a binary operation, a unary operation, and a list of inputs.
tree :: (a -> a -> a) -> (a -> a) -> [a] -> a
tree bin uni sigs = tree2 sigs
  where
  tree1 [] = []
  tree1 [a] = [uni a]
  tree1 (a:b:c) = bin a b : tree1 c
  tree2 [] = error "tree: empty list"
  tree2 [a] = a
  tree2 a = tree2 $ tree1 a

