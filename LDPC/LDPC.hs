module Main ( main ) where

import Data.List
import Data.Maybe

import Language.TRS

main :: IO ()
main = do
  --g <- gMatrix
  h <- hMatrix
  --r <- rowSwap
  --let hDiag = swapRows h r
  --writeFile "h_diag.svg" $ svg hDiag
  --writeFile "g.svg" $ svg g
  --writeFile "h.svg" $ svg h
  --putStrLn "Generating encoder..."
  --verilog "ldpc_encoder_802_3an" $ encoder g
  putStrLn "Generating decoder..."
  --verilog "ldpc_decoder_802_3an" $ gallagerADecoder h
  --verilog "ldpc_decoder_802_3an" $ softDecoder h 1723 6
  verilog "ldpc_decoder_802_3an" $ softDecoder (testMatrix 4 4) 3 6

matrixData :: String -> [(Int,Int)]
matrixData d = a1
  where
  a0 = zip [0..] $ lines $ map replaceComma d
  a1 = concatMap (\ (r,line) -> map (\ c -> (r, read c)) $ words line) a0
  replaceComma ',' = ' '
  replaceComma a   = a

type Matrix = ((Int,Int),[(Int,Int)]) -- ((rows,cols),points)

hMatrix :: IO Matrix
hMatrix = do
  f <- readFile "H.txt"
  return ((384,2048), matrixData f)

gMatrix :: IO Matrix
gMatrix = do
  f <- readFile "g.txt"
  return ((1723,2048), matrixData f)

testMatrix :: Int -> Int -> Matrix
testMatrix row col = ((row,col),mat)
  where
  mat = concatMap (\ r -> map (\ c -> (r,c)) [0 .. col - 1]) [0 .. row -1]

rowSwap :: IO [Int]
rowSwap = do
  f <- readFile "row_swap.txt"
  return $ map (+ (-1)) $ snd $ unzip $ matrixData f

swapRows :: Matrix -> [Int] -> Matrix
swapRows ((r,c),d) rows = ((r,c), map f d)
  where
  l = length rows
  f (r,c) = if r >= l then (r,c) else (rows !! r, c)

svg :: Matrix -> String
svg ((h,w),d) = unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
  , "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 20010904//EN\""
  , "\"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd\""
  , "["
  , " <!ATTLIST svg"
  , "  xmlns:xlink CDATA #FIXED \"http://www.w3.org/1999/xlink\">"
  , "]>"
  , "<svg"
  , "   xmlns=\"http://www.w3.org/2000/svg\""
  , "   xmlns:xlink=\"http://www.w3.org/1999/xlink\""
  , "   version=\"1\""
  , "   width=\"" ++ show w ++ "\""
  , "   height=\"" ++ show h ++ "\""
  , "   >"
  , "  <rect x=\"0\" y=\"0\" width=\"" ++ show w ++ "\"  height=\"" ++ show h ++ "\"  fill=\"#FFFFFF\" />"
  , concatMap (\ (r,c) -> "  <rect x=\"" ++ show c ++ "\" y=\"" ++ show r ++ "\" width=\"1\" height=\"1\"  fill=\"#000000\" />\n") d
  , "</svg>"
  ]


encoder :: Matrix -> System ()
encoder ((r,c),mat) = do
  in0 <- input "uncoded_block" r
  let inputBits = map inputBit [0 .. r - 1]
      inputBit i = select in0 i i
      rowSelects c = fst $ unzip $ filter ((c ==) . snd) mat
      outputBit c = tree (^.) id $ map (inputBits !!) $ rowSelects c
      outputBits = map outputBit [0 .. c - 1]
      outputSignal = foldl1 (++) $ reverse outputBits
  output "coded_block" outputSignal

gallagerADecoder :: Matrix -> System ()
gallagerADecoder ((r,c),mat) = do
  load <- input "load" 1
  in0  <- input "coded_block" c
  block <- array 0
  rule "loadRegs" $ do
    when load
    mapM_ (\ (n,r) -> r <== select in0 n n) $ zip [0..] block
  let colSelects r = snd $ unzip $ filter ((r ==) . fst) mat
      rowSelects c = fst $ unzip $ filter ((c ==) . snd) mat
      checkNode r = tree (^.) id $ map (value . (block !!)) $ colSelects r
      checkNodes = map checkNode [0 .. r - 1]
      changeValueNode c = voting $ map (checkNodes !!) $ rowSelects c
      decoded = inv $ tree (|.) id checkNodes
  rule "correctBlock" $ do
    when $ inv load
    when $ inv decoded
    mapM_ (\ (n,r) -> r <== value r ^. changeValueNode n) $ zip [0..] block
  output "decoded" decoded
  output "decoded_block" $ foldl1 (++) $ reverse $ map value block
  where

  array :: Int -> System [Reg] 
  array n | n == c = return []
  array n = do
    r <- reg ("r" ++ show n) 1 0
    rs <- array (n+1)
    return (r:rs)

voting :: [Signal] -> Signal
voting checks = ones >. constant (width ones) (fromIntegral $ length checks `div` 2)
  where
  ones = tree add (false ++) checks
  add a b = (false ++ a) +. (false ++ b)



softDecoder :: Matrix -> Width -> Width -> System ()
softDecoder h@((r,c),mat) decodedBlockWidth betaWidth = do
  start <- input "start" 1
  maxIterations <- input "max_iterations" 8
  state <- reg "state" 2 0

  let idle   = constant 2 0
      armed  = constant 2 1
      active = constant 2 2

  loadInputs <- inputs "load" 1 c
  dataInputs <- inputs "data" betaWidth c

  loadRegs <- regs "loadReg" 1 c 0
  dataRegs <- regs "dataReg" betaWidth c 0

  betaRegs <- regs "betaReg" betaWidth (length mat) 0

  iteration <- reg "iteration" (width maxIterations) 0

  done    <- reg "doneReg" 1 0
  correct <- reg "correctReg" 1 0
  result  <- reg "resultReg" decodedBlockWidth 0 

  output "done"    $ value done
  output "correct" $ value correct
  output "result"  $ value result

  rule "arm" $ do
    alwaysActiveWhenEnabled
    when start
    state <== armed
    done  <== false
    iteration <== maxIterations
    mapM_ (<== false) loadRegs


  let loadData (loadInput, dataInput, loadReg, dataReg, n) = do
        when (value state ==. armed)
        when loadInput
        when (inv $ value loadReg)
        loadReg <== true
        dataReg <== dataInput
        mapM_ (<== dataInput) betas
        where
        betas = mapMaybe isBeta $ zip mat betaRegs
        isBeta ((_,c),r) | c == n = Just r
        isBeta _ = Nothing

  
  rules "loading" $ map loadData $ zip5 loadInputs dataInputs loadRegs dataRegs [0..]

  rule "activate" $ do
    when (value state ==. armed)
    when (tree (&.) id $ map value loadRegs)
    state <== active

  (isCorrect, decodedWord, nextBetas) <- decodeIteration h dataRegs betaRegs

  rule "active" $ do
    when (value state ==. active)
    mapM_ (\ (a,b) -> a <== b) $ zip dataRegs nextBetas

  rule "decrementIteration" $ do
    alwaysActiveWhenEnabled
    when (value state ==. active)
    decrReg iteration

  rule "doneOn" $ do
    when (value state ==. active)
    when (value iteration ==. zero (width iteration) |. isCorrect)
    state   <== idle
    done    <== true
    correct <== isCorrect
    result  <== select decodedWord (decodedBlockWidth - 1) 0

  rule "doneOff" $ do
    when (value state ==. idle &. value done)
    done <== false

-- | XORed sign and smallest and second smallest magitude from a row of beta values.
signMags :: [Signal] -> (Signal,Signal,Signal)  -- (sign, smallest, second smallest)
signMags s = (sign, lower, higher)
  where
  sign = tree (^.) id $ map msb s
  (lower,higher) = smallest2 $ map lsbs s

  smallest2 :: [Signal] -> (Signal,Signal)
  smallest2 s = split $ tree smallest2' id $ pair s

  pair :: [Signal] -> [Signal]
  pair [] = []
  pair [_] = error "pair: Expecting even list."
  pair (a:b:c) = (a ++ b) : pair c

  smallest2' :: Signal -> Signal -> Signal
  smallest2' a b = l2 ++ l4
    where
    (l0,h0) = sort2 $ split a
    (l1,h1) = sort2 $ split b
    (l2,h2) = sort2 (l0,l1)
    (l3,_)  = sort2 (h0,h1)
    (l4,_)  = sort2 (h2,l3)

  split :: Signal -> (Signal,Signal)
  split a = (a1,a0)
    where
    w = width a
    a1 = select a (w - 1) (div w 2)
    a0 = select a (div w 2 - 1) 0

  sort2 :: (Signal,Signal) -> (Signal,Signal)
  sort2 (a,b) = (mux (a <. b) a b, mux (a <. b) b a)

-- | Sign-magnitude adder.  Precision is increased by 1-bit to prevent overflow.
addSignMag :: Signal -> Signal -> Signal
addSignMag a b = aS ++ ((false ++ aM) +. bM')
  where
  (aS, aM) = (msb a, lsbs a)
  (bS, bM) = (msb b, lsbs b)
  bM' = mux (aS ^. bS) (zero (width bM + 1) -. (false ++ bM)) (false ++ bM)

-- | Decode iteration.
decodeIteration :: Matrix -> [Reg] -> [Reg] -> System (Signal,Signal,[Signal])  -- (isCorrect, decode word, next betas)
decodeIteration ((r,c),mat) inputs' betas = do
  decodedWord <- decodedWord
  isCorrect <- isCorrect decodedWord
  return (isCorrect, decodedWord, nextBetas)
  where

  inputs = map value inputs'

  matBetas :: [((Int,Int),Signal)]
  matBetas = zip mat $ map value betas

  rowSig :: Int -> ((Int,Int),Signal) -> Maybe Signal
  rowSig n ((r,_),s) | n == r = Just s 
  rowSig _ _ = Nothing

  colSig :: Int -> ((Int,Int),Signal) -> Maybe Signal
  colSig n ((_,c),s) | n == c = Just s 
  colSig _ _ = Nothing

  checkNode :: Int -> System (Signal,Signal,Signal)
  checkNode n = do
    a <- label ("checkNodeSign" ++ show n) sign
    b <- label ("checkNodeSmallest" ++ show n) smallest
    c <- label ("checkNodeSecondSmallest" ++ show n) secondSmallest
    return (a,b,c)
    where
    (sign,smallest,secondSmallest) = signMags $ mapMaybe (rowSig n) matBetas

  checkNodes :: System [(Signal,Signal,Signal)]
  checkNodes = mapM checkNode [0 .. r - 1]

  checkNodeReturn :: [(Signal,Signal,Signal)] -> ((Int,Int),Signal) -> System ((Int,Int),Signal)
  checkNodeReturn checkNodes ((r,c),beta) = do
    a <- label ("checkNodeReturn" ++ show r ++ "_" ++ show c) $ (sign ++ mag)
    return ((r,c), a)
    where
    (s,l0,l1) = checkNodes !! r
    sign = s ^. msb beta
    mag  = mux (lsbs beta ==. l0) l1 l0

  checkNodeReturns :: System [((Int,Int),Signal)]
  checkNodeReturns = do
    checkNodes <- checkNodes
    mapM (checkNodeReturn checkNodes) matBetas

  decode :: [((Int,Int),Signal)] -> Int -> System Signal
  decode checkNodeReturns n = label ("decoded" ++ show n) decoded
    where
    operands = mapMaybe (colSig n) checkNodeReturns
    decoded = msb $ addTree $ map to2sComp $ (inputs !! n) : operands

  decodes :: System [Signal]
  decodes = do
    checkNodeReturns <- checkNodeReturns
    mapM (decode checkNodeReturns) [0 .. c - 1]

  decodedWord :: System Signal
  decodedWord = do
    d <- decodes
    label "decodedWord" $ foldl1 (++) $ reverse d

  isCorrect :: Signal -> System Signal
  isCorrect decodedWord = return $ inv notCorrect
    where
    colSelects r = snd $ unzip $ filter ((r ==) . fst) mat
    decodedBit b = select decodedWord b b
    checkNode r = tree (^.) id $ map decodedBit $ colSelects r
    checkNodes = map checkNode [0 .. r - 1]
    notCorrect = tree (|.) id checkNodes

  nextBetas :: [Signal]
  nextBetas = inputs --XXX



addTree :: [Signal] -> Signal
addTree sigs = tree extendAdd extend sigs
  where
  extendAdd a b = extend a +. extend b
  extend a = msb a ++ a

toSignMag :: Signal -> Signal
toSignMag sig = s ++ mux s m'' m
  where
  s = msb sig
  m = lsbs sig
  m' = zero (width m) -. m
  m'' = mux (m ==. zero (width m)) (ones (width m)) m'

to2sComp :: Signal -> Signal
to2sComp sig = s ++ mux s (zero (width m) -. m) m
  where
  (s,m) = (msb sig, lsbs sig)




