-- | The following example declare the top level ports and generates Verilog for a 'GCD' machine.
module Main ( main ) where

-- Import the TRS modules.
import Language.TRS

-- | The entry point of any Haskell program.
main :: IO ()
main = do
  putStrLn "Hello TRS!"
  putStrLn "Generating a netlist for a sorter machine..."
  verilog "sorter" $ sorterExample 8 8
  verilog "sorter_stimulus" $ sorterStimulus 8 8

data Sorter = Sorter [Reg]

sorter :: Width -> Depth -> System Sorter
sorter width depth = do
  regs <- regs "reg" width depth 0
  swap 0 regs
  return $ Sorter regs
  where
  swap :: Int -> [Reg] -> System ()
  swap _ [] = return ()
  swap _ [_] = return ()
  swap n (a:b:c) = do
    rule ("swap_" ++ show n ++ "_" ++ show (n + 1)) $ do
      when (value b <. value a)
      a <== value b
      b <== value a
    swap (n+1) (b:c)

load :: [Signal] -> Sorter -> Action ()
load signals s@(Sorter regs) = do 
  when $ isSorted s
  mapM_ (\ (r,s) -> r <== s) $ zip regs signals

sortedValues :: Sorter -> [Signal]
sortedValues (Sorter regs) = map value regs

isSorted :: Sorter -> Signal
isSorted (Sorter regs) = isSorted regs
  where
  isSorted :: [Reg] -> Signal
  isSorted [] = true
  isSorted [_] = true
  isSorted (a:b:c) = (value a <=. value b) &. isSorted (b:c)

sorterExample :: Width -> Depth -> System ()
sorterExample w d = do
  loadData <- input "load" 1
  ins <- inputs "in" w d
  s <- sorter w d
  rule "loadData" $ do
    when loadData
    load ins s
  output "sorted" $ isSorted s
  outputs "out" $ sortedValues s

sorterStimulus :: Width -> Depth -> System ()
sorterStimulus w d = do
  sorted <- input "sorted" 1
  outs   <- inputs "out" w d
  load <- reg "loadData" 1 0
  output "load" $ value load
  testValues    <- regs "testValues" w d 0
  outputs "in" $ map value testValues

  let tests = [ [0, 0, 0, 0, 0, 0, 0, 0]
              , [7, 6, 5, 4, 3, 2, 1, 0]
              , [6, 6, 4, 4, 2, 2, 0, 0]
              , [1, 0, 3, 2, 5, 4, 7, 6]
              , [6, 7, 4, 5, 2, 3, 0, 1]
              ]
      test vs = [startLoad, dropLoad, displaySorted]
        where
        startLoad = foldl (>>) (load <== true) $ map (\ (r,v) -> r <== constant (width r) v) $ zip testValues vs
        dropLoad = load <== false
        displaySorted = do
          when sorted
          display ("sorted: " ++ concatMap (\ _ -> "%d  ") [1 .. d] ++ " => " ++ concatMap (\ _ -> "%d  ") [1 .. d]) $ (map value testValues) ++ outs

  sequenceActions "testSequence" $ concatMap test tests ++ [finish "Test complete." []]

