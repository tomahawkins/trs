module Main ( main ) where

import Language.TRS

main :: IO ()
main = do
  putStrLn "Hello TRS!"
  verilog "timer" (timer 8)
  verilog "timer_monitor" (timerMonitor 8)
  --verilog "timer_stimulus" (timerStimulus 8)

data Timer = Timer Reg Reg

mkTimer :: Width -> System Timer
mkTimer w = do
  count <- reg "count" w 0
  active <- reg "active" 1 0
  rule "decrementCount" $ do
    alwaysActiveWhenEnabled
    when $ value count >. zero w
    decrReg count
  return $ Timer count active

startTimer :: Timer -> Signal -> Action ()
startTimer (Timer count active) time = do
  when $ inv $ value active
  count <== time -. one (width count)
  active <== true

timerFinished :: Timer -> Action ()
timerFinished (Timer count active) = do
  when (value count <=. one (width count))
  when $ value active
  active <== false

timer :: Int -> System ()
timer w = do
  start <- input "start" 1
  count <- input "count" w
  timer <- scope "timer" true $ mkTimer w

  rule "startRule" $ do
    when start
    startTimer timer count

  done <- reg "doneReg" 1 0
  output "done" $ value done

  (h,l) <- priority

  rule "doneOn" $ do
    h
    timerFinished timer
    done <== true

  rule "doneOff" $ do
    l
    done <== false


timerMonitor :: Int -> System ()
timerMonitor w = do
  start <- input "start" 1
  count <- input "count" w
  timer <- scope "timer" true $ mkTimer w
  doneI <- input "done" 1
  rule "startRule" $ do
    when start
    startTimer timer count

  done <- reg "doneReg" 1 0

  (h,l) <- priority

  rule "doneOn" $ do
    h
    timerFinished timer
    done <== true

  rule "doneOff" $ do
    l
    done <== false

  assert "doneCheck" $ always $ value done ==. doneI
  cover  "time1" (start &. count ==. constant 8 1 $$ doneI)
  cover  "time3" (start &. count ==. constant 8 3 $$ inv start $$ inv start $$ doneI)
  cover  "time5" (start &. count ==. constant 8 5 $$ inv start $$ inv start $$ inv start $$ inv start $$ doneI)


timerStimulus :: Width -> System ()
timerStimulus w = do
  done <- input "done" 1
  start <- reg "startReg" 1 0
  count <- reg "countReg" w 0
  output "start" $ value start
  output "count" $ value count

  cycle <- reg "cycle" 8 0

  rule "incrCycle" $ do
    alwaysActiveWhenEnabled
    incrReg cycle

  sequenceActions "start" $
    [ (start <== true) >> (count <== constant w 3) >> display "cycle: %d  start" [value cycle]
    , (start <== false)
    , (when done) >> finish "cycle: %d  done" [value cycle]
    ]

