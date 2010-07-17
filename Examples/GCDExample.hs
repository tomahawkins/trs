-- | The following example declare the top level ports and generates Verilog for a 'GCD' machine.
module Main ( main ) where

-- Import the TRS modules.
import Language.TRS

-- Import the 'GCD' module.
import GCD

-- | The entry point of any Haskell program.
main :: IO ()
main = do
  putStrLn "Hello TRS!"
  putStrLn "Generating a netlist for an 8-bit GCD machine (gcd.v)..."
  verilog "gcd"          (gcdExample  8)  -- Compiles the gcdExample system configured to 8-bits.
  putStrLn "Generating stimulus for GCD simulation (gcd_stimulus.v)..."
  verilog "gcd_stimulus" (gcdStimulus 8)  -- Compile stimulus for testing.

-- A top level interface for a GCD system, parameterized for data width.
gcdExample :: Width -> System ()
gcdExample w = do

  load  <- input "load"  1   -- Top level input to load data into GCD machine.
  dataA <- input "dataA" w   -- Two data inputs, with width w.
  dataB <- input "dataB" w

  gcd <- scope "gcd" true (mkGCD w)   -- GCD instantiation.

  rule "startGCD" $ do
    when load                -- Start only if load is true.
    start gcd dataA dataB    -- Start the GCD machine.

  resultReady <- reg "resultReadyReg" 1 0  -- A register to flag the output when the result in ready.
  resultData  <- reg "resultDataReg"  w 0  -- A register to capture the GCD result.

  (higherPriority,lowerPriority) <- priority  -- Declare a priority between rules.  See below...

  rule "captureResult" $ do  -- Capture the GCD result.
    higherPriority           -- "captureResult" must have higher priority than "notReady".
    r <- result gcd
    resultData  <== r        -- Save the GCD result.
    resultReady <== true     -- Set the resultReady flag.

  rule "notReady" $ do
    lowerPriority            -- "notReady" must have lower priority than "captureResult".
    resultReady <== false    -- Clear the resultReady flag.

  output "resultReady" (value resultReady)  -- Output result ready signal.
  output "resultData"  (value resultData)   -- Output the GCD result.

-- Stimulus for the GCD design.
gcdStimulus :: Width -> System ()
gcdStimulus w = do

  resultReady <- input "resultReady" 1
  resultData  <- input "resultData"  w

  load  <- reg "loadReg"  1 0
  dataA <- reg "dataAReg" w 0
  dataB <- reg "dataBReg" w 0

  output "load"  $ value load
  output "dataA" $ value dataA
  output "dataB" $ value dataB

  let tests = [ (18,16,3)
              , (255,254,1)
              , (255,255,255)
              , (10,20,10)
              , (21,12,3)
              ]
      test (a,b,r) = [start, dropLoad, end]
        where
        a' = constant w a
        b' = constant w b
        r' = constant w r
        start = do
          display "Starting GCD computation with %d and %d." [a',b']
          dataA <== a'
          dataB <== b'
          load  <== true
        dropLoad = load <== false
        end = do
          when resultReady
          display "GCD complete:  result = %d  expected = %d  pass = %d" [resultData, r', resultData ==. r']

  sequenceActions "testSequence" $ concatMap test tests ++ [finish "Test complete." []]




