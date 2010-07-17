module Main ( main ) where

import Language.TRS

main :: IO ()
main = do
  putStrLn "Hello TRS!"
  verilog "basic_example" (timer w)

timer :: Int -> System ()
timer w = do
  i <- input "in1" w
  e <- input "enable" 1
  r <- reg "reg1" w 0
  rule "reg1Update" $ do
    when e
    r <== i
  output "out1" (value r)

