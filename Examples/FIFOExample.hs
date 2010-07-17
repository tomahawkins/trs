module Main (main) where

import Language.TRS
import FIFO

main :: IO ()
main = do
  verilog "fifo" (fifo 4 4)

fifo :: Width -> Depth -> System ()
fifo w d = do
  fifo <- scope "fifo" true $ mkFIFO w d
  count <- reg "count" (bitsRequired d + 1) 0
  enq <- input "enque" 1
  deq <- input "deque" 1
  enqData <- input "enque_data" w

  deqData <- reg "dequeDataReg" w 0
  output "deque_data" $ value deqData



  let isEmpty = value count ==. zero (width count)
      isFull  = value count ==. constant (width count) (toInteger d)

  output "is_empty" isEmpty
  output "is_full"  isFull

  enqued <- reg "enquedReg" 1 0
  dequed <- reg "dequedReg" 1 0
  output "enqued" $ value enqued
  output "dequed" $ value dequed

  (h,l) <- priority

  rule "enqueRule" $ do
    h
    when enq
    when $ inv isFull
    incrReg count
    enque enqData fifo
    enqued <== true

  rule "notEnque" $ do
    l
    enqued <== false

  (h,l) <- priority

  rule "dequeRule" $ do
    h
    when deq
    when $ inv isEmpty
    decrReg count
    s <- deque fifo
    deqData <== s
    dequed <== false

  rule "notDeque" $ do
    l
    dequed <== false


