module FIFO
  ( FIFO
  , mkFIFO
  , enque
  , deque
  ) where

import Language.TRS

data FIFO = FIFO Width -- ???

mkFIFO :: Width -> Depth -> System FIFO
mkFIFO w d = do
  -- ???
  return $ FIFO w

enque :: Signal -> FIFO -> Action ()
enque signal fifo = do
  -- ???
  return ()

deque :: FIFO -> Action Signal
deque (FIFO w) = do
  -- ???
  return $ zero w



