-- | Arbiter parameterized for the number of requester
--   interfaces and width of requester priority counters.
module Arbiter
  ( Requester
  , request
  , transact
  , complete
  , arbiter
  ) where

import Language.TRS

-- | Requester abstract data type.
data Requester = Requester (Action ()) (Action ()) (Action ())

-- | Request method to access an arbitrated resource.
request :: Requester -> Action ()
request (Requester r _ _) = r

-- | Transact method to perform an action with an arbitrated resource.
transact :: Requester -> Action ()
transact (Requester _ t _) = t

-- | Complete method to complete a transaction or drop a request of an arbitrated resource.
complete :: Requester -> Action ()
complete (Requester _ _ c) = c


type Channel = (Signal,Reg,Reg,Reg)  -- (init,count,request,active)

-- | Creates an arbiter given a list of weight signals for each requester.
--   Returns a list of 'Requester' interfaces.  Order of input request signals
--   determines priority in the event of a tie.
arbiter :: [Signal] -> System [Requester]
arbiter weights = do
  channels <- mapM channel weights
  arbitrate (allOff channels) [] channels
  return $ map interface channels
  where
  channel :: Signal -> System Channel
  channel weight = do
    count   <- reg "count"   (width weight) 0
    request <- reg "request" 1 0
    active  <- reg "active"  1 0
    return (weight,count,request,active)

  interface :: Channel -> Requester
  interface (_,_,request',active) = Requester request transact complete
    where
    request  = when (inv (value request') &. inv (value active)) >> (request' <== true)
    transact = when $ value active
    complete = do
      when $ value request'
      request' <== false
      active   <== false

  allOff :: [Channel] -> Signal
  allOff channels = inv $ foldl (\ a (_,_,_,b) -> a |. value b) false channels

  arbitrate :: Signal -> [Channel] -> [Channel] -> System ()
  arbitrate _ _ [] = return ()
  arbitrate allOff higher ((a@(init,count,request,active)):lower) = do
    rule "arbitrate" $ do
      when allOff
      when $ value request
      when $ foldl (compare (<.) ) true higher
      when $ foldl (compare (<=.)) true lower
      active <== true
      count  <== init
      mapM_ decrement (higher ++ lower)
    arbitrate allOff (a:higher) lower
    where
    compare :: (Signal -> Signal -> Signal) -> Signal -> Channel -> Signal
    compare f a (_,c,r,_) = a &. (inv (value r) |. f count' c')
      where
      totalWidth = max (width c) (width count)
      c'     = zero (totalWidth - width c)     ++. value c
      count' = zero (totalWidth - width count) ++. value count

    decrement :: Channel -> Action ()
    decrement (_,c,_,_) = c <== mux (value c ==. zero (width c))
                                    (value c)
                                    (value c -. one (width c))

