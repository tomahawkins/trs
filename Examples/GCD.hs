-- | Euclids greatest common divisor (GCD) algorithm.
--   See: <http://en.wikipedia.org/wiki/Euclidean_algorithm>.
--
--   Euclids algorithm finds the greatest common divisor of
--   two numbers by iteratively subtracting the smaller number
--   from the larger number.  The iteration contiunes until both
--   numbers are equal.  The result is the greatest common divisor.
--
--   For example:
--
--   @                  A = 21 B = 12 @
--
--   @ A = 21 - 12  =>  A =  9 B = 12 @
--
--   @ B = 12 -  9  =>  A =  9 B =  3 @
--
--   @ A =  9 -  3  =>  A =  6 B =  3 @
--
--   @ A =  6 -  3  =>  A =  3 B =  3 @
--
--   Therefore, the GCD of 21 and 12 is 3!
--
module GCD
  ( GCD
  , mkGCD
  , start
  , result
  ) where

import Language.TRS

-- | The GCD abstract data type (aka. class).
data GCD = GCD Reg Reg Reg

-- | The constructor that creates a 'GCD' object.  Parameterized on data width.
mkGCD :: Width -> System GCD
mkGCD width = do
  active <- reg "gcdActive" 1 0  -- A flag to signal if the GCD machine is active.
  a      <- reg "gcdA" width 0   -- The "a" register.
  b      <- reg "gcdB" width 0   -- The "b" register.

  rule "subAB" $ do              -- A rule to update "a" if "a > b".
    when (value a >. value b)
    a <== value a -. value b

  rule "subBA" $ do              -- A rule to update "b" if "b > a".
    when (value b >. value a)
    b <== value b -. value a

  return (GCD active a b)        -- Return the GCD object.

-- | The 'start' method initiates a new GCD computation.
--
-- > start myGCD valueA valueB
start :: GCD -> Signal -> Signal -> Action ()
start (GCD active a b) a1 b1 = do
  when (inv (value active))     -- Only start if the GCD machine is not active.
  when (a1 /=. zero (width a1)) -- Prevent activating if a1 is zero.
  when (b1 /=. zero (width b1)) -- Prevent activating if b1 is zero.
  active <== true               -- Set the active flag.
  a <== a1                      -- Load the "a" register.
  b <== b1                      -- Load the "b" register.

-- | The 'result' method returns the result of a GCD computation.
--
-- > myResult <- result myGCD
result :: GCD -> Action Signal
result (GCD active a b) = do
  when (value active)         -- Only return a result if the GCD machine is active.
  when (value a ==. value b)  -- Only return a result if the computation is done.
  active <== false            -- Clear the active flag.
  return (value a)            -- Return the GCD result.

