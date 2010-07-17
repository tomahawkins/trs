module Language.TRS.Logic
  ( Bit   (..)
  , Input (..)
  , Label (..)
  , Reg   (..)
  , true
  , false
  , not_
  , and_
  , or_
  ) where

data Bit
  = Not Bit
  | And Bit Bit
  | Or  Bit Bit
  | Const Bool
  | BitInput Input Int
  | BitLabel Label Int
  | BitReg   Reg   Int deriving (Eq, Ord)

data Input = Input Int String Int
data Label = Label Int String [Bit]
data Reg   = Reg   Int String Int Integer

instance Eq Input where (Input a _ _) == (Input b _ _) = a == b
instance Eq Label where (Label a _ _) == (Label b _ _) = a == b
instance Eq Reg   where (Reg   a _ _ _) == (Reg   b _ _ _) = a == b
instance Ord Input where compare (Input a _ _) (Input b _ _) = compare a b
instance Ord Label where compare (Label a _ _) (Label b _ _) = compare a b
instance Ord Reg   where compare (Reg a _ _ _) (Reg b _ _ _) = compare a b

true :: Bit
true = Const True

false :: Bit
false = Const False

not_ :: Bit -> Bit
not_ a = case a of
  Const False -> true
  Const True  -> false
  Not a -> a
  a -> Not a

and_ :: Bit -> Bit -> Bit
and_ a b = case (a,b) of
  (Const False, _) -> false
  (_, Const False) -> false
  (Const True, b) -> b
  (a, Const True) -> a
  (a,b) -> And a b

or_ :: Bit -> Bit -> Bit
or_ a b = case (a,b) of
  (Const False, b) -> b
  (a, Const False) -> a
  (Const True, _) -> true
  (_, Const True) -> true
  (a,b) -> Or a b

