-- | RTL Types

module RTL.Types
  ( Name
  , UID
  , Wid
  , Value
  , Clock
  , RTL (..)
  , Reg (..)
  , Sig (..)
  , Out (..)
  ) where

import Utils

type UID   = Int
type Name  = String
type Wid   = Int
type Value = Integer
type Clock = String

data RTL = RTL [Reg] [Out]

data Reg = Reg UID Name Clock Wid (Maybe Value)

instance Show Reg where
  show (Reg _ name _ _ _) = name

instance Eq Reg where
  a == b = uid a == uid b

instance Ord Reg where
  compare a b = compare (uid a) (uid b)

instance Width Reg where
  width (Reg _ _ _ w _) = w
  showWidth r@(Reg _ n _ _ _) = show (width r) ++ "'" ++ n

instance UId Reg where
  uid (Reg u _ _ _ _) = u


data Out = Out Name Sig

data Sig
  = Input  Name Wid
  | Add    Sig Sig
  | Sub    Sig Sig
  | Not    Sig
  | And    Sig Sig
  | Xor    Sig Sig
  | Or     Sig Sig
  | Select Sig Int Int    -- Select sig msb lsb
  | Concat Sig Sig
  | Mux    Sig Sig Sig
  | Eq     Sig Sig
  | Lt     Sig Sig
  | Const  Int Integer
  | NonDet Int            -- Non-deterministic value.  NonDet width
  | RegVal Reg
  deriving (Eq, Ord)

instance Show Sig where
  show (Input n _)    = "(input " ++ n ++ ")"
  show (Add a b)      = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Sub a b)      = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (Not a)        = "(~ " ++ show a ++ ")"
  show (And a b)      = "(" ++ show a ++ " & " ++ show b ++ ")"
  show (Xor a b)      = "(" ++ show a ++ " ^ " ++ show b ++ ")"
  show (Or  a b)      = "(" ++ show a ++ " | " ++ show b ++ ")"
  show (Select a m l) = "(" ++ show a ++ "[" ++ show m ++ ":" ++ show l ++ "])"
  show (Concat a b)   = "{" ++ show a ++ ", " ++ show b ++ "}"
  show (Mux c a b)    = "(" ++ show c ++ " ? " ++ show a ++ " : " ++ show b ++ ")"
  show (Eq  a b)      = "(" ++ show a ++ " == " ++ show b ++ ")"
  show (Lt  a b)      = "(" ++ show a ++ " < " ++ show b ++ ")"
  show (Const _ v)    = show v
  show (NonDet w)     = replicate w 'x'
  show (RegVal reg)   = show reg

instance Width Sig where
  width sig = case sig of
    Input  _ w   -> w
    Add    a _   -> width a
    Sub    a _   -> width a
    Not    a     -> width a
    And    a _   -> width a
    Xor    a _   -> width a
    Or     a _   -> width a
    Select _ m l -> m - l + 1
    Concat a b   -> width a + width b
    Mux    _ a _ -> width a
    Eq     _ _   -> 1
    Lt     _ _   -> 1
    Const  w _   -> w
    NonDet w     -> w
    RegVal reg   -> width reg

  showWidth sig = case sig of
    (Input n w)    -> "(input " ++ show w ++ "'" ++ n ++ ")"
    (Add a b)      -> show (width sig) ++ "'(" ++ showWidth a ++ " + " ++ showWidth b ++ ")"
    (Sub a b)      -> show (width sig) ++ "'(" ++ showWidth a ++ " - " ++ showWidth b ++ ")"
    (Not a)        -> show (width sig) ++ "'(~ " ++ showWidth a ++ ")"
    (And a b)      -> show (width sig) ++ "'(" ++ showWidth a ++ " & " ++ showWidth b ++ ")"
    (Xor a b)      -> show (width sig) ++ "'(" ++ showWidth a ++ " ^ " ++ showWidth b ++ ")"
    (Or  a b)      -> show (width sig) ++ "'(" ++ showWidth a ++ " | " ++ showWidth b ++ ")"
    (Select a m l) -> show (width sig) ++ "'(" ++ showWidth a ++ "[" ++ show m ++ ":" ++ show l ++ "])"
    (Concat a b)   -> show (width sig) ++ "'{" ++ showWidth a ++ ", " ++ showWidth b ++ "}"
    (Mux c a b)    -> show (width sig) ++ "'(" ++ showWidth c ++ " ? " ++ showWidth a ++ " : " ++ showWidth b ++ ")"
    (Eq  a b)      -> show (width sig) ++ "'(" ++ showWidth a ++ " == " ++ showWidth b ++ ")"
    (Lt  a b)      -> show (width sig) ++ "'(" ++ showWidth a ++ " < " ++ showWidth b ++ ")"
    (Const _ v)    -> show (width sig) ++ "'" ++ show v
    (NonDet w)     -> show (width sig) ++ "'" ++ replicate w 'x'
    (RegVal reg)   -> showWidth reg


