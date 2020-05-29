module Value where

import Test.QuickCheck.Arbitrary

data Value = M | E | X | U | A | B | C | D | F | S
  deriving (Eq, Ord, Enum, Bounded)

instance Show Value where
  show v = case v of
    M -> "o"
    E -> " "
    U -> "_"
    X -> "X"
    A -> "A"
    B -> "B"
    C -> "C"
    D -> "D"
    F -> "F"
    S -> "S"

instance Read Value where
  readsPrec _ s =
    case s of
      "o" -> [(M, "")]
      " " -> [(E, "")]
      "_" -> [(U, "")]
      "X" -> [(X, "")]
      "A" -> [(A, "")]
      "B" -> [(B, "")]
      "C" -> [(C, "")]
      "D" -> [(D, "")]
      "F" -> [(F, "")]
      "S" -> [(S, "")]
      _ -> []

instance Arbitrary Value where
  arbitrary = arbitraryBoundedEnum

-- Ships!
boats = [A, B, C, D, F, S]

-- | Circular `succ`.
succVal :: Value -> Value
succVal v
  | v == maxBound = minBound
  | otherwise     = succ v
