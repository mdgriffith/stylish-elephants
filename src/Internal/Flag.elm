module Internal.Flag exposing (..)

{-| -}

import Bitwise


type Flag
    = Flag Int


none : Flag
none =
    Flag 0


{-| If the query is in the truth, return True
-}
present : Flag -> Flag -> Bool
present (Flag query) (Flag truth) =
    Bitwise.and query truth == query


{-| Flip all bits that are 1 in the first flag, in the second flag.
-}
add : Flag -> Flag -> Flag
add (Flag flipTo) (Flag truth) =
    Flag (Bitwise.or flipTo truth)


col : Int -> Flag
col i =
    Flag (2 ^ i)
