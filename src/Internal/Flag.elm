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


transparency =
    col 1


padding =
    col 2


spacing =
    col 3


fontSize =
    col 4


fontFamily =
    col 5


width =
    col 6


height =
    col 7


bgColor =
    col 8


bgImage =
    col 9


bgGradient =
    col 10


borderStyle =
    col 11


fontAlignment =
    col 12


fontWeight =
    col 13


fontColor =
    col 14


wordSpacing =
    col 15


letterSpacing =
    col 16


borderRound =
    col 17


shadows =
    col 19


overflow =
    col 20


cursor =
    col 21


scale =
    col 23


rotate =
    col 24


moveX =
    col 25


moveY =
    col 26


borderWidth =
    col 27


borderColor =
    col 28


yAlign =
    col 29


xAlign =
    col 30


focus =
    col 31


active =
    col 32


hover =
    col 33


gridTemplate =
    col 36


gridPosition =
    col 37
