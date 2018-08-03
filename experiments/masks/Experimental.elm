module Element.Input.Mask exposing (..)

{-| <https://ellie-app.com/JJxYGFKptVa1>

<https://ellie-app.com/JKMJgSTtn2a1>

<https://ellie-app.com/KdY8X99fqba1>

-}


type Masked input
    = Mask
        { capture : Parser input -- String -> input
        , format : List Formatter -- tel: "86" -> "(86 )    -    "
        , validate : List Validator -- Is a character allowed to be typed
        }


type Validator
    = Match Int (Char -> Bool)


type Formatter
    = Exactly String
    | FromInput Int Hint -- hint is shown if no string can be retrieved


type alias Hint =
    String



-- type Masked input =
--     { parser : Parser input
--     ,
--     }


{-| We want this to be in
---> formatting
-x-> result
-}
show str mask =
    mask


{-| ---> formatting
---> result

| if nothing's parsed

hint ---> formatting
hint -x-> result

-}
stringWith options mask =
    mask


type Pattern input
    = Capture Limit (Char -> Bool)
    | Decimal
    | Show String


type Limit
    = NoLimit
    | Min Int
    | Max Int
    | MinMax Int Int


type Captured input
    = Partial String
    | Full input String


{-| A placeholder ot represent different pieces having different styles
-}
type Styled
    = Styled String String


render : String -> Masked input -> List Styled


captureValue : String -> Masked input -> Captured input


capture : input -> Masked input


map : (a -> b) -> Masked a -> Masked b


map2 :
    (a -> b -> value)
    -> Masked a
    -> Masked b
    -> Masked value


andThen : (a -> Masked b) -> Masked a -> Masked b


valid : Captured input -> Maybe input
valid cap =
    case cap of
        Partial _ ->
            Nothing

        Full result _ ->
            Just result
