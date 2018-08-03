module Element.Input.Mask exposing (..)

{-| With masked input we can simultaneously descibe:

  - a parser to only allow specific arguments
      - We have a set of expected characters and how many of them to expect.
  - Static formating. Meaning show a `/` between two numbers, but it's not part of the input
  - Hints. These are mini placeholder values for a section of your input.
  - Autocomplete suggestions. These are suggestions that can be selected and completed with `tab`.

-}


type Masked input
    = Mask (List Pattern)


type Pattern
    = Capture Limit (Char -> Bool)
    | Decimal
    | Decoration String


type Limit
    = NoLimit
    | Min Int
    | Max Int
    | MinMax Int Int


type Input thing
    = Partial String
    | Full thing String


type alias CreditCard =
    { number : String
    , expMonth : String
    , expYear : String
    , ccv : String
    }


type alias CreditCardNumber =
    String


example =
    capture (\one two three four -> CreditCardNumber (one ++ two ++ three ++ four))
        |> Mask.stringWith
            { length = 4
            , valid = String.isDigit
            , hint = "1234"
            }
        |> Mask.show " "
        |> Mask.stringWith
            { length = 4
            , valid = String.isDigit
            , hint = "1234"
            }
        |> Mask.show " "
        |> Mask.stringWith
            { length = 4
            , valid = String.isDigit
            , hint = "1234"
            }
        |> Mask.space
        |> Mask.stringWith
            { length = 4
            , valid = String.isDigit
            , hint = "1234"
            }
        |> Mask.andThen
            (\creditcard ->
                capture (CreditCard creditNumber)
                    |> Mask.token "1234"
                    -- last four digits of credit card
                    |> Mask.space
                    |> Mask.stringWith
                        { length = 2
                        , valid = String.isDigit
                        , hint = "MM"
                        }
                    |> Mask.token "/"
                    |> Mask.stringWith
                        { length = 2
                        , valid = String.isDigit
                        , hint = "YY"
                        }
                    |> Mask.space
                    |> Mask.stringWith
                        { length = 3
                        , valid = String.isDigit
                        , hint = "CCV"
                        }
            )


float =
    Mask.int
        |> Mask.token "."
        |> Mask.andThen
            (\one ->
                masked (\two -> one + two)
                    --combine ints in a way
                    |> Mask.token (toString one)
                    |> Mask.token "."
                    |> Mask.stringWith
                        { length = 3 -- No length restrictions
                        , valid = String.isDigit
                        , hint = "CCV"
                        }
            )
