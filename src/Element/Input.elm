module Element.Input
    exposing
        ( Label
        , Option
        , OptionState(..)
        , Placeholder
        , Radio
        , button
        , checkbox
        , currentPassword
        , email
        , focusedOnLoad
        , labelAbove
        , labelBelow
        , labelLeft
        , labelRight
        , multiline
        , newPassword
        , option
        , optionWith
        , placeholder
        , radio
        , radioRow
        , search
        , spellChecked
        , text
        , username
        )

{-|


## Inputs

@docs button

@docs checkbox

@docs text, Placeholder, placeholder, username, newPassword, currentPassword, email, search, spellChecked

@docs multiline

@docs Radio, radio, radioRow, Option, option, optionWith, OptionState


## Labels

@docs Label, labelAbove, labelBelow, labelLeft, labelRight

@docs focusedOnLoad

-}

import Element exposing (Attribute, Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Html
import Html.Attributes
import Html.Events
import Internal.Grid
import Internal.Model as Internal
import Internal.Style exposing (classes)
import Json.Decode as Json


{-| -}
type Placeholder msg
    = Placeholder (List (Attribute msg)) (Element msg)


white =
    Element.rgb 1 1 1


darkGrey =
    Element.rgb (186 / 255) (189 / 255) (182 / 255)


charcoal =
    Element.rgb
        (136 / 255)
        (138 / 255)
        (133 / 255)


{-| -}
placeholder : List (Attribute msg) -> Element msg -> Placeholder msg
placeholder =
    Placeholder


{-| Every input has a required `label`.
-}
type Label msg
    = Label Internal.Grid.RelativePosition (List (Attribute msg)) (Element msg)


{-| -}
type Notice msg
    = Notice Internal.Grid.RelativePosition (List (Attribute msg)) (Element msg)


{-| -}
labelRight : List (Attribute msg) -> Element msg -> Label msg
labelRight =
    Label Internal.Grid.OnRight


{-| -}
labelLeft : List (Attribute msg) -> Element msg -> Label msg
labelLeft =
    Label Internal.Grid.OnLeft


{-| -}
labelAbove : List (Attribute msg) -> Element msg -> Label msg
labelAbove =
    Label Internal.Grid.Above


{-| -}
labelBelow : List (Attribute msg) -> Element msg -> Label msg
labelBelow =
    Label Internal.Grid.Below


{-| -}
warningRight : List (Attribute msg) -> Element msg -> Notice msg
warningRight =
    Notice Internal.Grid.OnRight


{-| -}
warningLeft : List (Attribute msg) -> Element msg -> Notice msg
warningLeft =
    Notice Internal.Grid.OnLeft


{-| -}
warningAbove : List (Attribute msg) -> Element msg -> Notice msg
warningAbove =
    Notice Internal.Grid.Above


{-| -}
warningBelow : List (Attribute msg) -> Element msg -> Notice msg
warningBelow =
    Notice Internal.Grid.Below


{-| -}
errorRight : List (Attribute msg) -> Element msg -> Notice msg
errorRight =
    Notice Internal.Grid.OnRight


{-| -}
errorLeft : List (Attribute msg) -> Element msg -> Notice msg
errorLeft =
    Notice Internal.Grid.OnLeft


{-| -}
errorAbove : List (Attribute msg) -> Element msg -> Notice msg
errorAbove =
    Notice Internal.Grid.Above


{-| -}
errorBelow : List (Attribute msg) -> Element msg -> Notice msg
errorBelow =
    Notice Internal.Grid.Below


{-| A standard button.

The `onPress` handler will be fired either `onClick` or when the element is focused and the enter key has been pressed.

    import Element.Input as Input

    Input.button []
        { onPress = Just ClickMsg
        , label = text "My Button"
        }

`onPress` takes a `Maybe msg`. If you provide the value `Nothing`, then the button will be disabled.

-}
button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button attrs { onPress, label } =
    Internal.element Internal.noStyleSheet
        Internal.asEl
        -- We don't explicitly label this node as a button,
        -- because buttons fire a bunch of times when you hold down the enter key.
        -- We'd like to fire just once on the enter key, which means using keyup instead of keydown.
        -- Because we have no way to disable keydown, though our messages get doubled.
        Nothing
        (Element.width Element.shrink
            :: Element.height Element.shrink
            :: Internal.Class "x-content-align" classes.contentCenterX
            :: Internal.Class "y-content-align" classes.contentCenterY
            :: Internal.Class "button" "se-button"
            :: Element.pointer
            :: focusDefault attrs
            :: Internal.Describe Internal.Button
            :: Internal.Attr (Html.Attributes.tabindex 0)
            :: (case onPress of
                    Nothing ->
                        Internal.Attr (Html.Attributes.disabled True) :: attrs

                    Just msg ->
                        Events.onClick msg
                            :: onEnter msg
                            :: attrs
               )
        )
        (Internal.Unkeyed [ label ])


focusDefault attrs =
    if List.any hasFocusStyle attrs then
        Internal.NoAttribute
    else
        Internal.htmlClass "focusable"


hasFocusStyle attr =
    case attr of
        Internal.StyleClass (Internal.PseudoSelector Internal.Focus _) ->
            True

        _ ->
            False


{-| -}
type alias Checkbox msg =
    { onChange : Maybe (Bool -> msg)
    , icon : Maybe (Element msg)
    , checked : Bool
    , label : Label msg
    }


{-| -}
checkbox :
    List (Attribute msg)
    ->
        { onChange : Maybe (Bool -> msg)
        , icon : Maybe (Element msg)
        , checked : Bool
        , label : Label msg
        }
    -> Element msg
checkbox attrs { label, icon, checked, onChange } =
    let
        input =
            ( Just "div"
            , [ Internal.Attr <|
                    Html.Attributes.attribute "role" "checkbox"
              , Internal.Attr <|
                    Html.Attributes.attribute "aria-checked" <|
                        if checked then
                            "true"
                        else
                            "false"
              , Element.centerY
              , Element.height Element.fill
              , Element.width Element.shrink
              ]
            , [ case icon of
                    Nothing ->
                        defaultCheckbox checked

                    Just actualIcon ->
                        actualIcon
              ]
            )

        attributes =
            (case onChange of
                Nothing ->
                    [ Internal.Attr (Html.Attributes.disabled True)
                    , Region.announce
                    ]

                Just checkMsg ->
                    [ Internal.Attr (Html.Events.onClick (checkMsg (not checked)))
                    , Region.announce
                    , onKeyLookup <|
                        \code ->
                            if code == enter then
                                Just <| checkMsg (not checked)
                            else if code == space then
                                Just <| checkMsg (not checked)
                            else
                                Nothing
                    ]
            )
                ++ (tabindex 0 :: Element.pointer :: Element.alignLeft :: Element.width Element.fill :: attrs)
    in
    Internal.Grid.relative (Just "label")
        attributes
        ({ right = Nothing
         , left = Nothing
         , primary = input
         , defaultWidth = Internal.Fill 1
         , below = Nothing
         , above = Nothing
         , inFront = Nothing
         }
            |> (\group ->
                    case label of
                        Label position labelAttrs child ->
                            place position
                                { layout = Internal.Grid.GridElement
                                , child = [ child ]
                                , attrs = Element.alignLeft :: labelAttrs
                                , width =
                                    case position of
                                        Internal.Grid.Above ->
                                            2

                                        Internal.Grid.Below ->
                                            2

                                        _ ->
                                            1
                                , height = 1
                                }
                                group
               )
         -- |> (\group ->
         --         -- case notice of
         --             -- Nothing ->
         --                 group
         --             -- Just (Notice position labelAttrs child) ->
         --             --     place position
         --             --         { layout = Internal.Grid.GridElement
         --             --         , child = [ child ]
         --             --         , attrs = Element.alignLeft :: labelAttrs
         --             --         , width =
         --             --             case position of
         --             --                 Internal.Grid.Above ->
         --             --                     2
         --             --                 Internal.Grid.Below ->
         --             --                     2
         --             --                 _ ->
         --             --                     1
         --             --         , height = 1
         --             --         }
         --             --         group
         --    )
        )


place : Internal.Grid.RelativePosition -> Internal.Grid.PositionedElement aligned msg -> Internal.Grid.Around aligned msg -> Internal.Grid.Around aligned msg
place position el group =
    case position of
        Internal.Grid.Above ->
            case group.above of
                Nothing ->
                    { group | above = Just el }

                Just existing ->
                    { group
                        | above =
                            Just
                                { el
                                    | child = el.child ++ existing.child
                                    , layout = Internal.Grid.Row
                                }
                    }

        Internal.Grid.Below ->
            case group.below of
                Nothing ->
                    { group | below = Just el }

                Just existing ->
                    { group
                        | below =
                            Just
                                { el
                                    | child = el.child ++ existing.child
                                    , layout = Internal.Grid.Row
                                }
                    }

        Internal.Grid.OnRight ->
            case group.right of
                Nothing ->
                    { group | right = Just el }

                Just existing ->
                    { group
                        | right =
                            Just
                                { el
                                    | child = el.child ++ existing.child
                                    , layout = Internal.Grid.Column
                                }
                    }

        Internal.Grid.OnLeft ->
            case group.left of
                Nothing ->
                    { group | left = Just el }

                Just existing ->
                    { group
                        | left =
                            Just
                                { el
                                    | child = el.child ++ existing.child
                                    , layout = Internal.Grid.Column
                                }
                    }

        Internal.Grid.InFront ->
            case group.inFront of
                Nothing ->
                    { group | inFront = Just el }

                Just existing ->
                    { group
                        | inFront =
                            Just
                                { el
                                    | child = el.child ++ existing.child
                                    , layout = Internal.Grid.GridElement
                                }
                    }



-- {-| -}
-- type alias Slider msg =
--     { onChange : Maybe (Int -> msg)
--     , range : ( Int, Int )
--     , value : Int
--     , label : Label msg
--     , notice : Maybe (Notice msg)
--     }
-- sliderX : List (Attribute msg) -> Slider msg -> Element msg
-- sliderX attributes input =
--     let
--         behavior =
--             case input.onChange of
--                 Nothing ->
--                     [ Internal.Attr (Html.Attributes.disabled True) ]
--                 Just changeCoord ->
--                     [ Events.onMouseCoords (\{ x, y } -> changeCoord x)
--                     ]
--         min =
--             Tuple.first input.range
--         max =
--             Tuple.second input.range
--         percentage =
--             ((toFloat input.value - toFloat min) / (toFloat max - toFloat min)) + toFloat min
--         icon =
--             Element.el
--                 [ Element.width (Element.px 10)
--                 , Element.height (Element.px 10)
--                 , Background.color lightBlue
--                 , Border.rounded 5
--                 , Element.alignLeft
--                 , Element.moveUp 5
--                 , Element.pointer
--                 , Element.moveRight percentage
--                 ]
--                 Element.none
--         controls =
--             Internal.el
--                 Nothing
--                 ([ Background.color grey
--                  , Element.width Element.fill
--                  , Element.height (Element.px 1)
--                  ]
--                     ++ behavior
--                     ++ attributes
--                 )
--                 (Internal.Unkeyed [ icon ])
--     in
--     positionLabels
--         [ Element.width Element.fill ]
--         input.label
--         input.notice
--         controls
-- type TextType
--     = Plain
--     | Username
--     | NewPassword
--     | CurrentPassword
--     | Email
--     | Search
--     | SpellChecked
--     | Multiline
--     | SpellCheckedMultiline


type alias TextInput =
    { type_ : TextKind
    , spellchecked : Bool
    , autofill : Maybe String
    }


type TextKind
    = TextInputNode String
    | TextArea


{-| -}
type alias Text msg =
    { onChange : Maybe (String -> msg)
    , text : String
    , placeholder : Maybe (Placeholder msg)
    , label : Label msg
    }


type Padding
    = Padding Int Int Int Int


{-|

    attributes

    <parent>
        attribute::width/height fill
        attribtue::alignment
        attribute::spacing
        attribute::fontsize/family/lineheight
        <el-wrapper>
                attribute::nearby(placeholder)
                attribute::width/height fill
                inFront ->
                    placeholder
                        attribute::padding


            <input>
                textarea ->
                    special height for height-content
                        attribtue::padding
                        attribute::lineHeight
                attributes
        <label>

-}
textHelper : TextInput -> List (Attribute msg) -> Text msg -> Element msg
textHelper textInput attrs textOptions =
    let
        attributes =
            Element.width Element.fill :: (defaultTextBoxStyle ++ attrs)

        behavior =
            case textOptions.onChange of
                Nothing ->
                    [ Internal.Attr (Html.Attributes.disabled True) ]

                Just checkMsg ->
                    [ Internal.Attr (Html.Events.onInput checkMsg) ]

        noNearbys =
            List.filter (not << forNearby) attributes

        forNearby attr =
            case attr of
                Internal.Nearby _ _ ->
                    True

                _ ->
                    False

        ( inputNode, inputAttrs, inputChildren ) =
            case textInput.type_ of
                TextInputNode inputType ->
                    ( "input"
                    , [ value textOptions.text
                      , Internal.Attr (Html.Attributes.type_ inputType)
                      , spellcheck textInput.spellchecked
                      , case textInput.autofill of
                            Nothing ->
                                Internal.NoAttribute

                            Just fill ->
                                autofill fill
                      ]
                        ++ noNearbys
                    , []
                    )

                TextArea ->
                    let
                        { maybePadding, heightContent, maybeSpacing, adjustedAttributes } =
                            attributes
                                |> List.foldr
                                    (\attr found ->
                                        case attr of
                                            Internal.Describe _ ->
                                                found

                                            Internal.Height val ->
                                                case found.heightContent of
                                                    Nothing ->
                                                        case val of
                                                            Internal.Content ->
                                                                { found
                                                                    | heightContent = Just val
                                                                    , adjustedAttributes = attr :: found.adjustedAttributes
                                                                }

                                                            _ ->
                                                                { found | heightContent = Just val }

                                                    Just i ->
                                                        found

                                            Internal.StyleClass (Internal.PaddingStyle t r b l) ->
                                                case found.maybePadding of
                                                    Nothing ->
                                                        { found
                                                            | maybePadding = Just (Padding t r b l)
                                                            , adjustedAttributes = attr :: found.adjustedAttributes
                                                        }

                                                    _ ->
                                                        found

                                            Internal.StyleClass (Internal.SpacingStyle x y) ->
                                                case found.maybeSpacing of
                                                    Nothing ->
                                                        { found
                                                            | maybeSpacing = Just y
                                                            , adjustedAttributes = attr :: found.adjustedAttributes
                                                        }

                                                    _ ->
                                                        found

                                            _ ->
                                                { found | adjustedAttributes = attr :: found.adjustedAttributes }
                                    )
                                    { maybePadding = Nothing
                                    , heightContent = Nothing
                                    , maybeSpacing = Nothing
                                    , adjustedAttributes = []
                                    }

                        -- NOTE: This is where default text spacing is set
                        spacing =
                            Maybe.withDefault 5 maybeSpacing
                    in
                    ( "textarea"
                    , [ spellcheck textInput.spellchecked
                      , Maybe.map autofill textInput.autofill
                            |> Maybe.withDefault Internal.NoAttribute
                      , case heightContent of
                            Nothing ->
                                Internal.NoAttribute

                            Just Internal.Content ->
                                let
                                    newlineCount =
                                        String.lines textOptions.text
                                            |> List.length
                                            |> (\x ->
                                                    if x < 1 then
                                                        1
                                                    else
                                                        x
                                               )

                                    heightValue count =
                                        case maybePadding of
                                            Nothing ->
                                                "calc(" ++ String.fromInt count ++ "em + " ++ String.fromInt (count * spacing) ++ "px)"

                                            Just (Padding t r b l) ->
                                                "calc(" ++ String.fromInt count ++ "em + " ++ String.fromInt ((t + b) + (count * spacing)) ++ "px)"
                                in
                                Internal.StyleClass
                                    (Internal.Single ("textarea-height-" ++ String.fromInt newlineCount)
                                        "height"
                                        (heightValue newlineCount)
                                    )

                            Just x ->
                                Internal.Height x
                      ]
                        ++ adjustedAttributes
                    , [ Internal.unstyled (Html.text textOptions.text) ]
                    )

        attributesFromChild =
            Internal.get attributes <|
                \attr ->
                    case attr of
                        Internal.Width (Internal.Fill _) ->
                            True

                        Internal.Height (Internal.Fill _) ->
                            True

                        Internal.AlignX _ ->
                            True

                        Internal.AlignY _ ->
                            True

                        Internal.StyleClass (Internal.SpacingStyle _ _) ->
                            True

                        Internal.StyleClass (Internal.FontSize _) ->
                            True

                        Internal.StyleClass (Internal.FontFamily _ _) ->
                            True

                        _ ->
                            False

        nearbys =
            Internal.get attributes <|
                \attr ->
                    case attr of
                        Internal.Nearby _ _ ->
                            True

                        _ ->
                            False

        parentAttributes =
            Element.spacing 5
                :: Region.announce
                :: attributesFromChild

        inputPadding =
            Internal.get attributes <|
                \attr ->
                    case attr of
                        Internal.StyleClass (Internal.PaddingStyle _ _ _ _) ->
                            True

                        _ ->
                            False

        inputElement =
            ( Just inputNode
            , List.concat
                [ [ focusDefault attrs
                  ]
                , inputAttrs
                , behavior
                ]
            , inputChildren
            )
    in
    onGrid (Internal.Class "cursor" "cursor-text" :: parentAttributes)
        (List.filterMap identity
            [ case textOptions.label of
                Label pos labelAttrs child ->
                    Just ( pos, labelAttrs, child )
            , case textOptions.placeholder of
                Nothing ->
                    case nearbys of
                        [] ->
                            Nothing

                        actualNearbys ->
                            Just
                                ( Internal.Grid.InFront
                                , actualNearbys
                                , Internal.Empty
                                )

                -- nearbys
                Just (Placeholder placeholderAttrs placeholderEl) ->
                    if String.trim textOptions.text == "" then
                        Just
                            ( Internal.Grid.InFront
                            , Font.color charcoal
                                :: Internal.Class "text-selection" classes.noTextSelection
                                :: defaultTextPadding
                                :: Element.height Element.fill
                                :: Element.width Element.fill
                                :: (inputPadding
                                        ++ nearbys
                                        ++ placeholderAttrs
                                   )
                            , placeholderEl
                            )
                    else
                        Nothing
            ]
        )
        inputElement


{-| -}
text :
    List (Attribute msg)
    ->
        { onChange : Maybe (String -> msg)
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        }
    -> Element msg
text =
    textHelper
        { type_ = TextInputNode "text"
        , spellchecked = False
        , autofill = Nothing
        }


{-| -}
spellChecked :
    List (Attribute msg)
    ->
        { onChange : Maybe (String -> msg)
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        }
    -> Element msg
spellChecked =
    textHelper
        { type_ = TextInputNode "text"
        , spellchecked = True
        , autofill = Nothing
        }


{-| -}
search :
    List (Attribute msg)
    ->
        { onChange : Maybe (String -> msg)
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        }
    -> Element msg
search =
    textHelper
        { type_ = TextInputNode "search"
        , spellchecked = False
        , autofill = Nothing
        }


{-| A password input that allows the browser to autofill.

It's `newPassword` instead of just `password` because it gives the browser a hint on what type of password input it is.

A password takes all the arguments a normal `Input.text` would, and also `show`, which will remove the password mask (e.g. `****` vs `pass1234`)

-}
newPassword :
    List (Attribute msg)
    ->
        { onChange : Maybe (String -> msg)
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        , show : Bool
        }
    -> Element msg
newPassword attrs pass =
    textHelper
        { type_ =
            TextInputNode <|
                if pass.show then
                    "text"
                else
                    "password"
        , spellchecked = False
        , autofill = Just "new-password"
        }
        attrs
        { onChange = pass.onChange
        , text = pass.text
        , placeholder = pass.placeholder
        , label = pass.label
        }


{-| -}
currentPassword :
    List (Attribute msg)
    ->
        { onChange : Maybe (String -> msg)
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        , show : Bool
        }
    -> Element msg
currentPassword attrs pass =
    textHelper
        { type_ =
            TextInputNode <|
                if pass.show then
                    "text"
                else
                    "password"
        , spellchecked = False
        , autofill = Just "current-password"
        }
        attrs
        { onChange = pass.onChange
        , text = pass.text
        , placeholder = pass.placeholder
        , label = pass.label
        }


{-| -}
username :
    List (Attribute msg)
    ->
        { onChange : Maybe (String -> msg)
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        }
    -> Element msg
username =
    textHelper
        { type_ = TextInputNode "text"
        , spellchecked = False
        , autofill = Just "username"
        }


{-| -}
email :
    List (Attribute msg)
    ->
        { onChange : Maybe (String -> msg)
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        }
    -> Element msg
email =
    textHelper
        { type_ = TextInputNode "email"
        , spellchecked = False
        , autofill = Just "email"
        }


{-| -}
multiline :
    List (Attribute msg)
    ->
        { onChange : Maybe (String -> msg)
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        , spellcheck : Bool
        }
    -> Element msg
multiline attrs multi =
    textHelper
        { type_ =
            TextArea
        , spellchecked = multi.spellcheck
        , autofill = Nothing
        }
        attrs
        { onChange = multi.onChange
        , text = multi.text
        , placeholder = multi.placeholder
        , label = multi.label
        }


applyLabel : List (Attribute msg) -> Label msg -> Element msg -> Element msg
applyLabel attrs label input =
    case label of
        Label position labelAttrs labelChild ->
            let
                labelElement =
                    Internal.element Internal.noStyleSheet
                        Internal.asEl
                        Nothing
                        labelAttrs
                        (Internal.Unkeyed [ labelChild ])
            in
            case position of
                Internal.Grid.Above ->
                    Internal.element Internal.noStyleSheet
                        Internal.asColumn
                        (Just "label")
                        attrs
                        (Internal.Unkeyed [ labelElement, input ])

                Internal.Grid.Below ->
                    Internal.element Internal.noStyleSheet
                        Internal.asColumn
                        (Just "label")
                        attrs
                        (Internal.Unkeyed [ input, labelElement ])

                Internal.Grid.OnRight ->
                    Internal.element Internal.noStyleSheet
                        Internal.asRow
                        (Just "label")
                        attrs
                        (Internal.Unkeyed [ input, labelElement ])

                Internal.Grid.OnLeft ->
                    Internal.element Internal.noStyleSheet
                        Internal.asRow
                        (Just "label")
                        attrs
                        (Internal.Unkeyed [ labelElement, input ])

                Internal.Grid.InFront ->
                    Internal.element Internal.noStyleSheet
                        Internal.asRow
                        (Just "label")
                        attrs
                        (Internal.Unkeyed [ labelElement, input ])


onGrid attributes elementsOnGrid input =
    let
        nonePositioned =
            { right = Nothing
            , left = Nothing
            , primary = input
            , defaultWidth = Internal.Content
            , below = Nothing
            , above = Nothing
            , inFront = Nothing
            }

        gatherPositioned ( pos, attrs, child ) group =
            place pos
                { layout = Internal.Grid.GridElement
                , child = [ child ]
                , attrs = Element.alignLeft :: attrs
                , width = 1
                , height = 1
                }
                group
    in
    Internal.Grid.relative (Just "label")
        attributes
        (List.foldl gatherPositioned nonePositioned elementsOnGrid)


{-| -}
type alias Radio option msg =
    { onChange : Maybe (option -> msg)
    , options : List (Option option msg)
    , selected : Maybe option
    , label : Label msg
    }


{-| Add choices to your radio and select menus.
-}
type Option value msg
    = Option value (OptionState -> Element msg)


{-| -}
type OptionState
    = Idle
    | Focused
    | Selected


{-| -}
option : value -> Element msg -> Option value msg
option val txt =
    Option val (defaultRadioOption txt)


{-| -}
optionWith : value -> (OptionState -> Element msg) -> Option value msg
optionWith val view =
    Option val view


{-|

    Input.radio
        [ padding 10
        , spacing 20
        ]
        { onChange = Just ChooseLunch
        , selected = Just model.lunch
        , label = Input.labelAbove (text "Lunch")
        , options =
            [ Input.styledChoice Burrito <|
                \selected ->
                    Element.row
                        [ spacing 5 ]
                        [ el None [] <|
                            if selected then
                                text ":D"
                            else
                                text ":("
                        , text "burrito"
                        ]
            , Input.option Taco (text "Taco!")
            , Input.option Gyro (text "Gyro")
            ]
        }

-}
radio : List (Attribute msg) -> Radio option msg -> Element msg
radio =
    radioHelper Column


{-| Same as radio, but displayed as a row
-}
radioRow : List (Attribute msg) -> Radio option msg -> Element msg
radioRow =
    radioHelper Row


defaultRadioOption : Element msg -> OptionState -> Element msg
defaultRadioOption optionLabel status =
    Element.row
        [ Element.spacing 10
        , Element.alignLeft
        , Element.width Element.shrink
        ]
        [ Element.el
            [ Element.width (Element.px 14)
            , Element.height (Element.px 14)
            , Background.color white
            , Border.rounded 7
            , case status of
                Selected ->
                    Internal.htmlClass "focusable"

                _ ->
                    Internal.NoAttribute

            -- , Border.shadow <|
            --     -- case status of
            --     --     Idle ->
            --     --         { offset = ( 0, 0 )
            --     --         , blur =
            --     --             1
            --     --         , color = Color.rgb 235 235 235
            --     --         }
            --     --     Focused ->
            --     --         { offset = ( 0, 0 )
            --     --         , blur =
            --     --             0
            --     --         , color = Color.rgba 235 235 235 0
            --     --         }
            --     --     Selected ->
            --     { offset = ( 0, 0 )
            --     , blur =
            --         1
            --     , color = Color.rgba 235 235 235 0
            --     }
            , Border.width <|
                case status of
                    Idle ->
                        1

                    Focused ->
                        1

                    Selected ->
                        5
            , Border.color <|
                case status of
                    Idle ->
                        Element.rgb (208 / 255) (208 / 255) (208 / 255)

                    Focused ->
                        Element.rgb (208 / 255) (208 / 255) (208 / 255)

                    Selected ->
                        Element.rgb (59 / 255) (153 / 255) (252 / 255)
            ]
            Element.none
        , Element.el [ Element.width Element.fill, Internal.htmlClass "unfocusable" ] optionLabel
        ]


radioHelper : Orientation -> List (Attribute msg) -> Radio option msg -> Element msg
radioHelper orientation attrs input =
    let
        renderOption (Option val view) =
            let
                status =
                    if Just val == input.selected then
                        Selected
                    else
                        Idle
            in
            Element.el
                [ Element.pointer
                , case orientation of
                    Row ->
                        Element.width Element.shrink

                    Column ->
                        Element.width Element.fill
                , case input.onChange of
                    Nothing ->
                        Internal.NoAttribute

                    Just send ->
                        Events.onClick (send val)
                , case status of
                    Selected ->
                        Internal.Attr <|
                            Html.Attributes.attribute "aria-checked"
                                "true"

                    _ ->
                        Internal.Attr <|
                            Html.Attributes.attribute "aria-checked"
                                "false"
                , Internal.Attr <|
                    Html.Attributes.attribute "role" "radio"
                ]
                (view status)

        optionArea =
            case orientation of
                Row ->
                    row attrs
                        (List.map renderOption input.options)

                Column ->
                    column attrs
                        (List.map renderOption input.options)

        prevNext =
            case input.options of
                [] ->
                    Nothing

                (Option val _) :: _ ->
                    List.foldl track ( NotFound, val, val ) input.options
                        |> (\( found, b, a ) ->
                                case found of
                                    NotFound ->
                                        Just ( b, val )

                                    BeforeFound ->
                                        Just ( b, val )

                                    _ ->
                                        Just ( b, a )
                           )

        track opt ( found, prev, nxt ) =
            case opt of
                Option val _ ->
                    case found of
                        NotFound ->
                            if Just val == input.selected then
                                ( BeforeFound, prev, nxt )
                            else
                                ( found, val, nxt )

                        BeforeFound ->
                            ( AfterFound, prev, val )

                        AfterFound ->
                            ( found, prev, nxt )

        inputVisible =
            List.isEmpty <|
                Internal.get attrs <|
                    \attr ->
                        case attr of
                            Internal.StyleClass (Internal.Transparency _ _) ->
                                True

                            Internal.Class "hidden" "hidden" ->
                                True

                            _ ->
                                False

        labelVisible =
            case input.label of
                Label _ labelAttrs _ ->
                    List.isEmpty <|
                        Internal.get labelAttrs <|
                            \attr ->
                                case attr of
                                    Internal.StyleClass (Internal.Transparency _ _) ->
                                        True

                                    Internal.Class "hidden" "hidden" ->
                                        True

                                    _ ->
                                        False

        hideIfEverythingisInvisible =
            if not labelVisible && not inputVisible then
                let
                    pseudos =
                        List.filterMap
                            (\attr ->
                                case attr of
                                    Internal.StyleClass style ->
                                        case style of
                                            Internal.PseudoSelector pseudo styles ->
                                                let
                                                    transparent =
                                                        List.filter forTransparency styles

                                                    forTransparency psuedoStyle =
                                                        case psuedoStyle of
                                                            Internal.Transparency _ _ ->
                                                                True

                                                            _ ->
                                                                False
                                                in
                                                case transparent of
                                                    [] ->
                                                        Nothing

                                                    _ ->
                                                        Just <| Internal.StyleClass <| Internal.PseudoSelector pseudo transparent

                                            _ ->
                                                Nothing

                                    _ ->
                                        Nothing
                            )
                            attrs
                in
                Internal.StyleClass (Internal.Transparency "transparent" 1.0) :: pseudos
            else
                []

        events =
            Internal.get
                attrs
            <|
                \attr ->
                    case attr of
                        Internal.Width (Internal.Fill _) ->
                            True

                        Internal.Height (Internal.Fill _) ->
                            True

                        Internal.Attr _ ->
                            True

                        _ ->
                            False
    in
    applyLabel
        (case input.onChange of
            Nothing ->
                Element.alignLeft :: Region.announce :: hideIfEverythingisInvisible ++ events

            Just onChange ->
                List.filterMap identity
                    [ Just Element.alignLeft
                    , Just (tabindex 0)
                    , Just (Internal.htmlClass "focus")
                    , Just Region.announce
                    , Just <|
                        Internal.Attr <|
                            Html.Attributes.attribute "role" "radiogroup"
                    , case prevNext of
                        Nothing ->
                            Nothing

                        Just ( prev, next ) ->
                            Just
                                (onKeyLookup <|
                                    \code ->
                                        if code == leftArrow then
                                            Just (onChange prev)
                                        else if code == upArrow then
                                            Just (onChange prev)
                                        else if code == rightArrow then
                                            Just (onChange next)
                                        else if code == downArrow then
                                            Just (onChange next)
                                        else if code == space then
                                            case input.selected of
                                                Nothing ->
                                                    Just (onChange prev)

                                                _ ->
                                                    Nothing
                                        else
                                            Nothing
                                )
                    ]
                    ++ events
                    ++ hideIfEverythingisInvisible
        )
        input.label
        optionArea


type Found
    = NotFound
    | BeforeFound
    | AfterFound


type Orientation
    = Row
    | Column


column : List (Attribute msg) -> List (Internal.Element msg) -> Internal.Element msg
column attributes children =
    Internal.element Internal.noStyleSheet
        Internal.asColumn
        Nothing
        (Element.height Element.shrink
            :: Element.width Element.fill
            :: attributes
        )
        (Internal.Unkeyed children)


row : List (Attribute msg) -> List (Internal.Element msg) -> Internal.Element msg
row attributes children =
    Internal.element
        Internal.noStyleSheet
        Internal.asRow
        Nothing
        (Element.width Element.fill
            :: attributes
        )
        (Internal.Unkeyed children)



{- Event Handlers -}


{-| -}
onEnter : msg -> Attribute msg
onEnter msg =
    onKey enter msg


{-| -}
onSpace : msg -> Attribute msg
onSpace msg =
    onKey space msg


{-| -}
onUpArrow : msg -> Attribute msg
onUpArrow msg =
    onKey upArrow msg


{-| -}
onRightArrow : msg -> Attribute msg
onRightArrow msg =
    onKey rightArrow msg


{-| -}
onLeftArrow : msg -> Attribute msg
onLeftArrow msg =
    onKey leftArrow msg


{-| -}
onDownArrow : msg -> Attribute msg
onDownArrow msg =
    onKey downArrow msg


enter : String
enter =
    "Enter"


tab : String
tab =
    "Tab"


delete : String
delete =
    "Delete"


backspace : String
backspace =
    "Backspace"


upArrow : String
upArrow =
    "ArrowUp"


leftArrow : String
leftArrow =
    "ArrowLeft"


rightArrow : String
rightArrow =
    "ArrowRight"


downArrow : String
downArrow =
    "ArrowDown"


space : String
space =
    " "


{-| -}
onKey : String -> msg -> Attribute msg
onKey desiredCode msg =
    let
        decode code =
            if code == desiredCode then
                Json.succeed msg
            else
                Json.fail "Not the enter key"

        isKey =
            Json.field "key" Json.string
                |> Json.andThen decode
    in
    Internal.Attr <|
        Html.Events.preventDefaultOn "keyup"
            (Json.map (\fired -> ( fired, True )) isKey)



-- preventKeydown : String -> a -> Attribute a
-- preventKeydown desiredCode msg =
--     let
--         decode code =
--             if code == desiredCode then
--                 Json.succeed msg
--             else
--                 Json.fail "Not the enter key"
--         isKey =
--             Json.field "key" Json.string
--                 |> Json.andThen decode
--     in
--     Events.onWithOptions "keydown"
--         { stopPropagation = False
--         , preventDefault = True
--         }
--         isKey


{-| -}
onKeyLookup : (String -> Maybe msg) -> Attribute msg
onKeyLookup lookup =
    let
        decode code =
            case lookup code of
                Nothing ->
                    Json.fail "No key matched"

                Just msg ->
                    Json.succeed msg

        isKey =
            Json.field "key" Json.string
                |> Json.andThen decode
    in
    Internal.Attr <| Html.Events.on "keyup" isKey


{-| -}
onFocusOut : msg -> Attribute msg
onFocusOut msg =
    Internal.Attr <| Html.Events.on "focusout" (Json.succeed msg)


{-| -}
onFocusIn : msg -> Attribute msg
onFocusIn msg =
    Internal.Attr <| Html.Events.on "focusin" (Json.succeed msg)


type_ : String -> Attribute msg
type_ =
    Internal.Attr << Html.Attributes.type_



-- checked : Bool -> Attribute msg
-- checked =
--     Internal.Attr << Html.Attributes.checked


selected : Bool -> Attribute msg
selected =
    Internal.Attr << Html.Attributes.selected


name : String -> Attribute msg
name =
    Internal.Attr << Html.Attributes.name


value : String -> Attribute msg
value =
    Internal.Attr << Html.Attributes.value


textValue : String -> Attribute msg
textValue =
    Internal.Attr << Html.Attributes.defaultValue


tabindex : Int -> Attribute msg
tabindex =
    Internal.Attr << Html.Attributes.tabindex


disabled : Bool -> Attribute msg
disabled =
    Internal.Attr << Html.Attributes.disabled


spellcheck : Bool -> Attribute msg
spellcheck =
    Internal.Attr << Html.Attributes.spellcheck


readonly : Bool -> Attribute msg
readonly =
    Internal.Attr << Html.Attributes.readonly


autofill : String -> Attribute msg
autofill =
    Internal.Attr << Html.Attributes.attribute "autocomplete"


{-| -}
focusedOnLoad : Attribute msg
focusedOnLoad =
    Internal.Attr <| Html.Attributes.autofocus True



{- Style Defaults -}


defaultTextBoxStyle : List (Attribute msg)
defaultTextBoxStyle =
    [ defaultTextPadding
    , Border.rounded 3
    , Border.color darkGrey
    , Background.color white
    , Border.width 1
    , Element.spacing 3
    ]


defaultTextPadding : Attribute msg
defaultTextPadding =
    Element.paddingXY 12 7


defaultCheckbox : Bool -> Element msg
defaultCheckbox checked =
    Element.el
        [ Internal.htmlClass "focusable"
        , Element.width (Element.px 14)
        , Element.height (Element.px 14)
        , Font.color white
        , Font.size 9
        , Font.center
        , Border.rounded 3
        , Border.color <|
            if checked then
                Element.rgb (59 / 255) (153 / 255) (252 / 255)
            else
                Element.rgb (211 / 255) (211 / 255) (211 / 255)
        , Border.shadow <|
            { offset = ( 0, 0 )
            , blur = 1
            , size = 1
            , color =
                if checked then
                    Element.rgba (238 / 255) (238 / 255) (238 / 255) 0
                else
                    Element.rgb (238 / 255) (238 / 255) (238 / 255)
            }
        , Background.color <|
            if checked then
                Element.rgb (59 / 255) (153 / 255) (252 / 255)
            else
                white
        , Border.width <|
            if checked then
                0
            else
                1
        ]
        (if checked then
            Element.el
                [ Border.color white
                , Element.height (Element.px 6)
                , Element.width (Element.px 9)
                , Element.rotate (degrees -45)
                , Element.centerX
                , Element.centerY
                , Element.moveUp 1
                , Border.widthEach
                    { top = 0
                    , left = 2
                    , bottom = 2
                    , right = 0
                    }
                ]
                Element.none
         else
            Element.none
        )
