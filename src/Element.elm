module Element
    exposing
        ( Attr
        , Attribute
        , Color
        , Column
        , Decoration
        , Device
        , DeviceClass(..)
        , Element
        , FocusStyle
        , IndexedColumn
        , IndexedTable
        , Length
        , Link
        , Option
        , Table
        , above
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , alpha
        , behind
        , below
        , centerX
        , centerY
        , classifyDevice
        , clip
        , clipX
        , clipY
        , column
        , download
        , downloadAs
        , el
        , fill
        , fillPortion
        , focusStyle
        , focused
        , forceHover
        , height
        , html
        , htmlAttribute
        , image
        , inFront
        , indexedTable
        , layout
        , layoutWith
        , link
        , map
        , mapAttribute
        , maximum
        , minimum
        , modular
        , mouseDown
        , mouseOver
        , moveDown
        , moveLeft
        , moveRight
        , moveUp
        , newTabLink
        , noHover
        , noStaticStyleSheet
        , none
        , onLeft
        , onRight
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , pointer
        , px
        , rgb
        , rgba
        , rotate
        , row
        , scale
        , scrollbarX
        , scrollbarY
        , scrollbars
        , shrink
        , spaceEvenly
        , spacing
        , spacingXY
        , table
        , text
        , textColumn
        , transparent
        , width
        )

{-|


# Basic Elements

@docs Element, Attribute, none, text, el


# Rows and Columns

Rows and columns are the most common layouts.

@docs row, column


# Text Layout

Text needs it's own layout primitives.

@docs paragraph, textColumn


# Data Table

@docs Table, Column, table

@docs IndexedTable, IndexedColumn, indexedTable


# Size

@docs width, height, Length, px, shrink, fill, fillPortion, minimum, maximum


## Padding and Spacing

There's no concept of margin in `style-elements`, instead we have padding and spacing.

Padding is what you'd expect, the distance between the outer edge and the content, and spacing is the space between children.

So, if we have the following row, with some padding and spacing.

    Element.row [ padding 10, spacing 7 ]
        [ Element.el [] none
        , Element.el [] none
        , Element.el [] none
        ]

Here's what we can expect.

<img src="https://mdgriffith.gitbooks.io/style-elements/content/assets/spacing-400.png" alt="Three boxes spaced 7 pixels apart.  There's a 10 pixel distance from the edge of the parent to the boxes." />

@docs padding, paddingXY, paddingEach

@docs spacing, spacingXY, spaceEvenly


## Alignment

Alignment can be used to align an `Element` within another `Element`.

    Element.el [ centerX, alignTop ] (text "I'm centered and aligned top!")

If alignment is set on elements in a layout such as `row`, then the element will push the other elements in that direction. Here's an example.

    Element.row []
        [ Element.el [] Element.none
        , Element.el [ alignLeft ] Element.none
        , Element.el [ centerX ] Element.none
        , Element.el [ alignRight ] Element.none
        ]

will result in a layout like

    |-|-|     |-|        |-|

Where there are two elements on the left, one in the center, and on on the right.

@docs centerX, centerY, alignLeft, alignRight, alignTop, alignBottom


# Transparency

@docs transparent, alpha, pointer


# Adjustment

@docs moveUp, moveDown, moveRight, moveLeft, rotate, scale


# Clipping and Scrollbars

Clip the content if it overflows.

@docs clip, clipX, clipY

If these are present, the element will add a scrollbar if necessary.

@docs scrollbars, scrollbarX, scrollbarY


# Rendering

@docs layout, layoutWith, Option, noStaticStyleSheet, forceHover, noHover, focusStyle, FocusStyle


# Links and Images

@docs Link, link, newTabLink, download, downloadAs

@docs image


# Color

@docs Color, rgba, rgb


# Nearby Elements

Let's say we want a dropdown menu. Essentially we want to say: _put this element below this other element, but don't affect the layout when you do_.

    Element.row []
        [ Element.el
            [ Element.below (Element.text "I'm below!")
            ]
            (Element.text "I'm normal!")
        ]

This will result in

    |- I'm normal! -|
       I'm below

Where `"I'm Below"` doesn't change the size of `Element.row`.

This is very useful for things like dropdown menus or tooltips.

@docs above, below, onRight, onLeft, inFront, behind


# Temporary Styling

@docs Attr, Decoration, mouseOver, mouseDown, focused


# Responsiveness

@docs Device, DeviceClass, classifyDevice


# Scaling

@docs modular


## Mapping

@docs map, mapAttribute


## Compatibility

@docs html, htmlAttribute

-}

import Html exposing (Html)
import Html.Attributes
import Internal.Flag as Flag exposing (Flag)
import Internal.Model as Internal
import Internal.Style exposing (classes)


{-| -}
type alias Color =
    Internal.Color


{-| -}
rgba : Float -> Float -> Float -> Float -> Color
rgba =
    Internal.Rgba


{-| -}
rgb : Float -> Float -> Float -> Color
rgb r g b =
    Internal.Rgba r g b 1


{-| The basic building block of your layout. Here we create a

    import Element

    view =
        Element.el [] (Element.text "Hello!")

-}
type alias Element msg =
    Internal.Element msg


{-| Standard attribute which cannot be a decoration.
-}
type alias Attribute msg =
    Internal.Attribute () msg


{-| This is a special attribute that counts as both a `Attribute msg` and a `Decoration`.
-}
type alias Attr decorative msg =
    Internal.Attribute decorative msg


{-| Only decorations
-}
type alias Decoration =
    Internal.Attribute Never Never


{-| -}
html : Html msg -> Element msg
html =
    Internal.unstyled


{-| -}
htmlAttribute : Html.Attribute msg -> Attribute msg
htmlAttribute =
    Internal.Attr


{-| -}
map : (msg -> msg1) -> Element msg -> Element msg1
map =
    Internal.map


{-| -}
mapAttribute : (msg -> msg1) -> Attribute msg -> Attribute msg1
mapAttribute =
    Internal.mapAttr


{-| -}
type alias Length =
    Internal.Length


{-| -}
px : Int -> Length
px =
    Internal.Px


{-| Shrink to an element to fit it's contents.
-}
shrink : Length
shrink =
    Internal.Content


{-| Fill the available space. The available space will be split evenly between elements that have `width fill`.
-}
fill : Length
fill =
    Internal.Fill 1


{-| -}
minimum : Int -> Length -> Length
minimum i l =
    Internal.Min i l


{-| -}
maximum : Int -> Length -> Length
maximum i l =
    Internal.Max i l


{-| Sometimes you may not want to split available space evenly. In this case you can use `fillPortion` to define which elements should have what portion of the available space.

So, two elements, one with `width (fillPortion 2)` and one with `width (fillPortion 3)`. The first would get 2 portions of the available space, while the second would get 3.

Also: `fill == fillPortion 1`

-}
fillPortion : Int -> Length
fillPortion =
    Internal.Fill


{-| This is your top level node where you can turn `Element` into `Html`.
-}
layout : List (Attribute msg) -> Element msg -> Html msg
layout =
    layoutWith { options = [] }


{-| -}
layoutWith : { options : List Option } -> List (Attribute msg) -> Element msg -> Html msg
layoutWith { options } attrs child =
    Internal.renderRoot options
        (Internal.htmlClass
            (String.join " "
                [ classes.root
                , classes.any
                , classes.single
                , classes.contentCenterX
                , classes.contentCenterY
                ]
            )
            :: (Internal.rootStyle ++ attrs)
        )
        child


{-| -}
type alias Option =
    Internal.Option


{-| Style elements embeds two StyleSheets, one that is constant, and one that changes dynamically based on styles collected from the elments being rendered.

This option will stop the static/constant stylesheet from rendering.

Make sure to render the constant/static stylesheet at least once on your page!

-}
noStaticStyleSheet : Option
noStaticStyleSheet =
    Internal.RenderModeOption Internal.NoStaticStyleSheet


{-| -}
defaultFocus :
    { borderColor : Maybe Color
    , backgroundColor : Maybe Color
    , shadow :
        Maybe
            { color : Color
            , offset : ( Int, Int )
            , blur : Int
            , size : Int
            }
    }
defaultFocus =
    Internal.focusDefaultStyle


{-| -}
type alias FocusStyle =
    { borderColor : Maybe Color
    , backgroundColor : Maybe Color
    , shadow :
        Maybe
            { color : Color
            , offset : ( Int, Int )
            , blur : Int
            , size : Int
            }
    }


{-| -}
focusStyle : FocusStyle -> Option
focusStyle =
    Internal.FocusStyleOption


{-| Disable all `mouseOver` styles.
-}
noHover : Option
noHover =
    Internal.HoverOption Internal.NoHover


{-| Any `hover` styles, aka attributes with `mouseOver` in the name, will be always turned on.

This is useful for when you're targeting a platform that has no mouse, such as mobile.

-}
forceHover : Option
forceHover =
    Internal.HoverOption Internal.ForceHover


{-| Nothing to see here!
-}
none : Element msg
none =
    Internal.Empty


{-| Create some plain text.

    text "Hello, you stylish developer!"

**Note** text does not wrap by default. In order to get text to wrap, check out `paragraph`!

-}
text : String -> Element msg
text content =
    Internal.Text content


{-| The basic building block of your layout.

    import Color exposing (blue, darkBlue)
    import Element exposing (Element)
    import Element.Background as Background
    import Element.Border as Border

    myElement : Element msg
    myElement =
        Element.el
            [ Background.color blue
            , Border.color darkBlue
            ]
            (Element.text "You've made a stylish element!")

-}
el : List (Attribute msg) -> Element msg -> Element msg
el attrs child =
    Internal.element Internal.noStyleSheet
        Internal.asEl
        Nothing
        (width shrink
            :: height shrink
            :: attrs
        )
        (Internal.Unkeyed [ child ])


{-| If you want a row of elements, use `row`!
-}
row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs children =
    Internal.element
        Internal.noStyleSheet
        Internal.asRow
        Nothing
        (Internal.htmlClass classes.contentLeft
            :: Internal.htmlClass classes.contentCenterY
            :: width fill
            :: height shrink
            :: attrs
        )
        (Internal.Unkeyed children)


{-| -}
column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs children =
    Internal.element Internal.noStyleSheet
        Internal.asColumn
        Nothing
        (Internal.htmlClass classes.contentTop
            :: Internal.htmlClass classes.contentLeft
            :: height fill
            :: width fill
            :: attrs
        )
        (Internal.Unkeyed children)


{-| -}
type alias Table records msg =
    { data : List records
    , columns : List (Column records msg)
    }


{-| -}
type alias Column record msg =
    { header : Element msg
    , view : record -> Element msg
    }


{-| Show some tabular data.

Start with a list of records and specify how each column should be rendered.

So, if we have a list of `persons`:

    type alias Person =
        { firstName : String
        , lastName : String
        }

    persons : List Person
    persons =
        [ { firstName = "David"
          , lastName = "Bowie"
          }
        , { firstName = "Florence"
          , lastName = "Welch"
          }
        ]

We could render it using

    Element.table []
        { data = persons
        , columns =
            [ { header = Element.text "First Name"
              , view =
                    (\person ->
                        Element.text person.firstName
                    )
              }
            , { header = Element.text "Last Name"
              , view =
                     (\person ->
                        Element.text person.lastName
                     )
              }
            ]
        }

**Note:** Sometimes you might not have a list of records directly in your model. In this case it can be really nice to write a function that transforms some part of your model into a list of records before feeding it into `Element.table`.

-}
table : List (Attribute msg) -> Table data msg -> Element msg
table attrs config =
    tableHelper attrs
        { data = config.data
        , columns =
            List.map InternalColumn config.columns
        }


{-| -}
type alias IndexedTable records msg =
    { data : List records
    , columns : List (IndexedColumn records msg)
    }


{-| -}
type alias IndexedColumn record msg =
    { header : Element msg
    , view : Int -> record -> Element msg
    }


{-| Same as `Element.table` except the `view` for each column will also receive the row index as well as the record.
-}
indexedTable : List (Attribute msg) -> IndexedTable data msg -> Element msg
indexedTable attrs config =
    tableHelper attrs
        { data = config.data
        , columns =
            List.map InternalIndexedColumn config.columns
        }


{-| -}
type alias InternalTable records msg =
    { data : List records
    , columns : List (InternalTableColumn records msg)
    }


{-| -}
type InternalTableColumn record msg
    = InternalIndexedColumn (IndexedColumn record msg)
    | InternalColumn (Column record msg)


tableHelper : List (Attribute msg) -> InternalTable data msg -> Element msg
tableHelper attrs config =
    let
        ( sX, sY ) =
            Internal.getSpacing attrs ( 0, 0 )

        columnHeader col =
            case col of
                InternalIndexedColumn colConfig ->
                    colConfig.header

                InternalColumn colConfig ->
                    colConfig.header

        maybeHeaders =
            List.map columnHeader config.columns
                |> (\headers ->
                        if List.all ((==) Internal.Empty) headers then
                            Nothing
                        else
                            Just (List.indexedMap (\col header -> onGrid 1 (col + 1) header) headers)
                   )

        template =
            Internal.StyleClass Flag.gridTemplate <|
                Internal.GridTemplateStyle
                    { spacing = ( px sX, px sY )
                    , columns = List.repeat (List.length config.columns) (Internal.Fill 1)
                    , rows = List.repeat (List.length config.data) Internal.Content
                    }

        onGrid rowLevel columnLevel elem =
            Internal.element
                Internal.noStyleSheet
                Internal.asEl
                Nothing
                [ Internal.StyleClass Flag.gridPosition
                    (Internal.GridPosition
                        { row = rowLevel
                        , col = columnLevel
                        , width = 1
                        , height = 1
                        }
                    )
                ]
                (Internal.Unkeyed [ elem ])

        add cell columnConfig cursor =
            case columnConfig of
                InternalIndexedColumn col ->
                    { cursor
                        | elements =
                            onGrid cursor.row
                                cursor.column
                                (col.view
                                    (if maybeHeaders == Nothing then
                                        cursor.row - 1
                                     else
                                        cursor.row - 2
                                    )
                                    cell
                                )
                                :: cursor.elements
                        , column = cursor.column + 1
                    }

                InternalColumn col ->
                    { cursor
                        | elements =
                            onGrid cursor.row cursor.column (col.view cell)
                                :: cursor.elements
                        , column = cursor.column + 1
                    }

        build columns rowData cursor =
            let
                newCursor =
                    List.foldl (add rowData)
                        cursor
                        columns
            in
            { newCursor
                | row = cursor.row + 1
                , column = 1
            }

        children =
            List.foldl (build config.columns)
                { elements = []
                , row =
                    if maybeHeaders == Nothing then
                        1
                    else
                        2
                , column = 1
                }
                config.data
    in
    Internal.element Internal.noStyleSheet
        Internal.asGrid
        Nothing
        (width fill
            :: template
            :: attrs
        )
        (Internal.Unkeyed
            (case maybeHeaders of
                Nothing ->
                    children.elements

                Just renderedHeaders ->
                    renderedHeaders ++ children.elements
            )
        )


{-| A paragraph will layout all children as wrapped, inline elements.

    import Element
    import Element.Font as Font

    Element.paragraph []
        [ text "lots of text ...."
        , el [ Font.bold ] (text "this is bold")
        , text "lots of text ...."
        ]

This is really useful when you want to markup text by having some parts be bold, or some be links, or whatever you so desire.

Also, if a child element has `alignLeft` or `alignRight`, then it will be moved to that side and the text will flow around it, (ah yes, `float` behavior).

This makes it particularly easy to do something like a [dropped capital](https://en.wikipedia.org/wiki/Initial).

    import Element
    import Element.Font as Font

    Element.paragraph []
        [ el
            [ alignLeft
            , padding 5
            , Font.lineHeight 1
            ]
            (text "S")
        , text "lots of text ...."
        ]

Which will look something like

<img src="https://mdgriffith.gitbooks.io/style-elements/content/assets/Screen%20Shot%202017-08-25%20at%209.41.52%20PM.png" />

-}
paragraph : List (Attribute msg) -> List (Element msg) -> Element msg
paragraph attrs children =
    Internal.element Internal.noStyleSheet
        Internal.asParagraph
        (Just "p")
        (width fill
            :: spacing 5
            :: attrs
        )
        (Internal.Unkeyed children)


{-| Now that we have a paragraph, we need someway to attach a bunch of paragraph's together.

To do that we can use a `textColumn`.

The main difference between a `column` and a `textColumn` is that `textColumn` will flow the text around elements that have `alignRight` or `alignLeft`, just like we just saw with paragraph.

In the following example, we have a `textColumn` where one child has `alignLeft`.

    Element.textColumn [ spacing 10, padding 10 ]
        [ paragraph [] [ text "lots of text ...." ]
        , el [ alignLeft ] none
        , paragraph [] [ text "lots of text ...." ]
        ]

Which will result in something like:

<img src="https://mdgriffith.gitbooks.io/style-elements/content/assets/Screen%20Shot%202017-08-25%20at%208.42.39%20PM.png" />

-}
textColumn : List (Attribute msg) -> List (Element msg) -> Element msg
textColumn attrs children =
    Internal.element
        Internal.noStyleSheet
        Internal.asTextColumn
        Nothing
        (width
            (fill
                |> minimum 500
                |> maximum 750
            )
            :: attrs
        )
        (Internal.Unkeyed children)


{-| Both a source and a description are required for images.

The description is used to describe the image to people using screen readers.

Leaving leaving the description blank will cause the image to be ignored by assistive technology. This can make sense for images that are purely decorative and add no additional information.

So, take a moment to describe your image as you would to someone who has a harder time seeing.

-}
image : List (Attribute msg) -> { src : String, description : String } -> Element msg
image attrs { src, description } =
    let
        imageAttributes =
            attrs
                |> List.filter
                    (\a ->
                        case a of
                            Internal.Width _ ->
                                True

                            Internal.Height _ ->
                                True

                            _ ->
                                False
                    )
    in
    Internal.element Internal.noStyleSheet
        Internal.asEl
        Nothing
        (clip
            :: attrs
        )
        (Internal.Unkeyed
            [ Internal.element Internal.noStyleSheet
                Internal.asEl
                (Just "img")
                (imageAttributes
                    ++ [ Internal.Attr <| Html.Attributes.src src
                       , Internal.Attr <| Html.Attributes.alt description
                       ]
                )
                (Internal.Unkeyed [])
            ]
        )


{-| -}
type alias Link msg =
    { url : String
    , label : Element msg
    }


{-|

    link []
        { url = "http://google.com"
        , label = text "My Link to Google"
        }

-}
link : List (Attribute msg) -> Link msg -> Element msg
link attrs { url, label } =
    Internal.element Internal.noStyleSheet
        Internal.asEl
        (Just "a")
        (Internal.Attr (Html.Attributes.href url)
            :: Internal.Attr (Html.Attributes.rel "noopener noreferrer")
            :: width shrink
            :: height shrink
            :: Internal.htmlClass classes.contentCenterX
            :: Internal.htmlClass classes.contentCenterY
            :: attrs
        )
        (Internal.Unkeyed [ label ])


{-| -}
newTabLink : List (Attribute msg) -> Link msg -> Element msg
newTabLink attrs { url, label } =
    Internal.element Internal.noStyleSheet
        Internal.asEl
        (Just "a")
        (Internal.Attr (Html.Attributes.href url)
            :: Internal.Attr (Html.Attributes.rel "noopener noreferrer")
            :: Internal.Attr (Html.Attributes.target "_blank")
            :: width shrink
            :: height shrink
            :: Internal.htmlClass classes.contentCenterX
            :: Internal.htmlClass classes.contentCenterY
            :: attrs
        )
        (Internal.Unkeyed [ label ])


{-| A link to download a file.
-}
download : List (Attribute msg) -> Link msg -> Element msg
download attrs { url, label } =
    Internal.element Internal.noStyleSheet
        Internal.asEl
        (Just "a")
        (Internal.Attr (Html.Attributes.href url)
            :: Internal.Attr (Html.Attributes.download "")
            :: width shrink
            :: height shrink
            :: Internal.htmlClass classes.contentCenterX
            :: Internal.htmlClass classes.contentCenterY
            :: attrs
        )
        (Internal.Unkeyed [ label ])


{-| A link to download a file, but you can specify the filename.
-}
downloadAs : List (Attribute msg) -> { label : Element msg, filename : String, url : String } -> Element msg
downloadAs attrs { url, filename, label } =
    Internal.element Internal.noStyleSheet
        Internal.asEl
        (Just "a")
        (Internal.Attr (Html.Attributes.href url)
            :: Internal.Attr (Html.Attributes.download filename)
            :: width shrink
            :: height shrink
            :: Internal.htmlClass classes.contentCenterX
            :: Internal.htmlClass classes.contentCenterY
            :: attrs
        )
        (Internal.Unkeyed [ label ])


{-| -}
below : Element msg -> Attribute msg
below element =
    Internal.Nearby Internal.Below element


{-| -}
above : Element msg -> Attribute msg
above element =
    Internal.Nearby Internal.Above element


{-| -}
onRight : Element msg -> Attribute msg
onRight element =
    Internal.Nearby Internal.OnRight element


{-| -}
onLeft : Element msg -> Attribute msg
onLeft element =
    Internal.Nearby Internal.OnLeft element


{-| -}
inFront : Element msg -> Attribute msg
inFront element =
    Internal.Nearby Internal.InFront element


{-| -}
behind : Element msg -> Attribute msg
behind element =
    Internal.Nearby Internal.Behind element


{-| -}
width : Length -> Attribute msg
width =
    Internal.Width


{-| -}
height : Length -> Attribute msg
height =
    Internal.Height


{-| -}
scale : Float -> Attr decorative msg
scale n =
    Internal.StyleClass Flag.scale (Internal.Transform (Internal.Scale n n 1))


{-| -}
rotate : Float -> Attr decorative msg
rotate angle =
    Internal.StyleClass Flag.rotate (Internal.Transform (Internal.Rotate 0 0 1 angle))


{-| -}
moveUp : Float -> Attr decorative msg
moveUp y =
    Internal.StyleClass Flag.moveY (Internal.Transform (Internal.Move Nothing (Just (negate y)) Nothing))


{-| -}
moveDown : Float -> Attr decorative msg
moveDown y =
    Internal.StyleClass Flag.moveY (Internal.Transform (Internal.Move Nothing (Just y) Nothing))


{-| -}
moveRight : Float -> Attr decorative msg
moveRight x =
    Internal.StyleClass Flag.moveX (Internal.Transform (Internal.Move (Just x) Nothing Nothing))


{-| -}
moveLeft : Float -> Attr decorative msg
moveLeft x =
    Internal.StyleClass Flag.moveX (Internal.Transform (Internal.Move (Just (negate x)) Nothing Nothing))


{-| -}
padding : Int -> Attribute msg
padding x =
    Internal.StyleClass Flag.padding (Internal.PaddingStyle x x x x)


{-| Set horizontal and vertical padding.
-}
paddingXY : Int -> Int -> Attribute msg
paddingXY x y =
    Internal.StyleClass Flag.padding (Internal.PaddingStyle y x y x)


{-| -}
paddingEach : { bottom : Int, left : Int, right : Int, top : Int } -> Attribute msg
paddingEach { top, right, bottom, left } =
    Internal.StyleClass Flag.padding (Internal.PaddingStyle top right bottom left)


{-| -}
centerX : Attribute msg
centerX =
    Internal.AlignX Internal.CenterX


{-| -}
centerY : Attribute msg
centerY =
    Internal.AlignY Internal.CenterY


{-| -}
alignTop : Attribute msg
alignTop =
    Internal.AlignY Internal.Top


{-| -}
alignBottom : Attribute msg
alignBottom =
    Internal.AlignY Internal.Bottom


{-| -}
alignLeft : Attribute msg
alignLeft =
    Internal.AlignX Internal.Left


{-| -}
alignRight : Attribute msg
alignRight =
    Internal.AlignX Internal.Right


{-| -}
spaceEvenly : Attribute msg
spaceEvenly =
    Internal.Class Flag.xAlign (.spaceEvenly Internal.Style.classes)


{-| -}
spacing : Int -> Attribute msg
spacing x =
    Internal.StyleClass Flag.spacing (Internal.SpacingStyle x x)


{-| In the majority of cases you'll just need to use `spacing`, which will work as intended.

However for some layouts, like `textColumn`, you may want to set a different spacing for the x axis compared to the y axis.

-}
spacingXY : Int -> Int -> Attribute msg
spacingXY x y =
    Internal.StyleClass Flag.spacing (Internal.SpacingStyle x y)


{-| Make an element transparent and have it ignore any mouse or touch events, though it will stil take up space.
-}
transparent : Bool -> Attr decorative msg
transparent on =
    if on then
        Internal.StyleClass Flag.transparency (Internal.Transparency "transparent" 1.0)
    else
        Internal.StyleClass Flag.transparency (Internal.Transparency "visible" 0.0)


{-| A capped value between 0.0 and 1.0, where 0.0 is transparent and 1.0 is fully opaque.

Semantically equavalent to html opacity.

-}
alpha : Float -> Attr decorative msg
alpha o =
    let
        transparency =
            o
                |> max 0.0
                |> min 1.0
                |> (\x -> 1 - x)
    in
    Internal.StyleClass Flag.transparency <| Internal.Transparency ("transparency-" ++ Internal.floatClass transparency) transparency



-- {-| -}
-- hidden : Bool -> Attribute msg
-- hidden on =
--     if on then
--         Internal.class "hidden"
--     else
--         Internal.NoAttribute


{-| -}
scrollbars : Attribute msg
scrollbars =
    Internal.Class Flag.overflow classes.scrollbars


{-| -}
scrollbarY : Attribute msg
scrollbarY =
    Internal.Class Flag.overflow classes.scrollbarsY


{-| -}
scrollbarX : Attribute msg
scrollbarX =
    Internal.Class Flag.overflow classes.scrollbarsX


{-| -}
clip : Attribute msg
clip =
    Internal.Class Flag.overflow classes.clip


{-| -}
clipY : Attribute msg
clipY =
    Internal.Class Flag.overflow classes.clipY


{-| -}
clipX : Attribute msg
clipX =
    Internal.Class Flag.overflow classes.clipX


{-| Set the cursor to the pointer hand.
-}
pointer : Attribute msg
pointer =
    Internal.Class Flag.cursor classes.cursorPointer


{-| -}
type alias Device =
    { class : DeviceClass
    , portrait : Bool
    }


{-| -}
type DeviceClass
    = Phone
    | Tablet
    | Desktop
    | BigDesktop


{-| Takes in a Window.Size and returns a device profile which can be used for responsiveness.
-}
classifyDevice : { window | height : Int, width : Int } -> Device
classifyDevice window =
    { device =
        if window.width <= 600 then
            Phone
        else if window.width > 600 && window.width <= 1200 then
            Tablet
        else if window.width > 1200 && window.width <= 1800 then
            Desktop
        else
            BigDesktop
    , portrait = window.width < window.height
    }


{-| When designing it's nice to use a modular scale to set spacial rythms.
scaled =
Scale.modular 16 1.25

A modular scale starts with a number, and multiplies it by a ratio a number of times.
Then, when setting font sizes you can use:

       Font.size (scaled 1) -- results in 16

       Font.size (scaled 2) -- 16 * 1.25 results in 20

       Font.size (scaled 4) -- 16 * 1.25 ^ (4 - 1) results in 31.25

We can also provide negative numbers to scale below 16px.

       Font.size (scaled -1) -- 16 * 1.25 ^ (-1) results in 12.8

-}
modular : Float -> Float -> Int -> Float
modular normal ratio rescale =
    if rescale == 0 then
        normal
    else if rescale < 0 then
        normal * ratio ^ toFloat rescale
    else
        normal * ratio ^ (toFloat rescale - 1)


{-| -}
mouseOver : List Decoration -> Attribute msg
mouseOver decs =
    Internal.StyleClass Flag.hover <|
        Internal.PseudoSelector Internal.Hover
            (decs
                |> Internal.unwrapDecorations
                |> List.map (Internal.tag Internal.Style.classes.hover)
            )


{-| -}
mouseDown : List Decoration -> Attribute msg
mouseDown decs =
    Internal.StyleClass Flag.active <|
        Internal.PseudoSelector Internal.Active
            (decs
                |> Internal.unwrapDecorations
                |> List.map (Internal.tag Internal.Style.classes.active)
            )


{-| -}
focused : List Decoration -> Attribute msg
focused decs =
    Internal.StyleClass Flag.focus <|
        Internal.PseudoSelector Internal.Focus
            (decs
                |> Internal.unwrapDecorations
                |> List.map (Internal.tag Internal.Style.classes.focus)
            )
