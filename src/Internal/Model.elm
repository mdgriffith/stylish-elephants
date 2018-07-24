module Internal.Model exposing (..)

{-| -}

import Html
import Html.Attributes
import Html.Keyed
import Internal.Flag as Flag exposing (Flag)
import Internal.Style exposing (classes, dot)
import Json.Encode as Json
import Set exposing (Set)
import VirtualDom


type Element msg
    = Unstyled (LayoutContext -> VirtualDom.Node msg)
    | Styled
        { styles : List Style
        , html : Maybe String -> LayoutContext -> VirtualDom.Node msg
        }
    | Text String
    | Empty


type LayoutContext
    = AsRow
    | AsColumn
    | AsEl
    | AsGrid
    | AsParagraph
    | AsTextColumn


type Aligned
    = Unaligned
    | Aligned (Maybe HAlign) (Maybe VAlign)


type HAlign
    = Left
    | CenterX
    | Right


type VAlign
    = Top
    | CenterY
    | Bottom


type Style
    = Style String (List Property)
      --       class  prop   val
    | FontFamily String (List Font)
    | FontSize Int
      -- classname, prop, value
    | Single String String String
    | Colored String String Color
    | SpacingStyle String Int Int
    | PaddingStyle String Int Int Int Int
    | GridTemplateStyle
        { spacing : ( Length, Length )
        , columns : List Length
        , rows : List Length
        }
    | GridPosition
        { row : Int
        , col : Int
        , width : Int
        , height : Int
        }
    | Transform Transformation
    | PseudoSelector PseudoClass (List Style)
    | Transparency String Float
    | Shadows String String


type PseudoClass
    = Focus
    | Hover
    | Active


type Font
    = Serif
    | SansSerif
    | Monospace
    | Typeface String
    | ImportFont String String


type Property
    = Property String String


type Transformation
    = MoveX Float
    | MoveY Float
    | MoveZ Float
    | MoveXYZ XYZ
    | Rotate XYZ Float
    | Scale XYZ


type alias XYZ =
    ( Float, Float, Float )


type alias Angle =
    Float


type Transform
    = Untransformed
    | Moved XYZ
      --              translate, scale, rotate
    | FullTransform XYZ XYZ XYZ Angle


type Attribute aligned msg
    = NoAttribute
    | Attr (VirtualDom.Attribute msg)
    | Describe Description
      -- invalidation key and literal class
    | Class Flag String
      -- invalidation key "border-color" as opposed to "border-color-10-10-10" that will be the key for the class
    | StyleClass Flag Style
    | AlignY VAlign
    | AlignX HAlign
    | Width Length
    | Height Length
    | Nearby Location (Element msg)



-- | TextShadow
--     { offset : ( Float, Float )
--     , blur : Float
--     , color : Color
--     }
-- | BoxShadow
--     { inset : Bool
--     , offset : ( Float, Float )
--     , size : Float
--     , blur : Float
--     , color : Color
--     }


type Description
    = Main
    | Navigation
      -- | Search
    | ContentInfo
    | Complementary
    | Heading Int
    | Label String
    | LivePolite
    | LiveAssertive
    | Button


type Length
    = Px Int
    | Content
    | Fill Int
    | Min Int Length
    | Max Int Length


type Axis
    = XAxis
    | YAxis
    | AllAxis


type Location
    = Above
    | Below
    | OnRight
    | OnLeft
    | InFront
    | Behind


{-| -}
type Color
    = Rgba Float Float Float Float


type NodeName
    = Generic
    | NodeName String
    | Embedded String String


div =
    Generic


type alias Gathered msg =
    { node : NodeName
    , attributes : List (VirtualDom.Attribute msg)
    , styles : List Style
    , children : List (VirtualDom.Node msg)
    , boxShadows : Maybe ( String, String )
    , textShadows : Maybe ( String, String )
    , transform : Maybe (Decorated TransformationGroup)
    , has : Flag.Field
    }


type alias Decorated x =
    { focus : Maybe x
    , hover : Maybe x
    , normal : Maybe x
    , active : Maybe x
    }


type alias TransformationGroup =
    { rotate : Maybe Rotation
    , translate : Maybe ( Maybe Float, Maybe Float, Maybe Float )
    , scale : Maybe ( Float, Float, Float )
    }


type Rotation
    = Rotation Float Float Float Float


htmlClass : String -> Attribute aligned msg
htmlClass cls =
    Attr <| Html.Attributes.class cls


{-| -}
unstyled : VirtualDom.Node msg -> Element msg
unstyled =
    Unstyled << always


{-| -}
renderNode : Gathered msg -> Children (VirtualDom.Node msg) -> Maybe String -> LayoutContext -> VirtualDom.Node msg
renderNode { attributes, node, has } children styles context =
    let
        createNode nodeName attrs withStyles =
            case children of
                Keyed keyed ->
                    VirtualDom.keyedNode nodeName
                        attrs
                        (case withStyles of
                            Nothing ->
                                keyed

                            Just stylesheet ->
                                ( "stylesheet"
                                , VirtualDom.node "style" [ Html.Attributes.class "stylesheet" ] [ VirtualDom.text stylesheet ]
                                )
                                    :: keyed
                        )

                Unkeyed unkeyed ->
                    (case nodeName of
                        "div" ->
                            Html.div

                        "p" ->
                            Html.p

                        _ ->
                            VirtualDom.node nodeName
                    )
                        attrs
                        (case withStyles of
                            Nothing ->
                                unkeyed

                            Just stylesheet ->
                                VirtualDom.node "style" [ Html.Attributes.class "stylesheet" ] [ VirtualDom.text stylesheet ] :: unkeyed
                        )

        html =
            case node of
                Generic ->
                    createNode "div" attributes styles

                NodeName nodeName ->
                    createNode nodeName attributes styles

                Embedded nodeName internal ->
                    VirtualDom.node nodeName
                        attributes
                        [ createNode internal [ Html.Attributes.class (classes.any ++ " " ++ classes.single) ] styles
                        ]
    in
    case context of
        AsRow ->
            if Flag.present Flag.widthFill has && not (Flag.present Flag.widthBetween has) then
                html
            else if Flag.present Flag.alignRight has then
                Html.u
                    -- "alignRight"
                    [ Html.Attributes.class
                        (String.join " "
                            [ classes.any
                            , classes.single
                            , classes.container
                            , classes.contentCenterY
                            , classes.alignContainerRight
                            ]
                        )
                    ]
                    [ html ]
            else if Flag.present Flag.centerX has then
                Html.s
                    -- "centerX"
                    [ Html.Attributes.class
                        (String.join " "
                            [ classes.any
                            , classes.single
                            , classes.container
                            , classes.contentCenterY
                            , classes.alignContainerCenterX
                            ]
                        )
                    ]
                    [ html ]
            else
                html

        AsColumn ->
            if Flag.present Flag.heightFill has && not (Flag.present Flag.heightBetween has) then
                html
            else if Flag.present Flag.centerY has then
                Html.s
                    [ Html.Attributes.class
                        (String.join " "
                            [ classes.any
                            , classes.single
                            , classes.container
                            , classes.alignContainerCenterY
                            ]
                        )
                    ]
                    [ html ]
            else if Flag.present Flag.alignBottom has then
                Html.u
                    [ Html.Attributes.class
                        (String.join " "
                            [ classes.any
                            , classes.single
                            , classes.container
                            , classes.alignContainerBottom
                            ]
                        )
                    ]
                    [ html ]
            else
                html

        _ ->
            html


addNodeName : String -> NodeName -> NodeName
addNodeName newNode old =
    case old of
        Generic ->
            NodeName newNode

        NodeName name ->
            Embedded name newNode

        Embedded x y ->
            Embedded x y


alignXName : HAlign -> String
alignXName align =
    case align of
        Left ->
            classes.alignedHorizontally ++ " " ++ classes.alignLeft

        Right ->
            classes.alignedHorizontally ++ " " ++ classes.alignRight

        CenterX ->
            classes.alignedHorizontally ++ " " ++ classes.alignCenterX


alignYName : VAlign -> String
alignYName align =
    case align of
        Top ->
            classes.alignedVertically ++ " " ++ classes.alignTop

        Bottom ->
            classes.alignedVertically ++ " " ++ classes.alignBottom

        CenterY ->
            classes.alignedVertically ++ " " ++ classes.alignCenterY


{-| replace a
-}
addIfNothing val existing =
    case existing of
        Nothing ->
            val

        x ->
            x


emptyTransformationStates =
    { focus = Nothing
    , hover = Nothing
    , normal = Nothing
    , active = Nothing
    }


emptyTransformGroup =
    { translate = Nothing
    , rotate = Nothing
    , scale = Nothing
    }



-- stackOn : Maybe PseudoClass -> Transformation -> Gathered msg -> Gathered msg
-- stackOn maybePseudo transform gathered =
--     let
--         states =
--             Maybe.withDefault emptyTransformationStates gathered.transform
--     in
--     case maybePseudo of
--         Nothing ->
--             let
--                 normal =
--                     states.normal
--             in
--             { gathered
--                 | transform =
--                     Just
--                         { states
--                             | normal =
--                                 normal
--                                     |> Maybe.withDefault emptyTransformGroup
--                                     |> stackTransforms transform
--                                     |> Just
--                         }
--             }
--         Just Hover ->
--             let
--                 hover =
--                     states.hover
--             in
--             { gathered
--                 | transform =
--                     Just
--                         { states
--                             | hover =
--                                 hover
--                                     |> Maybe.withDefault emptyTransformGroup
--                                     |> stackTransforms transform
--                                     |> Just
--                         }
--             }
--         Just Active ->
--             let
--                 active =
--                     states.active
--             in
--             { gathered
--                 | transform =
--                     Just
--                         { states
--                             | active =
--                                 active
--                                     |> Maybe.withDefault emptyTransformGroup
--                                     |> stackTransforms transform
--                                     |> Just
--                         }
--             }
--         Just Focus ->
--             let
--                 focus =
--                     states.focus
--             in
--             { gathered
--                 | transform =
--                     Just
--                         { states
--                             | focus =
--                                 focus
--                                     |> Maybe.withDefault emptyTransformGroup
--                                     |> stackTransforms transform
--                                     |> Just
--                         }
--             }
-- {-| -}
-- stackTransforms : Transformation -> TransformationGroup -> TransformationGroup
-- stackTransforms transform group =
--     case transform of
--         Move mx my mz ->
--             case group.translate of
--                 Nothing ->
--                     { group
--                         | translate =
--                             Just ( mx, my, mz )
--                     }
--                 Just ( existingX, existingY, existingZ ) ->
--                     { group
--                         | translate =
--                             Just
--                                 ( addIfNothing mx existingX
--                                 , addIfNothing my existingY
--                                 , addIfNothing mz existingZ
--                                 )
--                     }
--         Rotate x y z angle ->
--             { group
--                 | rotate = addIfNothing (Just (Rotation x y z angle)) group.rotate
--             }
--         Scale x y z ->
--             { group
--                 | scale = addIfNothing (Just ( x, y, z )) group.scale
--             }


addNormalStyle flag styleProp gatheredProps =
    if Flag.present flag gatheredProps.has then
        gatheredProps
    else
        { gatheredProps
            | attributes =
                case styleProp of
                    PseudoSelector _ _ ->
                        Html.Attributes.class Internal.Style.classes.transition
                            :: Html.Attributes.class (getStyleName styleProp)
                            :: gatheredProps.attributes

                    _ ->
                        Html.Attributes.class (getStyleName styleProp)
                            :: gatheredProps.attributes
            , styles = styleProp :: gatheredProps.styles
            , has = Flag.add flag gatheredProps.has
        }


gatherAttrRecursive : NodeName -> Flag.Field -> Transform -> List Style -> List (VirtualDom.Attribute msg) -> List (VirtualDom.Node msg) -> List (Attribute aligned msg) -> Gathered msg
gatherAttrRecursive node has transform styles attrs children elementAttrs =
    case elementAttrs of
        [] ->
            case transform of
                Untransformed ->
                    { attributes = attrs
                    , styles = styles
                    , node = div
                    , children = children
                    , transform = Nothing
                    , boxShadows = Nothing
                    , textShadows = Nothing
                    , has = has
                    }

                Moved ( x, y, z ) ->
                    let
                        val =
                            "translate3d("
                                ++ String.fromFloat x
                                ++ "px, "
                                ++ String.fromFloat y
                                ++ "px, "
                                ++ String.fromFloat z
                                ++ "px)"

                        class =
                            "mv-"
                                ++ String.fromFloat x
                                ++ "-"
                                ++ String.fromFloat y
                                ++ "-"
                                ++ String.fromFloat z
                    in
                    { attributes = Html.Attributes.class class :: attrs
                    , styles = Single class "transform" val :: styles
                    , node = div
                    , children = children
                    , transform = Nothing
                    , boxShadows = Nothing
                    , textShadows = Nothing
                    , has = has
                    }

                FullTransform ( tx, ty, tz ) ( sx, sy, sz ) ( ox, oy, oz ) angle ->
                    let
                        translate =
                            "translate3d("
                                ++ String.fromFloat tx
                                ++ "px, "
                                ++ String.fromFloat ty
                                ++ "px, "
                                ++ String.fromFloat tz
                                ++ "px)"

                        scale =
                            "scale3d("
                                ++ String.fromFloat sx
                                ++ ", "
                                ++ String.fromFloat sy
                                ++ ", "
                                ++ String.fromFloat sz
                                ++ ")"

                        rotate =
                            "rotate3d("
                                ++ String.fromFloat ox
                                ++ ", "
                                ++ String.fromFloat oy
                                ++ ", "
                                ++ String.fromFloat oz
                                ++ ", "
                                ++ String.fromFloat angle
                                ++ "rad)"
                                ++ ")"

                        class =
                            "tfrm-"
                                ++ String.fromFloat tx
                                ++ "-"
                                ++ String.fromFloat ty
                                ++ "-"
                                ++ String.fromFloat tz
                                ++ "-"
                                ++ String.fromFloat sx
                                ++ "-"
                                ++ String.fromFloat sy
                                ++ "-"
                                ++ String.fromFloat sz
                                ++ "-"
                                ++ String.fromFloat ox
                                ++ "-"
                                ++ String.fromFloat oy
                                ++ "-"
                                ++ String.fromFloat oz
                                ++ "-"
                                ++ String.fromFloat angle
                    in
                    { attributes = Html.Attributes.class class :: attrs
                    , styles = Single class "transform" (translate ++ " " ++ scale ++ " " ++ rotate) :: styles
                    , node = div
                    , children = children
                    , transform = Nothing
                    , boxShadows = Nothing
                    , textShadows = Nothing
                    , has = has
                    }

        attribute :: remaining ->
            case attribute of
                NoAttribute ->
                    gatherAttrRecursive node has transform styles attrs children remaining

                Class flag exactClassName ->
                    if Flag.present flag has then
                        gatherAttrRecursive node has transform styles attrs children remaining
                    else
                        gatherAttrRecursive node (Flag.add flag has) transform styles (Html.Attributes.class exactClassName :: attrs) children remaining

                Attr actualAttribute ->
                    gatherAttrRecursive node has transform styles (actualAttribute :: attrs) children remaining

                StyleClass flag style ->
                    if Flag.present flag has then
                        gatherAttrRecursive node has transform styles attrs children remaining
                    else
                        case style of
                            Transform newTransform ->
                                let
                                    newTransformation =
                                        case transform of
                                            Untransformed ->
                                                case newTransform of
                                                    MoveX x ->
                                                        Moved ( x, 0, 0 )

                                                    MoveY y ->
                                                        Moved ( 0, y, 0 )

                                                    MoveZ z ->
                                                        Moved ( 0, 0, z )

                                                    MoveXYZ xyz ->
                                                        Moved xyz

                                                    Rotate xyz angle ->
                                                        FullTransform ( 0, 0, 0 ) ( 1, 1, 1 ) xyz angle

                                                    Scale xyz ->
                                                        FullTransform ( 0, 0, 0 ) xyz ( 0, 0, 1 ) 0

                                            Moved (( x, y, z ) as moved) ->
                                                case newTransform of
                                                    MoveX newX ->
                                                        Moved ( newX, y, z )

                                                    MoveY newY ->
                                                        Moved ( x, newY, z )

                                                    MoveZ newZ ->
                                                        Moved ( x, y, newZ )

                                                    MoveXYZ xyz ->
                                                        Moved xyz

                                                    Rotate xyz angle ->
                                                        FullTransform moved ( 1, 1, 1 ) xyz angle

                                                    Scale scale ->
                                                        FullTransform moved scale ( 0, 0, 1 ) 0

                                            FullTransform (( x, y, z ) as moved) scaled origin angle ->
                                                case newTransform of
                                                    MoveX newX ->
                                                        FullTransform ( newX, y, z ) scaled origin angle

                                                    MoveY newY ->
                                                        FullTransform ( x, newY, z ) scaled origin angle

                                                    MoveZ newZ ->
                                                        FullTransform ( x, y, newZ ) scaled origin angle

                                                    MoveXYZ newMove ->
                                                        FullTransform newMove scaled origin angle

                                                    Rotate newOrigin newAngle ->
                                                        FullTransform moved scaled newOrigin newAngle

                                                    Scale newScale ->
                                                        FullTransform moved newScale origin angle
                                in
                                gatherAttrRecursive node
                                    (Flag.add flag has)
                                    newTransformation
                                    styles
                                    attrs
                                    children
                                    remaining

                            _ ->
                                gatherAttrRecursive node
                                    (Flag.add flag has)
                                    transform
                                    (style :: styles)
                                    (Html.Attributes.class (getStyleName style)
                                        :: attrs
                                    )
                                    children
                                    remaining

                Width width ->
                    if Flag.present Flag.width has then
                        gatherAttrRecursive node has transform styles attrs children remaining
                    else
                        case width of
                            Px px ->
                                gatherAttrRecursive node
                                    (Flag.add Flag.width has)
                                    transform
                                    (Single ("width-px-" ++ String.fromInt px) "width" (String.fromInt px ++ "px") :: styles)
                                    (Html.Attributes.class (Internal.Style.classes.widthExact ++ " width-px-" ++ String.fromInt px) :: attrs)
                                    children
                                    remaining

                            Content ->
                                gatherAttrRecursive node
                                    (Flag.add Flag.widthContent (Flag.add Flag.width has))
                                    transform
                                    styles
                                    (Html.Attributes.class Internal.Style.classes.widthContent :: attrs)
                                    children
                                    remaining

                            Fill portion ->
                                if portion == 1 then
                                    gatherAttrRecursive node
                                        (Flag.add Flag.widthFill (Flag.add Flag.width has))
                                        transform
                                        styles
                                        (Html.Attributes.class Internal.Style.classes.widthFill :: attrs)
                                        children
                                        remaining
                                else
                                    gatherAttrRecursive node
                                        (Flag.add Flag.widthFill (Flag.add Flag.width has))
                                        transform
                                        (Single
                                            (Internal.Style.classes.any
                                                ++ "."
                                                ++ Internal.Style.classes.row
                                                ++ " > "
                                                ++ (Internal.Style.dot <| "width-fill-" ++ String.fromInt portion)
                                            )
                                            "flex-grow"
                                            (String.fromInt (portion * 100000))
                                            :: styles
                                        )
                                        (Html.Attributes.class (Internal.Style.classes.widthFillPortion ++ " width-fill-" ++ String.fromInt portion) :: attrs)
                                        children
                                        remaining

                            _ ->
                                let
                                    ( addToFlags, newAttrs, newStyles ) =
                                        renderWidth width
                                in
                                gatherAttrRecursive node
                                    (Flag.merge addToFlags has)
                                    transform
                                    (newStyles ++ styles)
                                    (newAttrs ++ attrs)
                                    children
                                    remaining

                Height height ->
                    if Flag.present Flag.height has then
                        gatherAttrRecursive node has transform styles attrs children remaining
                    else
                        case height of
                            Px px ->
                                let
                                    val =
                                        String.fromInt px ++ "px"

                                    name =
                                        "height-px-" ++ val
                                in
                                gatherAttrRecursive node
                                    (Flag.add Flag.height has)
                                    transform
                                    (Single name "height " val :: styles)
                                    (Html.Attributes.class name :: attrs)
                                    children
                                    remaining

                            Content ->
                                gatherAttrRecursive node
                                    (Flag.add Flag.heightContent (Flag.add Flag.height has))
                                    transform
                                    styles
                                    (Html.Attributes.class Internal.Style.classes.heightContent :: attrs)
                                    children
                                    remaining

                            Fill portion ->
                                if portion == 1 then
                                    gatherAttrRecursive node
                                        (Flag.add Flag.heightFill (Flag.add Flag.height has))
                                        transform
                                        styles
                                        (Html.Attributes.class Internal.Style.classes.heightFill :: attrs)
                                        children
                                        remaining
                                else
                                    gatherAttrRecursive node
                                        (Flag.add Flag.heightFill (Flag.add Flag.height has))
                                        transform
                                        (Single
                                            (Internal.Style.classes.any
                                                ++ "."
                                                ++ Internal.Style.classes.row
                                                ++ " > "
                                                ++ (Internal.Style.dot <| "height-fill-" ++ String.fromInt portion)
                                            )
                                            "flex-grow"
                                            (String.fromInt (portion * 100000))
                                            :: styles
                                        )
                                        (Html.Attributes.class (Internal.Style.classes.heightFillPortion ++ " height-fill-" ++ String.fromInt portion) :: attrs)
                                        children
                                        remaining

                            _ ->
                                let
                                    ( addToFlags, newAttrs, newStyles ) =
                                        renderHeight height
                                in
                                gatherAttrRecursive node
                                    (Flag.merge addToFlags has)
                                    transform
                                    (newStyles ++ styles)
                                    (newAttrs ++ attrs)
                                    children
                                    remaining

                Describe description ->
                    case description of
                        Main ->
                            gatherAttrRecursive (addNodeName "main" node) has transform styles attrs children remaining

                        Navigation ->
                            gatherAttrRecursive (addNodeName "nav" node) has transform styles attrs children remaining

                        ContentInfo ->
                            gatherAttrRecursive (addNodeName "footer" node) has transform styles attrs children remaining

                        Complementary ->
                            gatherAttrRecursive (addNodeName "aside" node) has transform styles attrs children remaining

                        Heading i ->
                            if i <= 1 then
                                gatherAttrRecursive (addNodeName "h1" node) has transform styles attrs children remaining
                            else if i < 7 then
                                gatherAttrRecursive (addNodeName ("h" ++ String.fromInt i) node) has transform styles attrs children remaining
                            else
                                gatherAttrRecursive (addNodeName "h6" node) has transform styles attrs children remaining

                        Button ->
                            gatherAttrRecursive node has transform styles (VirtualDom.attribute "role" "button" :: attrs) children remaining

                        Label label ->
                            gatherAttrRecursive node has transform styles (VirtualDom.attribute "aria-label" label :: attrs) children remaining

                        LivePolite ->
                            gatherAttrRecursive node has transform styles (VirtualDom.attribute "aria-live" "polite" :: attrs) children remaining

                        LiveAssertive ->
                            gatherAttrRecursive node has transform styles (VirtualDom.attribute "aria-live" "assertive" :: attrs) children remaining

                Nearby location elem ->
                    let
                        newStyles =
                            case elem of
                                Empty ->
                                    styles

                                Text str ->
                                    styles

                                Unstyled html ->
                                    styles

                                Styled styled ->
                                    styles ++ styled.styles

                        nearbyElement =
                            Html.div
                                [ Html.Attributes.class <|
                                    case location of
                                        Above ->
                                            String.join " "
                                                [ classes.any, classes.single, classes.above ]

                                        Below ->
                                            String.join " "
                                                [ classes.any, classes.single, classes.below ]

                                        OnRight ->
                                            String.join " "
                                                [ classes.any, classes.single, classes.onRight ]

                                        OnLeft ->
                                            String.join " "
                                                [ classes.any, classes.single, classes.onLeft ]

                                        InFront ->
                                            String.join " "
                                                [ classes.any, classes.single, classes.inFront ]

                                        Behind ->
                                            String.join " "
                                                [ classes.any, classes.single, classes.behind ]
                                ]
                                [ case elem of
                                    Empty ->
                                        VirtualDom.text ""

                                    Text str ->
                                        textElement str

                                    Unstyled html ->
                                        html asEl

                                    Styled styled ->
                                        styled.html Nothing asEl
                                ]

                        newAttributes =
                            if location == Behind then
                                Html.Attributes.class classes.hasBehind :: attrs
                            else
                                attrs
                    in
                    gatherAttrRecursive node has transform newStyles newAttributes (nearbyElement :: children) remaining

                AlignX x ->
                    if Flag.present Flag.xAlign has then
                        gatherAttrRecursive node has transform styles attrs children remaining
                    else
                        gatherAttrRecursive node
                            (has
                                |> Flag.add Flag.xAlign
                                |> (\flags ->
                                        case x of
                                            CenterX ->
                                                Flag.add Flag.centerX flags

                                            Right ->
                                                Flag.add Flag.alignRight flags

                                            _ ->
                                                flags
                                   )
                            )
                            transform
                            styles
                            (Html.Attributes.class (alignXName x) :: attrs)
                            children
                            remaining

                AlignY y ->
                    if Flag.present Flag.yAlign has then
                        gatherAttrRecursive node has transform styles attrs children remaining
                    else
                        gatherAttrRecursive node
                            (Flag.add Flag.yAlign has
                                |> (\flags ->
                                        case y of
                                            CenterY ->
                                                Flag.add Flag.centerY flags

                                            Bottom ->
                                                Flag.add Flag.alignBottom flags

                                            _ ->
                                                flags
                                   )
                            )
                            transform
                            styles
                            (Html.Attributes.class (alignYName y) :: attrs)
                            children
                            remaining


renderWidth w =
    case w of
        Px px ->
            ( Flag.none
            , [ Html.Attributes.class (Internal.Style.classes.widthExact ++ " width-px-" ++ String.fromInt px) ]
            , [ Single ("width-px-" ++ String.fromInt px) "width" (String.fromInt px ++ "px") ]
            )

        Content ->
            ( Flag.add Flag.widthContent Flag.none
            , [ Html.Attributes.class Internal.Style.classes.widthContent ]
            , []
            )

        Fill portion ->
            if portion == 1 then
                ( Flag.add Flag.widthFill Flag.none
                , [ Html.Attributes.class Internal.Style.classes.widthFill ]
                , []
                )
            else
                ( Flag.add Flag.widthFill Flag.none
                , [ Html.Attributes.class (Internal.Style.classes.widthFillPortion ++ " width-fill-" ++ String.fromInt portion) ]
                , [ Single
                        (Internal.Style.classes.any
                            ++ "."
                            ++ Internal.Style.classes.row
                            ++ " > "
                            ++ (Internal.Style.dot <| "width-fill-" ++ String.fromInt portion)
                        )
                        "flex-grow"
                        (String.fromInt (portion * 100000))
                  ]
                )

        Min minSize len ->
            let
                cls =
                    "min-width-"
                        ++ String.fromInt minSize

                style =
                    Single
                        cls
                        "min-width"
                        (String.fromInt minSize ++ "px")

                ( newFlag, newAttrs, newStyle ) =
                    renderWidth len
            in
            ( Flag.add Flag.widthBetween newFlag
            , Html.Attributes.class cls :: newAttrs
            , style :: newStyle
            )

        Max maxSize len ->
            let
                cls =
                    "max-width-" ++ String.fromInt maxSize

                style =
                    Single cls
                        "max-width"
                        (String.fromInt maxSize ++ "px")

                ( newFlag, newAttrs, newStyle ) =
                    renderWidth len
            in
            ( Flag.add Flag.widthBetween newFlag
            , Html.Attributes.class cls :: newAttrs
            , style :: newStyle
            )


renderHeight h =
    case h of
        Px px ->
            let
                val =
                    String.fromInt px

                name =
                    "height-px-" ++ val
            in
            ( Flag.none
            , [ Html.Attributes.class name ]
            , [ Single name "height" (val ++ "px") ]
            )

        Content ->
            ( Flag.add Flag.heightContent Flag.none
            , [ Html.Attributes.class Internal.Style.classes.heightContent ]
            , []
            )

        Fill portion ->
            if portion == 1 then
                ( Flag.add Flag.heightFill Flag.none
                , [ Html.Attributes.class Internal.Style.classes.heightFill ]
                , []
                )
            else
                ( Flag.add Flag.heightFill Flag.none
                , [ Html.Attributes.class (Internal.Style.classes.heightFillPortion ++ " height-fill-" ++ String.fromInt portion) ]
                , [ Single
                        (Internal.Style.classes.any
                            ++ "."
                            ++ Internal.Style.classes.row
                            ++ " > "
                            ++ (Internal.Style.dot <| "height-fill-" ++ String.fromInt portion)
                        )
                        "flex-grow"
                        (String.fromInt (portion * 100000))
                  ]
                )

        Min minSize len ->
            let
                cls =
                    "min-height-"
                        ++ String.fromInt minSize

                style =
                    Single
                        cls
                        "min-height"
                        (String.fromInt minSize ++ "px")

                ( newFlag, newAttrs, newStyle ) =
                    renderWidth len
            in
            ( Flag.add Flag.heightBetween newFlag
            , Html.Attributes.class cls :: newAttrs
            , style :: newStyle
            )

        Max maxSize len ->
            let
                cls =
                    "max-height-" ++ String.fromInt maxSize

                style =
                    Single cls
                        "max-height"
                        (String.fromInt maxSize ++ "px")

                ( newFlag, newAttrs, newStyle ) =
                    renderWidth len
            in
            ( Flag.add Flag.heightBetween newFlag
            , Html.Attributes.class cls :: newAttrs
            , style :: newStyle
            )


gatherAttributes : Attribute aligned msg -> Gathered msg -> Gathered msg
gatherAttributes attr gathered =
    case attr of
        NoAttribute ->
            gathered

        Class flag exactClassName ->
            if Flag.present flag gathered.has then
                gathered
            else
                { gathered
                    | attributes = Html.Attributes.class exactClassName :: gathered.attributes
                    , has = Flag.add flag gathered.has
                }

        Attr attribute ->
            { gathered | attributes = attribute :: gathered.attributes }

        StyleClass flag style ->
            case style of
                -- DISABLED BECAUSE OF CONVENIENCE :/
                -- Transform transformation ->
                --     stackOn Nothing transformation gathered
                PseudoSelector pseudo props ->
                    let
                        ( transformationProps, otherProps ) =
                            List.partition (\x -> forTransforms x /= Nothing) props

                        forTransforms attribute =
                            case attribute of
                                Transform x ->
                                    Just x

                                _ ->
                                    Nothing

                        withTransforms =
                            transformationProps
                                |> List.filterMap forTransforms
                                |> always gathered

                        -- |> List.foldr (stackOn (Just pseudo)) gathered
                    in
                    addNormalStyle flag (PseudoSelector pseudo otherProps) withTransforms

                _ ->
                    addNormalStyle flag style gathered

        Width width ->
            if not (Flag.present Flag.width gathered.has) then
                gatherWidth width { gathered | has = Flag.add Flag.width gathered.has }
            else
                gathered

        Height height ->
            if not (Flag.present Flag.height gathered.has) then
                gatherHeight height { gathered | has = Flag.add Flag.height gathered.has }
            else
                gathered

        Describe description ->
            case description of
                Main ->
                    { gathered | node = addNodeName "main" gathered.node }

                Navigation ->
                    { gathered | node = addNodeName "nav" gathered.node }

                ContentInfo ->
                    { gathered | node = addNodeName "footer" gathered.node }

                Complementary ->
                    { gathered | node = addNodeName "aside" gathered.node }

                Heading i ->
                    if i <= 1 then
                        { gathered | node = addNodeName "h1" gathered.node }
                    else if i < 7 then
                        { gathered | node = addNodeName ("h" ++ String.fromInt i) gathered.node }
                    else
                        { gathered | node = addNodeName "h6" gathered.node }

                Button ->
                    { gathered | attributes = VirtualDom.attribute "role" "button" :: gathered.attributes }

                Label label ->
                    { gathered | attributes = VirtualDom.attribute "aria-label" label :: gathered.attributes }

                LivePolite ->
                    { gathered | attributes = VirtualDom.attribute "aria-live" "polite" :: gathered.attributes }

                LiveAssertive ->
                    { gathered | attributes = VirtualDom.attribute "aria-live" "assertive" :: gathered.attributes }

        Nearby location elem ->
            let
                styles =
                    case elem of
                        Empty ->
                            Nothing

                        Text str ->
                            Nothing

                        Unstyled html ->
                            Nothing

                        Styled styled ->
                            Just <| gathered.styles ++ styled.styles

                nearbyElement =
                    Html.div
                        [ Html.Attributes.class <|
                            case location of
                                Above ->
                                    String.join " "
                                        [ classes.any, classes.single, classes.above ]

                                Below ->
                                    String.join " "
                                        [ classes.any, classes.single, classes.below ]

                                OnRight ->
                                    String.join " "
                                        [ classes.any, classes.single, classes.onRight ]

                                OnLeft ->
                                    String.join " "
                                        [ classes.any, classes.single, classes.onLeft ]

                                InFront ->
                                    String.join " "
                                        [ classes.any, classes.single, classes.inFront ]

                                Behind ->
                                    String.join " "
                                        [ classes.any, classes.single, classes.behind ]
                        ]
                        [ case elem of
                            Empty ->
                                VirtualDom.text ""

                            Text str ->
                                textElement str

                            Unstyled html ->
                                html asEl

                            Styled styled ->
                                styled.html Nothing asEl
                        ]
            in
            { gathered
                | attributes =
                    if location == Behind then
                        Html.Attributes.class classes.hasBehind :: gathered.attributes
                    else
                        gathered.attributes
                , styles =
                    case styles of
                        Nothing ->
                            gathered.styles

                        Just newStyles ->
                            newStyles
                , children = nearbyElement :: gathered.children
            }

        AlignX x ->
            if not (Flag.present Flag.xAlign gathered.has) then
                { gathered
                    | attributes = Html.Attributes.class (alignXName x) :: gathered.attributes
                    , has =
                        gathered.has
                            |> Flag.add Flag.xAlign
                            |> (\flags ->
                                    case x of
                                        CenterX ->
                                            Flag.add Flag.centerX flags

                                        Right ->
                                            Flag.add Flag.alignRight flags

                                        _ ->
                                            flags
                               )
                }
            else
                gathered

        AlignY y ->
            if not (Flag.present Flag.yAlign gathered.has) then
                { gathered
                    | attributes = Html.Attributes.class (alignYName y) :: gathered.attributes
                    , has =
                        gathered.has
                            |> Flag.add Flag.yAlign
                            |> (\flags ->
                                    case y of
                                        CenterY ->
                                            Flag.add Flag.centerY flags

                                        Bottom ->
                                            Flag.add Flag.alignBottom flags

                                        _ ->
                                            flags
                               )
                }
            else
                gathered



-- BoxShadow shadow ->
--     case gathered.boxShadows of
--         Nothing ->
--             { gathered | boxShadows = Just ( boxShadowName shadow, formatBoxShadow shadow ) }
--         Just ( existingClass, existing ) ->
--             { gathered | boxShadows = Just ( boxShadowName shadow ++ "-" ++ existingClass, formatBoxShadow shadow ++ ", " ++ existing ) }
-- TextShadow shadow ->
--     case gathered.textShadows of
--         Nothing ->
--             { gathered | textShadows = Just ( textShadowName shadow, formatTextShadow shadow ) }
--         Just ( existingClass, existing ) ->
--             { gathered | textShadows = Just ( textShadowName shadow ++ "-" ++ existingClass, formatTextShadow shadow ++ ", " ++ existing ) }


gatherWidth w gathered =
    case w of
        Px px ->
            { gathered
                | attributes = Html.Attributes.class (Internal.Style.classes.widthExact ++ " width-px-" ++ String.fromInt px) :: gathered.attributes
                , styles = Single ("width-px-" ++ String.fromInt px) "width" (String.fromInt px ++ "px") :: gathered.styles
            }

        Content ->
            { gathered
                | attributes = Html.Attributes.class Internal.Style.classes.widthContent :: gathered.attributes
                , has = Flag.add Flag.widthContent gathered.has
            }

        Fill portion ->
            if portion == 1 then
                { gathered
                    | attributes = Html.Attributes.class Internal.Style.classes.widthFill :: gathered.attributes
                    , has = Flag.add Flag.widthFill gathered.has
                }
            else
                { gathered
                    | has = Flag.add Flag.widthFill gathered.has
                    , attributes = Html.Attributes.class (Internal.Style.classes.widthFillPortion ++ " width-fill-" ++ String.fromInt portion) :: gathered.attributes
                    , styles =
                        Single
                            (Internal.Style.classes.any
                                ++ "."
                                ++ Internal.Style.classes.row
                                ++ " > "
                                ++ (Internal.Style.dot <| "width-fill-" ++ String.fromInt portion)
                            )
                            "flex-grow"
                            (String.fromInt (portion * 100000))
                            :: gathered.styles
                }

        Min minSize len ->
            let
                cls =
                    "min-width-"
                        ++ String.fromInt minSize

                style =
                    Single
                        cls
                        "min-width"
                        (String.fromInt minSize ++ "px")

                newGathered =
                    { gathered
                        | has = Flag.add Flag.widthBetween gathered.has
                        , attributes = Html.Attributes.class cls :: gathered.attributes
                        , styles =
                            style :: gathered.styles
                    }
            in
            gatherWidth len newGathered

        Max maxSize len ->
            let
                cls =
                    "max-width-" ++ String.fromInt maxSize

                style =
                    Single cls
                        "max-width"
                        (String.fromInt maxSize ++ "px")

                newGathered =
                    { gathered
                        | has = Flag.add Flag.widthBetween gathered.has
                        , attributes = Html.Attributes.class cls :: gathered.attributes
                        , styles =
                            style :: gathered.styles
                    }
            in
            gatherWidth len newGathered


gatherHeight h gathered =
    case h of
        Px px ->
            { gathered
                | attributes = Html.Attributes.class ("height-px-" ++ String.fromInt px) :: gathered.attributes
                , styles = Single ("height-px-" ++ String.fromInt px) "height" (String.fromInt px ++ "px") :: gathered.styles
            }

        Content ->
            { gathered
                | attributes = Html.Attributes.class Internal.Style.classes.heightContent :: gathered.attributes
                , has = Flag.add Flag.heightContent gathered.has
            }

        Fill portion ->
            if portion == 1 then
                { gathered
                    | attributes = Html.Attributes.class Internal.Style.classes.heightFill :: gathered.attributes
                    , has = Flag.add Flag.heightFill gathered.has
                }
            else
                { gathered
                    | attributes = Html.Attributes.class (Internal.Style.classes.heightFillPortion ++ " height-fill-" ++ String.fromInt portion) :: gathered.attributes
                    , has = Flag.add Flag.heightFill gathered.has
                    , styles =
                        Single
                            (Internal.Style.classes.any
                                ++ "."
                                ++ Internal.Style.classes.column
                                ++ " > "
                                ++ (Internal.Style.dot <|
                                        "height-fill-"
                                            ++ String.fromInt portion
                                   )
                            )
                            "flex-grow"
                            (String.fromInt (portion * 100000))
                            :: gathered.styles
                }

        Min minSize len ->
            let
                cls =
                    "min-height-" ++ String.fromInt minSize

                style =
                    Single cls "min-height" (String.fromInt minSize ++ "px")

                newGathered =
                    { gathered
                        | has = Flag.add Flag.heightBetween gathered.has
                        , attributes = Html.Attributes.class cls :: gathered.attributes
                        , styles =
                            style :: gathered.styles
                    }
            in
            gatherHeight len newGathered

        Max maxSize len ->
            let
                cls =
                    "max-height-" ++ String.fromInt maxSize

                style =
                    Single cls "max-height" (String.fromInt maxSize ++ "px")

                newGathered =
                    { gathered
                        | has = Flag.add Flag.heightBetween gathered.has
                        , attributes = Html.Attributes.class cls :: gathered.attributes
                        , styles =
                            style :: gathered.styles
                    }
            in
            gatherHeight len newGathered



-- initGathered : Maybe String -> Gathered msg


initGathered node =
    { attributes = []
    , styles = []
    , node = node

    -- case maybeNodeName of
    --     Nothing ->
    --         Generic
    --     Just name ->
    --         NodeName name
    , children = []
    , transform = Nothing
    , boxShadows = Nothing
    , textShadows = Nothing
    , has = Flag.none
    }


{-| -}
renderTransformationGroup : Maybe PseudoClass -> TransformationGroup -> Maybe ( String, Style )
renderTransformationGroup maybePseudo group =
    let
        translate =
            Maybe.map
                (\( x, y, z ) ->
                    "translate3d("
                        ++ String.fromFloat (Maybe.withDefault 0 x)
                        ++ "px, "
                        ++ String.fromFloat (Maybe.withDefault 0 y)
                        ++ "px, "
                        ++ String.fromFloat (Maybe.withDefault 0 z)
                        ++ "px)"
                )
                group.translate

        scale =
            Maybe.map
                (\( x, y, z ) ->
                    "scale3d(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ", " ++ String.fromFloat z ++ ")"
                )
                group.scale

        rotate =
            Maybe.map
                (\(Rotation x y z angle) ->
                    "rotate3d(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ "," ++ String.fromFloat z ++ "," ++ String.fromFloat angle ++ "rad)"
                )
                group.rotate

        transformations =
            List.filterMap identity
                [ scale
                , translate
                , rotate
                ]

        name =
            String.join "-" <|
                List.filterMap identity
                    [ Maybe.map
                        (\( x, y, z ) ->
                            "move-"
                                ++ floatClass (Maybe.withDefault 0 x)
                                ++ "-"
                                ++ floatClass (Maybe.withDefault 0 y)
                                ++ "-"
                                ++ floatClass (Maybe.withDefault 0 z)
                        )
                        group.translate
                    , Maybe.map
                        (\( x, y, z ) ->
                            "scale" ++ floatClass x ++ "-" ++ floatClass y ++ "-" ++ floatClass z
                        )
                        group.scale
                    , Maybe.map
                        (\(Rotation x y z angle) ->
                            "rotate-" ++ floatClass x ++ "-" ++ floatClass y ++ "-" ++ floatClass z ++ "-" ++ floatClass angle
                        )
                        group.rotate
                    ]
    in
    case transformations of
        [] ->
            Nothing

        trans ->
            let
                transforms =
                    String.join " " trans

                ( classOnElement, classInStylesheet ) =
                    case maybePseudo of
                        Nothing ->
                            ( "transform-" ++ name
                            , "transform-" ++ name
                            )

                        Just pseudo ->
                            case pseudo of
                                Hover ->
                                    ( "transform-" ++ name ++ "-hover"
                                    , "transform-" ++ name ++ "-hover:hover"
                                    )

                                Focus ->
                                    ( "transform-" ++ name ++ "-focus"
                                    , "transform-" ++ name ++ "-focus:focus, .se:focus ~ .transform-" ++ name ++ "-focus"
                                    )

                                Active ->
                                    ( "transform-" ++ name ++ "-active"
                                    , "transform-" ++ name ++ "-active:active"
                                    )
            in
            Just ( classOnElement, Single classInStylesheet "transform" transforms )


finalize : Gathered msg -> Gathered msg
finalize gathered =
    let
        add new ( classes, styles ) =
            case new of
                Nothing ->
                    ( classes, styles )

                Just ( newClass, newStyle ) ->
                    ( newClass :: classes
                    , newStyle :: styles
                    )

        addTransform ( classes, styles ) =
            case gathered.transform of
                Nothing ->
                    ( classes, styles )

                Just transform ->
                    ( classes, styles )
                        |> add (Maybe.andThen (renderTransformationGroup Nothing) transform.normal)
                        |> add (Maybe.andThen (renderTransformationGroup (Just Focus)) transform.focus)
                        |> add (Maybe.andThen (renderTransformationGroup (Just Hover)) transform.hover)
                        |> add (Maybe.andThen (renderTransformationGroup (Just Active)) transform.active)

        addBoxShadows ( classes, styles ) =
            case gathered.boxShadows of
                Nothing ->
                    ( classes, styles )

                Just ( shadowClass, shades ) ->
                    ( shadowClass :: classes
                    , Single shadowClass "box-shadow" shades
                        :: styles
                    )

        addTextShadows ( classes, styles ) =
            case gathered.textShadows of
                Nothing ->
                    ( classes, styles )

                Just ( shadowClass, shades ) ->
                    ( shadowClass :: classes
                    , Single shadowClass "text-shadow" shades
                        :: styles
                    )

        ( newClasses, newStyles ) =
            ( [], gathered.styles )
                |> addBoxShadows
                |> addTextShadows
                |> addTransform
    in
    { gathered
        | styles = newStyles
        , attributes =
            Html.Attributes.class (String.join " " newClasses) :: gathered.attributes
    }


type EmbedStyle
    = NoStyleSheet
    | StaticRootAndDynamic OptionRecord
    | OnlyDynamic OptionRecord


noStyleSheet : EmbedStyle
noStyleSheet =
    NoStyleSheet



-- contextClasses : LayoutContext -> Attribute aligned msg


rowClass =
    Html.Attributes.class (classes.any ++ " " ++ classes.row)


columnClass =
    Html.Attributes.class (classes.any ++ " " ++ classes.column)


singleClass =
    Html.Attributes.class (classes.any ++ " " ++ classes.single)


gridClass =
    Html.Attributes.class (classes.any ++ " " ++ classes.grid)


paragraphClass =
    Html.Attributes.class (classes.any ++ " " ++ classes.paragraph)


pageClass =
    Html.Attributes.class (classes.any ++ " " ++ classes.page)


contextClasses context =
    case context of
        AsRow ->
            rowClass

        AsColumn ->
            columnClass

        AsEl ->
            singleClass

        AsGrid ->
            gridClass

        AsParagraph ->
            paragraphClass

        AsTextColumn ->
            pageClass


element : EmbedStyle -> LayoutContext -> NodeName -> List (Attribute aligned msg) -> Children (Element msg) -> Element msg
element embedMode context node attributes children =
    -- (Attr (contextClasses context) :: attributes)
    -- |> List.foldr gatherAttributes (initGathered node)
    -- |> finalize
    -- |> asElement embedMode children context
    attributes
        |> List.reverse
        |> gatherAttrRecursive node Flag.none untransformed [] [ contextClasses context ] []
        |> createElement embedMode children context


untransformed =
    Untransformed


space =
    VirtualDom.text " "


keyedSpace =
    ( " ", space )


createElement : EmbedStyle -> Children (Element msg) -> LayoutContext -> Gathered msg -> Element msg
createElement embedMode children context rendered =
    let
        gather child ( htmls, existingStyles ) =
            case child of
                Unstyled html ->
                    if context == asParagraph then
                        ( html context :: space :: htmls
                        , existingStyles
                        )
                    else
                        ( html context :: htmls
                        , existingStyles
                        )

                Styled styled ->
                    if context == asParagraph then
                        ( styled.html Nothing context :: space :: htmls
                        , styled.styles ++ existingStyles
                        )
                    else
                        ( styled.html Nothing context :: htmls
                        , styled.styles ++ existingStyles
                        )

                Text str ->
                    -- TEXT OPTIMIZATION
                    -- You can have raw text if the element is an el, and has `width-content` and `height-content`
                    -- Same if it's a column or row with one child and width-content, height-content
                    -- interferes with css grid
                    -- Maybe we could unpack text elements in a paragraph as well,
                    -- however  embedded style elements that are larger than the line height will overlap with exisitng text.
                    -- I don't think that's what we want.
                    if
                        context
                            == asEl
                            || context
                            == asParagraph
                    then
                        ( VirtualDom.text
                            (if context == asParagraph then
                                str ++ " "
                             else
                                str
                            )
                            :: htmls
                        , existingStyles
                        )
                    else
                        ( textElement
                            (if context == asParagraph then
                                str ++ " "
                             else
                                str
                            )
                            :: htmls
                        , existingStyles
                        )

                Empty ->
                    ( htmls, existingStyles )

        gatherKeyed ( key, child ) ( htmls, existingStyles ) =
            case child of
                Unstyled html ->
                    if context == asParagraph then
                        ( ( key, html context ) :: ( "sp", space ) :: htmls
                        , existingStyles
                        )
                    else
                        ( ( key, html context ) :: htmls
                        , existingStyles
                        )

                Styled styled ->
                    if context == asParagraph then
                        ( ( key, styled.html Nothing context )
                            :: ( "sp", space )
                            :: htmls
                        , styled.styles ++ existingStyles
                        )
                    else
                        ( ( key, styled.html Nothing context ) :: htmls
                        , styled.styles ++ existingStyles
                        )

                Text str ->
                    -- TEXT OPTIMIZATION
                    -- You can have raw text if the element is an el, and has `width-content` and `height-content`
                    -- Same if it's a column or row with one child and width-content, height-content
                    if
                        context
                            == asEl
                            || context
                            == asParagraph
                    then
                        ( ( key
                          , VirtualDom.text
                                (if context == asParagraph then
                                    str ++ " "
                                 else
                                    str
                                )
                          )
                            :: htmls
                        , existingStyles
                        )
                    else
                        ( ( key
                          , textElement
                                (if context == asParagraph then
                                    str ++ " "
                                 else
                                    str
                                )
                          )
                            :: htmls
                        , existingStyles
                        )

                Empty ->
                    ( htmls, existingStyles )
    in
    case embedMode of
        NoStyleSheet ->
            case children of
                Keyed keyedChildren ->
                    case List.foldr gatherKeyed ( [], rendered.styles ) keyedChildren of
                        ( keyed, styles ) ->
                            case styles of
                                [] ->
                                    Unstyled
                                        (renderNode rendered
                                            (Keyed <| List.map (\x -> ( "nearby-elements-pls", x )) rendered.children ++ keyed)
                                            Nothing
                                        )

                                _ ->
                                    Styled
                                        { styles = styles
                                        , html = renderNode rendered (Keyed keyed)
                                        }

                Unkeyed unkeyedChildren ->
                    case List.foldr gather ( [], rendered.styles ) unkeyedChildren of
                        ( unkeyed, styles ) ->
                            case styles of
                                [] ->
                                    Unstyled
                                        (renderNode rendered
                                            (Unkeyed <| rendered.children ++ unkeyed)
                                            Nothing
                                        )

                                _ ->
                                    Styled
                                        { styles = styles
                                        , html = renderNode rendered (Unkeyed unkeyed)
                                        }

        StaticRootAndDynamic opts ->
            case children of
                Keyed keyedChildren ->
                    case List.foldr gatherKeyed ( [], rendered.styles ) keyedChildren of
                        ( keyed, styles ) ->
                            Unstyled
                                (renderNode rendered
                                    (Keyed
                                        (( "static-stylesheet", VirtualDom.lazy staticRoot unit )
                                            :: ( "dynamic-stylesheet"
                                               , styles
                                                    |> List.foldl reduceStyles ( Set.empty, [ renderFocusStyle opts.focus ] )
                                                    |> Tuple.second
                                                    |> toStyleSheet opts
                                               )
                                            :: (List.map (\x -> ( "nearby-elements-pls", x )) rendered.children ++ keyed)
                                        )
                                    )
                                    Nothing
                                )

                Unkeyed unkeyedChildren ->
                    case List.foldr gather ( [], rendered.styles ) unkeyedChildren of
                        ( unkeyed, styles ) ->
                            Unstyled
                                (renderNode rendered
                                    (Unkeyed
                                        (VirtualDom.lazy staticRoot unit
                                            :: (styles
                                                    |> List.foldl reduceStyles ( Set.empty, [ renderFocusStyle opts.focus ] )
                                                    |> Tuple.second
                                                    |> toStyleSheet opts
                                               )
                                            :: (rendered.children ++ unkeyed)
                                        )
                                    )
                                    Nothing
                                )

        OnlyDynamic opts ->
            case children of
                Keyed keyedChildren ->
                    case List.foldr gatherKeyed ( [], rendered.styles ) keyedChildren of
                        ( keyed, styles ) ->
                            Unstyled
                                (renderNode rendered
                                    (Keyed
                                        (( "dynamic-stylesheet"
                                         , styles
                                            |> List.foldl reduceStyles ( Set.empty, [ renderFocusStyle opts.focus ] )
                                            |> Tuple.second
                                            |> toStyleSheet opts
                                         )
                                            :: (List.map (\x -> ( "nearby-elements-pls", x )) rendered.children ++ keyed)
                                        )
                                    )
                                    Nothing
                                )

                Unkeyed unkeyedChildren ->
                    case List.foldr gather ( [], rendered.styles ) unkeyedChildren of
                        ( unkeyed, styles ) ->
                            Unstyled
                                (renderNode rendered
                                    (Unkeyed
                                        ((styles
                                            |> List.foldl reduceStyles ( Set.empty, [ renderFocusStyle opts.focus ] )
                                            |> Tuple.second
                                            |> toStyleSheet opts
                                         )
                                            :: (rendered.children ++ unkeyed)
                                        )
                                    )
                                    Nothing
                                )


asElement : EmbedStyle -> Children (Element msg) -> LayoutContext -> Gathered msg -> Element msg
asElement embedMode children context rendered =
    let
        ( htmlChildren, styleChildren ) =
            case children of
                Keyed keyedChildren ->
                    List.foldr gatherKeyed ( [], rendered.styles ) keyedChildren
                        |> Tuple.mapFirst Keyed

                Unkeyed unkeyedChildren ->
                    List.foldr gather ( [], rendered.styles ) unkeyedChildren
                        |> Tuple.mapFirst Unkeyed

        gather child ( htmls, existingStyles ) =
            case child of
                Unstyled html ->
                    if context == asParagraph then
                        ( html context :: space :: htmls
                        , existingStyles
                        )
                    else
                        ( html context :: htmls
                        , existingStyles
                        )

                Styled styled ->
                    if context == asParagraph then
                        ( styled.html Nothing context :: space :: htmls
                        , styled.styles ++ existingStyles
                        )
                    else
                        ( styled.html Nothing context :: htmls
                        , styled.styles ++ existingStyles
                        )

                Text str ->
                    -- TEXT OPTIMIZATION
                    -- You can have raw text if the element is an el, and has `width-content` and `height-content`
                    -- Same if it's a column or row with one child and width-content, height-content
                    -- interferes with css grid
                    -- Maybe we could unpack text elements in a paragraph as well,
                    -- however  embedded style elements that are larger than the line height will overlap with exisitng text.
                    -- I don't think that's what we want.
                    if
                        context
                            == asEl
                            -- && Flag.present Flag.widthContent rendered.has
                            -- && Flag.present Flag.heightContent rendered.has
                            -- && (not <| Flag.present Flag.behind rendered.has)
                            || context
                            == asParagraph
                        -- && (not <| Flag.present Flag.behind rendered.has)
                    then
                        ( VirtualDom.text
                            (if context == asParagraph then
                                str ++ " "
                             else
                                str
                            )
                            :: htmls
                        , existingStyles
                        )
                    else
                        ( textElement
                            (if context == asParagraph then
                                str ++ " "
                             else
                                str
                            )
                            :: htmls
                        , existingStyles
                        )

                Empty ->
                    ( htmls, existingStyles )

        gatherKeyed ( key, child ) ( htmls, existingStyles ) =
            case child of
                Unstyled html ->
                    if context == asParagraph then
                        ( ( key, html context ) :: ( "sp", space ) :: htmls
                        , existingStyles
                        )
                    else
                        ( ( key, html context ) :: htmls
                        , existingStyles
                        )

                Styled styled ->
                    if context == asParagraph then
                        ( ( key, styled.html Nothing context )
                            :: ( "sp", space )
                            :: htmls
                        , styled.styles ++ existingStyles
                        )
                    else
                        ( ( key, styled.html Nothing context ) :: htmls
                        , styled.styles ++ existingStyles
                        )

                Text str ->
                    -- TEXT OPTIMIZATION
                    -- You can have raw text if the element is an el, and has `width-content` and `height-content`
                    -- Same if it's a column or row with one child and width-content, height-content
                    if
                        context
                            == asEl
                            -- && (not <| Flag.present Flag.behind rendered.has)
                            || context
                            == asParagraph
                        -- && (not <| Flag.present Flag.behind rendered.has)
                    then
                        ( ( key
                          , VirtualDom.text
                                (if context == asParagraph then
                                    str ++ " "
                                 else
                                    str
                                )
                          )
                            :: htmls
                        , existingStyles
                        )
                    else
                        ( ( key
                          , textElement
                                (if context == asParagraph then
                                    str ++ " "
                                 else
                                    str
                                )
                          )
                            :: htmls
                        , existingStyles
                        )

                Empty ->
                    ( htmls, existingStyles )

        ( renderStatic, renderDynamic, options ) =
            case embedMode of
                NoStyleSheet ->
                    ( False, False, defaultOptions )

                StaticRootAndDynamic opts ->
                    ( True, True, opts )

                OnlyDynamic opts ->
                    ( False, True, opts )

        styles =
            case embedMode of
                NoStyleSheet ->
                    []

                _ ->
                    styleChildren
                        |> List.foldl reduceStyles ( Set.empty, [ renderFocusStyle options.focus ] )
                        |> Tuple.second

        renderedChildren =
            case htmlChildren of
                Keyed keyed ->
                    Keyed
                        ((List.map (\x -> ( "nearby-elements-pls", x )) rendered.children ++ keyed)
                            |> addWhen renderDynamic
                                ( "dynamic-stylesheet", toStyleSheet options styles )
                            |> addWhen renderStatic
                                ( "static-stylesheet", VirtualDom.lazy staticRoot unit )
                        )

                Unkeyed unkeyed ->
                    Unkeyed
                        ((rendered.children ++ unkeyed)
                            |> addWhen renderDynamic (toStyleSheet options styles)
                            |> addWhen renderStatic (VirtualDom.lazy staticRoot unit)
                         -- (VirtualDom.lazy staticRoot unit)
                        )
    in
    case embedMode of
        NoStyleSheet ->
            case styleChildren of
                [] ->
                    Unstyled (renderNode rendered renderedChildren Nothing)

                _ ->
                    Styled
                        { styles = styleChildren
                        , html = renderNode rendered renderedChildren
                        }

        _ ->
            Unstyled
                (renderNode rendered
                    renderedChildren
                    Nothing
                )


unit =
    0


defaultOptions =
    { hover = AllowHover
    , focus = focusDefaultStyle
    , mode = Layout
    }


staticRoot _ =
    VirtualDom.node "style" [] [ VirtualDom.text Internal.Style.rules ]


addWhen ifThis x to =
    if ifThis then
        x :: to
    else
        to


{-| TODO:

This doesn't reduce equivalent attributes completely.

-}
filter : List (Attribute aligned msg) -> List (Attribute aligned msg)
filter attrs =
    Tuple.first <|
        List.foldr
            (\x ( found, has ) ->
                case x of
                    NoAttribute ->
                        ( found, has )

                    Class key _ ->
                        ( x :: found, has )

                    Attr attr ->
                        ( x :: found, has )

                    StyleClass _ style ->
                        ( x :: found, has )

                    Width width ->
                        if Set.member "width" has then
                            ( found, has )
                        else
                            ( x :: found, Set.insert "width" has )

                    Height height ->
                        if Set.member "height" has then
                            ( found, has )
                        else
                            ( x :: found, Set.insert "height" has )

                    Describe description ->
                        if Set.member "described" has then
                            ( found, has )
                        else
                            ( x :: found, Set.insert "described" has )

                    Nearby location elem ->
                        ( x :: found, has )

                    AlignX _ ->
                        if Set.member "align-x" has then
                            ( found, has )
                        else
                            ( x :: found, Set.insert "align-x" has )

                    AlignY _ ->
                        if Set.member "align-y" has then
                            ( found, has )
                        else
                            ( x :: found, Set.insert "align-y" has )
            )
            ( [], Set.empty )
            attrs


get : List (Attribute aligned msg) -> (Attribute aligned msg -> Bool) -> List (Attribute aligned msg)
get attrs isAttr =
    attrs
        |> filter
        |> List.foldr
            (\x found ->
                if isAttr x then
                    x :: found
                else
                    found
            )
            []


getSpacing : List (Attribute aligned msg) -> ( Int, Int ) -> ( Int, Int )
getSpacing attrs default =
    attrs
        |> List.foldr
            (\attr acc ->
                case acc of
                    Just x ->
                        Just x

                    Nothing ->
                        case attr of
                            StyleClass _ (SpacingStyle _ x y) ->
                                Just ( x, y )

                            _ ->
                                Nothing
            )
            Nothing
        |> Maybe.withDefault default


getSpacingAttribute : List (Attribute aligned msg) -> ( Int, Int ) -> Attribute aligned msg1
getSpacingAttribute attrs default =
    attrs
        |> List.foldr
            (\attr acc ->
                case acc of
                    Just x ->
                        Just x

                    Nothing ->
                        case attr of
                            StyleClass _ (SpacingStyle _ x y) ->
                                Just ( x, y )

                            _ ->
                                Nothing
            )
            Nothing
        |> Maybe.withDefault default
        |> (\( x, y ) -> StyleClass Flag.spacing (SpacingStyle (spacingName x y) x y))


textElement : String -> VirtualDom.Node msg
textElement str =
    Html.div
        [ Html.Attributes.class
            (String.join " "
                [ classes.any
                , classes.text
                , classes.widthContent
                , classes.heightContent
                ]
            )
        ]
        [ Html.text str ]


textElementFill : String -> VirtualDom.Node msg
textElementFill str =
    VirtualDom.node "div"
        [ VirtualDom.property "className"
            (Json.string <|
                String.join " "
                    [ classes.any
                    , classes.text
                    , classes.widthFill
                    , classes.heightFill
                    ]
            )
        ]
        [ VirtualDom.text str ]


type Children x
    = Unkeyed (List x)
    | Keyed (List ( String, x ))


toHtml : OptionRecord -> Element msg -> VirtualDom.Node msg
toHtml options el =
    case el of
        Unstyled html ->
            html asEl

        Styled { styles, html } ->
            let
                styleSheet =
                    styles
                        |> List.foldl reduceStyles ( Set.empty, [ renderFocusStyle options.focus ] )
                        |> Tuple.second
                        |> toStyleSheetString options
            in
            html (Just styleSheet) asEl

        Text text ->
            textElement text

        Empty ->
            textElement ""


{-| -}
renderRoot : List Option -> List (Attribute aligned msg) -> Element msg -> VirtualDom.Node msg
renderRoot optionList attributes child =
    let
        options =
            optionsToRecord optionList

        embedStyle =
            case options.mode of
                NoStaticStyleSheet ->
                    OnlyDynamic options

                _ ->
                    StaticRootAndDynamic options
    in
    element embedStyle asEl div attributes (Unkeyed [ child ])
        |> toHtml options


type RenderMode
    = Viewport
    | Layout
    | NoStaticStyleSheet
    | WithVirtualCss


type alias OptionRecord =
    { hover : HoverSetting
    , focus : FocusStyle
    , mode : RenderMode
    }


type HoverSetting
    = NoHover
    | AllowHover
    | ForceHover


type Option
    = HoverOption HoverSetting
    | FocusStyleOption FocusStyle
    | RenderModeOption RenderMode


type alias FocusStyle =
    { borderColor : Maybe Color
    , shadow : Maybe Shadow
    , backgroundColor : Maybe Color
    }


type alias Shadow =
    { color : Color
    , offset : ( Int, Int )
    , blur : Int
    , size : Int
    }


rootStyle : List (Attribute aligned msg)
rootStyle =
    let
        families =
            [ Typeface "Open Sans"
            , Typeface "Helvetica"
            , Typeface "Verdana"
            , SansSerif
            ]
    in
    [ StyleClass Flag.bgColor (Colored ("bg-color-" ++ formatColorClass (Rgba 1 1 1 1)) "background-color" (Rgba 1 1 1 1))
    , StyleClass Flag.fontColor (Colored ("font-color-" ++ formatColorClass (Rgba 0 0 0 1)) "color" (Rgba 0 0 0 1))
    , StyleClass Flag.fontSize (Single "font-size-20" "font-size" "20px")
    , StyleClass Flag.fontFamily <|
        FontFamily (List.foldl renderFontClassName "font-" families)
            families
    ]


renderFontClassName : Font -> String -> String
renderFontClassName font current =
    current
        ++ (case font of
                Serif ->
                    "serif"

                SansSerif ->
                    "sans-serif"

                Monospace ->
                    "monospace"

                Typeface name ->
                    name
                        |> String.toLower
                        |> String.words
                        |> String.join "-"

                ImportFont name url ->
                    name
                        |> String.toLower
                        |> String.words
                        |> String.join "-"
           )


renderFocusStyle :
    FocusStyle
    -> Style
renderFocusStyle focus =
    Style (classes.any ++ ":focus .focusable, " ++ Internal.Style.dot classes.any ++ ".focusable:focus")
        (List.filterMap identity
            [ Maybe.map (\color -> Property "border-color" (formatColor color)) focus.borderColor
            , Maybe.map (\color -> Property "background-color" (formatColor color)) focus.backgroundColor
            , Maybe.map
                (\shadow ->
                    Property "box-shadow"
                        (formatBoxShadow
                            { color = shadow.color
                            , offset =
                                shadow.offset
                                    |> Tuple.mapFirst toFloat
                                    |> Tuple.mapSecond toFloat
                            , inset = False
                            , blur = toFloat shadow.blur
                            , size = toFloat shadow.size
                            }
                        )
                )
                focus.shadow
            , Just <| Property "outline" "none"
            ]
        )


focusDefaultStyle : { backgroundColor : Maybe Color, borderColor : Maybe Color, shadow : Maybe Shadow }
focusDefaultStyle =
    { backgroundColor = Nothing
    , borderColor = Nothing
    , shadow =
        Just
            { color =
                Rgba (155 / 255) (203 / 255) 1 1
            , offset = ( 0, 0 )
            , blur = 3
            , size = 3
            }
    }


optionsToRecord : List Option -> OptionRecord
optionsToRecord options =
    let
        combine opt record =
            case opt of
                HoverOption hoverable ->
                    case record.hover of
                        Nothing ->
                            { record | hover = Just hoverable }

                        _ ->
                            record

                FocusStyleOption focusStyle ->
                    case record.focus of
                        Nothing ->
                            { record | focus = Just focusStyle }

                        _ ->
                            record

                RenderModeOption renderMode ->
                    case record.mode of
                        Nothing ->
                            { record | mode = Just renderMode }

                        _ ->
                            record

        andFinally record =
            { hover =
                case record.hover of
                    Nothing ->
                        AllowHover

                    Just hoverable ->
                        hoverable
            , focus =
                case record.focus of
                    Nothing ->
                        focusDefaultStyle

                    Just focusable ->
                        focusable
            , mode =
                case record.mode of
                    Nothing ->
                        Layout

                    Just actualMode ->
                        actualMode
            }
    in
    andFinally <|
        List.foldr combine
            { hover = Nothing
            , focus = Nothing
            , mode = Nothing
            }
            options


renderFont : List Font -> String
renderFont families =
    let
        fontName font =
            case font of
                Serif ->
                    "serif"

                SansSerif ->
                    "sans-serif"

                Monospace ->
                    "monospace"

                Typeface name ->
                    "\"" ++ name ++ "\""

                ImportFont name url ->
                    "\"" ++ name ++ "\""
    in
    families
        |> List.map fontName
        |> String.join ", "


reduceStyles : Style -> ( Set String, List Style ) -> ( Set String, List Style )
reduceStyles style ( cache, existing ) =
    let
        styleName =
            getStyleName style
    in
    if Set.member styleName cache then
        ( cache, existing )
    else
        ( Set.insert styleName cache
        , style :: existing
        )


toStyleSheet : OptionRecord -> List Style -> VirtualDom.Node msg
toStyleSheet options styleSheet =
    VirtualDom.node "style" [] [ VirtualDom.text (toStyleSheetString options styleSheet) ]


toStyleSheetString : OptionRecord -> List Style -> String
toStyleSheetString options stylesheet =
    let
        renderProps force (Property key val) existing =
            if force then
                existing ++ "\n  " ++ key ++ ": " ++ val ++ " !important;"
            else
                existing ++ "\n  " ++ key ++ ": " ++ val ++ ";"

        renderStyle force maybePseudo selector props =
            case maybePseudo of
                Nothing ->
                    selector ++ "{" ++ List.foldl (renderProps force) "" props ++ "\n}"

                Just pseudo ->
                    case pseudo of
                        Hover ->
                            selector ++ ":hover {" ++ List.foldl (renderProps force) "" props ++ "\n}"

                        Focus ->
                            let
                                renderedProps =
                                    List.foldl (renderProps force) "" props
                            in
                            String.join "\n"
                                [ selector
                                    ++ ":focus {"
                                    ++ renderedProps
                                    ++ "\n}"
                                , ".se:focus ~ "
                                    ++ selector
                                    ++ ":not(.focus)  {"
                                    ++ renderedProps
                                    ++ "\n}"
                                , ".se:focus "
                                    ++ selector
                                    ++ "  {"
                                    ++ renderedProps
                                    ++ "\n}"
                                ]

                        Active ->
                            selector ++ ":active {" ++ List.foldl (renderProps force) "" props ++ "\n}"

        renderStyleRule rule maybePseudo force =
            case rule of
                Style selector props ->
                    renderStyle force maybePseudo selector props

                Shadows name prop ->
                    renderStyle force
                        maybePseudo
                        ("." ++ name)
                        [ Property "box-shadow" prop
                        ]

                Transparency name transparency ->
                    let
                        opacity =
                            (1 - transparency)
                                |> min 1
                                |> max 0
                    in
                    renderStyle force
                        maybePseudo
                        ("." ++ name)
                        [ Property "opacity" (String.fromFloat opacity)
                        ]

                FontSize i ->
                    renderStyle force
                        maybePseudo
                        (".font-size-" ++ String.fromInt i)
                        [ Property "font-size" (String.fromInt i ++ "px")
                        ]

                FontFamily name typefaces ->
                    renderStyle force
                        maybePseudo
                        ("." ++ name)
                        [ Property "font-family" (renderFont typefaces)
                        ]

                Single class prop val ->
                    renderStyle force
                        maybePseudo
                        ("." ++ class)
                        [ Property prop val
                        ]

                Colored class prop color ->
                    renderStyle force
                        maybePseudo
                        ("." ++ class)
                        [ Property prop (formatColor color)
                        ]

                SpacingStyle cls x y ->
                    let
                        class =
                            "." ++ cls

                        xPx =
                            String.fromInt x ++ "px"

                        yPx =
                            String.fromInt y ++ "px"

                        row =
                            "." ++ .row Internal.Style.classes

                        column =
                            "." ++ .column Internal.Style.classes

                        page =
                            "." ++ .page Internal.Style.classes

                        paragraph =
                            "." ++ .paragraph Internal.Style.classes

                        left =
                            "." ++ .alignLeft Internal.Style.classes

                        right =
                            "." ++ .alignRight Internal.Style.classes

                        any =
                            "." ++ .any Internal.Style.classes
                    in
                    List.foldl (++)
                        ""
                        [ renderStyle force maybePseudo (class ++ row ++ " > " ++ any ++ " + " ++ any) [ Property "margin-left" xPx ]
                        , renderStyle force maybePseudo (class ++ column ++ " > " ++ any ++ " + " ++ any) [ Property "margin-top" yPx ]
                        , renderStyle force maybePseudo (class ++ page ++ " > " ++ any ++ " + " ++ any) [ Property "margin-top" yPx ]
                        , renderStyle force maybePseudo (class ++ page ++ " > " ++ left) [ Property "margin-right" xPx ]
                        , renderStyle force maybePseudo (class ++ page ++ " > " ++ right) [ Property "margin-left" xPx ]
                        , renderStyle force
                            maybePseudo
                            (class ++ paragraph)
                            [ Property "line-height" ("calc(1em + " ++ String.fromInt y ++ "px)")
                            ]
                        , renderStyle force
                            maybePseudo
                            ("textarea" ++ class)
                            [ Property "line-height" ("calc(1em + " ++ String.fromInt y ++ "px)")
                            ]

                        -- , renderStyle force
                        --     maybePseudo
                        --     (class ++ paragraph ++ " > " ++ any)
                        --     [ Property "margin-right" xPx
                        --     , Property "margin-bottom" yPx
                        --     ]
                        , renderStyle force
                            maybePseudo
                            (class ++ paragraph ++ " > " ++ left)
                            [ Property "margin-right" xPx
                            ]
                        , renderStyle force
                            maybePseudo
                            (class ++ paragraph ++ " > " ++ right)
                            [ Property "margin-left" xPx
                            ]
                        , renderStyle force
                            maybePseudo
                            (class ++ paragraph ++ "::after")
                            [ Property "content" "''"
                            , Property "display" "block"
                            , Property "height" "0"
                            , Property "width" "0"
                            , Property "margin-top" (String.fromInt (-1 * (y // 2)) ++ "px")
                            ]
                        , renderStyle force
                            maybePseudo
                            (class ++ paragraph ++ "::before")
                            [ Property "content" "''"
                            , Property "display" "block"
                            , Property "height" "0"
                            , Property "width" "0"
                            , Property "margin-bottom" (String.fromInt (-1 * (y // 2)) ++ "px")
                            ]
                        ]

                PaddingStyle cls top right bottom left ->
                    let
                        class =
                            "."
                                ++ cls
                    in
                    renderStyle force
                        maybePseudo
                        class
                        [ Property "padding"
                            (String.fromInt top
                                ++ "px "
                                ++ String.fromInt right
                                ++ "px "
                                ++ String.fromInt bottom
                                ++ "px "
                                ++ String.fromInt left
                                ++ "px"
                            )
                        ]

                GridTemplateStyle template ->
                    let
                        class =
                            ".grid-rows-"
                                ++ String.join "-" (List.map lengthClassName template.rows)
                                ++ "-cols-"
                                ++ String.join "-" (List.map lengthClassName template.columns)
                                ++ "-space-x-"
                                ++ lengthClassName (Tuple.first template.spacing)
                                ++ "-space-y-"
                                ++ lengthClassName (Tuple.second template.spacing)

                        ySpacing =
                            toGridLength (Tuple.second template.spacing)

                        xSpacing =
                            toGridLength (Tuple.first template.spacing)

                        toGridLength x =
                            toGridLengthHelper Nothing Nothing x

                        toGridLengthHelper minimum maximum x =
                            case x of
                                Px px ->
                                    String.fromInt px ++ "px"

                                Content ->
                                    case ( minimum, maximum ) of
                                        ( Nothing, Nothing ) ->
                                            "max-content"

                                        ( Just minSize, Nothing ) ->
                                            "minmax(" ++ String.fromInt minSize ++ "px, " ++ "max-content)"

                                        ( Nothing, Just maxSize ) ->
                                            "minmax(max-content, " ++ String.fromInt maxSize ++ "px)"

                                        ( Just minSize, Just maxSize ) ->
                                            "minmax(" ++ String.fromInt minSize ++ "px, " ++ String.fromInt maxSize ++ "px)"

                                Fill i ->
                                    case ( minimum, maximum ) of
                                        ( Nothing, Nothing ) ->
                                            String.fromInt i ++ "fr"

                                        ( Just minSize, Nothing ) ->
                                            "minmax(" ++ String.fromInt minSize ++ "px, " ++ String.fromInt i ++ "fr" ++ "fr)"

                                        ( Nothing, Just maxSize ) ->
                                            "minmax(max-content, " ++ String.fromInt maxSize ++ "px)"

                                        ( Just minSize, Just maxSize ) ->
                                            "minmax(" ++ String.fromInt minSize ++ "px, " ++ String.fromInt maxSize ++ "px)"

                                Min m len ->
                                    toGridLengthHelper (Just m) maximum len

                                Max m len ->
                                    toGridLengthHelper minimum (Just m) len

                        msColumns =
                            template.columns
                                |> List.map toGridLength
                                |> String.join ySpacing
                                |> (\x -> "-ms-grid-columns: " ++ x ++ ";")

                        msRows =
                            template.columns
                                |> List.map toGridLength
                                |> String.join ySpacing
                                |> (\x -> "-ms-grid-rows: " ++ x ++ ";")

                        base =
                            class ++ "{" ++ msColumns ++ msRows ++ "}"

                        columns =
                            template.columns
                                |> List.map toGridLength
                                |> String.join " "
                                |> (\x -> "grid-template-columns: " ++ x ++ ";")

                        rows =
                            template.rows
                                |> List.map toGridLength
                                |> String.join " "
                                |> (\x -> "grid-template-rows: " ++ x ++ ";")

                        gapX =
                            "grid-column-gap:" ++ toGridLength (Tuple.first template.spacing) ++ ";"

                        gapY =
                            "grid-row-gap:" ++ toGridLength (Tuple.second template.spacing) ++ ";"

                        modernGrid =
                            class ++ "{" ++ columns ++ rows ++ gapX ++ gapY ++ "}"

                        supports =
                            "@supports (display:grid) {" ++ modernGrid ++ "}"
                    in
                    base ++ supports

                GridPosition position ->
                    let
                        class =
                            ".grid-pos-"
                                ++ String.fromInt position.row
                                ++ "-"
                                ++ String.fromInt position.col
                                ++ "-"
                                ++ String.fromInt position.width
                                ++ "-"
                                ++ String.fromInt position.height

                        msPosition =
                            String.join " "
                                [ "-ms-grid-row: "
                                    ++ String.fromInt position.row
                                    ++ ";"
                                , "-ms-grid-row-span: "
                                    ++ String.fromInt position.height
                                    ++ ";"
                                , "-ms-grid-column: "
                                    ++ String.fromInt position.col
                                    ++ ";"
                                , "-ms-grid-column-span: "
                                    ++ String.fromInt position.width
                                    ++ ";"
                                ]

                        base =
                            class ++ "{" ++ msPosition ++ "}"

                        modernPosition =
                            String.join " "
                                [ "grid-row: "
                                    ++ String.fromInt position.row
                                    ++ " / "
                                    ++ String.fromInt (position.row + position.height)
                                    ++ ";"
                                , "grid-column: "
                                    ++ String.fromInt position.col
                                    ++ " / "
                                    ++ String.fromInt (position.col + position.width)
                                    ++ ";"
                                ]

                        modernGrid =
                            class ++ "{" ++ modernPosition ++ "}"

                        supports =
                            "@supports (display:grid) {" ++ modernGrid ++ "}"
                    in
                    base ++ supports

                PseudoSelector class styles ->
                    let
                        renderPseudoRule style =
                            case class of
                                Focus ->
                                    renderStyleRule style (Just Focus) False

                                Active ->
                                    renderStyleRule style (Just Active) False

                                Hover ->
                                    case options.hover of
                                        NoHover ->
                                            ""

                                        AllowHover ->
                                            renderStyleRule style (Just Hover) False

                                        ForceHover ->
                                            renderStyleRule style Nothing True
                    in
                    List.map renderPseudoRule styles
                        |> String.join " "

                Transform _ ->
                    ""

        renderTopLevels rule =
            case rule of
                FontFamily name typefaces ->
                    let
                        getImports font =
                            case font of
                                ImportFont _ url ->
                                    Just ("@import url('" ++ url ++ "');")

                                _ ->
                                    Nothing
                    in
                    typefaces
                        |> List.filterMap getImports
                        |> String.join "\n"
                        |> Just

                _ ->
                    Nothing

        combine style rendered =
            { rendered
                | rules = rendered.rules ++ renderStyleRule style Nothing False
                , topLevel =
                    case renderTopLevels style of
                        Nothing ->
                            rendered.topLevel

                        Just topLevel ->
                            rendered.topLevel ++ topLevel
            }
    in
    List.foldl combine { rules = "", topLevel = "" } stylesheet
        |> (\{ rules, topLevel } -> topLevel ++ rules)


lengthClassName : Length -> String
lengthClassName x =
    case x of
        Px px ->
            String.fromInt px ++ "px"

        Content ->
            "auto"

        Fill i ->
            String.fromInt i ++ "fr"

        Min min len ->
            "min" ++ String.fromInt min ++ lengthClassName len

        Max max len ->
            "max" ++ String.fromInt max ++ lengthClassName len


formatDropShadow shadow =
    String.join " "
        [ String.fromFloat (Tuple.first shadow.offset) ++ "px"
        , String.fromFloat (Tuple.second shadow.offset) ++ "px"
        , String.fromFloat shadow.blur ++ "px"
        , formatColor shadow.color
        ]


formatTextShadow shadow =
    String.join " "
        [ String.fromFloat (Tuple.first shadow.offset) ++ "px"
        , String.fromFloat (Tuple.second shadow.offset) ++ "px"
        , String.fromFloat shadow.blur ++ "px"
        , formatColor shadow.color
        ]


textShadowName shadow =
    String.concat
        [ "txt"
        , String.fromFloat (Tuple.first shadow.offset) ++ "px"
        , String.fromFloat (Tuple.second shadow.offset) ++ "px"
        , String.fromFloat shadow.blur ++ "px"
        , formatColorClass shadow.color
        ]


formatBoxShadow shadow =
    String.join " " <|
        List.filterMap identity
            [ if shadow.inset then
                Just "inset"
              else
                Nothing
            , Just <| String.fromFloat (Tuple.first shadow.offset) ++ "px"
            , Just <| String.fromFloat (Tuple.second shadow.offset) ++ "px"
            , Just <| String.fromFloat shadow.blur ++ "px"
            , Just <| String.fromFloat shadow.size ++ "px"
            , Just <| formatColor shadow.color
            ]


boxShadowName shadow =
    String.concat <|
        [ if shadow.inset then
            "box-inset"
          else
            "box-"
        , String.fromFloat (Tuple.first shadow.offset) ++ "px"
        , String.fromFloat (Tuple.second shadow.offset) ++ "px"
        , String.fromFloat shadow.blur ++ "px"
        , String.fromFloat shadow.size ++ "px"
        , formatColorClass shadow.color
        ]


floatClass : Float -> String
floatClass x =
    String.fromInt (round (x * 255))


formatColor : Color -> String
formatColor (Rgba red green blue alpha) =
    "rgba("
        ++ String.fromInt (round (red * 255))
        ++ ("," ++ String.fromInt (round (green * 255)))
        ++ ("," ++ String.fromInt (round (blue * 255)))
        ++ ("," ++ String.fromFloat alpha ++ ")")


formatColorClass : Color -> String
formatColorClass (Rgba red green blue alpha) =
    floatClass red
        ++ "-"
        ++ floatClass green
        ++ "-"
        ++ floatClass blue
        ++ "-"
        ++ floatClass alpha


pseudoClassName : PseudoClass -> String
pseudoClassName class =
    case class of
        Focus ->
            "focus"

        Hover ->
            "hover"

        Active ->
            "active"


spacingName x y =
    "spacing-" ++ String.fromInt x ++ "-" ++ String.fromInt y


paddingName top right bottom left =
    "pad-"
        ++ String.fromInt top
        ++ "-"
        ++ String.fromInt right
        ++ "-"
        ++ String.fromInt bottom
        ++ "-"
        ++ String.fromInt left


getStyleName : Style -> String
getStyleName style =
    case style of
        Shadows name _ ->
            name

        Transparency name o ->
            name

        Style class _ ->
            class

        FontFamily name _ ->
            name

        FontSize i ->
            "font-size-" ++ String.fromInt i

        Single class _ _ ->
            class

        Colored class _ _ ->
            class

        SpacingStyle cls x y ->
            cls

        PaddingStyle cls top right bottom left ->
            cls

        GridTemplateStyle template ->
            "grid-rows-"
                ++ String.join "-" (List.map lengthClassName template.rows)
                ++ "-cols-"
                ++ String.join "-" (List.map lengthClassName template.columns)
                ++ "-space-x-"
                ++ lengthClassName (Tuple.first template.spacing)
                ++ "-space-y-"
                ++ lengthClassName (Tuple.second template.spacing)

        GridPosition pos ->
            "gp grid-pos-"
                ++ String.fromInt pos.row
                ++ "-"
                ++ String.fromInt pos.col
                ++ "-"
                ++ String.fromInt pos.width
                ++ "-"
                ++ String.fromInt pos.height

        PseudoSelector selector subStyle ->
            pseudoClassName selector
                :: List.map getStyleName subStyle
                |> String.join " "

        Transform _ ->
            "transformation"



{- Constants -}


asGrid : LayoutContext
asGrid =
    AsGrid


asRow : LayoutContext
asRow =
    AsRow


asColumn : LayoutContext
asColumn =
    AsColumn


asEl : LayoutContext
asEl =
    AsEl


asParagraph : LayoutContext
asParagraph =
    AsParagraph


asTextColumn : LayoutContext
asTextColumn =
    AsTextColumn



{- Mapping -}


map : (msg -> msg1) -> Element msg -> Element msg1
map fn el =
    case el of
        Styled styled ->
            Styled
                { styles = styled.styles
                , html = \add context -> VirtualDom.map fn <| styled.html add context
                }

        Unstyled html ->
            Unstyled (VirtualDom.map fn << html)

        Text str ->
            Text str

        Empty ->
            Empty


mapAttr : (msg -> msg1) -> Attribute aligned msg -> Attribute aligned msg1
mapAttr fn attr =
    case attr of
        NoAttribute ->
            NoAttribute

        Describe description ->
            Describe description

        AlignX x ->
            AlignX x

        AlignY y ->
            AlignY y

        Width x ->
            Width x

        Height x ->
            Height x

        Class x y ->
            Class x y

        StyleClass flag style ->
            StyleClass flag style

        Nearby location elem ->
            Nearby location (map fn elem)

        Attr htmlAttr ->
            Attr (VirtualDom.mapAttribute fn htmlAttr)


mapAttrFromStyle : (msg -> msg1) -> Attribute Never msg -> Attribute () msg1
mapAttrFromStyle fn attr =
    case attr of
        NoAttribute ->
            NoAttribute

        Describe description ->
            Describe description

        AlignX x ->
            AlignX x

        AlignY y ->
            AlignY y

        Width x ->
            Width x

        Height x ->
            Height x

        -- invalidation key "border-color" as opposed to "border-color-10-10-10" that will be the key for the class
        Class x y ->
            Class x y

        StyleClass flag style ->
            StyleClass flag style

        Nearby location elem ->
            Nearby location (map fn elem)

        Attr htmlAttr ->
            Attr (VirtualDom.mapAttribute fn htmlAttr)


unwrapDecorations : List (Attribute Never Never) -> List Style
unwrapDecorations attrs =
    let
        joinShadows x styles =
            case x of
                Shadows name shadowProps ->
                    case styles.shadows of
                        Nothing ->
                            { styles | shadows = Just ( name, shadowProps ) }

                        Just ( existingName, existingShadow ) ->
                            { styles | shadows = Just ( existingName ++ name, existingShadow ++ ", " ++ shadowProps ) }

                _ ->
                    { styles | styles = x :: styles.styles }

        addShadow styles =
            case styles.shadows of
                Nothing ->
                    styles.styles

                Just ( shadowName, shadowProps ) ->
                    Shadows shadowName shadowProps :: styles.styles
    in
    attrs
        |> List.filterMap (onlyStyles << removeNever)
        |> List.foldr joinShadows { shadows = Nothing, styles = [] }
        |> addShadow


removeNever : Attribute Never Never -> Attribute () msg
removeNever style =
    mapAttrFromStyle Basics.never style


tag : String -> Style -> Style
tag label style =
    case style of
        Single class prop val ->
            Single (label ++ "-" ++ class) prop val

        Colored class prop val ->
            Colored (label ++ "-" ++ class) prop val

        Style class props ->
            Style (label ++ "-" ++ class) props

        Transparency class o ->
            Transparency (label ++ "-" ++ class) o

        x ->
            x


onlyStyles : Attribute aligned msg -> Maybe Style
onlyStyles attr =
    case attr of
        StyleClass _ style ->
            Just style

        _ ->
            Nothing
