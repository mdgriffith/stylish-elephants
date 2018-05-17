module Internal.Model exposing (..)

{-| -}

import Internal.Flag as Flag exposing (Flag)
import Internal.Style exposing (classes)
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
    | SpacingStyle Int Int
    | PaddingStyle Int Int Int Int
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
    = Move (Maybe Float) (Maybe Float) (Maybe Float)
    | Rotate Float Float Float Float
    | Scale Float Float Float


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
    | TextShadow
        { offset : ( Float, Float )
        , blur : Float
        , color : Color
        }
    | BoxShadow
        { inset : Bool
        , offset : ( Float, Float )
        , size : Float
        , blur : Float
        , color : Color
        }
    | Filter FilterType


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


type FilterType
    = FilterUrl String
    | Blur Float
    | Brightness Float
    | Contrast Float
    | Grayscale Float
    | HueRotate Float
    | Invert Float
    | OpacityFilter Float
    | Saturate Float
    | Sepia Float
    | DropShadow
        { offset : ( Float, Float )
        , size : Float
        , blur : Float
        , color : Color
        }


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


type alias Gathered msg =
    { node : NodeName
    , attributes : List (VirtualDom.Attribute msg)
    , styles : List Style
    , alignment : Aligned
    , width : Maybe Length
    , height : Maybe Length
    , nearbys : Maybe (List ( Location, Element msg ))
    , filters : Maybe String
    , boxShadows : Maybe ( String, String )
    , textShadows : Maybe ( String, String )
    , transform : Maybe (Decorated TransformationGroup)
    , has : Flag
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
    Attr <| VirtualDom.property "className" (Json.string cls)


vDomClass cls =
    VirtualDom.property "className" (Json.string cls)


{-| -}
unstyled : VirtualDom.Node msg -> Element msg
unstyled =
    Unstyled << always


{-| -}
renderNode : Gathered msg -> Children (VirtualDom.Node msg) -> Maybe String -> LayoutContext -> VirtualDom.Node msg
renderNode { alignment, attributes, node, width, height } children styles context =
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
                                , VirtualDom.node "style" [ vDomClass "stylesheet" ] [ VirtualDom.text stylesheet ]
                                )
                                    :: keyed
                        )

                Unkeyed unkeyed ->
                    VirtualDom.node nodeName
                        attrs
                        (case withStyles of
                            Nothing ->
                                unkeyed

                            Just stylesheet ->
                                VirtualDom.node "style" [ vDomClass "stylesheet" ] [ VirtualDom.text stylesheet ] :: unkeyed
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
                        [ createNode internal [ vDomClass (classes.any ++ " " ++ classes.single) ] styles
                        ]
    in
    case context of
        AsRow ->
            case width of
                Just (Fill _) ->
                    html

                _ ->
                    case alignment of
                        Aligned (Just Right) _ ->
                            VirtualDom.node
                                "u"
                                -- "alignRight"
                                [ vDomClass
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

                        Aligned (Just CenterX) _ ->
                            VirtualDom.node
                                "s"
                                -- "centerX"
                                [ vDomClass
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

                        _ ->
                            html

        AsColumn ->
            case height of
                Just (Fill _) ->
                    html

                _ ->
                    case alignment of
                        Aligned _ (Just CenterY) ->
                            VirtualDom.node
                                -- "centerY"
                                "s"
                                [ vDomClass
                                    --"se el container align-container-center-y"
                                    (String.join " "
                                        [ classes.any
                                        , classes.single
                                        , classes.container
                                        , classes.alignContainerCenterY
                                        ]
                                    )
                                ]
                                [ html ]

                        Aligned _ (Just Bottom) ->
                            VirtualDom.node
                                "u"
                                -- "alignBottom"
                                [ vDomClass
                                    -- "se el container align-container-bottom"
                                    (String.join " "
                                        [ classes.any
                                        , classes.single
                                        , classes.container
                                        , classes.alignContainerBottom
                                        ]
                                    )
                                ]
                                [ html ]

                        _ ->
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
            "aligned-horizontally " ++ classes.alignLeft

        Right ->
            "aligned-horizontally " ++ classes.alignRight

        CenterX ->
            "aligned-horizontally " ++ classes.alignCenterX


alignYName : VAlign -> String
alignYName align =
    case align of
        Top ->
            "aligned-vertically " ++ classes.alignTop

        Bottom ->
            "aligned-vertically " ++ classes.alignBottom

        CenterY ->
            "aligned-vertically " ++ classes.alignCenterY


noAreas : List (Attribute aligned msg) -> List (Attribute aligned msg)
noAreas attrs =
    let
        notAnArea a =
            case a of
                Describe _ ->
                    False

                _ ->
                    True
    in
    List.filter notAnArea attrs


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


stackOn : Maybe PseudoClass -> Transformation -> Gathered msg -> Gathered msg
stackOn maybePseudo transform gathered =
    let
        states =
            Maybe.withDefault emptyTransformationStates gathered.transform
    in
    case maybePseudo of
        Nothing ->
            let
                normal =
                    states.normal
            in
            { gathered
                | transform =
                    Just
                        { states
                            | normal =
                                normal
                                    |> Maybe.withDefault emptyTransformGroup
                                    |> stackTransforms transform
                                    |> Just
                        }
            }

        Just Hover ->
            let
                hover =
                    states.hover
            in
            { gathered
                | transform =
                    Just
                        { states
                            | hover =
                                hover
                                    |> Maybe.withDefault emptyTransformGroup
                                    |> stackTransforms transform
                                    |> Just
                        }
            }

        Just Active ->
            let
                active =
                    states.active
            in
            { gathered
                | transform =
                    Just
                        { states
                            | active =
                                active
                                    |> Maybe.withDefault emptyTransformGroup
                                    |> stackTransforms transform
                                    |> Just
                        }
            }

        Just Focus ->
            let
                focus =
                    states.focus
            in
            { gathered
                | transform =
                    Just
                        { states
                            | focus =
                                focus
                                    |> Maybe.withDefault emptyTransformGroup
                                    |> stackTransforms transform
                                    |> Just
                        }
            }


{-| -}
stackTransforms : Transformation -> TransformationGroup -> TransformationGroup
stackTransforms transform group =
    case transform of
        Move mx my mz ->
            case group.translate of
                Nothing ->
                    { group
                        | translate =
                            Just ( mx, my, mz )
                    }

                Just ( existingX, existingY, existingZ ) ->
                    { group
                        | translate =
                            Just
                                ( addIfNothing mx existingX
                                , addIfNothing my existingY
                                , addIfNothing mz existingZ
                                )
                    }

        Rotate x y z angle ->
            { group
                | rotate = addIfNothing (Just (Rotation x y z angle)) group.rotate
            }

        Scale x y z ->
            { group
                | scale = addIfNothing (Just ( x, y, z )) group.scale
            }


gatherAttributes : Attribute aligned msg -> Gathered msg -> Gathered msg
gatherAttributes attr gathered =
    let
        classNameAttr name =
            VirtualDom.property "className" (Json.string name)

        styleName name =
            "." ++ name

        formatStyleClass styleType =
            case styleType of
                Transform x ->
                    Transform x

                Shadows x y ->
                    Shadows x y

                PseudoSelector selector style ->
                    PseudoSelector selector (List.map formatStyleClass style)

                Style cls props ->
                    Style (styleName cls) props

                Single cls name val ->
                    Single (styleName cls) name val

                Colored cls name val ->
                    Colored (styleName cls) name val

                SpacingStyle x y ->
                    SpacingStyle x y

                PaddingStyle top right bottom left ->
                    PaddingStyle top right bottom left

                GridTemplateStyle grid ->
                    GridTemplateStyle grid

                GridPosition pos ->
                    GridPosition pos

                FontFamily name fam ->
                    FontFamily name fam

                FontSize i ->
                    FontSize i

                Transparency name o ->
                    Transparency name o
    in
    case attr of
        NoAttribute ->
            gathered

        Class flag exactClassName ->
            if Flag.present flag gathered.has then
                gathered
            else
                { gathered
                    | attributes = classNameAttr exactClassName :: gathered.attributes
                    , has = Flag.add flag gathered.has
                }

        Attr attribute ->
            { gathered | attributes = attribute :: gathered.attributes }

        StyleClass flag style ->
            let
                addNormalStyle styleProp gatheredProps =
                    if Flag.present flag gatheredProps.has then
                        gatheredProps
                    else
                        { gatheredProps
                            | attributes =
                                case styleProp of
                                    PseudoSelector _ _ ->
                                        VirtualDom.property "className" (Json.string Internal.Style.classes.transition) :: classNameAttr (getStyleName styleProp) :: gatheredProps.attributes

                                    _ ->
                                        classNameAttr (getStyleName styleProp) :: gatheredProps.attributes
                            , styles = formatStyleClass styleProp :: gatheredProps.styles
                            , has = Flag.add flag gatheredProps.has
                        }
            in
            case style of
                Transform transformation ->
                    stackOn Nothing transformation gathered

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
                                |> List.foldr (stackOn (Just pseudo)) gathered
                    in
                    addNormalStyle (PseudoSelector pseudo otherProps) withTransforms

                _ ->
                    addNormalStyle style gathered

        Width width ->
            if gathered.width == Nothing then
                let
                    widthHelper w gath =
                        case w of
                            Px px ->
                                { gath
                                    | attributes = classNameAttr (Internal.Style.classes.widthExact ++ " width-px-" ++ String.fromInt px) :: gath.attributes
                                    , styles = Single (styleName <| "width-px-" ++ String.fromInt px) "width" (String.fromInt px ++ "px") :: gath.styles
                                }

                            Content ->
                                { gath
                                    | attributes = classNameAttr Internal.Style.classes.widthContent :: gath.attributes
                                }

                            Fill portion ->
                                if portion == 1 then
                                    { gath
                                        | attributes = classNameAttr Internal.Style.classes.widthFill :: gath.attributes
                                    }
                                else
                                    { gath
                                        | width = Just width
                                        , attributes = classNameAttr (Internal.Style.classes.widthFillPortion ++ " width-fill-" ++ String.fromInt portion) :: gath.attributes
                                        , styles =
                                            Single ("." ++ Internal.Style.classes.any ++ "." ++ Internal.Style.classes.row ++ " > " ++ (styleName <| "width-fill-" ++ String.fromInt portion)) "flex-grow" (String.fromInt (portion * 100000)) :: gath.styles
                                    }

                            Min minSize len ->
                                let
                                    ( cls, style ) =
                                        ( "min-width-" ++ String.fromInt minSize, Single (".min-width-" ++ String.fromInt minSize) "min-width" (String.fromInt minSize ++ "px") )

                                    newGathered =
                                        { gath
                                            | attributes = classNameAttr cls :: gath.attributes
                                            , styles =
                                                style :: gath.styles
                                        }
                                in
                                widthHelper len newGathered

                            Max maxSize len ->
                                let
                                    ( cls, style ) =
                                        ( "max-width-" ++ String.fromInt maxSize, Single (".max-width-" ++ String.fromInt maxSize) "max-width" (String.fromInt maxSize ++ "px") )

                                    newGathered =
                                        { gath
                                            | attributes = classNameAttr cls :: gath.attributes
                                            , styles =
                                                style :: gath.styles
                                        }
                                in
                                widthHelper len newGathered
                in
                widthHelper width { gathered | width = Just width }
            else
                gathered

        Height height ->
            if gathered.height == Nothing then
                let
                    heightHelper h gath =
                        case h of
                            Px px ->
                                { gath
                                    | attributes = classNameAttr ("height-px-" ++ String.fromInt px) :: gath.attributes
                                    , styles = Single (styleName <| "height-px-" ++ String.fromInt px) "height" (String.fromInt px ++ "px") :: gath.styles
                                }

                            Content ->
                                { gath
                                    | attributes = classNameAttr Internal.Style.classes.heightContent :: gath.attributes
                                }

                            Fill portion ->
                                if portion == 1 then
                                    { gath
                                        | attributes = classNameAttr Internal.Style.classes.heightFill :: gath.attributes
                                    }
                                else
                                    { gath
                                        | attributes = classNameAttr (Internal.Style.classes.heightFillPortion ++ " height-fill-" ++ String.fromInt portion) :: gath.attributes
                                        , styles =
                                            Single ("." ++ Internal.Style.classes.any ++ "." ++ Internal.Style.classes.column ++ " > " ++ (styleName <| "height-fill-" ++ String.fromInt portion)) "flex-grow" (String.fromInt (portion * 100000)) :: gath.styles
                                    }

                            Min minSize len ->
                                let
                                    ( cls, style ) =
                                        ( "min-height-" ++ String.fromInt minSize, Single (".min-height-" ++ String.fromInt minSize) "min-height" (String.fromInt minSize ++ "px") )

                                    newGathered =
                                        { gath
                                            | attributes = classNameAttr cls :: gath.attributes
                                            , styles =
                                                style :: gath.styles
                                        }
                                in
                                heightHelper len newGathered

                            Max maxSize len ->
                                let
                                    ( cls, style ) =
                                        ( "max-height-" ++ String.fromInt maxSize, Single (".max-height-" ++ String.fromInt maxSize) "max-height" (String.fromInt maxSize ++ "px") )

                                    newGathered =
                                        { gath
                                            | attributes = classNameAttr cls :: gath.attributes
                                            , styles =
                                                style :: gath.styles
                                        }
                                in
                                heightHelper len newGathered
                in
                heightHelper height { gathered | height = Just height }
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
            in
            { gathered
                | styles =
                    case styles of
                        Nothing ->
                            gathered.styles

                        Just newStyles ->
                            newStyles
                , nearbys =
                    case gathered.nearbys of
                        Nothing ->
                            Just [ ( location, elem ) ]

                        Just nearby ->
                            Just (( location, elem ) :: nearby)
            }

        AlignX x ->
            case gathered.alignment of
                Unaligned ->
                    { gathered
                        | attributes = classNameAttr (alignXName x) :: gathered.attributes
                        , alignment = Aligned (Just x) Nothing
                    }

                Aligned (Just _) _ ->
                    gathered

                Aligned _ y ->
                    { gathered
                        | attributes = classNameAttr (alignXName x) :: gathered.attributes
                        , alignment = Aligned (Just x) y
                    }

        AlignY y ->
            case gathered.alignment of
                Unaligned ->
                    { gathered
                        | attributes = classNameAttr (alignYName y) :: gathered.attributes
                        , alignment = Aligned Nothing (Just y)
                    }

                Aligned _ (Just _) ->
                    gathered

                Aligned x _ ->
                    { gathered
                        | attributes = classNameAttr (alignYName y) :: gathered.attributes
                        , alignment = Aligned x (Just y)
                    }

        Filter cssFilters ->
            case gathered.filters of
                Nothing ->
                    { gathered | filters = Just (filterName cssFilters) }

                Just existing ->
                    { gathered | filters = Just (filterName cssFilters ++ " " ++ existing) }

        BoxShadow shadow ->
            case gathered.boxShadows of
                Nothing ->
                    { gathered | boxShadows = Just ( boxShadowName shadow, formatBoxShadow shadow ) }

                Just ( existingClass, existing ) ->
                    { gathered | boxShadows = Just ( boxShadowName shadow ++ "-" ++ existingClass, formatBoxShadow shadow ++ ", " ++ existing ) }

        TextShadow shadow ->
            case gathered.textShadows of
                Nothing ->
                    { gathered | textShadows = Just ( textShadowName shadow, formatTextShadow shadow ) }

                Just ( existingClass, existing ) ->
                    { gathered | textShadows = Just ( textShadowName shadow ++ "-" ++ existingClass, formatTextShadow shadow ++ ", " ++ existing ) }


floorAtZero : Int -> Int
floorAtZero x =
    if x > 0 then
        x
    else
        0


initGathered : Maybe String -> Gathered msg
initGathered maybeNodeName =
    { attributes = []
    , styles = []
    , width = Nothing
    , height = Nothing
    , alignment = Unaligned
    , node =
        case maybeNodeName of
            Nothing ->
                Generic

            Just name ->
                NodeName name
    , nearbys = Nothing
    , transform = Nothing
    , filters = Nothing
    , boxShadows = Nothing
    , textShadows = Nothing
    , has = Flag.none
    }


{-| Because of how it's constructed, we know that NearbyGroup is nonempty
-}
renderNearbyGroupAbsolute : List ( Location, Element msg ) -> List (VirtualDom.Node msg)
renderNearbyGroupAbsolute nearbys =
    let
        create ( location, elem ) =
            VirtualDom.node "div"
                [ vDomClass <|
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
    List.map create nearbys


{-| -}
uncapitalize : String -> String
uncapitalize str =
    let
        head =
            String.left 1 str
                |> String.toLower

        tail =
            String.dropLeft 1 str
    in
    head ++ tail


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
                            , ".transform-" ++ name
                            )

                        Just pseudo ->
                            case pseudo of
                                Hover ->
                                    ( "transform-" ++ name ++ "-hover"
                                    , ".transform-" ++ name ++ "-hover:hover"
                                    )

                                Focus ->
                                    ( "transform-" ++ name ++ "-focus"
                                    , ".transform-" ++ name ++ "-focus:focus, .se:focus ~ .transform-" ++ name ++ "-focus"
                                    )

                                Active ->
                                    ( "transform-" ++ name ++ "-active"
                                    , ".transform-" ++ name ++ "-active:active"
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

        -- addFilters ( classes, styles ) =
        --     case gathered.filters of
        --         Nothing ->
        --             ( classes, styles )
        --         Just filt ->
        --             let
        --                 name =
        --                     "filter-" ++ className filt
        --             in
        --             ( name :: classes
        --             , Single ("." ++ name) "filter" filt
        --                 :: styles
        --             )
        addBoxShadows ( classes, styles ) =
            case gathered.boxShadows of
                Nothing ->
                    ( classes, styles )

                Just ( shadowClass, shades ) ->
                    ( shadowClass :: classes
                    , Single ("." ++ shadowClass) "box-shadow" shades
                        :: styles
                    )

        addTextShadows ( classes, styles ) =
            case gathered.textShadows of
                Nothing ->
                    ( classes, styles )

                Just ( shadowClass, shades ) ->
                    ( shadowClass :: classes
                    , Single ("." ++ shadowClass) "text-shadow" shades
                        :: styles
                    )

        ( newClasses, newStyles ) =
            ( [], gathered.styles )
                -- |> addFilters
                |> addBoxShadows
                |> addTextShadows
                |> addTransform
    in
    { gathered
        | styles = newStyles
        , attributes =
            vDomClass (String.join " " newClasses) :: gathered.attributes
    }


type EmbedStyle
    = NoStyleSheet
    | StaticRootAndDynamic OptionRecord
    | OnlyDynamic OptionRecord


noStyleSheet : EmbedStyle
noStyleSheet =
    NoStyleSheet


contextClasses : LayoutContext -> Attribute aligned msg
contextClasses context =
    case context of
        AsRow ->
            htmlClass (classes.any ++ " " ++ classes.row)

        AsColumn ->
            htmlClass (classes.any ++ " " ++ classes.column)

        AsEl ->
            htmlClass (classes.any ++ " " ++ classes.single)

        AsGrid ->
            htmlClass (classes.any ++ " " ++ classes.grid)

        AsParagraph ->
            htmlClass (classes.any ++ " " ++ classes.paragraph)

        AsTextColumn ->
            htmlClass (classes.any ++ " " ++ classes.page)


element : EmbedStyle -> LayoutContext -> Maybe String -> List (Attribute aligned msg) -> Children (Element msg) -> Element msg
element embedMode context node attributes children =
    (contextClasses context :: attributes)
        |> List.foldr gatherAttributes (initGathered node)
        |> finalize
        |> asElement embedMode children context


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
                    ( html context :: htmls
                    , existingStyles
                    )

                Styled styled ->
                    ( styled.html Nothing context :: htmls
                    , styled.styles ++ existingStyles
                    )

                Text str ->
                    -- TEXT OPTIMIZATION
                    -- You can have raw text if the element is an el, and has `width-content` and `height-content`
                    -- Same if it's a column or row with one child and width-content, height-content
                    -- interferes with css grid
                    -- if rendered.width == Just Content && rendered.height == Just Content && context == asEl then
                    --     ( VirtualDom.text str
                    --         :: htmls
                    --     , existingStyles
                    --     ) else
                    if context == asEl then
                        ( textElementFill str
                            :: htmls
                        , existingStyles
                        )
                    else
                        ( textElement str
                            :: htmls
                        , existingStyles
                        )

                Empty ->
                    ( htmls, existingStyles )

        gatherKeyed ( key, child ) ( htmls, existingStyles ) =
            case child of
                Unstyled html ->
                    ( ( key, html context ) :: htmls
                    , existingStyles
                    )

                Styled styled ->
                    ( ( key, styled.html Nothing context ) :: htmls
                    , styled.styles ++ existingStyles
                    )

                Text str ->
                    -- TEXT OPTIMIZATION
                    -- You can have raw text if the element is an el, and has `width-content` and `height-content`
                    -- Same if it's a column or row with one child and width-content, height-content
                    if rendered.width == Just Content && rendered.height == Just Content && context == asEl then
                        ( ( key, VirtualDom.text str )
                            :: htmls
                        , existingStyles
                        )
                    else
                        ( ( key, textElement str )
                            :: htmls
                        , existingStyles
                        )

                Empty ->
                    ( htmls, existingStyles )
    in
    case embedMode of
        NoStyleSheet ->
            let
                renderedChildren =
                    case Maybe.map renderNearbyGroupAbsolute rendered.nearbys of
                        Nothing ->
                            htmlChildren

                        Just nearby ->
                            case htmlChildren of
                                Keyed keyed ->
                                    Keyed <| keyed ++ List.map (\x -> ( "nearby-elements-pls", x )) nearby

                                Unkeyed unkeyed ->
                                    Unkeyed (unkeyed ++ nearby)
            in
            case styleChildren of
                [] ->
                    Unstyled (renderNode rendered renderedChildren Nothing)

                _ ->
                    Styled
                        { styles = styleChildren
                        , html = renderNode rendered renderedChildren
                        }

        StaticRootAndDynamic options ->
            let
                styles =
                    styleChildren
                        |> List.foldr reduceStyles ( Set.empty, [ renderFocusStyle options.focus ] )
                        |> Tuple.second

                renderedChildren =
                    case Maybe.map renderNearbyGroupAbsolute rendered.nearbys of
                        Nothing ->
                            case htmlChildren of
                                Keyed keyed ->
                                    Keyed <|
                                        ( "static-stylesheet", VirtualDom.node "style" [] [ VirtualDom.text Internal.Style.rules ] )
                                            :: ( "dynamic-stylesheet", toStyleSheet options styles )
                                            :: keyed

                                Unkeyed unkeyed ->
                                    Unkeyed
                                        (VirtualDom.node "style" [] [ VirtualDom.text Internal.Style.rules ]
                                            :: toStyleSheet options styles
                                            :: unkeyed
                                        )

                        Just nearby ->
                            case htmlChildren of
                                Keyed keyed ->
                                    Keyed <|
                                        ( "static-stylesheet", VirtualDom.node "style" [] [ VirtualDom.text Internal.Style.rules ] )
                                            :: ( "dynamic-stylesheet", toStyleSheet options styles )
                                            :: keyed
                                            ++ List.map (\x -> ( "nearby-elements-pls", x )) nearby

                                Unkeyed unkeyed ->
                                    Unkeyed
                                        (VirtualDom.node "style" [] [ VirtualDom.text Internal.Style.rules ]
                                            :: toStyleSheet options styles
                                            :: unkeyed
                                            ++ nearby
                                        )
            in
            Unstyled
                (renderNode rendered
                    renderedChildren
                    Nothing
                )

        OnlyDynamic options ->
            let
                styles =
                    styleChildren
                        |> List.foldr reduceStyles ( Set.empty, [ renderFocusStyle options.focus ] )
                        |> Tuple.second

                renderedChildren =
                    case Maybe.map renderNearbyGroupAbsolute rendered.nearbys of
                        Nothing ->
                            case htmlChildren of
                                Keyed keyed ->
                                    Keyed <|
                                        ( "dynamic-stylesheet", toStyleSheet options styles )
                                            :: keyed

                                Unkeyed unkeyed ->
                                    Unkeyed
                                        (toStyleSheet options styles
                                            :: unkeyed
                                        )

                        Just nearby ->
                            case htmlChildren of
                                Keyed keyed ->
                                    Keyed <|
                                        ( "dynamic-stylesheet", toStyleSheet options styles )
                                            :: keyed
                                            ++ List.map (\x -> ( "nearby-elements-pls", x )) nearby

                                Unkeyed unkeyed ->
                                    Unkeyed
                                        (toStyleSheet options styles
                                            :: unkeyed
                                            ++ nearby
                                        )
            in
            Unstyled
                (renderNode rendered
                    renderedChildren
                    Nothing
                )


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

                    Filter _ ->
                        ( x :: found, has )

                    BoxShadow shadow ->
                        ( x :: found, has )

                    TextShadow shadow ->
                        ( x :: found, has )
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
                            StyleClass _ (SpacingStyle x y) ->
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
                            StyleClass _ (SpacingStyle x y) ->
                                Just ( x, y )

                            _ ->
                                Nothing
            )
            Nothing
        |> Maybe.withDefault default
        |> (\( x, y ) -> StyleClass Flag.spacing (SpacingStyle x y))


textElement : String -> VirtualDom.Node msg
textElement str =
    VirtualDom.node "div"
        [ VirtualDom.property "className"
            (Json.string
                (String.join " "
                    [ classes.any
                    , classes.text
                    , classes.widthContent
                    , classes.heightContent
                    ]
                )
            )
        ]
        [ VirtualDom.text str ]


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
                        |> List.foldr reduceStyles ( Set.empty, [ renderFocusStyle options.focus ] )
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
    element embedStyle asEl Nothing attributes (Unkeyed [ child ])
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



-- renderStyles : Element msg -> ( Set String, List Style ) -> ( Set String, List Style )
-- renderStyles el ( cache, existing ) =
--     case el of
--         Unstyled html ->
--             ( cache, existing )
--         Styled styled ->
--             let
--                 reduced =
--                     List.foldr reduceStyles ( cache, existing ) styled.styles
--             in
--             List.foldr renderStyles reduced styled.children


renderFocusStyle :
    FocusStyle
    -> Style
renderFocusStyle focus =
    Style ".se:focus .focusable, .se.focusable:focus"
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
                    if opacity <= 0 then
                        renderStyle force
                            maybePseudo
                            ("." ++ name)
                            [ Property "opacity" "0"
                            , Property "pointer-events" "none"
                            ]
                    else
                        renderStyle force
                            maybePseudo
                            ("." ++ name)
                            [ Property "opacity" (String.fromFloat opacity)
                            , Property "pointer-events" "auto"
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
                        class
                        [ Property prop val
                        ]

                Colored class prop color ->
                    renderStyle force
                        maybePseudo
                        class
                        [ Property prop (formatColor color)
                        ]

                SpacingStyle x y ->
                    let
                        class =
                            ".spacing-" ++ String.fromInt x ++ "-" ++ String.fromInt y

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

                PaddingStyle top right bottom left ->
                    let
                        class =
                            ".pad-"
                                ++ String.fromInt top
                                ++ "-"
                                ++ String.fromInt right
                                ++ "-"
                                ++ String.fromInt bottom
                                ++ "-"
                                ++ String.fromInt left
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
        , formatColor shadow.color
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


filterName : FilterType -> String
filterName filtr =
    case filtr of
        FilterUrl url ->
            "url(" ++ url ++ ")"

        Blur x ->
            "blur(" ++ String.fromFloat x ++ "px)"

        Brightness x ->
            "brightness(" ++ String.fromFloat x ++ "%)"

        Contrast x ->
            "contrast(" ++ String.fromFloat x ++ "%)"

        Grayscale x ->
            "grayscale(" ++ String.fromFloat x ++ "%)"

        HueRotate x ->
            "hueRotate(" ++ String.fromFloat x ++ "deg)"

        Invert x ->
            "invert(" ++ String.fromFloat x ++ "%)"

        OpacityFilter x ->
            "opacity(" ++ String.fromFloat x ++ "%)"

        Saturate x ->
            "saturate(" ++ String.fromFloat x ++ "%)"

        Sepia x ->
            "sepia(" ++ String.fromFloat x ++ "%)"

        DropShadow shadow ->
            let
                shadowModel =
                    { offset = shadow.offset
                    , size = shadow.size
                    , blur = shadow.blur
                    , color = shadow.color
                    }
            in
            "drop-shadow(" ++ formatDropShadow shadowModel ++ ")"


floatClass : Float -> String
floatClass x =
    String.fromInt (round (x * 100))


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


psuedoClassName : PseudoClass -> String
psuedoClassName class =
    case class of
        Focus ->
            "focus"

        Hover ->
            "hover"

        Active ->
            "active"


{-| This is a key to know which styles should override which other styles.
-}
styleKey : Style -> String
styleKey s =
    case s of
        Shadows _ _ ->
            "shadows"

        Transparency name x ->
            "transparency"

        Style class _ ->
            class

        FontSize i ->
            "fontsize"

        FontFamily _ _ ->
            "fontfamily"

        Single _ prop _ ->
            prop

        Colored _ prop _ ->
            prop

        SpacingStyle _ _ ->
            "spacing"

        PaddingStyle _ _ _ _ ->
            "padding"

        GridTemplateStyle _ ->
            "grid-template"

        GridPosition _ ->
            "grid-position"

        PseudoSelector class style ->
            psuedoClassName class ++ (String.concat <| List.map styleKey style)

        Transform _ ->
            "transform"


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

        SpacingStyle x y ->
            "spacing-" ++ String.fromInt x ++ "-" ++ String.fromInt y

        PaddingStyle top right bottom left ->
            "pad-"
                ++ String.fromInt top
                ++ "-"
                ++ String.fromInt right
                ++ "-"
                ++ String.fromInt bottom
                ++ "-"
                ++ String.fromInt left

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
            "grid-pos-"
                ++ String.fromInt pos.row
                ++ "-"
                ++ String.fromInt pos.col
                ++ "-"
                ++ String.fromInt pos.width
                ++ "-"
                ++ String.fromInt pos.height

        PseudoSelector selector subStyle ->
            psuedoClassName selector
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

        TextShadow shadow ->
            TextShadow shadow

        BoxShadow shadow ->
            BoxShadow shadow

        Filter filt ->
            Filter filt


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

        TextShadow shadow ->
            TextShadow shadow

        BoxShadow shadow ->
            BoxShadow shadow

        Filter filt ->
            Filter filt


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

        TextShadow shadow ->
            let
                stringName =
                    formatTextShadow shadow
            in
            Just <| Shadows ("txt-shadow-" ++ textShadowName shadow) stringName

        BoxShadow shadow ->
            let
                stringName =
                    formatBoxShadow shadow
            in
            Just <| Shadows ("box-shadow-" ++ boxShadowName shadow) stringName

        _ ->
            Nothing
