module Element.Border
    exposing
        ( color
        , dashed
        , dotted
        , glow
        , innerGlow
        , innerShadow
        , roundEach
        , rounded
        , shadow
        , solid
        , width
        , widthEach
        , widthXY
        )

{-| Border Properties

@docs color


# Border Widths

@docs width, widthXY, widthEach


# Border Styles

@docs solid, dashed, dotted


# Rounded Corners

@docs rounded, roundEach


# Shadows

@docs glow, innerGlow, shadow, innerShadow

-}

import Element exposing (Attr, Attribute, Color)
import Internal.Model as Internal
import Internal.Style as Style exposing (classes)


{-| -}
color : Color -> Attr decorative msg
color clr =
    Internal.StyleClass (Internal.Colored ("border-color-" ++ Internal.formatColorClass clr) "border-color" clr)


{-| -}
width : Int -> Attribute msg
width v =
    Internal.StyleClass (Internal.Single ("border-" ++ String.fromInt v) "border-width" (String.fromInt v ++ "px"))


{-| Set horizontal and vertical borders.
-}
widthXY : Int -> Int -> Attribute msg
widthXY x y =
    Internal.StyleClass (Internal.Single ("border-" ++ String.fromInt x ++ "-" ++ String.fromInt y) "border-width" (String.fromInt y ++ "px " ++ String.fromInt x ++ "px"))


{-| -}
widthEach : { bottom : Int, left : Int, right : Int, top : Int } -> Attribute msg
widthEach { bottom, top, left, right } =
    Internal.StyleClass
        (Internal.Single ("border-" ++ String.fromInt top ++ "-" ++ String.fromInt right ++ String.fromInt bottom ++ "-" ++ String.fromInt left)
            "border-width"
            (String.fromInt top
                ++ "px "
                ++ String.fromInt right
                ++ "px "
                ++ String.fromInt bottom
                ++ "px "
                ++ String.fromInt left
                ++ "px"
            )
        )



-- {-| No Borders
-- -}
-- none : Attribute msg
-- none =
--     Class "border" "border-none"


{-| -}
solid : Attribute msg
solid =
    Internal.Class "border" classes.borderSolid


{-| -}
dashed : Attribute msg
dashed =
    Internal.Class "border" classes.borderDashed


{-| -}
dotted : Attribute msg
dotted =
    Internal.Class "border" classes.borderDotted


{-| Round all corners.
-}
rounded : Int -> Attribute msg
rounded radius =
    Internal.StyleClass (Internal.Single ("border-radius-" ++ String.fromInt radius) "border-radius" (String.fromInt radius ++ "px"))


{-| -}
roundEach : { topLeft : Int, topRight : Int, bottomLeft : Int, bottomRight : Int } -> Attribute msg
roundEach { topLeft, topRight, bottomLeft, bottomRight } =
    Internal.StyleClass
        (Internal.Single ("border-radius-" ++ String.fromInt topLeft ++ "-" ++ String.fromInt topRight ++ String.fromInt bottomLeft ++ "-" ++ String.fromInt bottomRight)
            "border-radius"
            (String.fromInt topLeft
                ++ "px "
                ++ String.fromInt topRight
                ++ "px "
                ++ String.fromInt bottomRight
                ++ "px "
                ++ String.fromInt bottomLeft
                ++ "px"
            )
        )


{-| A simple glow by specifying the color and size.
-}
glow : Color -> Float -> Attr decorative msg
glow clr size =
    box
        { offset = ( 0, 0 )
        , size = size
        , blur = size * 2
        , color = clr
        }


{-| -}
innerGlow : Color -> Float -> Attr decorative msg
innerGlow clr size =
    innerShadow
        { offset = ( 0, 0 )
        , size = size
        , blur = size * 2
        , color = clr
        }


{-| -}
box :
    { offset : ( Float, Float )
    , size : Float
    , blur : Float
    , color : Color
    }
    -> Attr decorative msg
box shade =
    Internal.BoxShadow
        { inset = False
        , offset = shade.offset
        , size = shade.size
        , blur = shade.blur
        , color = shade.color
        }


{-| -}
innerShadow :
    { offset : ( Float, Float )
    , size : Float
    , blur : Float
    , color : Color
    }
    -> Attr decorative msg
innerShadow shade =
    Internal.BoxShadow
        { inset = True
        , offset = shade.offset
        , size = shade.size
        , blur = shade.blur
        , color = shade.color
        }


{-| -}
shadow :
    { offset : ( Float, Float )
    , blur : Float
    , size : Float
    , color : Color
    }
    -> Attr decorative msg
shadow shade =
    Internal.BoxShadow
        { inset = False
        , offset = shade.offset
        , size = shade.size
        , blur = shade.blur
        , color = shade.color
        }
