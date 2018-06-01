module Main exposing (..)

import Html
import Internal.Flag as Flag


main =
    Html.div []
        [ Html.text "Verify All Flags invalidate themselves"
        , Html.div []
            (List.indexedMap invalidateSelf allFlags)
        ]


invalidateSelf i flag =
    if Flag.present flag (Flag.add flag Flag.none) then
        Html.text ""
    else
        Html.div [] [ Html.text (toString (Flag.value flag) ++ " at index " ++ toString i ++ " does not invalidate itself") ]


allFlags =
    [ Flag.transparency
    , Flag.padding
    , Flag.spacing
    , Flag.fontSize
    , Flag.fontFamily
    , Flag.width
    , Flag.height
    , Flag.bgColor
    , Flag.bgImage
    , Flag.bgGradient
    , Flag.borderStyle
    , Flag.fontAlignment
    , Flag.fontWeight
    , Flag.fontColor
    , Flag.wordSpacing
    , Flag.letterSpacing
    , Flag.borderRound
    , Flag.shadows
    , Flag.overflow
    , Flag.cursor
    , Flag.scale
    , Flag.rotate
    , Flag.moveX
    , Flag.moveY
    , Flag.borderWidth
    , Flag.borderColor
    , Flag.yAlign
    , Flag.xAlign
    , Flag.focus
    , Flag.active
    , Flag.hover
    , Flag.gridTemplate
    , Flag.gridPosition
    , Flag.heightContent
    , Flag.heightFill
    , Flag.widthContent
    , Flag.widthFill
    , Flag.alignRight
    , Flag.alignBottom
    , Flag.centerX
    , Flag.centerY
    ]
