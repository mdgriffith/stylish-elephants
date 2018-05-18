module Main exposing (..)


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
