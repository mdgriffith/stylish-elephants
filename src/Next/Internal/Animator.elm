module Next.Internal.Animator exposing (..)

{-| -}

import Color exposing (Color)
import Time exposing (Time)


type Model checkpoint
    = Model
        { passed : List ( checkpoint, Time )
        , current : Time
        , upcoming : List ( checkpoint, Time )
        , timeline : Timeline checkpoint
        }


type Timeline checkpoint
    = Timeline (List (Checkpoint checkpoint))


type Checkpoint id
    = Checkpoint Time id
    | After Time id (Checkpoint id)
    | Before Time id (Checkpoint id)


timeline =
    Timeline


type Execution checkpoint
    = AtRest (Timeline checkpoint)
    | Executing (Model checkpoint)


executionState (Model model) =
    if model.upcoming == [] then
        AtRest model.timeline
    else
        Executing (Model model)


{-|

    Capture current model state ->
        AtRest (List checkpoint)
        Moving passed: (Time, ), current, upcoming: List (checkpoint, Time)

    Previous times for a checkpoint can be purged if they are duplicated
-}
start : Timeline checkpoint -> Model checkpoint -> Model checkpoint
start timeline model =
    model


scale : Float -> Timeline checkpoint -> Model checkpoint -> Model checkpoint
scale scaling timeline model =
    model


go time checkpoint model =
    start (Timeline [ Checkpoint time checkpoint ]) model


checkpoint =
    Checkpoint


after =
    After


before =
    Before



--opacity


type Animated thing checkpoint
    = Animated
        { base : thing
        , checkpoints : List ( checkpoint, thing )
        }


mostRecent events relevant =
    let
        find ( checkpoint, occurredAt ) found =
            case found of
                Nothing ->
                    let
                        findValue ( check, value ) alreadyFound =
                            case alreadyFound of
                                Nothing ->
                                    if check == checkpoint then
                                        Just value
                                    else
                                        Nothing

                                _ ->
                                    alreadyFound

                        foundValue =
                            List.foldl findValue Nothing relevant
                    in
                        Maybe.map (\value -> ( value, occurredAt )) foundValue

                Just x ->
                    found
    in
        List.foldl find Nothing events


{-| Starting Point ->
Find most recently passed, relevant checkpoint.
-> If failing, set as base.

Target ->
Find closest upcoming checkpoint
-> If there is none, maintain current value (value of last checkpoint)
-> Otherwise interpolate between starting point and upcoming checkpoint

-}
float : Model checkpoint -> Float -> List ( checkpoint, Float ) -> Float
float (Model model) base checkpoints =
    let
        ( start, startTime ) =
            mostRecent model.passed checkpoints
                |> Maybe.withDefault ( base, model.current )

        ( target, targetTime ) =
            mostRecent model.upcoming checkpoints
                |> Maybe.withDefault ( start, model.current )
    in
        linear model.current ( startTime, targetTime ) ( start, target )


{-| Starting Point ->
Find most recently passed, relevant checkpoint.
-> If failing, set as base.

Target ->
Find closest upcoming checkpoint
-> If there is none, maintain current value (value of last checkpoint)
-> Otherwise interpolate between starting point and upcoming checkpoint

-}
color : Model checkpoint -> Color -> List ( checkpoint, Color ) -> Color
color (Model model) base checkpoints =
    let
        ( start, startTime ) =
            mostRecent model.passed checkpoints
                |> Maybe.withDefault ( base, model.current )

        ( target, targetTime ) =
            mostRecent model.upcoming checkpoints
                |> Maybe.withDefault ( start, model.current )
    in
        linearColor model.current ( startTime, targetTime ) ( start, target )


linearColor : Float -> ( Float, Float ) -> ( Color, Color ) -> Color
linearColor a ( aMin, aMax ) ( colorMin, colorMax ) =
    let
        rgbMin =
            Color.toRgb colorMin

        rgbMax =
            Color.toRgb colorMax
    in
        Color.rgba
            (linearInt a ( aMin, aMax ) ( rgbMin.red, rgbMax.red ))
            (linearInt a ( aMin, aMax ) ( rgbMin.blue, rgbMax.blue ))
            (linearInt a ( aMin, aMax ) ( rgbMin.green, rgbMax.green ))
            (linear a ( aMin, aMax ) ( rgbMin.alpha, rgbMax.alpha ))


{-| -}
linearInt : Float -> ( Float, Float ) -> ( Int, Int ) -> Int
linearInt a ( aMin, aMax ) ( bMin, bMax ) =
    if a <= aMin then
        bMin
    else if a >= aMax then
        bMax
    else
        let
            deltaA =
                (a - aMin) / (aMax - aMin)
        in
            round <| (deltaA * (toFloat (bMax - bMin))) + toFloat bMin


{-| -}
linear : Float -> ( Float, Float ) -> ( Float, Float ) -> Float
linear a ( aMin, aMax ) ( bMin, bMax ) =
    if a <= aMin then
        bMin
    else if a >= aMax then
        bMax
    else
        let
            deltaA =
                (a - aMin) / (aMax - aMin)
        in
            (deltaA * (bMax - bMin)) + bMin



--translation : Float -> List ( checkpoint, Float ) -> Animated Float checkpoint
--translation initial checkpoints =
--    Animated
--        { base = initial
--        , checkpoints = checkpoints
--        }
