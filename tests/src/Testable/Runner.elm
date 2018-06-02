port module Testable.Runner exposing (TestableProgram, program, show)

{-| -}

import Browser
import Dict exposing (Dict)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Process
import Random
import Task
import Test.Runner
import Test.Runner.Failure as Failure
import Testable
import Time


show : Testable.Element msg -> Html msg
show =
    Testable.render


type alias TestableProgram =
    Program () (Model Msg) Msg


program : List ( String, Testable.Element Msg ) -> TestableProgram
program tests =
    let
        ( current, upcoming ) =
            case tests of
                [] ->
                    ( Nothing, [] )

                cur :: remaining ->
                    ( Just cur, remaining )
    in
    Browser.embed
        { init =
            always
                ( { current = current
                  , upcoming = upcoming
                  , finished = []
                  , stage = BeginRendering
                  }
                , Task.perform (always Analyze)
                    (Process.sleep 32
                        |> Task.andThen
                            (always Time.now)
                    )
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : { a | stage : Stage } -> Sub Msg
subscriptions model =
    Sub.batch
        [ styles RefreshBoundingBox
        ]


type alias Model msg =
    { current : Maybe ( String, Testable.Element msg )
    , upcoming : List ( String, Testable.Element msg )
    , finished : List (WithResults (Testable.Element msg))
    , stage : Stage
    }


type alias WithResults thing =
    { element : thing
    , label : String
    , results :
        List
            ( String
            , Maybe
                { given : Maybe String
                , description : String
                , reason : Failure.Reason
                }
            )
    }


prepareResults :
    List (WithResults (Testable.Element msg))
    ->
        List
            { label : String
            , results :
                List
                    ( String
                    , Maybe
                        { given : Maybe String
                        , description : String
                        }
                    )
            }
prepareResults withResults =
    let
        prepareNode ( x, maybeResult ) =
            ( x
            , case maybeResult of
                Nothing ->
                    Nothing

                Just res ->
                    Just
                        { given = res.given
                        , description = res.description
                        }
            )

        prepare { label, results } =
            { label = label
            , results = List.map prepareNode results
            }
    in
    List.map prepare withResults


type Stage
    = Rendered
    | BeginRendering


type Msg
    = NoOp
    | Analyze
    | RefreshBoundingBox
        (List
            { id : String
            , bbox : Testable.BoundingBox
            , style : List ( String, String )
            }
        )


runTest : Dict String Testable.Found -> String -> Testable.Element msg -> WithResults (Testable.Element msg)
runTest boxes label element =
    let
        tests =
            Testable.toTest label boxes element

        seed =
            Random.initialSeed 227852860

        results =
            Testable.runTests seed tests
    in
    { element = element
    , label = label
    , results = results
    }


update : Msg -> Model Msg -> ( Model Msg, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RefreshBoundingBox boxes ->
            let
                _ =
                    Debug.log "refreshed" model.stage
            in
            case model.current of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just ( label, current ) ->
                    let
                        toTuple box =
                            ( box.id, { style = Dict.fromList box.style, bbox = box.bbox } )

                        foundData =
                            boxes
                                |> List.map toTuple
                                |> Dict.fromList

                        currentResults =
                            runTest foundData label current
                    in
                    case model.upcoming of
                        [] ->
                            ( { model
                                | current = Nothing
                                , finished = model.finished ++ [ currentResults ]
                              }
                            , report (prepareResults (currentResults :: model.finished))
                            )

                        newCurrent :: remaining ->
                            ( { model
                                | finished = model.finished ++ [ currentResults ]
                                , current = Just newCurrent
                                , upcoming = remaining
                                , stage = BeginRendering
                              }
                            , Task.perform (always Analyze)
                                (Process.sleep 32
                                    |> Task.andThen
                                        (always Time.now)
                                )
                            )

        Analyze ->
            case model.current of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just ( label, current ) ->
                    ( model
                    , let
                        ids =
                            Debug.log "sending cmd analyze" (Testable.getIds current)
                      in
                      analyze ids
                    )


view : Model Msg -> Html Msg
view model =
    case model.current of
        Nothing ->
            if List.isEmpty model.upcoming then
                Element.layout [] <|
                    Element.column
                        [ Element.spacing 20
                        , Element.padding 20
                        , Element.width (Element.px 800)

                        -- , Background.color Color.grey
                        ]
                        (List.map viewResult model.finished)
            else
                Html.text "running?"

        Just ( label, current ) ->
            Testable.render current


palette =
    { white = Element.rgb 1 1 1
    , red = Element.rgb 1 0 0
    , green = Element.rgb 0 1 0
    , lightGrey = Element.rgb 0.7 0.7 0.7
    }


viewResult : WithResults (Testable.Element Msg) -> Element.Element Msg
viewResult testable =
    let
        isPassing result =
            case Tuple.second result of
                Nothing ->
                    True

                Just _ ->
                    False

        ( passing, failing ) =
            List.partition isPassing testable.results

        viewSingle result =
            case result of
                ( label, Nothing ) ->
                    Element.el
                        [ Background.color palette.green
                        , Font.color palette.white
                        , Element.paddingXY 20 10
                        , Element.alignLeft
                        , Border.rounded 3
                        ]
                    <|
                        Element.text ("Success! - " ++ label)

                ( label, Just ({ given, description } as reason) ) ->
                    Element.column
                        [ Background.color palette.red
                        , Font.color palette.white
                        , Element.paddingXY 20 10
                        , Element.alignLeft
                        , Element.width Element.shrink

                        -- , Element.spacing 25
                        , Border.rounded 3
                        ]
                        [ Element.el [ Element.width Element.fill ] <| Element.text label
                        , Element.el [ Element.width Element.fill ] <| Element.text (viewReason reason)
                        ]
    in
    Element.column
        [ Border.width 1
        , Border.color palette.lightGrey
        , Element.padding 20
        , Element.height Element.shrink
        , Element.alignLeft
        , Element.spacing 16
        ]
        [ Element.el [ Font.bold, Font.size 64 ] (Element.text testable.label)
        , Element.column [ Element.alignLeft, Element.spacing 20 ]
            (failing
                |> List.map viewSingle
            )
        , Element.el
            [ Element.alignLeft
            , Element.spacing 20
            , Background.color palette.green
            , Font.color palette.white
            , Element.paddingXY 20 10
            , Element.alignLeft
            , Border.rounded 3
            ]
            (Element.text (String.fromInt (List.length passing) ++ " tests passing!"))
        ]


viewReason { description, reason } =
    case reason of
        Failure.Custom ->
            description

        Failure.Equality one two ->
            description ++ " " ++ one ++ " " ++ two

        Failure.Comparison one two ->
            description ++ " " ++ one ++ " " ++ two

        Failure.ListDiff expected actual ->
            "expected\n"
                ++ String.join "    \n" expected
                ++ "actual\n"
                ++ String.join "    \n" actual

        Failure.CollectionDiff { expected, actual, extra, missing } ->
            String.join "\n"
                [ formatKeyValue "expected" expected
                , formatKeyValue "actual" actual
                , formatKeyValue "extra" (String.join ", " extra)
                , formatKeyValue "missing" (String.join ", " missing)
                ]

        Failure.TODO ->
            description

        Failure.Invalid _ ->
            description


formatKeyValue : String -> String -> String
formatKeyValue key val =
    key ++ ": " ++ val


port report :
    List
        { label : String
        , results :
            List
                ( String
                , Maybe
                    { given : Maybe String
                    , description : String
                    }
                )
        }
    -> Cmd msg


port test : String -> Cmd msg


port analyze : List String -> Cmd msg


port styles : (List { id : String, bbox : Testable.BoundingBox, style : List ( String, String ) } -> msg) -> Sub msg
