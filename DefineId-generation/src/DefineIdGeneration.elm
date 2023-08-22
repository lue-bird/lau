module DefineIdGeneration exposing (main)

{-| ui to generate id code
-}

import Array exposing (Array)
import ArraySized exposing (ArraySized)
import Bit exposing (Bit(..))
import Browser
import Element as Ui
import Element.Background as Background
import Element.Border as UiBorder
import Element.Font as Font
import Element.Input as UiInput
import Html exposing (Html)
import Lau.DefineId exposing (DefineId)
import N.Local exposing (n160)
import Random
import Typed exposing (tag)


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                -- TODO more randomness
                ( initialModel
                , Lau.DefineId.random |> Random.generate IdGenerated
                )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = GenerateId
    | IdGenerated DefineId


type alias Model =
    { defineId : DefineId }


initialModel : Model
initialModel =
    { defineId = ArraySized.repeat Bit.O n160 |> ArraySized.inToNumber |> tag Lau.DefineId.Tag }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateId ->
            -- TODO more randomness
            ( model, Lau.DefineId.random |> Random.generate IdGenerated )

        IdGenerated id ->
            ( { model | defineId = id }
            , Cmd.none
            )


view : Model -> Html Msg
view state =
    Ui.layout []
        (Ui.row
            [ Ui.width Ui.fill
            , Ui.height Ui.fill
            , Background.color (Ui.rgb 0 0 0)
            , Ui.spacing 30
            , Ui.padding 20
            ]
            [ idAndGenerateNewButtonUi state.defineId
            , Ui.none
            ]
        )


idAndGenerateNewButtonUi : DefineId -> Ui.Element Msg
idAndGenerateNewButtonUi defineId =
    Ui.column
        [ Ui.height Ui.fill
        , Ui.width Ui.fill
        , Ui.spacing 30
        , Ui.paddingXY 40 70
        ]
        [ Ui.el
            [ Font.size 24
            , Font.family [ Font.monospace ]
            , Font.color (Ui.rgb 1 1 1)
            , Background.color (Ui.rgba 0 0 0 0)
            ]
            (Ui.paragraph []
                [ Ui.text
                    ("Lau.DefineId.raw "
                        ++ (defineId
                                |> Typed.untag
                                |> ArraySized.map (\bit -> bit |> bitToChar |> String.fromChar)
                                |> ArraySized.toList
                                |> String.join " "
                           )
                    )
                ]
            )
        , Ui.el
            [ Font.color (Ui.rgb 1 1 1) ]
            (Ui.text "triple click on the text above, then Ctrl + c")
        , UiInput.button
            [ Font.color (Ui.rgb 1 1 1)
            , Font.size 30
            ]
            { label =
                Ui.column [ Ui.spacing 4 ]
                    [ Ui.text "generate new id"
                    , Ui.text "\u{1FBB0}"
                        |> Ui.el
                            [ Font.color (Ui.rgb 0 0.5 0.8)
                            , Font.size 22
                            , Ui.centerX
                            ]
                    ]
            , onPress = Just GenerateId
            }
        ]


bitToChar : Bit -> Char
bitToChar bit =
    case bit of
        O ->
            'O'

        I ->
            'I'
