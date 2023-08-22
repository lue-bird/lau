module DefineIdRawDeclarationGeneration exposing (main)

import Browser
import Element as Ui
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)


main : Program () {} Never
main =
    Browser.element
        { view = \_ -> view
        , update = \_ _ -> ( {}, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , init = \_ -> ( {}, Cmd.none )
        }


view : Html msg
view =
    Ui.text
        (String.concat
            [ "id : "
            , List.repeat 160 "Bit" |> String.join " -> "
            , " -> Id tag"
            , "\n"
            , "id "
            , List.range 0 159
                |> List.map (\i -> "b" ++ String.fromInt i)
                |> String.join " "
            , " ="
            , "\n"
            , List.range 0 9
                |> List.foldl
                    (\extend soFar ->
                        soFar
                            ++ "\n        |> InArr.extend nat16 (Arr.from16 "
                            ++ (List.range (extend * 16) (extend * 16 + 15)
                                    |> List.map (\i -> "b" ++ String.fromInt i)
                                    |> String.join " "
                               )
                            ++ ")"
                    )
                    "    Arr.empty"
            , "\n        |> tag"
            ]
        )
        |> Ui.el
            [ Ui.width Ui.fill
            , Ui.height Ui.fill
            , Font.color (Ui.rgb 1 1 1)
            , Background.color (Ui.rgb 0 0 0)
            , Font.family [ Font.monospace ]
            ]
        |> Ui.layout []
