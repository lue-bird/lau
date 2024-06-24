port module Main exposing (State, main)

import Color exposing (Color)
import FastDict
import Json.Encode
import Lau
import List.LocalExtra
import Web
import Web.Dom
import Web.Svg
import Web.Window


type alias State =
    { windowWidth : Int
    , windowHeight : Int
    , codeProject :
        { relationDefinitions :
            FastDict.Dict
                String
                { argumentVariable : String
                , equivalentFact : FactUiState
                }
        }
    }


type FactUiState
    = RelationUse { identifier : String, argument : ValueUiState }
    | Equal { a : ValueUiState, b : ValueUiState }
    | Not FactUiState
    | All (List FactUiState)
    | Any (List FactUiState)


type ValueUiState
    = ValueHole
    | Variable String
    | ValueLookup (FastDict.Dict String ValueUiState)


valueUiStateToLau : ValueUiState -> Maybe (Lau.ValueWithVariableName String)
valueUiStateToLau =
    \valueWithHoles ->
        case valueWithHoles of
            ValueHole ->
                Nothing

            Variable variableName ->
                Lau.Variable variableName |> Just

            ValueLookup entriesWithHoles ->
                Maybe.map Lau.ValueLookup
                    (entriesWithHoles
                        |> FastDict.toList
                        |> List.LocalExtra.allJustMap
                            (\( entryName, entryValueWithHoles ) ->
                                Maybe.map (\entryValue -> ( entryName, entryValue ))
                                    (entryValueWithHoles |> valueUiStateToLau)
                            )
                        |> Maybe.map FastDict.fromList
                    )


factUiStateToLau : FactUiState -> Maybe Lau.Fact
factUiStateToLau =
    \factWithHoles ->
        case factWithHoles of
            Not inverseFactWithHoles ->
                Maybe.map Lau.Not (inverseFactWithHoles |> factUiStateToLau)

            RelationUse relation ->
                Maybe.map
                    (\argument ->
                        Lau.RelationUse
                            { identifier = relation.identifier
                            , argument = argument
                            }
                    )
                    (relation.argument |> valueUiStateToLau)

            Equal toEquateWithHoles ->
                Maybe.map2 (\toEquateA toEquateB -> Lau.Equal [ toEquateA, toEquateB ])
                    (toEquateWithHoles.a |> valueUiStateToLau)
                    (toEquateWithHoles.b |> valueUiStateToLau)

            All partsWithHoles ->
                Maybe.map Lau.All
                    (partsWithHoles |> List.LocalExtra.allJustMap factUiStateToLau)

            Any branchesWithHoles ->
                Maybe.map Lau.Any
                    (branchesWithHoles |> List.LocalExtra.allJustMap factUiStateToLau)


main : Web.Program State
main =
    Web.program
        { initialState = app.initialState
        , interface = app.interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


app : { initialState : State, interface : State -> Web.Interface State }
app =
    { initialState =
        { windowWidth = 1920
        , windowHeight = 1080
        , codeProject =
            { relationDefinitions =
                FastDict.singleton "main"
                    { argumentVariable = "Result"
                    , equivalentFact =
                        Any
                            [ All
                                [ Not
                                    (All
                                        []
                                    )
                                , Equal
                                    { a = Variable "A"
                                    , b = Variable "B"
                                    }
                                ]
                            , All
                                [ RelationUse
                                    { identifier = "list"
                                    , argument = Variable "B"
                                    }
                                ]
                            , Equal
                                { a = Variable "A"
                                , b = ValueHole
                                }
                            ]
                    }
            }
        }
    , interface =
        \state ->
            [ [ Web.Window.sizeRequest, Web.Window.resizeListen ]
                |> Web.interfaceBatch
                |> Web.interfaceFutureMap
                    (\windowSize ->
                        { state
                            | windowWidth = windowSize.width
                            , windowHeight = windowSize.height
                        }
                    )
            , Web.Dom.element "div"
                [ Web.Dom.style "background-color" (Color.rgb 0 0 0 |> Color.toCssString)
                , Web.Dom.style "color" (Color.rgb 1 1 1 |> Color.toCssString)
                , Web.Dom.style "position" "fixed"
                , Web.Dom.style "top" "0"
                , Web.Dom.style "right" "0"
                , Web.Dom.style "bottom" "0"
                , Web.Dom.style "left" "0"
                ]
                [ Web.Svg.element "svg"
                    [ Web.Dom.attribute "viewBox"
                        ([ "0 0 "
                         , state.windowWidth |> String.fromInt
                         , " "
                         , state.windowWidth |> String.fromInt
                         ]
                            |> String.concat
                        )
                    , Web.Dom.attribute "width" ((state.windowWidth |> String.fromInt) ++ "px")
                    , Web.Dom.attribute "height" ((state.windowWidth |> String.fromInt) ++ "px")
                    , Web.Dom.style "display" "block"
                    , Web.Dom.style "margin" "auto"
                    , Web.Dom.style "font-family" "monospace"
                    , Web.Dom.style "font-size" (fontSize |> String.fromFloat)
                    ]
                    [ state.codeProject.relationDefinitions
                        |> FastDict.toList
                        |> List.foldl
                            (\( name, definition ) soFar ->
                                let
                                    definitionAsSvg =
                                        relationDefinitionSvg name definition
                                in
                                { height = soFar.height + 50 + definitionAsSvg.height
                                , svgsReverse =
                                    soFar.svgsReverse
                                        |> (::)
                                            (Web.Svg.element "g"
                                                [ svgAttributeTranslate { x = 0, y = soFar.height + 50 } ]
                                                [ definitionAsSvg.svg ]
                                            )
                                }
                            )
                            { height = 0
                            , svgsReverse = []
                            }
                        |> .svgsReverse
                        |> List.reverse
                        |> Web.Svg.element "g"
                            [ svgAttributeTranslate { x = 140, y = 140 } ]
                    ]
                ]
                |> Web.Dom.render
            ]
                |> Web.interfaceBatch
    }


svgAttributeTranslate : { x : Float, y : Float } -> Web.Dom.Modifier future_
svgAttributeTranslate offset =
    Web.Dom.attribute "transform"
        ("translate("
            ++ (offset.x |> String.fromFloat)
            ++ " "
            ++ (offset.y |> String.fromFloat)
            ++ ")"
        )


textSvg : String -> { height : Float, width : Float, svg : Web.Dom.Node future_ }
textSvg string =
    { height = fontSize
    , width = (string |> String.length |> Basics.toFloat) * fontWidth
    , svg =
        Web.Svg.element "text"
            [ Web.Dom.attribute "x" (0 |> String.fromFloat)
            , Web.Dom.attribute "y" (fontBaseline |> String.fromFloat)
            , svgAttributeFillUniform (Color.rgb 1 1 1)
            ]
            [ Web.Dom.text string ]
    }


relationDefinitionSvg :
    String
    ->
        { argumentVariable : String
        , equivalentFact : FactUiState
        }
    -> { svg : Web.Dom.Node future_, width : Float, height : Float }
relationDefinitionSvg name definition =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        headerWidth : Float
        headerWidth =
            strokeWidth + nameSvg.width + spaceWidth + argumentVariableSvg.width + spaceWidth + equalsTextSvg.width + strokeWidth

        fullWidth : Float
        fullWidth =
            headerWidth + equivalentFactSvg.width

        spaceWidth : Float
        spaceWidth =
            fontWidth / 2

        color : Color
        color =
            Color.rgb 0.2 0.2 0

        argumentVariableSvg : { svg : Web.Dom.Node future_, width : Float, height : Float }
        argumentVariableSvg =
            variableSvg definition.argumentVariable

        equivalentFactSvg : { width : Float, height : Float, svg : Web.Dom.Node future_ }
        equivalentFactSvg =
            definition.equivalentFact |> factSvg

        nameSvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
        nameSvg =
            textSvg name

        equalsTextSvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
        equalsTextSvg =
            textSvg "="

        headerHeight : Float
        headerHeight =
            (List.maximum
                [ nameSvg.height
                , variableSvgHeight
                , equalsTextSvg.height
                ]
                |> Maybe.withDefault 0
            )
                + strokeWidth

        fullHeight : Float
        fullHeight =
            Basics.max headerHeight equivalentFactSvg.height

        shapeSvg : { width : Float, height : Float, svg : Web.Dom.Node future_ }
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform color
                ]
                [ ( 0, 0 )
                , ( headerWidth + strokeWidth, 0 )
                , ( headerWidth, strokeWidth )
                , ( headerWidth, fullHeight - strokeWidth )
                , ( headerWidth + strokeWidth, fullHeight )
                , ( 0, fullHeight )
                ]
    in
    { height = shapeSvg.height
    , width = shapeSvg.width
    , svg =
        Web.Svg.element "g"
            []
            [ shapeSvg.svg
            , Web.Svg.element "g"
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = (variableSvgHeight + strokeWidth) / 2
                    }
                ]
                [ nameSvg.svg ]
            , Web.Svg.element "g"
                [ svgAttributeTranslate
                    { x = strokeWidth + nameSvg.width + spaceWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ argumentVariableSvg.svg ]
            , Web.Svg.element "g"
                [ svgAttributeTranslate
                    { x = strokeWidth + nameSvg.width + spaceWidth + argumentVariableSvg.width + spaceWidth
                    , y = (variableSvgHeight + strokeWidth) / 2
                    }
                ]
                [ equalsTextSvg.svg ]
            , Web.Svg.element "g"
                [ svgAttributeTranslate
                    { x = headerWidth
                    , y = 0
                    }
                ]
                [ equivalentFactSvg.svg ]
            ]
    }


factSvg : FactUiState -> { width : Float, height : Float, svg : Web.Dom.Node future_ }
factSvg =
    \fact ->
        case fact of
            All parts ->
                parts |> factAllSvg

            Any branches ->
                branches |> factAnySvg

            Not inverseFact ->
                inverseFact |> factNotSvg

            Equal toEquate ->
                factEqualsSvg toEquate

            RelationUse relationUse ->
                relationUseSvg relationUse


factAllSvg : List FactUiState -> { width : Float, height : Float, svg : Web.Dom.Node future_ }
factAllSvg =
    \parts ->
        let
            strokeWidth : Float
            strokeWidth =
                fontSize

            sideWidth : Float
            sideWidth =
                strokeWidth

            partsSvg :
                { width : Float
                , height : Float
                , svgs : List { y : Float, svg : Web.Dom.Node future_ }
                }
            partsSvg =
                case parts of
                    [] ->
                        { width = 0
                        , height = strokeWidth + strokeWidth
                        , svgs = []
                        }

                    part0 :: part1Up ->
                        (part0 :: part1Up) |> factListSvg

            fullWidth : Float
            fullWidth =
                sideWidth
                    + Basics.max partsSvg.width (strokeWidth + allStringSvg.width + strokeWidth)

            allStringSvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
            allStringSvg =
                textSvg "all"

            headerHeight : Float
            headerHeight =
                fontSize + strokeWidth

            fullHeight : Float
            fullHeight =
                headerHeight + partsSvg.height + sideWidth

            shapeSvg : { width : Float, height : Float, svg : Web.Dom.Node future_ }
            shapeSvg =
                polygonSvg
                    [ svgAttributeFillUniform (Color.rgb 0 0.14 0)
                    ]
                    ([ ( sideWidth + strokeWidth, headerHeight )
                     , ( fullWidth, headerHeight )
                     , ( fullWidth, 0 )
                     , ( strokeWidth, 0 )
                     , ( 0, strokeWidth )
                     , ( 0, headerHeight + partsSvg.height )
                     , ( strokeWidth, fullHeight )
                     , ( fullWidth, fullHeight )
                     , ( fullWidth, headerHeight + partsSvg.height )
                     , ( sideWidth + strokeWidth, headerHeight + partsSvg.height )
                     , ( sideWidth, headerHeight + partsSvg.height - strokeWidth )
                     ]
                        ++ (partsSvg.svgs
                                |> List.drop 1
                                |> List.concatMap
                                    (\partAsSvg ->
                                        [ ( sideWidth, headerHeight + partAsSvg.y - strokeWidth )
                                        , ( sideWidth + strokeWidth, headerHeight + partAsSvg.y )
                                        , ( sideWidth, headerHeight + partAsSvg.y + strokeWidth )
                                        ]
                                    )
                           )
                        ++ [ ( sideWidth, headerHeight + strokeWidth )
                           ]
                    )
        in
        { width = shapeSvg.width
        , height = shapeSvg.height
        , svg =
            Web.Svg.element "g"
                []
                [ shapeSvg.svg
                , Web.Svg.element "g"
                    [ svgAttributeTranslate
                        { x = sideWidth
                        , y = strokeWidth
                        }
                    ]
                    [ allStringSvg.svg ]
                , Web.Svg.element "g"
                    [ svgAttributeTranslate { x = sideWidth, y = headerHeight } ]
                    [ partsSvg.svgs
                        |> List.map
                            (\partAsSvg ->
                                partAsSvg.svg
                                    |> List.singleton
                                    |> Web.Svg.element "g"
                                        [ svgAttributeTranslate { x = 0, y = partAsSvg.y }
                                        ]
                            )
                        |> Web.Svg.element "g" []
                    ]
                ]
        }


factAnySvg : List FactUiState -> { width : Float, height : Float, svg : Web.Dom.Node future_ }
factAnySvg =
    \branches ->
        let
            strokeWidth : Float
            strokeWidth =
                fontSize

            sideWidth : Float
            sideWidth =
                strokeWidth

            branchesSvg :
                { width : Float
                , height : Float
                , svgs : List { y : Float, svg : Web.Dom.Node future_ }
                }
            branchesSvg =
                case branches of
                    [] ->
                        { width = 0
                        , height = strokeWidth + strokeWidth
                        , svgs = []
                        }

                    branch0 :: branch1Up ->
                        (branch0 :: branch1Up) |> factListSvg

            fullWidth : Float
            fullWidth =
                sideWidth
                    + Basics.max
                        branchesSvg.width
                        (strokeWidth + anyTextSvg.width + strokeWidth)

            anyTextSvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
            anyTextSvg =
                textSvg "any"

            headerHeight : Float
            headerHeight =
                fontSize + strokeWidth

            fullHeight : Float
            fullHeight =
                headerHeight + branchesSvg.height + sideWidth

            shapeSvg : { width : Float, height : Float, svg : Web.Dom.Node future_ }
            shapeSvg =
                polygonSvg
                    [ svgAttributeFillUniform (Color.rgb 0.2 0 0.2)
                    ]
                    ([ ( sideWidth + strokeWidth, headerHeight )
                     , ( fullWidth, headerHeight )
                     , ( fullWidth, 0 )
                     , ( sideWidth, 0 )
                     , ( 0, strokeWidth )
                     , ( 0, headerHeight + branchesSvg.height )
                     , ( sideWidth, fullHeight )
                     , ( fullWidth, fullHeight )
                     , ( fullWidth, headerHeight + branchesSvg.height )
                     , ( sideWidth + strokeWidth, headerHeight + branchesSvg.height )
                     , ( sideWidth, headerHeight + branchesSvg.height - strokeWidth )
                     ]
                        ++ (branchesSvg.svgs
                                |> List.drop 1
                                |> List.concatMap
                                    (\branchAsSvg ->
                                        [ ( sideWidth, headerHeight + branchAsSvg.y - strokeWidth )
                                        , ( sideWidth + strokeWidth, headerHeight + branchAsSvg.y )
                                        , ( sideWidth, headerHeight + branchAsSvg.y + strokeWidth )
                                        ]
                                    )
                           )
                        ++ [ ( sideWidth, headerHeight + strokeWidth )
                           ]
                    )
        in
        { width = shapeSvg.width
        , height = shapeSvg.height
        , svg =
            Web.Svg.element "g"
                []
                [ shapeSvg.svg
                , Web.Svg.element "g"
                    [ svgAttributeTranslate
                        { x = sideWidth
                        , y = (fontSize + strokeWidth) / 2
                        }
                    ]
                    [ anyTextSvg.svg ]
                , Web.Svg.element "g"
                    [ svgAttributeTranslate { x = sideWidth, y = headerHeight } ]
                    [ branchesSvg.svgs
                        |> List.map
                            (\branchAsSvg ->
                                branchAsSvg.svg
                                    |> List.singleton
                                    |> Web.Svg.element "g"
                                        [ svgAttributeTranslate { x = 0, y = branchAsSvg.y }
                                        ]
                            )
                        |> Web.Svg.element "g" []
                    ]
                ]
        }


factNotSvg : FactUiState -> { width : Float, height : Float, svg : Web.Dom.Node future_ }
factNotSvg =
    \inverseFact ->
        let
            strokeWidth : Float
            strokeWidth =
                fontSize

            sideWidth : Float
            sideWidth =
                strokeWidth

            factInverseSvg : { width : Float, height : Float, svg : Web.Dom.Node future_ }
            factInverseSvg =
                inverseFact |> factSvg

            headerWidth : Float
            headerWidth =
                strokeWidth + notTextSvg.width + strokeWidth

            fullWidth : Float
            fullWidth =
                headerWidth + factInverseSvg.width

            notTextSvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
            notTextSvg =
                textSvg "not"

            fullHeight : Float
            fullHeight =
                Basics.max (fontSize + strokeWidth)
                    factInverseSvg.height

            shapeSvg =
                polygonSvg
                    [ svgAttributeFillUniform (Color.rgb 0.2 0.04 0)
                    ]
                    [ ( 0, strokeWidth )
                    , ( strokeWidth, 0 )
                    , ( headerWidth + strokeWidth, 0 )
                    , ( headerWidth, strokeWidth )
                    , ( headerWidth, fullHeight - strokeWidth )
                    , ( headerWidth + strokeWidth, fullHeight )
                    , ( strokeWidth, fullHeight )
                    , ( 0, fullHeight - strokeWidth )
                    ]
        in
        { width = fullWidth
        , height = fullHeight
        , svg =
            Web.Svg.element "g"
                []
                [ shapeSvg.svg
                , Web.Svg.element "g"
                    [ svgAttributeTranslate
                        { x = strokeWidth
                        , y = (fontSize + strokeWidth) / 2
                        }
                    ]
                    [ notTextSvg.svg ]
                , Web.Svg.element "g"
                    [ svgAttributeTranslate { x = headerWidth, y = 0 } ]
                    [ factInverseSvg.svg ]
                ]
        }


factEqualsSvg :
    { a : ValueUiState, b : ValueUiState }
    -> { svg : Web.Dom.Node future_, width : Float, height : Float }
factEqualsSvg toEquate =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        fullWidth : Float
        fullWidth =
            strokeWidth
                + valueASvg.width
                + spaceWidth
                + equalsTextSvg.width
                + spaceWidth
                + valueBSvg.width
                + strokeWidth

        spaceWidth : Float
        spaceWidth =
            fontWidth / 2

        color : Color
        color =
            Color.rgb 0.2 0.2 0

        valueASvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
        valueASvg =
            toEquate.a |> valueSvg

        valueBSvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
        valueBSvg =
            toEquate.b |> valueSvg

        equalsTextSvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
        equalsTextSvg =
            textSvg "="

        fullHeight : Float
        fullHeight =
            (List.maximum
                [ valueASvg.height
                , equalsTextSvg.height
                , valueBSvg.height
                ]
                |> Maybe.withDefault 0
            )
                + strokeWidth

        shapeSvg : { width : Float, height : Float, svg : Web.Dom.Node future_ }
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform color
                ]
                [ ( 0, strokeWidth )
                , ( strokeWidth, 0 )
                , ( fullWidth, 0 )
                , ( fullWidth, fullHeight )
                , ( strokeWidth, fullHeight )
                , ( 0, fullHeight - strokeWidth )
                ]
    in
    { height = shapeSvg.height
    , width = shapeSvg.width
    , svg =
        Web.Svg.element "g"
            []
            [ shapeSvg.svg
            , Web.Svg.element "g"
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ valueASvg.svg ]
            , Web.Svg.element "g"
                [ svgAttributeTranslate
                    { x = strokeWidth + valueASvg.width + spaceWidth
                    , y = fullHeight / 2
                    }
                ]
                [ equalsTextSvg.svg ]
            , Web.Svg.element "g"
                [ svgAttributeTranslate
                    { x = strokeWidth + valueASvg.width + spaceWidth + equalsTextSvg.width + spaceWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ valueBSvg.svg ]
            ]
    }


polygonSvg :
    List (Web.Dom.Modifier future_)
    -> List ( Float, Float )
    -> { width : Float, height : Float, svg : Web.Dom.Node future_ }
polygonSvg modifiers points =
    let
        xMinimum : Float
        xMinimum =
            points |> List.map (\( x, _ ) -> x) |> List.minimum |> Maybe.withDefault 0

        xMaximum : Float
        xMaximum =
            points |> List.map (\( x, _ ) -> x) |> List.maximum |> Maybe.withDefault 0

        yMinimum : Float
        yMinimum =
            points |> List.map (\( _, y ) -> y) |> List.minimum |> Maybe.withDefault 0

        yMaximum : Float
        yMaximum =
            points |> List.map (\( _, y ) -> y) |> List.maximum |> Maybe.withDefault 0
    in
    { width = xMaximum - xMinimum
    , height = yMaximum - yMinimum
    , svg =
        Web.Svg.element "polygon"
            (Web.Dom.attribute "points"
                (points
                    |> List.map
                        (\( x, y ) ->
                            (x |> String.fromFloat) ++ "," ++ (y |> String.fromFloat)
                        )
                    |> String.join " "
                )
                :: modifiers
            )
            []
    }


relationUseSvg :
    { identifier : String, argument : ValueUiState }
    -> { svg : Web.Dom.Node future_, width : Float, height : Float }
relationUseSvg relationUse =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        fullWidth : Float
        fullWidth =
            strokeWidth
                + identifierTextSvg.width
                + spaceWidth
                + argumentAsSvg.width
                + strokeWidth

        spaceWidth : Float
        spaceWidth =
            fontWidth / 2

        color : Color
        color =
            Color.rgb 0.2 0.2 0

        argumentAsSvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
        argumentAsSvg =
            relationUse.argument |> valueSvg

        identifierTextSvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
        identifierTextSvg =
            textSvg relationUse.identifier

        fullHeight : Float
        fullHeight =
            (List.maximum
                [ identifierTextSvg.height
                , argumentAsSvg.height
                ]
                |> Maybe.withDefault 0
            )
                + strokeWidth

        shapeSvg : { width : Float, height : Float, svg : Web.Dom.Node future_ }
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform color
                ]
                [ ( 0, strokeWidth )
                , ( strokeWidth, 0 )
                , ( fullWidth, 0 )
                , ( fullWidth, fullHeight )
                , ( strokeWidth, fullHeight )
                , ( 0, fullHeight - strokeWidth )
                ]
    in
    { height = fullHeight
    , width = fullWidth
    , svg =
        Web.Svg.element "g"
            []
            [ shapeSvg.svg
            , Web.Svg.element "g"
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = fullHeight / 2
                    }
                ]
                [ identifierTextSvg.svg ]
            , Web.Svg.element "g"
                [ svgAttributeTranslate
                    { x = strokeWidth + identifierTextSvg.width + spaceWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ argumentAsSvg.svg ]
            ]
    }


valueSvg : ValueUiState -> { svg : Web.Dom.Node future_, width : Float, height : Float }
valueSvg =
    \value ->
        case value of
            ValueHole ->
                valueHoleSvg

            Variable variableName ->
                variableSvg variableName

            ValueLookup valueLookup ->
                Debug.todo ""


factListSvg :
    List FactUiState
    ->
        { width : Float
        , height : Float
        , svgs : List { y : Float, svg : Web.Dom.Node future_ }
        }
factListSvg =
    \facts ->
        let
            factsAsSvgs :
                { combinedHeight : Float
                , widthMaximum : Float
                , svgsReverse : List { y : Float, svg : Web.Dom.Node future_ }
                }
            factsAsSvgs =
                facts
                    |> List.foldl
                        (\fact soFar ->
                            let
                                factAsSvg =
                                    fact |> factSvg
                            in
                            { combinedHeight = soFar.combinedHeight + factAsSvg.height
                            , widthMaximum = Basics.max soFar.widthMaximum factAsSvg.width
                            , svgsReverse =
                                soFar.svgsReverse
                                    |> (::)
                                        { svg = factAsSvg.svg, y = soFar.combinedHeight }
                            }
                        )
                        { combinedHeight = 0
                        , widthMaximum = 0
                        , svgsReverse = []
                        }
        in
        { height = factsAsSvgs.combinedHeight
        , width = factsAsSvgs.widthMaximum
        , svgs =
            factsAsSvgs.svgsReverse
                |> List.reverse
        }


variableSvgHeight : Float
variableSvgHeight =
    fontSize + fontSize


fontBaseline : Float
fontBaseline =
    fontSize * 0.3


fontWidth : Float
fontWidth =
    fontSize * 0.42


fontSize : Float
fontSize =
    20


variableSvg : String -> { svg : Web.Dom.Node future_, width : Float, height : Float }
variableSvg variableName =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize * 2

        color : Color
        color =
            Color.rgb 0 0.11 0.21

        nameSvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
        nameSvg =
            textSvg variableName

        shapeSvg : { width : Float, height : Float, svg : Web.Dom.Node future_ }
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform color
                , Web.Dom.attribute "stroke" (color |> Color.toCssString)
                , Web.Dom.attribute "stroke-width" (strokeWidth |> String.fromFloat)
                , Web.Dom.attribute "stroke-linejoin" "round"
                ]
                [ ( strokeWidth / 2, strokeWidth / 2 )
                , ( nameSvg.width + strokeWidth / 2, strokeWidth / 2 )
                , ( nameSvg.width + strokeWidth / 2, fontSize )
                , ( strokeWidth / 2, fontSize )
                ]
    in
    { width = nameSvg.width + strokeWidth
    , height = fontSize + fontSize
    , svg =
        Web.Svg.element "g"
            []
            [ shapeSvg.svg
            , Web.Svg.element "g"
                [ svgAttributeTranslate
                    { x = strokeWidth / 2
                    , y = strokeWidth / 2
                    }
                ]
                [ nameSvg.svg ]
            ]
    }


valueHoleSvg : { svg : Web.Dom.Node future_, width : Float, height : Float }
valueHoleSvg =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize * 2

        color : Color
        color =
            Color.rgba 0 0 0 0.4

        nameSvg : { height : Float, width : Float, svg : Web.Dom.Node future_ }
        nameSvg =
            textSvg "drag a value here"

        shapeSvg : { width : Float, height : Float, svg : Web.Dom.Node future_ }
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform color
                , Web.Dom.attribute "stroke" (color |> Color.toCssString)
                , Web.Dom.attribute "stroke-width" (strokeWidth |> String.fromFloat)
                , Web.Dom.attribute "stroke-linejoin" "round"
                ]
                [ ( strokeWidth / 2, strokeWidth / 2 )
                , ( nameSvg.width + strokeWidth / 2, strokeWidth / 2 )
                , ( nameSvg.width + strokeWidth / 2, fontSize )
                , ( strokeWidth / 2, fontSize )
                ]
    in
    { width = nameSvg.width + strokeWidth
    , height = fontSize + fontSize
    , svg =
        Web.Svg.element "g"
            []
            [ shapeSvg.svg
            , Web.Svg.element "g"
                [ svgAttributeTranslate
                    { x = strokeWidth / 2
                    , y = strokeWidth / 2
                    }
                ]
                [ nameSvg.svg ]
            ]
    }


svgAttributeFillUniform : Color -> Web.Dom.Modifier future_
svgAttributeFillUniform color =
    Web.Dom.attribute "fill" (color |> Color.toCssString)


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
