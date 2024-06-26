port module Main exposing (State, main)

import Color exposing (Color)
import FastDict
import Json.Decode
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
    , relationDefinitions :
        FastDict.Dict
            String
            { argument : ValueUiState
            , equivalentFact : Maybe FactUiState
            }
    , strayThings : List { x : Float, y : Float, thing : DraggedThing }
    , dragged : DragState
    }


type alias DragState =
    Maybe
        { x : Float
        , y : Float
        , offsetX : Float
        , offsetY : Float
        , thing : DraggedThing
        }


type DraggedThing
    = DraggedFact FactUiState
    | DraggedValueLookup (FastDict.Dict String ValueUiState)
    | DraggedVariable String


type FactUiState
    = RelationUse { identifier : String, argument : ValueUiState }
    | Equal { a : ValueUiState, b : ValueUiState }
    | Not (Maybe FactUiState)
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
                Maybe.map Lau.Not (inverseFactWithHoles |> Maybe.andThen factUiStateToLau)

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
        , dragged = Nothing
        , relationDefinitions =
            FastDict.singleton "main"
                { argument = Variable "Result"
                , equivalentFact =
                    Any
                        [ All
                            [ Not
                                (All
                                    []
                                    |> Just
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
                        |> Just
                }
        , strayThings = []
        }
    , interface = interface
    }


interface : State -> Web.Interface State
interface state =
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
        , case state.dragged of
            Nothing ->
                Web.Dom.modifierNone

            Just stateDragged ->
                Web.Dom.listenTo "pointermove"
                    |> Web.Dom.modifierFutureMap
                        (\eventJson ->
                            case
                                eventJson
                                    |> Json.Decode.decodeValue
                                        (Json.Decode.map2 (\x y -> { x = x, y = y })
                                            (Json.Decode.field "clientX" Json.Decode.float)
                                            (Json.Decode.field "clientY" Json.Decode.float)
                                        )
                            of
                                Ok position ->
                                    { state
                                        | dragged =
                                            Just
                                                { x = position.x
                                                , y = position.y
                                                , offsetX = stateDragged.offsetX
                                                , offsetY = stateDragged.offsetY
                                                , thing = stateDragged.thing
                                                }
                                    }

                                Err _ ->
                                    state
                        )
        ]
        [ domSvgContainer
            { left = 0
            , top = 0
            , bottom = state.windowWidth
            , right = state.windowWidth
            }
            [ Web.Dom.attribute "width" ((state.windowWidth |> String.fromInt) ++ "px")
            , Web.Dom.attribute "height" ((state.windowWidth |> String.fromInt) ++ "px")
            , Web.Dom.style "display" "block"
            , Web.Dom.style "margin" "auto"
            , Web.Dom.style "font-family" "monospace"
            , Web.Dom.style "font-size" (fontSize |> String.fromFloat)
            ]
            [ Web.Svg.element "rect"
                [ Web.Dom.attribute "width" "100%"
                , Web.Dom.attribute "height" "100%"
                , Web.Dom.attribute "fill" (Color.rgb 0 0 0 |> Color.toCssString)
                , case state.dragged of
                    Nothing ->
                        Web.Dom.modifierNone

                    Just dragged ->
                        Web.Dom.listenTo "pointerup"
                            |> Web.Dom.modifierFutureMap
                                (\_ ->
                                    { state
                                        | dragged = Nothing
                                        , strayThings =
                                            state.strayThings
                                                |> (::)
                                                    { x = dragged.x + dragged.offsetX
                                                    , y = dragged.y + dragged.offsetY
                                                    , thing = dragged.thing
                                                    }
                                    }
                                )
                ]
                []
            , state.relationDefinitions
                |> FastDict.toList
                |> List.foldl
                    (\( name, definition ) soFar ->
                        let
                            definitionAsSvg =
                                relationDefinitionSvg state.dragged
                                    { name = name
                                    , argument = definition.argument
                                    , equivalentFact = definition.equivalentFact
                                    }
                                    |> sizedSvgFutureMap
                                        (\relationUiState ->
                                            { state
                                                | dragged = relationUiState.dragged
                                                , relationDefinitions =
                                                    state.relationDefinitions
                                                        |> FastDict.update name
                                                            (\_ -> Just relationUiState.relationDefinition)
                                            }
                                        )
                        in
                        { height = soFar.height + 50 + definitionAsSvg.height
                        , svgsReverse =
                            soFar.svgsReverse
                                |> (::)
                                    (stackSvg
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
                |> stackSvg
                    [ svgAttributeTranslate { x = 140, y = 140 } ]
            , state.strayThings
                |> List.indexedMap
                    (\strayThingIndex strayThing ->
                        let
                            strayThingSvg =
                                case strayThing.thing of
                                    DraggedVariable variableName ->
                                        variableSvg state.dragged variableName
                                            |> .svg
                                            |> Web.Dom.futureMap
                                                (\future ->
                                                    { state
                                                        | dragged = future.dragged
                                                        , strayThings =
                                                            case future.variable of
                                                                Nothing ->
                                                                    state.strayThings
                                                                        |> List.LocalExtra.removeElementAtIndex strayThingIndex

                                                                Just futureVariableName ->
                                                                    state.strayThings
                                                                        |> List.LocalExtra.elementAtIndexAlter strayThingIndex
                                                                            (\stray -> { stray | thing = DraggedVariable futureVariableName })
                                                    }
                                                )

                                    DraggedValueLookup valueLookup ->
                                        valueLookupSvg state.dragged valueLookup
                                            |> .svg
                                            |> Web.Dom.futureMap
                                                (\future ->
                                                    { state
                                                        | dragged = future.dragged
                                                        , strayThings =
                                                            case future.valueLookup of
                                                                Nothing ->
                                                                    state.strayThings
                                                                        |> List.LocalExtra.removeElementAtIndex strayThingIndex

                                                                Just futureValueLookup ->
                                                                    state.strayThings
                                                                        |> List.LocalExtra.elementAtIndexAlter strayThingIndex
                                                                            (\stray -> { stray | thing = DraggedValueLookup futureValueLookup })
                                                    }
                                                )

                                    DraggedFact fact ->
                                        factSvg state.dragged fact
                                            |> .svg
                                            |> Web.Dom.futureMap
                                                (\future ->
                                                    { state
                                                        | dragged = future.dragged
                                                        , strayThings =
                                                            case future.fact of
                                                                Nothing ->
                                                                    state.strayThings
                                                                        |> List.LocalExtra.removeElementAtIndex strayThingIndex

                                                                Just futureFact ->
                                                                    state.strayThings
                                                                        |> List.LocalExtra.elementAtIndexAlter strayThingIndex
                                                                            (\stray -> { stray | thing = DraggedFact futureFact })
                                                    }
                                                )
                        in
                        strayThingSvg
                            |> List.singleton
                            |> stackSvg
                                [ svgAttributeTranslate
                                    { x = strayThing.x
                                    , y = strayThing.y
                                    }
                                ]
                    )
                |> stackSvg []
            , case state.dragged of
                Nothing ->
                    stackSvg [] []

                Just dragged ->
                    stackSvg
                        [ svgAttributeTranslate
                            { x = dragged.x + dragged.offsetX
                            , y = dragged.y + dragged.offsetY
                            }
                        , Web.Dom.style "pointer-events" "none"
                        ]
                        [ case dragged.thing of
                            DraggedVariable variableName ->
                                variableShapeSvg variableName |> .svg

                            DraggedValueLookup draggedValue ->
                                draggedValue |> valueLookupShapeSvg |> .svg

                            DraggedFact draggedFact ->
                                draggedFact |> factShapeSvg |> .svg
                        ]
            ]
        ]
        |> Web.Dom.render
    ]
        |> Web.interfaceBatch


stackSvg :
    List (Web.Dom.Modifier future)
    -> List (Web.Dom.Node future)
    -> Web.Dom.Node future
stackSvg modifiers subs =
    Web.Svg.element "g" modifiers subs


domSvgContainer :
    { left : Int, top : Int, right : Int, bottom : Int }
    -> List (Web.Dom.Modifier future_)
    -> List (Web.Dom.Node future_)
    -> Web.Dom.Node future_
domSvgContainer size modifiers subs =
    Web.Svg.element "svg"
        (Web.Dom.attribute "viewBox"
            ([ size.left |> String.fromInt
             , " "
             , size.top |> String.fromInt
             , " "
             , size.right |> String.fromInt
             , " "
             , size.bottom |> String.fromInt
             ]
                |> String.concat
            )
            :: modifiers
        )
        subs


svgAttributeTranslate : { x : Float, y : Float } -> Web.Dom.Modifier future_
svgAttributeTranslate offset =
    Web.Dom.attribute "transform"
        ("translate("
            ++ (offset.x |> String.fromFloat)
            ++ " "
            ++ (offset.y |> String.fromFloat)
            ++ ")"
        )


type alias SizedSvg future =
    { height : Float, width : Float, svg : Web.Dom.Node future }


unselectableTextSvg : String -> SizedSvg future_
unselectableTextSvg string =
    { height = fontSize
    , width = (string |> String.length |> Basics.toFloat) * fontWidth
    , svg =
        Web.Svg.element "text"
            [ Web.Dom.style "user-select" "none"
            , Web.Dom.style "pointer-events" "none"
            , Web.Dom.attribute "x" (0 |> String.fromFloat)
            , Web.Dom.attribute "y" (fontBaseline |> String.fromFloat)
            , svgAttributeFillUniform (Color.rgb 1 1 1)
            ]
            [ Web.Dom.text string ]
    }


horizontalSvg : List (SizedSvg future) -> SizedSvg future
horizontalSvg =
    \subs ->
        let
            subsAsSvgs :
                { heightMaximum : Float
                , combinedWidth : Float
                , svgsReverse : List (Web.Dom.Node future)
                }
            subsAsSvgs =
                subs
                    |> List.foldl
                        (\asSvg soFar ->
                            { heightMaximum = Basics.max soFar.heightMaximum asSvg.height
                            , combinedWidth = soFar.combinedWidth + asSvg.width
                            , svgsReverse =
                                soFar.svgsReverse
                                    |> (::)
                                        (asSvg.svg
                                            |> List.singleton
                                            |> stackSvg
                                                [ svgAttributeTranslate { x = soFar.combinedWidth, y = 0 }
                                                ]
                                        )
                            }
                        )
                        { heightMaximum = 0
                        , combinedWidth = 0
                        , svgsReverse = []
                        }
        in
        { height = subsAsSvgs.heightMaximum
        , width = subsAsSvgs.combinedWidth
        , svg =
            subsAsSvgs.svgsReverse
                |> List.reverse
                |> stackSvg []
        }


relationDefinitionSvg :
    DragState
    ->
        { name : String
        , argument : ValueUiState
        , equivalentFact : Maybe FactUiState
        }
    ->
        SizedSvg
            { dragged : DragState
            , relationDefinition :
                { argument : ValueUiState
                , equivalentFact : Maybe FactUiState
                }
            }
relationDefinitionSvg dragState definition =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        headerWidth : Float
        headerWidth =
            strokeWidth
                + nameSvg.width
                + spaceWidth
                + argumentSvg.width
                + spaceWidth
                + equalsTextSvg.width
                + strokeWidth

        fullWidth : Float
        fullWidth =
            headerWidth + equivalentFactSvg.width

        spaceWidth : Float
        spaceWidth =
            fontWidth / 2

        color : Color
        color =
            Color.rgb 0.2 0.2 0

        argumentSvg : SizedSvg { dragged : DragState, value : ValueUiState }
        argumentSvg =
            definition.argument |> valueSvg dragState

        equivalentFactSvg : SizedSvg { dragged : DragState, fact : Maybe FactUiState }
        equivalentFactSvg =
            case definition.equivalentFact of
                Nothing ->
                    factMissingSvg dragState
                        |> sizedSvgFutureMap
                            (\dropped ->
                                { dragged = Nothing, fact = Just dropped }
                            )

                Just equivalentFact ->
                    factSvg dragState equivalentFact

        nameSvg : SizedSvg future_
        nameSvg =
            unselectableTextSvg definition.name

        equalsTextSvg : SizedSvg future_
        equalsTextSvg =
            unselectableTextSvg "="

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

        shapeSvg : SizedSvg future_
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform color
                ]
                (case definition.equivalentFact of
                    Nothing ->
                        [ ( 0, 0 )
                        , ( fullWidth, 0 )
                        , ( fullWidth, fullHeight )
                        , ( 0, fullHeight )
                        ]

                    Just _ ->
                        [ ( 0, 0 )
                        , ( headerWidth + strokeWidth, 0 )
                        , ( headerWidth, strokeWidth )
                        , ( headerWidth, fullHeight - strokeWidth )
                        , ( headerWidth + strokeWidth, fullHeight )
                        , ( 0, fullHeight )
                        ]
                )
    in
    { height = fullHeight
    , width = fullWidth
    , svg =
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = (variableSvgHeight + strokeWidth) / 2
                    }
                ]
                [ nameSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth + nameSvg.width + spaceWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ argumentSvg.svg
                    |> Web.Dom.futureMap
                        (\futureArgumentUiState ->
                            { dragged = futureArgumentUiState.dragged
                            , relationDefinition =
                                { equivalentFact = definition.equivalentFact
                                , argument = futureArgumentUiState.value
                                }
                            }
                        )
                ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth + nameSvg.width + spaceWidth + argumentSvg.width + spaceWidth
                    , y = (variableSvgHeight + strokeWidth) / 2
                    }
                ]
                [ equalsTextSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = headerWidth
                    , y = 0
                    }
                ]
                [ equivalentFactSvg.svg
                    |> Web.Dom.futureMap
                        (\equivalentFactUiState ->
                            { dragged = equivalentFactUiState.dragged
                            , relationDefinition =
                                { equivalentFact = equivalentFactUiState.fact
                                , argument = definition.argument
                                }
                            }
                        )
                ]
            ]
    }


factShapeSvg : FactUiState -> SizedSvg future_
factShapeSvg fact =
    case fact of
        All parts ->
            factAllShapeSvg parts

        Any branches ->
            factAnyShapeSvg branches

        Not inverseFact ->
            factNotShapeSvg inverseFact

        Equal toEquate ->
            factEqualsShapeSvg toEquate

        RelationUse relationUse ->
            relationUseShapeSvg relationUse


factSvg :
    DragState
    -> FactUiState
    -> SizedSvg { dragged : DragState, fact : Maybe FactUiState }
factSvg dragState fact =
    case fact of
        All parts ->
            factAllSvg dragState parts
                |> sizedSvgFutureMap
                    (\futureAll ->
                        { dragged = futureAll.dragged
                        , fact = Maybe.map All futureAll.parts
                        }
                    )

        Any branches ->
            factAnySvg dragState branches
                |> sizedSvgFutureMap
                    (\futureAny ->
                        { dragged = futureAny.dragged
                        , fact = Maybe.map Any futureAny.branches
                        }
                    )

        Not inverseFact ->
            factNotSvg dragState inverseFact
                |> sizedSvgFutureMap
                    (\futureNot ->
                        { dragged = futureNot.dragged
                        , fact = Maybe.map Not futureNot.inverseFact
                        }
                    )

        Equal toEquate ->
            factEqualsSvg dragState toEquate
                |> sizedSvgFutureMap
                    (\equal ->
                        { dragged = equal.dragged
                        , fact = Maybe.map Equal equal.values
                        }
                    )

        RelationUse relationUse ->
            relationUseSvg dragState relationUse
                |> sizedSvgFutureMap
                    (\relationUseFuture ->
                        { dragged = relationUseFuture.dragged
                        , fact = Maybe.map RelationUse relationUseFuture.relationUse
                        }
                    )


factAllShapeSvg : List FactUiState -> SizedSvg future_
factAllShapeSvg parts =
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
                    (part0 :: part1Up)
                        |> List.map factShapeSvg
                        |> verticalSvg

        fullWidth : Float
        fullWidth =
            sideWidth
                + Basics.max partsSvg.width (strokeWidth + allStringSvg.width + strokeWidth)

        allStringSvg : SizedSvg future_
        allStringSvg =
            unselectableTextSvg "all"

        headerHeight : Float
        headerHeight =
            fontSize + strokeWidth

        fullHeight : Float
        fullHeight =
            headerHeight + partsSvg.height + sideWidth

        shapeSvg : SizedSvg future_
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
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
                [ svgAttributeTranslate
                    { x = sideWidth
                    , y = strokeWidth
                    }
                ]
                [ allStringSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate { x = sideWidth, y = headerHeight } ]
                [ partsSvg.svgs
                    |> List.map
                        (\partAsSvg ->
                            partAsSvg.svg
                                |> List.singleton
                                |> stackSvg
                                    [ svgAttributeTranslate { x = 0, y = partAsSvg.y }
                                    ]
                        )
                    |> stackSvg []
                ]
            ]
    }


factAllSvg :
    DragState
    -> List FactUiState
    -> SizedSvg { dragged : DragState, parts : Maybe (List FactUiState) }
factAllSvg dragState parts =
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
            , svgs :
                List
                    { y : Float
                    , svg : Web.Dom.Node { dragged : DragState, parts : List FactUiState }
                    }
            }
        partsSvg =
            case parts of
                [] ->
                    { width = 0
                    , height = strokeWidth + strokeWidth
                    , svgs = []
                    }

                part0 :: part1Up ->
                    (part0 :: part1Up)
                        |> List.indexedMap
                            (\partIndex part ->
                                factSvg dragState part
                                    |> sizedSvgFutureMap
                                        (\partFutureUiState ->
                                            { dragged = partFutureUiState.dragged
                                            , parts =
                                                case partFutureUiState.fact of
                                                    Nothing ->
                                                        (part0 :: part1Up)
                                                            |> List.LocalExtra.removeElementAtIndex partIndex

                                                    Just futurePartFact ->
                                                        (part0 :: part1Up)
                                                            |> List.LocalExtra.elementAtIndexAlter partIndex
                                                                (\_ -> futurePartFact)
                                            }
                                        )
                            )
                        |> verticalSvg

        fullWidth : Float
        fullWidth =
            sideWidth
                + Basics.max partsSvg.width (strokeWidth + allStringSvg.width + strokeWidth)

        allStringSvg : SizedSvg future_
        allStringSvg =
            unselectableTextSvg "all"

        headerHeight : Float
        headerHeight =
            fontSize + strokeWidth

        fullHeight : Float
        fullHeight =
            headerHeight + partsSvg.height + sideWidth

        shapeSvg : SizedSvg { dragged : DragState, parts : Maybe (List FactUiState) }
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform (Color.rgb 0 0.14 0)
                , domListenToPointerDown
                    |> Web.Dom.modifierFutureMap
                        (\pointerDownEventPosition ->
                            case pointerDownEventPosition of
                                Err _ ->
                                    { dragged = dragState, parts = Just parts }

                                Ok pointer ->
                                    { dragged =
                                        Just
                                            { x = pointer.x
                                            , y = pointer.y
                                            , offsetX = -fontSize
                                            , offsetY = -fontSize
                                            , thing = DraggedFact (All parts)
                                            }
                                    , parts = Nothing
                                    }
                        )
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
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
                [ svgAttributeTranslate
                    { x = sideWidth
                    , y = strokeWidth
                    }
                ]
                [ allStringSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate { x = sideWidth, y = headerHeight } ]
                [ partsSvg.svgs
                    |> List.map
                        (\partAsSvg ->
                            partAsSvg
                                |> .svg
                                |> Web.Dom.futureMap
                                    (\future ->
                                        { dragged = future.dragged, parts = Just future.parts }
                                    )
                                |> List.singleton
                                |> stackSvg
                                    [ svgAttributeTranslate { x = 0, y = partAsSvg.y }
                                    ]
                        )
                    |> stackSvg []
                ]
            ]
    }


sizedSvgFutureMap :
    (future -> futureChanged)
    -> (SizedSvg future -> SizedSvg futureChanged)
sizedSvgFutureMap futureChange =
    \sized ->
        { width = sized.width
        , height = sized.height
        , svg =
            sized.svg |> Web.Dom.futureMap futureChange
        }


factAnyShapeSvg : List FactUiState -> SizedSvg future_
factAnyShapeSvg branches =
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
            , svgs :
                List { y : Float, svg : Web.Dom.Node future_ }
            }
        branchesSvg =
            case branches of
                [] ->
                    { width = 0
                    , height = strokeWidth + strokeWidth
                    , svgs = []
                    }

                branch0 :: branch1Up ->
                    (branch0 :: branch1Up)
                        |> List.map factShapeSvg
                        |> verticalSvg

        fullWidth : Float
        fullWidth =
            sideWidth
                + Basics.max
                    branchesSvg.width
                    (strokeWidth + anyTextSvg.width + strokeWidth)

        anyTextSvg : SizedSvg future_
        anyTextSvg =
            unselectableTextSvg "any"

        headerHeight : Float
        headerHeight =
            fontSize + strokeWidth

        fullHeight : Float
        fullHeight =
            headerHeight + branchesSvg.height + sideWidth

        shapeSvg : SizedSvg future_
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
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
                [ svgAttributeTranslate
                    { x = sideWidth
                    , y = (fontSize + strokeWidth) / 2
                    }
                ]
                [ anyTextSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate { x = sideWidth, y = headerHeight } ]
                [ branchesSvg.svgs
                    |> List.map
                        (\branchAsSvg ->
                            branchAsSvg.svg
                                |> List.singleton
                                |> stackSvg
                                    [ svgAttributeTranslate { x = 0, y = branchAsSvg.y }
                                    ]
                        )
                    |> stackSvg []
                ]
            ]
    }


factAnySvg :
    DragState
    -> List FactUiState
    -> SizedSvg { dragged : DragState, branches : Maybe (List FactUiState) }
factAnySvg dragState branches =
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
            , svgs :
                List
                    { y : Float
                    , svg : Web.Dom.Node { dragged : DragState, branches : List FactUiState }
                    }
            }
        branchesSvg =
            case branches of
                [] ->
                    { width = 0
                    , height = strokeWidth + strokeWidth
                    , svgs = []
                    }

                branch0 :: branch1Up ->
                    (branch0 :: branch1Up)
                        |> List.indexedMap
                            (\branchIndex branch ->
                                branch
                                    |> factSvg dragState
                                    |> sizedSvgFutureMap
                                        (\branchFutureUiState ->
                                            { dragged = branchFutureUiState.dragged
                                            , branches =
                                                case branchFutureUiState.fact of
                                                    Nothing ->
                                                        (branch0 :: branch1Up)
                                                            |> List.LocalExtra.removeElementAtIndex branchIndex

                                                    Just futureBranchFact ->
                                                        (branch0 :: branch1Up)
                                                            |> List.LocalExtra.elementAtIndexAlter branchIndex
                                                                (\_ -> futureBranchFact)
                                            }
                                        )
                            )
                        |> verticalSvg

        fullWidth : Float
        fullWidth =
            sideWidth
                + Basics.max
                    branchesSvg.width
                    (strokeWidth + anyTextSvg.width + strokeWidth)

        anyTextSvg : SizedSvg future_
        anyTextSvg =
            unselectableTextSvg "any"

        headerHeight : Float
        headerHeight =
            fontSize + strokeWidth

        fullHeight : Float
        fullHeight =
            headerHeight + branchesSvg.height + sideWidth

        shapeSvg : SizedSvg { dragged : DragState, branches : Maybe (List FactUiState) }
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform (Color.rgb 0.2 0 0.2)
                , domListenToPointerDown
                    |> Web.Dom.modifierFutureMap
                        (\pointerDownEventPosition ->
                            case pointerDownEventPosition of
                                Err _ ->
                                    { dragged = dragState, branches = Just branches }

                                Ok pointer ->
                                    { dragged =
                                        Just
                                            { x = pointer.x
                                            , y = pointer.y
                                            , offsetX = -fontSize
                                            , offsetY = -fontSize
                                            , thing = DraggedFact (Any branches)
                                            }
                                    , branches = Nothing
                                    }
                        )
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
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
                [ svgAttributeTranslate
                    { x = sideWidth
                    , y = (fontSize + strokeWidth) / 2
                    }
                ]
                [ anyTextSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate { x = sideWidth, y = headerHeight } ]
                [ branchesSvg.svgs
                    |> List.map
                        (\branchAsSvg ->
                            branchAsSvg
                                |> .svg
                                |> Web.Dom.futureMap
                                    (\branchFuture ->
                                        { dragged = branchFuture.dragged
                                        , branches = Just branchFuture.branches
                                        }
                                    )
                                |> List.singleton
                                |> stackSvg
                                    [ svgAttributeTranslate { x = 0, y = branchAsSvg.y }
                                    ]
                        )
                    |> stackSvg []
                ]
            ]
    }


factNotShapeSvg : Maybe FactUiState -> SizedSvg future_
factNotShapeSvg maybeInverseFact =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        sideWidth : Float
        sideWidth =
            strokeWidth

        factInverseSvg : SizedSvg future_
        factInverseSvg =
            case maybeInverseFact of
                Nothing ->
                    factMissingShapeSvg

                Just equivalentFact ->
                    factShapeSvg equivalentFact

        headerWidth : Float
        headerWidth =
            strokeWidth + notTextSvg.width + strokeWidth

        fullWidth : Float
        fullWidth =
            headerWidth + factInverseSvg.width

        notTextSvg : SizedSvg future_
        notTextSvg =
            unselectableTextSvg "not"

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
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = (fontSize + strokeWidth) / 2
                    }
                ]
                [ notTextSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate { x = headerWidth, y = 0 } ]
                [ factInverseSvg.svg ]
            ]
    }


factNotSvg :
    DragState
    -> Maybe FactUiState
    -> SizedSvg { dragged : DragState, inverseFact : Maybe (Maybe FactUiState) }
factNotSvg dragState maybeInverseFact =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        sideWidth : Float
        sideWidth =
            strokeWidth

        factInverseSvg : SizedSvg { dragged : DragState, fact : Maybe FactUiState }
        factInverseSvg =
            case maybeInverseFact of
                Nothing ->
                    factMissingSvg dragState
                        |> sizedSvgFutureMap
                            (\dropped ->
                                { dragged = Nothing, fact = Just dropped }
                            )

                Just equivalentFact ->
                    factSvg dragState equivalentFact

        headerWidth : Float
        headerWidth =
            strokeWidth + notTextSvg.width + strokeWidth

        fullWidth : Float
        fullWidth =
            headerWidth + factInverseSvg.width

        notTextSvg : SizedSvg future_
        notTextSvg =
            unselectableTextSvg "not"

        fullHeight : Float
        fullHeight =
            Basics.max (fontSize + strokeWidth)
                factInverseSvg.height

        shapeSvg : SizedSvg { dragged : DragState, inverseFact : Maybe (Maybe FactUiState) }
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform (Color.rgb 0.2 0.04 0)
                , domListenToPointerDown
                    |> Web.Dom.modifierFutureMap
                        (\pointerDownEventPosition ->
                            case pointerDownEventPosition of
                                Err _ ->
                                    { dragged = dragState, inverseFact = Just maybeInverseFact }

                                Ok pointer ->
                                    { dragged =
                                        Just
                                            { x = pointer.x
                                            , y = pointer.y
                                            , offsetX = -fontSize
                                            , offsetY = -fontSize
                                            , thing = DraggedFact (Not maybeInverseFact)
                                            }
                                    , inverseFact = Nothing
                                    }
                        )
                ]
                (case maybeInverseFact of
                    Nothing ->
                        [ ( 0, strokeWidth )
                        , ( strokeWidth, 0 )
                        , ( fullWidth, 0 )
                        , ( fullWidth, fullHeight )
                        , ( strokeWidth, fullHeight )
                        , ( 0, fullHeight - strokeWidth )
                        ]

                    Just _ ->
                        [ ( 0, strokeWidth )
                        , ( strokeWidth, 0 )
                        , ( headerWidth + strokeWidth, 0 )
                        , ( headerWidth, strokeWidth )
                        , ( headerWidth, fullHeight - strokeWidth )
                        , ( headerWidth + strokeWidth, fullHeight )
                        , ( strokeWidth, fullHeight )
                        , ( 0, fullHeight - strokeWidth )
                        ]
                )
    in
    { width = fullWidth
    , height = fullHeight
    , svg =
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = (fontSize + strokeWidth) / 2
                    }
                ]
                [ notTextSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate { x = headerWidth, y = 0 } ]
                [ factInverseSvg.svg
                    |> Web.Dom.futureMap
                        (\futureFactInverse ->
                            { inverseFact = Just futureFactInverse.fact
                            , dragged = futureFactInverse.dragged
                            }
                        )
                ]
            ]
    }


factMissingSvg : DragState -> SizedSvg FactUiState
factMissingSvg dragState =
    let
        shapeSvg : SizedSvg future_
        shapeSvg =
            factMissingShapeSvg
    in
    { height = shapeSvg.height
    , width = shapeSvg.width
    , svg =
        stackSvg
            [ case dragState of
                Nothing ->
                    Web.Dom.modifierNone

                Just dragged ->
                    case dragged.thing of
                        DraggedVariable _ ->
                            Web.Dom.modifierNone

                        DraggedValueLookup _ ->
                            Web.Dom.modifierNone

                        DraggedFact draggedFact ->
                            Web.Dom.listenTo "pointerup"
                                |> Web.Dom.modifierFutureMap
                                    (\_ -> draggedFact)
            ]
            [ shapeSvg.svg ]
    }


factMissingShapeSvg : SizedSvg future_
factMissingShapeSvg =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        fullWidth : Float
        fullWidth =
            strokeWidth
                + missingTextSvg.width
                + strokeWidth

        spaceWidth : Float
        spaceWidth =
            fontWidth / 2

        missingTextSvg : SizedSvg future_
        missingTextSvg =
            unselectableTextSvg "drag a fact here"

        fullHeight : Float
        fullHeight =
            missingTextSvg.height + strokeWidth + strokeWidth

        shapeSvg : SizedSvg future_
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform missingThingColor
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
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = fullHeight / 2
                    }
                ]
                [ missingTextSvg.svg ]
            ]
    }


factEqualsShapeSvg :
    { a : ValueUiState, b : ValueUiState }
    -> SizedSvg future_
factEqualsShapeSvg toEquate =
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

        valueASvg : SizedSvg future_
        valueASvg =
            toEquate.a |> valueShapeSvg

        valueBSvg : SizedSvg future_
        valueBSvg =
            toEquate.b |> valueShapeSvg

        equalsTextSvg : SizedSvg future_
        equalsTextSvg =
            unselectableTextSvg "="

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

        shapeSvg : SizedSvg future_
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
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ valueASvg.svg ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth + valueASvg.width + spaceWidth
                    , y = fullHeight / 2
                    }
                ]
                [ equalsTextSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth + valueASvg.width + spaceWidth + equalsTextSvg.width + spaceWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ valueBSvg.svg ]
            ]
    }


factEqualsSvg :
    DragState
    -> { a : ValueUiState, b : ValueUiState }
    ->
        SizedSvg
            { dragged : DragState
            , values : Maybe { a : ValueUiState, b : ValueUiState }
            }
factEqualsSvg dragState toEquate =
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

        valueASvg : SizedSvg { dragged : DragState, value : ValueUiState }
        valueASvg =
            toEquate.a |> valueSvg dragState

        valueBSvg : SizedSvg { dragged : DragState, value : ValueUiState }
        valueBSvg =
            toEquate.b |> valueSvg dragState

        equalsTextSvg : SizedSvg future_
        equalsTextSvg =
            unselectableTextSvg "="

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

        shapeSvg :
            SizedSvg
                { dragged : DragState
                , values : Maybe { a : ValueUiState, b : ValueUiState }
                }
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform color
                , domListenToPointerDown
                    |> Web.Dom.modifierFutureMap
                        (\pointerDownEventPosition ->
                            case pointerDownEventPosition of
                                Err _ ->
                                    { dragged = dragState, values = Just toEquate }

                                Ok pointer ->
                                    { dragged =
                                        Just
                                            { x = pointer.x
                                            , y = pointer.y
                                            , offsetX = -fontSize
                                            , offsetY = -fontSize
                                            , thing = DraggedFact (Equal toEquate)
                                            }
                                    , values = Nothing
                                    }
                        )
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
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ valueASvg.svg
                    |> Web.Dom.futureMap
                        (\futureA ->
                            { dragged = futureA.dragged
                            , values = Just { a = futureA.value, b = toEquate.b }
                            }
                        )
                ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth + valueASvg.width + spaceWidth
                    , y = fullHeight / 2
                    }
                ]
                [ equalsTextSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth + valueASvg.width + spaceWidth + equalsTextSvg.width + spaceWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ valueBSvg.svg
                    |> Web.Dom.futureMap
                        (\futureB ->
                            { dragged = futureB.dragged
                            , values = Just { a = toEquate.a, b = futureB.value }
                            }
                        )
                ]
            ]
    }


polygonSvg :
    List (Web.Dom.Modifier future_)
    -> List ( Float, Float )
    -> SizedSvg future_
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


relationUseShapeSvg :
    { identifier : String, argument : ValueUiState }
    -> SizedSvg future_
relationUseShapeSvg relationUse =
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

        argumentAsSvg : SizedSvg future_
        argumentAsSvg =
            relationUse.argument |> valueShapeSvg

        identifierTextSvg : SizedSvg future_
        identifierTextSvg =
            unselectableTextSvg relationUse.identifier

        fullHeight : Float
        fullHeight =
            (List.maximum
                [ identifierTextSvg.height
                , argumentAsSvg.height
                ]
                |> Maybe.withDefault 0
            )
                + strokeWidth

        shapeSvg : SizedSvg future_
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
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = fullHeight / 2
                    }
                ]
                [ identifierTextSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth + identifierTextSvg.width + spaceWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ argumentAsSvg.svg ]
            ]
    }


relationUseSvg :
    DragState
    -> { identifier : String, argument : ValueUiState }
    ->
        SizedSvg
            { dragged : DragState
            , relationUse : Maybe { identifier : String, argument : ValueUiState }
            }
relationUseSvg dragState =
    \relationUse ->
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

            argumentAsSvg : SizedSvg { dragged : DragState, value : ValueUiState }
            argumentAsSvg =
                relationUse.argument |> valueSvg dragState

            identifierTextSvg : SizedSvg future_
            identifierTextSvg =
                unselectableTextSvg relationUse.identifier

            fullHeight : Float
            fullHeight =
                (List.maximum
                    [ identifierTextSvg.height
                    , argumentAsSvg.height
                    ]
                    |> Maybe.withDefault 0
                )
                    + strokeWidth

            shapeSvg :
                SizedSvg
                    { dragged : DragState
                    , relationUse : Maybe { identifier : String, argument : ValueUiState }
                    }
            shapeSvg =
                polygonSvg
                    [ svgAttributeFillUniform color
                    , domListenToPointerDown
                        |> Web.Dom.modifierFutureMap
                            (\pointerDownEventPosition ->
                                case pointerDownEventPosition of
                                    Err _ ->
                                        { dragged = dragState, relationUse = Just relationUse }

                                    Ok pointer ->
                                        { dragged =
                                            Just
                                                { x = pointer.x
                                                , y = pointer.y
                                                , offsetX = -fontSize
                                                , offsetY = -fontSize
                                                , thing = DraggedFact (RelationUse relationUse)
                                                }
                                        , relationUse = Nothing
                                        }
                            )
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
            stackSvg
                []
                [ shapeSvg.svg
                , stackSvg
                    [ svgAttributeTranslate
                        { x = strokeWidth
                        , y = fullHeight / 2
                        }
                    ]
                    [ identifierTextSvg.svg
                    ]
                , stackSvg
                    [ svgAttributeTranslate
                        { x = strokeWidth + identifierTextSvg.width + spaceWidth
                        , y = strokeWidth / 2
                        }
                    ]
                    [ argumentAsSvg.svg
                        |> Web.Dom.futureMap
                            (\futureArgumentUiState ->
                                { dragged = futureArgumentUiState.dragged
                                , relationUse =
                                    Just
                                        { identifier = relationUse.identifier
                                        , argument = futureArgumentUiState.value
                                        }
                                }
                            )
                    ]
                ]
        }


valueShapeSvg : ValueUiState -> SizedSvg future_
valueShapeSvg value =
    case value of
        ValueHole ->
            valueHoleShapeSvg

        Variable variableName ->
            variableShapeSvg variableName

        ValueLookup valueLookup ->
            valueLookupShapeSvg valueLookup


valueSvg :
    DragState
    -> ValueUiState
    -> SizedSvg { dragged : DragState, value : ValueUiState }
valueSvg dragState =
    \value ->
        case value of
            ValueHole ->
                valueHoleSvg dragState
                    |> sizedSvgFutureMap
                        (\holeFuture ->
                            { dragged = Nothing, value = holeFuture }
                        )

            Variable variableName ->
                variableName
                    |> variableSvg dragState
                    |> sizedSvgFutureMap
                        (\future ->
                            { dragged = future.dragged
                            , value =
                                case future.variable of
                                    Nothing ->
                                        ValueHole

                                    Just futureVariableName ->
                                        Variable futureVariableName
                            }
                        )

            ValueLookup valueLookup ->
                valueLookupSvg dragState valueLookup
                    |> sizedSvgFutureMap
                        (\future ->
                            { dragged = future.dragged
                            , value =
                                case future.valueLookup of
                                    Nothing ->
                                        ValueHole

                                    Just futureValueLookup ->
                                        ValueLookup futureValueLookup
                            }
                        )


valueLookupShapeSvg : FastDict.Dict String ValueUiState -> SizedSvg future_
valueLookupShapeSvg valueLookup =
    Debug.todo ""


valueLookupSvg :
    DragState
    -> FastDict.Dict String ValueUiState
    ->
        SizedSvg
            { dragged : DragState
            , valueLookup : Maybe (FastDict.Dict String ValueUiState)
            }
valueLookupSvg valueLookup =
    Debug.todo ""


verticalSvg :
    List (SizedSvg future)
    ->
        { width : Float
        , height : Float
        , svgs : List { y : Float, svg : Web.Dom.Node future }
        }
verticalSvg =
    \facts ->
        let
            factsAsSvgs :
                { combinedHeight : Float
                , widthMaximum : Float
                , svgsReverse : List { y : Float, svg : Web.Dom.Node future }
                }
            factsAsSvgs =
                facts
                    |> List.foldl
                        (\asSvg soFar ->
                            { combinedHeight = soFar.combinedHeight + asSvg.height
                            , widthMaximum = Basics.max soFar.widthMaximum asSvg.width
                            , svgsReverse =
                                soFar.svgsReverse
                                    |> (::)
                                        { svg = asSvg.svg, y = soFar.combinedHeight }
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



-- TODO apparently offset does not work in svg
-- so we currently just take the "grab center"
-- future alternative could be a custom g element that triggers a custom event
-- https://discourse.elm-lang.org/t/dispatching-custom-events-only-if-needed/2740/7


domListenToPointerDown : Web.Dom.Modifier (Result Json.Decode.Error { x : Float, y : Float })
domListenToPointerDown =
    Web.Dom.listenTo "pointerdown"
        |> Web.Dom.modifierFutureMap
            (\pointerDownEventJson ->
                case
                    pointerDownEventJson
                        |> Json.Decode.decodeValue
                            (Json.Decode.map2
                                (\x y -> { x = x, y = y })
                                (Json.Decode.field "clientX" Json.Decode.float)
                                (Json.Decode.field "clientY" Json.Decode.float)
                            )
                of
                    Err jsonDecodeError ->
                        Err jsonDecodeError

                    Ok pointer ->
                        { x = pointer.x
                        , y = pointer.y
                        }
                            |> Ok
            )


variableSvg : DragState -> (String -> SizedSvg { dragged : DragState, variable : Maybe String })
variableSvg dragState variableName =
    let
        shape : SizedSvg future_
        shape =
            variableShapeSvg variableName
    in
    { width = shape.width
    , height = shape.height
    , svg =
        stackSvg
            [ domListenToPointerDown
                |> Web.Dom.modifierFutureMap
                    (\pointerDownEventPosition ->
                        case pointerDownEventPosition of
                            Err _ ->
                                { dragged = dragState, variable = Just variableName }

                            Ok pointer ->
                                { dragged =
                                    Just
                                        { x = pointer.x
                                        , y = pointer.y
                                        , offsetX = -fontSize
                                        , offsetY = -fontSize
                                        , thing = DraggedVariable variableName
                                        }
                                , variable = Nothing
                                }
                    )
            ]
            [ shape.svg ]
    }


variableShapeSvg : String -> SizedSvg future_
variableShapeSvg =
    \variableName ->
        let
            strokeWidth : Float
            strokeWidth =
                fontSize * 2

            color : Color
            color =
                Color.rgb 0 0.11 0.21

            nameSvg : SizedSvg future_
            nameSvg =
                unselectableTextSvg variableName

            shapeSvg : SizedSvg future_
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
        { width = shapeSvg.width + strokeWidth
        , height = shapeSvg.height + strokeWidth
        , svg =
            stackSvg
                []
                [ shapeSvg.svg
                , stackSvg
                    [ svgAttributeTranslate
                        { x = strokeWidth / 2
                        , y = strokeWidth / 2
                        }
                    ]
                    [ nameSvg.svg ]
                ]
        }


missingThingColor : Color
missingThingColor =
    Color.rgba 0 0 0 0.48


valueHoleSvg : DragState -> SizedSvg ValueUiState
valueHoleSvg dragState =
    let
        shapeSvg : SizedSvg future_
        shapeSvg =
            valueHoleShapeSvg
    in
    { width = shapeSvg.width
    , height = shapeSvg.height
    , svg =
        stackSvg
            [ case dragState of
                Nothing ->
                    Web.Dom.modifierNone

                Just stateDragged ->
                    case stateDragged.thing of
                        DraggedFact _ ->
                            Web.Dom.modifierNone

                        DraggedValueLookup draggedValueLookup ->
                            Web.Dom.listenTo "pointerup"
                                |> Web.Dom.modifierFutureMap
                                    (\_ -> ValueLookup draggedValueLookup)

                        DraggedVariable draggedVariable ->
                            Web.Dom.listenTo "pointerup"
                                |> Web.Dom.modifierFutureMap
                                    (\_ -> Variable draggedVariable)
            ]
            [ shapeSvg.svg ]
    }


valueHoleShapeSvg : SizedSvg future_
valueHoleShapeSvg =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize * 2

        nameSvg : SizedSvg future_
        nameSvg =
            unselectableTextSvg "drag a value here"

        shapeSvg : SizedSvg future_
        shapeSvg =
            polygonSvg
                [ svgAttributeFillUniform missingThingColor
                , Web.Dom.attribute "stroke" (missingThingColor |> Color.toCssString)
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
        stackSvg
            []
            [ shapeSvg.svg
            , stackSvg
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
