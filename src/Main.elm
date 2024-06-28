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
            { parameter : ValueUiState
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
    | DraggedValueLookup ValueLookupUiState
    | DraggedVariable String


type alias ValueLookupUiState =
    List { key : String, value : ValueUiState }


type FactUiState
    = RelationUse { identifier : String, argument : ValueUiState }
    | Equal { a : ValueUiState, b : ValueUiState }
    | Not (Maybe FactUiState)
    | All (List FactUiState)
    | Any (List FactUiState)


type ValueUiState
    = ValueHole
    | Variable String
    | ValueLookup ValueLookupUiState


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
                        |> List.LocalExtra.allJustMap
                            (\entry ->
                                Maybe.map (\entryValue -> ( entry.key, entryValue ))
                                    (entry.value |> valueUiStateToLau)
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
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


initialState : State
initialState =
    { windowWidth = 1920
    , windowHeight = 1080
    , dragged = Nothing
    , relationDefinitions =
        FastDict.singleton "main"
            { parameter =
                ValueLookup
                    [ { key = "state", value = Variable "state" }
                    , { key = "interface", value = Variable "interface" }
                    ]
            , equivalentFact =
                Any
                    [ All
                        [ Not
                            (RelationUse
                                { identifier = "is greater or equal to 0"
                                , argument = Variable "state"
                                }
                                |> Just
                            )
                        , Equal
                            { a = Variable "Interface"
                            , b =
                                ValueLookup
                                    [ { key = "svg render"
                                      , value =
                                            ValueLookup
                                                [ { key = "circle"
                                                  , value =
                                                        ValueLookup
                                                            [ { key = "radius"
                                                              , value =
                                                                    ValueLookup
                                                                        [ { key = "50", value = ValueLookup [] } ]
                                                              }
                                                            , { key = "y"
                                                              , value =
                                                                    ValueLookup
                                                                        [ { key = "50", value = ValueLookup [] } ]
                                                              }
                                                            ]
                                                  }
                                                ]
                                      }
                                    ]
                            }
                        ]
                    , All
                        [ RelationUse
                            { identifier = "is less than 10"
                            , argument = Variable "state"
                            }
                        , Equal
                            { a = Variable "interface"
                            , b = ValueHole
                            }
                        ]
                    ]
                    |> Just
            }
    , strayThings = []
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
                , domModifierFillUniform (Color.rgb 0 0 0)
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
                                    , parameter = definition.parameter
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
            , domModifierFillUniform (Color.rgb 1 1 1)
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
        , parameter : ValueUiState
        , equivalentFact : Maybe FactUiState
        }
    ->
        SizedSvg
            { dragged : DragState
            , relationDefinition :
                { parameter : ValueUiState
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
                + parameterSvg.width
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

        parameterSvg : SizedSvg { dragged : DragState, value : ValueUiState }
        parameterSvg =
            definition.parameter |> valueSvg dragState

        equivalentFactSvg : SizedSvg { dragged : DragState, fact : Maybe FactUiState }
        equivalentFactSvg =
            case definition.equivalentFact of
                Nothing ->
                    factInsertHoleSvg dragState
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
            List.maximum
                [ nameSvg.height + strokeWidth
                , variableSvgHeight
                , equalsTextSvg.height + strokeWidth
                ]
                |> Maybe.withDefault 0

        fullHeight : Float
        fullHeight =
            Basics.max headerHeight equivalentFactSvg.height

        shapeSvg : SizedSvg future_
        shapeSvg =
            polygonSvg
                [ domModifierFillUniform color
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
                    , y = strokeWidth / 2 + (variableSvgHeight - nameSvg.height) / 2
                    }
                ]
                [ nameSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth + nameSvg.width + spaceWidth
                    , y = 0
                    }
                ]
                [ parameterSvg.svg
                    |> Web.Dom.futureMap
                        (\futureArgumentUiState ->
                            { dragged = futureArgumentUiState.dragged
                            , relationDefinition =
                                { equivalentFact = definition.equivalentFact
                                , parameter = futureArgumentUiState.value
                                }
                            }
                        )
                ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth + nameSvg.width + spaceWidth + parameterSvg.width + spaceWidth
                    , y = strokeWidth / 2 + (variableSvgHeight - equalsTextSvg.height) / 2
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
                                , parameter = definition.parameter
                                }
                            }
                        )
                ]
            ]
    }


factNotShapeSvg : Maybe FactUiState -> SizedSvg future_
factNotShapeSvg maybeFactInverse =
    factNotSvgWithInteractivity
        { maybeFactInverse = maybeFactInverse
        , factInverseSvg =
            case maybeFactInverse of
                Nothing ->
                    factInsertHoleShapeSvg

                Just equivalentFact ->
                    factShapeSvg equivalentFact
        , shapeListenModifier = Web.Dom.modifierNone
        }


equalsShapeSvg : { a : ValueUiState, b : ValueUiState } -> SizedSvg future_
equalsShapeSvg toEquate =
    factEqualsSvgWithInteractivity
        { valueASvg = toEquate.a |> valueShapeSvg
        , valueBSvg = toEquate.b |> valueShapeSvg
        , shapeEventListenModifier = Web.Dom.modifierNone
        }


relationUseShapeSvg : { identifier : String, argument : ValueUiState } -> SizedSvg future_
relationUseShapeSvg relationUse =
    relationUseSvgWithInteractivity
        { shapeEventListenModifier = Web.Dom.modifierNone
        , identifier = relationUse.identifier
        , argumentAsSvg =
            relationUse.argument |> valueShapeSvg
        }


factShapeSvg : FactUiState -> SizedSvg future_
factShapeSvg fact =
    case fact of
        All parts ->
            factAllShapeSvg parts

        Any branches ->
            factAnyShapeSvg branches

        Not maybeFactInverse ->
            factNotShapeSvg maybeFactInverse

        Equal toEquate ->
            equalsShapeSvg toEquate

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

        Not maybeFactInverse ->
            factNotSvgWithInteractivity
                { factInverseSvg =
                    case maybeFactInverse of
                        Nothing ->
                            factInsertHoleSvg dragState
                                |> sizedSvgFutureMap
                                    (\dropped ->
                                        { dragged = Nothing, fact = Just (Not (Just dropped)) }
                                    )

                        Just equivalentFact ->
                            factSvg dragState equivalentFact
                                |> sizedSvgFutureMap
                                    (\futureFactInverse ->
                                        { fact = Just (Not futureFactInverse.fact)
                                        , dragged = futureFactInverse.dragged
                                        }
                                    )
                , maybeFactInverse = maybeFactInverse
                , shapeListenModifier =
                    domListenToPointerDown
                        |> Web.Dom.modifierFutureMap
                            (\pointerDownEventPosition ->
                                case pointerDownEventPosition of
                                    Err _ ->
                                        { dragged = dragState, fact = Just (Not maybeFactInverse) }

                                    Ok pointer ->
                                        { dragged =
                                            Just
                                                { x = pointer.x
                                                , y = pointer.y
                                                , offsetX = -fontSize
                                                , offsetY = -fontSize
                                                , thing = DraggedFact (Not maybeFactInverse)
                                                }
                                        , fact = Nothing
                                        }
                            )
                }

        Equal toEquate ->
            factEqualsSvgWithInteractivity
                { valueASvg =
                    toEquate.a
                        |> valueSvg dragState
                        |> sizedSvgFutureMap
                            (\futureA ->
                                { dragged = futureA.dragged
                                , fact = Just (Equal { a = futureA.value, b = toEquate.b })
                                }
                            )
                , valueBSvg =
                    toEquate.b
                        |> valueSvg dragState
                        |> sizedSvgFutureMap
                            (\futureB ->
                                { dragged = futureB.dragged
                                , fact = Just (Equal { a = toEquate.a, b = futureB.value })
                                }
                            )
                , shapeEventListenModifier =
                    domListenToPointerDown
                        |> Web.Dom.modifierFutureMap
                            (\pointerDownEventPosition ->
                                case pointerDownEventPosition of
                                    Err _ ->
                                        { dragged = dragState, fact = Just (Equal toEquate) }

                                    Ok pointer ->
                                        { dragged =
                                            Just
                                                { x = pointer.x
                                                , y = pointer.y
                                                , offsetX = -fontSize
                                                , offsetY = -fontSize
                                                , thing = DraggedFact (Equal toEquate)
                                                }
                                        , fact = Nothing
                                        }
                            )
                }

        RelationUse relationUse ->
            relationUseSvgWithInteractivity
                { shapeEventListenModifier =
                    domListenToPointerDown
                        |> Web.Dom.modifierFutureMap
                            (\pointerDownEventPosition ->
                                case pointerDownEventPosition of
                                    Err _ ->
                                        { dragged = dragState
                                        , fact = Just (RelationUse relationUse)
                                        }

                                    Ok pointer ->
                                        { dragged =
                                            Just
                                                { x = pointer.x
                                                , y = pointer.y
                                                , offsetX = -fontSize
                                                , offsetY = -fontSize
                                                , thing = DraggedFact (RelationUse relationUse)
                                                }
                                        , fact = Nothing
                                        }
                            )
                , identifier = relationUse.identifier
                , argumentAsSvg =
                    relationUse.argument
                        |> valueSvg dragState
                        |> sizedSvgFutureMap
                            (\futureArgumentUiState ->
                                { dragged = futureArgumentUiState.dragged
                                , fact =
                                    Just
                                        (RelationUse
                                            { identifier = relationUse.identifier
                                            , argument = futureArgumentUiState.value
                                            }
                                        )
                                }
                            )
                }


factAllShapeSvg : List FactUiState -> SizedSvg future_
factAllShapeSvg parts =
    blockVerticalFactListShapeSvg
        { name = "all"
        , color = Color.rgb 0 0.14 0
        , elements = parts
        }


factAllSvg :
    DragState
    -> List FactUiState
    -> SizedSvg { dragged : DragState, parts : Maybe (List FactUiState) }
factAllSvg dragState parts =
    blockVerticalFactListSvg
        { name = "all"
        , fact = All parts
        , dragState = dragState
        , elements = parts
        , color = Color.rgb 0 0.14 0
        }
        |> sizedSvgFutureMap
            (\future ->
                { dragged = future.dragged
                , parts = future.elements
                }
            )


blockVerticalFactListSvg :
    { dragState : DragState
    , elements : List FactUiState
    , color : Color
    , name : String
    , fact : FactUiState
    }
    -> SizedSvg { dragged : DragState, elements : Maybe (List FactUiState) }
blockVerticalFactListSvg { dragState, elements, color, name, fact } =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        sideWidth : Float
        sideWidth =
            strokeWidth

        insertHoles : List (SizedSvg { dragged : DragState, elements : List FactUiState })
        insertHoles =
            case elements of
                [] ->
                    factInsertHoleSvg dragState
                        |> sizedSvgFutureMap
                            (\futureUiState ->
                                { dragged = Nothing
                                , elements = [ futureUiState ]
                                }
                            )
                        |> List.singleton

                element0 :: element1Up ->
                    case dragState of
                        Nothing ->
                            []

                        Just dragged ->
                            case dragged.thing of
                                DraggedValueLookup _ ->
                                    []

                                DraggedVariable _ ->
                                    []

                                DraggedFact _ ->
                                    List.range 0 ((element0 :: element1Up) |> List.length)
                                        |> List.map
                                            (\insertIndex ->
                                                factInsertHoleSvg (Just dragged)
                                                    |> sizedSvgFutureMap
                                                        (\futureUiState ->
                                                            { dragged = Nothing
                                                            , elements =
                                                                (element0 :: element1Up)
                                                                    |> List.LocalExtra.insertElementAtIndex insertIndex
                                                                        futureUiState
                                                            }
                                                        )
                                            )

        elementsAndInsertHolesSvg :
            { width : Float
            , height : Float
            , svgs :
                List
                    { y : Float
                    , width : Float
                    , height : Float
                    , svg : Web.Dom.Node { dragged : DragState, elements : List FactUiState }
                    }
            }
        elementsAndInsertHolesSvg =
            let
                elementsSvgs : List (SizedSvg { dragged : DragState, elements : List FactUiState })
                elementsSvgs =
                    elements
                        |> List.indexedMap
                            (\partIndex part ->
                                factSvg dragState part
                                    |> sizedSvgFutureMap
                                        (\partFutureUiState ->
                                            { dragged = partFutureUiState.dragged
                                            , elements =
                                                case partFutureUiState.fact of
                                                    Nothing ->
                                                        elements
                                                            |> List.LocalExtra.removeElementAtIndex partIndex

                                                    Just futurePartFact ->
                                                        elements
                                                            |> List.LocalExtra.elementAtIndexAlter partIndex
                                                                (\_ -> futurePartFact)
                                            }
                                        )
                            )
            in
            List.LocalExtra.interweave insertHoles elementsSvgs
                |> verticalSvg

        fullWidth : Float
        fullWidth =
            sideWidth
                + Basics.max elementsAndInsertHolesSvg.width
                    (strokeWidth + allStringSvg.width + strokeWidth)

        allStringSvg : SizedSvg future_
        allStringSvg =
            unselectableTextSvg name

        headerHeight : Float
        headerHeight =
            fontSize + strokeWidth

        fullHeight : Float
        fullHeight =
            headerHeight + elementsAndInsertHolesSvg.height + sideWidth

        shapeSvg : SizedSvg { dragged : DragState, elements : Maybe (List FactUiState) }
        shapeSvg =
            polygonSvg
                [ domModifierFillUniform color
                , domListenToPointerDown
                    |> Web.Dom.modifierFutureMap
                        (\pointerDownEventPosition ->
                            case pointerDownEventPosition of
                                Err _ ->
                                    { dragged = dragState, elements = Just elements }

                                Ok pointer ->
                                    { dragged =
                                        Just
                                            { x = pointer.x
                                            , y = pointer.y
                                            , offsetX = -fontSize
                                            , offsetY = -fontSize
                                            , thing = DraggedFact fact
                                            }
                                    , elements = Nothing
                                    }
                        )
                ]
                ([ ( sideWidth + strokeWidth, headerHeight )
                 , ( fullWidth, headerHeight )
                 , ( fullWidth, 0 )
                 , ( strokeWidth, 0 )
                 , ( 0, strokeWidth )
                 , ( 0, headerHeight + elementsAndInsertHolesSvg.height )
                 , ( strokeWidth, fullHeight )
                 , ( fullWidth, fullHeight )
                 , ( fullWidth, headerHeight + elementsAndInsertHolesSvg.height )
                 , ( sideWidth + strokeWidth, headerHeight + elementsAndInsertHolesSvg.height )
                 ]
                    ++ (case insertHoles of
                            _ :: _ ->
                                elementsAndInsertHolesSvg.svgs
                                    |> List.indexedMap Tuple.pair
                                    |> List.concatMap
                                        (\( index, sub ) ->
                                            if (index |> Basics.remainderBy 2) == 1 then
                                                [ ( sideWidth + strokeWidth, headerHeight + sub.y )
                                                , ( sideWidth, headerHeight + sub.y + strokeWidth )
                                                , ( sideWidth, headerHeight + sub.y + sub.height - strokeWidth )
                                                , ( sideWidth + strokeWidth, headerHeight + sub.y + sub.height )
                                                ]

                                            else
                                                [ ( sideWidth + sub.width, headerHeight + sub.y )
                                                , ( sideWidth + sub.width, headerHeight + sub.y + sub.height )
                                                ]
                                        )
                                    |> List.reverse

                            _ ->
                                ( sideWidth, headerHeight + elementsAndInsertHolesSvg.height - strokeWidth )
                                    :: (elementsAndInsertHolesSvg.svgs
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
                [ elementsAndInsertHolesSvg.svgs
                    |> List.map
                        (\elementAsSvg ->
                            elementAsSvg
                                |> .svg
                                |> Web.Dom.futureMap
                                    (\future ->
                                        { dragged = future.dragged, elements = Just future.elements }
                                    )
                                |> List.singleton
                                |> stackSvg
                                    [ svgAttributeTranslate { x = 0, y = elementAsSvg.y }
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
    blockVerticalFactListShapeSvg
        { name = "any"
        , color = Color.rgb 0.2 0 0.2
        , elements = branches
        }


blockVerticalFactListShapeSvg :
    { elements : List FactUiState, name : String, color : Color }
    -> SizedSvg future_
blockVerticalFactListShapeSvg config =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        sideWidth : Float
        sideWidth =
            strokeWidth

        branchesSvg :
            Result
                (SizedSvg future_)
                { width : Float
                , height : Float
                , svgs :
                    List
                        { y : Float
                        , width : Float
                        , height : Float
                        , svg : Web.Dom.Node future_
                        }
                }
        branchesSvg =
            case config.elements of
                [] ->
                    Err factInsertHoleShapeSvg

                branch0 :: branch1Up ->
                    (branch0 :: branch1Up)
                        |> List.map factShapeSvg
                        |> verticalSvg
                        |> Ok

        anyTextSvg : SizedSvg future_
        anyTextSvg =
            unselectableTextSvg config.name

        headerHeight : Float
        headerHeight =
            fontSize + strokeWidth

        fullWidth : Float
        fullWidth =
            sideWidth
                + Basics.max
                    contentWidth
                    (strokeWidth + anyTextSvg.width + strokeWidth)

        contentWidth : Float
        contentWidth =
            case branchesSvg of
                Ok branchesSvgs ->
                    branchesSvgs.width

                Err missing ->
                    missing.width

        contentHeight : Float
        contentHeight =
            case branchesSvg of
                Ok branchesSvgs ->
                    branchesSvgs.height

                Err missing ->
                    missing.height

        fullHeight : Float
        fullHeight =
            headerHeight
                + contentHeight
                + sideWidth

        shapeSvg : SizedSvg future_
        shapeSvg =
            polygonSvg
                [ domModifierFillUniform config.color
                ]
                ([ ( fullWidth, headerHeight )
                 , ( fullWidth, 0 )
                 , ( sideWidth, 0 )
                 , ( 0, strokeWidth )
                 , ( 0, headerHeight + contentHeight )
                 , ( sideWidth, fullHeight )
                 , ( fullWidth, fullHeight )
                 , ( fullWidth, headerHeight + contentHeight )
                 ]
                    ++ (case branchesSvg of
                            Err _ ->
                                []

                            Ok branchesSvgs ->
                                ( sideWidth + strokeWidth, headerHeight + contentHeight )
                                    :: ( sideWidth, headerHeight + contentHeight - strokeWidth )
                                    :: (branchesSvgs.svgs
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
                                       , ( sideWidth + strokeWidth, headerHeight )
                                       ]
                       )
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
                [ case branchesSvg of
                    Err missing ->
                        missing.svg

                    Ok branchesSvgs ->
                        branchesSvgs
                            |> .svgs
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
    blockVerticalFactListSvg
        { name = "any"
        , fact = Any branches
        , dragState = dragState
        , elements = branches
        , color = Color.rgb 0.2 0 0.2
        }
        |> sizedSvgFutureMap
            (\future ->
                { dragged = future.dragged
                , branches = future.elements
                }
            )


factNotSvgWithInteractivity :
    { shapeListenModifier : Web.Dom.Modifier future
    , factInverseSvg : SizedSvg future
    , maybeFactInverse : Maybe FactUiState
    }
    -> SizedSvg future
factNotSvgWithInteractivity parts =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        sideWidth : Float
        sideWidth =
            strokeWidth

        headerWidth : Float
        headerWidth =
            strokeWidth + notTextSvg.width + strokeWidth

        fullWidth : Float
        fullWidth =
            headerWidth + parts.factInverseSvg.width

        notTextSvg : SizedSvg future_
        notTextSvg =
            unselectableTextSvg "not"

        fullHeight : Float
        fullHeight =
            Basics.max (fontSize + strokeWidth)
                parts.factInverseSvg.height

        shapeSvg : SizedSvg future
        shapeSvg =
            polygonSvg
                [ domModifierFillUniform (Color.rgb 0.2 0.04 0)
                , parts.shapeListenModifier
                ]
                (case parts.maybeFactInverse of
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
                [ parts.factInverseSvg.svg ]
            ]
    }


factInsertHoleSvg : DragState -> SizedSvg FactUiState
factInsertHoleSvg dragState =
    let
        shapeSvg : SizedSvg future_
        shapeSvg =
            factInsertHoleShapeSvg
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


factInsertHoleShapeSvg : SizedSvg future_
factInsertHoleShapeSvg =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        fullWidth : Float
        fullWidth =
            strokeWidth
                + (strokeWidth * 5)
                + strokeWidth

        fullHeight : Float
        fullHeight =
            strokeWidth + strokeWidth
    in
    polygonSvg
        [ domModifierFillUniform missingThingColor
        ]
        [ ( 0, strokeWidth )
        , ( strokeWidth, 0 )
        , ( fullWidth, 0 )
        , ( fullWidth, fullHeight )
        , ( strokeWidth, fullHeight )
        , ( 0, fullHeight - strokeWidth )
        ]


factEqualsSvgWithInteractivity :
    { shapeEventListenModifier : Web.Dom.Modifier future
    , valueASvg : SizedSvg future
    , valueBSvg : SizedSvg future
    }
    -> SizedSvg future
factEqualsSvgWithInteractivity parts =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        fullWidth : Float
        fullWidth =
            strokeWidth
                + parts.valueASvg.width
                + spaceWidth
                + equalsTextSvg.width
                + spaceWidth
                + parts.valueBSvg.width
                + strokeWidth

        spaceWidth : Float
        spaceWidth =
            fontWidth / 2

        color : Color
        color =
            Color.rgb 0.2 0.2 0

        equalsTextSvg : SizedSvg future_
        equalsTextSvg =
            unselectableTextSvg "="

        fullHeight : Float
        fullHeight =
            Basics.max
                parts.valueASvg.height
                (Basics.max (equalsTextSvg.height + strokeWidth)
                    parts.valueBSvg.height
                )

        shapeSvg : SizedSvg future
        shapeSvg =
            polygonSvg
                [ domModifierFillUniform color
                , parts.shapeEventListenModifier
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
                    , y = (fullHeight - parts.valueASvg.height) / 2
                    }
                ]
                [ parts.valueASvg.svg ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth + parts.valueASvg.width + spaceWidth
                    , y = fullHeight / 2
                    }
                ]
                [ equalsTextSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = strokeWidth + parts.valueASvg.width + spaceWidth + equalsTextSvg.width + spaceWidth
                    , y = (fullHeight - parts.valueBSvg.height) / 2
                    }
                ]
                [ parts.valueBSvg.svg ]
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


relationUseSvgWithInteractivity :
    { shapeEventListenModifier : Web.Dom.Modifier future
    , identifier : String
    , argumentAsSvg : SizedSvg future
    }
    -> SizedSvg future
relationUseSvgWithInteractivity parts =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        fullWidth : Float
        fullWidth =
            strokeWidth
                + identifierTextSvg.width
                + spaceWidth
                + parts.argumentAsSvg.width
                + strokeWidth

        spaceWidth : Float
        spaceWidth =
            fontWidth / 2

        color : Color
        color =
            Color.rgb 0.2 0.2 0

        identifierTextSvg : SizedSvg future_
        identifierTextSvg =
            unselectableTextSvg parts.identifier

        fullHeight : Float
        fullHeight =
            Basics.max (identifierTextSvg.height + strokeWidth)
                parts.argumentAsSvg.height

        shapeSvg : SizedSvg future
        shapeSvg =
            polygonSvg
                [ domModifierFillUniform color
                , parts.shapeEventListenModifier
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
                    , y = 0
                    }
                ]
                [ parts.argumentAsSvg.svg ]
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


valueLookupColor : Color
valueLookupColor =
    Color.rgb 0 0.1 0.21


circleSvg : { radius : Float } -> List (Web.Dom.Modifier future) -> SizedSvg future
circleSvg geometry modifiers =
    { width = geometry.radius * 2
    , height = geometry.radius * 2
    , svg =
        Web.Svg.element "circle"
            (Web.Dom.attribute "r" (geometry.radius |> String.fromFloat)
                :: modifiers
            )
            []
    }


valueLookupSvg :
    DragState
    -> ValueLookupUiState
    ->
        SizedSvg
            { dragged : DragState
            , valueLookup : Maybe ValueLookupUiState
            }
valueLookupSvg dragState valueLookup =
    valueLookupSvgWithInteractivity
        { listenToDragStart =
            domListenToPointerDown
                |> Web.Dom.modifierFutureMap
                    (\pointerDownEventPosition ->
                        case pointerDownEventPosition of
                            Err _ ->
                                { dragged = dragState
                                , valueLookup = Just valueLookup
                                }

                            Ok pointer ->
                                { dragged =
                                    Just
                                        { x = pointer.x
                                        , y = pointer.y
                                        , offsetX = -fontSize
                                        , offsetY = -fontSize
                                        , thing = DraggedValueLookup valueLookup
                                        }
                                , valueLookup = Nothing
                                }
                    )
        }
        (valueLookup
            |> List.indexedMap
                (\entryIndex entry ->
                    { key = entry.key
                    , value =
                        valueSvg dragState entry.value
                            |> sizedSvgFutureMap
                                (\entryValueFuture ->
                                    { dragged = entryValueFuture.dragged
                                    , valueLookup =
                                        valueLookup
                                            |> List.LocalExtra.elementAtIndexAlter entryIndex
                                                (\_ ->
                                                    { key = entry.key
                                                    , value = entryValueFuture.value
                                                    }
                                                )
                                            |> Just
                                    }
                                )
                    }
                )
        )


valueLookupShapeSvg : ValueLookupUiState -> SizedSvg future_
valueLookupShapeSvg valueLookup =
    valueLookupSvgWithInteractivity
        { listenToDragStart = Web.Dom.modifierNone }
        (valueLookup
            |> List.indexedMap
                (\entryIndex entry ->
                    { key = entry.key
                    , value = valueShapeSvg entry.value
                    }
                )
        )


valueLookupSvgWithInteractivity :
    { listenToDragStart : Web.Dom.Modifier future }
    ->
        List
            { key : String
            , value : SizedSvg future
            }
    -> SizedSvg future
valueLookupSvgWithInteractivity interactivity valueLookup =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize * 2
    in
    case valueLookup of
        [] ->
            circleSvg { radius = strokeWidth / 2 }
                [ interactivity.listenToDragStart
                , domModifierFillUniform valueLookupColor
                , svgAttributeTranslate { x = strokeWidth / 2, y = strokeWidth / 2 }
                ]

        entry0 :: entry1Up ->
            let
                entrySvgs :
                    { width : Float
                    , height : Float
                    , svgs :
                        List
                            { y : Float
                            , width : Float
                            , height : Float
                            , svg : Web.Dom.Node future
                            }
                    }
                entrySvgs =
                    valueLookup
                        |> List.indexedMap
                            (\entryIndex entry ->
                                let
                                    entryValueSvg : SizedSvg future
                                    entryValueSvg =
                                        entry.value

                                    entryNameSvg : SizedSvg future_
                                    entryNameSvg =
                                        unselectableTextSvg entry.key

                                    entryFullHeight : Float
                                    entryFullHeight =
                                        Basics.max (entryNameSvg.height + fontSize)
                                            entryValueSvg.height
                                in
                                { width = entryNameSvg.width + fontWidth + entryValueSvg.width
                                , height = entryFullHeight
                                , svg =
                                    stackSvg []
                                        [ stackSvg
                                            [ svgAttributeTranslate
                                                { x = 0
                                                , y = fontSize / 2 + (entryFullHeight - fontSize) / 2
                                                }
                                            , Web.Dom.style "font-style" "italic"
                                            ]
                                            [ entryNameSvg.svg ]
                                        , stackSvg
                                            [ svgAttributeTranslate
                                                { x = entryNameSvg.width + fontWidth
                                                , y = 0
                                                }
                                            ]
                                            [ entryValueSvg.svg ]
                                        ]
                                }
                            )
                        |> verticalSvg

                shapeSvg : SizedSvg future
                shapeSvg =
                    polygonSvg
                        [ interactivity.listenToDragStart
                        , domModifierFillUniform valueLookupColor
                        , Web.Dom.attribute "stroke" (valueLookupColor |> Color.toCssString)
                        , Web.Dom.attribute "stroke-width" (strokeWidth |> String.fromFloat)
                        , Web.Dom.attribute "stroke-linejoin" "round"
                        ]
                        [ ( strokeWidth / 2, strokeWidth / 2 )
                        , ( fullWidth - strokeWidth / 2, strokeWidth / 2 )
                        , ( fullWidth - strokeWidth / 2, fullHeight - strokeWidth / 2 )
                        , ( strokeWidth / 2, fullHeight - strokeWidth / 2 )
                        ]

                fullHeight : Float
                fullHeight =
                    entrySvgs.height

                fullWidth : Float
                fullWidth =
                    strokeWidth / 2 + entrySvgs.width
            in
            { width = fullWidth
            , height = fullHeight
            , svg =
                stackSvg
                    []
                    [ shapeSvg.svg
                    , entrySvgs.svgs
                        |> List.map
                            (\entryAsSvg ->
                                entryAsSvg.svg
                                    |> List.singleton
                                    |> stackSvg [ svgAttributeTranslate { x = 0, y = entryAsSvg.y } ]
                            )
                        |> stackSvg
                            [ svgAttributeTranslate
                                { x = strokeWidth / 2, y = 0 }
                            ]
                    ]
            }


verticalSvg :
    List (SizedSvg future)
    ->
        { width : Float
        , height : Float
        , svgs :
            List
                { y : Float
                , width : Float
                , height : Float
                , svg : Web.Dom.Node future
                }
        }
verticalSvg =
    \facts ->
        let
            factsAsSvgs :
                { combinedHeight : Float
                , widthMaximum : Float
                , svgsReverse :
                    List
                        { y : Float
                        , width : Float
                        , height : Float
                        , svg : Web.Dom.Node future
                        }
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
                                        { svg = asSvg.svg
                                        , width = asSvg.width
                                        , height = asSvg.height
                                        , y = soFar.combinedHeight
                                        }
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


variableColor : Color
variableColor =
    Color.rgb 0.3 0.1 0


variableShapeSvg : String -> SizedSvg future_
variableShapeSvg =
    \variableName ->
        let
            strokeWidth : Float
            strokeWidth =
                fontSize * 2

            nameSvg : SizedSvg future_
            nameSvg =
                unselectableTextSvg variableName

            shapeSvg : SizedSvg future_
            shapeSvg =
                polygonSvg
                    [ domModifierFillUniform variableColor
                    , domModifierFillUniform variableColor
                    , Web.Dom.attribute "stroke" (variableColor |> Color.toCssString)
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
    Color.rgba 0 0 0 0.54


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

        fullWidth =
            fontSize * 5 + strokeWidth

        shapeSvg : SizedSvg future_
        shapeSvg =
            polygonSvg
                [ domModifierFillUniform missingThingColor
                , Web.Dom.attribute "stroke" (missingThingColor |> Color.toCssString)
                , Web.Dom.attribute "stroke-width" (strokeWidth |> String.fromFloat)
                , Web.Dom.attribute "stroke-linejoin" "round"
                ]
                [ ( strokeWidth / 2, strokeWidth / 2 )
                , ( fullWidth - strokeWidth / 2, strokeWidth / 2 )
                , ( fullWidth - strokeWidth / 2, fontSize )
                , ( strokeWidth / 2, fontSize )
                ]
    in
    { width = fullWidth
    , height = fontSize + fontSize
    , svg = shapeSvg.svg
    }


domModifierFillUniform : Color -> Web.Dom.Modifier future_
domModifierFillUniform color =
    Web.Dom.attribute "fill" (color |> Color.toCssString)


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
