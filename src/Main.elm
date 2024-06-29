port module Main exposing (State, main)

import Angle exposing (Angle)
import Arc2d
import Color exposing (Color)
import FastDict
import Json.Decode
import Json.Encode
import Lau
import Length
import List.LocalExtra
import Parameter1d
import Point2d
import Quantity
import Svg.PathD
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
            { parameter : Maybe ValueUiState
            , equivalentFact : Maybe FactUiState
            }
    , strayThings : List { x : Float, y : Float, block : BlockUiState }
    , dragged : DragState
    }


type alias DragState =
    Maybe
        { x : Float
        , y : Float
        , offsetX : Float
        , offsetY : Float
        , block : BlockUiState
        }


type BlockUiState
    = BlockFact FactUiState
    | BlockValue ValueUiState


type alias ValueLookupUiState =
    List { key : String, value : Maybe ValueUiState }


type FactUiState
    = RelationUse { identifier : String, argument : Maybe ValueUiState }
    | Equal { a : Maybe ValueUiState, b : Maybe ValueUiState }
    | Not (Maybe FactUiState)
    | All (List FactUiState)
    | Any (List FactUiState)


type ValueUiState
    = Variable String
    | ValueLookup ValueLookupUiState


valueUiStateToLau : ValueUiState -> Maybe (Lau.ValueWithVariableName String)
valueUiStateToLau =
    \valueWithHoles ->
        case valueWithHoles of
            Variable variableName ->
                Lau.Variable variableName |> Just

            ValueLookup entriesWithHoles ->
                Maybe.map Lau.ValueLookup
                    (entriesWithHoles
                        |> List.LocalExtra.allJustMap
                            (\entry ->
                                Maybe.map (\entryValue -> ( entry.key, entryValue ))
                                    (entry.value |> Maybe.andThen valueUiStateToLau)
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
                    (relation.argument |> Maybe.andThen valueUiStateToLau)

            Equal toEquateWithHoles ->
                Maybe.map2 (\toEquateA toEquateB -> Lau.Equal [ toEquateA, toEquateB ])
                    (toEquateWithHoles.a |> Maybe.andThen valueUiStateToLau)
                    (toEquateWithHoles.b |> Maybe.andThen valueUiStateToLau)

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
                    [ { key = "state", value = Variable "state" |> Just }
                    , { key = "interface", value = Variable "interface" |> Just }
                    ]
                    |> Just
            , equivalentFact =
                Any
                    [ All
                        [ Not
                            (RelationUse
                                { identifier = "is greater or equal to 0"
                                , argument = Variable "state" |> Just
                                }
                                |> Just
                            )
                        , Equal
                            { a = Variable "Interface" |> Just
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
                                                                        [ { key = "50", value = ValueLookup [] |> Just } ]
                                                                        |> Just
                                                              }
                                                            , { key = "y"
                                                              , value =
                                                                    ValueLookup
                                                                        [ { key = "50", value = ValueLookup [] |> Just } ]
                                                                        |> Just
                                                              }
                                                            ]
                                                            |> Just
                                                  }
                                                ]
                                                |> Just
                                      }
                                    ]
                                    |> Just
                            }
                        ]
                    , All
                        [ RelationUse
                            { identifier = "is less than 10"
                            , argument = Variable "state" |> Just
                            }
                        , Equal
                            { a = Variable "interface" |> Just
                            , b = Nothing
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
                                                , block = stateDragged.block
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
                                                    , block = dragged.block
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
                            strayThingSvg : Web.Dom.Node { dragged : DragState, block : Maybe BlockUiState }
                            strayThingSvg =
                                case strayThing.block of
                                    BlockValue droppedValue ->
                                        valueSvg state.dragged droppedValue
                                            |> .svg
                                            |> Web.Dom.futureMap
                                                (\future ->
                                                    { dragged = future.dragged
                                                    , block = Maybe.map BlockValue future.value
                                                    }
                                                )

                                    BlockFact fact ->
                                        factSvg state.dragged fact
                                            |> .svg
                                            |> Web.Dom.futureMap
                                                (\future ->
                                                    { dragged = future.dragged
                                                    , block = Maybe.map BlockFact future.fact
                                                    }
                                                )
                        in
                        strayThingSvg
                            |> Web.Dom.futureMap
                                (\future ->
                                    { state
                                        | dragged = future.dragged
                                        , strayThings =
                                            case future.block of
                                                Nothing ->
                                                    state.strayThings
                                                        |> List.LocalExtra.removeElementAtIndex strayThingIndex

                                                Just futureThing ->
                                                    state.strayThings
                                                        |> List.LocalExtra.elementAtIndexAlter strayThingIndex
                                                            (\stray -> { stray | block = futureThing })
                                    }
                                )
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
                        [ case dragged.block of
                            BlockValue value ->
                                valueShapeSvg value |> .svg

                            BlockFact draggedFact ->
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


factOrHoleShapeSvg : Color -> Maybe FactUiState -> SizedSvg future_
factOrHoleShapeSvg backgroundColor fact =
    case fact of
        Nothing ->
            factInsertHoleShapeSvg backgroundColor

        Just equivalentFact ->
            factShapeSvg equivalentFact


factOrHoleSvg :
    Color
    -> DragState
    -> Maybe FactUiState
    -> SizedSvg { dragged : DragState, fact : Maybe FactUiState }
factOrHoleSvg backgroundColor dragState fact =
    case fact of
        Nothing ->
            factInsertHoleSvg backgroundColor dragState
                |> sizedSvgFutureMap
                    (\dropped ->
                        { dragged = Nothing, fact = Just dropped }
                    )

        Just equivalentFact ->
            factSvg dragState equivalentFact


factRelationBackgroundColor : Color
factRelationBackgroundColor =
    Color.rgb 0.2 0.2 0


relationDefinitionSvg :
    DragState
    ->
        { name : String
        , parameter : Maybe ValueUiState
        , equivalentFact : Maybe FactUiState
        }
    ->
        SizedSvg
            { dragged : DragState
            , relationDefinition :
                { parameter : Maybe ValueUiState
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

        parameterSvg : SizedSvg { dragged : DragState, value : Maybe ValueUiState }
        parameterSvg =
            valueOrHoleSvg factRelationBackgroundColor dragState definition.parameter

        equivalentFactSvg : SizedSvg { dragged : DragState, fact : Maybe FactUiState }
        equivalentFactSvg =
            factOrHoleSvg factRelationBackgroundColor dragState definition.equivalentFact

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
                , parameterSvg.height
                , equalsTextSvg.height + strokeWidth
                ]
                |> Maybe.withDefault 0

        fullHeight : Float
        fullHeight =
            Basics.max headerHeight equivalentFactSvg.height

        shapeSvg : SizedSvg future_
        shapeSvg =
            svgPolygon
                [ domModifierFillUniform factRelationBackgroundColor
                ]
                [ ( 0, 0 )
                , ( headerWidth, 0 )
                , ( fullWidth, 0 )
                , ( fullWidth, (fullHeight - equivalentFactSvg.height) / 2 )
                , ( headerWidth, (fullHeight - equivalentFactSvg.height) / 2 + strokeWidth )
                , ( headerWidth, fullHeight - (fullHeight - equivalentFactSvg.height) / 2 - strokeWidth )
                , ( fullWidth, fullHeight - (fullHeight - equivalentFactSvg.height) / 2 )
                , ( fullWidth, fullHeight )
                , ( headerWidth, fullHeight )
                , ( 0, fullHeight )
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
                    , y = strokeWidth / 2 + (headerHeight - nameSvg.height) / 2
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
                    , y = strokeWidth / 2 + (headerHeight - equalsTextSvg.height) / 2
                    }
                ]
                [ equalsTextSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate
                    { x = headerWidth
                    , y = (fullHeight - equivalentFactSvg.height) / 2
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
            factOrHoleShapeSvg factNotBackgroundColor maybeFactInverse
        , shapeListenModifier = Web.Dom.modifierNone
        }


equalsShapeSvg : { a : Maybe ValueUiState, b : Maybe ValueUiState } -> SizedSvg future_
equalsShapeSvg toEquate =
    factEqualsSvgWithInteractivity
        { valueASvg = valueOrHoleShapeSvg factRelationBackgroundColor toEquate.a
        , valueBSvg = valueOrHoleShapeSvg factRelationBackgroundColor toEquate.b
        , shapeEventListenModifier = Web.Dom.modifierNone
        }


relationUseShapeSvg : { identifier : String, argument : Maybe ValueUiState } -> SizedSvg future_
relationUseShapeSvg relationUse =
    relationUseSvgWithInteractivity
        { shapeEventListenModifier = Web.Dom.modifierNone
        , identifier = relationUse.identifier
        , argumentAsSvg =
            valueOrHoleShapeSvg factRelationBackgroundColor relationUse.argument
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


valueOrHoleSvg :
    Color
    -> DragState
    -> Maybe ValueUiState
    -> SizedSvg { dragged : DragState, value : Maybe ValueUiState }
valueOrHoleSvg backgroundColor dragState maybeValue =
    case maybeValue of
        Nothing ->
            valueHoleSvg backgroundColor dragState
                |> sizedSvgFutureMap
                    (\droppedValue ->
                        { dragged = Nothing, value = Just droppedValue }
                    )

        Just value ->
            valueSvg dragState value


valueOrHoleShapeSvg : Color -> Maybe ValueUiState -> SizedSvg future_
valueOrHoleShapeSvg backgroundColor maybeValue =
    case maybeValue of
        Just value ->
            valueShapeSvg value

        Nothing ->
            valueHoleShapeSvg backgroundColor


verticalFactListPolygonPoints :
    { headerWidth : Float
    , headerHeight : Float
    , sideWidth : Float
    , strokeWidth : Float
    }
    -> { height : Float, width : Float, svgs : List { element_ | y : Float } }
    -> List ( Float, Float )
verticalFactListPolygonPoints { headerWidth, headerHeight, sideWidth, strokeWidth } elementsSvg =
    let
        fullHeight : Float
        fullHeight =
            headerHeight
                + elementsSvg.height
                + sideWidth

        fullWidth : Float
        fullWidth =
            sideWidth
                + Basics.max elementsSvg.width headerWidth
    in
    [ ( fullWidth, headerHeight )
    , ( fullWidth, 0 )
    , ( sideWidth, 0 )
    , ( 0, strokeWidth )
    , ( 0, headerHeight + elementsSvg.height )
    , ( sideWidth, fullHeight )
    , ( fullWidth, fullHeight )
    , ( fullWidth, headerHeight + elementsSvg.height )
    , ( sideWidth + strokeWidth, headerHeight + elementsSvg.height )
    , ( sideWidth, headerHeight + elementsSvg.height - strokeWidth )
    ]
        ++ (elementsSvg.svgs
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
                    factOrHoleSvg factNotBackgroundColor dragState maybeFactInverse
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
                                                , block = BlockFact (Not maybeFactInverse)
                                                }
                                        , fact = Nothing
                                        }
                            )
                }

        Equal toEquate ->
            factEqualsSvgWithInteractivity
                { valueASvg =
                    valueOrHoleSvg factRelationBackgroundColor dragState toEquate.a
                        |> sizedSvgFutureMap
                            (\futureA ->
                                { dragged = futureA.dragged
                                , fact = Just (Equal { a = futureA.value, b = toEquate.b })
                                }
                            )
                , valueBSvg =
                    valueOrHoleSvg factRelationBackgroundColor dragState toEquate.b
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
                                                , block = BlockFact (Equal toEquate)
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
                                                , block = BlockFact (RelationUse relationUse)
                                                }
                                        , fact = Nothing
                                        }
                            )
                , identifier = relationUse.identifier
                , argumentAsSvg =
                    case relationUse.argument of
                        Nothing ->
                            valueHoleSvg factRelationBackgroundColor dragState
                                |> sizedSvgFutureMap
                                    (\futureArgumentUiState ->
                                        { dragged = Nothing
                                        , fact =
                                            Just
                                                (RelationUse
                                                    { identifier = relationUse.identifier
                                                    , argument = Just futureArgumentUiState
                                                    }
                                                )
                                        }
                                    )

                        Just argumentValue ->
                            valueSvg dragState argumentValue
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
blockVerticalFactListSvg config =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        sideWidth : Float
        sideWidth =
            strokeWidth

        blockNameStringSvg : SizedSvg future_
        blockNameStringSvg =
            unselectableTextSvg config.name

        headerWidth : Float
        headerWidth =
            strokeWidth + blockNameStringSvg.width + strokeWidth

        headerHeight : Float
        headerHeight =
            fontSize + strokeWidth

        insertHoles : List (SizedSvg { dragged : DragState, elements : List FactUiState })
        insertHoles =
            case config.elements of
                [] ->
                    factInsertHoleSvg config.color config.dragState
                        |> sizedSvgFutureMap
                            (\futureUiState ->
                                { dragged = Nothing
                                , elements = [ futureUiState ]
                                }
                            )
                        |> List.singleton

                element0 :: element1Up ->
                    case config.dragState of
                        Nothing ->
                            []

                        Just dragged ->
                            case dragged.block of
                                BlockValue _ ->
                                    []

                                BlockFact _ ->
                                    List.range 0 ((element0 :: element1Up) |> List.length)
                                        |> List.map
                                            (\insertIndex ->
                                                factInsertHoleSvg config.color (Just dragged)
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
                    , svg : Web.Dom.Node { dragged : DragState, elements : List FactUiState }
                    }
            }
        elementsAndInsertHolesSvg =
            let
                elementsSvgs : List (SizedSvg { dragged : DragState, elements : List FactUiState })
                elementsSvgs =
                    config.elements
                        |> List.indexedMap
                            (\partIndex part ->
                                factSvg config.dragState part
                                    |> sizedSvgFutureMap
                                        (\partFutureUiState ->
                                            { dragged = partFutureUiState.dragged
                                            , elements =
                                                case partFutureUiState.fact of
                                                    Nothing ->
                                                        config.elements
                                                            |> List.LocalExtra.removeElementAtIndex partIndex

                                                    Just futurePartFact ->
                                                        config.elements
                                                            |> List.LocalExtra.elementAtIndexAlter partIndex
                                                                (\_ -> futurePartFact)
                                            }
                                        )
                            )
            in
            List.LocalExtra.interweave insertHoles elementsSvgs
                |> verticalSvg

        shapeSvg : SizedSvg { dragged : DragState, elements : Maybe (List FactUiState) }
        shapeSvg =
            svgPolygon
                [ domModifierFillUniform config.color
                , domListenToPointerDown
                    |> Web.Dom.modifierFutureMap
                        (\pointerDownEventPosition ->
                            case pointerDownEventPosition of
                                Err _ ->
                                    { dragged = config.dragState, elements = Just config.elements }

                                Ok pointer ->
                                    { dragged =
                                        Just
                                            { x = pointer.x
                                            , y = pointer.y
                                            , offsetX = -fontSize
                                            , offsetY = -fontSize
                                            , block = BlockFact config.fact
                                            }
                                    , elements = Nothing
                                    }
                        )
                ]
                (verticalFactListPolygonPoints
                    { headerWidth = headerWidth
                    , headerHeight = headerHeight
                    , sideWidth = sideWidth
                    , strokeWidth = strokeWidth
                    }
                    elementsAndInsertHolesSvg
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
                [ blockNameStringSvg.svg ]
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

        elementsSvg :
            { width : Float
            , height : Float
            , svgs :
                List
                    { y : Float
                    , svg : Web.Dom.Node future_
                    }
            }
        elementsSvg =
            (case config.elements of
                [] ->
                    [ factInsertHoleShapeSvg config.color ]

                branch0 :: branch1Up ->
                    (branch0 :: branch1Up)
                        |> List.map factShapeSvg
            )
                |> verticalSvg

        blockNameTextSvg : SizedSvg future_
        blockNameTextSvg =
            unselectableTextSvg config.name

        headerHeight : Float
        headerHeight =
            fontSize + strokeWidth

        headerWidth : Float
        headerWidth =
            strokeWidth + blockNameTextSvg.width + strokeWidth

        shapeSvg : SizedSvg future_
        shapeSvg =
            svgPolygon
                [ domModifierFillUniform config.color
                ]
                (verticalFactListPolygonPoints
                    { headerWidth = headerWidth
                    , headerHeight = headerHeight
                    , sideWidth = sideWidth
                    , strokeWidth = strokeWidth
                    }
                    elementsSvg
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
                [ blockNameTextSvg.svg ]
            , stackSvg
                [ svgAttributeTranslate { x = sideWidth, y = headerHeight } ]
                [ elementsSvg
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


factNotBackgroundColor : Color
factNotBackgroundColor =
    Color.rgb 0.2 0.04 0


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
            svgPolygon
                [ domModifierFillUniform factNotBackgroundColor
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


factInsertHoleSvg : Color -> DragState -> SizedSvg FactUiState
factInsertHoleSvg backgroundColor dragState =
    let
        shapeSvg : SizedSvg future_
        shapeSvg =
            factInsertHoleShapeSvg backgroundColor
    in
    { height = shapeSvg.height
    , width = shapeSvg.width
    , svg =
        stackSvg
            [ case dragState of
                Nothing ->
                    Web.Dom.modifierNone

                Just dragged ->
                    case dragged.block of
                        BlockValue _ ->
                            Web.Dom.modifierNone

                        BlockFact draggedFact ->
                            Web.Dom.listenTo "pointerup"
                                |> Web.Dom.modifierFutureMap
                                    (\_ -> draggedFact)
            ]
            [ shapeSvg.svg ]
    }


factInsertHoleShapeSvg : Color -> SizedSvg future_
factInsertHoleShapeSvg backgroundColor =
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
    svgPolygon
        [ domModifierFillUniform
            (backgroundColor |> colorBrightnessScaleBy missingThingBrightnessScale)
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
            svgPolygon
                [ domModifierFillUniform factRelationBackgroundColor
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


svgPolygon :
    List (Web.Dom.Modifier future_)
    -> List ( Float, Float )
    -> SizedSvg future_
svgPolygon modifiers points =
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
            svgPolygon
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
        Variable variableName ->
            variableShapeSvg variableName

        ValueLookup valueLookup ->
            valueLookupShapeSvg valueLookup


valueSvg :
    DragState
    -> ValueUiState
    -> SizedSvg { dragged : DragState, value : Maybe ValueUiState }
valueSvg dragState =
    \value ->
        case value of
            Variable variableName ->
                variableName
                    |> variableSvg dragState
                    |> sizedSvgFutureMap
                        (\future ->
                            { dragged = future.dragged
                            , value =
                                case future.variable of
                                    Nothing ->
                                        Nothing

                                    Just futureVariableName ->
                                        Just (Variable futureVariableName)
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
                                        Nothing

                                    Just futureValueLookup ->
                                        Just (ValueLookup futureValueLookup)
                            }
                        )


valueLookupBackgroundColor : Color
valueLookupBackgroundColor =
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
                                        , block = BlockValue (ValueLookup valueLookup)
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
                        valueOrHoleSvg valueLookupBackgroundColor dragState entry.value
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
                    , value = valueOrHoleShapeSvg valueLookupBackgroundColor entry.value
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
        radius : Float
        radius =
            fontSize
    in
    case valueLookup of
        [] ->
            circleSvg { radius = radius }
                [ interactivity.listenToDragStart
                , domModifierFillUniform valueLookupBackgroundColor
                , svgAttributeTranslate { x = radius, y = radius }
                ]

        entry0 :: entry1Up ->
            let
                entrySvgs :
                    { width : Float
                    , height : Float
                    , svgs :
                        List
                            { y : Float
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

                shapeSvg : Web.Dom.Node future
                shapeSvg =
                    svgRoundedRect
                        [ interactivity.listenToDragStart
                        , domModifierFillUniform valueLookupBackgroundColor
                        ]
                        { radius = radius, width = fullWidth, height = fullHeight }
                        |> .svg

                fullHeight : Float
                fullHeight =
                    entrySvgs.height

                fullWidth : Float
                fullWidth =
                    radius + entrySvgs.width
            in
            { width = fullWidth
            , height = fullHeight
            , svg =
                stackSvg
                    []
                    [ shapeSvg
                    , entrySvgs.svgs
                        |> List.map
                            (\entryAsSvg ->
                                entryAsSvg.svg
                                    |> List.singleton
                                    |> stackSvg [ svgAttributeTranslate { x = 0, y = entryAsSvg.y } ]
                            )
                        |> stackSvg
                            [ svgAttributeTranslate { x = radius, y = 0 }
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
                                { dragged = dragState
                                , variable = Just variableName
                                }

                            Ok pointer ->
                                { dragged =
                                    Just
                                        { x = pointer.x
                                        , y = pointer.y
                                        , offsetX = -fontSize
                                        , offsetY = -fontSize
                                        , block = BlockValue (Variable variableName)
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
            radius : Float
            radius =
                fontSize

            nameSvg : SizedSvg future_
            nameSvg =
                unselectableTextSvg variableName

            fullWidth : Float
            fullWidth =
                nameSvg.width + radius * 2

            fullHeight : Float
            fullHeight =
                nameSvg.height + radius

            backgroundSvg : Web.Dom.Node future_
            backgroundSvg =
                svgRoundedRect
                    [ domModifierFillUniform variableColor
                    ]
                    { radius = radius, width = fullWidth, height = fullHeight }
                    |> .svg
        in
        { width = fullWidth
        , height = fullHeight
        , svg =
            stackSvg
                []
                [ backgroundSvg
                , stackSvg
                    [ svgAttributeTranslate
                        { x = radius
                        , y = radius
                        }
                    ]
                    [ nameSvg.svg ]
                ]
        }


missingThingBrightnessScale : Float
missingThingBrightnessScale =
    0.54


colorBrightnessScaleBy : Float -> (Color -> Color)
colorBrightnessScaleBy factor =
    \color ->
        let
            colorComponents : { red : Float, green : Float, blue : Float, alpha : Float }
            colorComponents =
                color |> Color.toRgba
        in
        Color.rgba
            (colorComponents.red * factor)
            (colorComponents.green * factor)
            (colorComponents.blue * factor)
            colorComponents.alpha


valueHoleSvg : Color -> DragState -> SizedSvg ValueUiState
valueHoleSvg backgroundColor dragState =
    let
        shapeSvg : SizedSvg future_
        shapeSvg =
            valueHoleShapeSvg backgroundColor
    in
    { width = shapeSvg.width
    , height = shapeSvg.height
    , svg =
        stackSvg
            [ case dragState of
                Nothing ->
                    Web.Dom.modifierNone

                Just stateDragged ->
                    case stateDragged.block of
                        BlockFact _ ->
                            Web.Dom.modifierNone

                        BlockValue draggedValue ->
                            Web.Dom.listenTo "pointerup"
                                |> Web.Dom.modifierFutureMap (\_ -> draggedValue)
            ]
            [ shapeSvg.svg ]
    }


valueHoleShapeSvg : Color -> SizedSvg future_
valueHoleShapeSvg backgroundColor =
    let
        radius : Float
        radius =
            fontSize

        fullWidth : Float
        fullWidth =
            fontSize * 5 + radius * 2

        fullHeight : Float
        fullHeight =
            fontSize + radius

        backgroundSvg : Web.Dom.Node future_
        backgroundSvg =
            svgRoundedRect
                [ domModifierFillUniform
                    (backgroundColor |> colorBrightnessScaleBy missingThingBrightnessScale)
                ]
                { radius = radius, width = fullWidth, height = fullHeight }
                |> .svg
    in
    { width = fullWidth
    , height = fullHeight
    , svg = backgroundSvg
    }


svgClosedPath :
    List (Web.Dom.Modifier future)
    -> { start : { x : Float, y : Float }, trail : List Svg.PathD.Segment }
    -> Web.Dom.Node future
svgClosedPath modifiers segments =
    Web.Svg.element "path"
        (Web.Dom.attribute "d"
            (Svg.PathD.pathD
                (case segments.trail of
                    [] ->
                        []

                    trailSegment0 :: trailSegment1Up ->
                        Svg.PathD.M ( segments.start.x, segments.start.y )
                            :: (trailSegment0 :: trailSegment1Up)
                            ++ [ Svg.PathD.Z ]
                )
            )
            :: modifiers
        )
        []


pathSegmentTopRightQuarterArcCounterclockwise :
    { end : { x : Float, y : Float }, radius : Float }
    -> List Svg.PathD.Segment
pathSegmentTopRightQuarterArcCounterclockwise geometry =
    pathDArc
        { centerX = geometry.end.x - geometry.radius
        , centerY = geometry.end.y
        , radius = geometry.radius
        , startAngle = Angle.turns (3 / 4)
        , angleSpan = Angle.turns (1 / 4)
        }


pathSegmentTopLeftQuarterArcCounterclockwise :
    { end : { x : Float, y : Float }, radius : Float }
    -> List Svg.PathD.Segment
pathSegmentTopLeftQuarterArcCounterclockwise geometry =
    pathDArc
        { centerX = geometry.end.x
        , centerY = geometry.end.y + geometry.radius
        , radius = geometry.radius
        , startAngle = Angle.turns (1 / 2)
        , angleSpan = Angle.turns (1 / 4)
        }


pathSegmentBottomLeftQuarterArcCounterclockwise :
    { end : { x : Float, y : Float }, radius : Float }
    -> List Svg.PathD.Segment
pathSegmentBottomLeftQuarterArcCounterclockwise geometry =
    pathDArc
        { centerX = geometry.end.x + geometry.radius
        , centerY = geometry.end.y
        , radius = geometry.radius
        , startAngle = Angle.turns (1 / 4)
        , angleSpan = Angle.turns (1 / 4)
        }


pathSegmentBottomRightQuarterArcCounterclockwise :
    { end : { x : Float, y : Float }, radius : Float }
    -> List Svg.PathD.Segment
pathSegmentBottomRightQuarterArcCounterclockwise geometry =
    pathDArc
        { centerX = geometry.end.x
        , centerY = geometry.end.y - geometry.radius
        , radius = geometry.radius
        , startAngle = Angle.turns 0
        , angleSpan = Angle.turns (1 / 4)
        }


svgRoundedRect :
    List (Web.Dom.Modifier future)
    -> { width : Float, height : Float, radius : Float }
    -> SizedSvg future
svgRoundedRect modifiers geometry =
    { width = geometry.width
    , height = geometry.height
    , svg =
        svgClosedPath
            modifiers
            { start = { x = 0, y = geometry.radius }
            , trail =
                [ pathSegmentTopLeftQuarterArcCounterclockwise { radius = geometry.radius, end = { x = geometry.radius, y = 0 } }
                , [ Svg.PathD.L ( geometry.width - geometry.radius, 0 ) ]
                , pathSegmentTopRightQuarterArcCounterclockwise { radius = geometry.radius, end = { x = geometry.width, y = geometry.radius } }
                , [ Svg.PathD.L ( geometry.width, geometry.height - geometry.radius ) ]
                , pathSegmentBottomRightQuarterArcCounterclockwise { radius = geometry.radius, end = { x = geometry.width - geometry.radius, y = geometry.height } }
                , [ Svg.PathD.L ( geometry.radius, geometry.height ) ]
                , pathSegmentBottomLeftQuarterArcCounterclockwise { radius = geometry.radius, end = { x = 0, y = geometry.height - geometry.radius } }
                , [ Svg.PathD.L ( 0, geometry.radius ) ]
                ]
                    |> List.concat
            }
    }


pathDArc :
    { centerX : Float, centerY : Float, radius : Float, startAngle : Angle, angleSpan : Angle }
    -> List Svg.PathD.Segment
pathDArc geometry =
    let
        arcGeometry : Arc2d.Arc2d Length.Meters coordinates
        arcGeometry =
            Arc2d.with
                { centerPoint = Point2d.meters geometry.centerX geometry.centerY
                , startAngle = geometry.startAngle |> Angle.normalize
                , sweptAngle = geometry.angleSpan |> Angle.normalize
                , radius = Length.meters geometry.radius
                }

        maxSegmentAngle : Angle
        maxSegmentAngle =
            Angle.turns (1 / 3)

        numSegments : Int
        numSegments =
            1 + floor (abs (Quantity.ratio (arcGeometry |> Arc2d.sweptAngle) maxSegmentAngle))
    in
    Parameter1d.trailing numSegments
        (\parameterValue ->
            Svg.PathD.A
                ( Arc2d.radius arcGeometry |> Length.inMeters
                , Arc2d.radius arcGeometry |> Length.inMeters
                )
                0
                False
                (arcGeometry |> Arc2d.sweptAngle |> Quantity.greaterThanOrEqualTo Quantity.zero)
                (Arc2d.pointOn arcGeometry parameterValue |> Point2d.toTuple Length.inMeters)
        )


domModifierFillUniform : Color -> Web.Dom.Modifier future_
domModifierFillUniform color =
    Web.Dom.attribute "fill" (color |> Color.toCssString)


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
