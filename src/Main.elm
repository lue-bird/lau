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
            , equivalentFact : FactUiState
            }
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
    | DraggedValue ValueUiState


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
            [ state.relationDefinitions
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
                        [ -- TODO non-interactive version?
                          case dragged.thing of
                            DraggedValue draggedValue ->
                                draggedValue |> valueSvg (Just dragged) |> .svg |> Web.Dom.futureMap (\_ -> state)

                            DraggedFact draggedFact ->
                                draggedFact |> factSvg (Just dragged) |> .svg |> Web.Dom.futureMap (\_ -> state)
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
        , equivalentFact : FactUiState
        }
    ->
        SizedSvg
            { dragged : DragState
            , relationDefinition :
                { argument : ValueUiState
                , equivalentFact : FactUiState
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

        equivalentFactSvg : SizedSvg { dragged : DragState, fact : FactUiState }
        equivalentFactSvg =
            definition.equivalentFact |> factSvg dragState

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


factSvg :
    DragState
    ->
        (FactUiState
         ->
            { width : Float
            , height : Float
            , svg : Web.Dom.Node { dragged : DragState, fact : FactUiState }
            }
        )
factSvg dragState =
    \fact ->
        case fact of
            All parts ->
                parts
                    |> factAllSvg dragState
                    |> sizedSvgFutureMap
                        (\futureAll ->
                            { dragged = futureAll.dragged
                            , fact = futureAll.parts |> All
                            }
                        )

            Any branches ->
                branches
                    |> factAnySvg dragState
                    |> sizedSvgFutureMap
                        (\futureAny ->
                            { dragged = futureAny.dragged
                            , fact = futureAny.branches |> Any
                            }
                        )

            Not inverseFact ->
                inverseFact
                    |> factNotSvg dragState
                    |> sizedSvgFutureMap
                        (\futureNot ->
                            { dragged = futureNot.dragged
                            , fact = futureNot.inverseFact |> Not
                            }
                        )

            Equal toEquate ->
                toEquate
                    |> factEqualsSvg dragState
                    |> sizedSvgFutureMap
                        (\equal ->
                            { dragged = equal.dragged
                            , fact = equal.values |> Equal
                            }
                        )

            RelationUse relationUse ->
                relationUse
                    |> relationUseSvg dragState
                    |> sizedSvgFutureMap
                        (\relationUseFuture ->
                            { dragged = relationUseFuture.dragged
                            , fact = relationUseFuture.relationUse |> RelationUse
                            }
                        )


factAllSvg :
    DragState
    ->
        (List FactUiState
         ->
            { width : Float
            , height : Float
            , svg : Web.Dom.Node { dragged : DragState, parts : List FactUiState }
            }
        )
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
                                part
                                    |> factSvg dragState
                                    |> sizedSvgFutureMap
                                        (\partFutureUiState ->
                                            { dragged = partFutureUiState.dragged
                                            , parts =
                                                (part0 :: part1Up)
                                                    |> List.LocalExtra.elementAtIndexAlter partIndex
                                                        (\_ -> partFutureUiState.fact)
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


sizedSvgFutureMap :
    (future -> futureChanged)
    -> { width : Float, height : Float, svg : Web.Dom.Node future }
    -> { width : Float, height : Float, svg : Web.Dom.Node futureChanged }
sizedSvgFutureMap futureChange =
    \sized ->
        { width = sized.width
        , height = sized.height
        , svg =
            sized.svg
                |> Web.Dom.futureMap futureChange
        }


factAnySvg :
    DragState
    ->
        (List FactUiState
         -> SizedSvg { dragged : DragState, branches : List FactUiState }
        )
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
                                                (branch0 :: branch1Up)
                                                    |> List.LocalExtra.elementAtIndexAlter branchIndex
                                                        (\_ -> branchFutureUiState.fact)
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


factNotSvg :
    DragState
    -> (FactUiState -> SizedSvg { dragged : DragState, inverseFact : FactUiState })
factNotSvg dragState inverseFact =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize

        sideWidth : Float
        sideWidth =
            strokeWidth

        factInverseSvg : SizedSvg { dragged : DragState, fact : FactUiState }
        factInverseSvg =
            inverseFact |> factSvg dragState

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
                [ factInverseSvg.svg
                    |> Web.Dom.futureMap
                        (\futureFactInverse ->
                            { inverseFact = futureFactInverse.fact
                            , dragged = futureFactInverse.dragged
                            }
                        )
                ]
            ]
    }


factEqualsSvg :
    DragState
    -> { a : ValueUiState, b : ValueUiState }
    ->
        SizedSvg
            { dragged : DragState
            , values : { a : ValueUiState, b : ValueUiState }
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
                [ valueASvg.svg
                    |> Web.Dom.futureMap
                        (\futureA ->
                            { dragged = futureA.dragged
                            , values = { a = futureA.value, b = toEquate.b }
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
                            , values = { a = toEquate.a, b = futureB.value }
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


relationUseSvg :
    DragState
    ->
        ({ identifier : String, argument : ValueUiState }
         ->
            SizedSvg
                { dragged : DragState
                , relationUse : { identifier : String, argument : ValueUiState }
                }
        )
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
                                    { identifier = relationUse.identifier
                                    , argument = futureArgumentUiState.value
                                    }
                                }
                            )
                    ]
                ]
        }


valueSvg :
    DragState
    -> (ValueUiState -> SizedSvg { dragged : DragState, value : ValueUiState })
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
                variableName |> variableSvg dragState

            ValueLookup valueLookup ->
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


variableSvg : DragState -> (String -> SizedSvg { dragged : DragState, value : ValueUiState })
variableSvg dragState =
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

            fullWidth =
                nameSvg.width + strokeWidth

            fullHeight =
                fontSize + fontSize
        in
        { width = fullWidth
        , height = fullHeight
        , svg =
            stackSvg
                [ Web.Dom.listenTo "pointerdown"
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
                                Err _ ->
                                    { dragged = dragState, value = Variable variableName }

                                Ok pointer ->
                                    { dragged =
                                        -- TODO apparently offset does not work in svg
                                        -- so we currently just take the "grab center"
                                        -- future alternative could be a custom g element that triggers a custom event
                                        -- https://discourse.elm-lang.org/t/dispatching-custom-events-only-if-needed/2740/7
                                        let
                                            offsetX : Float
                                            offsetX =
                                                -fullWidth / 2

                                            offsetY : Float
                                            offsetY =
                                                -fullHeight / 2
                                        in
                                        Just
                                            { x = pointer.x
                                            , y = pointer.y
                                            , offsetX = offsetX
                                            , offsetY = offsetY
                                            , thing = DraggedValue (Variable variableName)
                                            }
                                    , value = ValueHole
                                    }
                        )
                ]
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


valueHoleSvg : DragState -> SizedSvg ValueUiState
valueHoleSvg dragState =
    let
        strokeWidth : Float
        strokeWidth =
            fontSize * 2

        color : Color
        color =
            Color.rgba 0 0 0 0.4

        nameSvg : SizedSvg future_
        nameSvg =
            unselectableTextSvg "drag a value here"

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
    { width = nameSvg.width + strokeWidth
    , height = fontSize + fontSize
    , svg =
        stackSvg
            [ case dragState of
                Nothing ->
                    Web.Dom.modifierNone

                Just stateDragged ->
                    case stateDragged.thing of
                        DraggedFact _ ->
                            Web.Dom.modifierNone

                        DraggedValue draggedValue ->
                            Web.Dom.listenTo "pointerup"
                                |> Web.Dom.modifierFutureMap
                                    (\_ -> draggedValue)
            ]
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
