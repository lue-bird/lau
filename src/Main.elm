port module Main exposing (BlockUiState(..), DragState, FactUiState(..), State, TextInputUiState(..), ValueUiState(..), main)

{- dev notes

   Dragging offset:
   Apparently offset does not work in svg
   so we currently just take the top left with a stroke width distance.
   future alternative could be a custom svg g element that triggers a custom event
   https://discourse.elm-lang.org/t/dispatching-custom-events-only-if-needed/2740/7
-}

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
import Set exposing (Set)
import Svg.PathD
import Web
import Web.Dom
import Web.Svg
import Web.Window


type alias State =
    { windowWidth : Int
    , windowHeight : Int
    , relationDefinitionsNotShown :
        FastDict.Dict
            String
            { parameter : Maybe ValueUiState
            , equivalentFact : Maybe FactUiState
            }
    , relationDefinitionShown :
        { identifier : String
        , parameter : Maybe ValueUiState
        , equivalentFact : Maybe FactUiState
        }
    , strayThings : List { x : Float, y : Float, block : BlockUiState }
    , dragged : DragState
    , createVariable : TextInputUiState
    , createEntryKey : TextInputUiState
    , createRelationIdentifier : String
    }


type alias DragState =
    Maybe
        { x : Float
        , y : Float
        , block : BlockUiState
        }


type TextInputUiState
    = Editing String
    | DoneEditing String


type BlockUiState
    = BlockFact FactUiState
    | BlockValue ValueUiState


type FactUiState
    = RelationUse { identifier : String, argument : Maybe ValueUiState }
    | Equal { a : Maybe ValueUiState, b : Maybe ValueUiState }
    | Not (Maybe FactUiState)
    | All (List FactUiState)
    | Any (List FactUiState)


type ValueUiState
    = Variable String
    | ValueLookup (List { key : String, value : Maybe ValueUiState })


maybeFactVariables : Maybe FactUiState -> Set String
maybeFactVariables =
    \maybeFactUiState ->
        case maybeFactUiState of
            Nothing ->
                Set.empty

            Just factUiState ->
                factUiState |> factVariables


maybeValueVariables : Maybe ValueUiState -> Set String
maybeValueVariables =
    \maybeValueUiState ->
        case maybeValueUiState of
            Nothing ->
                Set.empty

            Just valueUiState ->
                valueUiState |> valueVariables


factVariables : FactUiState -> Set String
factVariables =
    \factUiState ->
        case factUiState of
            Not maybeFactInverse ->
                maybeFactInverse |> maybeFactVariables

            Equal values ->
                Set.union
                    (values.a |> maybeValueVariables)
                    (values.b |> maybeValueVariables)

            Any branches ->
                branches |> List.LocalExtra.setFlatMap factVariables

            All parts ->
                parts |> List.LocalExtra.setFlatMap factVariables

            RelationUse relationUse ->
                relationUse.argument |> maybeValueVariables


valueVariables : ValueUiState -> Set String
valueVariables =
    \value ->
        case value of
            Variable variableName ->
                variableName |> Set.singleton

            ValueLookup valueLookup ->
                valueLookup
                    |> List.LocalExtra.setFlatMap
                        (\entry -> entry.value |> maybeValueVariables)


maybeFactEntryKeys : Maybe FactUiState -> Set String
maybeFactEntryKeys =
    \maybeFact ->
        case maybeFact of
            Nothing ->
                Set.empty

            Just fact ->
                fact |> factEntryKeys


maybeValueEntryKeys : Maybe ValueUiState -> Set String
maybeValueEntryKeys =
    \maybeValue ->
        case maybeValue of
            Nothing ->
                Set.empty

            Just value ->
                value |> valueEntryKeys


factEntryKeys : FactUiState -> Set String
factEntryKeys =
    \fact ->
        case fact of
            RelationUse relationUse ->
                relationUse.argument |> maybeValueEntryKeys

            Not inverseFact ->
                inverseFact |> maybeFactEntryKeys

            All parts ->
                parts |> List.LocalExtra.setFlatMap factEntryKeys

            Any branches ->
                branches |> List.LocalExtra.setFlatMap factEntryKeys

            Equal values ->
                Set.union (values.a |> maybeValueEntryKeys)
                    (values.b |> maybeValueEntryKeys)


valueEntryKeys : ValueUiState -> Set String
valueEntryKeys =
    \value ->
        case value of
            Variable _ ->
                Set.empty

            ValueLookup valueLookup ->
                valueLookup
                    |> List.LocalExtra.setFlatMap
                        (\entry -> Set.insert entry.key (entry.value |> maybeValueEntryKeys))


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


relationDefinitionsUiStateToLau :
    FastDict.Dict
        String
        { parameter : Maybe ValueUiState
        , equivalentFact : Maybe FactUiState
        }
    -> Maybe (List Lau.RelationDefinition)
relationDefinitionsUiStateToLau =
    \relationDefinitionsUiState ->
        relationDefinitionsUiState
            |> FastDict.foldl
                (\identifier relationInfoUiState soFar ->
                    case soFar of
                        Nothing ->
                            Nothing

                        Just soFarLau ->
                            Maybe.map2
                                (\parameter equivalentFact ->
                                    soFarLau
                                        |> (::)
                                            { identifier = identifier
                                            , parameter = parameter
                                            , equivalentFact = equivalentFact
                                            }
                                )
                                (relationInfoUiState.parameter |> Maybe.andThen valueUiStateToLau)
                                (relationInfoUiState.equivalentFact |> Maybe.andThen factUiStateToLau)
                )
                (Just [])


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
                Maybe.map2 (\toEquateA toEquateB -> Lau.Equal { a = toEquateA, b = toEquateB })
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
    , relationDefinitionsNotShown = FastDict.empty
    , relationDefinitionShown =
        { identifier = "main"
        , parameter =
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
                        { a = Variable "interface" |> Just
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
                        { identifier = "is greater or equal to 0"
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
    , createVariable = Editing ""
    , createEntryKey = Editing ""
    , createRelationIdentifier = ""
    }


fontSize : Float
fontSize =
    16


strokeWidth : Float
strokeWidth =
    fontSize


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


pathDArc :
    { centerX : Float
    , centerY : Float
    , radius : Float
    , startAngle : Angle
    , angleSpan : Angle
    }
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


sizedSvgRoundedRect :
    List (Web.Dom.Modifier future)
    -> { width : Float, height : Float, radius : Float }
    -> SizedSvg future
sizedSvgRoundedRect modifiers geometry =
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


domModifierFillUniform : Color -> Web.Dom.Modifier future_
domModifierFillUniform color =
    Web.Dom.attribute "fill" (color |> Color.toCssString)


valueHoleShapeSvg : Color -> SizedSvg future_
valueHoleShapeSvg backgroundColor =
    sizedSvgRoundedRect
        [ domModifierFillUniform
            (backgroundColor |> colorBrightnessScaleBy missingThingBrightnessScale)
        ]
        { radius = strokeWidth
        , width = fontSize * 5 + strokeWidth
        , height = fontSize + strokeWidth
        }


valueOrHoleShapeSvg : Color -> Maybe ValueUiState -> SizedSvg future_
valueOrHoleShapeSvg backgroundColor maybeValue =
    case maybeValue of
        Just value ->
            valueShapeSvg value

        Nothing ->
            valueHoleShapeSvg backgroundColor


fontWidth : Float
fontWidth =
    fontSize * 0.53


sizedSvgText : List (Web.Dom.Modifier future) -> String -> SizedSvg future
sizedSvgText modifiers string =
    let
        width : Float
        width =
            (string |> String.length |> Basics.toFloat) * fontWidth
    in
    { height = fontSize
    , width = width
    , svg =
        Web.Svg.element "text"
            ([ Web.Dom.attribute "x" (0 |> String.fromFloat)
             , Web.Dom.attribute "y" (fontBaseline * fontSize |> String.fromFloat)
             , domModifierFillUniform (Color.rgb 1 1 1)
             ]
                ++ modifiers
            )
            [ Web.Dom.text string ]
    }


fontBaseline : Float
fontBaseline =
    0.77


sizedSvgUnselectableText : String -> SizedSvg future_
sizedSvgUnselectableText string =
    sizedSvgText
        [ Web.Dom.style "user-select" "none"
        , Web.Dom.style "pointer-events" "none"
        ]
        string


svgAttributeTranslate : { x : Float, y : Float } -> Web.Dom.Modifier future_
svgAttributeTranslate offset =
    Web.Dom.attribute "transform"
        ([ "translate("
         , offset.x |> String.fromFloat
         , " "
         , offset.y |> String.fromFloat
         , ")"
         ]
            |> String.concat
        )


svgStack :
    List (Web.Dom.Modifier future)
    -> List (Web.Dom.Node future)
    -> Web.Dom.Node future
svgStack modifiers subs =
    Web.Svg.element "g" modifiers subs


sizedSvgPad :
    { left : Float, right : Float, top : Float, bottom : Float }
    -> (SizedSvg future -> SizedSvg future)
sizedSvgPad additionalPadding =
    \sizedSvg ->
        { width = sizedSvg.width + additionalPadding.left + additionalPadding.right
        , height = sizedSvg.height + additionalPadding.bottom + additionalPadding.top
        , svg =
            svgStack
                [ svgAttributeTranslate
                    { x = additionalPadding.left
                    , y = additionalPadding.top
                    }
                ]
                [ sizedSvg.svg ]
        }


sizedSvgHorizontalCentered : List (SizedSvg future) -> SizedSvg future
sizedSvgHorizontalCentered elements =
    let
        fullHeight : Float
        fullHeight =
            elements |> List.map .height |> List.maximum |> Maybe.withDefault 0

        combined : { combinedWidth : Float, svgs : List (Web.Dom.Node future) }
        combined =
            elements
                |> List.foldl
                    (\element soFar ->
                        { combinedWidth = soFar.combinedWidth + element.width
                        , svgs =
                            soFar.svgs
                                |> (::)
                                    (svgStack
                                        [ svgAttributeTranslate
                                            { x = soFar.combinedWidth
                                            , y = (fullHeight - element.height) / 2
                                            }
                                        ]
                                        [ element.svg ]
                                    )
                        }
                    )
                    { combinedWidth = 0, svgs = [] }
    in
    { height = fullHeight
    , width = combined.combinedWidth
    , svg = combined.svgs |> svgStack []
    }


relationBackgroundColor : Color
relationBackgroundColor =
    Color.rgb 0.22 0.14 0


sizedSvgPolygon :
    List (Web.Dom.Modifier future)
    -> List ( Float, Float )
    -> SizedSvg future
sizedSvgPolygon modifiers points =
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
                            [ x |> String.fromFloat, ",", y |> String.fromFloat ] |> String.concat
                        )
                    |> String.join " "
                )
                :: modifiers
            )
            []
    }


factEqualsSvgWithInteractivity :
    { shapeEventListenModifier : Web.Dom.Modifier future
    , valueASvg : SizedSvg future
    , valueBSvg : SizedSvg future
    }
    -> SizedSvg future
factEqualsSvgWithInteractivity parts =
    let
        fullWidth : Float
        fullWidth =
            strokeWidth + contentSvg.width + strokeWidth / 2

        spaceWidth : Float
        spaceWidth =
            fontWidth

        equalsTextSvg : SizedSvg future_
        equalsTextSvg =
            sizedSvgUnselectableText "="

        contentSvg : SizedSvg future
        contentSvg =
            sizedSvgHorizontalCentered
                [ parts.valueASvg
                , equalsTextSvg
                    |> sizedSvgPad
                        { left = spaceWidth
                        , right = spaceWidth
                        , top = 0
                        , bottom = 0
                        }
                , parts.valueBSvg
                ]

        shapeSvg : SizedSvg future
        shapeSvg =
            sizedSvgPolygon
                [ domModifierFillUniform relationBackgroundColor
                , parts.shapeEventListenModifier
                ]
                [ ( 0, strokeWidth )
                , ( strokeWidth, 0 )
                , ( fullWidth, 0 )
                , ( fullWidth, contentSvg.height )
                , ( strokeWidth, contentSvg.height )
                , ( 0, contentSvg.height - strokeWidth )
                ]
    in
    { height = shapeSvg.height
    , width = shapeSvg.width
    , svg =
        svgStack
            []
            [ shapeSvg.svg
            , svgStack
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = 0
                    }
                ]
                [ contentSvg.svg ]
            ]
    }


factEqualsShapeSvg : { a : Maybe ValueUiState, b : Maybe ValueUiState } -> SizedSvg future_
factEqualsShapeSvg toEquate =
    factEqualsSvgWithInteractivity
        { valueASvg = valueOrHoleShapeSvg relationBackgroundColor toEquate.a
        , valueBSvg = valueOrHoleShapeSvg relationBackgroundColor toEquate.b
        , shapeEventListenModifier = Web.Dom.modifierNone
        }


relationUseSvgWithInteractivity :
    { shapeEventListenModifier : Web.Dom.Modifier future
    , identifier : SizedSvg future
    , argumentSvg : SizedSvg future
    }
    -> SizedSvg future
relationUseSvgWithInteractivity parts =
    let
        spaceWidth : Float
        spaceWidth =
            fontWidth

        contentSvg : SizedSvg future
        contentSvg =
            sizedSvgHorizontalCentered
                [ parts.identifier
                    |> sizedSvgPad
                        { top = strokeWidth / 2
                        , bottom = strokeWidth / 2
                        , left = 0
                        , right = spaceWidth
                        }
                , parts.argumentSvg
                ]

        fullWidth : Float
        fullWidth =
            strokeWidth + contentSvg.width + strokeWidth / 2

        shapeSvg : SizedSvg future
        shapeSvg =
            sizedSvgPolygon
                [ domModifierFillUniform relationBackgroundColor
                , parts.shapeEventListenModifier
                ]
                [ ( 0, strokeWidth )
                , ( strokeWidth, 0 )
                , ( fullWidth, 0 )
                , ( fullWidth, contentSvg.height )
                , ( strokeWidth, contentSvg.height )
                , ( 0, contentSvg.height - strokeWidth )
                ]
    in
    { height = contentSvg.height
    , width = fullWidth
    , svg =
        svgStack
            []
            [ shapeSvg.svg
            , svgStack
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = 0
                    }
                ]
                [ contentSvg.svg ]
            ]
    }


relationUseShapeSvg : { identifier : String, argument : Maybe ValueUiState } -> SizedSvg future_
relationUseShapeSvg relationUse =
    relationUseSvgWithInteractivity
        { shapeEventListenModifier = Web.Dom.modifierNone
        , identifier = sizedSvgUnselectableText relationUse.identifier
        , argumentSvg =
            valueOrHoleShapeSvg relationBackgroundColor relationUse.argument
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
            factEqualsShapeSvg toEquate

        RelationUse relationUse ->
            relationUseShapeSvg relationUse


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


factInsertHoleShapeSvg : Color -> SizedSvg future_
factInsertHoleShapeSvg backgroundColor =
    let
        fullWidth : Float
        fullWidth =
            strokeWidth
                + (strokeWidth * 5)
                + strokeWidth

        fullHeight : Float
        fullHeight =
            strokeWidth + strokeWidth
    in
    sizedSvgPolygon
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
        svgStack
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
        svgStack
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


factAllBackgroundColor : Color
factAllBackgroundColor =
    Color.rgb 0.01 0.14 0


factAnyBackgroundColor : Color
factAnyBackgroundColor =
    Color.rgb 0.15 0 0.1


factNotBackgroundColor : Color
factNotBackgroundColor =
    Color.rgb 0.25 0.02 0


factNotSvgWithInteractivity :
    { shapeListenModifier : Web.Dom.Modifier future
    , factInverseSvg : SizedSvg future
    }
    -> SizedSvg future
factNotSvgWithInteractivity parts =
    let
        headerWidth : Float
        headerWidth =
            strokeWidth + notTextSvg.width + strokeWidth / 2

        fullWidth : Float
        fullWidth =
            headerWidth + parts.factInverseSvg.width

        notTextSvg : SizedSvg future_
        notTextSvg =
            sizedSvgUnselectableText "not"

        fullHeight : Float
        fullHeight =
            Basics.max (fontSize + strokeWidth)
                parts.factInverseSvg.height

        shapeSvg : SizedSvg future
        shapeSvg =
            sizedSvgPolygon
                [ domModifierFillUniform factNotBackgroundColor
                , parts.shapeListenModifier
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
        svgStack
            []
            [ shapeSvg.svg
            , svgStack
                [ svgAttributeTranslate
                    { x = strokeWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ notTextSvg.svg ]
            , svgStack
                [ svgAttributeTranslate { x = headerWidth, y = 0 } ]
                [ parts.factInverseSvg.svg ]
            ]
    }


valueLookupBackgroundColor : Color
valueLookupBackgroundColor =
    Color.rgb 0 0.06 0.19


svgSizedVertical :
    List (SizedSvg future)
    -> SizedSvg future
svgSizedVertical =
    \elements ->
        let
            elementsAsSvgs :
                { combinedHeight : Float
                , widthMaximum : Float
                , svgsReverse : List (Web.Dom.Node future)
                }
            elementsAsSvgs =
                elements
                    |> List.foldl
                        (\asSvg soFar ->
                            { combinedHeight = soFar.combinedHeight + asSvg.height
                            , widthMaximum = Basics.max soFar.widthMaximum asSvg.width
                            , svgsReverse =
                                soFar.svgsReverse
                                    |> (::)
                                        (asSvg.svg
                                            |> List.singleton
                                            |> svgStack
                                                [ svgAttributeTranslate
                                                    { x = 0, y = soFar.combinedHeight }
                                                ]
                                        )
                            }
                        )
                        { combinedHeight = 0
                        , widthMaximum = 0
                        , svgsReverse = []
                        }
        in
        { height = elementsAsSvgs.combinedHeight
        , width = elementsAsSvgs.widthMaximum
        , svg =
            elementsAsSvgs.svgsReverse
                |> List.reverse
                |> svgStack []
        }


valueLookupEntryContentSvg : { key : String, value : SizedSvg future } -> SizedSvg future
valueLookupEntryContentSvg =
    \entry ->
        let
            entryNameSvg : SizedSvg future_
            entryNameSvg =
                sizedSvgUnselectableText entry.key
        in
        sizedSvgHorizontalCentered
            [ entryNameSvg
                |> sizedSvgPad
                    { left = strokeWidth / 2
                    , top = 0
                    , bottom = 0
                    , right = fontWidth
                    }
            , entry.value
            ]


circleSvg : { radius : Float } -> List (Web.Dom.Modifier future) -> SizedSvg future
circleSvg geometry modifiers =
    { width = geometry.radius * 2
    , height = geometry.radius * 2
    , svg =
        Web.Svg.element "circle"
            (Web.Dom.attribute "r" (geometry.radius |> String.fromFloat)
                :: svgAttributeTranslate { x = geometry.radius, y = geometry.radius }
                :: modifiers
            )
            []
    }


domListenToPointerDown : Web.Dom.Modifier { x : Float, y : Float }
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
                    Ok pointer ->
                        pointer

                    Err _ ->
                        -- silent failure :(
                        { x = 0, y = 0 }
            )


sizedSvgStack :
    List (Web.Dom.Modifier future)
    -> List (SizedSvg future)
    -> SizedSvg future
sizedSvgStack modifiers elements =
    { width =
        elements |> List.map .width |> List.maximum |> Maybe.withDefault 0
    , height =
        elements |> List.map .height |> List.maximum |> Maybe.withDefault 0
    , svg =
        elements |> List.map .svg |> svgStack modifiers
    }


variableSvgWithInteractivity :
    { dragStart : Web.Dom.Modifier future
    , variableNameSvg : SizedSvg future
    }
    -> SizedSvg future
variableSvgWithInteractivity interactivity =
    sizedSvgStack []
        [ sizedSvgRoundedRect
            [ domModifierFillUniform variableBackgroundColor
            , interactivity.dragStart
            ]
            { width = interactivity.variableNameSvg.width
            , height = interactivity.variableNameSvg.height
            , radius = strokeWidth
            }
        , interactivity.variableNameSvg
        ]


variableBackgroundColor : Color
variableBackgroundColor =
    Color.rgb 0 0.19 0.21


variableShapeSvg : String -> SizedSvg future_
variableShapeSvg variableName =
    variableSvgWithInteractivity
        { dragStart = Web.Dom.modifierNone
        , variableNameSvg =
            sizedSvgUnselectableText variableName
                |> sizedSvgPad
                    { left = strokeWidth / 2
                    , right = strokeWidth / 2
                    , top = strokeWidth / 2
                    , bottom = strokeWidth / 2
                    }
        }


valueSvg :
    DragState
    -> ValueUiState
    -> SizedSvg { dragged : DragState, value : Maybe ValueUiState }
valueSvg dragState =
    \value ->
        case value of
            Variable variableName ->
                variableShapeSvg variableName
                    |> List.singleton
                    |> sizedSvgStack
                        [ domListenToPointerDown
                            |> Web.Dom.modifierFutureMap
                                (\pointer ->
                                    { dragged =
                                        Just
                                            { x = pointer.x
                                            , y = pointer.y
                                            , block = BlockValue (Variable variableName)
                                            }
                                    , value = Nothing
                                    }
                                )
                        ]

            ValueLookup valueLookup ->
                let
                    existingEntrySvgs : List (SizedSvg { dragged : DragState, value : Maybe ValueUiState })
                    existingEntrySvgs =
                        valueLookup
                            |> List.indexedMap
                                (\entryIndex entry ->
                                    let
                                        entryContentSvg : SizedSvg { dragged : DragState, value : Maybe ValueUiState }
                                        entryContentSvg =
                                            valueLookupEntryContentSvg
                                                { key = entry.key
                                                , value =
                                                    valueOrHoleSvg valueLookupBackgroundColor dragState entry.value
                                                        |> sizedSvgFutureMap
                                                            (\entryValueFuture ->
                                                                { dragged = entryValueFuture.dragged
                                                                , value =
                                                                    ValueLookup
                                                                        (valueLookup
                                                                            |> List.LocalExtra.elementAtIndexAlter entryIndex
                                                                                (\_ ->
                                                                                    { key = entry.key
                                                                                    , value = entryValueFuture.value
                                                                                    }
                                                                                )
                                                                        )
                                                                        |> Just
                                                                }
                                                            )
                                                }
                                    in
                                    sizedSvgStack []
                                        [ sizedSvgRoundedRect
                                            [ domModifierFillUniform valueLookupBackgroundColor
                                            , domListenToPointerDown
                                                |> Web.Dom.modifierFutureMap
                                                    (\pointer ->
                                                        { dragged =
                                                            Just
                                                                { x = pointer.x
                                                                , y = pointer.y
                                                                , block = BlockValue (ValueLookup [ entry ])
                                                                }
                                                        , value =
                                                            case valueLookup |> List.LocalExtra.removeElementAtIndex entryIndex of
                                                                [] ->
                                                                    Nothing

                                                                newEntry0 :: newEntry1Up ->
                                                                    ValueLookup (newEntry0 :: newEntry1Up) |> Just
                                                        }
                                                    )
                                            ]
                                            { width = entryContentSvg.width
                                            , height = entryContentSvg.height
                                            , radius = strokeWidth
                                            }
                                        , entryContentSvg
                                        ]
                                )

                    listenToDragStart : Web.Dom.Modifier { dragged : DragState, value : Maybe ValueUiState }
                    listenToDragStart =
                        domListenToPointerDown
                            |> Web.Dom.modifierFutureMap
                                (\pointer ->
                                    { dragged =
                                        Just
                                            { x = pointer.x
                                            , y = pointer.y
                                            , block = BlockValue (ValueLookup valueLookup)
                                            }
                                    , value = Nothing
                                    }
                                )
                in
                case existingEntrySvgs of
                    [] ->
                        circleSvg { radius = strokeWidth }
                            [ listenToDragStart
                            , domModifierFillUniform valueLookupBackgroundColor
                            ]

                    entry0 :: entry1Up ->
                        let
                            svgWithoutEntryHoles : () -> SizedSvg { dragged : DragState, value : Maybe ValueUiState }
                            svgWithoutEntryHoles () =
                                let
                                    entryListSvg : SizedSvg { dragged : DragState, value : Maybe ValueUiState }
                                    entryListSvg =
                                        (entry0 :: entry1Up) |> svgSizedVertical
                                in
                                sizedSvgStack []
                                    [ sizedSvgRoundedRect
                                        [ listenToDragStart
                                        , domModifierFillUniform valueLookupBackgroundColor
                                        ]
                                        { radius = strokeWidth
                                        , width = entryListSvg.width
                                        , height = entryListSvg.height
                                        }
                                    , entryListSvg
                                    ]
                        in
                        case dragState of
                            Nothing ->
                                svgWithoutEntryHoles ()

                            Just dragged ->
                                case dragged.block of
                                    BlockFact _ ->
                                        svgWithoutEntryHoles ()

                                    BlockValue (Variable _) ->
                                        svgWithoutEntryHoles ()

                                    BlockValue (ValueLookup draggedValueLookup) ->
                                        let
                                            entriesWidthMaximum : Float
                                            entriesWidthMaximum =
                                                (entry0 :: entry1Up) |> List.map .width |> List.maximum |> Maybe.withDefault 0

                                            entriesInsertHoleSvg : Int -> SizedSvg { dragged : DragState, value : Maybe ValueUiState }
                                            entriesInsertHoleSvg insertIndex =
                                                sizedSvgRoundedRect
                                                    [ domModifierFillUniform
                                                        (valueLookupBackgroundColor
                                                            |> colorBrightnessScaleBy missingThingBrightnessScale
                                                        )
                                                    ]
                                                    { radius = strokeWidth
                                                    , width = entriesWidthMaximum
                                                    , height = fontSize + strokeWidth
                                                    }
                                                    |> List.singleton
                                                    |> sizedSvgStack
                                                        [ Web.Dom.listenTo "pointerup"
                                                            |> Web.Dom.modifierFutureMap
                                                                (\_ ->
                                                                    { dragged = Nothing
                                                                    , value =
                                                                        Just
                                                                            (ValueLookup
                                                                                (draggedValueLookup
                                                                                    |> List.foldr
                                                                                        (\draggedEntry soFar ->
                                                                                            if valueLookup |> List.any (\entry -> entry.key == draggedEntry.key) then
                                                                                                soFar
                                                                                                    |> List.map
                                                                                                        (\entry ->
                                                                                                            if entry.key == draggedEntry.key then
                                                                                                                draggedEntry

                                                                                                            else
                                                                                                                entry
                                                                                                        )

                                                                                            else
                                                                                                soFar
                                                                                                    |> List.LocalExtra.insertElementAtIndex
                                                                                                        insertIndex
                                                                                                        draggedEntry
                                                                                        )
                                                                                        valueLookup
                                                                                )
                                                                            )
                                                                    }
                                                                )
                                                        ]

                                            entriesSvg : SizedSvg { dragged : DragState, value : Maybe ValueUiState }
                                            entriesSvg =
                                                List.LocalExtra.interweave
                                                    (List.range 0 ((entry0 :: entry1Up) |> List.length)
                                                        |> List.map entriesInsertHoleSvg
                                                    )
                                                    (entry0 :: entry1Up)
                                                    |> svgSizedVertical
                                        in
                                        sizedSvgStack []
                                            [ sizedSvgRoundedRect
                                                [ listenToDragStart
                                                , domModifierFillUniform valueLookupBackgroundColor
                                                ]
                                                { radius = strokeWidth
                                                , width = entriesSvg.width
                                                , height = entriesSvg.height
                                                }
                                            , entriesSvg
                                            ]


factSvg :
    DragState
    -> FactUiState
    -> SizedSvg { dragged : DragState, fact : Maybe FactUiState }
factSvg dragState fact =
    case fact of
        All parts ->
            blockVerticalFactListSvg
                { name = "all"
                , fact = All parts
                , dragState = dragState
                , elements = parts
                , color = factAllBackgroundColor
                }
                |> sizedSvgFutureMap
                    (\future ->
                        { dragged = future.dragged
                        , fact = Maybe.map All future.elements
                        }
                    )

        Any branches ->
            blockVerticalFactListSvg
                { name = "any"
                , fact = Any branches
                , dragState = dragState
                , elements = branches
                , color = factAnyBackgroundColor
                }
                |> sizedSvgFutureMap
                    (\future ->
                        { dragged = future.dragged
                        , fact = Maybe.map Any future.elements
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
                , shapeListenModifier =
                    domListenToPointerDown
                        |> Web.Dom.modifierFutureMap
                            (\pointer ->
                                { dragged =
                                    Just
                                        { x = pointer.x
                                        , y = pointer.y
                                        , block = BlockFact (Not maybeFactInverse)
                                        }
                                , fact = Nothing
                                }
                            )
                }

        Equal toEquate ->
            factEqualsSvgWithInteractivity
                { valueASvg =
                    valueOrHoleSvg relationBackgroundColor dragState toEquate.a
                        |> sizedSvgFutureMap
                            (\futureA ->
                                { dragged = futureA.dragged
                                , fact = Just (Equal { a = futureA.value, b = toEquate.b })
                                }
                            )
                , valueBSvg =
                    valueOrHoleSvg relationBackgroundColor dragState toEquate.b
                        |> sizedSvgFutureMap
                            (\futureB ->
                                { dragged = futureB.dragged
                                , fact = Just (Equal { a = toEquate.a, b = futureB.value })
                                }
                            )
                , shapeEventListenModifier =
                    domListenToPointerDown
                        |> Web.Dom.modifierFutureMap
                            (\pointer ->
                                { dragged =
                                    Just
                                        { x = pointer.x
                                        , y = pointer.y
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
                            (\pointer ->
                                { dragged =
                                    Just
                                        { x = pointer.x
                                        , y = pointer.y
                                        , block = BlockFact (RelationUse relationUse)
                                        }
                                , fact = Nothing
                                }
                            )
                , identifier = sizedSvgUnselectableText relationUse.identifier
                , argumentSvg =
                    case relationUse.argument of
                        Nothing ->
                            valueHoleSvg relationBackgroundColor dragState
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


valueShapeSvg : ValueUiState -> SizedSvg future_
valueShapeSvg value =
    case value of
        Variable variableName ->
            variableShapeSvg variableName

        ValueLookup valueLookup ->
            valueLookupShapeSvg valueLookup


factNotShapeSvg : Maybe FactUiState -> SizedSvg future_
factNotShapeSvg maybeFactInverse =
    factNotSvgWithInteractivity
        { factInverseSvg =
            factOrHoleShapeSvg factNotBackgroundColor maybeFactInverse
        , shapeListenModifier = Web.Dom.modifierNone
        }


factAllShapeSvg : List FactUiState -> SizedSvg future_
factAllShapeSvg parts =
    blockVerticalFactListShapeSvg
        { name = "all"
        , color = factAllBackgroundColor
        , elements = parts
        }


factAnyShapeSvg : List FactUiState -> SizedSvg future_
factAnyShapeSvg branches =
    blockVerticalFactListShapeSvg
        { name = "any"
        , color = factAnyBackgroundColor
        , elements = branches
        }


valueLookupShapeSvg : List { key : String, value : Maybe ValueUiState } -> SizedSvg future_
valueLookupShapeSvg valueLookup =
    let
        entrySvgs : List (SizedSvg future_)
        entrySvgs =
            valueLookup
                |> List.map
                    (\entry ->
                        valueLookupEntryContentSvg
                            { key = entry.key
                            , value = valueOrHoleShapeSvg valueLookupBackgroundColor entry.value
                            }
                    )
    in
    case entrySvgs of
        [] ->
            circleSvg { radius = strokeWidth }
                [ domModifierFillUniform valueLookupBackgroundColor
                ]

        entry0 :: entry1Up ->
            let
                entryListSvg : SizedSvg future_
                entryListSvg =
                    (entry0 :: entry1Up) |> svgSizedVertical
            in
            sizedSvgStack []
                [ sizedSvgRoundedRect
                    [ domModifierFillUniform valueLookupBackgroundColor
                    ]
                    { radius = strokeWidth
                    , width = entryListSvg.width
                    , height = entryListSvg.height
                    }
                , entryListSvg
                ]


domStyleBackgroundColor : Color -> Web.Dom.Modifier future_
domStyleBackgroundColor color =
    Web.Dom.style "background-color" (color |> Color.toCssString)


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
    , let
        createVariableSvg : SizedSvg State
        createVariableSvg =
            case state.createVariable of
                DoneEditing variableName ->
                    sizedSvgHorizontalCentered
                        [ variableSvgWithInteractivity
                            { variableNameSvg =
                                sizedSvgUnselectableText variableName
                                    |> sizedSvgPad
                                        { left = strokeWidth / 2
                                        , right = strokeWidth / 2
                                        , top = strokeWidth / 2
                                        , bottom = strokeWidth / 2
                                        }
                            , dragStart =
                                domListenToPointerDown
                                    |> Web.Dom.modifierFutureMap
                                        (\pointer ->
                                            { state
                                                | createVariable = Editing ""
                                                , dragged =
                                                    Just
                                                        { x = pointer.x
                                                        , y = pointer.y
                                                        , block = BlockValue (Variable variableName)
                                                        }
                                            }
                                        )
                            }
                        , sizedSvgRenameButton
                            |> sizedSvgFutureMap (\() -> { state | createVariable = Editing variableName })
                        ]

                Editing variableName ->
                    variableSvgWithInteractivity
                        { variableNameSvg =
                            svgSizedTextInput variableName
                                |> sizedSvgFutureMap
                                    (\future ->
                                        { state | createVariable = future }
                                    )
                                |> sizedSvgPad
                                    { left = strokeWidth / 2
                                    , right = strokeWidth / 2
                                    , top = strokeWidth / 2
                                    , bottom = strokeWidth / 2
                                    }
                        , dragStart = Web.Dom.modifierNone
                        }

        createEntryKeySvg : SizedSvg State
        createEntryKeySvg =
            case state.createEntryKey of
                DoneEditing entryKey ->
                    sizedSvgHorizontalCentered
                        [ valueLookupShapeSvg
                            [ { key = entryKey, value = Nothing } ]
                            |> List.singleton
                            |> sizedSvgStack
                                [ domListenToPointerDown
                                    |> Web.Dom.modifierFutureMap
                                        (\pointer ->
                                            { state
                                                | createEntryKey = Editing ""
                                                , dragged =
                                                    Just
                                                        { x = pointer.x
                                                        , y = pointer.y
                                                        , block = BlockValue (ValueLookup [ { key = entryKey, value = Nothing } ])
                                                        }
                                            }
                                        )
                                ]
                        , sizedSvgRenameButton
                            |> sizedSvgFutureMap (\() -> { state | createEntryKey = Editing entryKey })
                        ]

                Editing entryKey ->
                    let
                        variableNameTextInputSvg : SizedSvg State
                        variableNameTextInputSvg =
                            svgSizedTextInput entryKey
                                |> sizedSvgFutureMap
                                    (\future ->
                                        { state
                                            | createEntryKey = future
                                        }
                                    )
                                |> sizedSvgPad
                                    { left = strokeWidth / 2
                                    , right = strokeWidth / 2
                                    , top = strokeWidth / 2
                                    , bottom = strokeWidth / 2
                                    }

                        valueLookupContentSvg : SizedSvg State
                        valueLookupContentSvg =
                            sizedSvgHorizontalCentered
                                [ variableNameTextInputSvg
                                , valueHoleShapeSvg valueLookupBackgroundColor
                                ]
                    in
                    sizedSvgStack []
                        [ sizedSvgRoundedRect
                            [ domModifierFillUniform valueLookupBackgroundColor
                            ]
                            { width = valueLookupContentSvg.width
                            , height = valueLookupContentSvg.height
                            , radius = strokeWidth
                            }
                        , valueLookupContentSvg
                        ]

        createRelationDefinitionSvg : SizedSvg State
        createRelationDefinitionSvg =
            let
                identifierTextInputSvg : SizedSvg State
                identifierTextInputSvg =
                    svgSizedTextInput state.createRelationIdentifier
                        |> sizedSvgFutureMap
                            (\future ->
                                case future of
                                    Editing editing ->
                                        { state
                                            | createRelationIdentifier = editing
                                        }

                                    DoneEditing newIdentifier ->
                                        { state
                                            | createRelationIdentifier = ""
                                            , relationDefinitionShown =
                                                { identifier = newIdentifier
                                                , parameter = Nothing
                                                , equivalentFact = Nothing
                                                }
                                            , relationDefinitionsNotShown =
                                                state.relationDefinitionsNotShown
                                                    |> FastDict.insert state.relationDefinitionShown.identifier
                                                        { parameter = state.relationDefinitionShown.parameter
                                                        , equivalentFact = state.relationDefinitionShown.equivalentFact
                                                        }
                                        }
                            )
            in
            relationUseSvgWithInteractivity
                { shapeEventListenModifier = Web.Dom.modifierNone
                , identifier = identifierTextInputSvg
                , argumentSvg =
                    valueHoleShapeSvg relationBackgroundColor
                }

        sidebarContent : SizedSvg State
        sidebarContent =
            svgSizedVertical
                [ factNotShapeSvg Nothing
                    |> List.singleton
                    |> sizedSvgStack
                        [ domListenToPointerDown
                            |> Web.Dom.modifierFutureMap
                                (\pointer ->
                                    { state
                                        | dragged =
                                            Just
                                                { x = pointer.x
                                                , y = pointer.y
                                                , block = BlockFact (Not Nothing)
                                                }
                                    }
                                )
                        ]
                , factAllShapeSvg []
                    |> List.singleton
                    |> sizedSvgStack
                        [ domListenToPointerDown
                            |> Web.Dom.modifierFutureMap
                                (\pointer ->
                                    { state
                                        | dragged =
                                            Just
                                                { x = pointer.x
                                                , y = pointer.y
                                                , block = BlockFact (All [])
                                                }
                                    }
                                )
                        ]
                , factAnyShapeSvg []
                    |> List.singleton
                    |> sizedSvgStack
                        [ domListenToPointerDown
                            |> Web.Dom.modifierFutureMap
                                (\pointer ->
                                    { state
                                        | dragged =
                                            Just
                                                { x = pointer.x
                                                , y = pointer.y
                                                , block = BlockFact (Any [])
                                                }
                                    }
                                )
                        ]
                    |> sizedSvgPad
                        { left = 0
                        , right = 0
                        , top = 0
                        , bottom = strokeWidth
                        }
                , valueLookupShapeSvg []
                    |> List.singleton
                    |> sizedSvgStack
                        [ domListenToPointerDown
                            |> Web.Dom.modifierFutureMap
                                (\pointer ->
                                    { state
                                        | dragged =
                                            Just
                                                { x = pointer.x
                                                , y = pointer.y
                                                , block = BlockValue (ValueLookup [])
                                                }
                                    }
                                )
                        ]
                , createEntryKeySvg
                , Set.union (state.relationDefinitionShown |> relationDefinitionEntryKeys)
                    (state.relationDefinitionsNotShown
                        |> fastDictSetFlatMap
                            (\identifier info ->
                                relationDefinitionEntryKeys
                                    { identifier = identifier
                                    , parameter = info.parameter
                                    , equivalentFact = info.equivalentFact
                                    }
                            )
                    )
                    |> Set.toList
                    |> List.map
                        (\entryKey ->
                            valueLookupShapeSvg [ { key = entryKey, value = Nothing } ]
                                |> List.singleton
                                |> sizedSvgStack
                                    [ domListenToPointerDown
                                        |> Web.Dom.modifierFutureMap
                                            (\pointer ->
                                                { state
                                                    | dragged =
                                                        Just
                                                            { x = pointer.x
                                                            , y = pointer.y
                                                            , block = BlockValue (ValueLookup [ { key = entryKey, value = Nothing } ])
                                                            }
                                                }
                                            )
                                    ]
                        )
                    |> svgSizedVertical
                    |> sizedSvgPad
                        { left = 0
                        , right = 0
                        , top = 0
                        , bottom = strokeWidth
                        }
                , createVariableSvg
                    |> sizedSvgPad
                        { left = 0
                        , right = 0
                        , top = 0
                        , bottom = 0
                        }
                , let
                    variables : Set String
                    variables =
                        state.relationDefinitionShown |> relationDefinitionVariables
                  in
                  variables
                    |> Set.toList
                    |> List.map
                        (\availableVariable ->
                            variableShapeSvg availableVariable
                                |> List.singleton
                                |> sizedSvgStack
                                    [ domListenToPointerDown
                                        |> Web.Dom.modifierFutureMap
                                            (\pointer ->
                                                { state
                                                    | dragged =
                                                        Just
                                                            { x = pointer.x
                                                            , y = pointer.y
                                                            , block = BlockValue (Variable availableVariable)
                                                            }
                                                }
                                            )
                                    ]
                        )
                    |> svgSizedVertical
                    |> sizedSvgPad
                        { left = 0
                        , right = 0
                        , bottom = strokeWidth
                        , top = 0
                        }
                , factEqualsShapeSvg { a = Nothing, b = Nothing }
                    |> List.singleton
                    |> sizedSvgStack
                        [ domListenToPointerDown
                            |> Web.Dom.modifierFutureMap
                                (\pointer ->
                                    { state
                                        | dragged =
                                            Just
                                                { x = pointer.x
                                                , y = pointer.y
                                                , block =
                                                    BlockFact (Equal { a = Nothing, b = Nothing })
                                                }
                                    }
                                )
                        ]
                , createRelationDefinitionSvg
                , let
                    relationIdentifiers : Set String
                    relationIdentifiers =
                        Set.insert state.relationDefinitionShown.identifier
                            (state.relationDefinitionsNotShown |> FastDict.keys |> Set.fromList)
                  in
                  relationIdentifiers
                    |> Set.toList
                    |> List.map
                        (\relationIdentifier ->
                            relationUseShapeSvg { identifier = relationIdentifier, argument = Nothing }
                                |> List.singleton
                                |> sizedSvgStack
                                    [ domListenToPointerDown
                                        |> Web.Dom.modifierFutureMap
                                            (\pointer ->
                                                { state
                                                    | dragged =
                                                        Just
                                                            { x = pointer.x
                                                            , y = pointer.y
                                                            , block =
                                                                BlockFact
                                                                    (RelationUse
                                                                        { identifier = relationIdentifier
                                                                        , argument = Nothing
                                                                        }
                                                                    )
                                                            }
                                                }
                                            )
                                    ]
                        )
                    |> svgSizedVertical
                ]
                |> sizedSvgPad
                    { left = strokeWidth
                    , right = strokeWidth
                    , top = strokeWidth * 2
                    , bottom = 0
                    }
      in
      Web.Dom.element "div"
        [ domStyleBackgroundColor (Color.rgb 0 0 0)
        , domStyleColor (Color.rgb 1 1 1)
        , Web.Dom.style "position" "fixed"
        , Web.Dom.style "top" "0"
        , Web.Dom.style "left" "0"
        , Web.Dom.style "font-size" (fontSize |> String.fromFloat)
        , Web.Dom.style "font-family" "Liga NovaMono"
        , case state.dragged of
            Nothing ->
                Web.Dom.modifierNone

            Just _ ->
                Web.Dom.style "cursor" "grabbing"
        ]
        [ domSvgContainer
            { left = 0
            , top = 0
            , bottom = state.windowWidth
            , right = state.windowWidth
            }
            [ Web.Dom.attribute "width" ((state.windowWidth |> String.fromInt) ++ "px")
            , Web.Dom.attribute "height" ((state.windowWidth |> String.fromInt) ++ "px")
            , case state.dragged of
                Nothing ->
                    Web.Dom.modifierNone

                Just stateDragged ->
                    domListenToPointerMove
                        |> Web.Dom.modifierFutureMap
                            (\position ->
                                { state
                                    | dragged =
                                        Just
                                            { x = position.x
                                            , y = position.y
                                            , block = stateDragged.block
                                            }
                                }
                            )
            ]
            [ svgStack
                [ case state.dragged of
                    Nothing ->
                        Web.Dom.modifierNone

                    Just _ ->
                        Web.Dom.listenTo "pointerup"
                            |> Web.Dom.modifierFutureMap
                                (\_ -> { state | dragged = Nothing })
                ]
                [ Web.Svg.element "rect"
                    [ Web.Dom.attribute "width"
                        ((sidebarContent.width |> String.fromFloat)
                            ++ "px"
                        )
                    , Web.Dom.attribute "height" "100%"
                    , domModifierFillUniform (Color.rgb 0.03 0.02 0)
                    ]
                    []
                , sidebarContent.svg
                ]
            , svgStack
                [ svgAttributeTranslate { x = sidebarContent.width, y = 0 }
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
                                                    { x = dragged.x + dragOffsetX - sidebarContent.width
                                                    , y = dragged.y + dragOffsetY
                                                    , block = dragged.block
                                                    }
                                    }
                                )
                ]
                [ Web.Svg.element "rect"
                    [ Web.Dom.attribute "width"
                        (((state.windowWidth |> Basics.toFloat) - sidebarContent.width |> String.fromFloat)
                            ++ "px"
                        )
                    , Web.Dom.attribute "height" "100%"
                    , domModifierFillUniform (Color.rgb 0 0 0)
                    ]
                    []
                , relationDefinitionSvg state.dragged
                    state.relationDefinitionShown
                    |> sizedSvgFutureMap
                        (\relationUiState ->
                            { state
                                | dragged = relationUiState.dragged
                                , relationDefinitionShown =
                                    relationUiState.relationDefinition
                            }
                        )
                    |> sizedSvgPad { bottom = 0, top = strokeWidth * 2, left = 0, right = 0 }
                    |> .svg
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
                                |> svgStack
                                    [ svgAttributeTranslate
                                        { x = strayThing.x, y = strayThing.y }
                                    ]
                        )
                    |> svgStack []
                ]
            , case state.dragged of
                Nothing ->
                    svgStack [] []

                Just dragged ->
                    svgStack
                        [ svgAttributeTranslate
                            { x = dragged.x + dragOffsetX
                            , y = dragged.y + dragOffsetY
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


domListenToPointerMove : Web.Dom.Modifier { x : Float, y : Float }
domListenToPointerMove =
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
                        position

                    Err _ ->
                        -- silent failure :(
                        { x = 0, y = 0 }
            )


domStyleColor : Color -> Web.Dom.Modifier future_
domStyleColor color =
    Web.Dom.style "color" (color |> Color.toCssString)


relationDefinitionEntryKeys :
    { identifier : String
    , parameter : Maybe ValueUiState
    , equivalentFact : Maybe FactUiState
    }
    -> Set String
relationDefinitionEntryKeys =
    \relationDefinition ->
        Set.union (relationDefinition.parameter |> maybeValueEntryKeys)
            (relationDefinition.equivalentFact |> maybeFactEntryKeys)


relationDefinitionVariables :
    { identifier : String
    , parameter : Maybe ValueUiState
    , equivalentFact : Maybe FactUiState
    }
    -> Set String
relationDefinitionVariables =
    \relationDefinition ->
        Set.union (relationDefinition.equivalentFact |> maybeFactVariables)
            (relationDefinition.parameter |> maybeValueVariables)


colorTransparent : Color
colorTransparent =
    Color.rgba 0 0 0 0


sizedSvgRenameButton : SizedSvg ()
sizedSvgRenameButton =
    let
        iconSvg : SizedSvg future
        iconSvg =
            --✎
            sizedSvgText [] "🖍"
                |> sizedSvgPad { left = fontWidth / 4, right = fontWidth / 4, top = 0, bottom = 0 }

        underline :
            List (Web.Dom.Modifier future)
            -> { width : Float, height : Float, radius : Float }
            -> { width : Float, height : Float, svg : Web.Dom.Node future }
        underline modifiers geometry =
            { width = geometry.width
            , height = geometry.height
            , svg =
                svgOpenPath
                    modifiers
                    { start = { x = geometry.width + geometry.radius, y = geometry.height - geometry.radius }
                    , trail =
                        [ pathSegmentBottomRightQuarterArcCounterclockwise { radius = geometry.radius, end = { x = geometry.width, y = geometry.height } }
                        , [ Svg.PathD.L ( 0, geometry.height ) ]
                        , pathSegmentBottomLeftQuarterArcCounterclockwise { radius = geometry.radius, end = { x = -geometry.radius, y = geometry.height - geometry.radius } }
                        ]
                            |> List.concat
                    }
            }
    in
    sizedSvgStack
        [ domListenToPointerDown |> Web.Dom.modifierFutureMap (\_ -> ())
        , Web.Dom.style "cursor" "pointer"
        ]
        [ underline
            [ domStrokeColor (Color.rgb 1 1 1)
            , domStrokeWidth 1
            , domModifierFillUniform colorTransparent
            ]
            { width = iconSvg.width
            , height = iconSvg.height
            , radius = fontWidth
            }
        , iconSvg
        ]
        |> sizedSvgPad
            { left = strokeWidth / 2
            , right = strokeWidth / 2
            , top = strokeWidth / 2
            , bottom = strokeWidth / 2
            }


domStrokeColor : Color -> Web.Dom.Modifier future_
domStrokeColor color =
    Web.Dom.style "stroke" (color |> Color.toCssString)


domStrokeWidth : Float -> Web.Dom.Modifier future_
domStrokeWidth strokeWidthInPixels =
    Web.Dom.style "stroke-width" ((strokeWidthInPixels |> String.fromFloat) ++ "px")


svgOpenPath :
    List (Web.Dom.Modifier future)
    -> { start : { x : Float, y : Float }, trail : List Svg.PathD.Segment }
    -> Web.Dom.Node future
svgOpenPath modifiers segments =
    Web.Svg.element "path"
        (Web.Dom.attribute "d"
            (Svg.PathD.pathD
                (case segments.trail of
                    [] ->
                        []

                    trailSegment0 :: trailSegment1Up ->
                        Svg.PathD.M ( segments.start.x, segments.start.y )
                            :: (trailSegment0 :: trailSegment1Up)
                )
            )
            :: modifiers
        )
        []


svgSizedTextInput : String -> SizedSvg TextInputUiState
svgSizedTextInput currentString =
    let
        characterCountSize : Int
        characterCountSize =
            (currentString |> String.length) + 2

        textInputWidth : Float
        textInputWidth =
            Basics.max (fontWidth * 10)
                (fontWidth * (characterCountSize |> Basics.toFloat))

        textInputHeight : Float
        textInputHeight =
            fontSize

        submit : String -> TextInputUiState
        submit newString =
            case newString of
                "" ->
                    Editing ""

                nonBlankNewString ->
                    DoneEditing nonBlankNewString
    in
    { height = textInputHeight
    , width = textInputWidth
    , svg =
        Web.Svg.element "foreignObject"
            [ Web.Dom.style "x" "0px"
            , Web.Dom.style "y" "-2px"
            , Web.Dom.style "width" ((textInputWidth |> String.fromFloat) ++ "px")
            , Web.Dom.style "height" ((textInputHeight + 4 |> String.fromFloat) ++ "px")
            ]
            [ Web.Dom.element "fieldset"
                [ Web.Dom.style "height" ((fontSize |> String.fromFloat) ++ "px")
                , Web.Dom.style "width" ((textInputWidth |> String.fromFloat) ++ "px")
                , Web.Dom.style "border" "none"
                ]
                [ Web.Dom.element "input"
                    [ Web.Dom.attribute "type" "text"
                    , domStyleBackgroundColor colorTransparent
                    , Web.Dom.style "color" "inherit"
                    , Web.Dom.style "border-left" "none"
                    , Web.Dom.style "border-right" "none"
                    , Web.Dom.style "border-top" "none"
                    , Web.Dom.style "border-bottom" "dotted 2px"
                    , Web.Dom.style "margin" "-2px"
                    , Web.Dom.style "position" "absolute"
                    , Web.Dom.style "left" "0"
                    , Web.Dom.style "top" "0"
                    , Web.Dom.style "font-size" "1em"
                    , Web.Dom.style "font-family" "inherit"
                    , Web.Dom.style "vertical-align" "baseline"
                    , Web.Dom.style "outline" "none"
                    , Web.Dom.style "height" "100%"
                    , Web.Dom.style "width" ((textInputWidth |> String.fromFloat) ++ "px") --"100%"
                    , Web.Dom.attribute "size" (characterCountSize |> String.fromInt)
                    , Web.Dom.stringProperty "value" currentString
                    , Web.Dom.listenTo "input"
                        |> Web.Dom.modifierFutureMap
                            (\inputEventJson ->
                                case
                                    inputEventJson
                                        |> Json.Decode.decodeValue
                                            (Json.Decode.field "target" (Json.Decode.field "value" Json.Decode.string))
                                of
                                    Err _ ->
                                        Editing currentString

                                    Ok newName ->
                                        Editing newName
                            )
                    , Web.Dom.listenTo "keydown"
                        |> Web.Dom.modifierFutureMap
                            (\keyDownEventJson ->
                                case
                                    keyDownEventJson
                                        |> Json.Decode.decodeValue
                                            (let
                                                enterKeyJsonDecode : Int -> Json.Decode.Decoder ()
                                                enterKeyJsonDecode keyCode =
                                                    if keyCode == 13 then
                                                        Json.Decode.succeed ()

                                                    else
                                                        Json.Decode.fail "non-enter"
                                             in
                                             Json.Decode.andThen enterKeyJsonDecode
                                                (Json.Decode.field "keyCode" Json.Decode.int)
                                            )
                                of
                                    Err _ ->
                                        Editing currentString

                                    Ok () ->
                                        submit currentString
                            )
                    , Web.Dom.listenTo "blur"
                        |> Web.Dom.modifierFutureMap
                            (\_ -> submit currentString)
                    ]
                    []
                ]
            ]
    }


dragOffsetX : Float
dragOffsetX =
    -strokeWidth


dragOffsetY : Float
dragOffsetY =
    -strokeWidth


domSvgContainer :
    { left : Int, top : Int, right : Int, bottom : Int }
    -> List (Web.Dom.Modifier future)
    -> List (Web.Dom.Node future)
    -> Web.Dom.Node future
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


relationDefinitionSvg :
    DragState
    ->
        { identifier : String
        , parameter : Maybe ValueUiState
        , equivalentFact : Maybe FactUiState
        }
    ->
        SizedSvg
            { dragged : DragState
            , relationDefinition :
                { identifier : String
                , parameter : Maybe ValueUiState
                , equivalentFact : Maybe FactUiState
                }
            }
relationDefinitionSvg dragState definition =
    relationDefinitionSvgWithInteractivity
        { identifier = sizedSvgUnselectableText definition.identifier
        , parameter =
            valueOrHoleSvg relationBackgroundColor dragState definition.parameter
                |> sizedSvgFutureMap
                    (\futureArgumentUiState ->
                        { dragged = futureArgumentUiState.dragged
                        , relationDefinition =
                            { identifier = definition.identifier
                            , equivalentFact = definition.equivalentFact
                            , parameter = futureArgumentUiState.value
                            }
                        }
                    )
        , equivalentFact =
            factOrHoleSvg relationBackgroundColor dragState definition.equivalentFact
                |> sizedSvgFutureMap
                    (\equivalentFactUiState ->
                        { dragged = equivalentFactUiState.dragged
                        , relationDefinition =
                            { identifier = definition.identifier
                            , equivalentFact = equivalentFactUiState.fact
                            , parameter = definition.parameter
                            }
                        }
                    )
        }


relationDefinitionSvgWithInteractivity :
    { identifier : SizedSvg future
    , parameter : SizedSvg future
    , equivalentFact : SizedSvg future
    }
    -> SizedSvg future
relationDefinitionSvgWithInteractivity definition =
    svgSizedVertical
        [ relationUseSvgWithInteractivity
            { shapeEventListenModifier = Web.Dom.modifierNone
            , identifier = definition.identifier
            , argumentSvg = definition.parameter
            }
            |> sizedSvgPad
                { left = 0, right = 0, top = 0, bottom = strokeWidth }
        , sizedSvgUnselectableText "is equivalent to"
            |> sizedSvgPad
                { left = 0, right = 0, top = 0, bottom = strokeWidth }
        , definition.equivalentFact
        ]


fastDictSetFlatMap :
    (comparableDictKey -> value -> Set comparableSetElement)
    -> (FastDict.Dict comparableDictKey value -> Set comparableSetElement)
fastDictSetFlatMap entryToSet =
    \dict ->
        dict
            |> FastDict.foldl
                (\key value soFar ->
                    Set.union (entryToSet key value) soFar
                )
                Set.empty


type alias SizedSvg future =
    { height : Float, width : Float, svg : Web.Dom.Node future }


factOrHoleShapeSvg : Color -> Maybe FactUiState -> SizedSvg future_
factOrHoleShapeSvg backgroundColor fact =
    case fact of
        Nothing ->
            factInsertHoleShapeSvg backgroundColor

        Just equivalentFact ->
            factShapeSvg equivalentFact


verticalFactListPolygonPoints :
    { headerWidth : Float
    , headerHeight : Float
    , sideWidth : Float
    , elementsHeight : Float
    , elementsWidth : Float
    }
    -> List ( Float, Float )
verticalFactListPolygonPoints sizes =
    let
        fullHeight : Float
        fullHeight =
            sizes.headerHeight
                + sizes.elementsHeight
                + sizes.sideWidth

        fullWidth : Float
        fullWidth =
            sizes.sideWidth
                + Basics.max sizes.elementsWidth sizes.headerWidth
    in
    [ ( fullWidth, sizes.headerHeight )
    , ( fullWidth, 0 )
    , ( sizes.sideWidth, 0 )
    , ( 0, strokeWidth )
    , ( 0, sizes.headerHeight + sizes.elementsHeight )
    , ( sizes.sideWidth, fullHeight )
    , ( fullWidth, fullHeight )
    , ( fullWidth, sizes.headerHeight + sizes.elementsHeight )
    , ( sizes.sideWidth + strokeWidth, sizes.headerHeight + sizes.elementsHeight )
    , ( sizes.sideWidth + strokeWidth, sizes.headerHeight )
    ]


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
        sideWidth : Float
        sideWidth =
            strokeWidth

        blockNameStringSvg : SizedSvg future_
        blockNameStringSvg =
            sizedSvgUnselectableText config.name

        headerWidth : Float
        headerWidth =
            strokeWidth + blockNameStringSvg.width + strokeWidth

        headerHeight : Float
        headerHeight =
            fontSize + strokeWidth

        elementsAndInsertHolesSvg : SizedSvg { dragged : DragState, elements : List FactUiState }
        elementsAndInsertHolesSvg =
            let
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
                |> svgSizedVertical

        shapeSvg : SizedSvg { dragged : DragState, elements : Maybe (List FactUiState) }
        shapeSvg =
            sizedSvgPolygon
                [ domModifierFillUniform config.color
                , domListenToPointerDown
                    |> Web.Dom.modifierFutureMap
                        (\pointer ->
                            { dragged =
                                Just
                                    { x = pointer.x
                                    , y = pointer.y
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
                    , elementsWidth = elementsAndInsertHolesSvg.width
                    , elementsHeight = elementsAndInsertHolesSvg.height
                    }
                )
    in
    { width = shapeSvg.width
    , height = shapeSvg.height
    , svg =
        svgStack
            []
            [ shapeSvg.svg
            , svgStack
                [ svgAttributeTranslate
                    { x = sideWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ blockNameStringSvg.svg ]
            , svgStack
                [ svgAttributeTranslate { x = sideWidth, y = headerHeight } ]
                [ elementsAndInsertHolesSvg
                    |> .svg
                    |> Web.Dom.futureMap
                        (\future ->
                            { dragged = future.dragged, elements = Just future.elements }
                        )
                ]
            ]
    }


blockVerticalFactListShapeSvg :
    { elements : List FactUiState, name : String, color : Color }
    -> SizedSvg future_
blockVerticalFactListShapeSvg config =
    let
        sideWidth : Float
        sideWidth =
            strokeWidth

        elementsSvg : SizedSvg future_
        elementsSvg =
            (case config.elements of
                [] ->
                    [ factInsertHoleShapeSvg config.color ]

                branch0 :: branch1Up ->
                    (branch0 :: branch1Up)
                        |> List.map factShapeSvg
            )
                |> svgSizedVertical

        blockNameTextSvg : SizedSvg future_
        blockNameTextSvg =
            sizedSvgUnselectableText config.name

        headerHeight : Float
        headerHeight =
            fontSize + strokeWidth

        headerWidth : Float
        headerWidth =
            strokeWidth + blockNameTextSvg.width + strokeWidth

        shapeSvg : SizedSvg future_
        shapeSvg =
            sizedSvgPolygon
                [ domModifierFillUniform config.color
                ]
                (verticalFactListPolygonPoints
                    { headerWidth = headerWidth
                    , headerHeight = headerHeight
                    , sideWidth = sideWidth
                    , elementsWidth = elementsSvg.width
                    , elementsHeight = elementsSvg.height
                    }
                )
    in
    { width = shapeSvg.width
    , height = shapeSvg.height
    , svg =
        svgStack
            []
            [ shapeSvg.svg
            , svgStack
                [ svgAttributeTranslate
                    { x = sideWidth
                    , y = strokeWidth / 2
                    }
                ]
                [ blockNameTextSvg.svg ]
            , svgStack
                [ svgAttributeTranslate { x = sideWidth, y = headerHeight } ]
                [ elementsSvg.svg ]
            ]
    }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
