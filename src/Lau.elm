module Lau exposing
    ( Script, Define, Use, ScriptElement, ScriptElementKind(..)
    , coreScript
    , definesAvailableAt, nativeDefines, nonNativeDefinesAvailableAt
    , scriptMorphValue
    )

{-| Core language.

@docs Script, Define, Use, ScriptElement, ScriptElementKind
@docs coreScript


## observe

@docs definesAvailableAt, nativeDefines, nonNativeDefinesAvailableAt


## interpret in elm

Only when one of these functions gets called, the values are evaluated.

  - Svg
      - `graphics ←def & (list character) (& (list character) (& (list (graphics attribute)) (list graphics)))`
      - `graphics attribute ←def (list character) (list character)`


## serialize

@docs scriptMorphValue

-}

import Bit exposing (Bit(..))
import Decimal
import Decimal.Morph
import Float.Morph
import Forest.Path
import Lau.DefineId exposing (DefineId)
import Morph exposing (Morph, MorphOrError)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Tree exposing (Tree)
import Tree.Morph
import Tree.Navigate
import Tree.Path exposing (TreePath)
import Value.Morph exposing (MorphValue)


{-| A code branch. A sub-tree is a fed argument.
-}
type alias Script =
    Tree ScriptElement


{-| Info specific to one thing without its arguments
-}
type alias ScriptElement =
    { relativeLocation : Point2d Pixels Float
    , kind : ScriptElementKind
    }


{-| Info specific to its [`ScriptElement`](#ScriptElement) kind. Either

  - [`Use`](#Use)
  - [`Define`](#Define)

-}
type ScriptElementKind
    = Use Use
    | Define Define


{-| Use an existing defined thing
-}
type alias Use =
    RecordWithoutConstructorFunction
        { defineId : DefineId }


{-| Define something new
-}
type alias Define =
    RecordWithoutConstructorFunction
        { id : DefineId }


{-| What is defined in scope for a the thing at a given path?
Consists of both [`nativeDefines`](#nativeDefines) and [`nonNativeDefinesAvailableAt`](#nonNativeDefinesAvailableAt) that given path.
-}
definesAvailableAt : TreePath -> Script -> List Define
definesAvailableAt path =
    \script ->
        (script |> nonNativeDefinesAvailableAt path)
            ++ nativeDefines


{-| Native defines would lead to infinite define circles because they are needed in the define construct itself.

This makes them a bit ugly to work with, e.g. you can't jump to their definition

-}
nativeDefines : List Define
nativeDefines =
    [ { id = asId }, { id = inId }, { id = nameId } ]


inId : DefineId
inId =
    Lau.DefineId.raw I O I I I O I O O O O O I I I I O O O O O I I O I O O I I O O I I I O O I I O O O I O I O O I I I I I O O I I I I O O O I O I O O O I O I O I O I O I I O O O O O I I I O O I O I O I O O O I I O O O I O I O I O I I I O O O I O O O I I O I I O I O I O O I I I O I I O I I I I I O O O O O I I O O I I I I I O O I O O I O O


asId : DefineId
asId =
    Lau.DefineId.raw O I O O O O I O I I I I I O I I O I I I O O O O O I O I O I O O I O I I I I I O O I I I I I O I O O I O I I O O I I O I I O I I I I I I I I O O O I O I I O I O I O O O O O I O O O I I O O I O O I I I O I I O O I O I O I O O I I I O O O O I I I O I I I I O I I O O O O O O I I I I I O I I I I I O I I I O I O I I I O O O


nameId : DefineId
nameId =
    Lau.DefineId.raw I I O O O I I O I I I O I I O O I O I I I O O O O O I I O I O O O I O I O O O O I I I O O O O I O O I O O I O I O O I I I I I I I O O O I O I I O I O O O O O O I O I I O I I O I O I O O O I I I O I O O O I I O I I I O I O I I O I O I I I O O O O O I I O O O I I O I O O I O O I O O I O O I O O I O I O I I I I O I O I O


{-| What is defined in scope for a the thing at a given path, excluding [`nativeDefines`](#nativeDefines)
-}
nonNativeDefinesAvailableAt : TreePath -> Script -> List Define
nonNativeDefinesAvailableAt path =
    \script ->
        case path |> Tree.Path.step of
            Nothing ->
                []

            Just childPath ->
                case script |> Tree.Navigate.to (Tree.Path.follow [ childPath |> Forest.Path.treeIndex ]) of
                    Nothing ->
                        []

                    Just laterDefinitions ->
                        (case script |> Tree.label |> .kind of
                            Define definition ->
                                -- add as child and name child else identity
                                (::) definition

                            Use _ ->
                                identity
                        )
                            (laterDefinitions |> definesAvailableAt (childPath |> Forest.Path.pathIntoTreeAtIndex))


{-| **In progress.**
Contains natively defined core definitions & definitions that build on those.


## language primitives

  - counting set/multiset/bag type. Will in the following be shown as `{ one, two }`
  - definition 🧩 {name, as, in-(gets #id replaced with as shown as name)} where any inputs coming from `in` will know of this definition
  - partial application if parts of an argument set is missing


## language core defines

  - an empty value is just an empty set. Giving an empty set is equivalent to not giving anything
  - a defined tag is a function `a -> { tag tag, value a }` by default
    `choice { a, a }` will be seen as equivalent to `a` (and `choice { a, a, x }` will be seen as equivalent to `choice { a, x }`) once all have been matched
  - `list a ←def or ○ ({ #head a, #tail (list a) })`
  - number
      - `1+`, `0`
      - `negative`, `positive`
  - `character is a natural-in-range [0; 1114112]` see <https://en.wikipedia.org/wiki/Code_point>

-}
coreScript : Script
coreScript =
    -- define
    -- tag defined as a function `value -> { #tag tag, #value value }`
    Debug.todo ""
        -- todo
        |> Tree.map
            (\kind ->
                { relativeLocation = Point2d.origin
                , kind = kind
                }
            )



-- serialize


{-| [`MorphValue`](https://dark.elm.dmy.fr/packages/lue-bird/elm-morph/latest/Value-Morph#MorphValue) from a [`Script`](#Script)
-}
scriptMorphValue : MorphValue Script
scriptMorphValue =
    Tree.Morph.value scriptElementMorphValue


scriptElementMorphValue : MorphValue ScriptElement
scriptElementMorphValue =
    Value.Morph.group (\relativeLocation kind -> { relativeLocation = relativeLocation, kind = kind })
        |> Value.Morph.part ( .relativeLocation, "relativeLocation" ) point2dMorphValue
        |> Value.Morph.part ( .kind, "kind" ) scriptElementKindMorphValue
        |> Value.Morph.groupFinish


scriptElementKindMorphValue : MorphValue ScriptElementKind
scriptElementKindMorphValue =
    Morph.choice
        (\feedVariant defineVariant scriptElementKind ->
            case scriptElementKind of
                Use feed ->
                    feedVariant feed

                Define define ->
                    defineVariant define
        )
        |> Value.Morph.variant ( Use, "use" ) useElementMorphValue
        |> Value.Morph.variant ( Define, "define" ) defineMorphValue
        |> Value.Morph.choiceFinish


useElementMorphValue : MorphValue Use
useElementMorphValue =
    Value.Morph.group (\defineId -> { defineId = defineId })
        |> Value.Morph.part ( .defineId, "defineId" ) Lau.DefineId.morphValue
        |> Value.Morph.groupFinish


defineMorphValue : MorphValue Define
defineMorphValue =
    Value.Morph.group (\id -> { id = id })
        |> Value.Morph.part ( .id, "id" ) Lau.DefineId.morphValue
        |> Value.Morph.groupFinish



-- use


point2dMorphValue : MorphValue (Point2d Pixels Float)
point2dMorphValue =
    point2dMorphRecord pixelsMorphFloat
        |> Morph.over
            (Value.Morph.group (\x y -> { x = x, y = y })
                |> Value.Morph.part ( .x, "x" ) numberFloatMorphValue
                |> Value.Morph.part ( .y, "y" ) numberFloatMorphValue
                |> Value.Morph.groupFinish
            )


point2dMorphRecord :
    Morph.OneToOne (Quantity Float units) Float
    -> Morph (Point2d units Float) { x : Float, y : Float }
point2dMorphRecord quantityMorphFloat =
    Morph.oneToOneOn Point2d.fromRecord Point2d.toRecord quantityMorphFloat


pixelsMorphFloat : MorphOrError (Quantity Float Pixels) Float error_
pixelsMorphFloat =
    Morph.oneToOne Pixels.float Pixels.toFloat


numberFloatMorphValue : MorphValue Float
numberFloatMorphValue =
    Float.Morph.decimalOrException
        |> Morph.over (Morph.oneToOne Ok (Result.withDefault Decimal.N0))
        |> Morph.over Decimal.Morph.value
