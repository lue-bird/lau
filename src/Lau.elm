module Lau exposing
    ( Script, DefineElement, FeedElement, ScriptElement, ScriptElementKind(..)
    , coreScript
    , definesAvailableAt, nativeDefines, nonNativeDefinesAvailableAt
    )

{-| Core language.

@docs Script, DefineElement, FeedElement, ScriptElement, ScriptElementKind
@docs coreScript


## observe

@docs definesAvailableAt, nativeDefines, nonNativeDefinesAvailableAt


## interpret in elm

Only when one of these functions gets called, the values are evaluated.

  - Svg
      - `graphics ←def & (list character) (& (list character) (& (list (graphics attribute)) (list graphics)))`
      - `graphics attribute ←def (list character) (list character)`

-}

import Bit exposing (Bit(..))
import Forest.Path
import Lau.DefineId exposing (DefineId)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Tree exposing (Tree)
import Tree.Navigate
import Tree.Path exposing (TreePath)


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

  - [`FeedElement`](#FeedElement)
  - [`DefineElement`](#DefineElement)

-}
type ScriptElementKind
    = Feed FeedElement
    | Define DefineElement


{-| Use an existing defined thing
-}
type alias FeedElement =
    RecordWithoutConstructorFunction
        { defineId : DefineId }


{-| Define something new
-}
type alias DefineElement =
    RecordWithoutConstructorFunction
        { id : DefineId }


{-| What is defined in scope for a the thing at a given path?
Consists of both [`nativeDefines`](#nativeDefines) and [`nonNativeDefinesAvailableAt`](#nonNativeDefinesAvailableAt) that given path.
-}
definesAvailableAt : TreePath -> Script -> List DefineElement
definesAvailableAt path =
    \script ->
        (script |> nonNativeDefinesAvailableAt path)
            ++ nativeDefines


inId : DefineId
inId =
    Lau.DefineId.raw I O I I I O I O O O O O I I I I O O O O O I I O I O O I I O O I I I O O I I O O O I O I O O I I I I I O O I I I I O O O I O I O O O I O I O I O I O I I O O O O O I I I O O I O I O I O O O I I O O O I O I O I O I I I O O O I O O O I I O I I O I O I O O I I I O I I O I I I I I O O O O O I I O O I I I I I O O I O O I O O


asId : DefineId
asId =
    Lau.DefineId.raw O I O O O O I O I I I I I O I I O I I I O O O O O I O I O I O O I O I I I I I O O I I I I I O I O O I O I I O O I I O I I O I I I I I I I I O O O I O I I O I O I O O O O O I O O O I I O O I O O I I I O I I O O I O I O I O O I I I O O O O I I I O I I I I O I I O O O O O O I I I I I O I I I I I O I I I O I O I I I O O O


{-| Native defines would lead to infinite define circles because they are needed in the define construct itself.

This makes them a bit ugly to work with, e.g. you can't jump to their definition

-}
nativeDefines : List DefineElement
nativeDefines =
    [ { id = asId }, { id = inId }, { id = nameId } ]


nameId : DefineId
nameId =
    Lau.DefineId.raw I I O O O I I O I I I O I I O O I O I I I O O O O O I I O I O O O I O I O O O O I I I O O O O I O O I O O I O I O O I I I I I I I O O O I O I I O I O O O O O O I O I I O I I O I O I O O O I I I O I O O O I I O I I I O I O I I O I O I I I O O O O O I I O O O I I O I O O I O O I O O I O O I O O I O I O I I I I O I O I O


{-| What is defined in scope for a the thing at a given path, excluding [`nativeDefines`](#nativeDefines)
-}
nonNativeDefinesAvailableAt : TreePath -> Script -> List DefineElement
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

                            Feed _ ->
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
