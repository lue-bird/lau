module Lau exposing
    ( Script, DefineElement, FeedElement, ScriptElement, ScriptElementKind(..)
    , coreScript
    , availableDefinesAt
    )

{-| Core language.

@docs Script, DefineElement, FeedElement, ScriptElement, ScriptElementKind
@docs coreScript


## operations

@docs availableDefinesAt

-}

import Bit exposing (Bit(..))
import Forest.Path
import Lau.DefinedId as Id exposing (DefinedId, id)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Serialize exposing (Codec)
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
        { definedId : DefinedId }


{-| Define something new
-}
type alias DefineElement =
    RecordWithoutConstructorFunction
        { id : DefinedId }


{-| What is defined in scope for a the thing at a given path?
-}
availableDefinesAt : TreePath -> Script -> List DefineElement
availableDefinesAt path =
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
                            (laterDefinitions |> availableDefinesAt (childPath |> Forest.Path.pathIntoTreeAtIndex))


{-| **In progress.**
Contains natively defined core definitions & definitions that build on those.


## language-native primitives

  - counting set/multiset/bag type. Will in the following be shown as `{ one, two }`
  - definition
  - partial application if parts of an argument set are missing


## derived features

  - empty type and empty value are just an empty set. D.h. giving an empty set is equivalent to not giving anything
  - a defined tag is a function `a -> { tag tag, value a }` by default (named tbd)
  - `choice` (untagged union/choice type) taking one case to `a` and returning a choice between `a` and the other possibilities.
    `choice { a, a }` will be seen as equivalent to `a` (and `choice { a, a, x }` will be seen as equivalent to `choice { a, x }`) once all have been matched
  - `list a ←def or ○ ({ #head a, #tail (list a) })`
  - number
      - `1+ ←unique-type-def & ○ natural`
      - `0 ←unique-type-def ○`
          - `bit ←def or 0 (1+ 0)`
          - `natural ←def or 0 (1+ natural)`
          - `positive ←def 1+ natural`
      - `negative ←def positive`
          - `whole ←def or natural negative`
      - `rational ←def & whole positive`
      - `digit ←def natural-in-range (& 0 9)`
          - `decimal ←def or (& decimal digit)`
  - `def-id ←def list-of 160 bit`
  - `character ←def natural-in-range (& 0 1114112)` see <https://en.wikipedia.org/wiki/Code_point>


### translation to elm code

Only when one of these functions gets called, the values are evaluated.

  - Svg
      - `graphics ←def & (list character) (& (list character) (& (list (graphics attribute)) (list graphics)))`
      - `graphics attribute ←def (list character) (list character)`

-}
coreScript : Script
coreScript =
    -- define 🧩 {name, as, in-(gets #id replaced with as shown as name)} = in
    -- tag defined as a function `value -> { #tag tag, #value value }`
    Tree.tree (Define { id = asId })
        [ Tree.tree (Define { id = inId })
            [ Tree.tree (Define { id = asId })
                [ Tree.tree (Feed { definedId = asId })
                    [ Tree.singleton (Feed { definedId = inId }) ]
                ]
            ]
        ]
        -- todo
        |> Tree.map
            (\kind ->
                { relativeLocation = Point2d.origin
                , kind = kind
                }
            )


{-| a throw-away id; results in not finding a function when searching
-}
inId : DefinedId
inId =
    id I O I I I O I O O O O O I I I I O O O O O I I O I O O I I O O I I I O O I I O O O I O I O O I I I I I O O I I I I O O O I O I O O O I O I O I O I O I I O O O O O I I I O O I O I O I O O O I I O O O I O I O I O I I I O O O I O O O I I O I I O I O I O O I I I O I I O I I I I I O O O O O I I O O I I I I I O O I O O I O O


asId : DefinedId
asId =
    id O I O O O O I O I I I I I O I I O I I I O O O O O I O I O I O O I O I I I I I O O I I I I I O I O O I O I I O O I I O I I O I I I I I I I I O O O I O I I O I O I O O O O O I O O O I I O O I O O I I I O I I O O I O I O I O O I I I O O O O I I I O I I I I O I I O O O O O O I I I I I O I I I I I O I I I O I O I I I O O O
