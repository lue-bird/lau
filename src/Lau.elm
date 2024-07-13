module Lau exposing
    ( Lookup(..), ValueWithVariableName(..)
    , Fact, FactWithVariableName(..)
    , Project, evaluate
    , lookupToString
    )

{-| Language representation and evaluation

@docs Lookup, ValueWithVariableName
@docs Fact, FactWithVariableName
@docs Project, evaluate


## possible future explorations

if between branches, a definition is used with the same lookup (without variables),
use the cached value. (memoizing laziness).

Currently I'm not super excited to do this because it will

  - increase memory usage if not applied with some heuristic
  - complicate the code
  - make running in parallel harder (maybe? not sure),
    e.g. when [the elm HVM compiler](https://github.com/Janiczek/elm-bend) gets created

-}

import FastDict
import List.LauExtra


{-| A couple of relations.

Needs to include a definition named "main" if you want to [evaluate](#evaluate) it.

-}
type alias Project =
    { relationDefinitions :
        FastDict.Dict
            String
            { argumentVariable : String
            , equivalentFact : Fact
            }
    }


{-| Something to be satisfied. Some call it rule/constraint/assertion/...

  - relation use: applying a defined relation with a given value
  - all with bag of facts: each fact needs to be satisfied at the same time
  - any with a bag of facts: any number of facts >= 1 need to be satisfied at the same time (independent of order)
  - = with a bag of values: structural equality
  - not with a fact: satisfied if and only if the fact is not satisfied

-}
type alias Fact =
    FactWithVariableName String


{-| A version of [`Fact`](#Fact) that allows using a given type of variable name
-}
type FactWithVariableName variableName
    = RelationUse { identifier : String, argument : ValueWithVariableName variableName }
    | Equal { a : ValueWithVariableName variableName, b : ValueWithVariableName variableName }
    | Not (FactWithVariableName variableName)
    | All (List (FactWithVariableName variableName))
    | Any (List (FactWithVariableName variableName))


{-| A variable or lookup that can have variables for some of its entry values
-}
type ValueWithVariableName variableName
    = Variable variableName
    | ValueLookup (FastDict.Dict String (ValueWithVariableName variableName))


{-| A record/dictionary (entries with a name and value).

Anything that goes in, out and between of lau has this shape

-}
type Lookup
    = Lookup (FastDict.Dict String Lookup)


lookupToString : Lookup -> String
lookupToString =
    \(Lookup entries) ->
        [ "{"
        , entries
            |> FastDict.toList
            |> List.map (\( identifier, value ) -> [ identifier, " ", value |> lookupToString ] |> String.concat)
            |> String.join " "
        , "}"
        ]
            |> String.concat


factBranchInvert : FactBranch -> List FactBranch
factBranchInvert =
    \factBranch ->
        case factBranch of
            BranchValid whyValid ->
                -- TODO better error?
                BranchInvalid ("not " ++ whyValid) |> List.singleton

            BranchInvalid whyInvalid ->
                -- TODO better error?
                BranchValid ("not " ++ whyInvalid) |> List.singleton

            BranchValidRelation relation ->
                BranchInvalidRelation relation |> List.singleton

            BranchInvalidRelation relation ->
                BranchValidRelation relation |> List.singleton

            BranchValidVariableSubstitution substitution ->
                BranchInvalidVariableSubstitution substitution |> List.singleton

            BranchInvalidVariableSubstitution substitution ->
                BranchValidVariableSubstitution substitution |> List.singleton

            BranchAll parts ->
                parts |> List.concatMap factBranchInvert


valueVariablesMap :
    (variableName -> ValueWithVariableName mappedVariableName)
    ->
        (ValueWithVariableName variableName
         -> ValueWithVariableName mappedVariableName
        )
valueVariablesMap variableChange =
    \value ->
        case value of
            Variable valueVariable ->
                valueVariable |> variableChange

            ValueLookup valueLookup ->
                valueLookup
                    |> FastDict.map (\_ entryValue -> entryValue |> valueVariablesMap variableChange)
                    |> ValueLookup


valueToValueLookup :
    ValueWithVariableName variableName
    -> Maybe (FastDict.Dict String (ValueWithVariableName variableName))
valueToValueLookup =
    \value ->
        case value of
            Variable _ ->
                Nothing

            ValueLookup valueLookup ->
                valueLookup |> Just


scopedVariableIsDeeperInThan : ScopedVariableName -> (ScopedVariableName -> Bool)
scopedVariableIsDeeperInThan bVariable =
    \aVariable ->
        (aVariable.scope |> scopeDepth) > (bVariable.scope |> scopeDepth)


scopeDepth : List ScopeRegion -> Int
scopeDepth =
    \scope ->
        scope
            |> List.filterMap
                (\scopeRegion ->
                    case scopeRegion of
                        ScopeRegionPartAtIndex _ ->
                            Nothing

                        ScopeRegionBranchAtIndex _ ->
                            Nothing

                        ScopeRegionIntoUseRelation _ ->
                            Just ()
                )
            |> List.length


equal2Expand :
    { a : ValueWithVariableName ScopedVariableName
    , b : ValueWithVariableName ScopedVariableName
    }
    -> FactBranch
equal2Expand =
    \bothValues ->
        case ( bothValues.a, bothValues.b ) of
            ( Variable aVariable, ValueLookup bLookup ) ->
                BranchValidVariableSubstitution
                    { variable = aVariable, value = bLookup |> ValueLookup }

            ( ValueLookup aLookup, Variable bVariable ) ->
                BranchValidVariableSubstitution
                    { variable = bVariable, value = aLookup |> ValueLookup }

            ( Variable aVariableName, Variable bVariableName ) ->
                if aVariableName == bVariableName then
                    BranchValid
                        ([ "variable "
                         , aVariableName |> scopedVariableNameToString
                         , " equal to itself"
                         ]
                            |> String.concat
                        )

                else if aVariableName |> scopedVariableIsDeeperInThan bVariableName then
                    BranchValidVariableSubstitution
                        { variable = aVariableName
                        , value = bVariableName |> Variable
                        }

                else
                    BranchValidVariableSubstitution
                        { variable = bVariableName
                        , value = aVariableName |> Variable
                        }

            ( ValueLookup aLookup, ValueLookup bLookup ) ->
                let
                    entriesExpanded : Result String (List FactBranch)
                    entriesExpanded =
                        FastDict.merge
                            (\entryKey _ _ -> Err entryKey)
                            (\_ aEntryValue bEntryValue soFar ->
                                case soFar of
                                    Err entryKey ->
                                        Err entryKey

                                    Ok expandedSoFar ->
                                        ({ a = aEntryValue, b = bEntryValue } |> equal2Expand)
                                            :: expandedSoFar
                                            |> Ok
                            )
                            (\entryKey _ _ -> Err entryKey)
                            aLookup
                            bLookup
                            (Ok [])
                in
                case entriesExpanded of
                    Err entryKey ->
                        BranchInvalid
                            ([ "equating a value with an entry key "
                             , entryKey
                             , " with a value that does not contain this key."
                             ]
                                |> String.concat
                            )

                    Ok expanded ->
                        expanded |> BranchAll


factExpand :
    { scope : List ScopeRegion }
    -> (FactWithVariableName ScopedVariableName -> List FactBranch)
factExpand context fact =
    case fact of
        RelationUse relation ->
            BranchValidRelation
                { identifier = relation.identifier
                , argument = relation.argument
                , innerScope = ScopeRegionIntoUseRelation relation.identifier :: context.scope
                }
                |> List.singleton

        Equal values ->
            values |> equal2Expand |> List.singleton

        Not inverseFact ->
            inverseFact
                |> factExpand context
                |> List.concatMap factBranchInvert
                |> BranchAll
                |> List.singleton

        All parts ->
            parts
                |> List.indexedMap
                    (\indexInAll part ->
                        part
                            |> factExpand
                                { scope =
                                    ScopeRegionPartAtIndex indexInAll
                                        :: context.scope
                                }
                    )
                |> List.LauExtra.oneOfEach
                |> List.map (\branchParts -> branchParts |> BranchAll)

        Any branches ->
            branches
                |> List.indexedMap
                    (\indexInAny part ->
                        part
                            |> factExpand
                                { scope =
                                    ScopeRegionBranchAtIndex indexInAny
                                        :: context.scope
                                }
                    )
                |> List.concat


{-| Find a value that satisfies the constraints for an argument variable in the main definition
-}
evaluate : Project -> Result String Lookup
evaluate =
    \project ->
        case project.relationDefinitions |> FastDict.get "main" of
            Nothing ->
                Err "The relation \"main\" isn't defined. Define it."

            Just mainImplementation ->
                RelationUse
                    { identifier = "main"
                    , argument =
                        { scope = [], name = mainImplementation.argumentVariable }
                            |> Variable
                    }
                    |> factExpand { scope = [] }
                    |> factBranchesExpandFully project


factBranchExpand : Project -> (FactBranch -> List FactBranch)
factBranchExpand project factBranch =
    case factBranch of
        BranchValid whyValid ->
            BranchValid whyValid |> List.singleton

        BranchInvalid whyInvalid ->
            BranchInvalid whyInvalid |> List.singleton

        BranchValidVariableSubstitution substitution ->
            BranchValidVariableSubstitution substitution |> List.singleton

        BranchInvalidVariableSubstitution substitution ->
            BranchInvalidVariableSubstitution substitution |> List.singleton

        BranchValidRelation relation ->
            case project.relationDefinitions |> FastDict.get relation.identifier of
                Nothing ->
                    BranchInvalid
                        ([ "The relation "
                         , relation.identifier
                         , " isn't defined. Define it or choose a differently named relation."
                         ]
                            |> String.concat
                        )
                        |> List.singleton

                Just definition ->
                    definition.equivalentFact
                        |> factVariablesMap
                            (\variableName ->
                                if variableName == definition.argumentVariable then
                                    relation.argument

                                else if variableName == "Result" then
                                    Debug.todo ("how did this happen... " ++ (definition.equivalentFact |> Debug.toString))

                                else
                                    Variable
                                        { scope = relation.innerScope
                                        , name = variableName
                                        }
                            )
                        |> factExpand { scope = relation.innerScope }

        BranchInvalidRelation inverseRelation ->
            case project.relationDefinitions |> FastDict.get inverseRelation.identifier of
                Nothing ->
                    BranchInvalid
                        ([ "The relation "
                         , inverseRelation.identifier
                         , " isn't defined. Define it or choose a differently named relation."
                         ]
                            |> String.concat
                        )
                        |> List.singleton

                Just definition ->
                    definition.equivalentFact
                        |> factVariablesMap
                            (\variableName ->
                                if variableName == definition.argumentVariable then
                                    inverseRelation.argument

                                else
                                    Variable
                                        { scope = inverseRelation.innerScope
                                        , name = variableName
                                        }
                            )
                        |> factExpand { scope = inverseRelation.innerScope }
                        |> List.concatMap factBranchInvert
                        |> BranchAll
                        |> List.singleton

        BranchAll parts ->
            parts |> branchAllExpand project


factVariablesMap :
    (variableName -> ValueWithVariableName mappedVariableName)
    ->
        (FactWithVariableName variableName
         -> FactWithVariableName mappedVariableName
        )
factVariablesMap variableChange =
    \fact ->
        case fact of
            RelationUse relationUse ->
                RelationUse
                    { identifier = relationUse.identifier
                    , argument = relationUse.argument |> valueVariablesMap variableChange
                    }

            Equal values ->
                Equal
                    { a = values.a |> valueVariablesMap variableChange
                    , b = values.b |> valueVariablesMap variableChange
                    }

            Not invertedFact ->
                Not (invertedFact |> factVariablesMap variableChange)

            All parts ->
                All
                    (parts
                        |> List.map (\part -> part |> factVariablesMap variableChange)
                    )

            Any branches ->
                Any
                    (branches
                        |> List.map (\branch -> branch |> factVariablesMap variableChange)
                    )


factBranchesExpandFully : Project -> (List FactBranch -> Result String Lookup)
factBranchesExpandFully project =
    \branchesIncludingInvalid ->
        let
            firstBranchWithLookupResult : Maybe Lookup
            firstBranchWithLookupResult =
                branchesIncludingInvalid
                    |> List.LauExtra.firstJustMap
                        (\branch ->
                            case branch |> factBranchToExpandedResult of
                                ExpansionFailed _ ->
                                    Nothing

                                ExpansionNeedsToContinue _ ->
                                    Nothing

                                ExpandedToLookup lookup ->
                                    lookup |> Just
                        )
        in
        case firstBranchWithLookupResult of
            Just lookup ->
                lookup |> Ok

            Nothing ->
                let
                    branchesExpandedResults : List ExpandedResult
                    branchesExpandedResults =
                        branchesIncludingInvalid
                            |> List.map factBranchToExpandedResult

                    expandableBranchFacts : List FactBranch
                    expandableBranchFacts =
                        branchesExpandedResults
                            |> List.filterMap
                                (\expandableResult ->
                                    case expandableResult of
                                        ExpansionNeedsToContinue factToContinueExpanding ->
                                            Just factToContinueExpanding

                                        ExpandedToLookup _ ->
                                            Nothing

                                        ExpansionFailed _ ->
                                            Nothing
                                )

                    determinedOrNeedsExpanding : Result (List FactBranch) Lookup
                    determinedOrNeedsExpanding =
                        expandableBranchFacts
                            |> List.concatMap (\branch -> branch |> factBranchExpand project)
                            |> List.foldl
                                (\branchExpanded soFar ->
                                    case soFar of
                                        Ok determined ->
                                            determined |> Ok

                                        Err soFarBranchesNeedExpanding ->
                                            case branchExpanded |> factBranchToExpandedResult of
                                                ExpandedToLookup determined ->
                                                    determined |> Ok

                                                _ ->
                                                    branchExpanded :: soFarBranchesNeedExpanding |> Err
                                )
                                (Err [])
                in
                case determinedOrNeedsExpanding of
                    Ok determined ->
                        determined |> Ok

                    Err [] ->
                        let
                            branchExpansionStops : List String
                            branchExpansionStops =
                                branchesExpandedResults
                                    |> List.filterMap
                                        (\branch ->
                                            case branch of
                                                ExpansionFailed whyFailed ->
                                                    whyFailed |> Just

                                                ExpansionNeedsToContinue _ ->
                                                    Nothing

                                                ExpandedToLookup _ ->
                                                    Nothing
                                        )
                        in
                        Err
                            ("In every branch I've tried, evaluation has stopped but none returned a concrete value: "
                                ++ (branchExpansionStops |> String.join "\n")
                            )

                    Err needsExpanding ->
                        if needsExpanding == expandableBranchFacts then
                            Err
                                ("the result isn't constrained to a specific solution. Here's what I figured out: "
                                    ++ (needsExpanding |> List.map factBranchToString |> String.join "\n")
                                )

                        else
                            let
                                _ =
                                    Debug.log
                                        ([ "------------------------ all facts expanded to --------------------------\n"
                                         , needsExpanding
                                            |> List.map (\factBranch -> "  - " ++ (factBranch |> factBranchToString))
                                            |> String.join "\n"
                                         , "\n"
                                         ]
                                            |> String.concat
                                        )
                                        ()
                            in
                            needsExpanding
                                |> factBranchesExpandFully project


type ExpandedResult
    = ExpandedToLookup Lookup
    | ExpansionNeedsToContinue FactBranch
    | ExpansionFailed String


factBranchToExpandedResult : FactBranch -> ExpandedResult
factBranchToExpandedResult =
    \fact ->
        case fact of
            BranchValidVariableSubstitution substitution ->
                case substitution.variable.scope of
                    [] ->
                        case substitution.value |> valueToValueLookup |> Maybe.andThen valueLookupToLookup of
                            Nothing ->
                                ExpansionNeedsToContinue (BranchValidVariableSubstitution substitution)

                            Just lookup ->
                                ExpandedToLookup lookup

                    _ :: _ ->
                        case substitution.value |> valueToValueLookup |> Maybe.andThen valueLookupToLookup of
                            Just lookup ->
                                ExpansionFailed
                                    ([ "I was only able to determine the value for a variable in a used relation: "
                                     , substitution.variable |> scopedVariableNameToString
                                     , " = "
                                     , lookup |> lookupToString
                                     , ". Try to connect this value to the argument variable(s) of that relation."
                                     ]
                                        |> String.concat
                                    )

                            Nothing ->
                                ExpansionNeedsToContinue (BranchValidVariableSubstitution substitution)

            BranchAll parts ->
                ExpansionNeedsToContinue (BranchAll parts)

            BranchValidRelation validRelation ->
                ExpansionNeedsToContinue (BranchValidRelation validRelation)

            BranchInvalidRelation invalidRelation ->
                ExpansionNeedsToContinue (BranchInvalidRelation invalidRelation)

            BranchValid whyValid ->
                ExpansionFailed
                    ([ "all values are possible: "
                     , whyValid
                     , ". Add some constructive constraints so I can choose a good value"
                     ]
                        |> String.concat
                    )

            BranchInvalid whyInvalid ->
                ExpansionFailed whyInvalid

            BranchInvalidVariableSubstitution invalidSubstitution ->
                ExpansionFailed
                    ([ "We only know that variable "
                     , invalidSubstitution.variable |> scopedVariableNameToString
                     , " is NOT equivalent to "
                     , invalidSubstitution.value |> valueToString
                     , " which is too little constraint to produce a solution"
                     ]
                        |> String.concat
                    )


valueLookupToLookup : FastDict.Dict String (ValueWithVariableName variableName_) -> Maybe Lookup
valueLookupToLookup =
    \valueLookup ->
        valueLookup
            |> FastDict.toList
            |> List.LauExtra.allJustMap
                (\( identifier, value ) ->
                    Maybe.map (\lookup -> ( identifier, lookup ))
                        (value |> valueToValueLookup |> Maybe.andThen valueLookupToLookup)
                )
            |> Maybe.map (\entries -> entries |> FastDict.fromList |> Lookup)


scopedVariableNameToString : ScopedVariableName -> String
scopedVariableNameToString =
    \scopedVariable ->
        case scopedVariable.scope of
            [] ->
                scopedVariable.name

            scopePart0 :: scopePart1Up ->
                [ "["
                , ((scopePart0 :: scopePart1Up)
                    |> List.reverse
                    |> List.map scopeRegionToString
                  )
                    |> String.join "/"
                , "]"
                , scopedVariable.name
                ]
                    |> String.concat


scopeRegionToString : ScopeRegion -> String
scopeRegionToString =
    \scopeRegion ->
        case scopeRegion of
            ScopeRegionPartAtIndex partIndex ->
                "part " ++ (partIndex |> String.fromInt)

            ScopeRegionBranchAtIndex branchIndex ->
                "branch " ++ (branchIndex |> String.fromInt)

            ScopeRegionIntoUseRelation identifier ->
                identifier


factBranchToString : FactBranch -> String
factBranchToString =
    \factBranch ->
        case factBranch of
            BranchValid whyValid ->
                [ "✅\"", whyValid, "\"" ] |> String.concat

            BranchInvalid whyInvalid ->
                [ "❌\"", whyInvalid, "\"" ] |> String.concat

            BranchValidVariableSubstitution substitution ->
                [ "("
                , substitution.variable |> scopedVariableNameToString
                , " = "
                , substitution.value |> valueToString
                , ")"
                ]
                    |> String.concat

            BranchInvalidVariableSubstitution substitution ->
                [ "("
                , substitution.variable |> scopedVariableNameToString
                , " ≠ "
                , substitution.value |> valueToString
                , ")"
                ]
                    |> String.concat

            BranchValidRelation relation ->
                [ "("
                , relation.identifier
                , " "
                , relation.argument |> valueToString
                , ")"
                ]
                    |> String.concat

            BranchInvalidRelation relation ->
                [ "(not ("
                , relation.identifier
                , " "
                , relation.argument |> valueToString
                , "))"
                ]
                    |> String.concat

            BranchAll parts ->
                [ "("
                , parts |> List.map factBranchToString |> String.join " and "
                , ")"
                ]
                    |> String.concat


valueToString : ValueWithVariableName ScopedVariableName -> String
valueToString =
    \value ->
        case value of
            Variable variableName ->
                variableName |> scopedVariableNameToString

            ValueLookup valueLookup ->
                [ "{"
                , valueLookup
                    |> FastDict.toList
                    |> List.map
                        (\( entryName, entryValue ) ->
                            [ entryName, " ", entryValue |> valueToString ] |> String.concat
                        )
                    |> String.join " , "
                , "}"
                ]
                    |> String.concat


type alias ScopedVariableName =
    { scope : List ScopeRegion
    , name : String
    }


type ScopeRegion
    = ScopeRegionPartAtIndex Int
    | ScopeRegionBranchAtIndex Int
    | ScopeRegionIntoUseRelation String


type FactBranch
    = BranchValid String
    | BranchInvalid String
    | BranchValidVariableSubstitution
        { variable : ScopedVariableName
        , value : ValueWithVariableName ScopedVariableName
        }
    | BranchInvalidVariableSubstitution
        { variable : ScopedVariableName
        , value : ValueWithVariableName ScopedVariableName
        }
    | BranchValidRelation
        { identifier : String
        , argument : ValueWithVariableName ScopedVariableName
        , innerScope : List ScopeRegion
        }
    | BranchInvalidRelation
        { identifier : String
        , argument : ValueWithVariableName ScopedVariableName
        , innerScope : List ScopeRegion
        }
    | BranchAll (List FactBranch)


branchAllExpand : Project -> (List FactBranch -> List FactBranch)
branchAllExpand project =
    \parts ->
        case parts of
            [] ->
                BranchValid "empty all" |> List.singleton

            [ only ] ->
                only |> List.singleton

            onePart :: twoPart :: otherParts ->
                case (onePart :: twoPart :: otherParts) |> List.LauExtra.firstJustMap factBranchIsInvalid of
                    Just reason ->
                        BranchInvalid reason |> List.singleton

                    Nothing ->
                        let
                            notObviouslyValidParts : List FactBranch
                            notObviouslyValidParts =
                                (onePart :: twoPart :: otherParts)
                                    |> List.filterMap
                                        (\part ->
                                            case part of
                                                BranchValid _ ->
                                                    Nothing

                                                notObviouslyValid ->
                                                    Just notObviouslyValid
                                        )

                            variableSubstitutionParts :
                                List
                                    { variable : ScopedVariableName
                                    , value : ValueWithVariableName ScopedVariableName
                                    }
                            variableSubstitutionParts =
                                notObviouslyValidParts
                                    |> List.filterMap
                                        (\part ->
                                            case part of
                                                BranchValidVariableSubstitution substitution ->
                                                    substitution |> Just

                                                _ ->
                                                    Nothing
                                        )

                            -- TODO invert-substitute (?)
                        in
                        case variableSubstitutionParts of
                            [] ->
                                notObviouslyValidParts
                                    |> List.map (\part -> part |> factBranchExpand project)
                                    |> List.LauExtra.oneOfEach
                                    |> List.map (\branchParts -> branchParts |> BranchAll)

                            oneVariableSubstitutionPart :: otherVariableSubstitutionParts ->
                                let
                                    substitutionOfDeepestInVariable :
                                        { value : ValueWithVariableName ScopedVariableName
                                        , variable : ScopedVariableName
                                        }
                                    substitutionOfDeepestInVariable =
                                        otherVariableSubstitutionParts
                                            |> List.foldl
                                                (\substitutionPart soFarSubstitutionOfDeepestInVariable ->
                                                    if
                                                        substitutionPart.variable
                                                            |> scopedVariableIsDeeperInThan soFarSubstitutionOfDeepestInVariable.variable
                                                    then
                                                        substitutionPart

                                                    else
                                                        soFarSubstitutionOfDeepestInVariable
                                                )
                                                oneVariableSubstitutionPart
                                in
                                (if substitutionOfDeepestInVariable |> substitutionIsErasing then
                                    notObviouslyValidParts
                                        |> List.filter
                                            (\part ->
                                                part /= BranchValidVariableSubstitution substitutionOfDeepestInVariable
                                            )

                                 else
                                    notObviouslyValidParts
                                )
                                    |> List.map
                                        (\part ->
                                            part
                                                |> factBranchVariablesMap
                                                    (\variableName ->
                                                        if variableName == substitutionOfDeepestInVariable.variable then
                                                            substitutionOfDeepestInVariable.value

                                                        else
                                                            Variable variableName
                                                    )
                                        )
                                    |> BranchAll
                                    |> List.singleton


factBranchIsInvalid : FactBranch -> Maybe String
factBranchIsInvalid =
    \factBranch ->
        case factBranch of
            BranchInvalid reason ->
                Just reason

            _ ->
                Nothing


factBranchVariablesMap :
    (ScopedVariableName -> ValueWithVariableName ScopedVariableName)
    -> (FactBranch -> FactBranch)
factBranchVariablesMap variableChange =
    \fact ->
        case fact of
            BranchValid whyValid ->
                BranchValid whyValid

            BranchInvalid whyInvalid ->
                BranchInvalid whyInvalid

            BranchValidRelation factRelation ->
                { identifier = factRelation.identifier
                , argument =
                    factRelation.argument
                        |> valueVariablesMap variableChange
                , innerScope = factRelation.innerScope
                }
                    |> BranchValidRelation

            BranchInvalidRelation factRelation ->
                { identifier = factRelation.identifier
                , argument =
                    factRelation.argument
                        |> valueVariablesMap variableChange
                , innerScope = factRelation.innerScope
                }
                    |> BranchInvalidRelation

            BranchValidVariableSubstitution substitution ->
                { a = substitution.variable |> Variable |> valueVariablesMap variableChange
                , b = substitution.value |> valueVariablesMap variableChange
                }
                    |> equal2Expand

            BranchInvalidVariableSubstitution substitution ->
                { a = substitution.variable |> Variable |> valueVariablesMap variableChange
                , b = substitution.value |> valueVariablesMap variableChange
                }
                    |> different2Expand

            BranchAll parts ->
                parts
                    |> List.map (\part -> part |> factBranchVariablesMap variableChange)
                    |> BranchAll


different2Expand :
    { a : ValueWithVariableName ScopedVariableName
    , b : ValueWithVariableName ScopedVariableName
    }
    -> FactBranch
different2Expand =
    \bothValues ->
        case ( bothValues.a, bothValues.b ) of
            ( Variable aVariable, ValueLookup bLookup ) ->
                BranchInvalidVariableSubstitution
                    { variable = aVariable, value = bLookup |> ValueLookup }

            ( ValueLookup aLookup, Variable bVariable ) ->
                BranchInvalidVariableSubstitution
                    { variable = bVariable, value = aLookup |> ValueLookup }

            ( Variable aVariable, Variable bVariable ) ->
                if aVariable == bVariable then
                    BranchInvalid
                        ([ "expected variables "
                         , aVariable.name
                         , " and "
                         , bVariable.name
                         , " to be different but they're set to be equal"
                         ]
                            |> String.concat
                        )

                else
                    BranchValidVariableSubstitution { variable = aVariable, value = bVariable |> Variable }

            ( ValueLookup aLookup, ValueLookup bLookup ) ->
                FastDict.merge
                    (\_ _ soFar -> soFar)
                    (\_ aEntryValue bEntryValue soFar ->
                        ({ a = aEntryValue, b = bEntryValue } |> different2Expand)
                            :: soFar
                    )
                    (\_ _ soFar -> soFar)
                    aLookup
                    bLookup
                    []
                    |> BranchAll


substitutionIsErasing :
    { variable : variableName, value : ValueWithVariableName variableName }
    -> Bool
substitutionIsErasing =
    \substitution ->
        case substitution.value of
            Variable variableName ->
                variableName /= substitution.variable

            ValueLookup lookup ->
                lookup
                    |> dictAll
                        (\_ entryReplacement ->
                            { variable = substitution.variable, value = entryReplacement }
                                |> substitutionIsErasing
                        )


dictAll : (k -> v -> Bool) -> FastDict.Dict k v -> Bool
dictAll isFound =
    \dict ->
        dict
            |> FastDict.restructure True
                (\state ->
                    isFound state.key state.value && state.left () && state.right ()
                )
