module Lau exposing
    ( Lookup(..), ValueWithVariableName(..)
    , Fact, FactWithVariableName(..)
    , evaluate, EvaluationError(..), WhyInvalid(..), WhyValid(..)
    , FactExpansionAll, FactExpansionAny, FactExpansionPart(..), RelationDefinition, ScopeRegion(..), ScopedVariableName, evaluationErrorToString, lookupToString
    )

{-| Language representation and evaluation

@docs Lookup, ValueWithVariableName
@docs Fact, FactWithVariableName
@docs Project, evaluate, EvaluationError, WhyInvalid, WhyValid


## possible future explorations

if between branches, a definition is used with the same lookup (without variables),
use the cached value. (memoizing laziness).

Currently I'm not super excited to do this because it will

  - increase memory usage if not applied with some heuristic
  - complicate the code
  - make running in parallel harder/less performant,
    e.g. when [the elm HVM compiler](https://github.com/Janiczek/elm-bend) gets created

-}

import FastDict
import List.LocalExtra


{-| What a relation with a given name and parameter is equivalent to
-}
type alias RelationDefinition =
    { identifier : String
    , parameter : ValueWithVariableName String
    , equivalentFact : Fact
    }


type alias RelationDefinitionsForExpansion =
    FastDict.Dict
        String
        { argumentVariable : String
        , equivalentFact : Fact
        }


relationDefinitionsToForExpansion : List RelationDefinition -> RelationDefinitionsForExpansion
relationDefinitionsToForExpansion relationDefinitions =
    relationDefinitions
        |> List.foldl
            (\relationDefinition soFar ->
                let
                    internalParameterVariableName =
                        "__internal__argument variable"
                in
                soFar
                    |> FastDict.insert relationDefinition.identifier
                        { argumentVariable = internalParameterVariableName
                        , equivalentFact =
                            All
                                [ Equal
                                    { a = Variable internalParameterVariableName
                                    , b = relationDefinition.parameter
                                    }
                                , relationDefinition.equivalentFact
                                ]
                        }
            )
            FastDict.empty


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
lookupToString (Lookup entries) =
    [ "{"
    , entries
        |> FastDict.toList
        |> List.map (\( identifier, value ) -> [ identifier, " ", value |> lookupToString ] |> String.concat)
        |> String.join " "
    , "}"
    ]
        |> String.concat


{-| TODO split into valid/unconstrained vs invalid
-}
type EvaluationError
    = EvaluationErrorInvalid WhyInvalid
    | EvaluationErrorValid WhyValid
    | EvaluationErrorRelationNotDefined String
    | EvaluationErrorMainRelationNotDefined
    | EvaluationErrorAllBranchesHaveStopped (List EvaluationError)
    | EvaluationErrorNoConcreteSolutionAny FactExpansionAny
    | EvaluationErrorOnlyDeeperInValidVariableSubstitution
        { variable : ScopedVariableName
        , value : ValueWithVariableName ScopedVariableName
        }
    | EvaluationErrorOnlyDeeperInInvalidVariableSubstitution
        { variable : ScopedVariableName
        , value : ValueWithVariableName ScopedVariableName
        }


type WhyValid
    = ValidBecauseAllEmpty
    | ValidBecauseVariableEqualToItself ScopedVariableName
    | ValidBecauseEquatingSameEmptyLookups
    | ValueBecauseNotOfInvalid WhyInvalid


type WhyInvalid
    = InvalidBecauseAnyEmpty
    | InvalidBecauseVariableNotEqualToItself ScopedVariableName
    | InvalidBecauseEquatingValuesWithDifferentEntryKeys String
    | InvalidBecauseNotOfValid WhyValid


scopedVariableNameToString : ScopedVariableName -> String
scopedVariableNameToString scopedVariable =
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
scopeRegionToString scopeRegion =
    case scopeRegion of
        ScopeRegionPartAtIndex partIndex ->
            "part " ++ (partIndex |> String.fromInt)

        ScopeRegionBranchAtIndex branchIndex ->
            "branch " ++ (branchIndex |> String.fromInt)

        ScopeRegionIntoUseRelation identifier ->
            identifier


whyValidToString : WhyValid -> String
whyValidToString whyValid =
    case whyValid of
        ValueBecauseNotOfInvalid inverseError ->
            -- TODO better error?
            "not " ++ (inverseError |> whyInvalidToString)

        ValidBecauseVariableEqualToItself variableName ->
            [ "variable "
            , variableName |> scopedVariableNameToString
            , " is set as equal to itself, which means all values are possible for the result. Add some constructive constraints so I can choose a good value."
            ]
                |> String.concat

        ValidBecauseEquatingSameEmptyLookups ->
            "an empty lookup is set as equal to another empty lookup, which means all values are possible for the result. Add some constructive constraints so I can choose a good value."

        ValidBecauseAllEmpty ->
            "empty all, which means all values are possible for the result. Add some constructive constraints in the all so I can choose a good value."


whyInvalidToString : WhyInvalid -> String
whyInvalidToString whyInvalid =
    case whyInvalid of
        InvalidBecauseNotOfValid inverseError ->
            -- TODO better error?
            "not " ++ (inverseError |> whyValidToString)

        InvalidBecauseAnyEmpty ->
            "empty any, which means no branches will ever result in a result. Add some facts inside the any or remove it."

        InvalidBecauseVariableNotEqualToItself variableName ->
            [ "expected variables "
            , variableName |> scopedVariableNameToString
            , " and "
            , variableName |> scopedVariableNameToString
            , " to be different but they're the same. Maybe you wanted to equate it with a different variable?"
            ]
                |> String.concat

        InvalidBecauseEquatingValuesWithDifferentEntryKeys entryKey ->
            [ "equating a value with an entry key "
            , entryKey
            , " with a value that does not contain this key."
            ]
                |> String.concat


valueToString : ValueWithVariableName ScopedVariableName -> String
valueToString value =
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


evaluationErrorToString : EvaluationError -> String
evaluationErrorToString evaluationError =
    case evaluationError of
        EvaluationErrorInvalid whyInvalid ->
            whyInvalid |> whyInvalidToString

        EvaluationErrorValid whyValid ->
            whyValid |> whyValidToString

        EvaluationErrorMainRelationNotDefined ->
            "The relation \"main\" used to run the whole app isn't defined. Define it."

        EvaluationErrorRelationNotDefined identifier ->
            [ "The used relation "
            , identifier
            , " isn't defined. Define it or choose a differently named relation."
            ]
                |> String.concat

        EvaluationErrorAllBranchesHaveStopped branchExpansionStops ->
            "In every branch I've tried, evaluation has stopped but none returned a concrete value: "
                ++ (branchExpansionStops
                        |> List.map evaluationErrorToString
                        |> String.join "\n"
                   )

        EvaluationErrorNoConcreteSolutionAny expanded ->
            "the result isn't constrained to a specific solution. Here's what I figured out: "
                ++ (expanded.factExpansionAny
                        |> List.map factExpansionAllToString
                        |> String.join "\n"
                   )

        EvaluationErrorOnlyDeeperInValidVariableSubstitution substitution ->
            [ "I was only able to determine the value for a variable in a used relation: "
            , substitution.variable |> scopedVariableNameToString
            , " = "
            , substitution.value |> valueToString
            , ". Try to connect this value to the argument variable(s) of that relation."
            ]
                |> String.concat

        EvaluationErrorOnlyDeeperInInvalidVariableSubstitution invalidSubstitution ->
            [ "We only know that variable "
            , invalidSubstitution.variable |> scopedVariableNameToString
            , " is NOT equal to "
            , invalidSubstitution.value |> valueToString
            , " which is too little constraint to produce a solution"
            ]
                |> String.concat


factExpansionAllToString : FactExpansionAll -> String
factExpansionAllToString factExpansionAll =
    [ "("
    , factExpansionAll.factExpansionAll |> List.map factExpansionPartToString |> String.join " and "
    , ")"
    ]
        |> String.concat


factExpansionPartToString : FactExpansionPart -> String
factExpansionPartToString factExpansionPart =
    case factExpansionPart of
        FactExpansionPartValid whyValid ->
            [ "✅\"", whyValid |> whyValidToString, "\"" ] |> String.concat

        FactExpansionPartInvalid whyInvalid ->
            [ "❌\"", whyInvalid |> whyInvalidToString, "\"" ] |> String.concat

        FactExpansionPartValidVariableSubstitution substitution ->
            [ "("
            , substitution.variable |> scopedVariableNameToString
            , " = "
            , substitution.value |> valueToString
            , ")"
            ]
                |> String.concat

        FactExpansionPartInvalidVariableSubstitution substitution ->
            [ "("
            , substitution.variable |> scopedVariableNameToString
            , " ≠ "
            , substitution.value |> valueToString
            , ")"
            ]
                |> String.concat

        FactExpansionPartValidRelation relation ->
            [ "("
            , relation.identifier
            , " "
            , relation.argument |> valueToString
            , ")"
            ]
                |> String.concat

        FactExpansionPartInvalidRelation relation ->
            [ "(not ("
            , relation.identifier
            , " "
            , relation.argument |> valueToString
            , "))"
            ]
                |> String.concat


factExpansionAnySingleton : FactExpansionAll -> FactExpansionAny
factExpansionAnySingleton factExpansionAll =
    { factExpansionAny = factExpansionAll |> List.singleton }


factExpansionAllSingleton : FactExpansionPart -> FactExpansionAll
factExpansionAllSingleton factExpansionPart =
    { factExpansionAll = factExpansionPart |> List.singleton }


scopedVariableIsDeeperInThan : ScopedVariableName -> (ScopedVariableName -> Bool)
scopedVariableIsDeeperInThan bVariable aVariable =
    (aVariable.scope |> scopeDepth) > (bVariable.scope |> scopeDepth)


scopeDepth : List ScopeRegion -> Int
scopeDepth scope =
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


variableSubstitutionToWithDeeperInAsVariable :
    { variable : ScopedVariableName
    , value : ValueWithVariableName ScopedVariableName
    }
    ->
        { variable : ScopedVariableName
        , value : ValueWithVariableName ScopedVariableName
        }
variableSubstitutionToWithDeeperInAsVariable variableSubstitution =
    case variableSubstitution.value of
        Variable otherVariable ->
            if otherVariable |> scopedVariableIsDeeperInThan variableSubstitution.variable then
                { variable = otherVariable, value = Variable variableSubstitution.variable }

            else
                { variable = variableSubstitution.variable, value = Variable otherVariable }

        ValueLookup otherValueLookup ->
            { variable = variableSubstitution.variable, value = ValueLookup otherValueLookup }


equal2Expand :
    { a : ValueWithVariableName ScopedVariableName
    , b : ValueWithVariableName ScopedVariableName
    }
    -> FactExpansionAll
equal2Expand bothValues =
    case ( bothValues.a, bothValues.b ) of
        ( Variable aVariable, ValueLookup bLookup ) ->
            FactExpansionPartValidVariableSubstitution
                { variable = aVariable, value = bLookup |> ValueLookup }
                |> factExpansionAllSingleton

        ( ValueLookup aLookup, Variable bVariable ) ->
            FactExpansionPartValidVariableSubstitution
                { variable = bVariable, value = aLookup |> ValueLookup }
                |> factExpansionAllSingleton

        ( Variable aVariableName, Variable bVariableName ) ->
            if aVariableName == bVariableName then
                FactExpansionPartValid (ValidBecauseVariableEqualToItself aVariableName)
                    |> factExpansionAllSingleton

            else
                FactExpansionPartValidVariableSubstitution
                    ({ variable = aVariableName
                     , value = Variable bVariableName
                     }
                        |> variableSubstitutionToWithDeeperInAsVariable
                    )
                    |> factExpansionAllSingleton

        ( ValueLookup aLookup, ValueLookup bLookup ) ->
            if (aLookup |> FastDict.isEmpty) && (bLookup |> FastDict.isEmpty) then
                FactExpansionPartValid ValidBecauseEquatingSameEmptyLookups
                    |> factExpansionAllSingleton

            else
                let
                    entriesExpanded : Result String (List FactExpansionAll)
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
                        FactExpansionPartInvalid
                            (InvalidBecauseEquatingValuesWithDifferentEntryKeys entryKey)
                            |> factExpansionAllSingleton

                    Ok expanded ->
                        { factExpansionAll = expanded |> List.concatMap .factExpansionAll }


factExpansionAnyInvert : FactExpansionAny -> FactExpansionAll
factExpansionAnyInvert factExpansionAny =
    { factExpansionAll =
        factExpansionAny.factExpansionAny
            |> List.concatMap
                (\inverseFactBranch ->
                    inverseFactBranch |> factExpansionAllInvert |> .factExpansionAll
                )
    }


factExpansionAllInvert : FactExpansionAll -> FactExpansionAll
factExpansionAllInvert factExpansionAll =
    { factExpansionAll =
        factExpansionAll.factExpansionAll
            |> List.map factExpansionPartInvert
    }


factExpansionPartInvert : FactExpansionPart -> FactExpansionPart
factExpansionPartInvert factExpansionPart =
    case factExpansionPart of
        FactExpansionPartValid whyValid ->
            FactExpansionPartInvalid (InvalidBecauseNotOfValid whyValid)

        FactExpansionPartInvalid whyInvalid ->
            FactExpansionPartValid (ValueBecauseNotOfInvalid whyInvalid)

        FactExpansionPartValidRelation relation ->
            FactExpansionPartInvalidRelation relation

        FactExpansionPartInvalidRelation relation ->
            FactExpansionPartValidRelation relation

        FactExpansionPartValidVariableSubstitution substitution ->
            FactExpansionPartInvalidVariableSubstitution substitution

        FactExpansionPartInvalidVariableSubstitution substitution ->
            FactExpansionPartValidVariableSubstitution substitution


factExpand :
    { scope : List ScopeRegion }
    -> (FactWithVariableName ScopedVariableName -> FactExpansionAny)
factExpand context fact =
    case fact of
        RelationUse relation ->
            FactExpansionPartValidRelation
                { identifier = relation.identifier
                , argument = relation.argument
                , innerScope = ScopeRegionIntoUseRelation relation.identifier :: context.scope
                }
                |> factExpansionAllSingleton
                |> factExpansionAnySingleton

        Equal values ->
            values |> equal2Expand |> factExpansionAnySingleton

        Not inverseFact ->
            inverseFact
                |> factExpand context
                |> factExpansionAnyInvert
                |> factExpansionAnySingleton

        All parts ->
            { factExpansionAny =
                parts
                    |> List.indexedMap
                        (\indexInAll part ->
                            part
                                |> factExpand
                                    { scope =
                                        ScopeRegionPartAtIndex indexInAll
                                            :: context.scope
                                    }
                                |> .factExpansionAny
                        )
                    |> List.LocalExtra.oneOfEach
                    |> List.map
                        (\newBranch ->
                            { factExpansionAll =
                                newBranch |> List.concatMap .factExpansionAll
                            }
                        )
            }

        Any branches ->
            { factExpansionAny =
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
                    |> List.concatMap .factExpansionAny
            }


{-| Find a value that satisfies the constraints for an argument variable in the main definition

Needs to include a definition named "main" if you want to [evaluate](#evaluate) it.

-}
evaluate : List RelationDefinition -> Result EvaluationError Lookup
evaluate relationDefinitions =
    let
        relationDefinitionsForExpansion : RelationDefinitionsForExpansion
        relationDefinitionsForExpansion =
            relationDefinitions |> relationDefinitionsToForExpansion
    in
    case relationDefinitionsForExpansion |> FastDict.get "main" of
        Nothing ->
            Err EvaluationErrorMainRelationNotDefined

        Just mainImplementation ->
            RelationUse
                { identifier = "main"
                , argument =
                    Variable { scope = [], name = mainImplementation.argumentVariable }
                }
                |> factExpand { scope = [] }
                |> factExpansionAnyExpandFully relationDefinitionsForExpansion


factExpansionAnyExpandFully : RelationDefinitionsForExpansion -> (FactExpansionAny -> Result EvaluationError Lookup)
factExpansionAnyExpandFully project factExpansionAny =
    let
        branchesExpandedStatues : List ExpandedStatus
        branchesExpandedStatues =
            factExpansionAny.factExpansionAny
                |> List.map factExpansionAllToExpandedStatus

        firstBranchWithLookupResult : Maybe Lookup
        firstBranchWithLookupResult =
            branchesExpandedStatues
                |> List.LocalExtra.firstJustMap
                    (\branchExpandedStatus ->
                        case branchExpandedStatus of
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
                expandableBranchFacts : List FactExpansionAll
                expandableBranchFacts =
                    branchesExpandedStatues
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
            in
            case expandableBranchFacts |> List.LocalExtra.allOkMap (\branch -> branch |> factExpansionAllExpand project) of
                Err error ->
                    EvaluationErrorRelationNotDefined error.relationNotDefined |> Err

                Ok branchExpansions ->
                    let
                        determinedOrNeedsExpanding : Result (List FactExpansionAll) Lookup
                        determinedOrNeedsExpanding =
                            branchExpansions
                                |> List.concatMap (\branchExpansion -> branchExpansion |> .factExpansionAny)
                                |> List.foldl
                                    (\branchExpanded soFar ->
                                        case soFar of
                                            Ok determined ->
                                                determined |> Ok

                                            Err soFarBranchesNeedExpanding ->
                                                case branchExpanded |> factExpansionAllToExpandedStatus of
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
                                branchExpansionStops : List EvaluationError
                                branchExpansionStops =
                                    branchesExpandedStatues
                                        |> List.filterMap
                                            (\branchExpandedStatus ->
                                                case branchExpandedStatus of
                                                    ExpansionFailed whyFailed ->
                                                        whyFailed |> Just

                                                    ExpansionNeedsToContinue _ ->
                                                        Nothing

                                                    ExpandedToLookup _ ->
                                                        Nothing
                                            )
                            in
                            case branchExpansionStops of
                                [] ->
                                    Err (EvaluationErrorInvalid InvalidBecauseAnyEmpty)

                                branchExpansionStop0 :: branchExpansionStop1Up ->
                                    Err (EvaluationErrorAllBranchesHaveStopped (branchExpansionStop0 :: branchExpansionStop1Up))

                        Err needsExpanding ->
                            if needsExpanding == expandableBranchFacts then
                                Err (EvaluationErrorNoConcreteSolutionAny { factExpansionAny = needsExpanding })

                            else
                                {- let
                                       _ =
                                           Debug.log
                                               ([ "------------------------ all facts expanded to --------------------------\n"
                                                   , needsExpanding
                                                   |> List.map (\factBranch -> "  - " ++ (factBranch |> factExpansionAllToString))
                                                   |> String.join "\n"
                                                   , "\n"
                                                   ]
                                                   |> String.concat
                                               )
                                               ()
                                   in
                                -}
                                { factExpansionAny = needsExpanding }
                                    |> factExpansionAnyExpandFully project


factExpansionAllToExpandedStatus : FactExpansionAll -> ExpandedStatus
factExpansionAllToExpandedStatus factExpansionAll =
    case factExpansionAll.factExpansionAll of
        [] ->
            ExpansionFailed (EvaluationErrorValid ValidBecauseAllEmpty)

        [ onlyPart ] ->
            onlyPart |> factExpansionPartToExpandedStatus

        part0 :: part1Up ->
            ExpansionNeedsToContinue { factExpansionAll = part0 :: part1Up }


valueToValueLookup :
    ValueWithVariableName variableName
    -> Maybe (FastDict.Dict String (ValueWithVariableName variableName))
valueToValueLookup value =
    case value of
        Variable _ ->
            Nothing

        ValueLookup valueLookup ->
            valueLookup |> Just


factExpansionPartToExpandedStatus : FactExpansionPart -> ExpandedStatus
factExpansionPartToExpandedStatus fact =
    case fact of
        FactExpansionPartValidVariableSubstitution substitution ->
            case substitution.variable.scope of
                [] ->
                    case substitution.value |> valueToValueLookup |> Maybe.andThen valueLookupToLookup of
                        Nothing ->
                            ExpansionNeedsToContinue
                                (FactExpansionPartValidVariableSubstitution substitution
                                    |> factExpansionAllSingleton
                                )

                        Just lookup ->
                            ExpandedToLookup lookup

                _ :: _ ->
                    ExpansionFailed
                        (EvaluationErrorOnlyDeeperInValidVariableSubstitution substitution)

        FactExpansionPartValidRelation validRelation ->
            ExpansionNeedsToContinue
                (FactExpansionPartValidRelation validRelation |> factExpansionAllSingleton)

        FactExpansionPartInvalidRelation invalidRelation ->
            ExpansionNeedsToContinue
                (FactExpansionPartInvalidRelation invalidRelation |> factExpansionAllSingleton)

        FactExpansionPartValid whyValid ->
            ExpansionFailed (EvaluationErrorValid whyValid)

        FactExpansionPartInvalid whyInvalid ->
            ExpansionFailed (EvaluationErrorInvalid whyInvalid)

        FactExpansionPartInvalidVariableSubstitution invalidSubstitution ->
            ExpansionFailed
                (EvaluationErrorOnlyDeeperInInvalidVariableSubstitution invalidSubstitution)


valueLookupToLookup : FastDict.Dict String (ValueWithVariableName variableName_) -> Maybe Lookup
valueLookupToLookup valueLookup =
    valueLookup
        |> FastDict.toList
        |> List.LocalExtra.allJustMap
            (\( identifier, value ) ->
                Maybe.map (\lookup -> ( identifier, lookup ))
                    (value |> valueToValueLookup |> Maybe.andThen valueLookupToLookup)
            )
        |> Maybe.map (\entries -> entries |> FastDict.fromList |> Lookup)


factExpansionAllExpand : RelationDefinitionsForExpansion -> (FactExpansionAll -> Result { relationNotDefined : String } FactExpansionAny)
factExpansionAllExpand project factExpansionAll =
    case factExpansionAll.factExpansionAll of
        [] ->
            FactExpansionPartValid ValidBecauseAllEmpty
                |> factExpansionAllSingleton
                |> factExpansionAnySingleton
                |> Ok

        [ only ] ->
            only
                |> factExpansionPartExpand project
                |> Result.mapError (\error -> { relationNotDefined = error.relationNotDefined })

        onePart :: twoPart :: otherParts ->
            case (onePart :: twoPart :: otherParts) |> List.LocalExtra.firstJustMap factExpansionPartIsInvalid of
                Just reason ->
                    FactExpansionPartInvalid reason
                        |> factExpansionAllSingleton
                        |> factExpansionAnySingleton
                        |> Ok

                Nothing ->
                    let
                        notObviouslyValidParts : List FactExpansionPart
                        notObviouslyValidParts =
                            (onePart :: twoPart :: otherParts)
                                |> List.filterMap
                                    (\part ->
                                        case part of
                                            FactExpansionPartValid _ ->
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
                                            FactExpansionPartValidVariableSubstitution substitution ->
                                                substitution |> Just

                                            _ ->
                                                Nothing
                                    )

                        -- TODO invert-substitute (?)
                    in
                    case variableSubstitutionParts of
                        [] ->
                            case notObviouslyValidParts |> List.LocalExtra.allOkMap (\part -> part |> factExpansionPartExpand project) of
                                Err error ->
                                    Err error

                                Ok partExpansions ->
                                    { factExpansionAny =
                                        partExpansions
                                            |> List.map .factExpansionAny
                                            |> List.LocalExtra.oneOfEach
                                            |> List.map
                                                (\newBranch ->
                                                    { factExpansionAll = newBranch |> List.concatMap .factExpansionAll }
                                                )
                                    }
                                        |> Ok

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
                            { factExpansionAll =
                                (if substitutionOfDeepestInVariable |> substitutionIsErasing then
                                    {- let
                                           _ =
                                               Debug.log
                                                   ([ "now substituting (erasing) "
                                                       , substitutionOfDeepestInVariable.variable |> scopedVariableNameToString
                                                       , " for "
                                                       , substitutionOfDeepestInVariable.value |> valueToString
                                                       ]
                                                       |> String.concat
                                                   )
                                                   ()
                                       in
                                    -}
                                    notObviouslyValidParts
                                        |> List.filter
                                            (\part ->
                                                part /= FactExpansionPartValidVariableSubstitution substitutionOfDeepestInVariable
                                            )

                                 else
                                    notObviouslyValidParts
                                )
                                    |> List.map
                                        (\part ->
                                            part
                                                |> factExpansionPartVariablesMap
                                                    (\variableName ->
                                                        if variableName == substitutionOfDeepestInVariable.variable then
                                                            substitutionOfDeepestInVariable.value

                                                        else
                                                            Variable variableName
                                                    )
                                        )
                                    |> List.concatMap .factExpansionAll
                            }
                                |> factExpansionAnySingleton
                                |> Ok


factExpansionPartExpand : RelationDefinitionsForExpansion -> (FactExpansionPart -> Result { relationNotDefined : String } FactExpansionAny)
factExpansionPartExpand relationDefinitions factExpansionPart =
    case factExpansionPart of
        FactExpansionPartValid whyValid ->
            FactExpansionPartValid whyValid
                |> factExpansionAllSingleton
                |> factExpansionAnySingleton
                |> Ok

        FactExpansionPartInvalid whyInvalid ->
            FactExpansionPartInvalid whyInvalid
                |> factExpansionAllSingleton
                |> factExpansionAnySingleton
                |> Ok

        FactExpansionPartValidVariableSubstitution substitution ->
            FactExpansionPartValidVariableSubstitution substitution
                |> factExpansionAllSingleton
                |> factExpansionAnySingleton
                |> Ok

        FactExpansionPartInvalidVariableSubstitution substitution ->
            FactExpansionPartInvalidVariableSubstitution substitution
                |> factExpansionAllSingleton
                |> factExpansionAnySingleton
                |> Ok

        FactExpansionPartValidRelation relation ->
            case relationDefinitions |> FastDict.get relation.identifier of
                Nothing ->
                    { relationNotDefined = relation.identifier }
                        |> Err

                Just definition ->
                    definition.equivalentFact
                        |> factVariablesMap
                            (\variableName ->
                                if variableName == definition.argumentVariable then
                                    relation.argument

                                else
                                    Variable
                                        { scope = relation.innerScope
                                        , name = variableName
                                        }
                            )
                        |> factExpand { scope = relation.innerScope }
                        |> Ok

        FactExpansionPartInvalidRelation inverseRelation ->
            case relationDefinitions |> FastDict.get inverseRelation.identifier of
                Nothing ->
                    { relationNotDefined = inverseRelation.identifier }
                        |> Err

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
                        |> factExpansionAnyInvert
                        |> factExpansionAnySingleton
                        |> Ok


valueVariablesMap :
    (variableName -> ValueWithVariableName mappedVariableName)
    ->
        (ValueWithVariableName variableName
         -> ValueWithVariableName mappedVariableName
        )
valueVariablesMap variableChange value =
    case value of
        Variable valueVariable ->
            valueVariable |> variableChange

        ValueLookup valueLookup ->
            valueLookup
                |> FastDict.map (\_ entryValue -> entryValue |> valueVariablesMap variableChange)
                |> ValueLookup


factVariablesMap :
    (variableName -> ValueWithVariableName mappedVariableName)
    ->
        (FactWithVariableName variableName
         -> FactWithVariableName mappedVariableName
        )
factVariablesMap variableChange fact =
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


factExpansionPartIsInvalid : FactExpansionPart -> Maybe WhyInvalid
factExpansionPartIsInvalid factExpansionPart =
    case factExpansionPart of
        FactExpansionPartInvalid reason ->
            Just reason

        _ ->
            Nothing


factExpansionPartVariablesMap :
    (ScopedVariableName -> ValueWithVariableName ScopedVariableName)
    -> (FactExpansionPart -> FactExpansionAll)
factExpansionPartVariablesMap variableChange fact =
    case fact of
        FactExpansionPartValid whyValid ->
            FactExpansionPartValid whyValid |> factExpansionAllSingleton

        FactExpansionPartInvalid whyInvalid ->
            FactExpansionPartInvalid whyInvalid |> factExpansionAllSingleton

        FactExpansionPartValidRelation factRelation ->
            { identifier = factRelation.identifier
            , argument =
                factRelation.argument
                    |> valueVariablesMap variableChange
            , innerScope = factRelation.innerScope
            }
                |> FactExpansionPartValidRelation
                |> factExpansionAllSingleton

        FactExpansionPartInvalidRelation factRelation ->
            { identifier = factRelation.identifier
            , argument =
                factRelation.argument
                    |> valueVariablesMap variableChange
            , innerScope = factRelation.innerScope
            }
                |> FactExpansionPartInvalidRelation
                |> factExpansionAllSingleton

        FactExpansionPartValidVariableSubstitution substitution ->
            { a = substitution.variable |> Variable |> valueVariablesMap variableChange
            , b = substitution.value |> valueVariablesMap variableChange
            }
                |> equal2Expand

        FactExpansionPartInvalidVariableSubstitution substitution ->
            { a = substitution.variable |> Variable |> valueVariablesMap variableChange
            , b = substitution.value |> valueVariablesMap variableChange
            }
                |> different2Expand


different2Expand :
    { a : ValueWithVariableName ScopedVariableName
    , b : ValueWithVariableName ScopedVariableName
    }
    -> FactExpansionAll
different2Expand bothValues =
    case ( bothValues.a, bothValues.b ) of
        ( Variable aVariable, ValueLookup bLookup ) ->
            FactExpansionPartInvalidVariableSubstitution
                { variable = aVariable, value = bLookup |> ValueLookup }
                |> factExpansionAllSingleton

        ( ValueLookup aLookup, Variable bVariable ) ->
            FactExpansionPartInvalidVariableSubstitution
                { variable = bVariable, value = aLookup |> ValueLookup }
                |> factExpansionAllSingleton

        ( Variable aVariable, Variable bVariable ) ->
            if aVariable == bVariable then
                FactExpansionPartInvalid
                    (InvalidBecauseVariableNotEqualToItself aVariable)
                    |> factExpansionAllSingleton

            else
                FactExpansionPartValidVariableSubstitution
                    ({ variable = aVariable, value = bVariable |> Variable }
                        |> variableSubstitutionToWithDeeperInAsVariable
                    )
                    |> factExpansionAllSingleton

        ( ValueLookup aLookup, ValueLookup bLookup ) ->
            { factExpansionAll =
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
                    |> List.concatMap .factExpansionAll
            }


substitutionIsErasing :
    { variable : variableName, value : ValueWithVariableName variableName }
    -> Bool
substitutionIsErasing substitution =
    case substitution.value of
        Variable variableName ->
            variableName /= substitution.variable

        ValueLookup lookup ->
            lookup
                |> fastDictAll
                    (\_ entryReplacement ->
                        { variable = substitution.variable, value = entryReplacement }
                            |> substitutionIsErasing
                    )


fastDictAll : (key -> value -> Bool) -> (FastDict.Dict key value -> Bool)
fastDictAll isFound fastDict =
    fastDict
        |> FastDict.restructure True
            (\state ->
                isFound state.key state.value && state.left () && state.right ()
            )


type ExpandedStatus
    = ExpandedToLookup Lookup
    | ExpansionNeedsToContinue FactExpansionAll
    | ExpansionFailed EvaluationError


{-| Unique identifier for a variable name during [evaluation](#evaluate) of a branch
-}
type alias ScopedVariableName =
    { scope : List ScopeRegion
    , name : String
    }


{-| Part of disambiguation of variables in expanded relation uses
-}
type ScopeRegion
    = ScopeRegionPartAtIndex Int
    | ScopeRegionBranchAtIndex Int
    | ScopeRegionIntoUseRelation String


{-| the [parts](#FactExpansionPart) to satisfy all in (partially) expanded form
-}
type alias FactExpansionAll =
    { factExpansionAll : List FactExpansionPart }


{-| Primitive, easy to evaluate or expand part of a [`FactExpansionAll`](#FactExpansionAll)
-}
type FactExpansionPart
    = FactExpansionPartValid WhyValid
    | FactExpansionPartInvalid WhyInvalid
    | FactExpansionPartValidVariableSubstitution
        { variable : ScopedVariableName
        , value : ValueWithVariableName ScopedVariableName
        }
    | FactExpansionPartInvalidVariableSubstitution
        { variable : ScopedVariableName
        , value : ValueWithVariableName ScopedVariableName
        }
    | FactExpansionPartValidRelation
        { identifier : String
        , argument : ValueWithVariableName ScopedVariableName
        , innerScope : List ScopeRegion
        }
    | FactExpansionPartInvalidRelation
        { identifier : String
        , argument : ValueWithVariableName ScopedVariableName
        , innerScope : List ScopeRegion
        }


{-| the possibilities to try in (partially) expanded form
-}
type alias FactExpansionAny =
    { factExpansionAny : List FactExpansionAll }
