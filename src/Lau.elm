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
import List.LocalExtra


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
    | Equal (List (ValueWithVariableName variableName))
    | Not (FactWithVariableName variableName)
    | All (List (FactWithVariableName variableName))
    | Any (List (FactWithVariableName variableName))


{-| A variable or [`lookup`](#lookup) that can have variables for some of its entry values
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
            BranchValid ->
                -- TODO better error?
                [ BranchInvalid "excluded" ]

            BranchInvalid _ ->
                [ BranchValid ]

            BranchValidRelation relation ->
                [ BranchInvalidRelation relation ]

            BranchInvalidRelation relation ->
                [ BranchValidRelation relation ]

            BranchValidVariableSubstitution substitution ->
                [ BranchInvalidVariableSubstitution substitution ]

            BranchInvalidVariableSubstitution substitution ->
                [ BranchValidVariableSubstitution substitution ]

            BranchAnd parts ->
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


valueToValueLookup : ValueWithVariableName variableName -> Maybe (FastDict.Dict String (ValueWithVariableName variableName))
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
        (aVariable.scope |> List.length) > (bVariable.scope |> List.length)


equalListExpand :
    List (ValueWithVariableName ScopedVariableName)
    -> List FactBranch
equalListExpand =
    \values ->
        List.LocalExtra.firstJustMap (\f -> f ())
            [ \() ->
                case values |> List.LocalExtra.allJustMap valueToVariable of
                    Nothing ->
                        Nothing

                    Just [] ->
                        [ BranchValid ] |> Just

                    Just (aVariable :: allVariablesEqualToA) ->
                        allVariablesEqualToA
                            |> List.map
                                (\bVariable ->
                                    if aVariable == bVariable then
                                        BranchValid

                                    else if aVariable |> scopedVariableIsDeeperInThan bVariable then
                                        BranchValidVariableSubstitution
                                            { variable = aVariable
                                            , value = bVariable |> Variable
                                            }

                                    else
                                        BranchValidVariableSubstitution
                                            { variable = bVariable
                                            , value = aVariable |> Variable
                                            }
                                )
                            |> Just
            , \() ->
                case values |> List.LocalExtra.allJustMap valueToValueLookup of
                    Nothing ->
                        Nothing

                    Just [] ->
                        [ BranchValid ] |> Just

                    Just (aValueLookup :: allValueLookupsEqualToA) ->
                        let
                            entryCountIsSameForAll : Bool
                            entryCountIsSameForAll =
                                allValueLookupsEqualToA
                                    |> List.all
                                        (\valueLookup ->
                                            (valueLookup |> FastDict.size) == (aValueLookup |> FastDict.size)
                                        )
                        in
                        if not entryCountIsSameForAll then
                            BranchInvalid
                                "some values have entries which others don't but you set them as equal which is impossible"
                                |> List.singleton
                                |> Just

                        else
                            let
                                entriesExpanded : Result String (List FactBranch)
                                entriesExpanded =
                                    aValueLookup
                                        |> FastDict.toList
                                        |> List.foldl
                                            (\( entryName, aEntryValue ) soFar ->
                                                case soFar of
                                                    Err sometimesMissingEntryName ->
                                                        Err sometimesMissingEntryName

                                                    Ok expandedSoFar ->
                                                        let
                                                            maybeAllEntryValuesEqualToAEntryValue : Maybe (List (ValueWithVariableName ScopedVariableName))
                                                            maybeAllEntryValuesEqualToAEntryValue =
                                                                allValueLookupsEqualToA
                                                                    |> List.LocalExtra.allJustMap
                                                                        (\valueLookupEqualToA ->
                                                                            valueLookupEqualToA |> FastDict.get entryName
                                                                        )
                                                        in
                                                        case maybeAllEntryValuesEqualToAEntryValue of
                                                            Nothing ->
                                                                Err entryName

                                                            Just entryValuesEqualToAEntryValue ->
                                                                ((aEntryValue :: entryValuesEqualToAEntryValue) |> equalListExpand)
                                                                    ++ expandedSoFar
                                                                    |> Ok
                                            )
                                            (Ok [])
                            in
                            case entriesExpanded of
                                Err sometimesMissingEntryName ->
                                    BranchInvalid
                                        ([ "some values have the entry name "
                                         , sometimesMissingEntryName
                                         , " which others don't but you set them as equal which is impossible"
                                         ]
                                            |> String.concat
                                        )
                                        |> List.singleton
                                        |> Just

                                Ok expanded ->
                                    expanded |> Just
            , \() ->
                let
                    maybeDeepestInVariable : Maybe ScopedVariableName
                    maybeDeepestInVariable =
                        values
                            |> List.foldl
                                (\value soFar ->
                                    case value of
                                        ValueLookup _ ->
                                            soFar

                                        Variable variableName ->
                                            case soFar of
                                                Nothing ->
                                                    variableName |> Just

                                                Just deepestInVariableSoFar ->
                                                    if variableName |> scopedVariableIsDeeperInThan deepestInVariableSoFar then
                                                        variableName |> Just

                                                    else
                                                        deepestInVariableSoFar |> Just
                                )
                                Nothing
                in
                case maybeDeepestInVariable of
                    Nothing ->
                        Nothing

                    Just deepestInVariable ->
                        let
                            valuesWithoutLeastRootVariable : List (ValueWithVariableName ScopedVariableName)
                            valuesWithoutLeastRootVariable =
                                values
                                    |> List.filterMap
                                        (\value ->
                                            if value == Variable deepestInVariable then
                                                Nothing

                                            else
                                                Just value
                                        )
                        in
                        valuesWithoutLeastRootVariable
                            |> List.map
                                (\otherValue ->
                                    BranchValidVariableSubstitution
                                        { variable = deepestInVariable, value = otherValue }
                                )
                            |> Just
            ]
            -- above cases should catch everything
            |> Maybe.withDefault []


valueToVariable : ValueWithVariableName variableName -> Maybe variableName
valueToVariable =
    \value ->
        case value of
            ValueLookup _ ->
                Nothing

            Variable variableName ->
                variableName |> Just


factExpand : FactWithVariableName ScopedVariableName -> List FactBranch
factExpand =
    \fact ->
        case fact of
            RelationUse relation ->
                BranchValidRelation
                    { identifier = relation.identifier
                    , argument =
                        relation.argument
                            |> valueVariablesMap
                                (\v -> Variable v)
                    }
                    |> List.singleton

            Equal values ->
                values
                    |> List.map
                        (\value ->
                            value |> valueVariablesMap Variable
                        )
                    |> equalListExpand

            Not inverseFact ->
                inverseFact
                    |> factExpand
                    |> List.concatMap factBranchInvert
                    |> BranchAnd
                    |> List.singleton

            All parts ->
                parts
                    |> List.map factExpand
                    |> List.LocalExtra.oneOfEach
                    |> List.map (\branchParts -> branchParts |> BranchAnd)

            Any branches ->
                branches
                    |> List.concatMap factExpand


{-| Find a value that satisfies the constraints for an argument variable in the main definition
-}
evaluate : Project -> Result String Lookup
evaluate =
    \project ->
        case project.relationDefinitions |> FastDict.get "main" of
            Nothing ->
                Err "The relation main isn't defined. Define it."

            Just mainImplementation ->
                RelationUse
                    { identifier = "main"
                    , argument =
                        { scope = [], name = mainImplementation.argumentVariable }
                            |> Variable
                    }
                    |> factExpand
                    |> factBranchesExpandFully project
                    |> Ok


scopedVariableNameToString : ScopedVariableName -> String
scopedVariableNameToString =
    \scopedVariable ->
        case scopedVariable.scope of
            [] ->
                scopedVariable.name

            scopePart0 :: scopePart1Up ->
                [ (scopePart0 :: scopePart1Up) |> String.join "/"
                , "/"
                , scopedVariable.name
                ]
                    |> String.concat


factBranchToString : FactBranch -> String
factBranchToString =
    \factBranch ->
        case factBranch of
            BranchValid ->
                "✅"

            BranchInvalid reason ->
                [ "❌\"", reason, "\"" ] |> String.concat

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

            BranchAnd parts ->
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


factBranchExpand : Project -> (FactBranch -> List FactBranch)
factBranchExpand project =
    \factBranch ->
        (case factBranch of
            BranchValid ->
                [ BranchValid ]

            BranchInvalid reason ->
                [ BranchInvalid reason ]

            BranchValidVariableSubstitution substitution ->
                [ BranchValidVariableSubstitution substitution ]

            BranchInvalidVariableSubstitution substitution ->
                [ BranchInvalidVariableSubstitution substitution ]

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

                                    else
                                        Variable { scope = [ relation.identifier ], name = variableName }
                                )
                            |> factExpand

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
                                        Variable { scope = [ inverseRelation.identifier ], name = variableName }
                                )
                            |> factExpand
                            |> List.concatMap factBranchInvert
                            |> BranchAnd
                            |> List.singleton

            BranchAnd parts ->
                parts |> branchAndExpand project
        )
            |> (\a ->
                    let
                        _ =
                            Debug.log "fact expanded to" (a |> List.map factBranchToString |> String.join "\n")
                    in
                    a
               )


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
                    (values
                        |> List.map (\value -> value |> valueVariablesMap variableChange)
                    )

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


factBranchesExpandFully : Project -> (List FactBranch -> Lookup)
factBranchesExpandFully project =
    \branchesIncludingInvalid ->
        let
            expandableBranches : List FactBranch
            expandableBranches =
                branchesIncludingInvalid
                    |> List.filterMap
                        (\branch ->
                            case branch of
                                BranchInvalid _ ->
                                    -- TODO use error
                                    Nothing

                                BranchValid ->
                                    -- TODO err "all values are possible. Add some constraints so I can choose a good value"
                                    Nothing

                                BranchInvalidVariableSubstitution _ ->
                                    -- TODO err "all values except specific ones possible. Add some constructive constraints so I can choose a good value"
                                    Nothing

                                branchNotDirectlyInvalid ->
                                    branchNotDirectlyInvalid |> Just
                        )
        in
        case expandableBranches |> List.LocalExtra.firstJustMap factBranchToDetermined of
            Just determined ->
                determined

            Nothing ->
                let
                    determinedOrNeedsExpanding : Result (List FactBranch) Lookup
                    determinedOrNeedsExpanding =
                        expandableBranches
                            |> List.concatMap (\branch -> branch |> factBranchExpand project)
                            |> List.foldl
                                (\branchExpanded soFar ->
                                    case soFar of
                                        Ok determined ->
                                            determined |> Ok

                                        Err soFarBranchesNeedExpanding ->
                                            case branchExpanded |> factBranchToDetermined of
                                                Just determined ->
                                                    determined |> Ok

                                                Nothing ->
                                                    branchExpanded :: soFarBranchesNeedExpanding |> Err
                                )
                                (Err [])
                in
                case determinedOrNeedsExpanding of
                    Ok determined ->
                        determined

                    Err needsExpanding ->
                        needsExpanding
                            |> (\a ->
                                    let
                                        _ =
                                            Debug.log "all facts expanded to" (a |> List.map factBranchToString |> String.join "\n")
                                    in
                                    a
                               )
                            |> factBranchesExpandFully project


factBranchToDetermined : FactBranch -> Maybe Lookup
factBranchToDetermined =
    \fact ->
        case fact of
            BranchValidVariableSubstitution substitution ->
                case substitution.variable.scope of
                    [] ->
                        case substitution.value |> valueToValueLookup |> Maybe.andThen valueLookupToLookup of
                            Nothing ->
                                Nothing

                            Just lookup ->
                                lookup |> Just

                    _ :: _ ->
                        -- continue expanding
                        Nothing

            BranchAnd _ ->
                -- continue expanding
                Nothing

            BranchValidRelation _ ->
                -- continue expanding
                Nothing

            BranchInvalidRelation _ ->
                -- continue expanding
                Nothing

            BranchValid ->
                --   stop expanding
                Nothing

            BranchInvalid _ ->
                --   stop expanding with error
                Nothing

            BranchInvalidVariableSubstitution _ ->
                --   stop expanding with error
                Nothing


valueLookupToLookup : FastDict.Dict String (ValueWithVariableName variableName_) -> Maybe Lookup
valueLookupToLookup =
    \valueLookup ->
        valueLookup
            |> FastDict.toList
            |> List.LocalExtra.allJustMap
                (\( identifier, value ) ->
                    Maybe.map (\lookup -> ( identifier, lookup ))
                        (value |> valueToValueLookup |> Maybe.andThen valueLookupToLookup)
                )
            |> Maybe.map (\entries -> entries |> FastDict.fromList |> Lookup)


{-| TODO convert to record
-}
type alias ScopedVariableName =
    { scope : List String
    , name : String
    }


type FactBranch
    = BranchValid
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
        }
    | BranchInvalidRelation
        { identifier : String
        , argument : ValueWithVariableName ScopedVariableName
        }
    | BranchAnd (List FactBranch)


branchAndExpand : Project -> (List FactBranch -> List FactBranch)
branchAndExpand project =
    \parts ->
        case parts of
            [] ->
                BranchValid |> List.singleton

            [ only ] ->
                only |> List.singleton

            onePart :: twoPart :: otherParts ->
                case (onePart :: twoPart :: otherParts) |> List.LocalExtra.firstJustMap factBranchIsInvalid of
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
                                                BranchValid ->
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
                                    |> List.LocalExtra.oneOfEach
                                    |> List.map (\branchParts -> branchParts |> BranchAnd)

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
                                    let
                                        _ =
                                            Debug.log "erasing" substitutionOfDeepestInVariable
                                    in
                                    notObviouslyValidParts
                                        |> List.filter
                                            (\part ->
                                                part /= BranchValidVariableSubstitution substitutionOfDeepestInVariable
                                            )

                                 else
                                    let
                                        _ =
                                            Debug.log "not erasing" substitutionOfDeepestInVariable
                                    in
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
                                    |> BranchAnd
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
            BranchValid ->
                BranchValid

            BranchInvalid reason ->
                BranchInvalid reason

            BranchValidRelation factRelation ->
                { identifier = factRelation.identifier
                , argument =
                    factRelation.argument
                        |> valueVariablesMap variableChange
                }
                    |> BranchValidRelation

            BranchInvalidRelation factRelation ->
                { identifier = factRelation.identifier
                , argument =
                    factRelation.argument
                        |> valueVariablesMap variableChange
                }
                    |> BranchInvalidRelation

            BranchValidVariableSubstitution substitution ->
                [ substitution.variable |> Variable |> valueVariablesMap variableChange
                , substitution.value |> valueVariablesMap variableChange
                ]
                    |> equalListExpand
                    |> BranchAnd

            BranchInvalidVariableSubstitution substitution ->
                { a = substitution.variable |> Variable |> valueVariablesMap variableChange
                , b = substitution.value |> valueVariablesMap variableChange
                }
                    |> different2Expand
                    |> BranchAnd

            BranchAnd parts ->
                parts
                    |> List.map (\part -> part |> factBranchVariablesMap variableChange)
                    |> BranchAnd


different2Expand :
    { a : ValueWithVariableName ScopedVariableName
    , b : ValueWithVariableName ScopedVariableName
    }
    -> List FactBranch
different2Expand =
    \bothValues ->
        case ( bothValues.a, bothValues.b ) of
            ( Variable aVariable, ValueLookup bLookup ) ->
                BranchInvalidVariableSubstitution
                    { variable = aVariable, value = bLookup |> ValueLookup }
                    |> List.singleton

            ( ValueLookup aLookup, Variable bVariable ) ->
                BranchInvalidVariableSubstitution
                    { variable = bVariable, value = aLookup |> ValueLookup }
                    |> List.singleton

            ( Variable aVariable, Variable bVariable ) ->
                [ if aVariable == bVariable then
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
                ]

            ( ValueLookup aLookup, ValueLookup bLookup ) ->
                FastDict.merge
                    (\_ _ soFar -> soFar)
                    (\_ aEntryValue bEntryValue soFar ->
                        ({ a = aEntryValue, b = bEntryValue } |> different2Expand)
                            ++ soFar
                    )
                    (\_ _ soFar -> soFar)
                    aLookup
                    bLookup
                    []


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
