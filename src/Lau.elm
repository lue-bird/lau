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
  - make running in parallel harder/less performant,
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


factExpansionAnySingleton : FactExpansionAll -> FactExpansionAny
factExpansionAnySingleton =
    \factExpansionAll ->
        { factExpansionAny = factExpansionAll |> List.singleton }


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


factExpansionAllSingleton : FactExpansionPart -> FactExpansionAll
factExpansionAllSingleton =
    \factExpansionPart -> { factExpansionAll = factExpansionPart |> List.singleton }


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


variableSubstitutionToWithDeeperInAsVariable :
    { variable : ScopedVariableName
    , value : ValueWithVariableName ScopedVariableName
    }
    ->
        { variable : ScopedVariableName
        , value : ValueWithVariableName ScopedVariableName
        }
variableSubstitutionToWithDeeperInAsVariable =
    \variableSubstitution ->
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
equal2Expand =
    \bothValues ->
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
                    FactExpansionPartValid
                        ([ "variable "
                         , aVariableName |> scopedVariableNameToString
                         , " equal to itself"
                         ]
                            |> String.concat
                        )
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
                            ([ "equating a value with an entry key "
                             , entryKey
                             , " with a value that does not contain this key."
                             ]
                                |> String.concat
                            )
                            |> factExpansionAllSingleton

                    Ok expanded ->
                        { factExpansionAll = expanded |> List.concatMap .factExpansionAll }


factExpansionAnyInvert : FactExpansionAny -> FactExpansionAll
factExpansionAnyInvert =
    \factExpansionAny ->
        { factExpansionAll =
            factExpansionAny.factExpansionAny
                |> List.concatMap
                    (\inverseFactBranch ->
                        inverseFactBranch |> factExpansionAllInvert |> .factExpansionAll
                    )
        }


factExpansionAllInvert : FactExpansionAll -> FactExpansionAll
factExpansionAllInvert =
    \factExpansionAll ->
        { factExpansionAll =
            factExpansionAll.factExpansionAll
                |> List.map factExpansionPartInvert
        }


factExpansionPartInvert : FactExpansionPart -> FactExpansionPart
factExpansionPartInvert =
    \factExpansionPart ->
        case factExpansionPart of
            FactExpansionPartValid whyValid ->
                -- TODO better error?
                FactExpansionPartInvalid ("not " ++ whyValid)

            FactExpansionPartInvalid whyInvalid ->
                -- TODO better error?
                FactExpansionPartValid ("not " ++ whyInvalid)

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
                    |> List.LauExtra.oneOfEach
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
                    |> factExpansionAnyExpandFully project


factExpansionAnyExpandFully : Project -> (FactExpansionAny -> Result String Lookup)
factExpansionAnyExpandFully project =
    \factExpansionAny ->
        let
            branchesExpandedStatues : List ExpandedStatus
            branchesExpandedStatues =
                factExpansionAny.factExpansionAny
                    |> List.map factExpansionAllToExpandedStatus

            firstBranchWithLookupResult : Maybe Lookup
            firstBranchWithLookupResult =
                branchesExpandedStatues
                    |> List.LauExtra.firstJustMap
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

                    determinedOrNeedsExpanding : Result (List FactExpansionAll) Lookup
                    determinedOrNeedsExpanding =
                        expandableBranchFacts
                            |> List.concatMap (\branch -> branch |> factExpansionAllExpand project |> .factExpansionAny)
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
                            branchExpansionStops : List String
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
                        Err
                            ("In every branch I've tried, evaluation has stopped but none returned a concrete value: "
                                ++ (branchExpansionStops |> String.join "\n")
                            )

                    Err needsExpanding ->
                        if needsExpanding == expandableBranchFacts then
                            Err
                                ("the result isn't constrained to a specific solution. Here's what I figured out: "
                                    ++ (needsExpanding |> List.map factExpansionAllToString |> String.join "\n")
                                )

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
factExpansionAllToExpandedStatus =
    \factExpansionAll ->
        case factExpansionAll.factExpansionAll of
            [] ->
                ExpansionFailed "empty all"

            [ onlyPart ] ->
                onlyPart |> factExpansionPartToExpandedStatus

            part0 :: part1Up ->
                ExpansionNeedsToContinue { factExpansionAll = part0 :: part1Up }


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


factExpansionPartToExpandedStatus : FactExpansionPart -> ExpandedStatus
factExpansionPartToExpandedStatus =
    \fact ->
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
                                ExpansionNeedsToContinue
                                    (FactExpansionPartValidVariableSubstitution substitution |> factExpansionAllSingleton)

            FactExpansionPartValidRelation validRelation ->
                ExpansionNeedsToContinue
                    (FactExpansionPartValidRelation validRelation |> factExpansionAllSingleton)

            FactExpansionPartInvalidRelation invalidRelation ->
                ExpansionNeedsToContinue
                    (FactExpansionPartInvalidRelation invalidRelation |> factExpansionAllSingleton)

            FactExpansionPartValid whyValid ->
                ExpansionFailed
                    ([ "all values are possible: "
                     , whyValid
                     , ". Add some constructive constraints so I can choose a good value"
                     ]
                        |> String.concat
                    )

            FactExpansionPartInvalid whyInvalid ->
                ExpansionFailed whyInvalid

            FactExpansionPartInvalidVariableSubstitution invalidSubstitution ->
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


factExpansionAllToString : FactExpansionAll -> String
factExpansionAllToString =
    \factExpansionAll ->
        [ "("
        , factExpansionAll.factExpansionAll |> List.map factExpansionPartToString |> String.join " and "
        , ")"
        ]
            |> String.concat


factExpansionPartToString : FactExpansionPart -> String
factExpansionPartToString =
    \factExpansionPart ->
        case factExpansionPart of
            FactExpansionPartValid whyValid ->
                [ "✅\"", whyValid, "\"" ] |> String.concat

            FactExpansionPartInvalid whyInvalid ->
                [ "❌\"", whyInvalid, "\"" ] |> String.concat

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


factExpansionAllExpand : Project -> (FactExpansionAll -> FactExpansionAny)
factExpansionAllExpand project =
    \factExpansionAll ->
        case factExpansionAll.factExpansionAll of
            [] ->
                FactExpansionPartValid "empty all" |> factExpansionAllSingleton |> factExpansionAnySingleton

            [ only ] ->
                only |> factExpansionPartExpand project

            onePart :: twoPart :: otherParts ->
                case (onePart :: twoPart :: otherParts) |> List.LauExtra.firstJustMap factExpansionPartIsInvalid of
                    Just reason ->
                        FactExpansionPartInvalid reason |> factExpansionAllSingleton |> factExpansionAnySingleton

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
                                { factExpansionAny =
                                    notObviouslyValidParts
                                        |> List.map (\part -> part |> factExpansionPartExpand project |> .factExpansionAny)
                                        |> List.LauExtra.oneOfEach
                                        |> List.map
                                            (\newBranch ->
                                                { factExpansionAll = newBranch |> List.concatMap .factExpansionAll }
                                            )
                                }

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


factExpansionPartExpand : Project -> (FactExpansionPart -> FactExpansionAny)
factExpansionPartExpand project =
    \factExpansionPart ->
        case factExpansionPart of
            FactExpansionPartValid whyValid ->
                FactExpansionPartValid whyValid |> factExpansionAllSingleton |> factExpansionAnySingleton

            FactExpansionPartInvalid whyInvalid ->
                FactExpansionPartInvalid whyInvalid |> factExpansionAllSingleton |> factExpansionAnySingleton

            FactExpansionPartValidVariableSubstitution substitution ->
                FactExpansionPartValidVariableSubstitution substitution
                    |> factExpansionAllSingleton
                    |> factExpansionAnySingleton

            FactExpansionPartInvalidVariableSubstitution substitution ->
                FactExpansionPartInvalidVariableSubstitution substitution
                    |> factExpansionAllSingleton
                    |> factExpansionAnySingleton

            FactExpansionPartValidRelation relation ->
                case project.relationDefinitions |> FastDict.get relation.identifier of
                    Nothing ->
                        FactExpansionPartInvalid
                            ([ "The relation "
                             , relation.identifier
                             , " isn't defined. Define it or choose a differently named relation."
                             ]
                                |> String.concat
                            )
                            |> factExpansionAllSingleton
                            |> factExpansionAnySingleton

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

            FactExpansionPartInvalidRelation inverseRelation ->
                case project.relationDefinitions |> FastDict.get inverseRelation.identifier of
                    Nothing ->
                        FactExpansionPartInvalid
                            ([ "The relation "
                             , inverseRelation.identifier
                             , " isn't defined. Define it or choose a differently named relation."
                             ]
                                |> String.concat
                            )
                            |> factExpansionAllSingleton
                            |> factExpansionAnySingleton

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


factExpansionPartIsInvalid : FactExpansionPart -> Maybe String
factExpansionPartIsInvalid =
    \factExpansionPart ->
        case factExpansionPart of
            FactExpansionPartInvalid reason ->
                Just reason

            _ ->
                Nothing


factExpansionPartVariablesMap :
    (ScopedVariableName -> ValueWithVariableName ScopedVariableName)
    -> (FactExpansionPart -> FactExpansionAll)
factExpansionPartVariablesMap variableChange =
    \fact ->
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
different2Expand =
    \bothValues ->
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
                        ([ "expected variables "
                         , aVariable.name
                         , " and "
                         , bVariable.name
                         , " to be different but they're set to be equal"
                         ]
                            |> String.concat
                        )
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


type ExpandedStatus
    = ExpandedToLookup Lookup
    | ExpansionNeedsToContinue FactExpansionAll
    | ExpansionFailed String


type alias ScopedVariableName =
    { scope : List ScopeRegion
    , name : String
    }


type ScopeRegion
    = ScopeRegionPartAtIndex Int
    | ScopeRegionBranchAtIndex Int
    | ScopeRegionIntoUseRelation String


type alias FactExpansionAll =
    { factExpansionAll : List FactExpansionPart }


type FactExpansionPart
    = FactExpansionPartValid String
    | FactExpansionPartInvalid String
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


type alias FactExpansionAny =
    { factExpansionAny : List FactExpansionAll }
