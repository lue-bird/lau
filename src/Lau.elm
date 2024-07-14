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


factBranchAllSingleton : FactBranchPart -> FactBranchAll
factBranchAllSingleton =
    \factBranch -> { branchAll = factBranch |> List.singleton }


factExpansionAnySingleton : FactBranchAll -> FactExpansionAny
factExpansionAnySingleton =
    \factBranchAll ->
        { factExpansionAny = factBranchAll |> List.singleton }


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
    -> FactBranchAll
equal2Expand =
    \bothValues ->
        case ( bothValues.a, bothValues.b ) of
            ( Variable aVariable, ValueLookup bLookup ) ->
                BranchPartValidVariableSubstitution
                    { variable = aVariable, value = bLookup |> ValueLookup }
                    |> factBranchAllSingleton

            ( ValueLookup aLookup, Variable bVariable ) ->
                BranchPartValidVariableSubstitution
                    { variable = bVariable, value = aLookup |> ValueLookup }
                    |> factBranchAllSingleton

            ( Variable aVariableName, Variable bVariableName ) ->
                if aVariableName == bVariableName then
                    BranchPartValid
                        ([ "variable "
                         , aVariableName |> scopedVariableNameToString
                         , " equal to itself"
                         ]
                            |> String.concat
                        )
                        |> factBranchAllSingleton

                else
                    BranchPartValidVariableSubstitution
                        ({ variable = aVariableName
                         , value = Variable bVariableName
                         }
                            |> variableSubstitutionToWithDeeperInAsVariable
                        )
                        |> factBranchAllSingleton

            ( ValueLookup aLookup, ValueLookup bLookup ) ->
                let
                    entriesExpanded : Result String (List FactBranchAll)
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
                        BranchPartInvalid
                            ([ "equating a value with an entry key "
                             , entryKey
                             , " with a value that does not contain this key."
                             ]
                                |> String.concat
                            )
                            |> factBranchAllSingleton

                    Ok expanded ->
                        { branchAll = expanded |> List.concatMap .branchAll }


factExpansionAnyInvert : FactExpansionAny -> FactBranchAll
factExpansionAnyInvert =
    \factExpansionAny ->
        { branchAll =
            factExpansionAny.factExpansionAny
                |> List.concatMap
                    (\inverseFactBranch ->
                        inverseFactBranch |> factBranchAllInvert |> .branchAll
                    )
        }


factBranchAllInvert : FactBranchAll -> FactBranchAll
factBranchAllInvert =
    \factBranchAll ->
        { branchAll = factBranchAll.branchAll |> List.map factBranchPartInvert
        }


factBranchPartInvert : FactBranchPart -> FactBranchPart
factBranchPartInvert =
    \factBranch ->
        case factBranch of
            BranchPartValid whyValid ->
                -- TODO better error?
                BranchPartInvalid ("not " ++ whyValid)

            BranchPartInvalid whyInvalid ->
                -- TODO better error?
                BranchPartValid ("not " ++ whyInvalid)

            BranchPartValidRelation relation ->
                BranchPartInvalidRelation relation

            BranchPartInvalidRelation relation ->
                BranchPartValidRelation relation

            BranchPartValidVariableSubstitution substitution ->
                BranchPartInvalidVariableSubstitution substitution

            BranchPartInvalidVariableSubstitution substitution ->
                BranchPartValidVariableSubstitution substitution


factExpand :
    { scope : List ScopeRegion }
    -> (FactWithVariableName ScopedVariableName -> FactExpansionAny)
factExpand context fact =
    case fact of
        RelationUse relation ->
            BranchPartValidRelation
                { identifier = relation.identifier
                , argument = relation.argument
                , innerScope = ScopeRegionIntoUseRelation relation.identifier :: context.scope
                }
                |> factBranchAllSingleton
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
                        (\branchParts ->
                            { branchAll =
                                branchParts |> List.concatMap .branchAll
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
    \branchesIncludingInvalid ->
        let
            branchesExpandedResults : List ExpandedResult
            branchesExpandedResults =
                branchesIncludingInvalid.factExpansionAny
                    |> List.map factBranchAllToExpandedResult

            firstBranchWithLookupResult : Maybe Lookup
            firstBranchWithLookupResult =
                branchesExpandedResults
                    |> List.LauExtra.firstJustMap
                        (\branchExpandedResult ->
                            case branchExpandedResult of
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
                    expandableBranchFacts : List FactBranchAll
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

                    determinedOrNeedsExpanding : Result (List FactBranchAll) Lookup
                    determinedOrNeedsExpanding =
                        expandableBranchFacts
                            |> List.concatMap (\branch -> branch |> factBranchAllExpand project |> .factExpansionAny)
                            |> List.foldl
                                (\branchExpanded soFar ->
                                    case soFar of
                                        Ok determined ->
                                            determined |> Ok

                                        Err soFarBranchesNeedExpanding ->
                                            case branchExpanded |> factBranchAllToExpandedResult of
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
                                    ++ (needsExpanding |> List.map factBranchAllToString |> String.join "\n")
                                )

                        else
                            {- let
                                   _ =
                                       Debug.log
                                           ([ "------------------------ all facts expanded to --------------------------\n"
                                            , needsExpanding
                                               |> List.map (\factBranch -> "  - " ++ (factBranch |> factBranchAllToString))
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


factBranchAllToExpandedResult : FactBranchAll -> ExpandedResult
factBranchAllToExpandedResult =
    \factBranchAll ->
        case factBranchAll.branchAll of
            [] ->
                ExpansionFailed "empty all"

            [ onlyPart ] ->
                onlyPart |> factBranchToExpandedResult

            part0 :: part1Up ->
                ExpansionNeedsToContinue { branchAll = part0 :: part1Up }


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


factBranchToExpandedResult : FactBranchPart -> ExpandedResult
factBranchToExpandedResult =
    \fact ->
        case fact of
            BranchPartValidVariableSubstitution substitution ->
                case substitution.variable.scope of
                    [] ->
                        case substitution.value |> valueToValueLookup |> Maybe.andThen valueLookupToLookup of
                            Nothing ->
                                ExpansionNeedsToContinue
                                    (BranchPartValidVariableSubstitution substitution
                                        |> factBranchAllSingleton
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
                                    (BranchPartValidVariableSubstitution substitution |> factBranchAllSingleton)

            BranchPartValidRelation validRelation ->
                ExpansionNeedsToContinue
                    (BranchPartValidRelation validRelation |> factBranchAllSingleton)

            BranchPartInvalidRelation invalidRelation ->
                ExpansionNeedsToContinue
                    (BranchPartInvalidRelation invalidRelation |> factBranchAllSingleton)

            BranchPartValid whyValid ->
                ExpansionFailed
                    ([ "all values are possible: "
                     , whyValid
                     , ". Add some constructive constraints so I can choose a good value"
                     ]
                        |> String.concat
                    )

            BranchPartInvalid whyInvalid ->
                ExpansionFailed whyInvalid

            BranchPartInvalidVariableSubstitution invalidSubstitution ->
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


factBranchAllToString : FactBranchAll -> String
factBranchAllToString =
    \factBranchAll ->
        [ "("
        , factBranchAll.branchAll |> List.map factBranchPartToString |> String.join " and "
        , ")"
        ]
            |> String.concat


factBranchPartToString : FactBranchPart -> String
factBranchPartToString =
    \factBranch ->
        case factBranch of
            BranchPartValid whyValid ->
                [ "✅\"", whyValid, "\"" ] |> String.concat

            BranchPartInvalid whyInvalid ->
                [ "❌\"", whyInvalid, "\"" ] |> String.concat

            BranchPartValidVariableSubstitution substitution ->
                [ "("
                , substitution.variable |> scopedVariableNameToString
                , " = "
                , substitution.value |> valueToString
                , ")"
                ]
                    |> String.concat

            BranchPartInvalidVariableSubstitution substitution ->
                [ "("
                , substitution.variable |> scopedVariableNameToString
                , " ≠ "
                , substitution.value |> valueToString
                , ")"
                ]
                    |> String.concat

            BranchPartValidRelation relation ->
                [ "("
                , relation.identifier
                , " "
                , relation.argument |> valueToString
                , ")"
                ]
                    |> String.concat

            BranchPartInvalidRelation relation ->
                [ "(not ("
                , relation.identifier
                , " "
                , relation.argument |> valueToString
                , "))"
                ]
                    |> String.concat


factBranchAllExpand : Project -> (FactBranchAll -> FactExpansionAny)
factBranchAllExpand project =
    \factBranchAll ->
        case factBranchAll.branchAll of
            [] ->
                BranchPartValid "empty all" |> factBranchAllSingleton |> factExpansionAnySingleton

            [ only ] ->
                only |> factBranchPartExpand project

            onePart :: twoPart :: otherParts ->
                case (onePart :: twoPart :: otherParts) |> List.LauExtra.firstJustMap factBranchIsInvalid of
                    Just reason ->
                        BranchPartInvalid reason |> factBranchAllSingleton |> factExpansionAnySingleton

                    Nothing ->
                        let
                            notObviouslyValidParts : List FactBranchPart
                            notObviouslyValidParts =
                                (onePart :: twoPart :: otherParts)
                                    |> List.filterMap
                                        (\part ->
                                            case part of
                                                BranchPartValid _ ->
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
                                                BranchPartValidVariableSubstitution substitution ->
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
                                        |> List.map (\part -> part |> factBranchPartExpand project |> .factExpansionAny)
                                        |> List.LauExtra.oneOfEach
                                        |> List.map
                                            (\branchParts ->
                                                { branchAll = branchParts |> List.concatMap .branchAll }
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
                                { branchAll =
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
                                                    part /= BranchPartValidVariableSubstitution substitutionOfDeepestInVariable
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
                                        |> List.concatMap .branchAll
                                }
                                    |> factExpansionAnySingleton


factBranchPartExpand : Project -> (FactBranchPart -> FactExpansionAny)
factBranchPartExpand project factBranch =
    case factBranch of
        BranchPartValid whyValid ->
            BranchPartValid whyValid |> factBranchAllSingleton |> factExpansionAnySingleton

        BranchPartInvalid whyInvalid ->
            BranchPartInvalid whyInvalid |> factBranchAllSingleton |> factExpansionAnySingleton

        BranchPartValidVariableSubstitution substitution ->
            BranchPartValidVariableSubstitution substitution
                |> factBranchAllSingleton
                |> factExpansionAnySingleton

        BranchPartInvalidVariableSubstitution substitution ->
            BranchPartInvalidVariableSubstitution substitution
                |> factBranchAllSingleton
                |> factExpansionAnySingleton

        BranchPartValidRelation relation ->
            case project.relationDefinitions |> FastDict.get relation.identifier of
                Nothing ->
                    BranchPartInvalid
                        ([ "The relation "
                         , relation.identifier
                         , " isn't defined. Define it or choose a differently named relation."
                         ]
                            |> String.concat
                        )
                        |> factBranchAllSingleton
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

        BranchPartInvalidRelation inverseRelation ->
            case project.relationDefinitions |> FastDict.get inverseRelation.identifier of
                Nothing ->
                    BranchPartInvalid
                        ([ "The relation "
                         , inverseRelation.identifier
                         , " isn't defined. Define it or choose a differently named relation."
                         ]
                            |> String.concat
                        )
                        |> factBranchAllSingleton
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


factBranchIsInvalid : FactBranchPart -> Maybe String
factBranchIsInvalid =
    \factBranch ->
        case factBranch of
            BranchPartInvalid reason ->
                Just reason

            _ ->
                Nothing


factBranchVariablesMap :
    (ScopedVariableName -> ValueWithVariableName ScopedVariableName)
    -> (FactBranchPart -> FactBranchAll)
factBranchVariablesMap variableChange =
    \fact ->
        case fact of
            BranchPartValid whyValid ->
                BranchPartValid whyValid |> factBranchAllSingleton

            BranchPartInvalid whyInvalid ->
                BranchPartInvalid whyInvalid |> factBranchAllSingleton

            BranchPartValidRelation factRelation ->
                { identifier = factRelation.identifier
                , argument =
                    factRelation.argument
                        |> valueVariablesMap variableChange
                , innerScope = factRelation.innerScope
                }
                    |> BranchPartValidRelation
                    |> factBranchAllSingleton

            BranchPartInvalidRelation factRelation ->
                { identifier = factRelation.identifier
                , argument =
                    factRelation.argument
                        |> valueVariablesMap variableChange
                , innerScope = factRelation.innerScope
                }
                    |> BranchPartInvalidRelation
                    |> factBranchAllSingleton

            BranchPartValidVariableSubstitution substitution ->
                { a = substitution.variable |> Variable |> valueVariablesMap variableChange
                , b = substitution.value |> valueVariablesMap variableChange
                }
                    |> equal2Expand

            BranchPartInvalidVariableSubstitution substitution ->
                { a = substitution.variable |> Variable |> valueVariablesMap variableChange
                , b = substitution.value |> valueVariablesMap variableChange
                }
                    |> different2Expand


different2Expand :
    { a : ValueWithVariableName ScopedVariableName
    , b : ValueWithVariableName ScopedVariableName
    }
    -> FactBranchAll
different2Expand =
    \bothValues ->
        case ( bothValues.a, bothValues.b ) of
            ( Variable aVariable, ValueLookup bLookup ) ->
                BranchPartInvalidVariableSubstitution
                    { variable = aVariable, value = bLookup |> ValueLookup }
                    |> factBranchAllSingleton

            ( ValueLookup aLookup, Variable bVariable ) ->
                BranchPartInvalidVariableSubstitution
                    { variable = bVariable, value = aLookup |> ValueLookup }
                    |> factBranchAllSingleton

            ( Variable aVariable, Variable bVariable ) ->
                if aVariable == bVariable then
                    BranchPartInvalid
                        ([ "expected variables "
                         , aVariable.name
                         , " and "
                         , bVariable.name
                         , " to be different but they're set to be equal"
                         ]
                            |> String.concat
                        )
                        |> factBranchAllSingleton

                else
                    BranchPartValidVariableSubstitution
                        ({ variable = aVariable, value = bVariable |> Variable }
                            |> variableSubstitutionToWithDeeperInAsVariable
                        )
                        |> factBranchAllSingleton

            ( ValueLookup aLookup, ValueLookup bLookup ) ->
                { branchAll =
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
                        |> List.concatMap .branchAll
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


type ExpandedResult
    = ExpandedToLookup Lookup
    | ExpansionNeedsToContinue FactBranchAll
    | ExpansionFailed String


type alias ScopedVariableName =
    { scope : List ScopeRegion
    , name : String
    }


type ScopeRegion
    = ScopeRegionPartAtIndex Int
    | ScopeRegionBranchAtIndex Int
    | ScopeRegionIntoUseRelation String


type alias FactBranchAll =
    { branchAll : List FactBranchPart }


type FactBranchPart
    = BranchPartValid String
    | BranchPartInvalid String
    | BranchPartValidVariableSubstitution
        { variable : ScopedVariableName
        , value : ValueWithVariableName ScopedVariableName
        }
    | BranchPartInvalidVariableSubstitution
        { variable : ScopedVariableName
        , value : ValueWithVariableName ScopedVariableName
        }
    | BranchPartValidRelation
        { identifier : String
        , argument : ValueWithVariableName ScopedVariableName
        , innerScope : List ScopeRegion
        }
    | BranchPartInvalidRelation
        { identifier : String
        , argument : ValueWithVariableName ScopedVariableName
        , innerScope : List ScopeRegion
        }


type alias FactExpansionAny =
    { factExpansionAny : List FactBranchAll }
