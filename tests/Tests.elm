module Tests exposing (suite)

import Expect
import FastDict
import Lau exposing (FactWithVariableName(..), ValueWithVariableName(..))
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Lau"
        [ Test.test "simple assignment"
            (\() ->
                { relationDefinitions =
                    FastDict.fromList
                        [ ( "main"
                          , { argumentVariable = "Result"
                            , equivalentFact =
                                Equal
                                    [ Variable "Result"
                                    , ValueLookup FastDict.empty
                                    ]
                            }
                          )
                        ]
                }
                    |> Lau.evaluate
                    |> Result.map Lau.lookupToString
                    |> Expect.equal
                        (Ok "{}")
            )
        , Test.test "simple transitive assignment"
            (\() ->
                { relationDefinitions =
                    FastDict.fromList
                        [ ( "main"
                          , { argumentVariable = "Result"
                            , equivalentFact =
                                All
                                    [ Equal
                                        [ Variable "ResultIntermediate"
                                        , ValueLookup FastDict.empty
                                        ]
                                    , Equal
                                        [ Variable "Result"
                                        , Variable "ResultIntermediate"
                                        ]
                                    ]
                            }
                          )
                        ]
                }
                    |> Lau.evaluate
                    |> Result.map Lau.lookupToString
                    |> Expect.equal
                        (Ok "{}")
            )
        , Test.test "simple transitive assignment of field value"
            (\() ->
                { relationDefinitions =
                    FastDict.fromList
                        [ ( "main"
                          , { argumentVariable = "Result"
                            , equivalentFact =
                                All
                                    [ Equal
                                        [ Variable "Value"
                                        , ValueLookup (FastDict.singleton "boolTrue" (ValueLookup FastDict.empty))
                                        ]
                                    , Equal
                                        [ Variable "Result"
                                        , ValueLookup (FastDict.singleton "maybeJust" (Variable "Value"))
                                        ]
                                    ]
                            }
                          )
                        ]
                }
                    |> Lau.evaluate
                    |> Result.map Lau.lookupToString
                    |> Expect.equal
                        (Ok "{maybeJust {boolTrue {}}}")
            )
        , Test.test "simple transitive assignment of field value with intermediate variable"
            (\() ->
                { relationDefinitions =
                    FastDict.fromList
                        [ ( "main"
                          , { argumentVariable = "Result"
                            , equivalentFact =
                                All
                                    [ Equal
                                        [ Variable "Value"
                                        , ValueLookup (FastDict.singleton "boolTrue" (ValueLookup FastDict.empty))
                                        ]
                                    , Equal
                                        [ Variable "IntermediateValue"
                                        , Variable "Value"
                                        ]
                                    , Equal
                                        [ Variable "Result"
                                        , ValueLookup (FastDict.singleton "maybeJust" (Variable "IntermediateValue"))
                                        ]
                                    ]
                            }
                          )
                        ]
                }
                    |> Lau.evaluate
                    |> Result.map Lau.lookupToString
                    |> Expect.equal
                        (Ok "{maybeJust {boolTrue {}}}")
            )
        , Test.test "grandParent"
            (\() ->
                { relationDefinitions =
                    FastDict.fromList
                        [ ( "parent"
                          , { argumentVariable = "ToRelate"
                            , equivalentFact =
                                All
                                    [ Equal
                                        [ Variable "ToRelate"
                                        , ValueLookup
                                            (FastDict.fromList
                                                [ ( "parent", Variable "Result" )
                                                , ( "child", Variable "Child" )
                                                ]
                                            )
                                        ]
                                    , Any
                                        [ All
                                            [ Equal
                                                [ Variable "Child"
                                                , ValueLookup (FastDict.singleton "anna" (ValueLookup FastDict.empty))
                                                ]
                                            , Any
                                                [ Equal
                                                    [ Variable "Parent"
                                                    , ValueLookup (FastDict.singleton "peter" (ValueLookup FastDict.empty))
                                                    ]
                                                , Equal
                                                    [ Variable "Parent"
                                                    , ValueLookup (FastDict.singleton "nuhr" (ValueLookup FastDict.empty))
                                                    ]
                                                ]
                                            ]
                                        , All
                                            [ Equal
                                                [ Variable "Child"
                                                , ValueLookup (FastDict.singleton "nuhr" (ValueLookup FastDict.empty))
                                                ]
                                            , Any
                                                [ Equal
                                                    [ Variable "Parent"
                                                    , ValueLookup (FastDict.singleton "michael" (ValueLookup FastDict.empty))
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                            }
                          )
                        , ( "grandParent"
                          , { argumentVariable = "ToRelate"
                            , equivalentFact =
                                All
                                    [ Equal
                                        [ Variable "ToRelate"
                                        , ValueLookup
                                            (FastDict.fromList
                                                [ ( "grandParent", Variable "GrandParent" )
                                                , ( "grandChild", Variable "GrandChild" )
                                                ]
                                            )
                                        ]
                                    , RelationUse
                                        { identifier = "parent"
                                        , argument =
                                            ValueLookup
                                                (FastDict.fromList
                                                    [ ( "parent", Variable "Parent" )
                                                    , ( "child", Variable "GrandChild" )
                                                    ]
                                                )
                                        }
                                    , RelationUse
                                        { identifier = "parent"
                                        , argument =
                                            ValueLookup
                                                (FastDict.fromList
                                                    [ ( "parent", Variable "GrandParent" )
                                                    , ( "child", Variable "Parent" )
                                                    ]
                                                )
                                        }
                                    ]
                            }
                          )
                        , ( "main"
                          , { argumentVariable = "Result"
                            , equivalentFact =
                                RelationUse
                                    { identifier = "grandParent"
                                    , argument =
                                        ValueLookup
                                            (FastDict.fromList
                                                [ ( "grandParent", Variable "Result" )
                                                , ( "grandChild", ValueLookup (FastDict.singleton "anna" (ValueLookup FastDict.empty)) )
                                                ]
                                            )
                                    }
                            }
                          )
                        ]
                }
                    |> Lau.evaluate
                    |> Result.map Lau.lookupToString
                    |> Expect.equal (Ok "{michael {}}")
            )
        ]
