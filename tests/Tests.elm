module Tests exposing (suite)

import Expect
import FastDict
import Lau
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
                                Lau.Equal
                                    [ Lau.Variable "Result"
                                    , Lau.ValueLookup FastDict.empty
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
                                Lau.All
                                    [ Lau.Equal
                                        [ Lau.Variable "ResultIntermediate"
                                        , Lau.ValueLookup FastDict.empty
                                        ]
                                    , Lau.Equal
                                        [ Lau.Variable "Result"
                                        , Lau.Variable "ResultIntermediate"
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
                                Lau.All
                                    [ Lau.Equal
                                        [ Lau.Variable "Value"
                                        , Lau.ValueLookup (FastDict.singleton "boolTrue" (Lau.ValueLookup FastDict.empty))
                                        ]
                                    , Lau.Equal
                                        [ Lau.Variable "Result"
                                        , Lau.ValueLookup (FastDict.singleton "maybeJust" (Lau.Variable "Value"))
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
                                Lau.All
                                    [ Lau.Equal
                                        [ Lau.Variable "Value"
                                        , Lau.ValueLookup (FastDict.singleton "boolTrue" (Lau.ValueLookup FastDict.empty))
                                        ]
                                    , Lau.Equal
                                        [ Lau.Variable "IntermediateValue"
                                        , Lau.Variable "Value"
                                        ]
                                    , Lau.Equal
                                        [ Lau.Variable "Result"
                                        , Lau.ValueLookup (FastDict.singleton "maybeJust" (Lau.Variable "IntermediateValue"))
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
                                Lau.All
                                    [ Lau.Equal
                                        [ Lau.Variable "ToRelate"
                                        , Lau.ValueLookup
                                            (FastDict.fromList
                                                [ ( "parent", Lau.Variable "Result" )
                                                , ( "child", Lau.Variable "Child" )
                                                ]
                                            )
                                        ]
                                    , Lau.Any
                                        [ Lau.All
                                            [ Lau.Equal
                                                [ Lau.Variable "Child"
                                                , Lau.ValueLookup (FastDict.singleton "anna" (Lau.ValueLookup FastDict.empty))
                                                ]
                                            , Lau.Any
                                                [ Lau.Equal
                                                    [ Lau.Variable "Parent"
                                                    , Lau.ValueLookup (FastDict.singleton "peter" (Lau.ValueLookup FastDict.empty))
                                                    ]
                                                , Lau.Equal
                                                    [ Lau.Variable "Parent"
                                                    , Lau.ValueLookup (FastDict.singleton "nuhr" (Lau.ValueLookup FastDict.empty))
                                                    ]
                                                ]
                                            ]
                                        , Lau.All
                                            [ Lau.Equal
                                                [ Lau.Variable "Child"
                                                , Lau.ValueLookup (FastDict.singleton "nuhr" (Lau.ValueLookup FastDict.empty))
                                                ]
                                            , Lau.Any
                                                [ Lau.Equal
                                                    [ Lau.Variable "Parent"
                                                    , Lau.ValueLookup (FastDict.singleton "michael" (Lau.ValueLookup FastDict.empty))
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
                                Lau.All
                                    [ Lau.Equal
                                        [ Lau.Variable "ToRelate"
                                        , Lau.ValueLookup
                                            (FastDict.fromList
                                                [ ( "grandParent", Lau.Variable "GrandParent" )
                                                , ( "grandChild", Lau.Variable "GrandChild" )
                                                ]
                                            )
                                        ]
                                    , Lau.RelationUse
                                        { identifier = "parent"
                                        , argument =
                                            Lau.ValueLookup
                                                (FastDict.fromList
                                                    [ ( "parent", Lau.Variable "Parent" )
                                                    , ( "child", Lau.Variable "GrandChild" )
                                                    ]
                                                )
                                        }
                                    , Lau.RelationUse
                                        { identifier = "parent"
                                        , argument =
                                            Lau.ValueLookup
                                                (FastDict.fromList
                                                    [ ( "parent", Lau.Variable "GrandParent" )
                                                    , ( "child", Lau.Variable "Parent" )
                                                    ]
                                                )
                                        }
                                    ]
                            }
                          )
                        , ( "main"
                          , { argumentVariable = "Result"
                            , equivalentFact =
                                Lau.RelationUse
                                    { identifier = "grandParent"
                                    , argument =
                                        Lau.ValueLookup
                                            (FastDict.fromList
                                                [ ( "grandParent", Lau.Variable "Result" )
                                                , ( "grandChild", Lau.ValueLookup (FastDict.singleton "anna" (Lau.ValueLookup FastDict.empty)) )
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
