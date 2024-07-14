module Tests exposing (suite)

import Expect
import FastDict
import Lau
import List.LauExtra
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Lau"
        [ Test.test "List oneOfEach"
            (\() ->
                [ [ 0, 1, 2 ]
                , [ 10, 11, 12 ]
                , [ 20, 21, 22 ]
                ]
                    |> List.LauExtra.oneOfEach
                    |> Expect.equalLists
                        [ [ 0, 10, 20 ]
                        , [ 0, 10, 21 ]
                        , [ 0, 10, 22 ]
                        , [ 0, 11, 20 ]
                        , [ 0, 11, 21 ]
                        , [ 0, 11, 22 ]
                        , [ 0, 12, 20 ]
                        , [ 0, 12, 21 ]
                        , [ 0, 12, 22 ]
                        , [ 1, 10, 20 ]
                        , [ 1, 10, 21 ]
                        , [ 1, 10, 22 ]
                        , [ 1, 11, 20 ]
                        , [ 1, 11, 21 ]
                        , [ 1, 11, 22 ]
                        , [ 1, 12, 20 ]
                        , [ 1, 12, 21 ]
                        , [ 1, 12, 22 ]
                        , [ 2, 10, 20 ]
                        , [ 2, 10, 21 ]
                        , [ 2, 10, 22 ]
                        , [ 2, 11, 20 ]
                        , [ 2, 11, 21 ]
                        , [ 2, 11, 22 ]
                        , [ 2, 12, 20 ]
                        , [ 2, 12, 21 ]
                        , [ 2, 12, 22 ]
                        ]
            )
        , Test.test "simple assignment"
            (\() ->
                { relationDefinitions =
                    FastDict.fromList
                        [ ( "main"
                          , { argumentVariable = "Result"
                            , equivalentFact =
                                Lau.Equal
                                    { a = Lau.Variable "Result"
                                    , b = Lau.ValueLookup FastDict.empty
                                    }
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
                                        { a = Lau.Variable "ResultIntermediate"
                                        , b = Lau.ValueLookup FastDict.empty
                                        }
                                    , Lau.Equal
                                        { a = Lau.Variable "Result"
                                        , b = Lau.Variable "ResultIntermediate"
                                        }
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
                                        { a = Lau.Variable "Value"
                                        , b = Lau.ValueLookup (FastDict.singleton "boolTrue" (Lau.ValueLookup FastDict.empty))
                                        }
                                    , Lau.Equal
                                        { a = Lau.Variable "Result"
                                        , b = Lau.ValueLookup (FastDict.singleton "maybeJust" (Lau.Variable "Value"))
                                        }
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
                                        { a = Lau.Variable "Value"
                                        , b = Lau.ValueLookup (FastDict.singleton "boolTrue" (Lau.ValueLookup FastDict.empty))
                                        }
                                    , Lau.Equal
                                        { a = Lau.Variable "IntermediateValue"
                                        , b = Lau.Variable "Value"
                                        }
                                    , Lau.Equal
                                        { a = Lau.Variable "Result"
                                        , b = Lau.ValueLookup (FastDict.singleton "maybeJust" (Lau.Variable "IntermediateValue"))
                                        }
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
                                        { a = Lau.Variable "ToRelate"
                                        , b =
                                            Lau.ValueLookup
                                                (FastDict.fromList
                                                    [ ( "parent", Lau.Variable "Parent" )
                                                    , ( "child", Lau.Variable "Child" )
                                                    ]
                                                )
                                        }
                                    , Lau.Any
                                        [ Lau.All
                                            [ Lau.Equal
                                                { a = Lau.Variable "Child"
                                                , b = Lau.ValueLookup (FastDict.singleton "anna" (Lau.ValueLookup FastDict.empty))
                                                }
                                            , Lau.Any
                                                [ Lau.Equal
                                                    { a = Lau.Variable "Parent"
                                                    , b = Lau.ValueLookup (FastDict.singleton "peter" (Lau.ValueLookup FastDict.empty))
                                                    }
                                                , Lau.Equal
                                                    { a = Lau.Variable "Parent"
                                                    , b = Lau.ValueLookup (FastDict.singleton "nuhr" (Lau.ValueLookup FastDict.empty))
                                                    }
                                                ]
                                            ]
                                        , Lau.All
                                            [ Lau.Equal
                                                { a = Lau.Variable "Child"
                                                , b = Lau.ValueLookup (FastDict.singleton "nuhr" (Lau.ValueLookup FastDict.empty))
                                                }
                                            , Lau.Any
                                                [ Lau.Equal
                                                    { a = Lau.Variable "Parent"
                                                    , b = Lau.ValueLookup (FastDict.singleton "michael" (Lau.ValueLookup FastDict.empty))
                                                    }
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
                                        { a = Lau.Variable "ToRelate"
                                        , b =
                                            Lau.ValueLookup
                                                (FastDict.fromList
                                                    [ ( "grandParent", Lau.Variable "GrandParent" )
                                                    , ( "grandChild", Lau.Variable "GrandChild" )
                                                    ]
                                                )
                                        }
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
