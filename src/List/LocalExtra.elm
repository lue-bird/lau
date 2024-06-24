module List.LocalExtra exposing (allJustMap, firstJustMap, oneOfEach)


allJustMap : (a -> Maybe b) -> (List a -> Maybe (List b))
allJustMap elementToMaybe =
    \list ->
        case list of
            [] ->
                [] |> Just

            head :: tail ->
                case head |> elementToMaybe of
                    Nothing ->
                        Nothing

                    Just headJust ->
                        Maybe.map ((::) headJust) (tail |> allJustMap elementToMaybe)


firstJustMap : (a -> Maybe b) -> List a -> Maybe b
firstJustMap elementToMaybeFound =
    \list ->
        case list of
            [] ->
                Nothing

            head :: tail ->
                case elementToMaybeFound head of
                    Nothing ->
                        firstJustMap elementToMaybeFound tail

                    Just found ->
                        Just found


oneOfEach : List (List a) -> List (List a)
oneOfEach list =
    case list of
        [] ->
            []

        [ onlyElement ] ->
            [ onlyElement ]

        list0 :: list1 :: list2Up ->
            let
                list1UpOneOfEach : List (List a)
                list1UpOneOfEach =
                    (list1 :: list2Up) |> oneOfEach
            in
            list0
                |> List.concatMap
                    (\oneOfHead ->
                        list1UpOneOfEach |> List.map ((::) oneOfHead)
                    )
