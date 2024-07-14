module List.LocalExtra exposing (allJustMap, allOkMap, elementAtIndexAlter, firstJustMap, insertElementAtIndex, interweave, oneOfEach, removeElementAtIndex, setFlatMap)

import Set exposing (Set)


setFlatMap : (a -> Set comparableSetElement) -> (List a -> Set comparableSetElement)
setFlatMap elementToSet =
    \list ->
        list
            |> List.foldl
                (\element soFar ->
                    Set.union (element |> elementToSet) soFar
                )
                Set.empty


interweave : List a -> List a -> List a
interweave aList bList =
    case aList of
        [] ->
            bList

        aHead :: aTail ->
            case bList of
                [] ->
                    aHead :: aTail

                bHead :: bTail ->
                    aHead :: bHead :: interweave aTail bTail


elementAtIndexAlter : Int -> (a -> a) -> (List a -> List a)
elementAtIndexAlter indexToAlter elementAlter =
    \list ->
        list
            |> List.indexedMap
                (\futurePartIndex futurePart ->
                    if futurePartIndex == indexToAlter then
                        futurePart |> elementAlter

                    else
                        futurePart
                )


removeElementAtIndex : Int -> (List a -> List a)
removeElementAtIndex indexToRemove =
    \list ->
        if indexToRemove <= -1 then
            list

        else
            case list of
                [] ->
                    []

                head :: tail ->
                    case indexToRemove of
                        0 ->
                            tail

                        indexToRemoveAtLeast1 ->
                            head :: (tail |> removeElementAtIndex (indexToRemoveAtLeast1 - 1))


insertElementAtIndex : Int -> a -> (List a -> List a)
insertElementAtIndex indexToInsertAt elementToInsert =
    \list ->
        if indexToInsertAt <= -1 then
            list

        else
            case indexToInsertAt of
                0 ->
                    elementToInsert :: list

                indexToInsertAtAtLeast1 ->
                    case list of
                        [] ->
                            []

                        head :: tail ->
                            head :: (tail |> insertElementAtIndex (indexToInsertAtAtLeast1 - 1) elementToInsert)


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
firstJustMap elementToMaybeFound list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case elementToMaybeFound head of
                Nothing ->
                    firstJustMap elementToMaybeFound tail

                Just found ->
                    Just found


allOkMap :
    (element -> Result error success)
    -> (List element -> Result error (List success))
allOkMap elementToResult list =
    case list of
        [] ->
            Ok []

        head :: tail ->
            case elementToResult head of
                Err error ->
                    Err error

                Ok headSuccess ->
                    Result.map (\tailOks -> headSuccess :: tailOks)
                        (allOkMap elementToResult tail)


oneOfEach : List (List a) -> List (List a)
oneOfEach list =
    case list of
        [] ->
            []

        [ onlyList ] ->
            onlyList |> List.map List.singleton

        list0 :: list1 :: list2Up ->
            let
                list1UpOneOfEach : List (List a)
                list1UpOneOfEach =
                    (list1 :: list2Up) |> oneOfEach
            in
            list0
                |> List.concatMap
                    (\oneOfHead ->
                        list1UpOneOfEach |> List.map (\t -> t |> (::) oneOfHead)
                    )
