Programming language goals:

  - fully declarative: do not specify steps necessary to get to a goal, just the goal
  - as simple as possible, avoiding sugar as much as possible

Turns out that's exactly what logic languages enable.

Textual-form language core
  - `fact`: Something to be satisfied. Add it without indentation to have it be satisfied throughout the whole project. Some call it rule/constraint/assertion/proposition
  - `variable`: a placeholder that can be filled in in any way to satisfy the facts it's present in
  - `_anyVariable`: a variable that's used once. Some call it boat/meh. While technically optional, it's highly recommended to be explicit about these!
  - `{FieldIdentifierA fieldValueA FieldIdentifierB fieldValueB}` multiple in any order form a record/dictionary
  - `% single line comment`: Documentation, disregarded at execution
  - `(&fact0 &fact1)` sugar for `{And0 fact0 And1 fact1}` and means both `fact0` and `fact1` need to be satisfied at the same time
  - `(|fact0 |fact1)` sugar for `{Or0 fact0 Or1 fact1}` means either `fact0` or `fact1` or both need to be satisfied at the same time, independent of order.
  - `fact0 = fact1` sugar for `{Equal0 fact0 Equal1 fact1}`
  - `"string"` sugar for a `List` of elements that are themselves character (wrapped codes)

```prolog
{NaturalZero {NaturalZero {}}}

{Natural1Up {PredecessorSuccessor {NaturalSuccessor predecessor} Predecessor predecessor}} =
  Natural predecessor

{Natural natural} =
  |{NaturalZero natural}
  |{Natural1Up {PredecessorSuccessor natural Predecessor _predecessor}}

{ListEmpty {ListEmpty {}}}

{ListHeadTail {ListFilled {ListHeadTail {Head head Tail tail}} Head head Tail tail}} =
  |List tail

{List list} =
  |{ListEmpty list}
  |{ListHeadTail {ListFilled list Head _head Tail _tail}}

{ListSingle {List list, OnlyElement onlyElement}}
  |(&{ListHeadTail {ListFilled list Head onlyElement Tail tail}} &{ListEmpty tail})

{ListElementCount {List list ElementCount elementCount}}
  |(&{ListEmpty list} &{NaturalZero elementCount})
  |(&{ListOneAndRest {ListFilled list One _one Rest rest}}
    &{ListElementCount {List rest, ElementCount restLength}}
    &NaturalSuccessor {PredecessorSuccessor elementCount Predecessor restLength}
  )

% Below syntax is not updated, yet

ListMap (list, elementMap, listMapped)
  % or ListZip (listWithMapped, list, listMapped) &ListAll (listWithMapped, elementMap)
  |(&ListEmpty list &ListEmpty listMapped)
  |(&ListHeadTail (list, head, tail) &ListHeadTail (listMapped, mappedHead, mappedTail) &elementMap (head, mappedHead) &elementMap (tail, mappedTail))

ListSplit (list, left, right)
  |ListEmpty left &Equal (list, right)
  |ListHeadTail (left, head, leftTail) &ListHeadTail (list, head, tail) &ListSplit (tail, leftTail, right)

ListOneAndLeftRight (list, specificElement, left, right)
  |ListHeadTail (list, specificElement, right) &ListEmpty left
  |ListHeadTail (list, head, tail) &ListOneAndRest (tail, specificElement, tailRest) &ListHeadTail(rest, head, tailRest)

ListOneAndRest (list, one, rest)
  |ListOneAndLeftRight (list, one, left, right) &ListSplit (leftRight, left, right) &ListPermutation(rest, leftRight)

ListContains (list, specificElement)
  |ListOneAndRest(list, specificElement, _rest)

ListAll (list, elementRule)
  |ListEmpty list
  |ListOneAndRest (list, one, rest) &elementRule one &ListAll (rest, elementRule)

ListAny (list, found)
  |ListContains (list, foundElement) &found foundElement

ListLast (list, last)
  |ListLastAndBefore (list, last, _before)

ListLastAndBefore (list, last)
  |ListSplit (list, left, right) &ListSingle (right, last)

ListReverse (list, reversed)
  |ListEmpty list &ListEmpty reversed
  |ListHeadTail (list, edgeElement, listAfterEdge) &ListLastAndBefore (reversed, edgeElement, listAfterEdge) &ListReverse (listAfterEdge, reversedBeforeEdge)

ListSorted (list, lessOrEqual)
  |ListEmpty list
  |ListHeadTail (list, el0, el1Up)
    |ListEmpty el1Up
    |ListHeadTail (el1Up, el1, el2Up) &lessOrEqual (el0, el1) &ListSorted (el1Up, lessOrEqual)

ListSort (unsorted, sorted, lessOrEqual)
  |ListSorted (sorted, lessOrEqual) &ListPermutation (unsorted, sorted)

ListPermutation (aList, bList)
  |ListEmpty aList &ListEmpty bList
  |ListOneAndRest (aList, sharedElement, aWithoutSharedElement) &ListOneAndRest (bList, sharedElement, bWithoutSharedElement) &ListPermutation (aWithoutSharedElement, bWithoutSharedElement)

ListContainsOnce (list, specificElement)
  |ListOneAndRest (list, specificElement, listWithoutSpecificElement) &Not ListContains (listWithoutSpecificElement, specificElement)

ListElementsTake (list, takeRule, taken)
  |ListEmpty list &ListEmpty taken
  |(&ListOneAndRest (list, one, rest)
    &(
      |takeRule one &Equals (one, takenOne) &ListOneAndRest (taken, takenOne, takenRest) &ListElementsTake (rest, takeRule, takenRest)
      |Not takeRule one &Not ListContains (taken, one) &ListElementsTake (rest, takeRule, taken)
    )
  )
```
