### most positional things shouldn't be positional

#### arguments

`3 + 4` or `(+ 3 4)` or `add(3, 4)` are common syntax
when in reality, the position of arguments doesn't matter: `4 + 3` = `3 + 4`

Summing a multiset/bag comes closer to what an add function is doing

```
    ↓ ← 3
+ ← ⁖ ← 4
```

This also allows us to manipulate the inputs easier

```elm
(peter |> age) + (anna |> age) + (leo |> age)
```

becomes

```
            peter
              ↓
+ ← map ← ⁖ ← ⁖ ← leo
      age ↑   ↑
             anna
```

It's the same for
`*`, `min` & `max`, `==`, `compare`, `union` & `intersection` for `Set` & `Dict`, ...
where position doesn't matter

- `_ - _` can be replaced by adding a negated number
- `_ / _` can be replaced by multiplying an inverse (`^ -1`)

exceptions where positionality matters are: `^`, `remainderBy`, `logBase`

#### record, choice

The tag is the only actual identifying information, no positionality needed
(no `User | Admin` or `Admin, User`)

Btw, a record is a set and a choice takes a set

#### list

Basically any user-facing list should be a kind of set like a multiset/bag or builder.
Only some are true stacks. Call it a stack, then and use it like one!

#### array

Arrays are usually
- misused as stacks
- should be a kind of set like a multiset/bag

all others are int-tagged sets in lau
