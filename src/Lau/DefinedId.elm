module Lau.DefinedId exposing
    ( DefinedId, Tag
    , id, random
    )

{-| Unique identifier of a defined thing.

@docs DefinedId, Tag


## create

@docs id, random

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit)
import Linear exposing (Direction(..))
import N exposing (Exactly)
import N.Local exposing (N160, n160)
import Random
import Typed exposing (Public, Tagged, Typed)


{-| One of `2^160` different possible unique keys.

Is it really unique?

Even if 10 billion users
need 1 million new ids each,

the probability of duplicate ids is

    10,000,000,000,000,000 / (2^128) =~ 10^-32

.

    import AssocList exposing (Dict)

    type alias Model =
        { usersById : Dict (Id User) User }

    type Msg
        = RegisterUser (Id User)

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            RegisterUser user id ->
                ( { model
                    | usersById =
                        Dict.insert user id model.usersById
                  }
                , Cmd.none
                )

Credit to [Punie/elm-id](https://package.elm-lang.org/packages/Punie/elm-id/latest/)
for the idea of using phantom types to describe the use-case!

-}
type alias DefinedId =
    Typed Tagged Tag Public (ArraySized Bit (Exactly N160))


{-| Tags a valid [`DefinedId`](#DefinedId)
-}
type Tag
    = Tag


{-| A `Generator` for a random new [`DefinedId`](#DefinedId).

You can use the [tool generateIds](browser/generate-ids/generateIds.html).

-}
random : Random.Generator DefinedId
random =
    ArraySized.random Bit.random n160
        |> Random.map (\bits -> bits |> ArraySized.inToNumber |> Typed.tag Tag)


{-| Create a [`DefinedId`](#DefinedId) from raw bits.

You can use the [tool generateIds](browser/generate-ids/generateIds.html).

-}
id : Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> DefinedId
id b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b32 b33 b34 b35 b36 b37 b38 b39 b40 b41 b42 b43 b44 b45 b46 b47 b48 b49 b50 b51 b52 b53 b54 b55 b56 b57 b58 b59 b60 b61 b62 b63 b64 b65 b66 b67 b68 b69 b70 b71 b72 b73 b74 b75 b76 b77 b78 b79 b80 b81 b82 b83 b84 b85 b86 b87 b88 b89 b90 b91 b92 b93 b94 b95 b96 b97 b98 b99 b100 b101 b102 b103 b104 b105 b106 b107 b108 b109 b110 b111 b112 b113 b114 b115 b116 b117 b118 b119 b120 b121 b122 b123 b124 b125 b126 b127 b128 b129 b130 b131 b132 b133 b134 b135 b136 b137 b138 b139 b140 b141 b142 b143 b144 b145 b146 b147 b148 b149 b150 b151 b152 b153 b154 b155 b156 b157 b158 b159 =
    ArraySized.empty
        |> ArraySized.attach Up (ArraySized.l16 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
        |> ArraySized.attach Up (ArraySized.l16 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31)
        |> ArraySized.attach Up (ArraySized.l16 b32 b33 b34 b35 b36 b37 b38 b39 b40 b41 b42 b43 b44 b45 b46 b47)
        |> ArraySized.attach Up (ArraySized.l16 b48 b49 b50 b51 b52 b53 b54 b55 b56 b57 b58 b59 b60 b61 b62 b63)
        |> ArraySized.attach Up (ArraySized.l16 b64 b65 b66 b67 b68 b69 b70 b71 b72 b73 b74 b75 b76 b77 b78 b79)
        |> ArraySized.attach Up (ArraySized.l16 b80 b81 b82 b83 b84 b85 b86 b87 b88 b89 b90 b91 b92 b93 b94 b95)
        |> ArraySized.attach Up (ArraySized.l16 b96 b97 b98 b99 b100 b101 b102 b103 b104 b105 b106 b107 b108 b109 b110 b111)
        |> ArraySized.attach Up (ArraySized.l16 b112 b113 b114 b115 b116 b117 b118 b119 b120 b121 b122 b123 b124 b125 b126 b127)
        |> ArraySized.attach Up (ArraySized.l16 b128 b129 b130 b131 b132 b133 b134 b135 b136 b137 b138 b139 b140 b141 b142 b143)
        |> ArraySized.attach Up (ArraySized.l16 b144 b145 b146 b147 b148 b149 b150 b151 b152 b153 b154 b155 b156 b157 b158 b159)
        |> ArraySized.inToNumber
        |> Typed.tag Tag
