Declarative, visual, minimal programming language.
Reduces things you need to know and keep in your mind.

[→ open studio](https://lue-bird.github.io/lau/)

  - declarative:
    instead of
    "1. put the bowl on the table, 2. put in milk, 3. put in cereal"
    you say "the bowl is on the table and has milk and cereal in it"
  - visual:
    put differently colored and shaped blocks together
  - minimal:
    value, variable, definition, all, any, =, not. That's everything.
    No spooky things to keep in mind like mutation, pointer, unmanaged effect, runtime error,
    no typeclass/trait/interface, no lambda, no local definition, no difference between types and code,
    no module, no class, no package, no namespace

## status (TODO)

  - language evaluation isn't working just yet
  - studio
      - block sidebar
          - fact blocks, empty value lookup
          - variable creating
          - relation definition creating
          - existing variables, existing relation definitions
      - block context action
          - fact/variable/value lookup deleting
          - value lookup entry deleting
          - relation definition deleting
          - value lookup entry adding
          - variable renaming
          - relation renaming
      - evaluation frame

## web
run locally with

```shell
npm install
npx vite
```
and open http://localhost:5173/ to see your app

For all future runs, you just need
```shell
npx vite
```


## links

  - [vite](https://vitejs.dev/)
  - [ryan-haskell/vite-plugin-elm-watch](https://github.com/ryan-haskell/vite-plugin-elm-watch)
  - implementation uses [`elm-state-interface`](https://dark.elm.dmy.fr/packages/lue-bird/elm-state-interface/latest/)
  - loved this [talk "Beautiful code: typography and visual programming" by Peter Hilton
  ](https://www.youtube.com/watch?v=rTeqsL4uxws)
  - [scratch](https://scratch.mit.edu/), a procedural, OO-ish, visual scripting language


## considering (so might never be part of the language)

  - you don't need to worry about caching and many other optimizations due to laziness
  - uis written in lau where interacting with the outside world is simple and safe which you can even embed in lau code itself
  - a story for exhaustive case handling (current workaround with giving value lookup might be a bit rough?)
  - value lookup entry moving
