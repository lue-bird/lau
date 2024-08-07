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
    typeclass/trait/interface, module, class, namespace, package, lambda, local definition, difference between types and code

## status (TODO)

  - scrolling
  - fix issue where double click removes outer
  - block sidebar
      - search (relation, variable, entry key)
      - variable renaming
      - value lookup entry key renaming
      - relation renaming
      - navigating to relation definition
  - evaluation frame
  - relation definition deleting

![screenshot of the in-progress ui](https://github.com/lue-bird/lau/assets/81869893/0cfc0399-9acb-4c1c-a2e7-e0dd7a7ca979)

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

  - uis written in lau where interacting with the outside world is simple and safe which you can even embed in lau code itself
  - relations adding its defined parameter (one level deep, so without variables) as an argument
  - relation pinning
  - you don't need to worry about caching and many other optimizations due to laziness
  - identification not by name internally (so that switching languages is easier, renaming is not a breaking change and names can overlap)
  - a story for exhaustive case handling (current workaround with giving value lookup might be a bit rough?)
  - convenient full value lookup dragging
  - other shapes of values (multi-set, so untagged entries) to allow for nicer syntax when argument order doesn't matter e.g. with sum, product, min, max, equals, union, intersection etc.

