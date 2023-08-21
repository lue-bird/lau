**So far, lau is a vision but not functional**

## lau
A brain load reducing visual programming language.

some highlights
- a fully graphical studio with embedded uis that you can write in lau itself
- you don't talk to lau in types, but it still know as much as you about your code
- definitions are unique around the world forever
- laziness all the way, you won't have to worry about caching and many other optimizations
- semantic versioning, existing code won't suddenly stop running after dependency upgrade

[→ try it!](https://lue-bird.github.io/lau/)

## why lau might not be for you

- non-existent community, no ecosystem and only one platform support at the moment: interactive svg in the web
- lau will experiment for quite a while. Expect things to break
- text tools don't really work, for example git, find, replace etc.

## why you might like lau

- no spooky things you have to keep in mind
    - no mutability
    - no pointers
    - no runtime errors
    - no effects – everything is values and transformations
    - everything is lazy
        - `List.map age |> List.take 3` = `List.take 3 |> List.map age` for example. No need to optimize
        - no need to care about duplicate calls with the same arguments. They're cashed
- just wire outputs of one thing as the input to something else
    - no brackets, no semicolons, no tabs/spaces, no separators or leading symbols
    - no positional arguments, no prefix vs infix vs postfix
    - no piping operators
    - no steep learning curve
    - no syntax errors, **only type errors**
- no modules, no classes, no packages, no module qualification, no lambdas...
    - just 1 simple way of scoping: defines
- when you refer to something, this reference will be unique around the 🌏 forever
    - no identification by name
        - defined things can be represented like you want: `:)` or maybe `🌲`?
            - no restrictions like lowercase, uppercase, no space, no `-`, ...
        - no name clashes
        - no shadowing
        - renaming or internal changes aren't breaking changes
        - quicker to read: no `Array.repeat ... |> Array.map ... |> Array.filter ...`,
          just `array repeat ... → map ... → filter ...`
    - no identification by content
        - implementation patches don't require reference updates
        - even if the content is the same, different functions might have different semantics
- embed what you/others have written in lau
    - visualization → `show`
        - for example color, number, character
    - generator → `generate`
        - for example color, number, character, import

[Read more about why lau is designed the way it is](design/)
