### identification

Standard way of identifying a thing
- in most programming languages: dependency ((+ module name) + thing name)
- in most file systems: directory (+ file)
    - in programs that rely on paths, (e.g. git (versioning), shotcut (video editing), LMMS (music making), ...)

→ a lot of drawbacks
- renaming API → breaking change
- thing/module/dependency name clashes
- many languages allow shadowing
- renaming a file → many tools don't recognize it as the same (e.g. git, ...)
- verbose
    - `List/Array/Set/Dict/... .map` instead of `map`.
      You and lau know it's an `Array` since its creation/transformation
    - ```elm
      Element.Font.color
          (Color.fromRgb
              { red = 1, green = 1, blue = 1 }
          )
      ```
      instead of
      ```
                red ← 1
                ↓
      font 🖌 ← ⁖ ← green ← 1
                ↑
                blue ← 1
      ```
      (you'd usually go for a color editor here)
