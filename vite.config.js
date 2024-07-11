import { defineConfig } from "vite"
import elm from "vite-plugin-elm"
import eol2 from "elm-optimize-level-2"
import { devNull } from "node:os"
import * as esbuild from "esbuild"

// thanks https://discourse.elm-lang.org/t/what-i-ve-learned-about-minifying-elm-code/7632

export default defineConfig({
    plugins: [
        elm({
            compiler: {
                compile: compile
            }
        })
    ]
})

const compile = (targets) => {
    return compileWithEOL2(targets)
        .then(eol2Output =>
            esbuild.transform(
                removeIife(eol2Output),
                {
                    minify: true,
                    pure: pureFuncs,
                    target: "es5",
                    format: "iife"
                }
            ).code
        )
}

const pureFuncs = ["F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"]

const removeIife = (elmJsCode) => {
    return `var scope = window;${elmJsCode.slice(
        elmJsCode.indexOf("{") + 1,
        elmJsCode.lastIndexOf("}")
    )}`
}

const compileWithEOL2 = (targets) => {
    return eol2.run({
        inputFilePath: targets,
        outputFilePath: devNull,
        optimizeSpeed: true,
        processOpts: { stdio: ["inherit", "ignore", "inherit"] },
    })
}
