import { defineConfig } from "vite"
import elm from "vite-plugin-elm"
import eol2 from "elm-optimize-level-2"
import { devNull } from "node:os"

export default defineConfig({
    plugins: [
        elm({ compiler: { compile: compileWithEOL2 } })
    ]
})

const compileWithEOL2 = async (targets) => {
    return await eol2.run({
        inputFilePath: targets,
        outputFilePath: devNull,
        optimizeSpeed: true,
        processOpts: { stdio: ["inherit", "ignore", "inherit"] },
    })
}
