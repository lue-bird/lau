import { defineConfig } from "vite"
import elm from "vite-plugin-elm"
import fs from "node:fs"
import eol2 from "elm-optimize-level-2"
import { temporaryFile } from "tempy"

export default defineConfig({
    plugins: [
        elm({ compiler: { compile: compileWithEOL2 } })
    ]
})

const compileWithEOL2 = async (targets) => {
    const output = temporaryFile({ extension: 'elm' });
    await eol2.run({
        inputFilePath: targets,
        outputFilePath: output,
        optimizeSpeed: true,
        processOpts: { stdio: ['inherit', 'ignore', 'inherit'] },
    })
    return fs.readFileSync(output).toString()
}
