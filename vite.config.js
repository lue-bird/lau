import { defineConfig } from "vite"
import elm from "vite-plugin-elm-watch"
import * as elmOptimizeLevel2 from "elm-optimize-level-2"
import * as path from "node:path"

export default defineConfig((environment) => ({
    plugins: [
        elm({
            mode:
                environment.command === "build" ?
                    "optimize"
                    : "debug"

        }),
        environment.command === "build" ?
            applyEOL2()
            : []
    ],
    base: "/lau/",
    resolve: {
        alias: {
            $fonts: path.resolve('./static/fonts')
        }
    }
}))

const jsFileRegex = /\.(js)$/
function applyEOL2() {
    return {
        name: 'apply-elm-optimize-level-2',

        async transform(src, id) {
            if (jsFileRegex.test(id)) {
                return {
                    code: await elmOptimizeLevel2.transform(src, true),
                    map: null
                }
            }
        }
    }
}
