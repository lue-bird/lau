import * as Web from "@lue-bird/elm-state-interface"
import { Elm } from "./Main.elm"

const elmApp = Elm.Main.init()
Web.programStart({ ports: elmApp.ports, domElement: document.getElementById("app") })
