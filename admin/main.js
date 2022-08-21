import { Elm } from "./src-elm/Main.elm";

import { invoke } from "@tauri-apps/api";
import { appWindow } from "@tauri-apps/api/window";
import { Command } from "@tauri-apps/api/shell";

// import "./style.css";


const root = document.querySelector("#app");
const app = Elm.Main.init({ node: root, flags: 5 });

app.ports.greet.subscribe(function(message) {
  invoke('greet', {name: message}).then((res) => app.ports.messageReceiver.send(res));
});
