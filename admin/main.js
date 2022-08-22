import { Elm } from "./src-elm/Main.elm";

import { invoke } from "@tauri-apps/api";
import { appWindow } from "@tauri-apps/api/window";
import { Command } from "@tauri-apps/api/shell";

const root = document.querySelector("#app");
const app = Elm.Main.init({ node: root, flags: 5 });

app.ports.saveStafferField.subscribe(function(message) {
  invoke('saveStafferField', {data: message});
});

app.ports.getStaffers.subscribe(function(message) {
  invoke('getStaffers').then((res) => app.ports.receiveStaffers.send(res));
});
