import { Elm } from "./src-elm/Main.elm";

import { invoke } from "@tauri-apps/api";

import { appWindow } from "@tauri-apps/api/window";
import { Command } from "@tauri-apps/api/shell";

import "./style.css";


const root = document.querySelector("#app");
const app = Elm.Main.init({ node: root, flags: 5 });

// app.ports?.invokeFunction.subscribe(function (f) {
//   invoke(f.name, f.args).then((res) => app.ports?.addMessage.send(res));
// });

app.ports?.runShellCommand.subscribe(async function (shellCommand) {
  let c = new Command(shellCommand.program, shellCommand.args);
  // console.log(c);
  //
  // const somethin = await appWindow.minimize();
  // console.log(somethin);
  c.execute();
});
