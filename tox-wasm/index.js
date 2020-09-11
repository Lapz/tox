// For more comments about what's going on here, check out the `hello_world`
import * as monaco from "monaco-editor";
import { Terminal } from "xterm";
const rust = import("./pkg");

const term = new Terminal();

self.MonacoEnvironment = {
  getWorkerUrl: function (moduleId, label) {
    if (label === "json") {
      return "./json.worker.bundle.js";
    }
    if (label === "css") {
      return "./css.worker.bundle.js";
    }
    if (label === "html") {
      return "./html.worker.bundle.js";
    }
    if (label === "typescript" || label === "javascript") {
      return "./ts.worker.bundle.js";
    }
    return "./editor.worker.bundle.js";
  },
};

const editor = monaco.editor.create(document.getElementById("container"), {
  value: ['fn main() { let x:int ="a"; }'].join("\n"),
  language: "rust",
});

const initXterm = () => {
  const div = document.createElement("div");
  div.id = "terminal";

  const body = document.getElementById("root");

  body.appendChild(div);
  term.open(document.getElementById("terminal"));
};

initXterm();

const runButton = document.getElementById("run");

runButton.addEventListener("click", () => {
  rust
    .then((m) => {
      try {
        m.compile(editor.getValue());
      } catch (e) {
        console.error(e);
        term.clear();
        term.write(e);
      }
    })
    .catch(console.error);
});
