import { WASI, File, Directory, PreopenDirectory, OpenFile, ConsoleStdout } from "https://esm.sh/@bjorn3/browser_wasi_shim@0.4.2";

const libFiles = [
  "giriş.kip",
  "temel.kip",
  "temel-doğruluk.kip",
  "temel-dizge.kip",
  "temel-etki.kip",
  "temel-liste.kip",
  "temel-tam-sayı.kip",
];

const sourceEl = document.getElementById("source");
const stdinEl = document.getElementById("stdin");
const outputEl = document.getElementById("output");
const transpiledEl = document.getElementById("transpiled");
const runBtn = document.getElementById("run");
const transpileBtn = document.getElementById("transpile");
const langEl = document.getElementById("lang");
const exampleEl = document.getElementById("example");

const examples = [
  { id: "argüman-tipleri", file: "argüman-tipleri.kip" },
  { id: "asal-sayılar", file: "asal-sayılar.kip", stdin: "asal-sayılar.in" },
  { id: "aynı-hal-ardışık-fark", file: "aynı-hal-ardışık-fark.kip" },
  { id: "bir-fazlası", file: "bir-fazlası.kip", stdin: "bir-fazlası.in" },
  { id: "deneme-yazma", file: "deneme-yazma.kip" },
  { id: "değilse-örüntü", file: "değilse-örüntü.kip" },
  { id: "dizge-escape", file: "dizge-escape.kip" },
  { id: "dizge-işlemleri", file: "dizge-işlemleri.kip" },
  { id: "dosya-io", file: "dosya-io.kip" },
  { id: "doğruluk-tersi", file: "doğruluk-tersi.kip" },
  { id: "ek-öbek", file: "ek-öbek.kip" },
  { id: "fibonacci", file: "fibonacci.kip", stdin: "fibonacci.in" },
  { id: "geri-dönüş-tipi", file: "geri-dönüş-tipi.kip" },
  { id: "gün-örneği", file: "gün-örneği.kip" },
  { id: "iki-tür-bindirme", file: "iki-tür-bindirme.kip" },
  { id: "ikili-ağaç-araması", file: "ikili-ağaç-araması.kip" },
  { id: "iç-örnek-eşleşme", file: "iç-örnek-eşleşme.kip" },
  { id: "iç-örnek-şu", file: "iç-örnek-şu.kip" },
  { id: "karmaşık-eşleşme", file: "karmaşık-eşleşme.kip" },
  { id: "kesişim-çıktı", file: "kesişim-çıktı.kip" },
  { id: "liste-toplamı", file: "liste-toplamı.kip" },
  { id: "selamlamak", file: "selamlamak.kip", stdin: "selamlamak.in" },
  { id: "tam-sayı-aritmetiği", file: "tam-sayı-aritmetiği.kip" },
  { id: "tam-sayı-fark-sırası", file: "tam-sayı-fark-sırası.kip" },
  { id: "tam-sayı-hali", file: "tam-sayı-hali.kip" },
  { id: "tam-sayı-toplamı", file: "tam-sayı-toplamı.kip" },
  { id: "tam-sayı-yazmak", file: "tam-sayı-yazmak.kip" },
  { id: "tireli-örüntü", file: "tireli-örüntü.kip" },
  { id: "ya-da-yapı", file: "ya-da-yapı.kip" },
  { id: "çok-satırlı-eşleşme", file: "çok-satırlı-eşleşme.kip" },
];

sourceEl.value = `(* Example program *)
(bu tam-sayıyı) (şu tam-sayıyı) (o tam-sayıyı) işlemek,
  (onla 0'ın eşitliği) doğruysa,
    durmaktır,
  yanlışsa,
    bunu yazıp,
    şunu (bunla şunun toplamını) (onla 1'in farkını) işlemektir.

çalıştırmak,
  "Bir sayı girin:" yazıp,
  isim olarak okuyup,
  ((ismin tam-sayı-hali)
    yokluksa,
      "Geçersiz sayı." yazmaktır,
    n'nin varlığıysa,
      0'ı 1'i n'yi işlemektir).

çalıştır.`;

async function loadText(path) {
  const res = await fetch(path);
  if (!res.ok) {
    throw new Error(`Failed to load ${path}`);
  }
  return res.text();
}

async function loadBinary(path) {
  const res = await fetch(path);
  if (!res.ok) {
    throw new Error(`Failed to load ${path}`);
  }
  return new Uint8Array(await res.arrayBuffer());
}

async function mountAssets(fs) {
  fs.mkdirSync("/lib", { recursive: true });
  fs.mkdirSync("/vendor", { recursive: true });

  for (const file of libFiles) {
    const text = await loadText(`./assets/lib/${file}`);
    fs.writeFileSync(`/lib/${file}`, text);
  }

  const fst = await loadBinary("./assets/vendor/trmorph.fst");
  fs.writeFileSync("/vendor/trmorph.fst", fst);
}

function setStdin(wasmFs, text) {
  if (!text) {
    return;
  }
  if (wasmFs.stdin && typeof wasmFs.stdin.write === "function") {
    wasmFs.stdin.write(text);
    if (typeof wasmFs.stdin.end === "function") {
      wasmFs.stdin.end();
    }
    return;
  }
  if (typeof wasmFs.setStdin === "function") {
    wasmFs.setStdin(text);
  }
}

async function runKip() {
  runBtn.disabled = true;
  outputEl.textContent = "Running...";
  if (transpiledEl) {
    transpiledEl.textContent = "Transpiling...";
  }

  try {
    const execArgs = ["kip-playground", "--exec", "/main.kip", "--lang", langEl.value];
    const jsArgs = ["kip-playground", "--emit-js", "/main.kip", "--lang", langEl.value];

    const results = await Promise.allSettled([
      runWasm({ args: execArgs, stdinText: stdinEl.value || "" }),
      runWasm({ args: jsArgs, stdinText: "" }),
    ]);

    const [execResult, jsResult] = results;
    outputEl.textContent =
      execResult.status === "fulfilled" ? execResult.value : String(execResult.reason);
    if (transpiledEl) {
      transpiledEl.textContent =
        jsResult.status === "fulfilled" ? jsResult.value : String(jsResult.reason);
    }
  } catch (err) {
    outputEl.textContent = String(err);
    if (transpiledEl) {
      transpiledEl.textContent = String(err);
    }
  } finally {
    runBtn.disabled = false;
  }
}

async function runTranspile() {
  if (!transpiledEl) {
    return;
  }
  if (!transpileBtn) {
    return;
  }
  transpileBtn.disabled = true;
  transpiledEl.textContent = "Transpiling...";

  try {
    const jsArgs = ["kip-playground", "--emit-js", "/main.kip", "--lang", langEl.value];
    const jsResult = await runWasm({ args: jsArgs, stdinText: "" });
    transpiledEl.textContent = jsResult;
  } catch (err) {
    transpiledEl.textContent = String(err);
  } finally {
    transpileBtn.disabled = false;
  }
}

let wasmModulePromise = null;

async function loadWasmModule() {
  if (wasmModulePromise) {
    return wasmModulePromise;
  }
  wasmModulePromise = (async () => {
    try {
      return await WebAssembly.compileStreaming(fetch("./kip-playground.wasm"));
    } catch (err) {
      const res = await fetch("./kip-playground.wasm");
      return WebAssembly.compile(await res.arrayBuffer());
    }
  })();
  return wasmModulePromise;
}

async function runWasm({ args, stdinText }) {
  const encoder = new TextEncoder();
  const rootContents = new Map();
  const libContents = new Map();
  const vendorContents = new Map();

  for (const file of libFiles) {
    const text = await loadText(`./assets/lib/${file}`);
    libContents.set(file, new File(encoder.encode(text), { readonly: true }));
  }

  const fst = await loadBinary("./assets/vendor/trmorph.fst");
  vendorContents.set("trmorph.fst", new File(fst, { readonly: true }));

  rootContents.set("lib", new Directory(libContents));
  rootContents.set("vendor", new Directory(vendorContents));
  rootContents.set("main.kip", new File(encoder.encode(sourceEl.value)));

  const preopen = new PreopenDirectory("/", rootContents);

  const stdoutChunks = [];
  const stderrChunks = [];
  const stdout = ConsoleStdout.lineBuffered((line) => stdoutChunks.push(line));
  const stderr = ConsoleStdout.lineBuffered((line) => stderrChunks.push(line));

  const stdinBytes = encoder.encode(stdinText || "");
  const stdinFile = new OpenFile(new File(stdinBytes, { readonly: true }));

  const wasi = new WASI(args, ["KIP_DATADIR=/"], [stdinFile, stdout, stderr, preopen]);

  const module = await loadWasmModule();
  const instance = await WebAssembly.instantiate(module, {
    wasi_snapshot_preview1: wasi.wasiImport,
  });

  try {
    wasi.start(instance);
  } catch (err) {
    if (!(err && err.code !== undefined)) {
      throw err;
    }
  }

  const combined = [...stdoutChunks, ...stderrChunks].filter(Boolean).join("\n");
  return combined || "(no output)";
}

function formatExampleLabel(example) {
  return example.id;
}

function buildExampleOptions() {
  if (!exampleEl) {
    return;
  }
  exampleEl.innerHTML = "";
  exampleEl.appendChild(new Option("Custom", "__custom__"));
  for (const example of examples) {
    exampleEl.appendChild(new Option(formatExampleLabel(example), example.id));
  }
  exampleEl.value = "__custom__";
}

async function loadExample(exampleId) {
  const example = examples.find((entry) => entry.id === exampleId);
  if (!example) {
    return;
  }
  const [source, stdin] = await Promise.all([
    loadText(`./assets/examples/${example.file}`),
    example.stdin ? loadText(`./assets/examples/${example.stdin}`) : Promise.resolve(""),
  ]);
  sourceEl.value = source;
  stdinEl.value = stdin;
}

if (exampleEl) {
  buildExampleOptions();
  exampleEl.addEventListener("change", async (event) => {
    const value = event.target.value;
    if (value === "__custom__") {
      return;
    }
    try {
      await loadExample(value);
    } catch (err) {
      outputEl.textContent = String(err);
    }
  });
}

runBtn.addEventListener("click", runKip);
if (transpileBtn) {
  transpileBtn.addEventListener("click", runTranspile);
}
