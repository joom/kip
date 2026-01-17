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
const runBtn = document.getElementById("run");
const langEl = document.getElementById("lang");

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

  try {
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

    const stdinBytes = encoder.encode(stdinEl.value || "");
    const stdinFile = new OpenFile(new File(stdinBytes, { readonly: true }));

    const wasi = new WASI(
      ["kip-playground", "--exec", "/main.kip", "--lang", langEl.value],
      ["KIP_DATADIR=/"],
      [stdinFile, stdout, stderr, preopen]
    );

    let module;
    try {
      module = await WebAssembly.compileStreaming(fetch("./kip-playground.wasm"));
    } catch (err) {
      const res = await fetch("./kip-playground.wasm");
      module = await WebAssembly.compile(await res.arrayBuffer());
    }
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
    outputEl.textContent = combined || "(no output)";
  } catch (err) {
    outputEl.textContent = String(err);
  } finally {
    runBtn.disabled = false;
  }
}

runBtn.addEventListener("click", runKip);
