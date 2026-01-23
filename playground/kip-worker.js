import { WASI, File, Directory, PreopenDirectory, ConsoleStdout, Fd } from "https://esm.sh/@bjorn3/browser_wasi_shim@0.4.2";

const libFiles = [
  "giriş.kip",
  "temel.kip",
  "temel-doğruluk.kip",
  "temel-dizge.kip",
  "temel-etki.kip",
  "temel-liste.kip",
  "temel-tam-sayı.kip",
  "temel-ondalık-sayı.kip",
];

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

class InteractiveStdin extends Fd {
  constructor(signal, buffer) {
    super();
    this.signal = new Int32Array(signal);
    this.buffer = new Uint8Array(buffer);
    this.pending = new Uint8Array();
    this.offset = 0;
  }

  fd_fdstat_get() {
    const fdstat = {
      write_bytes(view, offset) {
        view.setUint8(offset, 2);
        view.setUint16(offset + 2, 0, true);
        view.setBigUint64(offset + 8, 0n, true);
        view.setBigUint64(offset + 16, 0n, true);
      },
    };
    return { ret: 0, fdstat };
  }

  fd_read(len) {
    if (this.offset < this.pending.length) {
      const end = Math.min(this.pending.length, this.offset + len);
      const chunk = this.pending.slice(this.offset, end);
      this.offset = end;
      return { ret: 0, data: chunk };
    }

    Atomics.store(this.signal, 0, 1);
    postMessage({ type: "stdin-request" });
    Atomics.wait(this.signal, 0, 1);

    if (this.signal[0] !== 2) {
      return { ret: 0, data: new Uint8Array() };
    }

    const length = this.signal[1];
    const next = this.buffer.slice(0, length);
    this.pending = next;
    this.offset = 0;
    this.signal[0] = 0;
    this.signal[1] = 0;

    if (this.pending.length === 0) {
      return { ret: 0, data: new Uint8Array() };
    }

    const end = Math.min(this.pending.length, len);
    const chunk = this.pending.slice(0, end);
    this.offset = end;
    return { ret: 0, data: chunk };
  }
}

class EmptyStdin extends Fd {
  fd_fdstat_get() {
    const fdstat = {
      write_bytes(view, offset) {
        view.setUint8(offset, 2);
        view.setUint16(offset + 2, 0, true);
        view.setBigUint64(offset + 8, 0n, true);
        view.setBigUint64(offset + 16, 0n, true);
      },
    };
    return { ret: 0, fdstat };
  }

  fd_read() {
    return { ret: 0, data: new Uint8Array() };
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

async function runWasm({ args, source, signal, buffer }) {
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
  rootContents.set("main.kip", new File(encoder.encode(source)));

  const preopen = new PreopenDirectory("/", rootContents);

  const stdout = ConsoleStdout.lineBuffered((line) => postMessage({ type: "stdout", line }));
  const stderr = ConsoleStdout.lineBuffered((line) => postMessage({ type: "stderr", line }));
  const stdinFile = signal && buffer ? new InteractiveStdin(signal, buffer) : new EmptyStdin();

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
}

self.addEventListener("message", async (event) => {
  const { type, args, source, signal, buffer } = event.data || {};
  if (type !== "run") {
    return;
  }
  try {
    await runWasm({ args, source, signal, buffer });
    postMessage({ type: "exit" });
  } catch (err) {
    postMessage({ type: "error", error: String(err) });
  }
});
