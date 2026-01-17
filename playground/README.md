# Kip Playground (WASM)

This folder contains a browser playground runner that compiles Kip to `wasm32-wasi` and
loads it in a simple in-browser UI.

## Prerequisites

- GHC WASM toolchain (`wasm32-wasi-ghc` and `wasm32-wasi-ghc-pkg`)
- WASI-enabled C compiler (`wasm32-wasi-clang`)
- A WASM build of **Foma** (libfoma + headers)

You need a WASM build of Foma because Kip’s morphology layer links against `libfoma`.
Build Foma for WASI and set:

```
export FOMA_WASM_PREFIX=/absolute/path/to/foma-wasm-install
```

Where the directory contains:

```
${FOMA_WASM_PREFIX}/include/fomalib.h
${FOMA_WASM_PREFIX}/lib/libfoma.a
```

### Building Foma for WASI

This repo includes a helper script that builds a WASI-compatible static
`libfoma.a` from source:

```
export WASI_SDK_PATH=/absolute/path/to/wasi-sdk
./playground/build-foma-wasm.sh
export FOMA_WASM_PREFIX=/absolute/path/to/kip/playground/foma-wasm
```

You can override the source repo and ref with:

```
FOMA_REPO=https://github.com/mhulden/foma.git \
FOMA_REF=master \
./playground/build-foma-wasm.sh
```

## Build

```
./playground/build-wasm.sh
```

This produces:

```
playground/dist/kip-playground.wasm
playground/dist/index.html
playground/dist/playground.js
playground/dist/style.css
playground/dist/assets/...
```

## Run locally

Serve the `playground/dist` folder with any static server (for example):

```
python3 -m http.server --directory playground/dist 8000
```

Open `http://localhost:8000` and use the Run button.

## Notes

- The playground uses an in-memory WASI filesystem; `lib/` and `vendor/` are copied into
  `playground/dist/assets/` and mounted at runtime.
- Set `KIP_DATADIR=/` in the WASI environment so `lib/` and `vendor/` resolve correctly.
