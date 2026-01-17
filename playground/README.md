# Kip Playground (WASM)

This folder contains a browser playground runner that compiles Kip to `wasm32-wasi` and
loads it in a simple in-browser UI.

## Prerequisites

You need:

- GHC WASM toolchain (`wasm32-wasi-ghc` + `wasm32-wasi-ghc-pkg`)
- WASI SDK (`wasm32-wasi-clang`, `llvm-ar`, `llvm-ranlib`)
- A WASM build of **Foma** (libfoma + headers)

The fastest way to get the toolchain on macOS is the official
`ghc-wasm-meta` setup script, which installs everything into
`playground/.ghc-wasm`:

```
git clone https://gitlab.haskell.org/ghc/ghc-wasm-meta.git playground/.ghc-wasm-meta
cd playground/.ghc-wasm-meta
FLAVOUR=9.10 PREFIX=/absolute/path/to/kip/playground/.ghc-wasm ./setup.sh
```

Then load the environment:

```
source /absolute/path/to/kip/playground/.ghc-wasm/env
export WASI_SDK_PATH=/absolute/path/to/kip/playground/.ghc-wasm/wasi-sdk
```

The build uses `wasm32-wasi-cabal` and expects `ghc-pkg` to match the
WASM GHC version. Add a shim once:

```
ln -sf /absolute/path/to/kip/playground/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc-pkg \
  /absolute/path/to/kip/playground/.ghc-wasm/wasm32-wasi-ghc/bin/ghc-pkg
```

### Foma WASM build

Kip links against `libfoma`, so you must build Foma for WASI and set:

```
export FOMA_WASM_PREFIX=/absolute/path/to/kip/playground/foma-wasm
```

The directory must contain:

```
${FOMA_WASM_PREFIX}/include/fomalib.h
${FOMA_WASM_PREFIX}/include/fomalibconf.h
${FOMA_WASM_PREFIX}/lib/libfoma.a
```

### Building Foma for WASI

This repo includes a helper script that builds a WASI-compatible static
`libfoma.a` from source:

```
export WASI_SDK_PATH=/absolute/path/to/kip/playground/.ghc-wasm/wasi-sdk
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
source /absolute/path/to/kip/playground/.ghc-wasm/env
export WASI_SDK_PATH=/absolute/path/to/kip/playground/.ghc-wasm/wasi-sdk
export FOMA_WASM_PREFIX=/absolute/path/to/kip/playground/foma-wasm
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
python3 -m http.server --directory playground/dist 8001
```

Open `http://localhost:8001` and use the Run button.

## Notes

- The playground uses an in-memory WASI filesystem; `lib/` and `vendor/` are copied into
  `playground/dist/assets/` and mounted at runtime.
- Set `KIP_DATADIR=/` in the WASI environment so `lib/` and `vendor/` resolve correctly.
