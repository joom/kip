#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="${ROOT_DIR}/playground/dist"

WASM_GHC="${WASM_GHC:-wasm32-wasi-ghc}"
WASM_GHC_PKG="${WASM_GHC_PKG:-wasm32-wasi-ghc-pkg}"
WASM_CC="${WASM_CC:-wasm32-wasi-clang}"
WASM_CABAL="${WASM_CABAL:-wasm32-wasi-cabal}"
ZLIB_WASM_PREFIX="${ZLIB_WASM_PREFIX:-${ROOT_DIR}/playground/.foma-wasm/zlib-wasm}"
WASM_OPT="${WASM_OPT:-wasm-opt}"

if [[ -z "${FOMA_WASM_PREFIX:-}" ]]; then
  echo "FOMA_WASM_PREFIX is not set."
  echo "Set it to the prefix containing include/fomalib.h and lib/libfoma.a."
  exit 1
fi

mkdir -p "${DIST_DIR}"

GHC_OPT_FLAGS="-O2 -split-sections -optl=-Wl,--gc-sections"
WASM_OPT_FLAGS="-O3"

pushd "${ROOT_DIR}" >/dev/null

"${WASM_CABAL}" build kip-playground \
  --with-compiler="${WASM_GHC}" \
  --with-hc-pkg="${WASM_GHC_PKG}" \
  --with-gcc="${WASM_CC}" \
  --ghc-options="${GHC_OPT_FLAGS}" \
  --extra-include-dirs="${FOMA_WASM_PREFIX}/include" \
  --extra-include-dirs="${ZLIB_WASM_PREFIX}/include" \
  --extra-lib-dirs="${FOMA_WASM_PREFIX}/lib" \
  --extra-lib-dirs="${ZLIB_WASM_PREFIX}/lib"

BIN_PATH="$(cabal list-bin kip-playground --with-compiler="${WASM_GHC}")"
cp "${BIN_PATH}" "${DIST_DIR}/kip-playground.wasm"

popd >/dev/null

cp "${ROOT_DIR}/playground/index.html" "${DIST_DIR}/index.html"
cp "${ROOT_DIR}/playground/playground.js" "${DIST_DIR}/playground.js"
cp "${ROOT_DIR}/playground/style.css" "${DIST_DIR}/style.css"
cp "${ROOT_DIR}/playground/logo.png" "${DIST_DIR}/logo.png"
cp "${ROOT_DIR}/playground/kip-worker.js" "${DIST_DIR}/kip-worker.js"
cp "${ROOT_DIR}/playground/coi-serviceworker.js" "${DIST_DIR}/coi-serviceworker.js"

if command -v "${WASM_OPT}" >/dev/null 2>&1; then
  "${WASM_OPT}" ${WASM_OPT_FLAGS} -o "${DIST_DIR}/kip-playground.wasm" "${DIST_DIR}/kip-playground.wasm"
else
  echo "Note: ${WASM_OPT} not found; skipping post-link WASM optimization."
fi

"${ROOT_DIR}/playground/build-assets.sh"
"${ROOT_DIR}/playground/build-iz-wasm.sh"

echo "WASM build complete: ${DIST_DIR}/kip-playground.wasm"
