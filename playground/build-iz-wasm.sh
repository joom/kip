#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="${ROOT_DIR}/playground/dist"
ASSET_DIR="${DIST_DIR}/assets"
WASMTIME="${WASMTIME:-wasmtime}"

if command -v "${WASMTIME}" >/dev/null 2>&1; then
  if [[ ! -d "${ASSET_DIR}/lib" ]]; then
    echo "Missing ${ASSET_DIR}/lib; run ./playground/build-assets.sh first."
    exit 1
  fi

  WASMTIME_CMD=("${WASMTIME}")
  if "${WASMTIME}" --help 2>/dev/null | grep -q " run "; then
    WASMTIME_CMD=("${WASMTIME}" "run")
  fi

  if "${WASMTIME}" --help 2>/dev/null | grep -q -- "--mapdir"; then
    "${WASMTIME_CMD[@]}" \
      --mapdir "/::${ASSET_DIR}" \
      --env "KIP_DATADIR=/" \
      "${DIST_DIR}/kip-playground.wasm" --build /lib
  else
    "${WASMTIME_CMD[@]}" \
      --dir "${ASSET_DIR}" \
      --env "KIP_DATADIR=${ASSET_DIR}" \
      "${DIST_DIR}/kip-playground.wasm" --build "${ASSET_DIR}/lib"
  fi
else
  echo "Note: ${WASMTIME} not found; skipping precompilation of .iz caches."
fi
