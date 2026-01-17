#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="${ROOT_DIR}/playground/dist"
ASSET_DIR="${DIST_DIR}/assets"
EXAMPLES_DIR="${ASSET_DIR}/examples"

mkdir -p "${ASSET_DIR}/lib" "${ASSET_DIR}/vendor" "${EXAMPLES_DIR}"

cp -R "${ROOT_DIR}/lib/." "${ASSET_DIR}/lib/"
cp "${ROOT_DIR}/vendor/trmorph.fst" "${ASSET_DIR}/vendor/trmorph.fst"
cp "${ROOT_DIR}/tests/succeed/"*.kip "${EXAMPLES_DIR}/"
cp "${ROOT_DIR}/tests/succeed/"*.in "${EXAMPLES_DIR}/"
