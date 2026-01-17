#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DIST_DIR="${ROOT_DIR}/playground/dist"
ASSET_DIR="${DIST_DIR}/assets"

mkdir -p "${ASSET_DIR}/lib" "${ASSET_DIR}/vendor"

cp -R "${ROOT_DIR}/lib/." "${ASSET_DIR}/lib/"
cp "${ROOT_DIR}/vendor/trmorph.fst" "${ASSET_DIR}/vendor/trmorph.fst"
