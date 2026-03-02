#!/usr/bin/env bash
set -euo pipefail

# Re-vendor AtomVM ESP32 firmware blobs into this repo.
# Usage:
#   scripts/revendor_wokwi_firmware.sh
#   ATOMVM_ROOT=/path/to/AtomVM scripts/revendor_wokwi_firmware.sh

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ATOMVM_ROOT="${ATOMVM_ROOT:-$ROOT_DIR/../AtomVM}"
DEST_DIR="$ROOT_DIR/static/wokwi/firmware"

NAMES=(
  "bootloader.bin"
  "atomvm-esp32.bin"
  "partition-table.bin"
  "elixir_esp32boot.avm"
)

SOURCES=(
  "$ATOMVM_ROOT/src/platforms/esp32/build/bootloader/bootloader.bin"
  "$ATOMVM_ROOT/src/platforms/esp32/build/atomvm-esp32.bin"
  "$ATOMVM_ROOT/src/platforms/esp32/build/partition_table/partition-table.bin"
  "$ATOMVM_ROOT/build/libs/esp32boot/elixir_esp32boot.avm"
)

mkdir -p "$DEST_DIR"

echo "Using ATOMVM_ROOT: $ATOMVM_ROOT"
for i in "${!NAMES[@]}"; do
  name="${NAMES[$i]}"
  src="${SOURCES[$i]}"
  if [[ ! -f "$src" ]]; then
    echo "Missing source file: $src" >&2
    exit 1
  fi
done

for i in "${!NAMES[@]}"; do
  name="${NAMES[$i]}"
  src="${SOURCES[$i]}"
  dst="$DEST_DIR/$name"
  cp "$src" "$dst"
  echo "Copied $name"
done

echo
echo "Vendored firmware files:"
ls -lh "$DEST_DIR"/bootloader.bin \
       "$DEST_DIR"/atomvm-esp32.bin \
       "$DEST_DIR"/partition-table.bin \
       "$DEST_DIR"/elixir_esp32boot.avm
