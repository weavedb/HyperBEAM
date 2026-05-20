#!/usr/bin/env bash
# Patch @permaweb/ao-loader for Node 24+ wasm-memory64.
#
# ao-loader's growMemory passes a Number to wasmMemory.grow(), but Node 24+
# wasm-memory64 expects a BigInt and rejects Number with "Cannot convert N
# to a BigInt". The float pages calc also rounds to N.9999... which BigInt
# rejects as fractional. This script idempotently rewrites growMemory in
# the installed dist file so genesis-wasm-server can run on Node 24+.
#
# Run after `npm install` in the genesis-wasm-server directory.

set -eu

target_dir="${1:-_build/genesis_wasm/genesis-wasm-server}"
file="${target_dir}/node_modules/@permaweb/ao-loader/dist/index.cjs"

if [ ! -f "${file}" ]; then
    echo "patch-ao-loader: ${file} not found, skipping"
    exit 0
fi

if grep -q "Node 24+ wasm-memory64" "${file}"; then
    echo "patch-ao-loader: already applied"
    exit 0
fi

# Use node to do the rewrite — sed regex over a minified emscripten bundle
# is fragile; this approach matches the three growMemory blocks textually
# and replaces them with the patched form.
node - "${file}" <<'EOF'
const fs = require('fs');
const path = process.argv[2];
let src = fs.readFileSync(path, 'utf8');

const oldBlock = `var growMemory = (size) => {
          var b = wasmMemory.buffer;
          var pages = (size - b.byteLength + 65535) / 65536;
          try {
            wasmMemory.grow(pages);
            updateMemoryViews();
            return 1;
          } catch (e) {
            err(\`growMemory: Attempted to grow heap from \${b.byteLength} bytes to \${size} bytes, but got error: \${e}\`);
          }
        };`;

const newBlock = `var growMemory = (size) => {
          var b = wasmMemory.buffer;
          // Node 24+ wasm-memory64: wasmMemory.grow needs an integer page
          // count, and if the underlying memory is memory64, grow() expects
          // a BigInt (Number args are rejected). Compute as integer pages,
          // then try BigInt first and fall back to Number for memory32.
          var pages = Math.ceil((size - b.byteLength + 65535) / 65536);
          try {
            try {
              wasmMemory.grow(BigInt(pages));
            } catch (_e) {
              wasmMemory.grow(pages);
            }
            updateMemoryViews();
            return 1;
          } catch (e) {
            err(\`growMemory: Attempted to grow heap from \${b.byteLength} bytes to \${size} bytes, but got error: \${e}\`);
          }
        };`;

const before = src.split(oldBlock).length - 1;
if (before === 0) {
    console.error(`patch-ao-loader: pattern not found (file may have changed upstream)`);
    process.exit(1);
}
src = src.split(oldBlock).join(newBlock);
fs.writeFileSync(path, src);
console.error(`patch-ao-loader: replaced ${before} occurrence(s)`);
EOF

echo "patch-ao-loader: done"
