import * as esbuild from "esbuild";
import { popcorn } from "@swmansion/popcorn/esbuild";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const assetsDir = dirname(fileURLToPath(import.meta.url));
const rootDir = resolve(assetsDir, "..");
const outDir = resolve(rootDir, "static/assets");

await esbuild.build({
  absWorkingDir: assetsDir,
  entryPoints: [resolve(assetsDir, "js/app.js")],
  bundle: true,
  format: "esm",
  sourcemap: true,
  outdir: outDir,
  nodePaths: [resolve(rootDir, "deps"), resolve(rootDir, "_build/dev")],
  plugins: [
    popcorn({
      rootDir,
      app: "wasm_live_view",
      extraApps: [
        "phoenix",
        "phoenix_live_view",
        "phoenix_html",
        "phoenix_template",
        "phoenix_pubsub",
        "phoenix_ecto",
        "jason",
        "plug",
        "plug_crypto",
        "ecto",
        "decimal",
        "req",
        "finch",
        "mint",
        "hpax",
        "nimble_options",
        "nimble_pool",
        "mime",
        "telemetry",
        "extty",
        "iex",
        "easel",
        "nimble_parsec",
        "atomvm_packbeam",
      ],
    }),
  ],
});
