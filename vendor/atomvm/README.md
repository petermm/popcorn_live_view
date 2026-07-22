# Vendored AtomVM (Release WASM)

Popcorn's published `@swmansion/popcorn` npm package currently ships a **Debug**
AtomVM build (`SAFE_HEAP` + assertions). That turns FissionVM `assert()` failures
into browser aborts (`Aborted(native code called abort())`) during
`Popcorn.Wasm.run_js` / tracked-object eval — which breaks the `:re` stub used by
`/regex-tester` and any other heavy `run_js` usage.

This directory holds a **Release** `AtomVM.mjs` + `AtomVM.wasm` used by `mix cook`.

Rebuild (from the popcorn monorepo):

```bash
./scripts/build-atomvm.sh release-wasm
# then copy out/AtomVM.{mjs,wasm} here
```

Or from FissionVM:

```bash
# platforms/emscripten Release build → copy src/AtomVM.{mjs,wasm} here
```
