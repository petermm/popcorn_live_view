# Vendored Popcorn OTP/BEAM wasm runtime

This is the unpublished Popcorn **OTP/BEAM** stack from
https://github.com/software-mansion/popcorn `main` (`popcorn/elixir` + `popcorn/js`),
not Hex/npm 0.3.3 (that release still uses AtomVM).

- `elixir/` — Mix dependency (`{:popcorn, path: "vendor/popcorn/elixir"}`)
- `js/` — `@swmansion/popcorn` npm package used by `assets/build.mjs`

Rebuild from a popcorn checkout (needs Emscripten, autoconf 2.72, OTP 28.3.1):

```bash
./scripts/build-beam.sh --with-crypto release
pnpm install
pnpm -F ./popcorn/js build
```

Then copy `popcorn/elixir` and `popcorn/js/{package.json,dist}` here.
