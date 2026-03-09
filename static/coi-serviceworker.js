/*
 * coi-serviceworker — injects Cross-Origin-Opener-Policy: same-origin and
 * Cross-Origin-Embedder-Policy: credentialless headers so SharedArrayBuffer
 * works on hosts (like GitHub Pages) that don't support custom HTTP headers.
 *
 * Adapted from https://github.com/gzuidhof/coi-serviceworker (MIT)
 */

self.addEventListener("install", () => self.skipWaiting());
self.addEventListener("activate", (e) =>
  e.waitUntil(self.clients.claim())
);

self.addEventListener("fetch", function (event) {
  if (event.request.cache === "only-if-cached" && event.request.mode !== "same-origin") {
    return;
  }

  const isSameOrigin =
    new URL(event.request.url).origin === self.location.origin;

  // Leave all cross-origin responses untouched.
  // Rewriting third-party headers can break framing and CORP checks.
  if (!isSameOrigin) {
    return;
  }

  event.respondWith(
    fetch(event.request)
      .then(function (response) {
        if (response.status === 0) return response;

        const headers = new Headers(response.headers);

        // Our own pages/assets: inject full cross-origin isolation headers so
        // SharedArrayBuffer / WASM threads work.
        headers.set("Cross-Origin-Opener-Policy", "same-origin");
        const isSafari = /Safari\//.test(self.navigator.userAgent) && !/Chrome\//.test(self.navigator.userAgent);
        headers.set("Cross-Origin-Embedder-Policy", isSafari ? "require-corp" : "credentialless");
        headers.set("Cross-Origin-Resource-Policy", "cross-origin");

        return new Response(response.body, {
          status: response.status,
          statusText: response.statusText,
          headers,
        });
      })
      .catch((e) => {
        console.error("COI service worker fetch failed:", e);
        return new Response(e.toString(), { status: 502, statusText: "Bad Gateway" });
      })
  );
});
