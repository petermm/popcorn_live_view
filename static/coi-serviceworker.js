/*
 * coi-serviceworker — injects Cross-Origin-Opener-Policy: same-origin and
 * Cross-Origin-Embedder-Policy: require-corp headers so SharedArrayBuffer
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

  event.respondWith(
    fetch(event.request)
      .then(function (response) {
        if (response.status === 0) return response;

        const headers = new Headers(response.headers);
        headers.set("Cross-Origin-Opener-Policy", "same-origin");
        headers.set("Cross-Origin-Embedder-Policy", "require-corp");
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
