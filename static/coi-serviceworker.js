/*
 * coi-serviceworker — injects Cross-Origin-Opener-Policy: same-origin,
 * Cross-Origin-Embedder-Policy: require-corp, and Cross-Origin-Resource-Policy:
 * cross-origin so SharedArrayBuffer + AtomVM module workers work on hosts
 * (like GitHub Pages) that don't support custom HTTP headers.
 *
 * Adapted from https://github.com/gzuidhof/coi-serviceworker (MIT)
 *
 * Notes:
 * - On localhost, server.exs already sets COOP/COEP; prefer not registering this SW.
 * - Must not invent 502s: if re-headering fails, fall through to the network response.
 * - Module workers reload AtomVM.mjs; re-wrapping gzip responses can break them, so
 *   strip content-encoding/content-length when cloning (body stream is already decoded
 *   by the browser in modern SW fetch, or we re-fetch without Accept-Encoding).
 */

self.addEventListener("install", () => self.skipWaiting());
self.addEventListener("activate", (e) => e.waitUntil(self.clients.claim()));

// Allow the page to kick a waiting worker into place after register().update().
self.addEventListener("message", (event) => {
  if (event.data && event.data.type === "SKIP_WAITING") {
    self.skipWaiting();
  }
});

self.addEventListener("fetch", function (event) {
  if (
    event.request.cache === "only-if-cached" &&
    event.request.mode !== "same-origin"
  ) {
    return;
  }

  let url;
  try {
    url = new URL(event.request.url);
  } catch (_e) {
    return;
  }

  // Leave all cross-origin responses untouched.
  if (url.origin !== self.location.origin) {
    return;
  }

  event.respondWith(handleSameOrigin(event.request));
});

async function handleSameOrigin(request) {
  // Prefer a plain URL fetch for workers/modules — some Request objects from
  // module-worker loads are awkward to re-dispatch as-is.
  const isWorkerish =
    request.destination === "worker" ||
    request.destination === "sharedworker" ||
    request.destination === "script" ||
    request.destination === "";

  let response;
  try {
    if (isWorkerish && request.method === "GET") {
      response = await fetch(request.url, {
        method: "GET",
        // Avoid gzip edge-cases when re-wrapping the body with new headers.
        headers: { "Accept-Encoding": "identity" },
        credentials: "same-origin",
        cache: request.cache === "only-if-cached" ? "default" : request.cache,
        redirect: "follow",
      });
    } else {
      response = await fetch(request);
    }
  } catch (e) {
    console.error("COI service worker network fetch failed:", e);
    // Last resort: try bare URL once more; never invent a synthetic 502 that
    // looks like the origin failed when the SW is the problem.
    try {
      return await fetch(request.url, { credentials: "same-origin" });
    } catch (e2) {
      console.error("COI service worker fallback fetch failed:", e2);
      return new Response("COI SW fetch failed: " + String(e2), {
        status: 504,
        statusText: "Gateway Timeout",
        headers: { "Content-Type": "text/plain" },
      });
    }
  }

  if (response.status === 0) {
    return response;
  }

  try {
    const headers = new Headers(response.headers);

    // Our own pages/assets: inject full cross-origin isolation headers so
    // SharedArrayBuffer / WASM threads (and AtomVM module workers) work.
    // Prefer require-corp + CORP everywhere — credentialless is rejected by
    // Safari/WebKit when spawning dedicated module workers for AtomVM.mjs.
    headers.set("Cross-Origin-Opener-Policy", "same-origin");
    headers.set("Cross-Origin-Embedder-Policy", "require-corp");
    headers.set("Cross-Origin-Resource-Policy", "cross-origin");

    // When constructing a new Response around an existing body stream, drop
    // encoding/length headers so the browser doesn't try to re-decode gzip.
    headers.delete("content-encoding");
    headers.delete("content-length");

    return new Response(response.body, {
      status: response.status,
      statusText: response.statusText,
      headers,
    });
  } catch (e) {
    console.error("COI service worker header rewrite failed:", e);
    // Return the original network response rather than a fake 502.
    return response;
  }
}
