/**
 * PopcornTransport - A WebSocket-compatible transport that bridges
 * Phoenix Channels over Popcorn's WASM postMessage layer.
 *
 * Used as `transport` option for Phoenix.Socket:
 *   new LiveSocket("/live", Socket, { transport: PopcornTransport })
 */
export default class PopcornTransport {
  // WebSocket readyState constants
  static CONNECTING = 0;
  static OPEN = 1;
  static CLOSING = 2;
  static CLOSED = 3;

  constructor(_endpointURL, _protocols) {
    this.readyState = PopcornTransport.CONNECTING;
    this.OPEN = PopcornTransport.OPEN;
    this.CONNECTING = PopcornTransport.CONNECTING;
    this.CLOSING = PopcornTransport.CLOSING;
    this.CLOSED = PopcornTransport.CLOSED;
    this.binaryType = "arraybuffer";
    this.bufferedAmount = 0;

    // Callbacks set by Phoenix.Socket
    this.onopen = null;
    this.onclose = null;
    this.onerror = null;
    this.onmessage = null;

    // Skip heartbeat - no real network connection to keep alive
    this.skipHeartbeat = true;

    // Register this transport instance for receiving messages from WASM
    window.__popcornTransportConn = this;

    // Connect on next tick (mimics WebSocket async connect behavior)
    setTimeout(() => this._connect(), 0);
  }

  async _connect() {
    const popcorn = PopcornTransport._popcornInstance;
    if (!popcorn) {
      console.error("PopcornTransport: No Popcorn instance set. Call PopcornTransport.setPopcornInstance() first.");
      this.onerror && this.onerror("no_popcorn_instance");
      return;
    }

    try {
      await popcorn.call({ type: "transport_connect" }, {});
      this.readyState = PopcornTransport.OPEN;
      this.onopen && this.onopen();
    } catch (e) {
      console.error("PopcornTransport: connect failed", e);
      this.onerror && this.onerror(e);
    }
  }

  send(data) {
    console.debug("[PopcornTransport] send:", data);
    const popcorn = PopcornTransport._popcornInstance;
    if (!popcorn) {
      console.error("PopcornTransport: No Popcorn instance");
      return;
    }
    // Fire-and-forget: channel protocol handles its own ack/reply pattern
    popcorn.cast({ type: "channel_msg", payload: data }, {});
  }

  close(code, _reason) {
    const popcorn = PopcornTransport._popcornInstance;
    this.readyState = PopcornTransport.CLOSING;
    if (popcorn) {
      popcorn.cast({ type: "transport_close" }, {});
    }
    this.readyState = PopcornTransport.CLOSED;
    this.onclose && this.onclose({ code: code || 1000 });
  }

  // Called by WASM side (via run_js) to push messages back to the JS client
  receiveMessage(data) {
    console.debug("[PopcornTransport] recv:", data);
    if (this.onmessage) {
      this.onmessage({ data: data });
    }
  }

  static setPopcornInstance(popcorn) {
    PopcornTransport._popcornInstance = popcorn;
  }
}

// Global callback for WASM to push messages to the transport
window.__popcornTransportReceive = function (data) {
  const conn = window.__popcornTransportConn;
  if (conn) {
    conn.receiveMessage(data);
  }
};
