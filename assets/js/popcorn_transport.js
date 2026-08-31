/**
 * PopcornTransport - A WebSocket-compatible transport that bridges
 * Phoenix Channels over Popcorn's OTP/BEAM wasm genserver proxy.
 *
 * Used as `transport` option for Phoenix.Socket:
 *   new LiveSocket("/live", Socket, { transport: PopcornTransport })
 */
export default class PopcornTransport {
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

    this.onopen = null;
    this.onclose = null;
    this.onerror = null;
    this.onmessage = null;

    this.skipHeartbeat = true;
    this._unsubscribe = null;

    window.__popcornTransportConn = this;

    setTimeout(() => this._connect(), 0);
  }

  async _connect() {
    const popcorn = PopcornTransport._popcornInstance;
    if (!popcorn) {
      console.error(
        "PopcornTransport: No Popcorn instance set. Call PopcornTransport.setPopcornInstance() first.",
      );
      this.onerror && this.onerror("no_popcorn_instance");
      return;
    }

    this._unsubscribe = popcorn.onEvent((event) => {
      if (event && event.type === "channel_msg") {
        this.receiveMessage(event.payload);
      }
    });

    try {
      const result = await popcorn.genserver.call(
        "main",
        { type: "transport_connect" },
        { timeoutMs: 15_000 },
      );
      if (!result.ok) throw result.error;
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
    popcorn.genserver.cast("main", { type: "channel_msg", payload: data });
  }

  close(code, _reason) {
    const popcorn = PopcornTransport._popcornInstance;
    this.readyState = PopcornTransport.CLOSING;
    if (this._unsubscribe) {
      this._unsubscribe();
      this._unsubscribe = null;
    }
    if (popcorn) {
      popcorn.genserver.cast("main", { type: "transport_close" });
    }
    this.readyState = PopcornTransport.CLOSED;
    this.onclose && this.onclose({ code: code || 1000 });
  }

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
