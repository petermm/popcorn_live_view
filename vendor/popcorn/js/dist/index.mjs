function err$1(t, data) {
    return new PopcornError({ t, data });
}
class PopcornError extends Error {
    cause;
    serialized;
    constructor(cause) {
        super(message(cause), { cause });
        this.name = "PopcornError";
        Object.setPrototypeOf(this, new.target.prototype);
        this.cause = cause;
        this.serialized = cause;
    }
    get t() {
        return this.serialized.t;
    }
    get data() {
        return this.serialized.data;
    }
    serialize() {
        return {
            t: this.serialized.t,
            data: { ...this.serialized.data },
        };
    }
    static deserialize(value) {
        return new PopcornError(parse(value));
    }
}
function message(error) {
    switch (error.t) {
        case "timeout:init":
            return `Init timed out after ${error.data.timeoutMs}ms`;
        case "timeout:send":
            return `Send timed out after ${error.data.timeoutMs}ms`;
        case "timeout:call":
            return `Call timed out after ${error.data.timeoutMs}ms`;
        case "worker:load":
            return error.data.message;
        case "vm:exited":
            return "VM exited";
        case "bridge:not-started":
            return "Bridge did not start";
        case "bridge:invalid-target":
            return "Target must be a non-empty name or a PID from this VM boot";
        case "bridge:unserializable":
            return "Message can't be serialized to ETF";
        case "bridge:listener-not-found":
            return `Target listener not found: '${error.data.targetName}'`;
        case "genserver:noproc":
            return `No process registered for genserver target: '${error.data.target}'`;
        case "genserver:exit":
            return `Genserver exited: ${error.data.reason}`;
        case "genserver:unserializable":
            return "Genserver reply can't be serialized to JSON";
        case "stdio:overflow":
            return `Stdin chunk exceeds the ${error.data.capacityBytes} byte queue capacity`;
        case "beam:missing-boot-script":
            return `Missing boot script: '${error.data.url}'`;
        case "beam:missing-manifest":
            return `Missing tarball manifest: '${error.data.url}'`;
        case "beam:missing-tarball":
            return `Missing tarball: '${error.data.name}'. Available tarballs: ${error.data.all.join(", ")}`;
        case "internal:check":
            return error.data.detail === undefined
                ? "Check failed"
                : `Check failed: ${error.data.detail}`;
        case "internal:unreachable":
            return "Entered unreachable code";
        case "runtime:eval-unavailable":
            return "JS eval is unavailable; run_js requires a Content-Security-Policy that allows 'unsafe-eval'";
        default:
            unreachable$1();
    }
}
function parse(value) {
    check$1(objectWithKeys$1(value, ["t", "data"]));
    switch (value.t) {
        case "timeout:init":
        case "timeout:send":
            check$1(isTimeoutData(value.data));
            return { t: value.t, data: value.data };
        case "worker:load":
            check$1(isWorkerLoadData(value.data));
            return { t: value.t, data: value.data };
        case "vm:exited":
            check$1(isVmExitedData(value.data));
            return { t: value.t, data: value.data };
        case "bridge:not-started":
            check$1(isEmptyData(value.data));
            return { t: value.t, data: value.data };
        case "bridge:invalid-target":
            check$1(isEmptyData(value.data));
            return { t: value.t, data: value.data };
        case "bridge:unserializable":
            check$1(isUnserializableData(value.data));
            return { t: value.t, data: value.data };
        case "bridge:listener-not-found":
            check$1(isListenerNotFoundData(value.data));
            return { t: value.t, data: value.data };
        case "stdio:overflow":
            check$1(isStdioOverflowData(value.data));
            return { t: value.t, data: value.data };
        case "beam:missing-boot-script":
        case "beam:missing-manifest":
            check$1(isUrlData(value.data));
            return { t: value.t, data: value.data };
        case "beam:missing-tarball":
            check$1(isMissingTarballData(value.data));
            return { t: value.t, data: value.data };
        case "internal:check":
            check$1(isInternalCheckData(value.data));
            return { t: value.t, data: value.data };
        case "internal:unreachable":
        case "runtime:eval-unavailable":
            check$1(isEmptyData(value.data));
            return { t: value.t, data: value.data };
        default:
            unreachable$1();
    }
}
function isTimeoutData(value) {
    return objectWithKeys$1(value, ["timeoutMs"]) !== null;
}
function isWorkerLoadData(value) {
    return objectWithKeys$1(value, ["message"]) !== null;
}
function isVmExitedData(value) {
    return objectWithKeys$1(value, ["reason"]) !== null;
}
function isListenerNotFoundData(value) {
    return objectWithKeys$1(value, ["targetName"]) !== null;
}
function isStdioOverflowData(value) {
    return objectWithKeys$1(value, ["capacityBytes", "attemptedBytes"]) !== null;
}
function isUnserializableData(value) {
    return (objectWithKeys$1(value, ["data", "part", "reason"]) &&
        isUnserializableReason(value.reason));
}
function isUnserializableReason(value) {
    return (value === "cyclic-object" ||
        value === "non-plain-object" ||
        value === "lossy-int" ||
        value === "non-finite-float" ||
        value === "unsupported");
}
function isUrlData(value) {
    return objectWithKeys$1(value, ["url"]) !== null;
}
function isMissingTarballData(value) {
    return objectWithKeys$1(value, ["name", "all"]) !== null;
}
function isInternalCheckData(value) {
    return objectWithKeys$1(value, []) !== null;
}
function isEmptyData(value) {
    return objectWithKeys$1(value, []) !== null;
}
function objectWithKeys$1(value, keys) {
    const isObject = value !== null && typeof value === "object";
    return isObject && keys.every((key) => Object.hasOwn(value, key));
}
function unreachable$1() {
    throw err$1("internal:unreachable", {});
}
function check$1(ok, msg) {
    if (!ok) {
        throw err$1("internal:check", { detail: msg });
    }
}

function base64ToBytes(b64) {
    const binary = atob(b64);
    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) {
        bytes[i] = binary.charCodeAt(i);
    }
    return bytes;
}
function check(ok, msg) {
    if (!ok)
        throw err$1("internal:check", { detail: msg });
}
function unreachable() {
    throw err$1("internal:unreachable", {});
}
function objectWithKeys(value, keys) {
    if (value === null || typeof value !== "object")
        return null;
    if (value.constructor !== Object)
        return null;
    const hasAllKeys = keys.every((k) => Object.hasOwn(value, k));
    if (!hasAllKeys)
        return null;
    return value;
}

const VERSION = 0x83;
const NEW_FLOAT_EXT = 0x46;
const SMALL_INTEGER_EXT = 0x61;
const INTEGER_EXT = 0x62;
const SMALL_TUPLE_EXT = 0x68;
const LARGE_TUPLE_EXT = 0x69;
const NIL_EXT = 0x6a;
const LIST_EXT = 0x6c;
const BINARY_EXT = 0x6d;
const SMALL_BIG_EXT = 0x6e;
const MAP_EXT = 0x74;
const ATOM_UTF8_EXT = 0x76;
const SMALL_ATOM_UTF8_EXT = 0x77;
const UTF8$1 = new TextEncoder();
class AtomTerm {
    name;
    constructor(name) {
        this.name = name;
    }
}
class TupleTerm {
    entries;
    constructor(entries) {
        this.entries = entries;
    }
}
function atom(name) {
    return new AtomTerm(name);
}
const a = atom;
function tuple(first, second, ...rest) {
    check(arguments.length > 1, "tuple requires at least two entries");
    return new TupleTerm([first, second, ...rest]);
}
const t = tuple;
/** Pre-encoded ETF sub-term bytes (no version prefix), spliced verbatim. */
class RawTerm {
    bytes;
    constructor(bytes) {
        this.bytes = bytes;
    }
    /**
     * Wraps a full external term (`term_to_binary` output) as a spliceable
     * sub-term by dropping its leading version byte.
     */
    static fromExternal(external) {
        if (external[0] !== VERSION) {
            throw new TypeError("expected a version-prefixed ETF external term");
        }
        return new RawTerm(external.subarray(1));
    }
}
function encode(data, mapper) {
    try {
        return { ok: true, data: new Encoder(mapper).encode(data) };
    }
    catch (error) {
        let reason = "unsupported";
        let part = data;
        if (error instanceof TypeError && isUnserializableReason(error.message)) {
            reason = error.message;
            part = error.cause;
        }
        return {
            ok: false,
            error: err$1("bridge:unserializable", { data, part, reason }),
        };
    }
}
class Encoder {
    mapper;
    ancestors = new Set();
    output = [];
    buffer = new ArrayBuffer(8);
    view = new DataView(this.buffer);
    constructor(mapper = (value) => value) {
        this.mapper = mapper;
    }
    encode(value) {
        this.byte(VERSION);
        this.value(value);
        return new Uint8Array(this.output);
    }
    value(value) {
        if (value === null) {
            this.atom("nil");
            return;
        }
        switch (typeof value) {
            case "boolean":
                this.atom(value ? "true" : "false");
                return;
            case "string":
                this.binary(value);
                return;
            case "number":
                this.number(value);
                return;
            case "object":
                this.object(value);
                return;
            default:
                throw err("unsupported", value);
        }
    }
    number(value) {
        if (!Number.isFinite(value)) {
            throw err("non-finite-float", value);
        }
        if (!Number.isInteger(value)) {
            this.byte(NEW_FLOAT_EXT);
            this.float64(value);
            return;
        }
        if (!Number.isSafeInteger(value)) {
            throw err("lossy-int", value);
        }
        if (value >= 0 && value < 2 ** 8) {
            this.byte(SMALL_INTEGER_EXT);
            this.byte(value);
            return;
        }
        if (value >= -2147483648 && value < 2 ** 31) {
            this.byte(INTEGER_EXT);
            this.int32(value);
            return;
        }
        this.smallBigInt(value);
    }
    // https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#small_big_ext
    smallBigInt(value) {
        let magnitude = BigInt(Math.abs(value));
        const digits = [];
        while (magnitude > 0n) {
            digits.push(Number(magnitude & 0xffn));
            magnitude >>= 8n;
        }
        this.byte(SMALL_BIG_EXT);
        this.byte(digits.length);
        this.byte(value < 0 ? 1 : 0);
        this.bytes(digits);
    }
    object(rawValue) {
        const value = this.mapper(rawValue);
        if (value instanceof RawTerm) {
            this.bytes(value.bytes);
            return;
        }
        if (value instanceof AtomTerm) {
            this.atom(value.name);
            return;
        }
        if (this.ancestors.has(value)) {
            throw err("cyclic-object", value);
        }
        this.ancestors.add(value);
        try {
            if (value instanceof TupleTerm) {
                this.tuple(value.entries);
                return;
            }
            if (Array.isArray(value)) {
                this.array(value);
                return;
            }
            const prototype = Object.getPrototypeOf(value);
            const isObject = prototype === Object.prototype || prototype === null;
            if (!isObject) {
                throw err("non-plain-object", value);
            }
            this.map(value);
        }
        finally {
            this.ancestors.delete(value);
        }
    }
    tuple(entries) {
        if (entries.length < 2 ** 8) {
            this.byte(SMALL_TUPLE_EXT);
            this.byte(entries.length);
        }
        else {
            this.byte(LARGE_TUPLE_EXT);
            this.uint32(entries.length);
        }
        for (const entry of entries) {
            this.value(entry);
        }
    }
    array(value) {
        if (value.length === 0) {
            this.byte(NIL_EXT);
            return;
        }
        this.byte(LIST_EXT);
        this.uint32(value.length);
        for (const item of value) {
            this.value(item);
        }
        this.byte(NIL_EXT);
    }
    map(value) {
        const keys = Object.keys(value).sort();
        this.byte(MAP_EXT);
        this.uint32(keys.length);
        for (const key of keys) {
            this.binary(key);
            this.value(value[key]);
        }
    }
    atom(atom) {
        const bytes = UTF8$1.encode(atom);
        if (bytes.length >= 2 ** 16) {
            throw err("unsupported", atom);
        }
        if (bytes.length < 2 ** 8) {
            this.byte(SMALL_ATOM_UTF8_EXT);
            this.byte(bytes.length);
        }
        else {
            this.byte(ATOM_UTF8_EXT);
            this.uint16(bytes.length);
        }
        this.bytes(bytes);
    }
    binary(value) {
        const bytes = UTF8$1.encode(value);
        this.byte(BINARY_EXT);
        this.uint32(bytes.length);
        this.bytes(bytes);
    }
    byte(value) {
        this.output.push(value);
    }
    bytes(values) {
        for (let index = 0; index < values.length; index++) {
            this.output.push(values[index]);
        }
    }
    uint32(value) {
        this.view.setUint32(0, value);
        this.bytes(new Uint8Array(this.buffer, 0, 4));
    }
    uint16(value) {
        this.view.setUint16(0, value);
        this.bytes(new Uint8Array(this.buffer, 0, 2));
    }
    int32(value) {
        this.view.setInt32(0, value);
        this.bytes(new Uint8Array(this.buffer, 0, 4));
    }
    float64(value) {
        this.view.setFloat64(0, value);
        this.bytes(new Uint8Array(this.buffer));
    }
}
function err(reason, part) {
    return new TypeError(reason, { cause: part });
}

function readWorkerEvent(value) {
    const data = objectWithKeys(value, ["type", "payload"]);
    check(data !== null && typeof data.type === "string");
    switch (data.type) {
        case "otp:stdout":
        case "otp:stderr":
        case "otp:error":
        case "otp:message":
        case "otp:run_js":
        case "otp:tracked-value-delete":
        case "popcorn:boot-vm-ready":
        case "popcorn:boot-end":
        case "popcorn:boot-fail":
        case "popcorn:send-end":
            return data;
        case "otp:stdin-consumed":
            check(Number(data.payload) > 0);
            return data;
        default:
            unreachable();
    }
}
function serializeSendPayload(target, payload, mapper) {
    if (isNameTarget(target)) {
        check(target.name.length > 0);
    }
    else {
        check(target.pid.byteLength > 0);
    }
    const etf = encode(payload, mapper);
    if (!etf.ok)
        return etf;
    return { ok: true, data: { target, etf: etf.data } };
}
function isNameTarget(target) {
    return Object.hasOwn(target, "name");
}
/** Usable only from main context. */
function toVm(worker, event, transfer) {
    worker.postMessage(event, transfer ?? []);
}

const TRACKED_REF_KEY = "popcorn_ref";
const PID_REF_KEY = "popcorn_pid";
const UTF8 = new TextEncoder();
const STDIN_QUEUE_CAPACITY_BYTES = 64 * 1024;
const DEFAULT_TTY_SIZE = { columns: 80, rows: 24 };
const DEFAULT_TIMEOUTS_MS = {
    boot: 10_000,
    appStartup: 60_000,
    send: 5_000,
};
const LOG_PREFIX = "[Popcorn]";
const DEFAULT_PROXY_NAME = "popcorn_proxy";
const DEFAULT_CALL_TIMEOUT_MS = 5_000;
function createPidClass() {
    return class {
        bytes;
        constructor(bytes) {
            this.bytes = bytes;
        }
    };
}
function assertRunJsFn(value) {
    check(typeof value === "function");
}
class Popcorn {
    vmWorker;
    state = { status: "created" };
    opts;
    ttySize;
    output;
    requestSeq = 0;
    settleBoot = null;
    eventHandlers = new Set();
    pendingSends = new Map();
    pendingCalls = new Map();
    callSeq = 0;
    trackedValues = new Map();
    trackedKeySeq = 0;
    io = createIoState();
    vmReady = false;
    genserver = {
        call: (target, request, opts) => this.call(target, request, opts),
        cast: (target, request, opts) => this.cast(target, request, opts),
    };
    TrackedValue = class {
        value;
        cleanup;
        constructor(value, cleanup) {
            this.value = value;
            this.cleanup = cleanup;
        }
    };
    Pid = createPidClass();
    onWorkerMessage = (event) => {
        const data = readWorkerEvent(event.data);
        switch (data.type) {
            case "popcorn:boot-vm-ready":
            case "popcorn:boot-end":
            case "popcorn:boot-fail":
                return;
            case "otp:message":
                this.emit(this.reviveHandles(data.payload));
                return;
            case "otp:run_js":
                this.vmReady = true;
                this.runJs(data.payload);
                return;
            case "otp:tracked-value-delete":
                this.deleteTrackedValue(data.payload);
                return;
            case "otp:stdout":
                this.handleStdout(data.payload);
                return;
            case "otp:stderr":
                this.handleStderr(data.payload);
                return;
            case "otp:stdin-consumed":
                check(data.payload > 0 && data.payload <= this.io.stdin.reservedBytes);
                this.io.stdin.reservedBytes -= data.payload;
                return;
            case "otp:error":
                this.handleOtpError(data.payload);
                return;
            case "popcorn:send-end": {
                this.completeSend(data.payload);
                return;
            }
            default:
                unreachable();
        }
    };
    constructor(opts) {
        const ttySize = opts.tty?.size ?? DEFAULT_TTY_SIZE;
        check(isValidTtySize(ttySize));
        check(opts.beam?.otpAssetsRoot === undefined ||
            opts.beam.otpAssetsRoot.endsWith("/"), "otpAssetsRoot must end with a slash");
        this.opts = {
            ...opts,
            beam: {
                ...opts.beam,
                emulatorArgs: opts.beam?.emulatorArgs ??
                    schedulers({ base: 1, dirtyCpu: 1, dirtyIo: 1 }),
            },
        };
        this.ttySize = { ...ttySize };
        this.output = resolveOutputHandlers(opts);
        this.spawnWorker();
    }
    spawnWorker() {
        this.vmWorker = this.opts.workerUrl
            ? new Worker(this.opts.workerUrl, { type: "module" })
            : // Keep this as one expression so Vite recognizes and bundles the worker.
                new Worker(new URL("./worker.mjs", import.meta.url), {
                    type: "module",
                });
        this.vmWorker.addEventListener("message", this.onWorkerMessage);
    }
    static async init(opts) {
        if (!canEval()) {
            return { ok: false, error: err$1("runtime:eval-unavailable", {}) };
        }
        const popcorn = new Popcorn(opts);
        const result = await popcorn.boot();
        if (!result.ok) {
            return result;
        }
        return { ok: true, data: popcorn };
    }
    /**
     * Starts the VM and resolves after its bridge is ready and the entrypoint
     * application has started. `timeoutsMs.boot` bounds the wait for the VM
     * bridge; `timeoutsMs.appStartup` bounds the entrypoint startup that
     * follows. Register event handlers before calling this
     * method when the application sends messages during startup. Processes
     * registered later by handle_continue or spawned work can still return
     * genserver:noproc immediately after boot.
     */
    async boot() {
        if (this.state.status === "booted") {
            return { ok: true, data: this };
        }
        if (this.state.status === "booting") {
            // TODO(jgonet): make it easier to construct check() errors without throwing
            const error = err$1("internal:check", {
                detail: "Boot already in progress",
            });
            return { ok: false, error };
        }
        const reboot = this.state.status === "closed";
        if (reboot) {
            this.spawnWorker();
        }
        this.Pid = createPidClass();
        this.io = createIoState();
        this.output = resolveOutputHandlers(this.opts);
        this.state = { status: "booting" };
        return await new Promise((resolve) => {
            const timeoutsMs = { ...DEFAULT_TIMEOUTS_MS, ...this.opts.timeoutsMs };
            const settle = (result) => {
                if (this.settleBoot === null)
                    return;
                clearTimeout(timer);
                cleanup();
                if (!result.ok) {
                    this.deinit();
                }
                resolve(result);
            };
            this.settleBoot = settle;
            const startPhase = (timeoutMs) => setTimeout(() => {
                const error = err$1("timeout:init", { timeoutMs });
                settle({ ok: false, error });
            }, timeoutMs);
            // The VM phase covers module instantiation and bridge readiness; the
            // app phase covers the entrypoint's application tree, which runs
            // arbitrary user startup code and can be much slower.
            let timer = startPhase(timeoutsMs.boot);
            const onBootMessage = (event) => {
                const data = readWorkerEvent(event.data);
                switch (data.type) {
                    case "popcorn:boot-vm-ready":
                        clearTimeout(timer);
                        timer = startPhase(timeoutsMs.appStartup);
                        break;
                    case "popcorn:boot-end":
                        this.state = { status: "booted" };
                        settle({ ok: true, data: this });
                        break;
                    case "popcorn:boot-fail": {
                        const error = PopcornError.deserialize(data.payload);
                        settle({ ok: false, error });
                        break;
                    }
                }
            };
            const cleanup = () => {
                this.settleBoot = null;
                this.vmWorker.removeEventListener("message", onBootMessage);
            };
            this.vmWorker.addEventListener("message", onBootMessage);
            toVm(this.vmWorker, {
                type: "popcorn:boot",
                payload: { ...this.opts.beam, ttySize: this.ttySize },
            });
        });
    }
    writeStdin(chunk) {
        if (this.state.status === "closed") {
            return { ok: false, error: this.state.error };
        }
        check(this.state.status === "booted");
        const bytes = toBytes(chunk);
        check(bytes.byteLength > 0);
        const attemptedBytes = this.io.stdin.reservedBytes + bytes.byteLength;
        if (attemptedBytes > STDIN_QUEUE_CAPACITY_BYTES) {
            const error = err$1("stdio:overflow", {
                capacityBytes: STDIN_QUEUE_CAPACITY_BYTES,
                attemptedBytes,
            });
            return { ok: false, error };
        }
        this.io.stdin.reservedBytes = attemptedBytes;
        const event = { type: "popcorn:stdin", payload: { chunk: bytes } };
        toVm(this.vmWorker, event, [bytes.buffer]);
        return { ok: true, data: null };
    }
    resizeTty(columns, rows) {
        if (this.state.status === "closed") {
            return { ok: false, error: this.state.error };
        }
        check(this.state.status === "booted");
        check(isValidTtySize({ columns, rows }));
        toVm(this.vmWorker, {
            type: "popcorn:tty-resize",
            payload: { columns, rows },
        });
        return { ok: true, data: null };
    }
    /**
     * Resolves after VM sent message to registered process.
     */
    async send(rawTarget, payload) {
        if (this.state.status !== "booted") {
            if (this.state.status === "closed") {
                return { ok: false, error: this.state.error };
            }
            return { ok: false, error: err$1("bridge:not-started", {}) };
        }
        return await this.sendBridge(rawTarget, payload);
    }
    async sendBridge(rawTarget, payload) {
        let target;
        if (typeof rawTarget === "string" && rawTarget.length > 0) {
            target = { name: rawTarget };
        }
        else if (rawTarget instanceof this.Pid) {
            target = { pid: rawTarget.bytes };
        }
        else {
            return { ok: false, error: err$1("bridge:invalid-target", {}) };
        }
        const tracked = [];
        const command = serializeSendPayload(target, payload ?? {}, this.handleMapper(tracked));
        if (!command.ok) {
            return command;
        }
        for (const { key, value, cleanup } of tracked) {
            this.trackedValues.set(key, { value, cleanup });
        }
        const requestId = this.nextRequestId();
        const timeoutMs = { ...DEFAULT_TIMEOUTS_MS, ...this.opts.timeoutsMs }.send;
        return await new Promise((resolve) => {
            const timer = setTimeout(() => {
                const wasMessageStale = this.pendingSends.delete(requestId);
                if (wasMessageStale) {
                    resolve({ ok: false, error: err$1("timeout:send", { timeoutMs }) });
                }
            }, timeoutMs);
            this.pendingSends.set(requestId, (result) => {
                clearTimeout(timer);
                resolve(result);
            });
            toVm(this.vmWorker, {
                type: "popcorn:send",
                payload: { id: requestId, message: command.data },
            }, [command.data.etf.buffer]);
        });
    }
    /**
     * Receives BEAM messages delivered while this handler is registered.
     * Messages with no handlers are dropped. A handler registered before boot
     * can run before the boot promise resolves.
     */
    onEvent(handler) {
        this.eventHandlers.add(handler);
        return () => {
            this.eventHandlers.delete(handler);
        };
    }
    deinit(reason = { reason: "deinit" }) {
        if (this.state.status === "closed") {
            return;
        }
        const error = err$1("vm:exited", reason);
        if (this.settleBoot !== null) {
            this.settleBoot({ ok: false, error });
            return;
        }
        this.state = { status: "closed", error };
        this.vmReady = false;
        for (const resolve of this.pendingSends.values()) {
            resolve({ ok: false, error });
        }
        this.pendingSends.clear();
        for (const pending of this.pendingCalls.values()) {
            pending.settle({ ok: false, error });
        }
        this.pendingCalls.clear();
        this.clearTrackedValues();
        this.vmWorker.removeEventListener("message", this.onWorkerMessage);
        this.vmWorker.terminate();
        // we keep onEvent() callbacks across reboots
    }
    clearTrackedValues() {
        for (const entry of this.trackedValues.values()) {
            try {
                entry.cleanup?.();
            }
            catch { }
        }
        this.trackedValues.clear();
    }
    emit(event) {
        const popcorn = objectWithKeys(event, ["_popcorn"])?._popcorn;
        const envelope = objectWithKeys(popcorn, ["t", "id", "payload"]);
        if (envelope !== null) {
            check(envelope.t === "proxy");
            this.completeCall(envelope.id, envelope.payload);
            return;
        }
        if (this.eventHandlers.size === 0) {
            console.warn(`${LOG_PREFIX} Dropped message with no event handlers`, event);
        }
        for (const handler of this.eventHandlers) {
            handler(event);
        }
    }
    completeCall(id, payload) {
        const pending = this.pendingCalls.get(id);
        const lateReply = pending === undefined;
        if (lateReply)
            return;
        this.pendingCalls.delete(id);
        pending.settle(this.parseCallReply(pending, payload));
    }
    parseCallReply(pending, payload) {
        const reply = payload;
        if (reply.ok)
            return { ok: true, data: reply.value };
        switch (reply.error.kind) {
            case "noproc": {
                const rawTarget = pending.target;
                const isName = typeof rawTarget === "string";
                const target = isName ? rawTarget : "<pid>";
                return {
                    ok: false,
                    error: err$1("genserver:noproc", { target }),
                };
            }
            case "exit":
                return {
                    ok: false,
                    error: err$1("genserver:exit", { reason: reply.error.reason }),
                };
            case "unserializable":
                return { ok: false, error: err$1("genserver:unserializable", {}) };
            case "timeout":
                return {
                    ok: false,
                    error: err$1("timeout:call", { timeoutMs: pending.timeoutMs }),
                };
            default:
                unreachable();
        }
    }
    async call(rawTarget, request, opts) {
        if (this.state.status !== "booted") {
            if (this.state.status === "closed") {
                return { ok: false, error: this.state.error };
            }
            return { ok: false, error: err$1("bridge:not-started", {}) };
        }
        return await this.callBridge(rawTarget, request, opts);
    }
    async callBridge(rawTarget, request, opts) {
        const timeoutMs = opts?.timeoutMs ?? DEFAULT_CALL_TIMEOUT_MS;
        const proxy = opts?.proxy ?? DEFAULT_PROXY_NAME;
        const id = this.nextCallId();
        const result = new Promise((resolve) => {
            const timer = setTimeout(() => {
                const isUnresolved = this.pendingCalls.delete(id);
                if (isUnresolved) {
                    resolve({ ok: false, error: err$1("timeout:call", { timeoutMs }) });
                }
            }, timeoutMs);
            this.pendingCalls.set(id, {
                target: rawTarget,
                timeoutMs,
                settle: (settled) => {
                    clearTimeout(timer);
                    resolve(settled);
                },
            });
        });
        const sent = await this.sendBridge(proxy, {
            kind: "call",
            id,
            target: rawTarget,
            request: request,
            timeout_ms: timeoutMs,
        });
        if (!sent.ok) {
            const pending = this.pendingCalls.get(id);
            this.pendingCalls.delete(id);
            pending?.settle({ ok: false, error: sent.error });
        }
        return result;
    }
    async cast(rawTarget, request, opts) {
        if (this.state.status !== "booted") {
            if (this.state.status === "closed") {
                return { ok: false, error: this.state.error };
            }
            return { ok: false, error: err$1("bridge:not-started", {}) };
        }
        return await this.castBridge(rawTarget, request, opts);
    }
    async castBridge(rawTarget, request, opts) {
        const proxy = opts?.proxy ?? DEFAULT_PROXY_NAME;
        return await this.sendBridge(proxy, {
            kind: "cast",
            target: rawTarget,
            request: request,
        });
    }
    nextCallId() {
        this.callSeq += 1;
        return `call:${this.callSeq}`;
    }
    async runJs(request) {
        let payload;
        try {
            const fn = this.jsWithCurrentEnv(request.code);
            assertRunJsFn(fn);
            const args = this.reviveHandles(request.args);
            check(this.vmReady);
            const actions = {
                send: (target, payload) => this.sendBridge(target, payload),
                call: (target, payload, opts) => this.callBridge(target, payload, opts),
                cast: (target, payload, opts) => this.castBridge(target, payload, opts),
            };
            const result = await fn(args, actions);
            const value = request.return === "ref" ? this.asRef(result) : result;
            payload = { ok: true, value: value ?? null };
        }
        catch (error) {
            check(error instanceof Error);
            payload = { ok: false, error: error.toString() };
        }
        const target = { pid: request.replyTo };
        const tracked = [];
        const command = serializeSendPayload(target, payload, this.handleMapper(tracked));
        if (command.ok) {
            for (const { key, value, cleanup } of tracked) {
                this.trackedValues.set(key, { value, cleanup });
            }
            this.sendRunJsReply(command.data);
            return;
        }
        const failure = serializeSendPayload(target, {
            ok: false,
            error: { unserializable: command.error.data.reason },
        });
        check(failure.ok);
        this.sendRunJsReply(failure.data);
    }
    asRef(value) {
        if (value instanceof this.TrackedValue)
            return value;
        return new this.TrackedValue(value);
    }
    sendRunJsReply(message) {
        toVm(this.vmWorker, { type: "popcorn:run-js-reply", payload: { message } }, [message.etf.buffer]);
    }
    jsWithCurrentEnv(code) {
        const make = new Function("TrackedValue", `"use strict"; return (${code});`);
        return make(this.TrackedValue);
    }
    reviveHandles(value) {
        const key = trackedRefKey(value);
        if (key !== null) {
            const entry = this.trackedValues.get(key);
            check(entry !== undefined);
            return entry.value;
        }
        const pidToken = pidRefToken(value);
        if (pidToken !== null) {
            return new this.Pid(base64ToBytes(pidToken));
        }
        if (Array.isArray(value)) {
            return value.map((item) => this.reviveHandles(item));
        }
        const obj = objectWithKeys(value, []);
        if (obj !== null) {
            const revived = {};
            for (const [k, v] of Object.entries(obj)) {
                revived[k] = this.reviveHandles(v);
            }
            return revived;
        }
        return value;
    }
    /** Maps pids and `TrackedValue`s during encoding, collecting handles into
     * `tracked` for the caller to register once encoding succeeds. */
    handleMapper(tracked) {
        return (value) => {
            if (value instanceof this.Pid) {
                return RawTerm.fromExternal(value.bytes);
            }
            if (value instanceof this.TrackedValue) {
                const key = (this.trackedKeySeq += 1);
                tracked.push({ key, value: value.value, cleanup: value.cleanup });
                return { [TRACKED_REF_KEY]: key };
            }
            return value;
        };
    }
    deleteTrackedValue(key) {
        const entry = this.trackedValues.get(key);
        check(entry !== undefined);
        try {
            entry.cleanup?.();
        }
        finally {
            this.trackedValues.delete(key);
        }
    }
    completeSend(payload) {
        const resolve = this.pendingSends.get(payload.id) ?? null;
        const didTimeout = resolve === null;
        if (didTimeout)
            return;
        this.pendingSends.delete(payload.id);
        const result = payload.result;
        resolve(result.ok
            ? { ok: true, data: null }
            : { ok: false, error: PopcornError.deserialize(result.error) });
    }
    nextRequestId() {
        this.requestSeq += 1;
        return `send:${this.requestSeq}`;
    }
    handleStdout(chunk) {
        this.output.stdout(chunk);
    }
    handleStderr(chunk) {
        this.output.stderr(chunk);
    }
    handleOtpError(payload) {
        const onError = this.opts.onError ?? defaultOnError;
        onError(payload);
        check(this.state.status === "booting" || this.state.status === "booted");
        // if failed while booting, settle early
        const booting = this.state.status === "booting";
        if (booting) {
            check(this.settleBoot !== null);
            const error = err$1("vm:exited", exitReason(payload));
            this.settleBoot({ ok: false, error });
            return;
        }
        this.deinit(exitReason(payload));
    }
}
function schedulers(opts) {
    const { base, dirtyCpu, dirtyIo } = opts;
    check(base > 0);
    check(dirtyCpu > 0);
    check(dirtyIo > 0);
    return ["-S", base, "-SDcpu", dirtyCpu, "-SDio", dirtyIo].map(String);
}
function isValidTtySize({ columns, rows }) {
    const colInRange = 0 < columns && columns <= 0xffff;
    const rowInRange = 0 < rows && rows <= 0xffff;
    return colInRange && rowInRange;
}
function resolveOutputHandlers(opts) {
    if (opts.tty?.output === "bytes") {
        const onStdout = opts.onStdout;
        const onStderr = opts.onStderr;
        return {
            stdout: onStdout ?? defaultOnStdoutBytes,
            stderr: onStderr ?? defaultOnStderrBytes,
        };
    }
    const stdoutDecoder = new TextDecoder();
    const stderrDecoder = new TextDecoder();
    const onStdout = opts.onStdout ?? defaultOnStdout;
    const onStderr = opts.onStderr ?? defaultOnStderr;
    return {
        stdout: (chunk) => decodeOutput(stdoutDecoder, onStdout, chunk),
        stderr: (chunk) => decodeOutput(stderrDecoder, onStderr, chunk),
    };
}
function decodeOutput(decoder, onOutput, chunk) {
    const output = decoder.decode(chunk, { stream: true });
    if (output.length > 0)
        onOutput(output);
}
function createIoState() {
    return {
        stdin: {
            reservedBytes: 0,
        },
    };
}
function toBytes(chunk) {
    return typeof chunk === "string" ? UTF8.encode(chunk) : chunk.slice();
}
function exitReason(payload) {
    switch (payload.kind) {
        case "abort":
            return { reason: "abort", data: payload.data };
        case "error":
            return { reason: "error", data: payload.data };
        case "exit":
            return { reason: "exit", data: payload.data };
        default:
            return unreachable();
    }
}
function trackedRefKey(value) {
    const marker = objectWithKeys(value, [TRACKED_REF_KEY]);
    const hasOnlyMarker = marker !== null && Object.keys(marker).length === 1;
    if (!hasOnlyMarker) {
        return null;
    }
    const key = marker[TRACKED_REF_KEY];
    check(typeof key === "number");
    return key;
}
function pidRefToken(value) {
    const marker = objectWithKeys(value, [PID_REF_KEY]);
    const hasOnlyMarker = marker !== null && Object.keys(marker).length === 1;
    if (!hasOnlyMarker) {
        return null;
    }
    const token = marker[PID_REF_KEY];
    check(typeof token === "string");
    return token;
}
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval#direct_and_indirect_eval
function indirectEval(code) {
    return (0, eval)(code);
}
function canEval() {
    try {
        indirectEval("0");
        return true;
    }
    catch {
        return false;
    }
}
function defaultOnStdout(chunk) {
    console.log(`${LOG_PREFIX} stdout:`, chunk);
}
function defaultOnStderr(chunk) {
    console.error(`${LOG_PREFIX} stderr:`, chunk);
}
function defaultOnStdoutBytes(chunk) {
    console.log(`${LOG_PREFIX} stdout:`, chunk);
}
function defaultOnStderrBytes(chunk) {
    console.error(`${LOG_PREFIX} stderr:`, chunk);
}
function defaultOnError(payload) {
    switch (payload.kind) {
        case "abort":
            console.error(`${LOG_PREFIX} abort:`, payload.data);
            return;
        case "error":
            console.error(`${LOG_PREFIX} error:`, payload.data);
            return;
        case "exit":
            console.info(`${LOG_PREFIX} exit:`, payload.data);
            return;
        default:
            unreachable();
    }
}

export { Popcorn, PopcornError, a, atom, schedulers, t, tuple };
