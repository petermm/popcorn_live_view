import createModule from './beam.mjs';

function err(t, data) {
    return new PopcornError({ t, data });
}
function isErr(error, t) {
    const isInstance = error instanceof PopcornError;
    if (!isInstance)
        return false;
    return true;
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
    throw err("internal:unreachable", {});
}
function check$1(ok, msg) {
    if (!ok) {
        throw err("internal:check", { detail: msg });
    }
}

function dirname(path) {
    const idx = path.lastIndexOf("/");
    if (idx <= 0)
        return "/";
    return path.slice(0, idx);
}
async function fetchBinary(url) {
    const response = await fetch(url);
    if (response.ok !== true)
        return null;
    return new Uint8Array(await response.arrayBuffer());
}
async function fetchJson(url) {
    const response = await fetch(url);
    if (response.ok !== true)
        return null;
    try {
        return await response.json();
    }
    catch {
        return null;
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
        throw err("internal:check", { detail: msg });
}
function unreachable() {
    throw err("internal:unreachable", {});
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

new TextEncoder();

function readMainEvent(value) {
    const data = objectWithKeys(value, ["type", "payload"]);
    check(data !== null && typeof data.type === "string");
    switch (data.type) {
        case "popcorn:boot":
        case "popcorn:stdin":
        case "popcorn:tty-resize":
        case "popcorn:send":
        case "popcorn:run-js-reply":
            return data;
        default:
            unreachable();
    }
}
function deserializeBridgeMessage(text) {
    try {
        const parsed = JSON.parse(text);
        if (!isBridgeEnvelope(parsed))
            return null;
        switch (parsed.type) {
            case "vm_message":
                return { type: "otp:message", payload: parsed.data };
            case "vm_error":
                return {
                    type: "otp:error",
                    payload: { kind: "error", data: parsed.data },
                };
            case "run_js":
                return {
                    type: "otp:run_js",
                    payload: {
                        code: parsed.code,
                        args: parsed.args,
                        replyTo: base64ToBytes(parsed.reply_to),
                        return: parsed.return,
                    },
                };
            default:
                return null;
        }
    }
    catch {
        return null;
    }
}
/** Usable only from webworkers. */
function toMain(event) {
    self.postMessage(event, { transfer: getTransferables(event) });
}
function getTransferables(event) {
    const isTtyEvent = event.type === "otp:stdout" || event.type === "otp:stderr";
    if (!isTtyEvent)
        return [];
    check(event.payload.buffer instanceof ArrayBuffer);
    return [event.payload.buffer];
}
function isBridgeEnvelope(value) {
    const KNOWN_MESSAGE_TYPES = ["vm_message", "vm_error", "run_js"];
    const data = objectWithKeys(value, ["type"]);
    return data !== null && KNOWN_MESSAGE_TYPES.includes(data.type);
}

// Tar format constants.
const T = {
    /// entire block size (bytes)
    BLK_N: 512,
    /// filename field offset
    NAME_OFF: 0,
    /// name field size (bytes)
    NAME_N: 100,
    /// file size field offset
    SIZE_OFF: 124,
    /// file size field size (bytes). Stored as ASCII octal string.
    SIZE_N: 12,
    /// entry type field offset, 1 byte.
    TYPEFLAG_OFF: 156,
    /// prefix field offset, 1 byte. Used when name is longer than `name` field.
    PREFIX_OFF: 345,
    /// prefix field size (bytes)
    PREFIX_N: 155,
    /// entry type=dir, '5' in ASCII
    TYPE_DIR: 53,
};
function extractTar(data, onDir, onFile) {
    check(data.length % T.BLK_N === 0, "tar:bad_chunk");
    const decoder = new TextDecoder();
    let offset = 0;
    while (offset + T.BLK_N <= data.length) {
        const header = data.slice(offset, offset + T.BLK_N);
        if (isZeroBlock(header))
            break;
        const name = readString(decoder, header, T.NAME_OFF, T.NAME_N);
        const prefix = readString(decoder, header, T.PREFIX_OFF, T.PREFIX_N);
        const fullName = prefix ? `${prefix}/${name}` : name;
        const size = parseOctal(readString(decoder, header, T.SIZE_OFF, T.SIZE_N));
        const type = header[T.TYPEFLAG_OFF];
        offset += T.BLK_N;
        const path = fullName.startsWith("/") ? fullName : `/${fullName}`;
        if (type === T.TYPE_DIR) {
            onDir(path);
        }
        else if (fullName) {
            const contents = data.slice(offset, offset + size);
            onFile(path, contents);
        }
        offset += Math.ceil(size / T.BLK_N) * T.BLK_N;
    }
}
function isZeroBlock(block) {
    for (let i = 0; i < block.length; i++) {
        if (block[i] !== 0)
            return false;
    }
    return true;
}
function readString(decoder, data, start, length) {
    check(length > 0, "tar:bad_string");
    let end = start;
    const max = start + length;
    while (end < max && data[end] !== 0)
        end++;
    if (end === start)
        return "";
    return decoder.decode(data.slice(start, end));
}
function parseOctal(value) {
    const parsed = parseInt(value, 8);
    check(!Number.isNaN(parsed), "tar:bad_octal");
    return parsed;
}

const DEFAULT_USER = "web_user";
const DEFAULT_HOME_DIR = "/home/web_user";
const FS_DIRS = ["/bin", "/lib", "/etc", "/tmp", "/home", DEFAULT_HOME_DIR];
const BOOT_NAME = "vm";
const BOOT_PATH = `/bin/${BOOT_NAME}.boot`;
const MANIFEST_NAME = "manifest.json";
const ENTRYPOINT_READY_EXPR = 'wasm:send(#{<<"_popcorn">> => #{<<"t">> => <<"boot_ready">>}})';
const ENTRYPOINT_FAILED_EXPR = 'wasm:send(#{<<"_popcorn">> => #{<<"t">> => <<"boot_failed">>}})';
// https://www.erlang.org/doc/apps/erts/inet_cfg.html
const INETRC_PATH = "/etc/inetrc";
// lookup types: `native | file | dns`
// We need `file` lookup to avoid spawning
// /bin/inet_gethost which is not available
const INETRC = "{lookup, [file]}.\n";
const STDOUT_FD = 1;
const UTF8 = new TextEncoder();
const BASE_ARGS = [
    "-root",
    "/",
    "-bindir",
    "/bin",
    "-progname",
    "erl",
    "-home",
    DEFAULT_HOME_DIR,
    "-kernel",
    "start_distribution",
    "false",
];
const CORE_APPS = new Set(["kernel", "stdlib", "compiler"]);
function start(options) {
    const state = { module: null, isVmReady: false };
    const vm = trackVmReady(state);
    return {
        boot: boot(options, state, vm),
        vmReady: vm.vmReady,
        send: (message) => send(state.isVmReady ? state.module : null, message),
        writeStdin: (chunk) => writeStdin(state.module, chunk),
        resizeTty: (columns, rows) => resizeTty(state.module, columns, rows),
    };
}
async function boot(opts, state, vm) {
    const { otpAssetsRoot, emulatorArgs, extraArgs, env, ttySize, createModule, emit, } = opts;
    const loadedFsData = await loadFsData(otpAssetsRoot);
    if (!loadedFsData.ok) {
        return { ok: false, error: loadedFsData.error };
    }
    const fsData = loadedFsData.data;
    const { vmReady, handleVmReady } = vm;
    const { appReady, handleAppReady } = trackAppReady(fsData.entrypoint);
    const runtimeEnv = {
        ...env,
        BINDIR: "/bin",
        EMU: "beam",
        HOME: DEFAULT_HOME_DIR,
        USER: DEFAULT_USER,
        LOGNAME: DEFAULT_USER,
        COLUMNS: String(ttySize.columns),
        LINES: String(ttySize.rows),
        ERL_INETRC: INETRC_PATH,
    };
    const moduleConfig = {
        print: (text) => emit({ type: "otp:stdout", payload: UTF8.encode(text) }),
        printErr: (text) => emit({ type: "otp:stderr", payload: UTF8.encode(text) }),
        onExit: (code) => emit({ type: "otp:error", payload: { kind: "exit", data: code } }),
        onAbort: (text) => emit({ type: "otp:error", payload: { kind: "abort", data: text } }),
        onBeamMessage: (text) => {
            const event = deserializeBridgeMessage(text);
            if (event === null)
                return;
            if (handleVmReady(event))
                return;
            if (handleAppReady(event))
                return;
            emit(event);
        },
        onError: (text) => emit({ type: "otp:error", payload: { kind: "error", data: text } }),
        onStdinConsumed: (size) => emit({ type: "otp:stdin-consumed", payload: size }),
        onTrackedValueDelete: (key) => emit({ type: "otp:tracked-value-delete", payload: key }),
        onTtyChunk: (fd, chunk) => emit({
            type: fd === STDOUT_FD ? "otp:stdout" : "otp:stderr",
            payload: chunk,
        }),
        arguments: buildArgs({
            appNames: fsData.appNames,
            entrypoint: fsData.entrypoint,
            emulator: emulatorArgs ?? [],
            extra: extraArgs ?? [],
        }),
        preRun: [
            (mod) => {
                state.module = mod;
            },
            (mod) => {
                Object.assign(mod.ENV, runtimeEnv);
                initFs({ module: mod, fsData });
            },
        ],
    };
    try {
        const ready = Promise.all([vmReady, appReady]);
        const module = await createModule(moduleConfig);
        check(state.module === module);
        await ready;
        return { ok: true, data: null };
    }
    catch (error) {
        return { ok: false, error: toPopcornError(error) };
    }
}
function trackVmReady(state) {
    let resolve = () => { };
    const vmReady = new Promise((r) => {
        resolve = r;
    });
    const handleVmReady = (event) => {
        if (!isBridgeMarker(event, "vm_ready"))
            return false;
        state.isVmReady = true;
        resolve();
        return true;
    };
    return { vmReady, handleVmReady };
}
function trackAppReady(entrypoint) {
    let resolve = () => { };
    let reject = (_error) => { };
    let appReady = Promise.resolve();
    if (entrypoint !== null) {
        appReady = new Promise((res, rej) => {
            resolve = res;
            reject = rej;
        });
    }
    const handleAppReady = (event) => {
        if (isBridgeMarker(event, "boot_ready")) {
            resolve();
            return true;
        }
        if (isBridgeMarker(event, "boot_failed")) {
            reject(err("vm:exited", { reason: "exit", data: 1 }));
            return true;
        }
        return false;
    };
    return { appReady, handleAppReady };
}
function toPopcornError(error) {
    if (isErr(error))
        return error;
    const message = error instanceof Error ? error.message : String(error);
    return err("worker:load", { message });
}
function buildArgs({ appNames, entrypoint, emulator, extra, }) {
    const args = [...emulator, "--", ...BASE_ARGS, "-boot", BOOT_NAME];
    for (const app of CORE_APPS) {
        args.push("-pa", `/lib/${app}/ebin`);
    }
    for (const app of appNames) {
        if (CORE_APPS.has(app))
            continue;
        args.push("-pa", `/lib/${app}/ebin`);
    }
    if (entrypoint !== null) {
        args.push("-eval", `case application:ensure_all_started(${entrypoint}) of {ok, _} -> ${ENTRYPOINT_READY_EXPR}; _ -> ${ENTRYPOINT_FAILED_EXPR}, erlang:halt(1) end.`);
    }
    for (const arg of extra) {
        args.push(arg);
    }
    return args;
}
function isBridgeMarker(event, type) {
    if (event === null || event.type !== "otp:message")
        return false;
    const popcorn = objectWithKeys(event.payload, ["_popcorn"])?._popcorn;
    return objectWithKeys(popcorn, ["t"])?.t === type;
}
async function loadFsData(assetsRoot) {
    const manifestUrl = resolveAssetsPath(assetsRoot, MANIFEST_NAME);
    const manifest = await fetchJson(manifestUrl);
    if (manifest === null) {
        return {
            ok: false,
            error: err("beam:missing-manifest", { url: manifestUrl }),
        };
    }
    const appNames = Object.keys(manifest.apps);
    for (const name of CORE_APPS) {
        if (!Object.hasOwn(manifest.apps, name)) {
            return {
                ok: false,
                error: err("beam:missing-tarball", { name, all: appNames }),
            };
        }
    }
    const bootUrl = resolveAssetsPath(assetsRoot, manifest.vm.boot);
    const bootFile = await fetchBinary(bootUrl);
    if (bootFile === null) {
        return {
            ok: false,
            error: err("beam:missing-boot-script", { url: bootUrl }),
        };
    }
    const loadedTarballs = await Promise.all(appNames.map(async (name) => {
        const entry = manifest.apps[name];
        const tarUrl = resolveAssetsPath(assetsRoot, entry.tar);
        const tar = await fetchBinary(tarUrl);
        if (tar === null) {
            return {
                ok: false,
                error: err("beam:missing-tarball", { name, all: appNames }),
            };
        }
        return { ok: true, data: tar };
    }));
    const tarballs = [];
    for (const tarball of loadedTarballs) {
        if (!tarball.ok) {
            return { ok: false, error: tarball.error };
        }
        tarballs.push(tarball.data);
    }
    return {
        ok: true,
        data: {
            appNames,
            entrypoint: manifest.entrypoint ?? null,
            bootFile,
            tarballs,
        },
    };
}
function initFs({ module, fsData }) {
    const writeFile = (path, content) => {
        module.FS_createDataFile(path, null, content, true, true, true);
    };
    for (const dir of FS_DIRS) {
        module.FS_mkdirTree(dir);
    }
    writeFile(BOOT_PATH, fsData.bootFile);
    writeFile(INETRC_PATH, UTF8.encode(INETRC));
    const createDir = (dirPath) => {
        module.FS_mkdirTree(dirPath);
    };
    const createFile = (path, content) => {
        module.FS_mkdirTree(dirname(path));
        writeFile(path, content);
    };
    for (const tarball of fsData.tarballs) {
        extractTar(tarball, createDir, createFile);
    }
}
function resolveAssetsPath(assetsRoot, relativePath) {
    check(assetsRoot.endsWith("/"));
    if (relativePath.startsWith("/") || isAbsoluteUrl(relativePath)) {
        return relativePath;
    }
    const url = new URL(relativePath, new URL(assetsRoot, self.location.href));
    if (assetsRoot.startsWith("/")) {
        return url.pathname;
    }
    return url.toString();
}
function isAbsoluteUrl(path) {
    return /^[a-zA-Z][a-zA-Z\d+\-.]*:/.test(path);
}
function send(module, message) {
    if (module === null) {
        return { ok: false, error: err("bridge:not-started", {}) };
    }
    let target;
    if (isNameTarget(message.target)) {
        const targetName = message.target.name;
        target = {
            kind: TARGET_REGISTERED_NAME,
            argType: "string",
            value: targetName,
            length: utf8Length(targetName),
        };
    }
    else {
        const bytes = message.target.pid;
        target = {
            kind: TARGET_PID_BYTES,
            argType: "array",
            value: bytes,
            length: bytes.length,
        };
    }
    const status = module.ccall("sendVmMessage", "number", ["number", target.argType, "number", "array", "number"], [
        target.kind,
        target.value,
        target.length,
        message.etf,
        message.etf.byteLength,
    ]);
    if (status === 0) {
        return { ok: true, data: null };
    }
    if (status === 1) {
        const t = isNameTarget(message.target) ? message.target.name : "<pid>";
        return {
            ok: false,
            error: err("bridge:listener-not-found", { targetName: t }),
        };
    }
    if (status === 2) {
        return {
            ok: false,
            error: err("bridge:unserializable", {
                data: null,
                part: null,
                reason: "unsupported",
            }),
        };
    }
    unreachable();
}
function writeStdin(module, chunk) {
    check(module !== null);
    const status = module.ccall("popcornStdinEnqueue", "number", ["array", "number"], [chunk, chunk.byteLength]);
    check(status === 0);
}
function resizeTty(module, columns, rows) {
    check(module !== null);
    const status = module.ccall("popcornTtyResize", "number", ["number", "number"], [columns, rows]);
    check(status === 0);
}
const TARGET_REGISTERED_NAME = 0;
const TARGET_PID_BYTES = 1;
function isNameTarget(target) {
    return Object.hasOwn(target, "name");
}
function utf8Length(text) {
    return UTF8.encode(text).length;
}

let instance = null;
self.onmessage = async (event) => {
    const data = readMainEvent(event.data);
    switch (data.type) {
        case "popcorn:boot": {
            check(instance === null);
            instance = start({
                otpAssetsRoot: data.payload.otpAssetsRoot ??
                    // The plugin generates this directory after Vite analyzes the worker.
                    new URL(/* @vite-ignore */ "./otp/", import.meta.url).href,
                emulatorArgs: data.payload.emulatorArgs,
                extraArgs: data.payload.extraArgs,
                env: data.payload.env,
                ttySize: data.payload.ttySize,
                createModule,
                emit: toMain,
            });
            void instance.vmReady.then(() => toMain({ type: "popcorn:boot-vm-ready", payload: {} }));
            const result = await instance.boot;
            if (!result.ok) {
                toMain({
                    type: "popcorn:boot-fail",
                    payload: result.error.serialize(),
                });
                return;
            }
            toMain({ type: "popcorn:boot-end", payload: {} });
            break;
        }
        case "popcorn:send": {
            check(instance !== null);
            const result = instance.send(data.payload.message);
            toMain({
                type: "popcorn:send-end",
                payload: {
                    id: data.payload.id,
                    result: result.ok
                        ? { ok: true, data: null }
                        : { ok: false, error: result.error.serialize() },
                },
            });
            break;
        }
        case "popcorn:run-js-reply": {
            // ignore the `send()` result, process could've died
            check(instance !== null);
            instance.send(data.payload.message);
            break;
        }
        case "popcorn:stdin": {
            check(instance !== null);
            instance.writeStdin(data.payload.chunk);
            break;
        }
        case "popcorn:tty-resize": {
            check(instance !== null);
            instance.resizeTty(data.payload.columns, data.payload.rows);
            break;
        }
        default:
            unreachable();
    }
};
