import { type Result } from "./errors";
import { type PopcornEvent } from "./events";
import type { AnyValue, BeamBootOptions, OtpErrorPayload, Pid, TtySize } from "./types";
type TtyOutput = "text" | "bytes";
type OutputChunk<Output extends TtyOutput> = Output extends "bytes" ? Uint8Array : string;
export type PopcornOpts<Output extends TtyOutput = "text"> = {
    beam?: Pick<BeamBootOptions, "emulatorArgs" | "extraArgs" | "env"> & {
        otpAssetsRoot?: string;
    };
    tty?: {
        size?: TtySize;
        output?: Output;
    };
    timeoutsMs?: {
        boot?: number;
        appStartup?: number;
        send?: number;
    };
    onStdout?: (chunk: OutputChunk<Output>) => void;
    onStderr?: (chunk: OutputChunk<Output>) => void;
    onError?: (event: OtpErrorPayload) => void;
    workerUrl?: string | URL;
};
type VmExitReason = {
    reason: "deinit";
} | {
    reason: "abort";
    data: string;
} | {
    reason: "error";
    data: string;
} | {
    reason: "exit";
    data: number;
};
type CallOpts = {
    timeoutMs?: number;
    proxy?: string;
};
export type GenServer = {
    /**
     * Sends a `call` to the `target` GenServer (through the `proxy`), waiting for a response.
     *
     * ## Parameters
     *
     * - `target` — the GenServer to call, either a registered name or a `Pid`.
     * - `request` — the request payload.
     * - `opts` — call options.
     *
     * ### Options
     *
     * - `timeoutMs` — the maximum time to wait for a response, in milliseconds.
     * - `proxy` — the name of the `Popcorn.Proxy` process to use for the call.
     *
     * ## Returns
     *
     * A `Promise` that resolves with the server's reply, or rejects with an error.
     *
     * ## Errors
     *
     * TODO: gather errors
     *
     */
    call(target: string | Pid, request?: AnyValue, opts?: CallOpts): Promise<Result<AnyValue>>;
    /**
     * Sends a `cast` to the `target` GenServer (through the `proxy`), in fire-and-forget manner.
     *
     * ## Parameters
     *
     * - `target` — the GenServer to cast to, either a registered name or a `Pid`.
     * - `request` — the request payload.
     * - `opts` — cast options.
     *
     * ### Options
     *
     * - `proxy` — the registered name or `Pid` of the `Popcorn.Proxy` process to use for the cast.
     *
     * ## Returns
     *
     * A `Promise` that resolves once the message is delivered to the proxy.
     *
     * ## Errors
     *
     * TODO: gather errors
     *
     */
    cast(target: string | Pid, request?: AnyValue, opts?: {
        proxy?: string;
    }): Promise<Result<null>>;
};
export declare class Popcorn<Output extends TtyOutput = "text"> {
    private vmWorker;
    private state;
    private readonly opts;
    private readonly ttySize;
    private output;
    private requestSeq;
    private settleBoot;
    private readonly eventHandlers;
    private readonly pendingSends;
    private readonly pendingCalls;
    private callSeq;
    private readonly trackedValues;
    private trackedKeySeq;
    private io;
    private vmReady;
    readonly genserver: GenServer;
    private readonly TrackedValue;
    private Pid;
    private readonly onWorkerMessage;
    constructor(opts: PopcornOpts<Output>);
    private spawnWorker;
    static init<Output extends TtyOutput = "text">(opts: PopcornOpts<Output>): Promise<Result<Popcorn<Output>>>;
    /**
     * Starts the VM and resolves after its bridge is ready and the entrypoint
     * application has started. `timeoutsMs.boot` bounds the wait for the VM
     * bridge; `timeoutsMs.appStartup` bounds the entrypoint startup that
     * follows. Register event handlers before calling this
     * method when the application sends messages during startup. Processes
     * registered later by handle_continue or spawned work can still return
     * genserver:noproc immediately after boot.
     */
    boot(): Promise<Result<Popcorn<Output>>>;
    writeStdin(chunk: string | Uint8Array): Result<null>;
    resizeTty(columns: number, rows: number): Result<null>;
    /**
     * Resolves after VM sent message to registered process.
     */
    send(rawTarget: string | Pid, payload?: AnyValue): Promise<Result<null>>;
    private sendBridge;
    /**
     * Receives BEAM messages delivered while this handler is registered.
     * Messages with no handlers are dropped. A handler registered before boot
     * can run before the boot promise resolves.
     */
    onEvent(handler: (event: PopcornEvent) => void): () => void;
    deinit(reason?: VmExitReason): void;
    private clearTrackedValues;
    private emit;
    private completeCall;
    private parseCallReply;
    private call;
    private callBridge;
    private cast;
    private castBridge;
    private nextCallId;
    private runJs;
    private asRef;
    private sendRunJsReply;
    private jsWithCurrentEnv;
    private reviveHandles;
    /** Maps pids and `TrackedValue`s during encoding, collecting handles into
     * `tracked` for the caller to register once encoding succeeds. */
    private handleMapper;
    private deleteTrackedValue;
    private completeSend;
    private nextRequestId;
    private handleStdout;
    private handleStderr;
    private handleOtpError;
}
export type SchedulerOptions = {
    base: number;
    dirtyCpu: number;
    dirtyIo: number;
};
export declare function schedulers(opts: SchedulerOptions): string[];
export {};
