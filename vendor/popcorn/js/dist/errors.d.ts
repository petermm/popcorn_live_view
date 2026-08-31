export type Result<T, E extends Tag = Tag> = {
    ok: true;
    data: T;
} | {
    ok: false;
    error: PopcornError<E>;
};
type Tag = keyof PopcornErrors;
export type PopcornErrors = {
    "timeout:init": {
        timeoutMs: number;
    };
    "timeout:send": {
        timeoutMs: number;
    };
    "timeout:call": {
        timeoutMs: number;
    };
    "worker:load": {
        message: string;
    };
    "vm:exited": VmExitedData;
    "bridge:not-started": EmptyData;
    "bridge:invalid-target": EmptyData;
    "bridge:unserializable": UnserializableData;
    "bridge:listener-not-found": {
        targetName: string;
    };
    "genserver:noproc": {
        target: string;
    };
    "genserver:exit": {
        reason: string;
    };
    "genserver:unserializable": EmptyData;
    "stdio:overflow": {
        capacityBytes: number;
        attemptedBytes: number;
    };
    "beam:missing-boot-script": {
        url: string;
    };
    "beam:missing-manifest": {
        url: string;
    };
    "beam:missing-tarball": {
        name: string;
        all: string[];
    };
    "internal:check": {
        detail?: string;
    };
    "internal:unreachable": EmptyData;
    "runtime:eval-unavailable": EmptyData;
};
export type SerializedError<T extends Tag = Tag> = {
    [K in T]: {
        t: K;
        data: PopcornErrors[K];
    };
}[T];
type EmptyData = Record<never, never>;
export type UnserializableReason = "cyclic-object" | "non-plain-object" | "lossy-int" | "non-finite-float" | "unsupported";
type UnserializableData = {
    data: unknown;
    part: unknown;
    reason: UnserializableReason;
};
type VmExitedData = {
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
export declare function err<T extends Tag>(t: T, data: PopcornErrors[T]): PopcornError<T>;
export declare function isErr<T extends Tag = Tag>(error: unknown, t?: T): error is PopcornError<T>;
export declare class PopcornError<T extends Tag = Tag> extends Error {
    readonly cause: SerializedError<T>;
    private readonly serialized;
    constructor(cause: SerializedError<T>);
    get t(): T;
    get data(): PopcornErrors[T];
    serialize(): SerializedError<T>;
    static deserialize(value: unknown): PopcornError;
}
export declare function isUnserializableReason(value: unknown): value is UnserializableReason;
export {};
