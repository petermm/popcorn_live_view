import { type Result, type SerializedError } from "./errors";
import { type Mapper } from "./etf";
import type { AnyValue, BeamBootOptions, BeamEvent, BeamSendPayload, BeamTarget } from "./types";
type BootEvent = {
    type: "popcorn:boot";
    payload: Pick<BeamBootOptions, "emulatorArgs" | "extraArgs" | "env" | "ttySize"> & {
        otpAssetsRoot?: string;
    };
};
type StdinEvent = {
    type: "popcorn:stdin";
    payload: {
        chunk: Uint8Array;
    };
};
type TtyResizeEvent = {
    type: "popcorn:tty-resize";
    payload: {
        columns: number;
        rows: number;
    };
};
type SendEvent = {
    type: "popcorn:send";
    payload: SendRequestPayload;
};
export type RunJsReplyPayload = {
    message: BeamSendPayload;
};
type RunJsReplyEvent = {
    type: "popcorn:run-js-reply";
    payload: RunJsReplyPayload;
};
export type SendRequestPayload = {
    id: string;
    message: BeamSendPayload;
};
export type SerializedSendResult = {
    ok: true;
    data: null;
} | {
    ok: false;
    error: SerializedError;
};
export type SendCompletionPayload = {
    id: string;
    result: SerializedSendResult;
};
type SendEndEvent = {
    type: "popcorn:send-end";
    payload: SendCompletionPayload;
};
type BootEndEvent = {
    type: "popcorn:boot-vm-ready";
    payload: {};
} | {
    type: "popcorn:boot-end";
    payload: {};
} | {
    type: "popcorn:boot-fail";
    payload: SerializedError;
};
export type MainToVmEvent = BootEvent | SendEvent | RunJsReplyEvent | StdinEvent | TtyResizeEvent;
export type PopcornEvent = AnyValue;
type RuntimeEvent = BeamEvent | SendEndEvent;
export type VmToMainEvent = RuntimeEvent | BootEndEvent;
export declare function readMainEvent(value: unknown): MainToVmEvent;
export declare function readWorkerEvent(value: unknown): VmToMainEvent;
export declare function serializeSendPayload(target: BeamTarget, payload: AnyValue, mapper?: Mapper): Result<BeamSendPayload, "bridge:unserializable">;
export declare function deserializeBridgeMessage(text: string): Extract<BeamEvent, {
    type: "otp:message" | "otp:error" | "otp:run_js";
}> | null;
/** Usable only from main context. */
export declare function toVm(worker: Worker, event: MainToVmEvent, transfer?: Transferable[]): void;
/** Usable only from webworkers. */
export declare function toMain(event: VmToMainEvent): void;
export {};
