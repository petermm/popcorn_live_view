import { type Result } from "./errors";
import type { BeamBootOptions, BeamSendPayload } from "./types";
export type Beam = {
    boot: Promise<Result<null>>;
    vmReady: Promise<void>;
    send: (message: BeamSendPayload) => Result<null>;
    writeStdin: (chunk: Uint8Array) => void;
    resizeTty: (columns: number, rows: number) => void;
};
export declare function start(options: BeamBootOptions): Beam;
