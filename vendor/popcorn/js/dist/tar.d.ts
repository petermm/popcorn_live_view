type OnDir = (path: string) => void;
type OnFile = (path: string, data: Uint8Array<ArrayBuffer>) => void;
export declare function extractTar(data: Uint8Array, onDir: OnDir, onFile: OnFile): void;
export {};
