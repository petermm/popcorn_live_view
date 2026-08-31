export declare function dirname(path: string): string;
export declare function fetchBinary(url: string): Promise<Uint8Array | null>;
export declare function fetchJson<T>(url: string): Promise<T | null>;
export declare function base64ToBytes(b64: string): Uint8Array;
export declare function check(ok: boolean, msg?: string): asserts ok;
export declare function unreachable(): never;
export declare function objectWithKeys<K extends string>(value: unknown, keys: K[]): null | Record<K, unknown>;
