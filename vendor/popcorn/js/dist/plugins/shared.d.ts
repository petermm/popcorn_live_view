export type Options = {
    rootDir: string;
    app: string | null;
    extraApps?: string[];
    brotli?: boolean;
    strip?: boolean;
};
export type Prepared = {
    dir: string;
    notes: unknown[];
};
export declare function popcorn(opts: Options): Promise<Prepared>;
export declare function copyRuntime(targetDir: string): Promise<void>;
