import { mkdir, cp, rm } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { p as popcorn$1, c as copyRuntime } from './shared.mjs';
import 'node:child_process';
import 'node:os';
import 'node:util';
import 'node:url';
import 'node:zlib';

function popcorn(options) {
    let outputDir;
    return {
        name: "popcorn-otp",
        setup(build) {
            build.onStart(() => {
                const opts = build.initialOptions;
                const outdir = opts.outdir ?? (opts.outfile === undefined ? undefined : dirname(opts.outfile));
                assert(opts.format === "esm", "Popcorn OTP works only with esm builds.");
                assert(outdir !== undefined, "outdir is not specified, cannot copy files");
                outputDir = resolve(outdir);
            });
            build.onEnd(async (result) => {
                if (result.errors.length > 0)
                    return;
                assert(outputDir !== undefined, "outdir was not resolved");
                const outDir = outputDir;
                const prepared = await popcorn$1(options);
                try {
                    await mkdir(outDir, { recursive: true });
                    await Promise.all([
                        copyRuntime(outDir),
                        cp(resolve(prepared.dir, "otp"), resolve(outDir, "otp"), {
                            recursive: true,
                        }),
                    ]);
                }
                finally {
                    await rm(prepared.dir, { recursive: true, force: true });
                }
            });
        },
    };
}
function assert(ok, message) {
    if (!ok) {
        throw new Error(`[popcorn-otp] ${message}`);
    }
}

export { popcorn };
