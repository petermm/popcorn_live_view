import { mkdir, cp, rm } from 'node:fs/promises';
import { resolve, dirname } from 'node:path';
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
        renderStart(outputOptions) {
            assert(outputOptions.format === "es", "Popcorn OTP works only with esm builds.");
            let dir = outputOptions.dir;
            if (dir === undefined && outputOptions.file !== undefined) {
                dir = dirname(outputOptions.file);
            }
            assert(dir !== undefined, "output dir is not specified, cannot copy files");
            outputDir = resolve(dir);
        },
        async writeBundle() {
            assert(outputDir !== undefined, "output dir was not resolved");
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
        },
    };
}
function assert(ok, message) {
    if (!ok) {
        throw new Error(`[popcorn-otp] ${message}`);
    }
}

export { popcorn };
