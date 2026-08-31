import { execFile } from 'node:child_process';
import { mkdtemp, rm, mkdir, writeFile, copyFile, readFile } from 'node:fs/promises';
import { dirname, resolve, normalize, basename } from 'node:path';
import { tmpdir } from 'node:os';
import { promisify } from 'node:util';
import { fileURLToPath } from 'node:url';
import { brotliCompress, gzip, constants } from 'node:zlib';

const execFileAsync = promisify(execFile);
const brotliCompressAsync = promisify(brotliCompress);
const gzipAsync = promisify(gzip);
async function popcorn(opts) {
    const useBrotli = opts.brotli ?? false;
    const strip = opts.strip ?? true;
    const assetVariants = [
        "uncompressed",
        "gzip",
        useBrotli && "brotli",
    ];
    const distDir = p `${dirname(fileURLToPath(import.meta.url))}/..`;
    const preparedDir = await mkdtemp(p `${tmpdir()}/popcorn-otp-`);
    try {
        const report = await withTmp(async (packedDir) => {
            const report = await packTarballs({
                rootDir: resolve(opts.rootDir),
                outDir: packedDir,
                manifestPath: p `${distDir}/otp/manifest.json`,
                app: opts.app,
                extraApps: opts.extraApps ?? [],
                strip,
            });
            if (!report.ok) {
                throw new Error(`[popcorn-otp] ${formatPackError(report.error)}`);
            }
            await Promise.all([
                copy(report.manifestPath, p `${preparedDir}/otp/manifest.json`),
                copy(report.bootPath, p `${preparedDir}/otp/bin/vm.boot`),
                copy(report.tarPaths, p `${preparedDir}/otp/lib`, {
                    variants: assetVariants,
                }),
            ]);
            return report;
        });
        return {
            dir: preparedDir,
            notes: report.notes ?? [],
        };
    }
    catch (error) {
        await rm(preparedDir, { recursive: true, force: true });
        throw error;
    }
}
async function copyRuntime(targetDir) {
    const distDir = p `${dirname(fileURLToPath(import.meta.url))}/..`;
    await Promise.all(["worker.mjs", "beam.mjs", "beam.emu.mjs", "beam.wasm"].map((file) => copy(p `${distDir}/${file}`, p `${targetDir}/${file}`)));
}
async function packTarballs(opts) {
    const { rootDir, outDir, manifestPath, app, extraApps, strip } = opts;
    const toolDir = p `${dirname(fileURLToPath(import.meta.url))}/beam_tools`;
    const packerArgs = [
        "run",
        "--no-start",
        "-e",
        "Popcorn.BeamTools.CLI.main(System.argv())",
        "--",
        "--root-dir",
        rootDir,
        "--out-dir",
        outDir,
        "--manifest-path",
        manifestPath,
    ];
    if (app !== null) {
        packerArgs.push("--entrypoint-app", app);
    }
    for (const extraApp of extraApps) {
        packerArgs.push("--extra-app", extraApp);
    }
    if (strip) {
        packerArgs.push("--strip");
    }
    const env = {
        ...process.env,
        MIX_BUILD_PATH: p `${outDir}/beam_tools_build`,
        MIX_QUIET: "1",
    };
    const { stdout } = await execFileAsync("mix", packerArgs, {
        cwd: toolDir,
        env,
    });
    return JSON.parse(stdout);
}
function hasCode(error, code) {
    return (typeof error === "object" &&
        error !== null &&
        error.code === code);
}
function isMissingDepError(error) {
    return hasCode(error, "missing_dep");
}
function isUnsupportedAppsError(error) {
    return hasCode(error, "unsupported_apps");
}
function isMissingExtraAppsError(error) {
    return hasCode(error, "missing_extra_apps");
}
function toolchainOf(error) {
    if (typeof error !== "object" || error === null) {
        return undefined;
    }
    return error.toolchain;
}
function errorLines(error) {
    if (isMissingDepError(error)) {
        return [
            `${error.app} depends on ${error.dep}, which isn't available.`,
            `BEAM applications come from your project build and your active`,
            `Erlang/Elixir installation; nothing is bundled with the package.`,
            `Apps built by your project: ${error.available_apps.join(", ")}.`,
        ];
    }
    if (isUnsupportedAppsError(error)) {
        const apps = error.apps
            .map(({ app, capability }) => `${app} (needs ${capability})`)
            .join(", ");
        return [
            `These applications need native support the Wasm runtime wasn't built`,
            `with: ${apps}.`,
            `Drop them from your dependencies, or use a runtime built with it.`,
        ];
    }
    if (isMissingExtraAppsError(error)) {
        return [
            `Extra apps not found: ${error.apps.join(", ")}.`,
            `They have to come from your project build or your Erlang/Elixir install.`,
        ];
    }
    return [`packaging failed: ${JSON.stringify(error)}`];
}
function formatPackError(error) {
    const lines = errorLines(error);
    const toolchain = toolchainOf(error);
    if (toolchain !== undefined) {
        lines.push(`Using ${toolchain.executable} (Erlang/OTP ${toolchain.otp}, Elixir ${toolchain.elixir}).`);
    }
    return lines.join("\n  ");
}
async function copy(source, target, { variants = ["uncompressed"] } = {}) {
    const sources = typeof source === "string" ? [source] : source;
    const targetIsDir = typeof source !== "string";
    await Promise.all(sources.map(async (sourcePath) => {
        const targetPath = targetIsDir
            ? p `${target}/${basename(sourcePath)}`
            : target;
        await mkdir(dirname(targetPath), { recursive: true });
        let content;
        const read = () => (content ??= readFile(sourcePath));
        await Promise.all(variants
            .filter((variant) => Boolean(variant))
            .map(async (variant) => {
            switch (variant) {
                case "uncompressed":
                    await copyFile(sourcePath, targetPath);
                    break;
                case "gzip": {
                    const input = await read();
                    const buffer = await gzipAsync(input, { level: 9 });
                    await writeFile(`${targetPath}.gz`, buffer);
                    break;
                }
                case "brotli": {
                    const Q = constants.BROTLI_PARAM_QUALITY;
                    const opts = { params: { [Q]: 11 } };
                    const input = await read();
                    const buffer = await brotliCompressAsync(input, opts);
                    await writeFile(`${targetPath}.br`, buffer);
                    break;
                }
            }
        }));
    }));
}
function p(strings, ...values) {
    return normalize(String.raw(strings, ...values));
}
async function withTmp(f) {
    const dir = await mkdtemp(p `${tmpdir()}/popcorn-otp-`);
    try {
        return await f(dir);
    }
    finally {
        await rm(dir, { recursive: true, force: true });
    }
}

export { copyRuntime as c, popcorn as p };
