/**
 * # Decompiler
 *
 * Decompile from JavaScript/TypeScript to Lisp.
 *
 * ## Description
 *
 * Simple decompiler implementation translating JavaScript or
 * TypeScript code to Lisp code.
 *
 * ### Comments
 *
 * Note that currently, there is no support for comments as the
 * parser we are using, `@typescript-eslint/parser`, throws them
 * away.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Decompile a JavaScript or TypeScript program.
 */
declare function decompile(x: any, options?: any): any;
/**
 * Read a JavaScript or TypeScript program from disk
 * and decompile it. The result is written to disk.
 */
declare function decompileFileX(file: any, options?: any): any;
/**
 * Read JavaScript or TypeScript programs from disk
 * and decompile them. The results are written to disk.
 */
declare function decompileFilesX(files: any, options?: any): any;
/**
 * Decompile a JavaScript or TypeScript module.
 */
declare function decompileModule(m: any, options?: any): any;
export { decompile, decompileFileX, decompileFilesX, decompileModule };
