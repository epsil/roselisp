"use strict";
// SPDX-License-Identifier: MPL-2.0
/**
 * # Command-line interface
 *
 * Simple command-line interface.
 *
 * ## Description
 *
 * This file defines and invokes a `main` function that reads input
 * from the command line and takes appropriate action. Options
 * parsing is done with [`minimist`][npm:minimist].
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [npm:minimist]: https://www.npmjs.com/package/minimist
 */
Object.defineProperty(exports, "__esModule", { value: true });
const minimist = require("minimist");
const buildOptions = require("minimist-options");
const decompiler_1 = require("./decompiler");
const language_1 = require("./language");
const repl_1 = require("./repl");
/**
 * Command line options. Passed to
 * [`minimist-options`][npm:minimist-options]
 * for parsing.
 *
 * [npm:minimist-options]: https://www.npmjs.com/package/minimist-options
 */
const cliOptions = {
    case: {
        default: 'camelcase',
        type: 'string'
    },
    comments: {
        default: true,
        type: 'boolean'
    },
    compile: {
        alias: 'c',
        default: false,
        type: 'boolean'
    },
    decompile: {
        alias: 'd',
        default: false,
        type: 'boolean'
    },
    eval: {
        alias: 'e',
        default: '',
        type: 'string'
    },
    fcommonjs: {
        default: false,
        type: 'boolean'
    },
    fesModuleInterop: {
        alias: 'fes-module-interop',
        default: false,
        type: 'boolean'
    },
    fevalBindings: {
        alias: 'feval-bindings',
        default: false,
        type: 'boolean'
    },
    finlineFunctions: {
        alias: 'finline-functions',
        default: false,
        type: 'boolean'
    },
    fdottedlists: {
        default: false,
        type: 'boolean'
    },
    fsemicolon: {
        default: true,
        type: 'boolean'
    },
    fstringobjects: {
        default: false,
        type: 'boolean'
    },
    from: {
        default: 'roselisp',
        type: 'string'
    },
    help: {
        alias: 'h',
        default: false,
        type: 'boolean'
    },
    indent: {
        default: 2,
        type: 'number'
    },
    language: {
        default: '',
        type: 'string'
    },
    optimize: {
        default: true,
        type: 'boolean'
    },
    outDir: {
        alias: 'out-dir',
        default: '.',
        type: 'string'
    },
    quick: {
        alias: 'q',
        default: false,
        type: 'boolean'
    },
    repl: {
        alias: 'i',
        default: false,
        type: 'boolean'
    },
    to: {
        default: 'javascript',
        type: 'string'
    }
};
/**
 * Help message. Displayed when the program
 * is invoked with `-h` or `--help`.
 */
const helpMessage = `Lisp interpreter and transpiler in JavaScript

REPL:

  roselisp

Interpret a file:

  roselisp input.scm

Compile a file to JavaScript:

  roselisp -c input.scm

Compile a file to TypeScript:

  roselisp -c --language typescript input.scm

Options:

  --compile   Compiles one or more files (short form -c).
              Otherwise, the default is interpretation.
  --quick     Incremental compilation (short form -q).
              Only compiles a file if the input file is
              newer than the output file.
  --eval      Evaluate an expression (short form -e).
              For example, roselisp -e "(+ 1 1)"
              evaluates the expression (+ 1 1) and
              prints the result to standard output.
              Otherwise interprets it (default).
  --indent    The number of spaces to indent
              (default: 2).
  --language  Language: javascript or typescript
              (default: javascript).
  --out-dir   Output directory for compiled files
              (default: same directory).`;
/**
 * Normalize CLI options.
 */
function normalizeCliOptions(options) {
    const languageOption = options['language'].toLowerCase();
    const toOption = (languageOption === '') ? options['to'] : languageOption;
    return Object.assign(Object.assign({}, options), { to: toOption });
}
/**
 * `main` function. Invoked when the program is
 * run from the command line.
 */
function main() {
    const flags = normalizeCliOptions(minimist(process.argv.slice(2), buildOptions(cliOptions)));
    const input = flags._;
    const compileFlag = flags.compile;
    const decompileFlag = flags.decompile;
    const evalFlag = flags.eval;
    const replFlag = flags.repl;
    const helpFlag = flags.help;
    if (helpFlag) {
        console.log(helpMessage);
    }
    else if (evalFlag) {
        console.log((0, language_1.interpretString)(evalFlag));
    }
    else if (replFlag || (input.length === 0)) {
        (0, repl_1.repl)();
    }
    else if (decompileFlag) {
        (0, decompiler_1.decompileFilesX)(input, flags);
    }
    else if (compileFlag) {
        (0, language_1.compileFilesX)(input, flags);
    }
    else {
        (0, language_1.interpretFiles)(input);
    }
}
// Invoke the `main` function.
main();
