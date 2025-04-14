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

import * as minimist from 'minimist';

import * as buildOptions from 'minimist-options';

import {
  decompileFilesX
} from './decompiler';

import {
  compileFilesX,
  interpretFiles,
  interpretString
} from './language';

import {
  repl
} from './repl';

/**
 * Command line options. Passed to
 * [`minimist-options`][npm:minimist-options]
 * for parsing.
 *
 * [npm:minimist-options]: https://www.npmjs.com/package/minimist-options
 */
const cliOptions: any = {
  camelCase: {
    alias: 'camel-case',
    default: true,
    type: 'boolean'
  },
  comments: {
    default: true,
    type: 'boolean'
  },
  compile: {
    default: false,
    alias: 'c',
    type: 'boolean'
  },
  decompile: {
    default: false,
    alias: 'd',
    type: 'boolean'
  },
  eval: {
    default: '',
    alias: 'e',
    type: 'string'
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
  help: {
    default: false,
    alias: 'h',
    type: 'boolean'
  },
  indent: {
    default: 2,
    type: 'number'
  },
  language: {
    type: 'string'
  },
  outDir: {
    alias: 'out-dir',
    type: 'string',
    default: '.'
  },
  quick: {
    default: false,
    alias: 'q',
    type: 'boolean'
  },
  repl: {
    default: false,
    alias: 'i',
    type: 'boolean'
  }
};

/**
 * Help message. Displayed when the program
 * is invoked with `-h` or `--help`.
 */
const helpMessage: any = 'Lisp interpreter and transpiler in JavaScript\n' +
  '\n' +
  'REPL:\n' +
  '\n' +
  '  roselisp\n' +
  '\n' +
  'Interpret a file:\n' +
  '\n' +
  '  roselisp input.scm\n' +
  '\n' +
  'Compile a file to JavaScript:\n' +
  '\n' +
  '  roselisp -c input.scm\n' +
  '\n' +
  'Compile a file to TypeScript:\n' +
  '\n' +
  '  roselisp -c --language TypeScript input.scm\n' +
  '\n' +
  'Options:\n' +
  '\n' +
  '  --compile   Compiles one or more files (short form -c).\n' +
  '              Otherwise, the default is interpretation.\n' +
  '  --quick     Incremental compilation (short form -q).\n' +
  '              Only compiles a file if the input file is\n' +
  '              newer than the output file.\n' +
  '  --eval      Evaluate an expression (short form -e).\n' +
  '              For example, roselisp -e "(+ 1 1)"\n' +
  '              evaluates the expression (+ 1 1) and\n' +
  '              prints the result to standard output.\n' +
  '              Otherwise interprets it (default).\n' +
  '  --indent    The number of spaces to indent\n' +
  '              (default: 2).\n' +
  '  --language  Language: JavaScript or TypeScript\n' +
  '              (default: JavaScript).\n' +
  '  --out-dir   Output directory for compiled files\n' +
  '              (default: same directory).';

/**
 * Normalize CLI options.
 */
function normalizeCliOptions(options: any): any {
  const languageOption: any = options['language'] || '';
  const inlineFunctionsOption: any = options['finlineFunctions'];
  const language: any = languageOption.match(new RegExp('^TypeScript$', 'i')) ? 'TypeScript' : 'JavaScript';
  return {
    ...options,
    language: language,
    inlineFunctions: inlineFunctionsOption
  };
}

/**
 * Normalize compilation options.
 */
function normalizeCompilationOptions(options: any): any {
  const evalBindingsOption: any = options['fevalBindings'];
  return {
    ...options,
    eval: evalBindingsOption
  };
}

/**
 * `main` function. Invoked when the program is
 * run from the command line.
 */

function main(): void {
  const flags: any = normalizeCliOptions(minimist(process.argv.slice(2), (buildOptions as any)(cliOptions)));
  const input: any = flags._;
  const compileFlag: any = flags.compile;
  const decompileFlag: any = flags.decompile;
  const evalFlag: any = flags.eval;
  const replFlag: any = flags.repl;
  const helpFlag: any = flags.help;
  if (helpFlag) {
    console.log(helpMessage);
  } else if (evalFlag) {
    console.log(interpretString(evalFlag));
  } else if (replFlag || (input.length === 0)) {
    repl();
  } else if (decompileFlag) {
    decompileFilesX(input, flags);
  } else if (compileFlag) {
    const compilationOptions: any = normalizeCompilationOptions(flags);
    compileFilesX(input, compilationOptions);
  } else {
    interpretFiles(input);
  }
}

// Invoke the `main` function.
main();