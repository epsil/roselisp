// SPDX-License-Identifier: MPL-2.0
/**
 * # REPL
 *
 * Read--eval--print loop (REPL).
 *
 * ## Description
 *
 * This file defines a very simple read--eval--print loop
 * ([REPL][w:REPL]), i.e., an interactive language shell.
 * Reading is done with Node's [`readline`][node:readline]
 * library.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [w:REPL]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
 * [node:readline]: https://nodejs.org/api/readline.html
 */

import {
  stdin,
  stdout
} from 'process';

import * as readline from 'readline';

import {
  version
} from './constants';

import {
  LispEnvironment
} from './env';

import {
  interpret as eval_,
  langEnvironment,
  printSexpAsExpression
} from './language';

import {
  read
} from './parser';

/**
 * REPL prompt.
 */
const replPrompt: any = '> ';

/**
 * Message displayed when starting the REPL.
 */
const initialReplMessage: any = ';; Roselisp version ' + version + '.\n' +
  ';; Type ,h for help and ,q to quit.';

/**
 * Help message displayed by the REPL's `help` command.
 */
const replHelpMessage: any = 'Enter an S-expression to evaluate it.\n' +
  'Use the up and down keys to access previous expressions.\n' +
  '\n' +
  'Type ,q to quit.';

/**
 * Start a simple REPL.
 *
 * The REPL reads from standard input using Node's
 * [`readline`][node:readline] module.
 *
 * [node:readline]: https://nodejs.org/api/readline.html
 */

function repl(): void {
  const rl: any = readline.createInterface({
    input: stdin,
    output: stdout
  });
  let quitFlag: any = false;
  function quit(): any {
    if (!quitFlag) {
      quitFlag = true;
      return rl.close();
    }
  }
  const env: any = makeReplEnvironment([[Symbol.for('exit'), quit, 'function'], [Symbol.for('help'), help, 'function'], [Symbol.for('quit'), quit, 'function']]);
  // Read-eval-print loop
  function loopF(...args: any[]): any {
    function callback(x: any): any {
      if (quitFlag || isQuitCmdP(x)) {
        return quit();
      } else if (isHelpCmdP(x)) {
        help();
        return loopF();
      } else {
        let result: any = undefined;
        // Read (R).
        const exp: any = read(x);
        // Evaluate (E).
        try {
          result = eval_(exp, env);
          // Print (P).
          printValue(result);
        } catch (e) {
          if (e instanceof Error) {
            console.log(e);
          } else {
            throw e;
          }
        }
        // Loop (L).
        return loopF();
      }
    }
    return rl.question(replPrompt, callback);
  }
  console.log(initialReplMessage);
  loopF();
}

/**
 * Make an environment for the REPL.
 */
function makeReplEnvironment(bindings: any): any {
  return new LispEnvironment(bindings, langEnvironment);
}

/**
 * Whether `str` is a command for quitting the REPL.
 */
function isHelpCmdP(str: any): any {
  return [',h', ',help', '(help)'].includes(str);
}

/**
 * Whether `str` is a command for quitting the REPL.
 */
function isQuitCmdP(str: any): any {
  return [',q', ',quit', '(quit)', ',exit', '(exit)'].includes(str);
}

/**
 * Print a value.
 */
function printValue(x: any, options: any = {}): any {
  return console.log(printSexpAsExpression(x, options));
}

/**
 * Display help message.
 */
function help(): any {
  return console.log(replHelpMessage);
}

export {
  repl
};