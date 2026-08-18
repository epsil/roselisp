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
  equalp_
} from './equal';

import {
  LispEnvironment,
  withEnvironment
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
 * Read utility.
 */
function r(input: any): any {
  return read('(' + input + ')');
}

/**
 * Eval utility.
 */
function e(input: any, env: any = makeReplEnvironment()): any {
  return input.map(function (exp: any): any {
    let result: any = undefined;
    try {
      result = eval_(exp, env);
    } catch (err) {
      if (err instanceof Error) {
        console.log(err);
      } else {
        throw err;
      }
    }
    return result;
  });
}

/**
 * Read--Eval utility.
 */
function re(input: any, env: any = makeReplEnvironment()): any {
  return e(r(input), env);
}

/**
 * Print utility.
 */
function p(input: any): any {
  return input.map(function (x: any): any {
    return printSexpAsExpression(x);
  }).join('\n');
}

/**
 * Read--Eval--Print utility.
 */
function rep(input: any, env: any = makeReplEnvironment()): any {
  return p(re(input, env));
}

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
  function quitx(): any {
    if (!quitFlag) {
      quitFlag = true;
      return rl.close();
    }
  }
  const env: any = makeReplEnvironment([[Symbol.for('exit'), quitx, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('help'), help, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('quit'), quitx, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]]]);
  // Read-eval-print loop
  function loopF(...args: any[]): any {
    function callback(x: any): any {
      const exp: any = r(x);
      if (quitFlag || quitCmdP(exp)) {
        return quitx();
      } else if (helpCmdP(exp)) {
        help();
        return loopF();
      } else {
        withEnvironment(env, function (): any {
          // Read (R), Evaluate (E), Print (P).
          return console.log(p(e(exp, env)));
        });
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
function makeReplEnvironment(bindings: any = []): any {
  return new LispEnvironment(bindings, langEnvironment);
}

/**
 * Whether `exp` is a command for quitting the REPL.
 */
function helpCmdP(exp: any): any {
  return equalp_(exp, [[Symbol.for('unquote'), Symbol.for('h')]]) || equalp_(exp, [[Symbol.for('unquote'), Symbol.for('help')]]) || equalp_(exp, [[Symbol.for('help')]]);
}

/**
 * Whether `exp` is a command for quitting the REPL.
 */
function quitCmdP(exp: any): any {
  return equalp_(exp, [[Symbol.for('unquote'), Symbol.for('q')]]) || equalp_(exp, [[Symbol.for('unquote'), Symbol.for('quit')]]) || equalp_(exp, [[Symbol.for('quit')]]) || equalp_(exp, [[Symbol.for('unquote'), Symbol.for('exit')]]) || equalp_(exp, [[Symbol.for('exit')]]) || equalp_(exp, [[Symbol.for('unquote'), Symbol.for('x')]]);
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
  r,
  re,
  rep,
  repl
};