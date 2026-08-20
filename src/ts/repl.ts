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
  withEnvironment,
  withEnvironmentF
} from './env';

import {
  interpret as eval_,
  langEnvironment,
  load_,
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
function e(input: any, env: any = makeInteractiveEnvironment()): any {
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
function re(input: any, env: any = makeInteractiveEnvironment()): any {
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
function rep(input: any, env: any = makeInteractiveEnvironment()): any {
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
  let printFlag: any = true;
  let quitFlag: any = false;
  function help(): any {
    printFlag = false;
    return console.log(replHelpMessage);
  }
  function quitx(): any {
    printFlag = false;
    quitFlag = true;
    return rl.close();
  }
  const interactiveEnv: any = makeInteractiveEnvironment({
    help,
    quit: quitx
  });
  // Read-eval-print loop
  function loopF(...args: any[]): any {
    function callback(x: any): any {
      return withEnvironmentF(interactiveEnv, (): any => {
        // Read (R), Evaluate (E), Print (P).
        printFlag = true;
        let result: any = p(e(rewriteExpression(r(x)), interactiveEnv));
        if (printFlag) {
          console.log(result);
        }
        if (!quitFlag) {
          return loopF();
        }
      });
    }
    return rl.question(replPrompt, callback);
  }
  console.log(initialReplMessage);
  loopF();
}

/**
 * Make an environment for the REPL.
 */
function makeInteractiveEnvironment(options: any = {}): any {
  const help_: any = options['help'];
  const quit_: any = options['quit'];
  const parentEnv: any = new LispEnvironment([[Symbol.for('exit'), quit_, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('help'), help_, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('quit'), quit_, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('load'), load_, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]]], langEnvironment);
  const env: any = new LispEnvironment([], parentEnv);
  return env;
}

/**
 * Whether `exp` is a command for quitting the REPL.
 */
function helpCmdP(exp: any): any {
  return equalp_(exp, [[Symbol.for('help')]]);
}

/**
 * Whether `exp` is a command for quitting the REPL.
 */
function quitCmdP(exp: any): any {
  return equalp_(exp, [[Symbol.for('quit')]]) || equalp_(exp, [[Symbol.for('exit')]]);
}

/**
 * Print a value.
 */
function printValue(x: any, options: any = {}): any {
  return console.log(printSexpAsExpression(x, options));
}

/**
 * Rewrite `(unquote ...)` expressions to regular
 * function calls.
 */
function rewriteExpression(exp: any): any {
  if (Array.isArray(exp) && (exp.length >= 1) && Array.isArray(exp[0]) && (exp[0].length === 2) && (exp[0][0] === Symbol.for('unquote'))) {
    let [[, x], ...y]: any[] = exp;
    if (x === Symbol.for('h')) {
      x = Symbol.for('help');
    } else if ([Symbol.for('x'), Symbol.for('q')].includes(x)) {
      x = Symbol.for('quit');
    }
    return [[x, ...y]];
  } else {
    return exp;
  }
}

export {
  r,
  re,
  rep,
  repl
};