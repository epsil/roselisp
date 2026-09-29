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

import {
  copyIntoArrayX
} from './util';

withEnvironment.ftype = 'macro';

/**
 * REPL class.
 *
 * Encapsulates a [`readline`][node:readline] instance that reads
 * from standard input.
 *
 * [node:readline]: https://nodejs.org/api/readline.html
 */
class REPL {
  /**
   * REPL prompt.
   */
  private prompt: any = '> ';

  /**
   * REPL state.
   */
  private state: any = 'prompt';

  /**
   * REPL user input.
   */
  private input: any = '';

  /**
   * REPL history.
   *
   * A list of strings. The latest entry is stored
   * at the beginning of the list.
   */
  private history: any = [];

  /**
   * Flag for printing values.
   */
  private printFlag: any = true;

  /**
   * Flag for quitting.
   */
  private quitFlag: any = false;

  /**
   * `readline` instance.
   */
  private rl: any = undefined;

  /**
   * `readline` history.
   */
  private rlHistory: any = [];

  /**
   * The REPL environment.
   */
  private env: any;

  /**
   * Message displayed when starting the REPL.
   */
  private startupMessage: any = ';; Roselisp version ' + version + '.\n' +
    ';; Type ,h for help and ,q to quit.';

  /**
   * Help message displayed by the REPL's `help` command.
   */
  private helpMessage: any = `;; Enter an S-expression to evaluate it.
;; Use the up and down keys to access previous expressions.
;;
;; Type ,q to quit.`;

  /**
   * Create a new REPL.
   */
  constructor() {
    this.env = makeInteractiveEnvironment({
      help: (): any => {
        this.printFlag = false;
        return console.log(this.helpMessage);
      },
      quit: (): any => {
        this.printFlag = false;
        return this.quitFlag = true;
      }
    });
  }

  /**
   * Start the REPL.
   */
  start(): any {
    // Initialize the `readline` instance.
    this.rl = readline.createInterface({
      input: stdin,
      output: stdout
    });
    this.rl.on('line', (input: any): any => withEnvironmentF(this.env, (): any => {
      this.printFlag = true;
      if (this.state === 'read') {
        this.input = this.input + '\n' +
          input;
        this.state = 'prompt';
      } else {
        this.input = input;
      }
      let result: any = this.rep(this.input, this.env);
      if (this.quitFlag) {
        return this.rl.close();
      } else if (this.state === 'read') {
        return this.readLine();
      } else {
        if (this.printFlag) {
          console.log(result);
        }
        return this.readLine();
      }
    }));
    this.rl.on('history', (x: any): any => this.rlHistory = x);
    // Start the read--eval--print loop.
    this.printStartupMessage();
    return this.readLine();
  }

  /**
   * Read--eval--print method.
   */
  rep(input: any, env: any = this.env): any {
    // Ignore leading whitespace.
    if (input.match(/^\s*$/)) {
      this.state = 'read';
      return '';
    }
    // Parse user input into expressions. Note the plural: we allow
    // for multiple expressions to be entered at a single prompt,
    // so that what is processed here is not a single expression,
    // but rather a list of expressions.
    let parsedExpressions: any = [];
    try {
      parsedExpressions = read('(' + input + ')');
    } catch (err) {
      if (err instanceof Error) {
        if (err.message === 'eof') {
          // If we are reading a multi-line expression,
          // then continue listening for user input.
          this.state = 'read';
          return '';
        } else {
          throw err;
        }
      } else {
        throw err;
      }
    }
    /**
     * Update the REPL history.
     */
    this.history.unshift(input);
    // Synchronize the `readline` instance's history with the
    // REPL history. This is needed in order to handle multi-line
    // expressions correctly, because otherwise the `readline`
    // instance will create one history entry per line.
    copyIntoArrayX(this.history, this.rlHistory);
    // Rewrite the expressions slightly in order to handle REPL
    // shortcuts such as `,h` and `,q`.
    const rewrittenExpressions: any = rewriteExpressions(parsedExpressions);
    // Evaluate the expressions one by one, producing a list
    // of values.
    const evaluatedExpressions: any = rewrittenExpressions.map((exp: any): any => {
      let result: any = undefined;
      if (!this.quitFlag) {
        try {
          result = eval_(exp, env);
        } catch (err) {
          if (err instanceof Error) {
            console.log(err);
          } else {
            throw err;
          }
        }
      }
      return result;
    });
    // Continue unless the user has quit. If the user has quit,
    // the return value is just the empty string.
    let result: any = '';
    if (!this.quitFlag) {
      // Print the values, producing a list of value strings.
      const printedExpressions: any = evaluatedExpressions.map((printSexpAsExpression.length === 1) ? printSexpAsExpression : (x: any): any => printSexpAsExpression(x));
      // Concatenate the value strings into a single string,
      // with each value string on its own line.
      result = printedExpressions.join('\n');
    }
    return result;
  }

  /**
   * Read a line of user input.
   */
  private readLine(): any {
    this.rl.setPrompt((this.state === 'read') ? '' : this.prompt);
    return this.rl.prompt();
  }

  /**
   * Print startup message.
   */
  private printStartupMessage(): any {
    return console.log(this.startupMessage);
  }
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
 * Rewrite `(unquote ...)` expressions to regular
 * function calls.
 */
function rewriteExpressions(exp: any): any {
  if (Array.isArray(exp) && (exp.length >= 1) && Array.isArray(exp[0]) && (exp[0].length === 2) && (exp[0][0] === Symbol.for('unquote'))) {
    let [[, x], ...y]: any[] = exp;
    if (x === Symbol.for('h')) {
      x = Symbol.for('help');
    } else if ([Symbol.for('ex'), Symbol.for('x'), Symbol.for('q')].includes(x)) {
      x = Symbol.for('quit');
    }
    return [[x, ...y]];
  } else {
    return exp;
  }
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
 * Read--eval--print function.
 */
function rep(input: any): any {
  return new REPL().rep(input);
}

/**
 * Read--eval--print--loop function.
 */

function repl(): void {
  new REPL().start();
}

export {
  REPL,
  rep,
  repl
};