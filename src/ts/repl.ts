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
  execSync
} from 'child_process';

import {
  readFileSync,
  writeFileSync
} from 'fs';

import {
  homedir
} from 'os';

import {
  join
} from 'path';

import {
  chdir,
  cwd,
  stdin,
  stdout
} from 'process';

import * as readline from 'readline';

import {
  packageName,
  version
} from './constants';

import {
  interpret as eval_,
  load_ as load,
  langEnvironment,
  loadModule,
  printSexp,
  printSexpAsExpression
} from './core';

import {
  LispEnvironment,
  currentEnvironment,
  withEnvironment
} from './env';

import {
  equalp_
} from './equal';

import {
  read
} from './parser';

import {
  copyIntoArrayX
} from './util';

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
   * Flag for printing values.
   */
  printFlag: any = true;

  /**
   * Flag for quitting.
   */
  quitFlag: any = false;

  /**
   * The REPL environment.
   */
  env: any;

  /**
   * The toplevel environment.
   */
  toplevel: any;

  /**
   * The current module, if any.
   */
  currentModule: any = undefined;

  /**
   * REPL options.
   */
  private options: any = {};

  /**
   * REPL prompt string.
   */
  private promptStr: any = '> ';

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
   * A list of strings, with the latest entry stored
   * at the beginning of the list.
   */
  private history: any = [];

  /**
   * `readline` instance.
   */
  private rl: any = undefined;

  /**
   * `readline` history.
   */
  private rlHistory: any = [];

  /**
   * Absolute path to `~/.roselisp_history`, where
   * persistent history is stored.
   */
  private historyFile: any = join(homedir(), '.' + packageName + '_history');

  /**
   * Message displayed when starting the REPL.
   */
  private startupMessage: any = ';; Roselisp version ' + version + '.\n' +
    ';; Type ,h for help and ,q to quit.';

  // Whether to echo user input.
  private echo: any = true;

  /**
   * Create a new REPL.
   */
  constructor(options: any = {}) {
    this.options = this.normalizeOptions(options);
    this.env = makeInteractiveEnvironment(this);
    this.toplevel = this.env;
  }

  /**
   * Start the REPL.
   */
  start(options: any = this.options): any {
    this.echo = !options['sparseRepl'];
    if (options['replHistory']) {
      this.history = this.readHistory();
      this.rlHistory = [...this.history];
    }
    // Initialize the `readline` instance.
    this.rl = readline.createInterface({
      input: options['input'] || stdin,
      history: this.rlHistory,
      output: this.echo ? (options['output'] || stdout) : undefined
    });
    // Register a listener for the `line` event that will
    // receive a single line of user input and take
    // appropriate action.
    this.rl.on('line', (input: any): any => withRepl(this, (): any => withEnvironment(this.env, (): any => {
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
        return this.prompt();
      } else {
        if (this.printFlag && (result !== '')) {
          console.log(result);
        }
        return this.prompt();
      }
    })));
    // Register a listener for the `close` event that will
    // terminate the output properly.
    this.rl.on('close', (): any => {
      if (!this.echo) {
        return stdout.write('\n');
      }
    });
    // Start the read--eval--print loop.
    this.printStartupMessage();
    return this.prompt();
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
    // for multiple expressions to be entered at a single prompt
    // (e.g., `> (+ 1 1) (+ 2 2)`), so that what is processed here
    // is not a single expression, but rather a list of expressions.
    let parsedExpressions: any = [];
    try {
      parsedExpressions = read('(' + input + ')');
    } catch (e) {
      if (e instanceof Error) {
        if (e.message === 'eof') {
          // If we are reading a multi-line expression,
          // then continue listening for user input.
          this.state = 'read';
          return '';
        } else {
          throw e;
        }
      } else {
        throw e;
      }
    }
    // Update the REPL history.
    this.history.unshift(input);
    // Synchronize the `readline` instance's history with the
    // REPL history. This is needed in order to handle multi-line
    // expressions correctly, because otherwise the `readline`
    // instance will create one history item per line.
    copyIntoArrayX(this.history, this.rlHistory);
    // Rewrite the expressions slightly in order to handle REPL
    // shortcuts such as `,h` and `,q`.
    const rewrittenExpressions: any = rewriteExpressions(parsedExpressions);
    // Evaluate the expressions one by one, producing a list
    // of values.
    const evaluatedExpressions: any = [];
    for (let exp of rewrittenExpressions) {
      if (this.quitFlag) {
        break;
      }
      try {
        evaluatedExpressions.push(eval_(exp, env, this.options));
      } catch (e) {
        console.log(e);
      }
    }
    // Continue unless the user has quit. If the user has quit,
    // the return value is just the empty string.
    let result: any = '';
    if (!this.quitFlag) {
      if (this.options['replHistory']) {
        this.writeHistoryEntry(input);
      }
      // Print the values, producing a list of value strings.
      const printedExpressions: any = [];
      for (let x of evaluatedExpressions) {
        if (x === undefined) {
          // Don't print `#u`. FIXME: A better way to determine this
          // would be to check if the original expression called a
          // function whose function type returns `Void`.
          continue;
        } else {
          printedExpressions.push(printSexpAsExpression(x));
        }
      }
      // Concatenate the value strings into a single string,
      // with each value string on its own line.
      result = printedExpressions.join('\n');
    }
    return result;
  }

  /**
   * Normalize options.
   */
  private normalizeOptions(options: any): any {
    let result: any = {
      ...options
    };
    return result;
  }

  /**
   * Print startup message.
   */
  private printStartupMessage(): any {
    return console.log(this.startupMessage);
  }

  /**
   * Read a line of user input.
   */
  private prompt(): any {
    const promptStr: any = (this.state === 'read') ? '' : this.promptStr;
    this.rl.setPrompt(promptStr);
    if (!this.echo) {
      stdout.write(promptStr);
    }
    return this.rl.prompt();
  }

  /**
   * Read persistent history.
   */
  private readHistory(str: any = ''): any {
    let history: any = str;
    try {
      history = readFileSync(this.historyFile, {
        encoding: 'utf8'
      });
    } catch {
    }
    return stringToHistory(history);
  }

  /**
   * Write persistent history.
   */
  private writeHistory(history: any = this.history): any {
    try {
      return writeFileSync(this.historyFile, historyToString(history), {
        encoding: 'utf8'
      });
    } catch {
    }
  }

  /**
   * Append an entry to the persistent history.
   */
  private writeHistoryEntry(entry: any): any {
    try {
      return writeFileSync(this.historyFile, entry + '\n', {
        encoding: 'utf8',
        flag: 'a'
      });
    } catch {
    }
  }
}

/**
 * List current bindings.
 */
function binding(): void {
  const env: any = currentRepl().env;
  for (let entry of env.entries()) {
    const sym: any = entry[0];
    const symPrinted: any = printSexp(sym);
    const [val]: any[] = entry[1];
    const valPrinted: any = printSexpAsExpression(val);
    console.log(symPrinted, '		', valPrinted);
  }
}

/**
 * Change the current directory.
 */
function cd(dir: any): void {
  try {
    chdir(dir);
    pwd();
  } catch (e) {
    console.log(e);
  }
}

/**
 * Edit a file.
 *
 * Uses the system's default editor as specified with the environment
 * variable `$EDITOR`; if that is not defined, then the command
 * `editor` is invoked instead.
 */
function edit(...args: any[]): void {
  const cmd: any = ['${EDITOR:-editor}', ...(Array.isArray(args) ? args : [Symbol.for('.'), args])].join(' ');
  try {
    execSync(cmd, {
      shell: '/bin/bash',
      stdio: 'inherit'
    });
  } catch (e) {
    console.log(e);
  }
}

/**
 * Enter a module.
 */
function enterx(x: any = undefined): void {
  if (x === undefined) {
    const m: any = currentRepl().currentModule;
    if (m) {
      enterx(join(m.modulePath, m.name));
    }
  } else if (!x) {
    toplevel();
  } else {
    const m: any = loadModule(x, currentEnvironment());
    if (m) {
      currentRepl().env = m.getExportEnvironment();
    }
  }
}

/**
 * Display help message.
 */
function help(): void {
  currentRepl().printFlag = false;
  console.log(`;; Enter an S-expression to evaluate it.
;; Use the up and down keys to access previous expressions.
;;
;; Type ,q to quit.`);
}

/**
 * List the contents of the current directory.
 */
function ls(...args: any[]): void {
  shell(...['ls', ...(Array.isArray(args) ? args : [Symbol.for('.'), args])]);
}

/**
 * Print the current directory.
 */
function pwd(): void {
  console.log(cwd());
}

/**
 * Execute a shell command.
 */
function shell(...args: any[]): void {
  const cmd: any = args.join(' ');
  try {
    execSync(cmd, {
      stdio: 'inherit'
    });
  } catch (e) {
    console.log(e);
  }
}

/**
 * Go back to the toplevel.
 */
function toplevel(): void {
  currentRepl().env = currentRepl().toplevel;
}

/**
 * Quit the REPL.
 */
function quit(): void {
  currentRepl().printFlag = false;
  currentRepl().quitFlag = true;
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

function repl(options: any = {}): void {
  new REPL(options).start();
}

/**
 * Rewrite `(unquote ...)` expressions to regular
 * function calls.
 */
function rewriteExpressions(exp: any): any {
  if (Array.isArray(exp) && (exp.length >= 1) && Array.isArray(exp[0]) && (exp[0].length === 2) && (exp[0][0] === Symbol.for('unquote'))) {
    let [[, x], ...y]: any[] = exp;
    // Aliases.
    if (x === Symbol.for('b')) {
      x = Symbol.for('binding');
    } else if ([Symbol.for('e'), Symbol.for('ed')].includes(x)) {
      x = Symbol.for('edit');
    } else if ([Symbol.for('en'), Symbol.for('enter')].includes(x)) {
      x = Symbol.for('enter!');
    } else if (x === Symbol.for('h')) {
      x = Symbol.for('help');
    } else if (x === Symbol.for('l')) {
      x = Symbol.for('load');
    } else if (x === Symbol.for('m')) {
      x = Symbol.for('module');
    } else if ([Symbol.for('req'), Symbol.for('r')].includes(x)) {
      x = Symbol.for('require');
    } else if ([Symbol.for('ex'), Symbol.for('x'), Symbol.for('q')].includes(x)) {
      x = Symbol.for('quit');
    }
    // Rewrite an expression like `,cd ..`
    // to `(cd "..")`.
    return [[x, ...y.map((x: any): any => {
      if (typeof x === 'symbol') {
        return x.description as string;
      } else {
        return x + '';
      }
    })]];
  } else if (Array.isArray(exp) && (exp.length === 0)) {
    const []: any[] = exp;
    return [[Symbol.for('help')]];
  } else {
    return exp;
  }
}

/**
 * Make an environment for the REPL.
 */
function makeInteractiveEnvironment(repl: any, bindings: any = []): any {
  const parent: any = langEnvironment;
  const env: any = new LispEnvironment([[Symbol.for('binding'), binding, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Void')]]], [Symbol.for('bindings'), binding, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Void')]]], [Symbol.for('cd'), cd, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Void')]]], [Symbol.for('edit'), edit, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('enter!'), enterx, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('Void')]], [Symbol.for('exit'), quit, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('help'), help, [Symbol.for('->'), Symbol.for('Void')]], [Symbol.for('load'), load, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('ls'), ls, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('module'), enterx, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('Void')]], [Symbol.for('pwd'), pwd, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Void')]]], [Symbol.for('quit'), quit, [Symbol.for('->'), Symbol.for('Void')]], [Symbol.for('shell'), shell, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Void')]]], [Symbol.for('toplevel'), toplevel, [Symbol.for('->'), Symbol.for('Void')]], ...bindings], parent);
  const env1: any = new LispEnvironment([], env);
  return env1;
}

/**
 * Pointer to the current REPL instance.
 */
let currentReplPointer: any = undefined;

/**
 * Force the thunk `f` with `current-repl-pointer`
 * bound to `repl`. The previous value of
 * `current-repl-pointer` is restored afterwards.
 */
function withRepl(repl: any, f: any): any {
  let result: any = undefined;
  const tmp: any = currentReplPointer;
  try {
    currentReplPointer = repl;
    result = (f as any)();
  } finally {
    currentReplPointer = tmp;
  }
  return result;
}

/**
 * Return the current REPL instance.
 * Used by REPL meta-commands to
 * communicate with the REPL.
 */
function currentRepl(): any {
  return currentReplPointer;
}

/**
 * Convert a history object to a string.
 * The reverse of `string->history`.
 */
function historyToString(history: any): any {
  return [...history].reverse().join('\n');
}

/**
 * Convert a string to a history object.
 *
 * The input to this function is a string of entered lines, separated
 * by newlines, with the latest entry at the end of the string. The
 * output is a list of strings, one per expression, in the opposite
 * order (i.e., with the latest entry at the beginning of the list,
 * which is what `readline` expects). Expressions spanning multiple
 * lines are parsed as a single expression, so there might be fewer
 * entries in the output than there are lines in the input.
 */
function stringToHistory(str: any): any {
  // Note that since the format is so simple, this function has to be
  // fairly error-tolerant so it can deal gracefully with syntax
  // errors in the user input. With a more robust format for
  // persistent history (e.g., serializing the history as an
  // S-expression), this would not be needed at all; nevertheless, we
  // have opted to go with a simple but brittle format for the sake of
  // convention.
  let result: any = [];
  const spaceRegexp: any = /^\s*$/;
  let buffer: any = '';
  const exp: any = undefined;
  const lines: any = str.split('\n');
  for (let line of lines) {
    buffer = (buffer === '') ? line : (buffer + '\n' +
      line);
    try {
      read(buffer);
      result.unshift(buffer);
      buffer = '';
    } catch (e) {
      if (e instanceof Error) {
        if (e.message !== 'eof') {
          if (!buffer.match(spaceRegexp)) {
            result.unshift(buffer);
          }
          buffer = '';
        }
      } else {
        throw e;
      }
    }
  }
  return result;
}

export {
  REPL,
  historyToString,
  rep,
  repl,
  stringToHistory
};