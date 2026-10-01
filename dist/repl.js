"use strict";
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.repl = exports.rep = exports.REPL = void 0;
const process_1 = require("process");
const readline = require("readline");
const constants_1 = require("./constants");
const equal_1 = require("./equal");
const env_1 = require("./env");
const language_1 = require("./language");
const parser_1 = require("./parser");
const util_1 = require("./util");
env_1.withEnvironment.ftype = 'macro';
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
     * Create a new REPL.
     */
    constructor() {
        /**
         * REPL prompt.
         */
        this.prompt = '> ';
        /**
         * REPL state.
         */
        this.state = 'prompt';
        /**
         * REPL user input.
         */
        this.input = '';
        /**
         * REPL history.
         *
         * A list of strings. The latest entry is stored
         * at the beginning of the list.
         */
        this.history = [];
        /**
         * Flag for printing values.
         */
        this.printFlag = true;
        /**
         * Flag for quitting.
         */
        this.quitFlag = false;
        /**
         * `readline` instance.
         */
        this.rl = undefined;
        /**
         * `readline` history.
         */
        this.rlHistory = [];
        /**
         * Message displayed when starting the REPL.
         */
        this.startupMessage = ';; Roselisp version ' + constants_1.version + '.\n' +
            ';; Type ,h for help and ,q to quit.';
        /**
         * Help message displayed by the REPL's `help` command.
         */
        this.helpMessage = `;; Enter an S-expression to evaluate it.
;; Use the up and down keys to access previous expressions.
;;
;; Type ,q to quit.`;
        this.env = makeInteractiveEnvironment({
            help: () => {
                this.printFlag = false;
                return console.log(this.helpMessage);
            },
            quit: () => {
                this.printFlag = false;
                return this.quitFlag = true;
            }
        });
    }
    /**
     * Start the REPL.
     */
    start() {
        // Initialize the `readline` instance.
        this.rl = readline.createInterface({
            input: process_1.stdin,
            output: process_1.stdout
        });
        this.rl.on('line', (input) => (0, env_1.withEnvironmentF)(this.env, () => {
            this.printFlag = true;
            if (this.state === 'read') {
                this.input = this.input + '\n' +
                    input;
                this.state = 'prompt';
            }
            else {
                this.input = input;
            }
            let result = this.rep(this.input, this.env);
            if (this.quitFlag) {
                return this.rl.close();
            }
            else if (this.state === 'read') {
                return this.readLine();
            }
            else {
                if (this.printFlag) {
                    console.log(result);
                }
                return this.readLine();
            }
        }));
        this.rl.on('history', (x) => this.rlHistory = x);
        // Start the read--eval--print loop.
        this.printStartupMessage();
        return this.readLine();
    }
    /**
     * Read--eval--print method.
     */
    rep(input, env = this.env) {
        // Ignore leading whitespace.
        if (input.match(/^\s*$/)) {
            this.state = 'read';
            return '';
        }
        // Parse user input into expressions. Note the plural: we allow
        // for multiple expressions to be entered at a single prompt,
        // so that what is processed here is not a single expression,
        // but rather a list of expressions.
        let parsedExpressions = [];
        try {
            parsedExpressions = (0, parser_1.read)('(' + input + ')');
        }
        catch (err) {
            if (err instanceof Error) {
                if (err.message === 'eof') {
                    // If we are reading a multi-line expression,
                    // then continue listening for user input.
                    this.state = 'read';
                    return '';
                }
                else {
                    throw err;
                }
            }
            else {
                throw err;
            }
        }
        // Update the REPL history.
        this.history.unshift(input);
        // Synchronize the `readline` instance's history with the
        // REPL history. This is needed in order to handle multi-line
        // expressions correctly, because otherwise the `readline`
        // instance will create one history entry per line.
        (0, util_1.copyIntoArrayX)(this.history, this.rlHistory);
        // Rewrite the expressions slightly in order to handle REPL
        // shortcuts such as `,h` and `,q`.
        const rewrittenExpressions = rewriteExpressions(parsedExpressions);
        // Evaluate the expressions one by one, producing a list
        // of values.
        const evaluatedExpressions = rewrittenExpressions.map((exp) => {
            let result = undefined;
            if (!this.quitFlag) {
                try {
                    result = (0, language_1.interpret)(exp, env);
                }
                catch (err) {
                    if (err instanceof Error) {
                        console.log(err);
                    }
                    else {
                        throw err;
                    }
                }
            }
            return result;
        });
        // Continue unless the user has quit. If the user has quit,
        // the return value is just the empty string.
        let result = '';
        if (!this.quitFlag) {
            // Print the values, producing a list of value strings.
            const printedExpressions = evaluatedExpressions.map((language_1.printSexpAsExpression.length === 1) ? language_1.printSexpAsExpression : (x) => (0, language_1.printSexpAsExpression)(x));
            // Concatenate the value strings into a single string,
            // with each value string on its own line.
            result = printedExpressions.join('\n');
        }
        return result;
    }
    /**
     * Read a line of user input.
     */
    readLine() {
        this.rl.setPrompt((this.state === 'read') ? '' : this.prompt);
        return this.rl.prompt();
    }
    /**
     * Print startup message.
     */
    printStartupMessage() {
        return console.log(this.startupMessage);
    }
}
exports.REPL = REPL;
/**
 * Whether `exp` is a command for quitting the REPL.
 */
function helpCmdP(exp) {
    return (0, equal_1.equalp_)(exp, [[Symbol.for('help')]]);
}
/**
 * Whether `exp` is a command for quitting the REPL.
 */
function quitCmdP(exp) {
    return (0, equal_1.equalp_)(exp, [[Symbol.for('quit')]]) || (0, equal_1.equalp_)(exp, [[Symbol.for('exit')]]);
}
/**
 * Rewrite `(unquote ...)` expressions to regular
 * function calls.
 */
function rewriteExpressions(exp) {
    if (Array.isArray(exp) && (exp.length >= 1) && Array.isArray(exp[0]) && (exp[0].length === 2) && (exp[0][0] === Symbol.for('unquote'))) {
        let [[, x], ...y] = exp;
        if (x === Symbol.for('h')) {
            x = Symbol.for('help');
        }
        else if ([Symbol.for('ex'), Symbol.for('x'), Symbol.for('q')].includes(x)) {
            x = Symbol.for('quit');
        }
        return [[x, ...y]];
    }
    else {
        return exp;
    }
}
/**
 * Make an environment for the REPL.
 */
function makeInteractiveEnvironment(options = {}) {
    const help_ = options['help'];
    const quit_ = options['quit'];
    const parentEnv = new env_1.LispEnvironment([[Symbol.for('exit'), quit_, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('help'), help_, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('quit'), quit_, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('load'), language_1.load_, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]]], language_1.langEnvironment);
    const env = new env_1.LispEnvironment([], parentEnv);
    return env;
}
/**
 * Read--eval--print function.
 */
function rep(input) {
    return new REPL().rep(input);
}
exports.rep = rep;
/**
 * Read--eval--print--loop function.
 */
function repl() {
    new REPL().start();
}
exports.repl = repl;
