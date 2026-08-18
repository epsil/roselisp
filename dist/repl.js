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
exports.repl = exports.rep = exports.re = exports.r = void 0;
const process_1 = require("process");
const readline = require("readline");
const constants_1 = require("./constants");
const equal_1 = require("./equal");
const env_1 = require("./env");
const language_1 = require("./language");
const parser_1 = require("./parser");
/**
 * REPL prompt.
 */
const replPrompt = '> ';
/**
 * Message displayed when starting the REPL.
 */
const initialReplMessage = ';; Roselisp version ' + constants_1.version + '.\n' +
    ';; Type ,h for help and ,q to quit.';
/**
 * Help message displayed by the REPL's `help` command.
 */
const replHelpMessage = 'Enter an S-expression to evaluate it.\n' +
    'Use the up and down keys to access previous expressions.\n' +
    '\n' +
    'Type ,q to quit.';
/**
 * Read utility.
 */
function r(input) {
    return (0, parser_1.read)('(' + input + ')');
}
exports.r = r;
/**
 * Eval utility.
 */
function e(input, env = makeReplEnvironment()) {
    return input.map(function (exp) {
        let result = undefined;
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
        return result;
    });
}
/**
 * Read--Eval utility.
 */
function re(input, env = makeReplEnvironment()) {
    return e(r(input), env);
}
exports.re = re;
/**
 * Print utility.
 */
function p(input) {
    return input.map(function (x) {
        return (0, language_1.printSexpAsExpression)(x);
    }).join('\n');
}
/**
 * Read--Eval--Print utility.
 */
function rep(input, env = makeReplEnvironment()) {
    return p(re(input, env));
}
exports.rep = rep;
/**
 * Start a simple REPL.
 *
 * The REPL reads from standard input using Node's
 * [`readline`][node:readline] module.
 *
 * [node:readline]: https://nodejs.org/api/readline.html
 */
function repl() {
    const rl = readline.createInterface({
        input: process_1.stdin,
        output: process_1.stdout
    });
    let quitFlag = false;
    function quitx() {
        if (!quitFlag) {
            quitFlag = true;
            return rl.close();
        }
    }
    const env = makeReplEnvironment([[Symbol.for('exit'), quitx, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('help'), help, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('quit'), quitx, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]]]);
    // Read-eval-print loop
    function loopF(...args) {
        function callback(x) {
            const exp = r(x);
            if (quitFlag || quitCmdP(exp)) {
                return quitx();
            }
            else if (helpCmdP(exp)) {
                help();
                return loopF();
            }
            else {
                (0, env_1.withEnvironmentF)(env, () => {
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
exports.repl = repl;
/**
 * Make an environment for the REPL.
 */
function makeReplEnvironment(bindings = []) {
    return new env_1.LispEnvironment(bindings, language_1.langEnvironment);
}
/**
 * Whether `exp` is a command for quitting the REPL.
 */
function helpCmdP(exp) {
    return (0, equal_1.equalp_)(exp, [[Symbol.for('unquote'), Symbol.for('h')]]) || (0, equal_1.equalp_)(exp, [[Symbol.for('unquote'), Symbol.for('help')]]) || (0, equal_1.equalp_)(exp, [[Symbol.for('help')]]);
}
/**
 * Whether `exp` is a command for quitting the REPL.
 */
function quitCmdP(exp) {
    return (0, equal_1.equalp_)(exp, [[Symbol.for('unquote'), Symbol.for('q')]]) || (0, equal_1.equalp_)(exp, [[Symbol.for('unquote'), Symbol.for('quit')]]) || (0, equal_1.equalp_)(exp, [[Symbol.for('quit')]]) || (0, equal_1.equalp_)(exp, [[Symbol.for('unquote'), Symbol.for('exit')]]) || (0, equal_1.equalp_)(exp, [[Symbol.for('exit')]]) || (0, equal_1.equalp_)(exp, [[Symbol.for('unquote'), Symbol.for('x')]]);
}
/**
 * Print a value.
 */
function printValue(x, options = {}) {
    return console.log((0, language_1.printSexpAsExpression)(x, options));
}
/**
 * Display help message.
 */
function help() {
    return console.log(replHelpMessage);
}
