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
exports.repl = void 0;
const process_1 = require("process");
const readline = require("readline");
const constants_1 = require("./constants");
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
    function quit() {
        if (!quitFlag) {
            quitFlag = true;
            return rl.close();
        }
    }
    const env = makeReplEnvironment([[Symbol.for('exit'), quit, 'function'], [Symbol.for('help'), help, 'function'], [Symbol.for('quit'), quit, 'function']]);
    // Read-eval-print loop
    function loopF(...args) {
        function callback(x) {
            if (quitFlag || isQuitCmdP(x)) {
                return quit();
            }
            else if (isHelpCmdP(x)) {
                help();
                return loopF();
            }
            else {
                let result = undefined;
                // Read (R).
                const exp = (0, parser_1.read)(x);
                // Evaluate (E).
                try {
                    result = (0, language_1.interpret)(exp, env);
                    // Print (P).
                    printValue(result);
                }
                catch (e) {
                    if (e instanceof Error) {
                        console.log(e);
                    }
                    else {
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
exports.repl = repl;
/**
 * Make an environment for the REPL.
 */
function makeReplEnvironment(bindings) {
    return new env_1.LispEnvironment(bindings, language_1.langEnvironment);
}
/**
 * Whether `str` is a command for quitting the REPL.
 */
function isHelpCmdP(str) {
    return [',h', ',help', '(help)'].includes(str);
}
/**
 * Whether `str` is a command for quitting the REPL.
 */
function isQuitCmdP(str) {
    return [',q', ',quit', '(quit)', ',exit', '(exit)'].includes(str);
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
