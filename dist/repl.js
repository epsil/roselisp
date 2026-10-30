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
exports.stringToHistory = exports.repl = exports.rep = exports.historyToString = exports.REPL = void 0;
const child_process_1 = require("child_process");
const fs_1 = require("fs");
const os_1 = require("os");
const path_1 = require("path");
const process_1 = require("process");
const readline = require("readline");
const constants_1 = require("./constants");
const core_1 = require("./core");
const env_1 = require("./env");
const parser_1 = require("./parser");
const util_1 = require("./util");
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
    constructor(options = {}) {
        /**
         * Flag for printing values.
         */
        this.printFlag = true;
        /**
         * Flag for quitting.
         */
        this.quitFlag = false;
        /**
         * The current module, if any.
         */
        this.currentModule = undefined;
        /**
         * REPL options.
         */
        this.options = {};
        /**
         * REPL prompt string.
         */
        this.promptStr = '> ';
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
         * A list of strings, with the latest entry stored
         * at the beginning of the list.
         */
        this.history = [];
        /**
         * `readline` instance.
         */
        this.rl = undefined;
        /**
         * `readline` history.
         */
        this.rlHistory = [];
        /**
         * Absolute path to `~/.roselisp_history`, where
         * persistent history is stored.
         */
        this.historyFile = (0, path_1.join)((0, os_1.homedir)(), '.' + constants_1.packageName + '_history');
        /**
         * Message displayed when starting the REPL.
         */
        this.startupMessage = ';; Roselisp version ' + constants_1.version + '.\n' +
            ';; Type ,h for help and ,q to quit.';
        // Whether to echo user input.
        this.echo = true;
        this.options = this.normalizeOptions(options);
        this.env = makeInteractiveEnvironment(this);
        this.toplevel = this.env;
    }
    /**
     * Start the REPL.
     */
    start(options = this.options) {
        this.echo = !options['sparseRepl'];
        if (options['replHistory']) {
            this.history = this.readHistory();
            this.rlHistory = [...this.history];
        }
        // Initialize the `readline` instance.
        this.rl = readline.createInterface({
            input: options['input'] || process_1.stdin,
            history: this.rlHistory,
            output: this.echo ? (options['output'] || process_1.stdout) : undefined
        });
        // Register a listener for the `line` event that will
        // receive a single line of user input and take
        // appropriate action.
        this.rl.on('line', (input) => withRepl(this, () => (0, env_1.withEnvironment)(this.env, () => {
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
                return this.prompt();
            }
            else {
                if (this.printFlag && (result !== '')) {
                    console.log(result);
                }
                return this.prompt();
            }
        })));
        // Register a listener for the `close` event that will
        // terminate the output properly.
        this.rl.on('close', () => {
            if (!this.echo) {
                return process_1.stdout.write('\n');
            }
        });
        // Start the read--eval--print loop.
        this.printStartupMessage();
        return this.prompt();
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
        // for multiple expressions to be entered at a single prompt
        // (e.g., `> (+ 1 1) (+ 2 2)`), so that what is processed here
        // is not a single expression, but rather a list of expressions.
        let parsedExpressions = [];
        try {
            parsedExpressions = (0, parser_1.read)('(' + input + ')');
        }
        catch (e) {
            if (e instanceof Error) {
                if (e.message === 'eof') {
                    // If we are reading a multi-line expression,
                    // then continue listening for user input.
                    this.state = 'read';
                    return '';
                }
                else {
                    throw e;
                }
            }
            else {
                throw e;
            }
        }
        // Update the REPL history.
        this.history.unshift(input);
        // Synchronize the `readline` instance's history with the
        // REPL history. This is needed in order to handle multi-line
        // expressions correctly, because otherwise the `readline`
        // instance will create one history item per line.
        (0, util_1.copyIntoArrayX)(this.history, this.rlHistory);
        // Rewrite the expressions slightly in order to handle REPL
        // shortcuts such as `,h` and `,q`.
        const rewrittenExpressions = rewriteExpressions(parsedExpressions);
        // Evaluate the expressions one by one, producing a list
        // of values.
        const evaluatedExpressions = [];
        for (let exp of rewrittenExpressions) {
            if (this.quitFlag) {
                break;
            }
            try {
                evaluatedExpressions.push((0, core_1.interpret)(exp, env, this.options));
            }
            catch (e) {
                console.log(e);
            }
        }
        // Continue unless the user has quit. If the user has quit,
        // the return value is just the empty string.
        let result = '';
        if (!this.quitFlag) {
            if (this.options['replHistory']) {
                this.writeHistoryEntry(input);
            }
            // Print the values, producing a list of value strings.
            const printedExpressions = [];
            for (let x of evaluatedExpressions) {
                if (x === undefined) {
                    // Don't print `#u`. FIXME: A better way to determine this
                    // would be to check if the original expression called a
                    // function whose function type returns `Void`.
                    continue;
                }
                else {
                    printedExpressions.push((0, core_1.printSexpAsExpression)(x));
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
    normalizeOptions(options) {
        let result = Object.assign({}, options);
        return result;
    }
    /**
     * Print startup message.
     */
    printStartupMessage() {
        return console.log(this.startupMessage);
    }
    /**
     * Read a line of user input.
     */
    prompt() {
        const promptStr = (this.state === 'read') ? '' : this.promptStr;
        this.rl.setPrompt(promptStr);
        if (!this.echo) {
            process_1.stdout.write(promptStr);
        }
        return this.rl.prompt();
    }
    /**
     * Read persistent history.
     */
    readHistory(str = '') {
        let history = str;
        try {
            history = (0, fs_1.readFileSync)(this.historyFile, {
                encoding: 'utf8'
            });
        }
        catch (_a) {
        }
        return stringToHistory(history);
    }
    /**
     * Write persistent history.
     */
    writeHistory(history = this.history) {
        try {
            return (0, fs_1.writeFileSync)(this.historyFile, historyToString(history), {
                encoding: 'utf8'
            });
        }
        catch (_a) {
        }
    }
    /**
     * Append an entry to the persistent history.
     */
    writeHistoryEntry(entry) {
        try {
            return (0, fs_1.writeFileSync)(this.historyFile, entry + '\n', {
                encoding: 'utf8',
                flag: 'a'
            });
        }
        catch (_a) {
        }
    }
}
exports.REPL = REPL;
/**
 * List current bindings.
 */
function binding() {
    const env = currentRepl().env;
    for (let entry of env.entries()) {
        const sym = entry[0];
        const symPrinted = (0, core_1.printSexp)(sym);
        const [val] = entry[1];
        const valPrinted = (0, core_1.printSexpAsExpression)(val);
        console.log(symPrinted, '		', valPrinted);
    }
}
/**
 * Change the current directory.
 */
function cd(dir) {
    try {
        (0, process_1.chdir)(dir);
        pwd();
    }
    catch (e) {
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
function edit(...args) {
    const cmd = ['${EDITOR:-editor}', ...(Array.isArray(args) ? args : [Symbol.for('.'), args])].join(' ');
    try {
        (0, child_process_1.execSync)(cmd, {
            shell: '/bin/bash',
            stdio: 'inherit'
        });
    }
    catch (e) {
        console.log(e);
    }
}
/**
 * Enter a module.
 */
function enterx(x = undefined) {
    if (x === undefined) {
        const m = currentRepl().currentModule;
        if (m) {
            enterx((0, path_1.join)(m.modulePath, m.name));
        }
    }
    else if (!x) {
        toplevel();
    }
    else {
        const m = (0, core_1.loadModule)(x, (0, env_1.currentEnvironment)());
        if (m) {
            currentRepl().env = m.getExportEnvironment();
        }
    }
}
/**
 * Display help message.
 */
function help() {
    currentRepl().printFlag = false;
    console.log(`;; Enter an S-expression to evaluate it.
;; Use the up and down keys to access previous expressions.
;;
;; Type ,q to quit.`);
}
/**
 * List the contents of the current directory.
 */
function ls(...args) {
    shell(...['ls', ...(Array.isArray(args) ? args : [Symbol.for('.'), args])]);
}
/**
 * Print the current directory.
 */
function pwd() {
    console.log((0, process_1.cwd)());
}
/**
 * Execute a shell command.
 */
function shell(...args) {
    const cmd = args.join(' ');
    try {
        (0, child_process_1.execSync)(cmd, {
            stdio: 'inherit'
        });
    }
    catch (e) {
        console.log(e);
    }
}
/**
 * Go back to the toplevel.
 */
function toplevel() {
    currentRepl().env = currentRepl().toplevel;
}
/**
 * Quit the REPL.
 */
function quit() {
    currentRepl().printFlag = false;
    currentRepl().quitFlag = true;
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
function repl(options = {}) {
    new REPL(options).start();
}
exports.repl = repl;
/**
 * Rewrite `(unquote ...)` expressions to regular
 * function calls.
 */
function rewriteExpressions(exp) {
    if (Array.isArray(exp) && (exp.length >= 1) && Array.isArray(exp[0]) && (exp[0].length === 2) && (exp[0][0] === Symbol.for('unquote'))) {
        let [[, x], ...y] = exp;
        // Aliases.
        if (x === Symbol.for('b')) {
            x = Symbol.for('binding');
        }
        else if ([Symbol.for('e'), Symbol.for('ed')].includes(x)) {
            x = Symbol.for('edit');
        }
        else if ([Symbol.for('en'), Symbol.for('enter')].includes(x)) {
            x = Symbol.for('enter!');
        }
        else if (x === Symbol.for('h')) {
            x = Symbol.for('help');
        }
        else if (x === Symbol.for('l')) {
            x = Symbol.for('load');
        }
        else if (x === Symbol.for('m')) {
            x = Symbol.for('module');
        }
        else if ([Symbol.for('req'), Symbol.for('r')].includes(x)) {
            x = Symbol.for('require');
        }
        else if ([Symbol.for('ex'), Symbol.for('x'), Symbol.for('q')].includes(x)) {
            x = Symbol.for('quit');
        }
        // Rewrite an expression like `,cd ..`
        // to `(cd "..")`.
        return [[x, ...y.map((x) => {
                    if (typeof x === 'symbol') {
                        return x.description;
                    }
                    else {
                        return x + '';
                    }
                })]];
    }
    else if (Array.isArray(exp) && (exp.length === 0)) {
        const [] = exp;
        return [[Symbol.for('help')]];
    }
    else {
        return exp;
    }
}
/**
 * Make an environment for the REPL.
 */
function makeInteractiveEnvironment(repl, bindings = []) {
    const parent = core_1.langEnvironment;
    const env = new env_1.LispEnvironment([[Symbol.for('binding'), binding, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Void')]]], [Symbol.for('bindings'), binding, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Void')]]], [Symbol.for('cd'), cd, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Void')]]], [Symbol.for('edit'), edit, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('enter!'), enterx, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('Void')]], [Symbol.for('exit'), quit, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('help'), help, [Symbol.for('->'), Symbol.for('Void')]], [Symbol.for('load'), core_1.load_, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('ls'), ls, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('*'), Symbol.for('Any')]]], [Symbol.for('module'), enterx, [Symbol.for('->'), Symbol.for('Any'), Symbol.for('Void')]], [Symbol.for('pwd'), pwd, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Void')]]], [Symbol.for('quit'), quit, [Symbol.for('->'), Symbol.for('Void')]], [Symbol.for('shell'), shell, [Symbol.for('quote'), [Symbol.for('->'), Symbol.for('Void')]]], [Symbol.for('toplevel'), toplevel, [Symbol.for('->'), Symbol.for('Void')]], ...bindings], parent);
    const env1 = new env_1.LispEnvironment([], env);
    return env1;
}
/**
 * Pointer to the current REPL instance.
 */
let currentReplPointer = undefined;
/**
 * Force the thunk `f` with `current-repl-pointer`
 * bound to `repl`. The previous value of
 * `current-repl-pointer` is restored afterwards.
 */
function withRepl(repl, f) {
    let result = undefined;
    const tmp = currentReplPointer;
    try {
        currentReplPointer = repl;
        result = f();
    }
    finally {
        currentReplPointer = tmp;
    }
    return result;
}
/**
 * Return the current REPL instance.
 * Used by REPL meta-commands to
 * communicate with the REPL.
 */
function currentRepl() {
    return currentReplPointer;
}
/**
 * Convert a history object to a string.
 * The reverse of `string->history`.
 */
function historyToString(history) {
    return [...history].reverse().join('\n');
}
exports.historyToString = historyToString;
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
function stringToHistory(str) {
    // Note that since the format is so simple, this function has to be
    // fairly error-tolerant so it can deal gracefully with syntax
    // errors in the user input. With a more robust format for
    // persistent history (e.g., serializing the history as an
    // S-expression), this would not be needed at all; nevertheless, we
    // have opted to go with a simple but brittle format for the sake of
    // convention.
    let result = [];
    const spaceRegexp = /^\s*$/;
    let buffer = '';
    const exp = undefined;
    const lines = str.split('\n');
    for (let line of lines) {
        buffer = (buffer === '') ? line : (buffer + '\n' +
            line);
        try {
            (0, parser_1.read)(buffer);
            result.unshift(buffer);
            buffer = '';
        }
        catch (e) {
            if (e instanceof Error) {
                if (e.message !== 'eof') {
                    if (!buffer.match(spaceRegexp)) {
                        result.unshift(buffer);
                    }
                    buffer = '';
                }
            }
            else {
                throw e;
            }
        }
    }
    return result;
}
exports.stringToHistory = stringToHistory;
