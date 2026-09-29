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
/**
 * REPL class.
 *
 * Encapsulates a [`readline`][node:readline] instance that reads
 * from standard input.
 *
 * [node:readline]: https://nodejs.org/api/readline.html
 */
declare class REPL {
    /**
     * REPL prompt.
     */
    private prompt;
    /**
     * REPL state.
     */
    private state;
    /**
     * REPL user input.
     */
    private input;
    /**
     * REPL history.
     *
     * A list of strings. The latest entry is stored
     * at the beginning of the list.
     */
    private history;
    /**
     * Flag for printing values.
     */
    private printFlag;
    /**
     * Flag for quitting.
     */
    private quitFlag;
    /**
     * `readline` instance.
     */
    private rl;
    /**
     * `readline` history.
     */
    private rlHistory;
    /**
     * The REPL environment.
     */
    private env;
    /**
     * Message displayed when starting the REPL.
     */
    private startupMessage;
    /**
     * Help message displayed by the REPL's `help` command.
     */
    private helpMessage;
    /**
     * Create a new REPL.
     */
    constructor();
    /**
     * Start the REPL.
     */
    start(): any;
    /**
     * Read--eval--print method.
     */
    rep(input: any, env?: any): any;
    /**
     * Read a line of user input.
     */
    private readLine;
    /**
     * Print startup message.
     */
    private printStartupMessage;
}
/**
 * Read--eval--print function.
 */
declare function rep(input: any): any;
/**
 * Read--eval--print--loop function.
 */
declare function repl(): void;
export { REPL, rep, repl };
