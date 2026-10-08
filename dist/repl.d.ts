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
     * Flag for printing values.
     */
    printFlag: any;
    /**
     * Flag for quitting.
     */
    quitFlag: any;
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
    currentModule: any;
    /**
     * REPL options.
     */
    private options;
    /**
     * REPL prompt string.
     */
    private promptStr;
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
     * A list of strings, with the latest entry stored
     * at the beginning of the list.
     */
    private history;
    /**
     * `readline` instance.
     */
    private rl;
    /**
     * `readline` history.
     */
    private rlHistory;
    /**
     * Absolute path to `~/.roselisp_history`, where
     * persistent history is stored.
     */
    private historyFile;
    /**
     * Message displayed when starting the REPL.
     */
    private startupMessage;
    private echo;
    /**
     * Create a new REPL.
     */
    constructor(options?: any);
    /**
     * Start the REPL.
     */
    start(options?: any): any;
    /**
     * Read--eval--print method.
     */
    rep(input: any, env?: any): any;
    /**
     * Normalize options.
     */
    private normalizeOptions;
    /**
     * Print startup message.
     */
    private printStartupMessage;
    /**
     * Read a line of user input.
     */
    private prompt;
    /**
     * Read persistent history.
     */
    private readHistory;
    /**
     * Write persistent history.
     */
    private writeHistory;
    /**
     * Append an entry to the persistent history.
     */
    private writeHistoryEntry;
}
/**
 * Read--eval--print function.
 */
declare function rep(input: any): any;
/**
 * Read--eval--print--loop function.
 */
declare function repl(options?: any): void;
/**
 * Convert a history object to a string.
 * The reverse of `string->history`.
 */
declare function historyToString(history: any): any;
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
declare function stringToHistory(str: any): any;
export { REPL, historyToString, rep, repl, stringToHistory };
