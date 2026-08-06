/**
 * # Parser
 *
 * S-expression parsing.
 *
 * ## Description
 *
 * A simple S-expression parser that returns an S-expression wrapped
 * in a rose tree, i.e., a syntax object. Metadata that is not part of
 * the S-expression, such as comments, is stored in the wrapper instead.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
/**
 * Parse a string of Lisp code and return an S-expression.
 */
declare function read(input: any): any;
declare namespace read {
    var fsource: (symbol | symbol[])[];
}
/**
 * Parse a string of Lisp code and return an S-expression.
 */
declare function readSexp(str: any, options?: any): any;
declare namespace readSexp {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Parse a string of Lisp code and return a syntax object.
 */
declare function readSyntax(str: any, options?: any): any;
declare namespace readSyntax {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Convert a string of Lisp code to an array of tokens.
 * For example, the string:
 *
 *     '((lambda (x) x) "Lisp")'
 *
 * Is converted to the array:
 *
 *     [s`(`, s`(`, s`lambda`, s`(`, s`x`, s`)`, s`x`, s`)`, 'Lisp', s`)`]
 *
 * The output of this function is passed to `parse-syntax`.
 */
declare function tokenize(str: any, options?: any): any;
declare namespace tokenize {
    var fsource: (symbol | (number | symbol)[] | (string | symbol)[] | (symbol | (boolean | symbol)[])[] | (symbol | (symbol | (string | symbol)[])[] | (symbol | ((string | symbol)[] | (symbol | (string | symbol | (number | symbol)[])[])[])[] | ((string | symbol)[] | (symbol | (string | symbol)[][] | (symbol | (symbol | (symbol | (string | symbol)[])[] | ((symbol | (number | symbol)[])[] | (string | symbol)[])[] | ((string | symbol)[] | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | ((symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (string | symbol | (symbol | (number | symbol)[])[])[])[])[])[])[])[])[])[])[] | ((string | symbol)[] | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | ((string | symbol)[] | (symbol | (string | symbol)[] | (symbol | (string | symbol)[])[])[])[] | ((symbol | (number | symbol)[])[] | (string | symbol)[] | (symbol | (symbol | (number | symbol)[])[])[])[])[])[] | ((string | symbol)[] | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | ((symbol | (number | symbol)[])[] | (string | symbol)[])[] | ((string | symbol)[] | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | ((symbol | (number | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[])[] | (symbol | (symbol | (symbol | symbol[])[])[] | ((string | symbol)[] | (symbol | (string | symbol)[])[])[])[])[])[])[])[])[] | ((string | symbol)[] | (symbol | (symbol | (symbol | (number | symbol)[])[])[] | ((string | symbol)[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | (string | symbol)[])[] | (symbol | (symbol | (number | symbol)[])[] | (symbol | (string | symbol)[])[])[])[])[])[])[])[])[];
}
/**
 * Take the array of tokens produced by `tokenize` and make a
 * syntax object that corresponds to the structure of the Lisp code.
 *
 * For example,
 *
 *     [s`(`, s`(`, s`lambda`, s`(`, s`x`, s`)`, s`x`, s`)`, 'Lisp', s`)`]
 *
 * is transformed into a syntax object containing the value:
 *
 *     [[s`lambda`, [s`x`], s`x`], 'Lisp']
 *
 * Which corresponds to:
 *
 *     ((lambda (x) x) "Lisp")
 *
 * The output of this function is a S-expression wrapped in a
 * syntax object.
 */
declare function parseSyntax(tokens: any, options?: any): any;
declare namespace parseSyntax {
    var fsource: (symbol | (symbol | undefined)[] | (symbol | (symbol | (symbol | (symbol | undefined)[])[])[])[] | (symbol | (symbol | (number | symbol | symbol[])[])[][] | (symbol | (symbol | (symbol | symbol[])[])[] | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[] | ((string | symbol)[] | (symbol | undefined)[] | (symbol | (symbol | (number | symbol | symbol[])[] | (symbol | undefined[])[])[])[])[])[][])[])[])[];
}
/**
 * Take the array of tokens produced by `tokenize` and make a
 * nested array that corresponds to the structure of the Lisp code.
 *
 * The output of this function is a fully valid S-expression which
 * can be evaluated in a Lisp environment.
 */
declare function parseSexp(tokens: any, options?: any): any;
declare namespace parseSexp {
    var fsource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Whether `comment` is a `;;`-comment (level 2),
 * a `;;;`-comment (level 3), or some other level.
 */
declare function getCommentLevel(comment: any): any;
declare namespace getCommentLevel {
    var fsource: (symbol | (symbol | (symbol | (string | symbol)[])[])[])[];
}
/**
 * Get the comment level, e.g., 2 for a `;;`-comment,
 * 3 for a `;;;`-comment, etc.
 */
declare function commentLevelP(comment: any, level: any): any;
declare namespace commentLevelP {
    var fsource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Token class.
 */
declare class Token {
    /**
     * The type of the token.
     */
    tag: any;
    /**
     * The value of the token.
     */
    value: any;
    /**
     * Make a token.
     */
    constructor(value?: any, tag?: any);
    /**
     * Get the token tag
     * (i.e., its type).
     */
    getTag(): any;
    /**
     * Get the value of the token.
     */
    getValue(): any;
    /**
     * Set the token tag
     * (i.e., its type).
     */
    setTag(tag: any): any;
    /**
     * Set the value of the token.
     */
    setValue(value: any): any;
}
/**
 * Token class for representing comments.
 */
declare class CommentToken extends Token {
    constructor(value: any, tag?: any);
}
/**
 * Token class for representing leading comments.
 */
declare class LeadingCommentToken extends CommentToken {
    constructor(value: any);
}
/**
 * Token class for representing trailing comments.
 */
declare class TrailingCommentToken extends CommentToken {
    constructor(value: any);
}
/**
 * Token class for representing numbers.
 */
declare class NumberToken extends Token {
    constructor(value: any);
}
/**
 * Token class for representing strings.
 */
declare class StringToken extends Token {
    constructor(value: any);
}
/**
 * Token class for representing symbols.
 */
declare class SymbolToken extends Token {
    constructor(value: any);
}
export { parseSyntax as parseRose, readSyntax as readRose, CommentToken, LeadingCommentToken, NumberToken, StringToken, SymbolToken, Token, TrailingCommentToken, commentLevelP, getCommentLevel, parseSexp, parseSyntax, read, readSexp, readSyntax, tokenize };
