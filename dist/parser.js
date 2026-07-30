"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Parser
 *
 * S-expression parsing.
 *
 * ## Description
 *
 * A simple S-expression parser that returns an S-expression wrapped
 * in a rose tree. Metadata that is not part of the S-expression,
 * such as comments, is stored in the rose tree wrapper instead.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.tokenize = exports.readSexp = exports.readRose = exports.read = exports.parseSexp = exports.parseRose = exports.getCommentLevel = exports.commentLevelP = exports.TrailingCommentToken = exports.Token = exports.SymbolToken = exports.StringToken = exports.NumberToken = exports.LeadingCommentToken = exports.CommentToken = void 0;
const constants_1 = require("./constants");
const rose_1 = require("./rose");
/**
 * Parse a string of Lisp code and return an S-expression.
 */
function read(input) {
    return readSexp(input);
}
exports.read = read;
read.fsource = [Symbol.for('define'), [Symbol.for('read'), Symbol.for('input')], [Symbol.for('read-sexp'), Symbol.for('input')]];
/**
 * Parse a string of Lisp code and return an S-expression.
 */
function readSexp(str, options = {}) {
    return (0, rose_1.roseToSexp)(readRose(str, options));
}
exports.readSexp = readSexp;
readSexp.fsource = [Symbol.for('define'), [Symbol.for('read-sexp'), Symbol.for('str'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('read-rose'), Symbol.for('_'), Symbol.for('options')], [Symbol.for('rose->sexp'), Symbol.for('_')]]];
/**
 * Parse a string of Lisp code and return an S-expression
 * wrapped in a rose tree.
 */
function readRose(str, options = {}) {
    // Parsing is implemented in two stages: a lexical analysis stage
    // (`tokenize`) and a syntax analysis stage (`parse-rose`).
    // The lexical analysis stage converts a string to a stream of
    // tokens, which is represented as an array of Lisp symbols.
    // The syntax analysis stage converts the token stream to a rose
    // tree, which contains an S-expression that can be evaluated.
    return parseRose(tokenize(str, options), options);
}
exports.readRose = readRose;
readRose.fsource = [Symbol.for('define'), [Symbol.for('read-rose'), Symbol.for('str'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('tokenize'), Symbol.for('_'), Symbol.for('options')], [Symbol.for('parse-rose'), Symbol.for('_'), Symbol.for('options')]]];
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
 * The output of this function is passed to `parse-rose`.
 */
function tokenize(str, options = {}) {
    let comments = options['comments'];
    if (comments === undefined) {
        comments = true;
    }
    let pos = 0;
    const len = str.length;
    let char = '';
    let buffer = '';
    const result = [];
    let state = 'start';
    while (state !== 'stop') {
        if (state === 'start') {
            state = (len === 0) ? 'stop' : 'read';
        }
        else if (state === 'read') {
            if (pos >= len) {
                state = 'stop';
            }
            else {
                char = str[pos];
                if (whitespacep(char)) {
                    pos++;
                }
                else if (char === '(') {
                    result.push(new SymbolToken(char));
                    pos++;
                }
                else if (char === ')') {
                    result.push(new SymbolToken(char));
                    pos++;
                }
                else if (char === '"') {
                    state = 'string';
                    pos++;
                }
                else if (commentp(char)) {
                    state = 'comment';
                }
                else if (char === '\'') {
                    result.push(new SymbolToken(char));
                    pos++;
                }
                else if (char === '`') {
                    result.push(new SymbolToken(char));
                    pos++;
                }
                else if (char === ',') {
                    if ((pos < len) && (str[pos + 1] === '@')) {
                        const nextToken = str[pos + 1];
                        result.push(new SymbolToken(char + nextToken));
                        pos = pos + 2;
                    }
                    else {
                        result.push(new SymbolToken(char));
                        pos++;
                    }
                }
                else {
                    state = 'symbol';
                }
            }
        }
        else if (state === 'symbol') {
            char = str[pos];
            if ((pos >= len) || char.match(new RegExp('\\s')) || (char === ')')) {
                const num = parseFloat(buffer);
                if (!isNaN(num)) {
                    result.push(new NumberToken(num));
                }
                else {
                    result.push(new SymbolToken(buffer));
                }
                buffer = '';
                state = 'read';
            }
            else if (char === '\\') {
                char = str[pos + 1];
                buffer = buffer + char;
                pos = pos + 2;
            }
            else {
                buffer = buffer + char;
                pos++;
            }
        }
        else if (state === 'string') {
            char = str[pos];
            if (pos >= len) {
                result.push(new StringToken(buffer));
                buffer = '';
                state = 'read';
            }
            else if (char === '\\') {
                if (pos < len) {
                    const nextToken = str[pos + 1];
                    if (nextToken === 'n') {
                        buffer = buffer + '\n';
                    }
                    else if (nextToken === 't') {
                        buffer = buffer + '	';
                    }
                    else if (nextToken === 'r') {
                        buffer = buffer + '\n';
                    }
                    else {
                        buffer = buffer + nextToken;
                    }
                    pos = pos + 2;
                }
                else {
                    pos++;
                }
            }
            else if (char === '"') {
                pos++;
                result.push(new StringToken(buffer));
                buffer = '';
                state = 'read';
            }
            else {
                buffer = buffer + char;
                pos++;
            }
        }
        else if (state === 'comment') {
            char = str[pos];
            if (pos >= len) {
                if (comments) {
                    result.push(new LeadingCommentToken(removeIndentation(buffer)));
                }
                buffer = '';
                state = 'stop';
            }
            else if (char === '\n') {
                while ((char === '\n') &&
                    (pos < len)) {
                    buffer = buffer + char;
                    pos++;
                    char = str[pos];
                }
                // Skip past indentation on the next line and see if there
                // is another leading comment; if so, merge it into this.
                while (indentationp(char)) {
                    pos++;
                    char = str[pos];
                }
                if (!commentp(char)) {
                    // Exit `comment` state.
                    if (comments) {
                        result.push(new LeadingCommentToken(removeIndentation(buffer)));
                    }
                    buffer = '';
                    state = 'read';
                }
            }
            else {
                buffer = buffer + char;
                pos++;
            }
        }
    }
    return result;
}
exports.tokenize = tokenize;
tokenize.fsource = [Symbol.for('define'), [Symbol.for('tokenize'), Symbol.for('str'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('when'), [Symbol.for('undefined?'), Symbol.for('comments')], [Symbol.for('set!'), Symbol.for('comments'), true]], [Symbol.for('define'), Symbol.for('pos'), 0], [Symbol.for('define'), Symbol.for('len'), [Symbol.for('js/length'), Symbol.for('str')]], [Symbol.for('define'), Symbol.for('char'), ''], [Symbol.for('define'), Symbol.for('buffer'), ''], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('state'), 'start'], [Symbol.for('while'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('state'), 'stop']], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('state'), 'start'], [Symbol.for('set!'), Symbol.for('state'), [Symbol.for('if'), [Symbol.for('='), Symbol.for('len'), 0], 'stop', 'read']]], [[Symbol.for('eq?'), Symbol.for('state'), 'read'], [Symbol.for('cond'), [[Symbol.for('>='), Symbol.for('pos'), Symbol.for('len')], [Symbol.for('set!'), Symbol.for('state'), 'stop']], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('char'), [Symbol.for('aget'), Symbol.for('str'), Symbol.for('pos')]], [Symbol.for('cond'), [[Symbol.for('whitespace?'), Symbol.for('char')], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]]], [[Symbol.for('eq?'), Symbol.for('char'), '('], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('SymbolToken'), Symbol.for('char')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]]], [[Symbol.for('eq?'), Symbol.for('char'), ')'], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('SymbolToken'), Symbol.for('char')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]]], [[Symbol.for('eq?'), Symbol.for('char'), '"'], [Symbol.for('set!'), Symbol.for('state'), 'string'], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]]], [[Symbol.for('comment?'), Symbol.for('char')], [Symbol.for('set!'), Symbol.for('state'), 'comment']], [[Symbol.for('eq?'), Symbol.for('char'), '\''], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('SymbolToken'), Symbol.for('char')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]]], [[Symbol.for('eq?'), Symbol.for('char'), '`'], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('SymbolToken'), Symbol.for('char')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]]], [[Symbol.for('eq?'), Symbol.for('char'), ','], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('<'), Symbol.for('pos'), Symbol.for('len')], [Symbol.for('eq?'), [Symbol.for('aget'), Symbol.for('str'), [Symbol.for('+'), Symbol.for('pos'), 1]], '@']], [Symbol.for('define'), Symbol.for('next-token'), [Symbol.for('aget'), Symbol.for('str'), [Symbol.for('+'), Symbol.for('pos'), 1]]], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('SymbolToken'), [Symbol.for('string-append'), Symbol.for('char'), Symbol.for('next-token')]]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 2]]], [Symbol.for('else'), [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('SymbolToken'), Symbol.for('char')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('state'), 'symbol']]]]]], [[Symbol.for('eq?'), Symbol.for('state'), 'symbol'], [Symbol.for('set!'), Symbol.for('char'), [Symbol.for('aget'), Symbol.for('str'), Symbol.for('pos')]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('>='), Symbol.for('pos'), Symbol.for('len')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\s'], Symbol.for('char')], [Symbol.for('eq?'), Symbol.for('char'), ')']], [Symbol.for('define'), Symbol.for('num'), [Symbol.for('parse-float'), Symbol.for('buffer')]], [Symbol.for('if'), [Symbol.for('not'), [Symbol.for('is-NaN'), Symbol.for('num')]], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('NumberToken'), Symbol.for('num')]], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('SymbolToken'), Symbol.for('buffer')]]], [Symbol.for('set!'), Symbol.for('buffer'), ''], [Symbol.for('set!'), Symbol.for('state'), 'read']], [[Symbol.for('eq?'), Symbol.for('char'), '\\'], [Symbol.for('set!'), Symbol.for('char'), [Symbol.for('aget'), Symbol.for('str'), [Symbol.for('+'), Symbol.for('pos'), 1]]], [Symbol.for('set!'), Symbol.for('buffer'), [Symbol.for('string-append'), Symbol.for('buffer'), Symbol.for('char')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 2]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('buffer'), [Symbol.for('string-append'), Symbol.for('buffer'), Symbol.for('char')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]]]]], [[Symbol.for('eq?'), Symbol.for('state'), 'string'], [Symbol.for('set!'), Symbol.for('char'), [Symbol.for('aget'), Symbol.for('str'), Symbol.for('pos')]], [Symbol.for('cond'), [[Symbol.for('>='), Symbol.for('pos'), Symbol.for('len')], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('StringToken'), Symbol.for('buffer')]], [Symbol.for('set!'), Symbol.for('buffer'), ''], [Symbol.for('set!'), Symbol.for('state'), 'read']], [[Symbol.for('eq?'), Symbol.for('char'), '\\'], [Symbol.for('cond'), [[Symbol.for('<'), Symbol.for('pos'), Symbol.for('len')], [Symbol.for('define'), Symbol.for('next-token'), [Symbol.for('aget'), Symbol.for('str'), [Symbol.for('+'), Symbol.for('pos'), 1]]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('next-token'), 'n'], [Symbol.for('set!'), Symbol.for('buffer'), [Symbol.for('string-append'), Symbol.for('buffer'), '\n']]], [[Symbol.for('eq?'), Symbol.for('next-token'), 't'], [Symbol.for('set!'), Symbol.for('buffer'), [Symbol.for('string-append'), Symbol.for('buffer'), '	']]], [[Symbol.for('eq?'), Symbol.for('next-token'), 'r'], [Symbol.for('set!'), Symbol.for('buffer'), [Symbol.for('string-append'), Symbol.for('buffer'), '\n']]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('buffer'), [Symbol.for('string-append'), Symbol.for('buffer'), Symbol.for('next-token')]]]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 2]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]]]]], [[Symbol.for('eq?'), Symbol.for('char'), '"'], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('StringToken'), Symbol.for('buffer')]], [Symbol.for('set!'), Symbol.for('buffer'), ''], [Symbol.for('set!'), Symbol.for('state'), 'read']], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('buffer'), [Symbol.for('string-append'), Symbol.for('buffer'), Symbol.for('char')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]]]]], [[Symbol.for('eq?'), Symbol.for('state'), 'comment'], [Symbol.for('set!'), Symbol.for('char'), [Symbol.for('aget'), Symbol.for('str'), Symbol.for('pos')]], [Symbol.for('cond'), [[Symbol.for('>='), Symbol.for('pos'), Symbol.for('len')], [Symbol.for('when'), Symbol.for('comments'), [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('LeadingCommentToken'), [Symbol.for('remove-indentation'), Symbol.for('buffer')]]]], [Symbol.for('set!'), Symbol.for('buffer'), ''], [Symbol.for('set!'), Symbol.for('state'), 'stop']], [[Symbol.for('eq?'), Symbol.for('char'), '\n'], [Symbol.for('while'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('char'), '\n'], [Symbol.for('<'), Symbol.for('pos'), Symbol.for('len')]], [Symbol.for('set!'), Symbol.for('buffer'), [Symbol.for('string-append'), Symbol.for('buffer'), Symbol.for('char')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]], [Symbol.for('set!'), Symbol.for('char'), [Symbol.for('aget'), Symbol.for('str'), Symbol.for('pos')]]], [Symbol.for('while'), [Symbol.for('indentation?'), Symbol.for('char')], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]], [Symbol.for('set!'), Symbol.for('char'), [Symbol.for('aget'), Symbol.for('str'), Symbol.for('pos')]]], [Symbol.for('unless'), [Symbol.for('comment?'), Symbol.for('char')], [Symbol.for('when'), Symbol.for('comments'), [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('new'), Symbol.for('LeadingCommentToken'), [Symbol.for('remove-indentation'), Symbol.for('buffer')]]]], [Symbol.for('set!'), Symbol.for('buffer'), ''], [Symbol.for('set!'), Symbol.for('state'), 'read']]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('buffer'), [Symbol.for('string-append'), Symbol.for('buffer'), Symbol.for('char')]], [Symbol.for('set!'), Symbol.for('pos'), [Symbol.for('+'), Symbol.for('pos'), 1]]]]]]], Symbol.for('result')];
/**
 * Take the array of tokens produced by `tokenize` and make a
 * rose tree that corresponds to the structure of the Lisp code.
 *
 * For example,
 *
 *     [s`(`, s`(`, s`lambda`, s`(`, s`x`, s`)`, s`x`, s`)`, 'Lisp', s`)`]
 *
 * is transformed into a rose tree containing the value:
 *
 *     [[s`lambda`, [s`x`], s`x`], 'Lisp']
 *
 * Which corresponds to:
 *
 *     ((lambda (x) x) "Lisp")
 *
 * The output of this function is a S-expression wrapped in a
 * rose tree.
 */
function parseRose(tokens, options = {}) {
    // In order to implement this function in a non-recursive way, a
    // stack is needed to keep track of expressions and their
    // subexpressions. Each stack entry is a list
    // `(expression value expression-node value-node)`, where
    // `expression` is an expression and `value` is the value-part
    // of the expression, i.e., the part to insert subexpressions
    // into. The other two values are the corresponding rose tree
    // nodes. In most cases, the value and the expression are one
    // and the same, but for some expressions, like
    // `(quote (1 2))`, the value is a subexpression. When `3` is
    // added to this expression, the result should be
    // `(quote (1 2 3))`, not `(quote (1 2) 3)`.
    const stack = [];
    // Currently parsed expression.
    let exp = undefined;
    // Rose tree node for `exp`.
    let node = undefined;
    // Comments for the currently parsed expression.
    let comments = [];
    // The current expression. The final value
    // of this variable is the return value.
    let currentExp = undefined;
    // Rose tree node for `current-exp`.
    let currentExpNode = undefined;
    // The insertion point of the current expression.
    // For a quoted expression like `(quote ())`,
    // it points to the inner `()`.
    let currentVal = undefined;
    // Rose tree node for `current-val`.
    let currentValNode = undefined;
    // The parent expression.
    let parentExp;
    // Rose tree node for `parent-exp`.
    let parentExpNode;
    // Parent expression insertion point.
    let parentVal;
    // Rose tree node for `parent-val`.
    let parentValNode;
    // Helper function for inserting an expression
    // into another.
    function insertx(val, exp, valNode, expNode) {
        exp.push(val);
        return expNode.insert(valNode);
    }
    insertx.fsource = [Symbol.for('define'), [Symbol.for('insert!'), Symbol.for('val'), Symbol.for('exp'), Symbol.for('val-node'), Symbol.for('exp-node')], [Symbol.for('push-right!'), Symbol.for('exp'), Symbol.for('val')], [Symbol.for('send'), Symbol.for('exp-node'), Symbol.for('insert'), Symbol.for('val-node')]];
    // Helper function for updating current values.
    function updatex(exp, node) {
        if (currentVal) {
            insertx(exp, currentVal, node, currentValNode);
            currentVal = undefined;
            currentValNode = undefined;
        }
        else {
            currentExp = exp;
            currentExpNode = node;
        }
        if (parentExp) {
            return insertx(currentExp, parentVal, currentExpNode, parentValNode);
        }
    }
    updatex.fsource = [Symbol.for('define'), [Symbol.for('update!'), Symbol.for('exp'), Symbol.for('node')], [Symbol.for('cond'), [Symbol.for('current-val'), [Symbol.for('insert!'), Symbol.for('exp'), Symbol.for('current-val'), Symbol.for('node'), Symbol.for('current-val-node')], [Symbol.for('set!'), Symbol.for('current-val'), undefined], [Symbol.for('set!'), Symbol.for('current-val-node'), undefined]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('current-exp'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('current-exp-node'), Symbol.for('node')]]], [Symbol.for('when'), Symbol.for('parent-exp'), [Symbol.for('insert!'), Symbol.for('current-exp'), Symbol.for('parent-val'), Symbol.for('current-exp-node'), Symbol.for('parent-val-node')]]];
    // Iterate over the list of tokens.
    const _end = tokens.length;
    for (let i = 0; i < _end; i++) {
        // The current token.
        const token = tokens[i];
        if (token instanceof CommentToken) {
            // Comments.
            if (token instanceof LeadingCommentToken) {
                comments.push(token);
            }
        }
        else if (token instanceof SymbolToken) {
            // Symbolic values.
            const tokenString = token.getValue();
            if (operatorSymbols.has(tokenString)) {
                // Quoting.
                exp = [operatorSymbols.get(tokenString)];
                [node, comments] = attachComments(exp, comments, options);
                if (currentVal) {
                    insertx(exp, currentVal, node, currentValNode);
                    currentVal = exp;
                    currentValNode = node;
                }
                else {
                    currentExp = exp;
                    currentExpNode = node;
                    currentVal = exp;
                    currentValNode = node;
                }
            }
            else if (tokenString === '(') {
                // Opening parenthesis.
                exp = [];
                [node, comments] = attachComments(exp, comments, options);
                if (currentVal) {
                    insertx(exp, currentVal, node, currentValNode);
                    currentVal = exp;
                    currentValNode = node;
                }
                else {
                    currentExp = exp;
                    currentExpNode = node;
                }
                if (parentExp) {
                    insertx(currentExp, parentVal, currentExpNode, parentValNode);
                }
                currentVal = currentVal || currentExp;
                currentValNode = currentValNode || currentExpNode;
                const entry = [currentExp, currentVal, currentExpNode, currentValNode];
                stack.push(entry);
                parentExp = currentExp;
                parentVal = currentVal;
                parentExpNode = currentExpNode;
                parentValNode = currentValNode;
                currentVal = undefined;
                currentValNode = undefined;
            }
            else if (tokenString === ')') {
                // Closing parenthesis.
                const entry = stack.pop();
                [currentExp, currentVal, currentExpNode, currentValNode] = entry;
                const parentEntry = (stack.length > 0) ? stack[stack.length - 1] : [undefined, undefined, undefined, undefined];
                [parentExp, parentVal, parentExpNode, parentValNode] = parentEntry;
                currentVal = undefined;
                currentValNode = undefined;
            }
            else if (literalValues.has(tokenString)) {
                // Literal value.
                exp = literalValues.get(tokenString);
                [node, comments] = attachComments(exp, comments, options);
                updatex(exp, node);
            }
            else {
                // Symbolic value.
                exp = Symbol.for(token.getValue());
                [node, comments] = attachComments(exp, comments, options);
                updatex(exp, node);
            }
        }
        else {
            // Non-symbolic value.
            exp = token.getValue();
            [node, comments] = attachComments(exp, comments, options);
            updatex(exp, node);
        }
    }
    return currentExpNode;
}
exports.parseRose = parseRose;
parseRose.fsource = [Symbol.for('define'), [Symbol.for('parse-rose'), Symbol.for('tokens'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('stack'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('exp'), undefined], [Symbol.for('define'), Symbol.for('node'), undefined], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('current-exp'), undefined], [Symbol.for('define'), Symbol.for('current-exp-node'), undefined], [Symbol.for('define'), Symbol.for('current-val'), undefined], [Symbol.for('define'), Symbol.for('current-val-node'), undefined], [Symbol.for('define'), Symbol.for('parent-exp')], [Symbol.for('define'), Symbol.for('parent-exp-node')], [Symbol.for('define'), Symbol.for('parent-val')], [Symbol.for('define'), Symbol.for('parent-val-node')], [Symbol.for('define'), [Symbol.for('insert!'), Symbol.for('val'), Symbol.for('exp'), Symbol.for('val-node'), Symbol.for('exp-node')], [Symbol.for('push-right!'), Symbol.for('exp'), Symbol.for('val')], [Symbol.for('send'), Symbol.for('exp-node'), Symbol.for('insert'), Symbol.for('val-node')]], [Symbol.for('define'), [Symbol.for('update!'), Symbol.for('exp'), Symbol.for('node')], [Symbol.for('cond'), [Symbol.for('current-val'), [Symbol.for('insert!'), Symbol.for('exp'), Symbol.for('current-val'), Symbol.for('node'), Symbol.for('current-val-node')], [Symbol.for('set!'), Symbol.for('current-val'), undefined], [Symbol.for('set!'), Symbol.for('current-val-node'), undefined]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('current-exp'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('current-exp-node'), Symbol.for('node')]]], [Symbol.for('when'), Symbol.for('parent-exp'), [Symbol.for('insert!'), Symbol.for('current-exp'), Symbol.for('parent-val'), Symbol.for('current-exp-node'), Symbol.for('parent-val-node')]]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('js/length'), Symbol.for('tokens')]]]], [Symbol.for('define'), Symbol.for('token'), [Symbol.for('aget'), Symbol.for('tokens'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('token'), Symbol.for('CommentToken')], [Symbol.for('when'), [Symbol.for('is-a?'), Symbol.for('token'), Symbol.for('LeadingCommentToken')], [Symbol.for('push-right!'), Symbol.for('comments'), Symbol.for('token')]]], [[Symbol.for('is-a?'), Symbol.for('token'), Symbol.for('SymbolToken')], [Symbol.for('define'), Symbol.for('token-string'), [Symbol.for('send'), Symbol.for('token'), Symbol.for('get-value')]], [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('operator-symbols'), Symbol.for('token-string')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('list'), [Symbol.for('hash-ref'), Symbol.for('operator-symbols'), Symbol.for('token-string')]]], [Symbol.for('set!-values'), [Symbol.for('node'), Symbol.for('comments')], [Symbol.for('attach-comments'), Symbol.for('exp'), Symbol.for('comments'), Symbol.for('options')]], [Symbol.for('cond'), [Symbol.for('current-val'), [Symbol.for('insert!'), Symbol.for('exp'), Symbol.for('current-val'), Symbol.for('node'), Symbol.for('current-val-node')], [Symbol.for('set!'), Symbol.for('current-val'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('current-val-node'), Symbol.for('node')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('current-exp'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('current-exp-node'), Symbol.for('node')], [Symbol.for('set!'), Symbol.for('current-val'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('current-val-node'), Symbol.for('node')]]]], [[Symbol.for('eq?'), Symbol.for('token-string'), '('], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('quote'), []]], [Symbol.for('set!-values'), [Symbol.for('node'), Symbol.for('comments')], [Symbol.for('attach-comments'), Symbol.for('exp'), Symbol.for('comments'), Symbol.for('options')]], [Symbol.for('cond'), [Symbol.for('current-val'), [Symbol.for('insert!'), Symbol.for('exp'), Symbol.for('current-val'), Symbol.for('node'), Symbol.for('current-val-node')], [Symbol.for('set!'), Symbol.for('current-val'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('current-val-node'), Symbol.for('node')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('current-exp'), Symbol.for('exp')], [Symbol.for('set!'), Symbol.for('current-exp-node'), Symbol.for('node')]]], [Symbol.for('when'), Symbol.for('parent-exp'), [Symbol.for('insert!'), Symbol.for('current-exp'), Symbol.for('parent-val'), Symbol.for('current-exp-node'), Symbol.for('parent-val-node')]], [Symbol.for('set!'), Symbol.for('current-val'), [Symbol.for('or'), Symbol.for('current-val'), Symbol.for('current-exp')]], [Symbol.for('set!'), Symbol.for('current-val-node'), [Symbol.for('or'), Symbol.for('current-val-node'), Symbol.for('current-exp-node')]], [Symbol.for('define'), Symbol.for('entry'), [Symbol.for('list'), Symbol.for('current-exp'), Symbol.for('current-val'), Symbol.for('current-exp-node'), Symbol.for('current-val-node')]], [Symbol.for('push-right!'), Symbol.for('stack'), Symbol.for('entry')], [Symbol.for('set!'), Symbol.for('parent-exp'), Symbol.for('current-exp')], [Symbol.for('set!'), Symbol.for('parent-val'), Symbol.for('current-val')], [Symbol.for('set!'), Symbol.for('parent-exp-node'), Symbol.for('current-exp-node')], [Symbol.for('set!'), Symbol.for('parent-val-node'), Symbol.for('current-val-node')], [Symbol.for('set!'), Symbol.for('current-val'), undefined], [Symbol.for('set!'), Symbol.for('current-val-node'), undefined]], [[Symbol.for('eq?'), Symbol.for('token-string'), ')'], [Symbol.for('define'), Symbol.for('entry'), [Symbol.for('pop-right!'), Symbol.for('stack')]], [Symbol.for('set!-values'), [Symbol.for('current-exp'), Symbol.for('current-val'), Symbol.for('current-exp-node'), Symbol.for('current-val-node')], Symbol.for('entry')], [Symbol.for('define'), Symbol.for('parent-entry'), [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('js/length'), Symbol.for('stack')], 0], [Symbol.for('js/last'), Symbol.for('stack')], [Symbol.for('quote'), [undefined, undefined, undefined, undefined]]]], [Symbol.for('set!-values'), [Symbol.for('parent-exp'), Symbol.for('parent-val'), Symbol.for('parent-exp-node'), Symbol.for('parent-val-node')], Symbol.for('parent-entry')], [Symbol.for('set!'), Symbol.for('current-val'), undefined], [Symbol.for('set!'), Symbol.for('current-val-node'), undefined]], [[Symbol.for('hash-has-key?'), Symbol.for('literal-values'), Symbol.for('token-string')], [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('hash-ref'), Symbol.for('literal-values'), Symbol.for('token-string')]], [Symbol.for('set!-values'), [Symbol.for('node'), Symbol.for('comments')], [Symbol.for('attach-comments'), Symbol.for('exp'), Symbol.for('comments'), Symbol.for('options')]], [Symbol.for('update!'), Symbol.for('exp'), Symbol.for('node')]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('string->symbol'), [Symbol.for('send'), Symbol.for('token'), Symbol.for('get-value')]]], [Symbol.for('set!-values'), [Symbol.for('node'), Symbol.for('comments')], [Symbol.for('attach-comments'), Symbol.for('exp'), Symbol.for('comments'), Symbol.for('options')]], [Symbol.for('update!'), Symbol.for('exp'), Symbol.for('node')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('exp'), [Symbol.for('send'), Symbol.for('token'), Symbol.for('get-value')]], [Symbol.for('set!-values'), [Symbol.for('node'), Symbol.for('comments')], [Symbol.for('attach-comments'), Symbol.for('exp'), Symbol.for('comments'), Symbol.for('options')]], [Symbol.for('update!'), Symbol.for('exp'), Symbol.for('node')]]]], Symbol.for('current-exp-node')];
/**
 * Take the array of tokens produced by `tokenize` and make a
 * nested array that corresponds to the structure of the Lisp code.
 *
 * The output of this function is a fully valid S-expression which
 * can be evaluated in a Lisp environment.
 */
function parseSexp(tokens, options = {}) {
    return (0, rose_1.roseToSexp)(parseRose(tokens, options));
}
exports.parseSexp = parseSexp;
parseSexp.fsource = [Symbol.for('define'), [Symbol.for('parse-sexp'), Symbol.for('tokens'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('~>'), Symbol.for('tokens'), [Symbol.for('parse-rose'), Symbol.for('_'), Symbol.for('options')], [Symbol.for('rose->sexp'), Symbol.for('_')]]];
/**
 * Remove indentation from a multi-line string.
 */
function removeIndentation(str) {
    return str.replace(new RegExp('^[^\\S\\r\\n]+$', 'gm'), '');
}
removeIndentation.fsource = [Symbol.for('define'), [Symbol.for('remove-indentation'), Symbol.for('str')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^[^\\S\\r\\n]+$', 'gm'], Symbol.for('str'), '']];
/**
 * Whether a character is indentation
 * (i.e., tabs or spaces, but not newlines).
 */
function indentationp(char) {
    // Newlines are whitespace, but not indentation.
    return char.match(new RegExp('^[^\\S\\r\\n]+$'));
}
indentationp.fsource = [Symbol.for('define'), [Symbol.for('indentation?'), Symbol.for('char')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^[^\\S\\r\\n]+$'], Symbol.for('char')]];
/**
 * Whether a character is whitespace
 * (i.e., tabs, spaces or newlines).
 */
function whitespacep(char) {
    return char.match(new RegExp('^\\s$'));
}
whitespacep.fsource = [Symbol.for('define'), [Symbol.for('whitespace?'), Symbol.for('char')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^\\s$'], Symbol.for('char')]];
/**
 * Whether a character is a newline.
 *
 * Our concept of newline is that of a
 * [Unix newline][w:Unix text files]
 * (i.e., LF "Line Feed", U+000A).
 *
 * [w:Unix text files]: https://en.wikipedia.org/wiki/Text_file#Unix_text_files
 */
function newlinep(char) {
    return char === '\n';
}
newlinep.fsource = [Symbol.for('define'), [Symbol.for('newline?'), Symbol.for('char')], [Symbol.for('eq?'), Symbol.for('char'), '\n']];
/**
 * Whether a character is a comment character
 * (i.e., `;`).
 */
function commentp(char) {
    return char === ';';
}
commentp.fsource = [Symbol.for('define'), [Symbol.for('comment?'), Symbol.for('char')], [Symbol.for('eq?'), Symbol.for('char'), ';']];
/**
 * Attach comments to a rose tree node, conditional on options.
 * Returns the resulting node and an empty list of comments.
 */
function attachComments(node, comments, options = {}) {
    let commentsOption = options['comments'];
    if (commentsOption === undefined) {
        commentsOption = true;
    }
    const result = (node instanceof rose_1.Rose) ? node : (0, rose_1.sexpToRose)(node);
    if (commentsOption && comments && (comments.length > 0)) {
        result.setProperty('comments', comments);
    }
    return [result, []];
}
attachComments.fsource = [Symbol.for('define'), [Symbol.for('attach-comments'), Symbol.for('node'), Symbol.for('comments'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('comments-option'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('when'), [Symbol.for('undefined?'), Symbol.for('comments-option')], [Symbol.for('set!'), Symbol.for('comments-option'), true]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('if'), [Symbol.for('is-a?'), Symbol.for('node'), Symbol.for('Rose')], Symbol.for('node'), [Symbol.for('sexp->rose'), Symbol.for('node')]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('comments-option'), Symbol.for('comments'), [Symbol.for('>'), [Symbol.for('js/length'), Symbol.for('comments')], 0]], [Symbol.for('send'), Symbol.for('result'), Symbol.for('set-property'), 'comments', Symbol.for('comments')]], [Symbol.for('values'), Symbol.for('result'), [Symbol.for('quote'), []]]];
/**
 * Whether `comment` is a `;;`-comment (level 2),
 * a `;;;`-comment (level 3), or some other level.
 */
function getCommentLevel(comment) {
    const str = (typeof comment === 'string') ? comment : comment.value;
    return str.match(new RegExp('^[ ]*;*'))[0].trim().length;
}
exports.getCommentLevel = getCommentLevel;
getCommentLevel.fsource = [Symbol.for('define'), [Symbol.for('get-comment-level'), Symbol.for('comment')], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('if'), [Symbol.for('string?'), Symbol.for('comment')], Symbol.for('comment'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('comment')]]], [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^[ ]*;*'], Symbol.for('_')], [Symbol.for('ann'), Symbol.for('_'), Symbol.for('Any')], [Symbol.for('first'), Symbol.for('_')], [Symbol.for('string-trim'), Symbol.for('_')], [Symbol.for('string-length'), Symbol.for('_')]]];
/**
 * Get the comment level, e.g., 2 for a `;;`-comment,
 * 3 for a `;;;`-comment, etc.
 */
function commentLevelP(comment, level) {
    return getCommentLevel(comment) === level;
}
exports.commentLevelP = commentLevelP;
commentLevelP.fsource = [Symbol.for('define'), [Symbol.for('comment-level?'), Symbol.for('comment'), Symbol.for('level')], [Symbol.for('='), [Symbol.for('get-comment-level'), Symbol.for('comment')], Symbol.for('level')]];
/**
 * Map of operator symbols.
 * Used by `parse-rose`.
 */
const operatorSymbols = new Map([['\'', constants_1.quoteSym_], ['`', constants_1.quasiquoteSym_], [',', constants_1.unquoteSym_], [',@', constants_1.unquoteSplicingSym_]]);
/**
 * Map of literal symbols.
 * Used by `parse-rose`.
 */
const literalValues = new Map([['#f', false], ['#t', true], ['#n', null], ['#u', undefined]]);
/**
 * Token class.
 */
class Token {
    /**
     * Make a token.
     */
    constructor(value = undefined, tag = 'token') {
        this.setValue(value);
        this.setTag(tag);
    }
    /**
     * Get the token tag
     * (i.e., its type).
     */
    getTag() {
        return this.tag;
    }
    /**
     * Get the value of the token.
     */
    getValue() {
        return this.value;
    }
    /**
     * Set the token tag
     * (i.e., its type).
     */
    setTag(tag) {
        return this.tag = tag;
    }
    /**
     * Set the value of the token.
     */
    setValue(value) {
        return this.value = value;
    }
}
exports.Token = Token;
/**
 * Token class for representing comments.
 */
class CommentToken extends Token {
    constructor(value, tag = 'comment') {
        super(value, tag);
    }
}
exports.CommentToken = CommentToken;
/**
 * Token class for representing leading comments.
 */
class LeadingCommentToken extends CommentToken {
    constructor(value) {
        super(value, 'leading-comment');
    }
}
exports.LeadingCommentToken = LeadingCommentToken;
/**
 * Token class for representing trailing comments.
 */
class TrailingCommentToken extends CommentToken {
    constructor(value) {
        super(value, 'trailing-comment');
    }
}
exports.TrailingCommentToken = TrailingCommentToken;
/**
 * Token class for representing numbers.
 */
class NumberToken extends Token {
    constructor(value) {
        super(value, 'number');
    }
}
exports.NumberToken = NumberToken;
/**
 * Token class for representing strings.
 */
class StringToken extends Token {
    constructor(value) {
        super(value, 'string');
    }
}
exports.StringToken = StringToken;
/**
 * Token class for representing symbols.
 */
class SymbolToken extends Token {
    constructor(value) {
        super(value, 'symbol');
    }
}
exports.SymbolToken = SymbolToken;
