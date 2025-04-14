"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # ESTree printer implementation
 *
 * Printer code for ESTree trees, rose trees and raw S-expressions.
 *
 * ## Description
 *
 * TODO: Better description
 *
 * This is a partial, incomplete printer for ESTree trees, with
 * limited support for TSESTree extensions. With further work, it
 * might be developed into a complete implementation.
 *
 * Why not use an external library? For example, [recast][npm:recast]
 * could be an option as far as basic ESTree trees are concerned. The
 * challenge is that we also need to emit comments, and for the sake
 * of TypeScript support, the printer must be able to understand type
 * annotations in the form of [TSESTree][doc:typescript-estree]
 * extensions. However, comments and type annotations are both
 * nonstandard extensions to the [ESTree
 * specification][github:estree], and are not supported by recast.
 *
 * It might be possible to replace the whole lot with
 * [Prettier][www:prettier], which does support both comments and
 * types. However, it has proven difficult to [hook
 * into][doc:prettier:plugins] Prettier's [API][doc:prettier:api] in
 * the right way.
 *
 * Thus we are currently left with the option of writing our own
 * implementation. It is incomplete, and possibly buggy, but at least
 * there is a test suite that attempts to salvage the situation
 * somewhat.
 *
 * ## External links
 *
 * -   [The ESTree spec][github:estree]
 * -   [TSESTree details][doc:typescript-estree]
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [npm:recast]: https://www.npmjs.com/package/recast
 * [doc:typescript-estree]: https://typescript-eslint.io/packages/typescript-estree/
 * [github:estree]: https://github.com/estree/estree
 * [www:prettier]: https://prettier.io/
 * [doc:prettier:plugins]: https://prettier.io/docs/en/plugins#printers
 * [doc:prettier:api]: https://prettier.io/docs/en/api
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.writeToString = exports.printSexpAsExpression = exports.printSexp = exports.printRose = exports.printNode = exports.printEstree = exports.print = exports.printAsExpression = exports.printEstreeNode = void 0;
const estree_1 = require("./estree");
const rose_1 = require("./rose");
const visitor_1 = require("./visitor");
const [stringLength, length, findf, symbolp, booleanp, stringp, procedurep, arrayp, take, lastCdr] = (() => {
    function length_(lst) {
        if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
            return (() => {
                function linkedListLength_(lst) {
                    let len = 0;
                    let current = lst;
                    while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.'))) {
                        len = len + (lst.length - 2);
                        current = current[current.length - 1];
                    }
                    return len;
                }
                return linkedListLength_;
            })()(lst);
        }
        else {
            return lst.length;
        }
    }
    function findf_(proc, lst, notFound = false) {
        const idx = lst.findIndex(proc);
        if (idx >= 0) {
            return lst[idx];
        }
        else {
            return notFound;
        }
    }
    function symbolp_(obj) {
        return typeof obj === 'symbol';
    }
    function booleanp_(obj) {
        return typeof obj === 'boolean';
    }
    function stringp_(obj) {
        return (typeof obj === 'string') || (obj instanceof String);
    }
    function procedurep_(obj) {
        return obj instanceof Function;
    }
    function arrayp_(obj) {
        return Array.isArray(obj);
    }
    function take_(lst, n) {
        const n1 = lst.length - n;
        if (n1 === 0) {
            return lst;
        }
        else {
            return lst.slice(0, -n1);
        }
    }
    function lastCdr_(lst) {
        if (!Array.isArray(lst)) {
            return undefined;
        }
        else if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
            let result = lst;
            while (Array.isArray(result) && (result.length >= 3) && (result[result.length - 2] === Symbol.for('.'))) {
                result = result[result.length - 1];
            }
            return result;
        }
        else {
            return [];
        }
    }
    function linkedListLength_(lst) {
        let len = 0;
        let current = lst;
        while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.'))) {
            len = len + (lst.length - 2);
            current = current[current.length - 1];
        }
        return len;
    }
    return [length_, length_, findf_, symbolp_, booleanp_, stringp_, procedurep_, arrayp_, take_, lastCdr_];
})();
/**
 * `DocCommand` class.
 *
 * Used for implementing more complicated constructs,
 * such as `indent` and `group`.
 */
class DocCommand {
    constructor(type, ...args) {
        this.type = type;
        this.args = args;
    }
}
/**
 * Empty string.
 */
const empty = '';
/**
 * Space.
 */
const space = ' ';
/**
 * Newline.
 */
const line = '\n';
/**
 * `Doc` command `literalline`.
 */
const literalline = new DocCommand('literalline');
/**
 * `Doc` command `align`.
 */
function align(offset, doc, options = {}) {
    return new DocCommand('align', offset, doc, options);
}
align.lispSource = [Symbol.for('define'), [Symbol.for('align'), Symbol.for('offset'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'align', Symbol.for('offset'), Symbol.for('doc'), Symbol.for('options')]];
/**
 * `Doc` command `indent`.
 */
function indent(doc, options = {}) {
    return new DocCommand('indent', doc, options);
}
indent.lispSource = [Symbol.for('define'), [Symbol.for('indent'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'indent', Symbol.for('doc'), Symbol.for('options')]];
/**
 * `Doc` command `noindent`.
 */
function noindent(doc, options = {}) {
    return new DocCommand('noindent', doc, options);
}
noindent.lispSource = [Symbol.for('define'), [Symbol.for('noindent'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'noindent', Symbol.for('doc'), Symbol.for('options')]];
/**
 * `Doc` command `join`.
 *
 * Join a list of documents with a separator.
 */
function join(sep, docs) {
    let result = [];
    const _end = docs.length;
    for (let i = 0; i < _end; i++) {
        if (i !== 0) {
            result.push(sep);
        }
        result.push(docs[i]);
    }
    return result;
}
join.lispSource = [Symbol.for('define'), [Symbol.for('join'), Symbol.for('sep'), Symbol.for('docs')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('docs')]]]], [Symbol.for('unless'), [Symbol.for('='), Symbol.for('i'), 0], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('sep')]], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('aget'), Symbol.for('docs'), Symbol.for('i')]]], Symbol.for('result')];
/**
 * `Doc` command `group`.
 *
 * Makes a document group.
 */
function group(doc, options = {}) {
    return new DocCommand('group', doc, options);
}
group.lispSource = [Symbol.for('define'), [Symbol.for('group'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'group', Symbol.for('doc'), Symbol.for('options')]];
/**
 * Get the type of a `Doc` object.
 */
function docType(doc) {
    if (typeof doc === 'string') {
        return 'string';
    }
    else if (Array.isArray(doc)) {
        return 'array';
    }
    else if (doc instanceof DocCommand) {
        return doc.type;
    }
    else {
        return 'undefined';
    }
}
docType.lispSource = [Symbol.for('define'), [Symbol.for('doc-type'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('string?'), Symbol.for('doc')], 'string'], [[Symbol.for('array?'), Symbol.for('doc')], 'array'], [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('get-field'), Symbol.for('type'), Symbol.for('doc')]], [Symbol.for('else'), 'undefined']]];
/**
 * Unwrap a `Doc` command.
 */
function docValue(doc) {
    if (doc instanceof DocCommand) {
        return doc.args[0];
    }
    else {
        return doc;
    }
}
docValue.lispSource = [Symbol.for('define'), [Symbol.for('doc-value'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]]], [Symbol.for('else'), Symbol.for('doc')]]];
/**
 * Unwrap a `Doc` command and print it to a string.
 */
function docValueString(doc) {
    return printDoc(docValue(doc));
}
docValueString.lispSource = [Symbol.for('define'), [Symbol.for('doc-value-string'), Symbol.for('doc')], [Symbol.for('print-doc'), [Symbol.for('doc-value'), Symbol.for('doc')]]];
/**
 * Whether a `Doc` object should break across multiple lines.
 */
function docShouldBreakP(doc) {
    if (doc instanceof DocCommand) {
        return (() => {
            const arr = doc.args;
            return arr[arr.length - 1];
        })()['should-break'];
    }
    else {
        return false;
    }
}
docShouldBreakP.lispSource = [Symbol.for('define'), [Symbol.for('doc-should-break?'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('oget'), [Symbol.for('array-list-last'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], 'should-break']], [Symbol.for('else'), Symbol.for('#f')]]];
/**
 * Whether a `Doc` object contains any comments.
 */
function docHasCommentsP(doc) {
    if (doc instanceof DocCommand) {
        return (() => {
            const arr = doc.args;
            return arr[arr.length - 1];
        })()['has-comments'];
    }
    else {
        return false;
    }
}
docHasCommentsP.lispSource = [Symbol.for('define'), [Symbol.for('doc-has-comments?'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('oget'), [Symbol.for('array-list-last'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], 'has-comments']], [Symbol.for('else'), Symbol.for('#f')]]];
/**
 * Wrap a `Doc` object in a pair of parentheses.
 */
function docWrap(doc, options = {}, settings = {}) {
    const open = settings['open'] || '(';
    const close = settings['close'] || ')';
    const offset = stringLength(open);
    if (options['has-comments'] || docHasCommentsP(doc)) {
        return printDoc([open, line, align(offset, doc), line, close], options);
    }
    else {
        return printDoc([open, doc, close]);
    }
}
docWrap.lispSource = [Symbol.for('define'), [Symbol.for('doc-wrap'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('open'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('settings'), 'open'], '(']], [Symbol.for('define'), Symbol.for('close'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('settings'), 'close'], ')']], [Symbol.for('define'), Symbol.for('offset'), [Symbol.for('string-length'), Symbol.for('open')]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'has-comments'], [Symbol.for('doc-has-comments?'), Symbol.for('doc')]], [Symbol.for('print-doc'), [Symbol.for('list'), Symbol.for('open'), Symbol.for('line'), [Symbol.for('align'), Symbol.for('offset'), Symbol.for('doc')], Symbol.for('line'), Symbol.for('close')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('print-doc'), [Symbol.for('list'), Symbol.for('open'), Symbol.for('doc'), Symbol.for('close')]]]]];
/**
 * Print comments of an ESTree node and attach them
 * to a `Doc` object.
 */
function attachComments(result, node, options = {}) {
    const commentsOption = options['comments'];
    const comments = node.comments;
    const code = docValueString(result);
    let leadingComments = '';
    let trailingComments = '';
    if ((commentsOption === false) || !comments || (comments.length === 0)) {
        return result;
    }
    const _end = length(comments);
    for (let i = 0; i < _end; i++) {
        const comment = comments[i];
        if (comment instanceof estree_1.BlockComment) {
            let blockComment = makeBlockComment(comment.originalText);
            if ((i === (comments.length - 1)) && (code === '')) {
                blockComment = blockComment.replace(new RegExp('\\n*$'), '');
            }
            leadingComments = leadingComments + blockComment + (((code === '') || blockComment.match(new RegExp('\\n*$'))) ? empty : line);
        }
        else if (comment instanceof estree_1.LeadingComment) {
            let leadingComment = makeLineComment(comment.originalText);
            if ((i === (comments.length - 1)) && (code === '')) {
                leadingComment = leadingComment.replace(new RegExp('\\n$'), '');
            }
            leadingComments = leadingComments + leadingComment + (((code === '') || leadingComment.match(new RegExp('\\n$'))) ? empty : line);
        }
        else if (comment instanceof estree_1.TrailingComment) {
            const trailingComment = makeLineComment(comment.originalText);
            trailingComments = trailingComments + space + trailingComment;
        }
    }
    return group([leadingComments, code, trailingComments], {
        'should-break': true,
        'has-comments': true
    });
}
attachComments.lispSource = [Symbol.for('define'), [Symbol.for('attach-comments'), Symbol.for('result'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('comments-option'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('code'), [Symbol.for('doc-value-string'), Symbol.for('result')]], [Symbol.for('define'), Symbol.for('leading-comments'), ''], [Symbol.for('define'), Symbol.for('trailing-comments'), ''], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('comments-option'), Symbol.for('#f')], [Symbol.for('not'), Symbol.for('comments')], [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('comments')], 0]], [Symbol.for('return'), Symbol.for('result')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('length'), Symbol.for('comments')]]]], [Symbol.for('define'), Symbol.for('comment'), [Symbol.for('aget'), Symbol.for('comments'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('BlockComment')], [Symbol.for('define'), Symbol.for('block-comment'), [Symbol.for('make-block-comment'), [Symbol.for('get-field'), Symbol.for('original-text'), Symbol.for('comment')]]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('='), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('comments')], 1]], [Symbol.for('eq?'), Symbol.for('code'), '']], [Symbol.for('set!'), Symbol.for('block-comment'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\n*$'], Symbol.for('block-comment'), '']]], [Symbol.for('set!'), Symbol.for('leading-comments'), [Symbol.for('string-append'), Symbol.for('leading-comments'), Symbol.for('block-comment'), [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('code'), ''], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n*$'], Symbol.for('block-comment')]], Symbol.for('empty'), Symbol.for('line')]]]], [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('LeadingComment')], [Symbol.for('define'), Symbol.for('leading-comment'), [Symbol.for('make-line-comment'), [Symbol.for('get-field'), Symbol.for('original-text'), Symbol.for('comment')]]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('='), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('comments')], 1]], [Symbol.for('eq?'), Symbol.for('code'), '']], [Symbol.for('set!'), Symbol.for('leading-comment'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\n$'], Symbol.for('leading-comment'), '']]], [Symbol.for('set!'), Symbol.for('leading-comments'), [Symbol.for('string-append'), Symbol.for('leading-comments'), Symbol.for('leading-comment'), [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('code'), ''], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n$'], Symbol.for('leading-comment')]], Symbol.for('empty'), Symbol.for('line')]]]], [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('TrailingComment')], [Symbol.for('define'), Symbol.for('trailing-comment'), [Symbol.for('make-line-comment'), [Symbol.for('get-field'), Symbol.for('original-text'), Symbol.for('comment')]]], [Symbol.for('set!'), Symbol.for('trailing-comments'), [Symbol.for('string-append'), Symbol.for('trailing-comments'), Symbol.for('space'), Symbol.for('trailing-comment')]]]]], [Symbol.for('group'), [Symbol.for('list'), Symbol.for('leading-comments'), Symbol.for('code'), Symbol.for('trailing-comments')], [Symbol.for('js-obj'), 'should-break', Symbol.for('#t'), 'has-comments', Symbol.for('#t')]]];
/**
 * Make a line comment.
 */
function makeLineComment(text) {
    const [, content, trailingNewlines] = text.match(new RegExp('^([\\s\\S]*?)([\\n]*)$'));
    return content.split('\n').map(function (x) {
        return x.replace(new RegExp('^'), (x === '') ? '//' : '// ');
    }).join('\n') + trailingNewlines;
}
makeLineComment.lispSource = [Symbol.for('define'), [Symbol.for('make-line-comment'), Symbol.for('text')], [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('content'), Symbol.for('trailing-newlines')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^([\\s\\S]*?)([\\n]*)$'], Symbol.for('text')]], [Symbol.for('string-append'), [Symbol.for('~>'), Symbol.for('content'), [Symbol.for('string-split'), '\n'], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^'], Symbol.for('x'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('x'), ''], '//', '// ']]], Symbol.for('_')], [Symbol.for('string-join'), '\n']], Symbol.for('trailing-newlines')]];
/**
 * Make a block comment.
 */
function makeBlockComment(text) {
    const [, content, trailingNewlines] = text.match(new RegExp('^([\\s\\S]*?)([\\n]*)$'));
    return '/**' + line + content.split('\n').map(function (x) {
        return x.replace(new RegExp('^'), (x === '') ? ' *' : ' * ');
    }).join('\n') + line + ' */' + trailingNewlines;
}
makeBlockComment.lispSource = [Symbol.for('define'), [Symbol.for('make-block-comment'), Symbol.for('text')], [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('content'), Symbol.for('trailing-newlines')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^([\\s\\S]*?)([\\n]*)$'], Symbol.for('text')]], [Symbol.for('string-append'), '/**', Symbol.for('line'), [Symbol.for('~>'), Symbol.for('content'), [Symbol.for('string-split'), '\n'], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^'], Symbol.for('x'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('x'), ''], ' *', ' * ']]], Symbol.for('_')], [Symbol.for('string-join'), '\n']], Symbol.for('line'), ' */', Symbol.for('trailing-newlines')]];
/**
 * Whether an expression is "simple", i.e., does not
 * need to be wrapped in parentheses when printed.
 */
function estreeSimpleP(exp) {
    return ['Literal', 'Identifier', 'ThisExpression', 'CallExpression', 'NewExpression', 'UnaryExpression', 'ArrayExpression', 'ObjectExpression', 'MemberExpression'].includes((0, estree_1.estreeType)(exp));
}
estreeSimpleP.lispSource = [Symbol.for('define'), [Symbol.for('estree-simple?'), Symbol.for('exp')], [Symbol.for('memq?'), [Symbol.for('estree-type'), Symbol.for('exp')], [Symbol.for('quote'), ['Literal', 'Identifier', 'ThisExpression', 'CallExpression', 'NewExpression', 'UnaryExpression', 'ArrayExpression', 'ObjectExpression', 'MemberExpression']]]];
/**
 * Whether an expression is "complex", i.e., needs
 * to be wrapped in parentheses when printed.
 */
function estreeComplexP(exp) {
    return ['FunctionExpression', 'ArrowFunctionExpression', 'FunctionDeclaration', 'TSAsExpression'].includes((0, estree_1.estreeType)(exp));
}
estreeComplexP.lispSource = [Symbol.for('define'), [Symbol.for('estree-complex?'), Symbol.for('exp')], [Symbol.for('memq?'), [Symbol.for('estree-type'), Symbol.for('exp')], [Symbol.for('quote'), ['FunctionExpression', 'ArrowFunctionExpression', 'FunctionDeclaration', 'TSAsExpression']]]];
/**
 * Whether an expression is a string literal.
 */
function estreeStringLiteralP(exp) {
    return (0, estree_1.estreeTypeP)(exp, 'Literal') && (typeof exp.value === 'string');
}
estreeStringLiteralP.lispSource = [Symbol.for('define'), [Symbol.for('estree-string-literal?'), Symbol.for('exp')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('exp'), 'Literal'], [Symbol.for('string?'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('exp')]]]];
/**
 * Whether an ESTree node has any comments.
 */
function estreeHasCommentsP(node) {
    return node.comments.length > 0;
}
estreeHasCommentsP.lispSource = [Symbol.for('define'), [Symbol.for('estree-has-comments?'), Symbol.for('node')], [Symbol.for('>'), [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')]], 0]];
/**
 * Whether an ESTree node has any block comments.
 */
function estreeHasBlockCommentP(node) {
    return findf(function (comment) {
        return comment instanceof estree_1.BlockComment;
    }, node.comments);
}
estreeHasBlockCommentP.lispSource = [Symbol.for('define'), [Symbol.for('estree-has-block-comment?'), Symbol.for('node')], [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('comment')], [Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('BlockComment')]], [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')]]];
/**
 * Whether an ESTree node has any leading comments.
 */
function estreeHasLeadingCommentP(node) {
    return findf(function (comment) {
        return comment instanceof estree_1.LeadingComment;
    }, node.comments);
}
estreeHasLeadingCommentP.lispSource = [Symbol.for('define'), [Symbol.for('estree-has-leading-comment?'), Symbol.for('node')], [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('comment')], [Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('LeadingComment')]], [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')]]];
/**
 * Whether an ESTree node has any trailing comments.
 */
function estreeHasTrailingCommentP(node) {
    return findf(function (comment) {
        return comment instanceof estree_1.TrailingComment;
    }, node.comments);
}
estreeHasTrailingCommentP.lispSource = [Symbol.for('define'), [Symbol.for('estree-has-trailing-comment?'), Symbol.for('node')], [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('comment')], [Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('TrailingComment')]], [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')]]];
/**
 * Print an ESTree node or an S-expression.
 */
function print(obj, options = {}) {
    if ((0, estree_1.estreep)(obj)) {
        return printEstree(obj, options);
    }
    else {
        return printSexp(obj, options);
    }
}
exports.print = print;
print.lispSource = [Symbol.for('define'), [Symbol.for('print'), Symbol.for('obj'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('estree?'), Symbol.for('obj')], [Symbol.for('print-estree'), Symbol.for('obj'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('print-sexp'), Symbol.for('obj'), Symbol.for('options')]]]];
/**
 * Print an ESTree node.
 */
function printEstree(node, options = {}) {
    return printToString(node, options);
}
exports.printEstree = printEstree;
printEstree.lispSource = [Symbol.for('define'), [Symbol.for('print-estree'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-to-string'), Symbol.for('node'), Symbol.for('options')]];
/**
 * Print a rose tree.
 */
function printRose(node, options = {}) {
    return printSexp(node.getValue(), options);
}
exports.printRose = printRose;
printRose.lispSource = [Symbol.for('define'), [Symbol.for('print-rose'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-sexp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')], Symbol.for('options')]];
/**
 * Print an S-expression.
 */
function printSexp(exp, options = {}) {
    return writeToString(exp, options);
}
exports.printSexp = printSexp;
printSexp.lispSource = [Symbol.for('define'), [Symbol.for('print-sexp'), Symbol.for('exp'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('write-to-string'), Symbol.for('exp'), Symbol.for('options')]];
/**
 * Print an S-expression as an expression
 * that can be evaluated.
 */
function printSexpAsExpression(exp, options = {}) {
    return printSexp(exp, Object.assign(Object.assign({}, options), { quoteToplevel: true }));
}
exports.printAsExpression = printSexpAsExpression;
exports.printSexpAsExpression = printSexpAsExpression;
printSexpAsExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-sexp-as-expression'), Symbol.for('exp'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-sexp'), Symbol.for('exp'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'quoteToplevel', Symbol.for('#t')]]]];
/**
 * Print an S-expression to a string.
 */
function writeToString(obj, options = {}) {
    let result = writeToDoc(obj, options);
    if (!options['doc']) {
        result = printDoc(result, options);
    }
    return result;
}
exports.writeToString = writeToString;
writeToString.lispSource = [Symbol.for('define'), [Symbol.for('write-to-string'), Symbol.for('obj'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('write-to-doc'), Symbol.for('obj'), Symbol.for('options')]], [Symbol.for('unless'), [Symbol.for('oget'), Symbol.for('options'), 'doc'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('print-doc'), Symbol.for('result'), Symbol.for('options')]]], Symbol.for('result')];
/**
 * Print an S-expression to a `Doc` object.
 */
function writeToDoc(obj, options = {}) {
    const docOption = options['doc'];
    const prettyOption = options['pretty'];
    const quoteToplevelOption = options['quoteToplevel'];
    const visitor = (0, visitor_1.makeVisitor)([[rose_1.rosep, function (obj) {
                return writeToDoc(obj.getValue(), options);
            }], [symbolp, function (obj) {
                return [quoteToplevelOption ? '\'' : empty, (obj === Symbol.for('.')) ? '.' : obj.description];
            }], [booleanp, function (obj) {
                if (obj) {
                    return '#t';
                }
                else {
                    return '#f';
                }
            }], [stringp, function (obj) {
                return ['"', join(literalline, obj.replace(new RegExp('\\\\', 'g'), '\\\\').replace(new RegExp('"', 'g'), '\\"').split(line)), '"'];
            }], [procedurep, function (obj) {
                return '#<procedure>';
            }], [arrayp, function (obj) {
                const op = obj[0];
                const spec = prettyOption && prettyPrintMap.get(op);
                let result = (spec instanceof Function) ? spec(obj, options) : (Number.isFinite(spec) ? prettyPrintWithOffset(spec, obj, options) : prettyPrintForm(obj, options));
                if (quoteToplevelOption) {
                    result = ['\'', result];
                }
                return result;
            }], [function (...args) {
                return true;
            }, function (obj) {
                return obj + '';
            }]]);
    let result = (0, visitor_1.visit)(visitor, obj);
    return result;
}
writeToDoc.lispSource = [Symbol.for('define'), [Symbol.for('write-to-doc'), Symbol.for('obj'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('doc-option'), [Symbol.for('oget'), Symbol.for('options'), 'doc']], [Symbol.for('define'), Symbol.for('pretty-option'), [Symbol.for('oget'), Symbol.for('options'), 'pretty']], [Symbol.for('define'), Symbol.for('quote-toplevel-option'), [Symbol.for('oget'), Symbol.for('options'), 'quoteToplevel']], [Symbol.for('define'), Symbol.for('visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('rose?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('write-to-doc'), [Symbol.for('send'), Symbol.for('obj'), Symbol.for('get-value')], Symbol.for('options')]]]], [[Symbol.for('unquote'), Symbol.for('symbol?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('list'), [Symbol.for('if'), Symbol.for('quote-toplevel-option'), '\'', Symbol.for('empty')], [Symbol.for('if'), [Symbol.for('cons-dot?'), Symbol.for('obj')], '.', [Symbol.for('symbol->string'), Symbol.for('obj')]]]]]], [[Symbol.for('unquote'), Symbol.for('boolean?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('if'), Symbol.for('obj'), '#t', '#f']]]], [[Symbol.for('unquote'), Symbol.for('string?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('list'), '"', [Symbol.for('~>'), Symbol.for('obj'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\\\', 'g'], Symbol.for('_'), '\\\\'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '"', 'g'], Symbol.for('_'), '\\"'], [Symbol.for('string-split'), Symbol.for('line')], [Symbol.for('join'), Symbol.for('literalline'), Symbol.for('_')]], '"']]]], [[Symbol.for('unquote'), Symbol.for('procedure?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], '#<procedure>']]], [[Symbol.for('unquote'), Symbol.for('array?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('obj')]], [Symbol.for('define'), Symbol.for('spec'), [Symbol.for('and'), Symbol.for('pretty-option'), [Symbol.for('send'), Symbol.for('pretty-print-map'), Symbol.for('get'), Symbol.for('op')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('cond'), [[Symbol.for('procedure?'), Symbol.for('spec')], [Symbol.for('spec'), Symbol.for('obj'), Symbol.for('options')]], [[Symbol.for('number?'), Symbol.for('spec')], [Symbol.for('pretty-print-with-offset'), Symbol.for('spec'), Symbol.for('obj'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('pretty-print-form'), Symbol.for('obj'), Symbol.for('options')]]]], [Symbol.for('when'), Symbol.for('quote-toplevel-option'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '\'', Symbol.for('result')]]], Symbol.for('result')]]], [[Symbol.for('unquote'), [Symbol.for('const'), Symbol.for('#t')]], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('string-append'), Symbol.for('obj'), '']]]]]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('obj')]], Symbol.for('result')];
/**
 * Pretty-print a list expression.
 *
 * Helper function for `write-to-doc`.
 */
function prettyPrintForm(form, options) {
    return ['(', join(' ', form.map(function (x) {
            return writeToDoc(x, Object.assign(Object.assign({}, options), { quoteToplevel: false }));
        })), ')'];
}
prettyPrintForm.lispSource = [Symbol.for('define'), [Symbol.for('pretty-print-form'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('list'), '(', [Symbol.for('join'), ' ', [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'quoteToplevel', Symbol.for('#f')]]]], Symbol.for('form')]], ')']];
/**
 * Pretty-print a list expression with an indentation offset.
 *
 * Helper function for `write-to-doc`.
 */
function prettyPrintWithOffset(offset, form, options) {
    const prettyOption = options['pretty'];
    if (!prettyOption) {
        return writeToString(form, options);
    }
    if (!Array.isArray(form)) {
        return writeToString(form, options);
    }
    const op = form[0];
    const elements = form.map(function (x) {
        return writeToDoc(x, Object.assign(Object.assign({}, options), { quoteToplevel: false }));
    });
    const elements1 = take(elements, offset + 1);
    const elements2 = (() => {
        const n = offset + 1;
        if (n === 0) {
            return elements;
        }
        else {
            return elements.slice(n);
        }
    })();
    let result = [join(space, elements1), (elements2.length > 0) ? [line, indent(join(line, elements2))] : empty];
    result = ['(', result, ')'];
    return result;
}
prettyPrintWithOffset.lispSource = [Symbol.for('define'), [Symbol.for('pretty-print-with-offset'), Symbol.for('offset'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('pretty-option'), [Symbol.for('oget'), Symbol.for('options'), 'pretty']], [Symbol.for('unless'), Symbol.for('pretty-option'), [Symbol.for('return'), [Symbol.for('write-to-string'), Symbol.for('form'), Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('form')], [Symbol.for('return'), [Symbol.for('write-to-string'), Symbol.for('form'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('form')]], [Symbol.for('define'), Symbol.for('elements'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'quoteToplevel', Symbol.for('#f')]]]], Symbol.for('form')]], [Symbol.for('define'), Symbol.for('elements1'), [Symbol.for('take'), Symbol.for('elements'), [Symbol.for('+'), Symbol.for('offset'), 1]]], [Symbol.for('define'), Symbol.for('elements2'), [Symbol.for('drop'), Symbol.for('elements'), [Symbol.for('+'), Symbol.for('offset'), 1]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('list'), [Symbol.for('join'), Symbol.for('space'), Symbol.for('elements1')], [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('elements2')], 0], [Symbol.for('list'), Symbol.for('line'), [Symbol.for('indent'), [Symbol.for('join'), Symbol.for('line'), Symbol.for('elements2')]]], Symbol.for('empty')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '(', Symbol.for('result'), ')']], Symbol.for('result')];
/**
 * Pretty-print a `cond` expression.
 */
function prettyPrintCond(form, options) {
    return ['(', writeToDoc(form[0], options), line, align(1, join(line, form.slice(1).map(function (x) {
            return ['(', writeToDoc(x[0], options), line, align(1, join(line, x.slice(1).map(function (x1) {
                    return writeToDoc(x1, Object.assign(Object.assign({}, options), { quoteToplevel: false }));
                }))), ')'];
        }))), ')'];
}
prettyPrintCond.lispSource = [Symbol.for('define'), [Symbol.for('pretty-print-cond'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('list'), '(', [Symbol.for('write-to-doc'), [Symbol.for('first'), Symbol.for('form')], Symbol.for('options')], Symbol.for('line'), [Symbol.for('align'), 1, [Symbol.for('join'), Symbol.for('line'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('list'), '(', [Symbol.for('write-to-doc'), [Symbol.for('first'), Symbol.for('x')], Symbol.for('options')], Symbol.for('line'), [Symbol.for('align'), 1, [Symbol.for('join'), Symbol.for('line'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x1')], [Symbol.for('write-to-doc'), Symbol.for('x1'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'quoteToplevel', Symbol.for('#f')]]]], [Symbol.for('rest'), Symbol.for('x')]]]], ')']], [Symbol.for('rest'), Symbol.for('form')]]]], ')']];
/**
 * Pretty-print an `if` expression.
 */
function prettyPrintIf(form, options) {
    return ['(', join(' ', take(form, 2).map(function (x) {
            return writeToDoc(x, options);
        })), line, align(4, join(line, form.slice(2).map(function (x) {
            return writeToDoc(x, options);
        }))), ')'];
}
prettyPrintIf.lispSource = [Symbol.for('define'), [Symbol.for('pretty-print-if'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('list'), '(', [Symbol.for('join'), ' ', [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('take'), Symbol.for('form'), 2]]], Symbol.for('line'), [Symbol.for('align'), 4, [Symbol.for('join'), Symbol.for('line'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('drop'), Symbol.for('form'), 2]]]], ')']];
/**
 * Pretty-print a `module` expression.
 */
function prettyPrintModule(form, options) {
    const noModuleFormOption = options['noModuleForm'];
    if (noModuleFormOption) {
        return join([line, line], form.slice(3).map(function (x) {
            return writeToDoc(x, options);
        }));
    }
    else {
        return ['(', join(' ', take(form, 3).map(function (x) {
                return writeToDoc(x, options);
            })), line, indent(join([line, line], form.slice(3).map(function (x) {
                return writeToDoc(x, options);
            }))), ')'];
    }
}
prettyPrintModule.lispSource = [Symbol.for('define'), [Symbol.for('pretty-print-module'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('no-module-form-option'), [Symbol.for('oget'), Symbol.for('options'), 'noModuleForm']], [Symbol.for('cond'), [Symbol.for('no-module-form-option'), [Symbol.for('join'), [Symbol.for('list'), Symbol.for('line'), Symbol.for('line')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('drop'), Symbol.for('form'), 3]]]], [Symbol.for('else'), [Symbol.for('list'), '(', [Symbol.for('join'), ' ', [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('take'), Symbol.for('form'), 3]]], Symbol.for('line'), [Symbol.for('indent'), [Symbol.for('join'), [Symbol.for('list'), Symbol.for('line'), Symbol.for('line')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('drop'), Symbol.for('form'), 3]]]], ')']]]];
/**
 * Map of pretty printing specifications.
 *
 * Somewhat similar to [`(declare (indent indent-spec))`
 * in GNU Emacs][emacs:declare].
 *
 * [emacs:declare]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Indenting-Macros.html
 */
const prettyPrintMap = new Map([[Symbol.for('as~>'), 2], [Symbol.for('begin'), 0], [Symbol.for('catch'), 2], [Symbol.for('class'), 1], [Symbol.for('cond'), prettyPrintCond], [Symbol.for('define'), 1], [Symbol.for('define/public'), 1], [Symbol.for('do'), 1], [Symbol.for('finally'), 0], [Symbol.for('fn'), 1], [Symbol.for('for'), 1], [Symbol.for('if'), prettyPrintIf], [Symbol.for('js/arrow'), 1], [Symbol.for('js/function'), 1], [Symbol.for('js/while'), 1], [Symbol.for('lambda'), 1], [Symbol.for('let*-values'), 1], [Symbol.for('let-values'), 1], [Symbol.for('module'), prettyPrintModule], [Symbol.for('provide'), 0], [Symbol.for('try'), 0], [Symbol.for('unless'), 1], [Symbol.for('when'), 1], [Symbol.for('while'), 1]]);
/**
 * Print a `Doc` object to a string.
 */
function printDoc(doc, options = {}) {
    return printDocListToString(printDocToDocList(doc, options), options);
}
printDoc.lispSource = [Symbol.for('define'), [Symbol.for('print-doc'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('~>'), Symbol.for('doc'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('options')], [Symbol.for('print-doc-list-to-string'), Symbol.for('options')]]];
/**
 * Print a `Doc` object to a `Doc` list.
 */
function printDocToDocList(doc, options = {}) {
    const dtype = docType(doc);
    if (dtype === 'string') {
        return join(line, doc.split(line));
    }
    else if (dtype === 'array') {
        let result = [];
        for (let x of doc) {
            const xResult = printDocToDocList(x, options);
            if (Array.isArray(xResult)) {
                result = [...result, ...xResult];
            }
            else {
                result.push(xResult);
            }
        }
        return result;
    }
    else if (dtype === 'align') {
        const args = doc.args;
        const offset = args[0];
        const contents = (Array.isArray(args) && (args.length >= 3) && (args[args.length - 2] === Symbol.for('.')) && (() => {
            const x = lastCdr(args);
            return Array.isArray(x) && (x.length === 0);
        })()) ? (() => {
            let i = 1;
            let result = args;
            while (i > 0) {
                if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
                    result = args[args.length - 1];
                }
                else {
                    result = args.slice(1);
                }
                i--;
            }
            if (Array.isArray(result)) {
                result = result[0];
            }
            return result;
        })() : args[1];
        const contentsPrinted = printDocToDocList(contents, options).filter(function (x) {
            return x !== empty;
        });
        let result = [];
        const indentation = ' '.repeat(offset);
        if (contentsPrinted.length > 0) {
            result.push(indentation);
        }
        const _end = contentsPrinted.length;
        for (let i = 0; i < _end; i++) {
            let current = contentsPrinted[i];
            const next = (i < (contentsPrinted.length - 1)) ? contentsPrinted[i + 1] : empty;
            result.push(current);
            if ((current === line) && (next !== line) && (next !== empty)) {
                result.push(indentation);
            }
        }
        return result;
    }
    else if (dtype === 'indent') {
        const args = doc.args;
        const contents = args[0];
        const offset = options['indent'] || 2;
        return printDocToDocList(new DocCommand('align', offset, contents, options), options);
    }
    else if (dtype === 'group') {
        const args = doc.args;
        const contents = args[0];
        const contentsPrinted = printDocToDocList(contents, options);
        return contentsPrinted;
    }
    else {
        return [doc];
    }
}
printDocToDocList.lispSource = [Symbol.for('define'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('dtype'), [Symbol.for('doc-type'), Symbol.for('doc')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('dtype'), 'string'], [Symbol.for('join'), Symbol.for('line'), [Symbol.for('string-split'), Symbol.for('doc'), Symbol.for('line')]]], [[Symbol.for('eq?'), Symbol.for('dtype'), 'array'], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('x-result'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('x-result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), Symbol.for('x-result')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('x-result')]]], Symbol.for('result')], [[Symbol.for('eq?'), Symbol.for('dtype'), 'align'], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('offset'), [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('contents'), [Symbol.for('second'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('contents-printed'), [Symbol.for('filter'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('empty')]]], [Symbol.for('print-doc-to-doc-list'), Symbol.for('contents'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('indentation'), [Symbol.for('string-repeat'), ' ', Symbol.for('offset')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('contents-printed')], 0], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('indentation')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('contents-printed')]]]], [Symbol.for('define'), Symbol.for('current'), [Symbol.for('aget'), Symbol.for('contents-printed'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('next'), [Symbol.for('if'), [Symbol.for('<'), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('contents-printed')], 1]], [Symbol.for('aget'), Symbol.for('contents-printed'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('empty')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('current')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('current'), Symbol.for('line')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('next'), Symbol.for('line')]], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('next'), Symbol.for('empty')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('indentation')]]], Symbol.for('result')], [[Symbol.for('eq?'), Symbol.for('dtype'), 'indent'], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('contents'), [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('offset'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'indent'], 2]], [Symbol.for('print-doc-to-doc-list'), [Symbol.for('new'), Symbol.for('DocCommand'), 'align', Symbol.for('offset'), Symbol.for('contents'), Symbol.for('options')], Symbol.for('options')]], [[Symbol.for('eq?'), Symbol.for('dtype'), 'group'], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('contents'), [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('contents-printed'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('contents'), Symbol.for('options')]], Symbol.for('contents-printed')], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('doc')]]]];
/**
 * Print a `Doc` list to a string.
 */
function printDocListToString(doc, options = {}) {
    const dtype = docType(doc);
    if (dtype === 'string') {
        return doc;
    }
    else if (dtype === 'array') {
        return doc.map(function (x) {
            return printDocListToString(x, options);
        }).join(empty);
    }
    else if (dtype === 'literalline') {
        return line;
    }
    else {
        return empty;
    }
}
printDocListToString.lispSource = [Symbol.for('define'), [Symbol.for('print-doc-list-to-string'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('dtype'), [Symbol.for('doc-type'), Symbol.for('doc')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('dtype'), 'string'], Symbol.for('doc')], [[Symbol.for('eq?'), Symbol.for('dtype'), 'array'], [Symbol.for('string-join'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-doc-list-to-string'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('doc')], Symbol.for('empty')]], [[Symbol.for('eq?'), Symbol.for('dtype'), 'literalline'], Symbol.for('line')], [Symbol.for('else'), Symbol.for('empty')]]];
/**
 * Print an ESTree node to a string.
 */
function printToString(node, options = {}) {
    return printDoc(printNode(node, options), options);
}
printToString.lispSource = [Symbol.for('define'), [Symbol.for('print-to-string'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('node'), Symbol.for('options')], Symbol.for('options')]];
/**
 * Print an ESTree node to a `Doc` object.
 */
function printNode(node, options = {}) {
    return (0, visitor_1.visit)(printVisitor, node, options);
}
exports.printEstreeNode = printNode;
exports.printNode = printNode;
printNode.lispSource = [Symbol.for('define'), [Symbol.for('print-node'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('visit'), Symbol.for('print-visitor'), Symbol.for('node'), Symbol.for('options')]];
/**
 * Visitor function for printing ESTree nodes.
 */
function printVisitor(node, options) {
    const type = (0, estree_1.estreeType)(node);
    const comments = options['comments'];
    const printer = printerMap.get(type) || defaultPrinter;
    let result = printer(node, options);
    if (comments) {
        result = attachComments(result, node, options);
    }
    return result;
}
printVisitor.lispSource = [Symbol.for('define'), [Symbol.for('print-visitor'), Symbol.for('node'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('type'), [Symbol.for('estree-type'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('define'), Symbol.for('printer'), [Symbol.for('or'), [Symbol.for('hash-ref'), Symbol.for('printer-map'), Symbol.for('type')], Symbol.for('default-printer')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('printer'), Symbol.for('node'), Symbol.for('options')]], [Symbol.for('when'), Symbol.for('comments'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('attach-comments'), Symbol.for('result'), Symbol.for('node'), Symbol.for('options')]]], Symbol.for('result')];
/**
 * Print an `ExpressionStatement` ESTree node to a `Doc` object.
 */
function printExpressionStatement(node, options = {}) {
    return [printNode(node.expression, options), ';'];
}
printExpressionStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-expression-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('expression'), Symbol.for('node')], Symbol.for('options')], ';']];
/**
 * Print a `ReturnStatement` ESTree node to a `Doc` object.
 */
function printReturnStatement(node, options = {}) {
    const argument = node.argument;
    if (argument) {
        let argumentPrinted = printNode(argument, options);
        if (docShouldBreakP(argumentPrinted)) {
            argumentPrinted = ['(', line, indent(argumentPrinted), line, ')'];
        }
        return ['return', space, argumentPrinted, ';'];
    }
    else {
        return ['return', ';'];
    }
}
printReturnStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-return-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]], [Symbol.for('cond'), [Symbol.for('argument'), [Symbol.for('define'), Symbol.for('argument-printed'), [Symbol.for('print-node'), Symbol.for('argument'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('doc-should-break?'), Symbol.for('argument-printed')], [Symbol.for('set!'), Symbol.for('argument-printed'), [Symbol.for('list'), '(', Symbol.for('line'), [Symbol.for('indent'), Symbol.for('argument-printed')], Symbol.for('line'), ')'], Symbol.for('options')]], [Symbol.for('list'), 'return', Symbol.for('space'), Symbol.for('argument-printed'), ';']], [Symbol.for('else'), [Symbol.for('list'), 'return', ';']]]];
/**
 * Print a `YieldExpression` ESTree node to a `Doc` object.
 */
function printYieldExpression(node, options = {}) {
    return ['yield', node.argument ? [space, printNode(node.argument, options)] : empty];
}
printYieldExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-yield-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'yield', [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')], [Symbol.for('list'), Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')], Symbol.for('options')]], Symbol.for('empty')]]];
/**
 * Print a `ThrowStatement` ESTree node to a `Doc` object.
 */
function printThrowStatement(node, options = {}) {
    return ['throw', space, printNode(node.argument, options), ';'];
}
printThrowStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-throw-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'throw', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')], Symbol.for('options')], ';']];
/**
 * Print an `AwaitExpression` ESTree node to a `Doc` object.
 */
function printAwaitExpression(node, options = {}) {
    return ['await', space, printNode(node.argument, options)];
}
printAwaitExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-await-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'await', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')], Symbol.for('options')]]];
/**
 * Print a `BreakStatement` ESTree node to a `Doc` object.
 */
function printBreakStatement(node, options = {}) {
    return ['break', node.label ? [space, printNode(node.label, options)] : empty, ';'];
}
printBreakStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-break-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'break', [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('label'), Symbol.for('node')], [Symbol.for('list'), Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('label'), Symbol.for('node')], Symbol.for('options')]], Symbol.for('empty')], ';']];
/**
 * Print a `ContinueStatement` ESTree node to a `Doc` object.
 */
function printContinueStatement(node, options = {}) {
    return ['continue', node.label ? [space, printNode(node.label, options)] : empty, ';'];
}
printContinueStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-continue-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'continue', [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('label'), Symbol.for('node')], [Symbol.for('list'), Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('label'), Symbol.for('node')], Symbol.for('options')]], Symbol.for('empty')], ';']];
/**
 * Print a `ThisExpression` ESTree node to a `Doc` object.
 */
function printThisExpression(node, options = {}) {
    return 'this';
}
printThisExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-this-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'this'];
/**
 * Print an `Identifier` ESTree node to a `Doc` object.
 */
function printIdentifier(node, options = {}) {
    const language = options['language'];
    const noImplicitAny = options['noImplicitAny'];
    let type_ = node.typeAnnotation;
    if (noImplicitAny && !type_) {
        type_ = new estree_1.TSAnyKeyword();
    }
    return [node.name, ((language === 'TypeScript') && type_) ? [node.optional ? '?:' : ':', space, printNode(type_, options)] : empty];
}
printIdentifier.lispSource = [Symbol.for('define'), [Symbol.for('print-identifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('no-implicit-any'), [Symbol.for('oget'), Symbol.for('options'), 'noImplicitAny']], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('no-implicit-any'), [Symbol.for('not'), Symbol.for('type_')]], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]], [Symbol.for('list'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('node')], [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], Symbol.for('type_')], [Symbol.for('list'), [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('optional'), Symbol.for('node')], '?:', ':'], Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('type_'), Symbol.for('options')]], Symbol.for('empty')]]];
/**
 * Print a `Literal` ESTree node to a `Doc` object.
 */
function printLiteral(node, options = {}) {
    const value = node.value;
    if (typeof value === 'string') {
        return printStringLiteral(node, options);
    }
    else if (value === true) {
        return 'true';
    }
    else if (value === false) {
        return 'false';
    }
    else if (value === null) {
        return 'null';
    }
    else if (value === undefined) {
        return 'undefined';
    }
    else {
        return value + '';
    }
}
printLiteral.lispSource = [Symbol.for('define'), [Symbol.for('print-literal'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('string?'), Symbol.for('value')], [Symbol.for('print-string-literal'), Symbol.for('node'), Symbol.for('options')]], [[Symbol.for('eq?'), Symbol.for('value'), Symbol.for('#t')], 'true'], [[Symbol.for('eq?'), Symbol.for('value'), Symbol.for('#f')], 'false'], [[Symbol.for('eq?'), Symbol.for('value'), Symbol.for('js/null')], 'null'], [[Symbol.for('eq?'), Symbol.for('value'), Symbol.for('undefined')], 'undefined'], [Symbol.for('else'), [Symbol.for('string-append'), Symbol.for('value'), '']]]];
/**
 * Print a string `Literal` ESTree node to a `Doc` object.
 *
 * Helper function for `print-literal`.
 */
function printStringLiteral(node, options = {}) {
    let str = node.value.replace(new RegExp('\\\\', 'g'), '\\\\').replace(new RegExp('\'', 'g'), '\\\'').replace(new RegExp('\\n', 'g'), '\\n');
    return ['\'', str, '\''];
}
printStringLiteral.lispSource = [Symbol.for('define'), [Symbol.for('print-string-literal'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('_')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\\\', 'g'], Symbol.for('_'), '\\\\'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\'', 'g'], Symbol.for('_'), '\\\''], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\n', 'g'], Symbol.for('_'), '\\n']]], [Symbol.for('list'), '\'', Symbol.for('str'), '\'']];
/**
 * Print a template string.
 *
 * Helper function for `print-template-element` and
 * `print-template-literal`.
 */
function printTemplateString(str) {
    return ['`', join(literalline, str.split(line)), '`'];
}
printTemplateString.lispSource = [Symbol.for('define'), [Symbol.for('print-template-string'), Symbol.for('str')], [Symbol.for('list'), '`', [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('string-split'), Symbol.for('line')], [Symbol.for('join'), Symbol.for('literalline'), Symbol.for('_')]], '`']];
/**
 * Print a `TemplateElement` ESTree node to a `Doc` object.
 */
function printTemplateElement(node, options = {}) {
    let str = node.value.raw;
    return printTemplateString(str);
}
printTemplateElement.lispSource = [Symbol.for('define'), [Symbol.for('print-template-element'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('get-field'), Symbol.for('raw'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]]], [Symbol.for('print-template-string'), Symbol.for('str')]];
/**
 * Print a `TemplateLiteral` ESTree node to a `Doc` object.
 */
function printTemplateLiteral(node, options = {}) {
    let str = node.quasis[0].value.raw;
    return printTemplateString(str);
}
printTemplateLiteral.lispSource = [Symbol.for('define'), [Symbol.for('print-template-literal'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('get-field'), Symbol.for('raw'), [Symbol.for('get-field'), Symbol.for('value'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('quasis'), Symbol.for('node')]]]]], [Symbol.for('print-template-string'), Symbol.for('str')]];
/**
 * Print a `TaggedTemplateExpression` ESTree node to a `Doc` object.
 */
function printTaggedTemplateExpression(node, options = {}) {
    const tag = node.tag;
    const tagPrinted = printNode(tag, options);
    const quasi = node.quasi;
    const quasiPrinted = printNode(quasi, options);
    return [tagPrinted, quasiPrinted];
}
printTaggedTemplateExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-tagged-template-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('tag'), [Symbol.for('get-field'), Symbol.for('tag'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('tag-printed'), [Symbol.for('print-node'), Symbol.for('tag'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('quasi'), [Symbol.for('get-field'), Symbol.for('quasi'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('quasi-printed'), [Symbol.for('print-node'), Symbol.for('quasi'), Symbol.for('options')]], [Symbol.for('list'), Symbol.for('tag-printed'), Symbol.for('quasi-printed')]];
/**
 * Print a `UnaryExpression` ESTree node to a `Doc` object.
 */
function printUnaryExpression(node, options = {}) {
    const prefix = node.prefix;
    const operator = node.operator;
    const operatorPrinted = operator;
    const argument = node.argument;
    let argumentPrinted = printNode(argument, options);
    if (!estreeSimpleP(argument)) {
        argumentPrinted = docWrap(argumentPrinted, options);
    }
    if (prefix) {
        return [operatorPrinted, ((operatorPrinted === 'delete') || (operatorPrinted === 'typeof')) ? space : empty, argumentPrinted];
    }
    else {
        return [argumentPrinted, operatorPrinted];
    }
}
printUnaryExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-unary-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('prefix'), [Symbol.for('get-field'), Symbol.for('prefix'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator-printed'), Symbol.for('operator')], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('argument-printed'), [Symbol.for('print-node'), Symbol.for('argument'), Symbol.for('options')]], [Symbol.for('unless'), [Symbol.for('estree-simple?'), Symbol.for('argument')], [Symbol.for('set!'), Symbol.for('argument-printed'), [Symbol.for('doc-wrap'), Symbol.for('argument-printed'), Symbol.for('options')]]], [Symbol.for('cond'), [Symbol.for('prefix'), [Symbol.for('list'), Symbol.for('operator-printed'), [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('operator-printed'), 'delete'], [Symbol.for('eq?'), Symbol.for('operator-printed'), 'typeof']], Symbol.for('space'), Symbol.for('empty')], Symbol.for('argument-printed')]], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('argument-printed'), Symbol.for('operator-printed')]]]];
/**
 * Print a `BinaryExpression` ESTree node to a `Doc` object.
 */
function printBinaryExpression(node, options = {}) {
    let type_ = (0, estree_1.estreeType)(node);
    const operator = node.operator;
    const operatorPrinted = operator;
    const left = node.left;
    let leftPrinted = printNode(left, options);
    let leftPrintedStr = docValueString(leftPrinted);
    const right = node.right;
    const rightPrinted = printNode(right, options);
    let rightPrintedStr = docValueString(rightPrinted);
    let shouldBreak = docShouldBreakP(leftPrinted) || docShouldBreakP(rightPrinted);
    const isMultilineStringLiteral = estreeStringLiteralP(left) && left.value.match('\\n$');
    const isMultilineBinaryExpression = !isMultilineStringLiteral && (0, estree_1.estreeTypeP)(left, 'BinaryExpression') && estreeStringLiteralP(left.right) && left.right.value.match(new RegExp('\\n$'));
    const isMultilineString = isMultilineStringLiteral || isMultilineBinaryExpression;
    let result;
    if (!(estreeSimpleP(left) || ((0, estree_1.estreeTypeP)(left, type_) && (left.operator === operator)))) {
        leftPrintedStr = docWrap(leftPrintedStr, Object.assign(Object.assign({}, options), { 'has-comments': docHasCommentsP(leftPrinted) }));
    }
    if (!(estreeSimpleP(right) || ((0, estree_1.estreeTypeP)(right, type_) && (right.operator === operator) && ['+', '*', '&&', '||'].includes(operator)))) {
        rightPrintedStr = docWrap(rightPrintedStr, Object.assign(Object.assign({}, options), { 'has-comments': docHasCommentsP(rightPrinted) }));
    }
    if (shouldBreak) {
        result = ['(', line, align(1, leftPrintedStr), estreeHasTrailingCommentP(left) ? [line, align(1, operator)] : [space, operator], line, align(1, rightPrintedStr), line, ')'];
    }
    else if (isMultilineString) {
        result = [leftPrintedStr, space, operator, line, indent(rightPrintedStr)];
    }
    else {
        result = [leftPrintedStr, space, operator, space, rightPrintedStr];
    }
    return group(result, {
        'should-break': shouldBreak
    });
}
printBinaryExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-binary-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('estree-type'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator-printed'), Symbol.for('operator')], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-printed'), [Symbol.for('print-node'), Symbol.for('left'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('left-printed-str'), [Symbol.for('doc-value-string'), Symbol.for('left-printed')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-printed'), [Symbol.for('print-node'), Symbol.for('right'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('right-printed-str'), [Symbol.for('doc-value-string'), Symbol.for('right-printed')]], [Symbol.for('define'), Symbol.for('should-break'), [Symbol.for('or'), [Symbol.for('doc-should-break?'), Symbol.for('left-printed')], [Symbol.for('doc-should-break?'), Symbol.for('right-printed')]]], [Symbol.for('define'), Symbol.for('is-multiline-string-literal'), [Symbol.for('and'), [Symbol.for('estree-string-literal?'), Symbol.for('left')], [Symbol.for('regexp-match'), '\\n$', [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('left')]]]], [Symbol.for('define'), Symbol.for('is-multiline-binary-expression'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('is-multiline-string-literal')], [Symbol.for('estree-type?'), Symbol.for('left'), 'BinaryExpression'], [Symbol.for('estree-string-literal?'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('left')]], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n$'], [Symbol.for('~>'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('_')], [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('_')]]]]], [Symbol.for('define'), Symbol.for('is-multiline-string'), [Symbol.for('or'), Symbol.for('is-multiline-string-literal'), Symbol.for('is-multiline-binary-expression')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('estree-simple?'), Symbol.for('left')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('left'), Symbol.for('type_')], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('left')], Symbol.for('operator')]]], [Symbol.for('set!'), Symbol.for('left-printed-str'), [Symbol.for('doc-wrap'), Symbol.for('left-printed-str'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'has-comments', [Symbol.for('doc-has-comments?'), Symbol.for('left-printed')]]]]]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('estree-simple?'), Symbol.for('right')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('right'), Symbol.for('type_')], [Symbol.for('eq?'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('right')], Symbol.for('operator')], [Symbol.for('memq?'), Symbol.for('operator'), [Symbol.for('quote'), ['+', '*', '&&', '||']]]]], [Symbol.for('set!'), Symbol.for('right-printed-str'), [Symbol.for('doc-wrap'), Symbol.for('right-printed-str'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'has-comments', [Symbol.for('doc-has-comments?'), Symbol.for('right-printed')]]]]]], [Symbol.for('cond'), [Symbol.for('should-break'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '(', Symbol.for('line'), [Symbol.for('align'), 1, Symbol.for('left-printed-str')], [Symbol.for('if'), [Symbol.for('estree-has-trailing-comment?'), Symbol.for('left')], [Symbol.for('list'), Symbol.for('line'), [Symbol.for('align'), 1, Symbol.for('operator')]], [Symbol.for('list'), Symbol.for('space'), Symbol.for('operator')]], Symbol.for('line'), [Symbol.for('align'), 1, Symbol.for('right-printed-str')], Symbol.for('line'), ')']]], [Symbol.for('is-multiline-string'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('left-printed-str'), Symbol.for('space'), Symbol.for('operator'), Symbol.for('line'), [Symbol.for('indent'), Symbol.for('right-printed-str')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('left-printed-str'), Symbol.for('space'), Symbol.for('operator'), Symbol.for('space'), Symbol.for('right-printed-str')]]]], [Symbol.for('group'), Symbol.for('result'), [Symbol.for('js-obj'), 'should-break', Symbol.for('should-break')]]];
/**
 * Print a `LogicalExpression` ESTree node to a `Doc` object.
 */
function printLogicalExpression(node, options = {}) {
    return printBinaryExpression(node, options);
}
printLogicalExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-logical-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-binary-expression'), Symbol.for('node'), Symbol.for('options')]];
/**
 * Print an `AssignmentExpression` ESTree node to a `Doc` object.
 */
function printAssignmentExpression(node, options = {}) {
    // TODO: Break up statement if one of the sides have comments.
    const language = options['language'];
    const operator = node.operator;
    const operatorPrinted = operator;
    const left = node.left;
    let leftPrinted = printNode(left, options);
    const right = node.right;
    const rightPrinted = printNode(right, Object.assign(Object.assign({}, options), { noImplicitAny: false }));
    let result = [leftPrinted, space, operator, docHasCommentsP(rightPrinted) ? [line, indent(rightPrinted)] : [space, rightPrinted]];
    if ((0, estree_1.estreeTypeP)(left, 'ObjectPattern')) {
        result = docWrap(result, options);
    }
    return result;
}
printAssignmentExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-assignment-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator-printed'), Symbol.for('operator')], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-printed'), [Symbol.for('print-node'), Symbol.for('left'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-printed'), [Symbol.for('print-node'), Symbol.for('right'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('left-printed'), Symbol.for('space'), Symbol.for('operator'), [Symbol.for('if'), [Symbol.for('doc-has-comments?'), Symbol.for('right-printed')], [Symbol.for('list'), Symbol.for('line'), [Symbol.for('indent'), Symbol.for('right-printed')]], [Symbol.for('list'), Symbol.for('space'), Symbol.for('right-printed')]]]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('left'), 'ObjectPattern'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('doc-wrap'), Symbol.for('result'), Symbol.for('options')]]], Symbol.for('result')];
/**
 * Print an `AssignmentPattern` ESTree node to a `Doc` object.
 */
function printAssignmentPattern(node, options = {}) {
    return printNode(new estree_1.VariableDeclarator(node.left, node.right), options);
}
printAssignmentPattern.lispSource = [Symbol.for('define'), [Symbol.for('print-assignment-pattern'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-node'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')], [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], Symbol.for('options')]];
/**
 * Print a `CallExpression` ESTree node to a `Doc` object.
 */
function printCallExpression(node, options = {}) {
    const callee = node.callee;
    const calleeType = (0, estree_1.estreeType)(callee);
    let calleePrinted = printNode(callee, options);
    const args = node.arguments;
    const argsPrinted = args.map(function (x) {
        return printNode(x, options);
    });
    const optional = node.optional;
    if (estreeComplexP(callee)) {
        calleePrinted = docWrap(calleePrinted, options);
    }
    return [calleePrinted, optional ? '?.' : empty, '(', join([',', space], argsPrinted), ')'];
}
printCallExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-call-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('callee'), [Symbol.for('get-field'), Symbol.for('callee'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('callee-type'), [Symbol.for('estree-type'), Symbol.for('callee')]], [Symbol.for('define'), Symbol.for('callee-printed'), [Symbol.for('print-node'), Symbol.for('callee'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('arguments'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('args-printed'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('args')]], [Symbol.for('define'), Symbol.for('optional'), [Symbol.for('get-field'), Symbol.for('optional'), Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('estree-complex?'), Symbol.for('callee')], [Symbol.for('set!'), Symbol.for('callee-printed'), [Symbol.for('doc-wrap'), Symbol.for('callee-printed'), Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('callee-printed'), [Symbol.for('if'), Symbol.for('optional'), '?.', Symbol.for('empty')], '(', [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('args-printed')], ')']];
/**
 * Print a `SequenceExpression` ESTree node to a `Doc` object.
 */
function printSequenceExpression(node, options = {}) {
    const expressions = node.expressions;
    const expressionsPrinted = expressions.map(function (x) {
        return printNode(x, options);
    });
    let result;
    result = join([',', space], expressionsPrinted);
    if (expressions.length > 1) {
        result = docWrap(result, options);
    }
    return result;
}
printSequenceExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-sequence-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expressions'), [Symbol.for('get-field'), Symbol.for('expressions'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('expressions-printed'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('expressions')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('expressions-printed')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('expressions')], 1], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('doc-wrap'), Symbol.for('result'), Symbol.for('options')]]], Symbol.for('result')];
/**
 * Print a `BlockStatement` ESTree node to a `Doc` object.
 */
function printBlockStatement(node, options = {}) {
    const body = node.body;
    const bodyModified = (() => {
        if (node.comments && (body.length > 0)) {
            body[0].comments = [...node.comments, ...(body[0].comments || [])];
            node.comments = [];
        }
        return node.body;
    })();
    const bodyIndented = indent(join(line, bodyModified.map(function (x) {
        return printNode(x, options);
    })));
    const bodyPrinted = printDoc(bodyIndented);
    return ['{', line, bodyIndented, (bodyPrinted === '') ? empty : line, '}'];
}
printBlockStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-block-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-modified'), [Symbol.for('begin'), [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('body')], 0]], [Symbol.for('set-field!'), Symbol.for('comments'), [Symbol.for('first'), Symbol.for('body')], [Symbol.for('append'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')], [Symbol.for('or'), [Symbol.for('get-field'), Symbol.for('comments'), [Symbol.for('first'), Symbol.for('body')]], [Symbol.for('quote'), []]]]], [Symbol.for('set!'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')], [Symbol.for('quote'), []]]], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]]], [Symbol.for('define'), Symbol.for('body-indented'), [Symbol.for('indent'), [Symbol.for('~>'), Symbol.for('body-modified'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), Symbol.for('line'), Symbol.for('_')]]]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-doc'), Symbol.for('body-indented')]], [Symbol.for('list'), '{', Symbol.for('line'), Symbol.for('body-indented'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('body-printed'), ''], Symbol.for('empty'), Symbol.for('line')], '}']];
/**
 * Print a `MemberExpression` ESTree node to a `Doc` object.
 */
function printMemberExpression(node, options = {}) {
    const language = options['language'];
    const object = node.object;
    const objectType = (0, estree_1.estreeType)(object);
    let objectPrinted = printNode(object, options);
    const property = node.property;
    const propertyPrinted = printNode(property, options);
    const computed = node.computed;
    const optional = node.optional;
    if (!estreeSimpleP(object) || (objectType === 'ObjectExpression')) {
        // If the object expression is complicated, wrap it in
        // parentheses.
        objectPrinted = docWrap(objectPrinted, options);
    }
    if (computed) {
        // Kludge: prevent errors with expressions like
        // `x[y]`, where `y` is `any`-typed.
        // TODO: Move this code into the compiler.
        if ((language === 'TypeScript') && !['Literal', 'UnaryExpression', 'BinaryExpression'].includes((0, estree_1.estreeType)(property))) {
            objectPrinted = docWrap([objectPrinted, ' as any'], options);
        }
        return [objectPrinted, '[', propertyPrinted, ']'];
    }
    else {
        return [objectPrinted, optional ? '?.' : '.', propertyPrinted];
    }
}
printMemberExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-member-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('object'), [Symbol.for('get-field'), Symbol.for('object'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('object-type'), [Symbol.for('estree-type'), Symbol.for('object')]], [Symbol.for('define'), Symbol.for('object-printed'), [Symbol.for('print-node'), Symbol.for('object'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('property'), [Symbol.for('get-field'), Symbol.for('property'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('property-printed'), [Symbol.for('print-node'), Symbol.for('property'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('computed'), [Symbol.for('get-field'), Symbol.for('computed'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('optional'), [Symbol.for('get-field'), Symbol.for('optional'), Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('estree-simple?'), Symbol.for('object')]], [Symbol.for('eq?'), Symbol.for('object-type'), 'ObjectExpression']], [Symbol.for('set!'), Symbol.for('object-printed'), [Symbol.for('doc-wrap'), Symbol.for('object-printed'), Symbol.for('options')]]], [Symbol.for('cond'), [Symbol.for('computed'), [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('not'), [Symbol.for('memq?'), [Symbol.for('estree-type'), Symbol.for('property')], [Symbol.for('quote'), ['Literal', 'UnaryExpression', 'BinaryExpression']]]]], [Symbol.for('set!'), Symbol.for('object-printed'), [Symbol.for('doc-wrap'), [Symbol.for('list'), Symbol.for('object-printed'), ' as any'], Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('object-printed'), '[', Symbol.for('property-printed'), ']']], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('object-printed'), [Symbol.for('if'), Symbol.for('optional'), '?.', '.'], Symbol.for('property-printed')]]]];
/**
 * Print an `UpdateExpression` ESTree node to a `Doc` object.
 */
function printUpdateExpression(node, options = {}) {
    return printUnaryExpression(node, options);
}
printUpdateExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-update-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-unary-expression'), Symbol.for('node'), Symbol.for('options')]];
/**
 * Print a `SpreadElement` ESTree node to a `Doc` object.
 */
function printSpreadElement(node, options = {}) {
    const language = options['language'];
    const noImplicitAny = options['noImplicitAny'];
    const argument = node.argument;
    let argumentPrinted = printNode(argument, Object.assign(Object.assign({}, options), { noImplicitAny: false }));
    let type_ = node.typeAnnotation;
    if (noImplicitAny && !type_) {
        type_ = new estree_1.TSArrayType(new estree_1.TSAnyKeyword());
    }
    if (!estreeSimpleP(argument)) {
        argumentPrinted = docWrap(argumentPrinted, options);
    }
    return ['...', argumentPrinted, ((language === 'TypeScript') && type_) ? [':', space, printNode(type_, options)] : empty];
}
printSpreadElement.lispSource = [Symbol.for('define'), [Symbol.for('print-spread-element'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('no-implicit-any'), [Symbol.for('oget'), Symbol.for('options'), 'noImplicitAny']], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('argument-printed'), [Symbol.for('print-node'), Symbol.for('argument'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('no-implicit-any'), [Symbol.for('not'), Symbol.for('type_')]], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('new'), Symbol.for('TSArrayType'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]], [Symbol.for('unless'), [Symbol.for('estree-simple?'), Symbol.for('argument')], [Symbol.for('set!'), Symbol.for('argument-printed'), [Symbol.for('doc-wrap'), Symbol.for('argument-printed'), Symbol.for('options')]]], [Symbol.for('list'), '...', Symbol.for('argument-printed'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], Symbol.for('type_')], [Symbol.for('list'), ':', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('type_'), Symbol.for('options')]], Symbol.for('empty')]]];
/**
 * Print a `RestElement` ESTree node to a `Doc` object.
 */
function printRestElement(node, options = {}) {
    return printSpreadElement(node, options);
}
printRestElement.lispSource = [Symbol.for('define'), [Symbol.for('print-rest-element'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-spread-element'), Symbol.for('node'), Symbol.for('options')]];
/**
 * Print a function declaration or function expression to a
 * `Doc` object. Also handles arrow functions.
 */
function printFunction(node, options = {}, settings = {}) {
    const language = options['language'];
    const arrow = settings['arrow'];
    const async_ = node.async;
    const returnTypeSetting = settings['returnType'];
    const returnType = (typeof returnTypeSetting === 'string') ? returnTypeSetting : node.returnType;
    const returnTypePrinted = (typeof returnType === 'string') ? returnType : (returnType ? printTsType(returnType, options) : (async_ ? 'Promise<any>' : 'any'));
    return [async_ ? ['async', space] : empty, arrow ? empty : ['function', space], node.id ? printNode(node.id, options) : empty, '(', join([',', space], node.params.map(function (x) {
            return printNode(x, Object.assign(Object.assign({}, options), { noImplicitAny: true }));
        })), ')', ((language === 'TypeScript') && (returnTypePrinted !== '')) ? [':', space, returnTypePrinted] : empty, arrow ? [space, '=>', space] : space, printNode(node.body, options)];
}
printFunction.lispSource = [Symbol.for('define'), [Symbol.for('print-function'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('arrow'), [Symbol.for('oget'), Symbol.for('settings'), 'arrow']], [Symbol.for('define'), Symbol.for('async_'), [Symbol.for('get-field'), Symbol.for('async'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('return-type-setting'), [Symbol.for('oget'), Symbol.for('settings'), 'returnType']], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('if'), [Symbol.for('string?'), Symbol.for('return-type-setting')], Symbol.for('return-type-setting'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('node')]]], [Symbol.for('define'), Symbol.for('return-type-printed'), [Symbol.for('cond'), [[Symbol.for('string?'), Symbol.for('return-type')], Symbol.for('return-type')], [Symbol.for('return-type'), [Symbol.for('print-ts-type'), Symbol.for('return-type'), Symbol.for('options')]], [Symbol.for('async_'), 'Promise<any>'], [Symbol.for('else'), 'any']]], [Symbol.for('list'), [Symbol.for('if'), Symbol.for('async_'), [Symbol.for('list'), 'async', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('arrow'), Symbol.for('empty'), [Symbol.for('list'), 'function', Symbol.for('space')]], [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')], [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')], Symbol.for('options')], Symbol.for('empty')], '(', [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#t')]]]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], ')', [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('return-type-printed'), '']]], [Symbol.for('list'), ':', Symbol.for('space'), Symbol.for('return-type-printed')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('arrow'), [Symbol.for('list'), Symbol.for('space'), '=>', Symbol.for('space')], Symbol.for('space')], [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')], Symbol.for('options')]]];
/**
 * Print a `FunctionDeclaration` ESTree node to a `Doc` object.
 */
function printFunctionDeclaration(node, options = {}) {
    return printFunction(node, options);
}
printFunctionDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-function-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-function'), Symbol.for('node'), Symbol.for('options')]];
/**
 * Print a `FunctionExpression` ESTree node to a `Doc` object.
 */
function printFunctionExpression(node, options = {}) {
    return printFunction(node, options);
}
printFunctionExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-function-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-function'), Symbol.for('node'), Symbol.for('options')]];
/**
 * Print a `ArrowFunctionExpression` ESTree node to a `Doc` object.
 */
function printArrowFunctionExpression(node, options = {}) {
    return printFunction(node, options, {
        arrow: true
    });
}
printArrowFunctionExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-arrow-function-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-function'), Symbol.for('node'), Symbol.for('options'), [Symbol.for('js-obj'), 'arrow', Symbol.for('#t')]]];
/**
 * Print a `VariableDeclaration` ESTree node to a `Doc` object.
 */
function printVariableDeclaration(node, options = {}) {
    return [node.kind, space, join([',', space], node.declarations.map(function (x) {
            return printNode(x, options);
        })), ';'];
}
printVariableDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-variable-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), [Symbol.for('get-field'), Symbol.for('kind'), Symbol.for('node')], Symbol.for('space'), [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], ';']];
/**
 * Print a `VariableDeclarator` ESTree node to a `Doc` object.
 */
function printVariableDeclarator(node, options = {}) {
    const language = options['language'];
    const id = node.id;
    const idPrinted = printNode(id, Object.assign(Object.assign({}, options), { noImplicitAny: true }));
    if (node.init) {
        const init = node.init;
        const initPrinted = printNode(init, Object.assign(Object.assign({}, options), { noImplicitAny: false }));
        return [idPrinted, space, '=', docHasCommentsP(initPrinted) ? [line, indent(initPrinted)] : [space, initPrinted]];
    }
    else {
        return idPrinted;
    }
}
printVariableDeclarator.lispSource = [Symbol.for('define'), [Symbol.for('print-variable-declarator'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('id-printed'), [Symbol.for('print-node'), Symbol.for('id'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#t')]]]], [Symbol.for('cond'), [[Symbol.for('get-field'), Symbol.for('init'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('init-printed'), [Symbol.for('print-node'), Symbol.for('init'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]]], [Symbol.for('list'), Symbol.for('id-printed'), Symbol.for('space'), '=', [Symbol.for('if'), [Symbol.for('doc-has-comments?'), Symbol.for('init-printed')], [Symbol.for('list'), Symbol.for('line'), [Symbol.for('indent'), Symbol.for('init-printed')]], [Symbol.for('list'), Symbol.for('space'), Symbol.for('init-printed')]]]], [Symbol.for('else'), Symbol.for('id-printed')]]];
/**
 * Print an `IfStatement` ESTree node to a `Doc` object.
 */
function printIfStatement(node, options = {}) {
    const test = node.test;
    let testPrinted = printNode(test, options);
    const testPrintedStr = (0, estree_1.estreeTypeP)(test, 'AssignmentExpression') ? docWrap(docValueString(testPrinted), options) : docValueString(testPrinted);
    const consequent = node.consequent;
    let consequentPrinted = printNode(consequent, options);
    const alternate = node.alternate;
    let result = 'if (' + testPrintedStr + ')' + (docShouldBreakP(consequentPrinted) ? line : space) + docValueString(consequentPrinted);
    if (alternate) {
        let alternatePrinted = printNode(alternate, options);
        result = result + ' else ' + docValueString(alternatePrinted);
    }
    return result;
}
printIfStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-if-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('test-printed-str'), [Symbol.for('if'), [Symbol.for('estree-type?'), Symbol.for('test'), 'AssignmentExpression'], [Symbol.for('doc-wrap'), [Symbol.for('doc-value-string'), Symbol.for('test-printed')], Symbol.for('options')], [Symbol.for('doc-value-string'), Symbol.for('test-printed')]]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('get-field'), Symbol.for('consequent'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('consequent-printed'), [Symbol.for('print-node'), Symbol.for('consequent'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('alternate'), [Symbol.for('get-field'), Symbol.for('alternate'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('string-append'), 'if (', Symbol.for('test-printed-str'), ')', [Symbol.for('if'), [Symbol.for('doc-should-break?'), Symbol.for('consequent-printed')], Symbol.for('line'), Symbol.for('space')], [Symbol.for('doc-value-string'), Symbol.for('consequent-printed')]]], [Symbol.for('when'), Symbol.for('alternate'), [Symbol.for('define'), Symbol.for('alternate-printed'), [Symbol.for('print-node'), Symbol.for('alternate'), Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('string-append'), Symbol.for('result'), ' else ', [Symbol.for('doc-value-string'), Symbol.for('alternate-printed')]]]], Symbol.for('result')];
/**
 * Print a `ConditionalExpression` ESTree node to a `Doc` object.
 */
function printConditionalExpression(node, options = {}) {
    const test = node.test;
    let testPrinted = printNode(test, options);
    const consequent = node.consequent;
    let consequentPrinted = printNode(consequent, options);
    const alternate = node.alternate;
    let alternatePrinted = printNode(alternate, options);
    let result;
    if (!estreeSimpleP(test)) {
        testPrinted = docWrap(testPrinted, options);
    }
    if (!estreeSimpleP(consequent)) {
        consequentPrinted = docWrap(consequentPrinted, options);
    }
    if (!((0, estree_1.estreeTypeP)(alternate, 'SequenceExpression') || estreeSimpleP(alternate))) {
        alternatePrinted = docWrap(alternatePrinted, options);
    }
    return [testPrinted, space, '?', space, consequentPrinted, space, ':', space, alternatePrinted];
}
printConditionalExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-conditional-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('get-field'), Symbol.for('consequent'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('consequent-printed'), [Symbol.for('print-node'), Symbol.for('consequent'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('alternate'), [Symbol.for('get-field'), Symbol.for('alternate'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('alternate-printed'), [Symbol.for('print-node'), Symbol.for('alternate'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('unless'), [Symbol.for('estree-simple?'), Symbol.for('test')], [Symbol.for('set!'), Symbol.for('test-printed'), [Symbol.for('doc-wrap'), Symbol.for('test-printed'), Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('estree-simple?'), Symbol.for('consequent')], [Symbol.for('set!'), Symbol.for('consequent-printed'), [Symbol.for('doc-wrap'), Symbol.for('consequent-printed'), Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('estree-type?'), Symbol.for('alternate'), 'SequenceExpression'], [Symbol.for('estree-simple?'), Symbol.for('alternate')]], [Symbol.for('set!'), Symbol.for('alternate-printed'), [Symbol.for('doc-wrap'), Symbol.for('alternate-printed'), Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('test-printed'), Symbol.for('space'), '?', Symbol.for('space'), Symbol.for('consequent-printed'), Symbol.for('space'), ':', Symbol.for('space'), Symbol.for('alternate-printed')]];
/**
 * Print a `WhileStatement` ESTree node to a `Doc` object.
 */
function printWhileStatement(node, options = {}) {
    const test = node.test;
    let testPrinted = printNode(test, options);
    const body = node.body;
    const bodyPrinted = printNode(body, options);
    if ((0, estree_1.estreeTypeP)(test, 'AssignmentExpression')) {
        testPrinted = docWrap(testPrinted, options);
    }
    return ['while', space, '(', testPrinted, ')', space, bodyPrinted];
}
printWhileStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-while-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('test'), 'AssignmentExpression'], [Symbol.for('set!'), Symbol.for('test-printed'), [Symbol.for('doc-wrap'), Symbol.for('test-printed'), Symbol.for('options')]]], [Symbol.for('list'), 'while', Symbol.for('space'), '(', Symbol.for('test-printed'), ')', Symbol.for('space'), Symbol.for('body-printed')]];
/**
 * Print a `DoWhileStatement` ESTree node to a `Doc` object.
 */
function printDoWhileStatement(node, options = {}) {
    const test = node.test;
    let testPrinted = printNode(test, options);
    const body = node.body;
    const bodyPrinted = printNode(body, options);
    if ((0, estree_1.estreeTypeP)(test, 'AssignmentExpression')) {
        testPrinted = docWrap(testPrinted, options);
    }
    return ['do', space, bodyPrinted, space, 'while', space, '(', testPrinted, ')', ';'];
}
printDoWhileStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-do-while-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('test'), 'AssignmentExpression'], [Symbol.for('set!'), Symbol.for('test-printed'), [Symbol.for('doc-wrap'), Symbol.for('test-printed'), Symbol.for('options')]]], [Symbol.for('list'), 'do', Symbol.for('space'), Symbol.for('body-printed'), Symbol.for('space'), 'while', Symbol.for('space'), '(', Symbol.for('test-printed'), ')', ';']];
/**
 * Print a `ForStatement` ESTree node to a `Doc` object.
 */
function printForStatement(node, options = {}) {
    const init = node.init;
    const initPrinted = printDoc(printNode(init, options), options).replace(new RegExp(';$'), '');
    const test = node.test;
    let testPrinted = printDoc(printNode(test, options), options);
    const update = node.update;
    const updatePrinted = printDoc(printNode(update, options), options).replace(new RegExp(';$'), '');
    const body = node.body;
    const bodyPrinted = printNode(body, options);
    return ['for', space, '(', initPrinted, ';', space, testPrinted, ';', space, updatePrinted, ')', space, bodyPrinted];
}
printForStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-for-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('init-printed'), [Symbol.for('~>'), [Symbol.for('print-node'), Symbol.for('init'), Symbol.for('options')], [Symbol.for('print-doc'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ';$'], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('update'), [Symbol.for('get-field'), Symbol.for('update'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('update-printed'), [Symbol.for('~>'), [Symbol.for('print-node'), Symbol.for('update'), Symbol.for('options')], [Symbol.for('print-doc'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ';$'], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('list'), 'for', Symbol.for('space'), '(', Symbol.for('init-printed'), ';', Symbol.for('space'), Symbol.for('test-printed'), ';', Symbol.for('space'), Symbol.for('update-printed'), ')', Symbol.for('space'), Symbol.for('body-printed')]];
/**
 * Print a `ForOfStatement` ESTree node to a `Doc` object.
 */
function printForOfStatement(node, options = {}) {
    const language = options['language'];
    const left = node.left;
    let leftPrinted = printDoc(printNode(left, options), options).replace(new RegExp(';$'), '');
    const right = node.right;
    const rightPrinted = printNode(right, options);
    const body = node.body;
    const bodyPrinted = printNode(body, options);
    let resultStr;
    if (language === 'TypeScript') {
        leftPrinted = leftPrinted.replace(new RegExp(': any$'), '');
    }
    return ['for', space, '(', leftPrinted, space, 'of', space, rightPrinted, ')', space, bodyPrinted];
}
printForOfStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-for-of-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-printed'), [Symbol.for('~>'), [Symbol.for('print-node'), Symbol.for('left'), Symbol.for('options')], [Symbol.for('print-doc'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ';$'], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-printed'), [Symbol.for('print-node'), Symbol.for('right'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result-str')], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('set!'), Symbol.for('left-printed'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ': any$'], Symbol.for('left-printed'), '']]], [Symbol.for('list'), 'for', Symbol.for('space'), '(', Symbol.for('left-printed'), Symbol.for('space'), 'of', Symbol.for('space'), Symbol.for('right-printed'), ')', Symbol.for('space'), Symbol.for('body-printed')]];
/**
 * Print a `ForInStatement` ESTree node to a `Doc` object.
 */
function printForInStatement(node, options = {}) {
    const language = options['language'];
    const left = node.left;
    let leftPrinted = printDoc(printNode(left, options), options).replace(new RegExp(';$'), '');
    const right = node.right;
    const rightPrinted = printNode(right, options);
    const body = node.body;
    const bodyPrinted = printNode(body, options);
    return ['for', space, '(', leftPrinted, space, 'in', space, rightPrinted, (language === 'TypeScript') ? ' as any[]' : empty, ')', space, '{', line, indent(bodyPrinted), line, '}'];
}
printForInStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-for-in-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-printed'), [Symbol.for('~>'), [Symbol.for('print-node'), Symbol.for('left'), Symbol.for('options')], [Symbol.for('print-doc'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ';$'], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-printed'), [Symbol.for('print-node'), Symbol.for('right'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('list'), 'for', Symbol.for('space'), '(', Symbol.for('left-printed'), Symbol.for('space'), 'in', Symbol.for('space'), Symbol.for('right-printed'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], ' as any[]', Symbol.for('empty')], ')', Symbol.for('space'), '{', Symbol.for('line'), [Symbol.for('indent'), Symbol.for('body-printed')], Symbol.for('line'), '}']];
/**
 * Print a `TryStatement` ESTree node to a `Doc` object.
 */
function printTryStatement(node, options = {}) {
    const block = node.block;
    const blockPrinted = printNode(block, options);
    const handler = node.handler;
    const finalizer = node.finalizer;
    let result = ['try', space, blockPrinted];
    if (handler) {
        const handlerParam = handler.param;
        const handlerParamPrinted = handlerParam ? printNode(handlerParam, options) : false;
        const handlerBodyPrinted = printNode(handler.body, options);
        result = [...result, ...[space, 'catch', space], ...(handlerParam ? ['(', handlerParamPrinted, ')', space] : []), ...['{', line, indent(handlerBodyPrinted), line, '}']];
    }
    if (finalizer) {
        const finalizerPrinted = printNode(finalizer, options);
        result = [...result, ...[space, 'finally', space, finalizerPrinted]];
    }
    return result;
}
printTryStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-try-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('block'), [Symbol.for('get-field'), Symbol.for('block'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('block-printed'), [Symbol.for('print-node'), Symbol.for('block'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('handler'), [Symbol.for('get-field'), Symbol.for('handler'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('finalizer'), [Symbol.for('get-field'), Symbol.for('finalizer'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('list'), 'try', Symbol.for('space'), Symbol.for('block-printed')]], [Symbol.for('when'), Symbol.for('handler'), [Symbol.for('define'), Symbol.for('handler-param'), [Symbol.for('get-field'), Symbol.for('param'), Symbol.for('handler')]], [Symbol.for('define'), Symbol.for('handler-param-printed'), [Symbol.for('if'), Symbol.for('handler-param'), [Symbol.for('print-node'), Symbol.for('handler-param'), Symbol.for('options')], Symbol.for('#f')]], [Symbol.for('define'), Symbol.for('handler-body-printed'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('handler')], Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('space'), 'catch', Symbol.for('space')], [Symbol.for('if'), Symbol.for('handler-param'), [Symbol.for('list'), '(', Symbol.for('handler-param-printed'), ')', Symbol.for('space')], [Symbol.for('quote'), []]], [Symbol.for('list'), '{', Symbol.for('line'), [Symbol.for('indent'), Symbol.for('handler-body-printed')], Symbol.for('line'), '}']]]], [Symbol.for('when'), Symbol.for('finalizer'), [Symbol.for('define'), Symbol.for('finalizer-printed'), [Symbol.for('print-node'), Symbol.for('finalizer'), Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('space'), 'finally', Symbol.for('space'), Symbol.for('finalizer-printed')]]]], Symbol.for('result')];
/**
 * Print a `ClassDeclaration` ESTree node to a `Doc` object.
 */
function printClassDeclaration(node, options = {}) {
    const id = node.id;
    const body = node.body;
    const bodyIndented = indent(printNode(body, options));
    const bodyPrinted = printDoc(bodyIndented, options);
    const superClass = node.superClass;
    return ['class', space, id ? [printNode(id, options), space] : empty, superClass ? ['extends', space, printNode(superClass, options), space] : empty, '{', line, bodyIndented, (bodyPrinted === '') ? empty : line, '}'];
}
printClassDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-class-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-indented'), [Symbol.for('indent'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-doc'), Symbol.for('body-indented'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('super-class'), [Symbol.for('get-field'), Symbol.for('superClass'), Symbol.for('node')]], [Symbol.for('list'), 'class', Symbol.for('space'), [Symbol.for('if'), Symbol.for('id'), [Symbol.for('list'), [Symbol.for('print-node'), Symbol.for('id'), Symbol.for('options')], Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('super-class'), [Symbol.for('list'), 'extends', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('super-class'), Symbol.for('options')], Symbol.for('space')], Symbol.for('empty')], '{', Symbol.for('line'), Symbol.for('body-indented'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('body-printed'), ''], Symbol.for('empty'), Symbol.for('line')], '}']];
/**
 * Print a `ClassExpression` ESTree node to a `Doc` object.
 */
function printClassExpression(node, options = {}) {
    return printClassDeclaration(new estree_1.ClassDeclaration(null, node.body, node.superClass), options);
}
printClassExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-class-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-class-declaration'), [Symbol.for('new'), Symbol.for('ClassDeclaration'), Symbol.for('js/null'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')], [Symbol.for('get-field'), Symbol.for('superClass'), Symbol.for('node')]], Symbol.for('options')]];
/**
 * Print a `ClassBody` ESTree node to a `Doc` object.
 */
function printClassBody(node, options = {}) {
    return join([line, line], node.body.map(function (x) {
        return printNode(x, options);
    }));
}
printClassBody.lispSource = [Symbol.for('define'), [Symbol.for('print-class-body'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('_')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), Symbol.for('line'), Symbol.for('line')], Symbol.for('_')]]];
/**
 * Print a `PropertyDefinition` ESTree node to a `Doc` object.
 */
function printPropertyDefinition(node, options = {}) {
    const language = options['language'];
    const key = node.key;
    const value = node.value;
    const staticFlag = node.static;
    const accessibility = node.accessibility;
    return [((language === 'TypeScript') && (accessibility === 'private')) ? ['private', space] : empty, staticFlag ? ['static', space] : empty, printNode(key, options), (language === 'TypeScript') ? [':', space, 'any'] : empty, value ? [space, '=', space, printNode(value, options)] : empty, ';'];
}
printPropertyDefinition.lispSource = [Symbol.for('define'), [Symbol.for('print-property-definition'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-field'), Symbol.for('key'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('static-flag'), [Symbol.for('get-field'), Symbol.for('static'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('accessibility'), [Symbol.for('get-field'), Symbol.for('accessibility'), Symbol.for('node')]], [Symbol.for('list'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('eq?'), Symbol.for('accessibility'), 'private']], [Symbol.for('list'), 'private', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('static-flag'), [Symbol.for('list'), 'static', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options')], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('list'), ':', Symbol.for('space'), 'any'], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('value'), [Symbol.for('list'), Symbol.for('space'), '=', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('value'), Symbol.for('options')]], Symbol.for('empty')], ';']];
/**
 * Print a `MethodDefinition` ESTree node to a `Doc` object.
 */
function printMethodDefinition(node, options = {}) {
    const language = options['language'];
    const key = node.key;
    let keyPrinted = printNode(key, options);
    const keyPrintedStr = printDoc(keyPrinted, options);
    const value = node.value;
    const valuePrinted = printDoc(printFunction(value, options, {
        returnType: (keyPrintedStr === 'constructor') ? '' : 'any'
    }), options).replace(new RegExp('^function '), '');
    const staticFlag = node.static;
    const computedFlag = node.computed;
    const generatorFlag = value.generator;
    const accessibility = node.accessibility;
    return [((language === 'TypeScript') && (accessibility === 'private')) ? ['private', space] : empty, staticFlag ? ['static', space] : empty, generatorFlag ? '*' : empty, computedFlag ? ['[', keyPrinted, ']'] : keyPrinted, valuePrinted];
}
printMethodDefinition.lispSource = [Symbol.for('define'), [Symbol.for('print-method-definition'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-field'), Symbol.for('key'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('key-printed'), [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('key-printed-str'), [Symbol.for('print-doc'), Symbol.for('key-printed'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('value-printed'), [Symbol.for('~>'), Symbol.for('value'), [Symbol.for('print-function'), Symbol.for('_'), Symbol.for('options'), [Symbol.for('js-obj'), 'returnType', [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('key-printed-str'), 'constructor'], '', 'any']]], [Symbol.for('print-doc'), Symbol.for('_'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^function '], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('static-flag'), [Symbol.for('get-field'), Symbol.for('static'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('computed-flag'), [Symbol.for('get-field'), Symbol.for('computed'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('generator-flag'), [Symbol.for('get-field'), Symbol.for('generator'), Symbol.for('value')]], [Symbol.for('define'), Symbol.for('accessibility'), [Symbol.for('get-field'), Symbol.for('accessibility'), Symbol.for('node')]], [Symbol.for('list'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('eq?'), Symbol.for('accessibility'), 'private']], [Symbol.for('list'), 'private', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('static-flag'), [Symbol.for('list'), 'static', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('generator-flag'), '*', Symbol.for('empty')], [Symbol.for('if'), Symbol.for('computed-flag'), [Symbol.for('list'), '[', Symbol.for('key-printed'), ']'], Symbol.for('key-printed')], Symbol.for('value-printed')]];
/**
 * Print an `ArrayExpression` ESTree node to a `Doc` object.
 */
function printArrayExpression(node, options = {}) {
    const language = options['language'];
    const noImplicitAny = options['noImplicitAny'];
    let type_ = node.typeAnnotation;
    const printedExpressions = [];
    let shouldBreak = false;
    let printedExp;
    let result;
    for (let exp of node.elements) {
        if (exp) {
            printedExp = printNode(exp, Object.assign(Object.assign({}, options), { noImplicitAny: false }));
        }
        else {
            printedExp = empty;
        }
        printedExpressions.push(docValueString(printedExp));
        if (docShouldBreakP(printedExp)) {
            shouldBreak = true;
        }
    }
    if (shouldBreak) {
        result = ['[', line, join([',', line], printedExpressions.map(function (x) {
                return align(1, x);
            })), line, ']'];
    }
    else {
        result = ['[', join([',', space], printedExpressions), ']'];
    }
    if (noImplicitAny && !type_) {
        type_ = new estree_1.TSArrayType(new estree_1.TSAnyKeyword());
    }
    if (type_ && (language === 'TypeScript')) {
        result = [...result, ...[':', space, printNode(type_, options)]];
    }
    if (shouldBreak) {
        result = group(result, {
            'should-break': shouldBreak
        });
    }
    return result;
}
printArrayExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-array-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('no-implicit-any'), [Symbol.for('oget'), Symbol.for('options'), 'noImplicitAny']], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('printed-expressions'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('should-break'), Symbol.for('#f')], [Symbol.for('define'), Symbol.for('printed-exp')], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('for'), [[Symbol.for('exp'), [Symbol.for('get-field'), Symbol.for('elements'), Symbol.for('node')]]], [Symbol.for('if'), Symbol.for('exp'), [Symbol.for('set!'), Symbol.for('printed-exp'), [Symbol.for('print-node'), Symbol.for('exp'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]]], [Symbol.for('set!'), Symbol.for('printed-exp'), Symbol.for('empty')]], [Symbol.for('push-right!'), Symbol.for('printed-expressions'), [Symbol.for('doc-value-string'), Symbol.for('printed-exp')]], [Symbol.for('when'), [Symbol.for('doc-should-break?'), Symbol.for('printed-exp')], [Symbol.for('set!'), Symbol.for('should-break'), Symbol.for('#t')]]], [Symbol.for('cond'), [Symbol.for('should-break'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '[', Symbol.for('line'), [Symbol.for('~>'), Symbol.for('printed-expressions'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('align'), 1, Symbol.for('x')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('line')], Symbol.for('_')]], Symbol.for('line'), ']']]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '[', [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('printed-expressions')], ']']]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('no-implicit-any'), [Symbol.for('not'), Symbol.for('type_')]], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('new'), Symbol.for('TSArrayType'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('type_'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript']], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), [Symbol.for('list'), ':', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('type_'), Symbol.for('options')]]]]], [Symbol.for('when'), Symbol.for('should-break'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('group'), Symbol.for('result'), [Symbol.for('js-obj'), 'should-break', Symbol.for('should-break')]]]], Symbol.for('result')];
/**
 * Print an `ArrayPattern` ESTree node to a `Doc` object.
 */
function printArrayPattern(node, options = {}) {
    return printArrayExpression(node, options);
}
printArrayPattern.lispSource = [Symbol.for('define'), [Symbol.for('print-array-pattern'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-array-expression'), Symbol.for('node'), Symbol.for('options')]];
/**
 * Print a `NewExpression` ESTree node to a `Doc` object.
 */
function printNewExpression(node, options = {}) {
    return ['new', space, printNode(new estree_1.CallExpression(node.callee, node.arguments), options)];
}
printNewExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-new-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'new', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('new'), Symbol.for('CallExpression'), [Symbol.for('get-field'), Symbol.for('callee'), Symbol.for('node')], [Symbol.for('get-field'), Symbol.for('arguments'), Symbol.for('node')]], Symbol.for('options')]]];
/**
 * Print an `ImportDeclaration` ESTree node to a `Doc` object.
 */
function printImportDeclaration(node, options = {}) {
    const specifiers = node.specifiers;
    const source = node.source;
    if ((specifiers.length === 1) && !(0, estree_1.estreeTypeP)(specifiers[0], 'ImportSpecifier')) {
        return ['import', space, printNode(specifiers[0], options), space, 'from', space, printNode(source, options), ';'];
    }
    else {
        return ['import', space, '{', line, indent(join([',', line], specifiers.map(function (x) {
                return printNode(x, options);
            }))), line, '}', space, 'from', space, printNode(source, options), ';'];
    }
}
printImportDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-import-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('get-field'), Symbol.for('specifiers'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('source'), [Symbol.for('get-field'), Symbol.for('source'), Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('specifiers')], 1], [Symbol.for('not'), [Symbol.for('estree-type?'), [Symbol.for('first'), Symbol.for('specifiers')], 'ImportSpecifier']]], [Symbol.for('list'), 'import', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('first'), Symbol.for('specifiers')], Symbol.for('options')], Symbol.for('space'), 'from', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('source'), Symbol.for('options')], ';']], [Symbol.for('else'), [Symbol.for('list'), 'import', Symbol.for('space'), '{', Symbol.for('line'), [Symbol.for('~>'), Symbol.for('specifiers'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('line')], Symbol.for('_')], [Symbol.for('indent'), Symbol.for('_')]], Symbol.for('line'), '}', Symbol.for('space'), 'from', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('source'), Symbol.for('options')], ';']]]];
/**
 * Print an `ImportSpecifier` ESTree node to a `Doc` object.
 */
function printImportSpecifier(node, options = {}) {
    const local = node.local;
    const localPrinted = printDoc(printNode(local, options), options);
    const imported = node.imported;
    const importedPrinted = printDoc(printNode(imported, options), options);
    if (localPrinted === importedPrinted) {
        return localPrinted;
    }
    else {
        return [localPrinted, space, 'as', space, importedPrinted];
    }
}
printImportSpecifier.lispSource = [Symbol.for('define'), [Symbol.for('print-import-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('local'), [Symbol.for('get-field'), Symbol.for('local'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('local-printed'), [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('local'), Symbol.for('options')], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('imported'), [Symbol.for('get-field'), Symbol.for('imported'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('imported-printed'), [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('imported'), Symbol.for('options')], Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('local-printed'), Symbol.for('imported-printed')], Symbol.for('local-printed')], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('local-printed'), Symbol.for('space'), 'as', Symbol.for('space'), Symbol.for('imported-printed')]]]];
/**
 * Print an `ImportDefaultSpecifier` ESTree node to a `Doc` object.
 */
function printImportDefaultSpecifier(node, options = {}) {
    return printNode(node.local, options);
}
printImportDefaultSpecifier.lispSource = [Symbol.for('define'), [Symbol.for('print-import-default-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('local'), Symbol.for('node')], Symbol.for('options')]];
/**
 * Print an `ImportNamespaceSpecifier` ESTree node to a `Doc` object.
 */
function printImportNamespaceSpecifier(node, options = {}) {
    return ['*', space, 'as', space, printNode(node.local, options)];
}
printImportNamespaceSpecifier.lispSource = [Symbol.for('define'), [Symbol.for('print-import-namespace-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), '*', Symbol.for('space'), 'as', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('local'), Symbol.for('node')], Symbol.for('options')]]];
/**
 * Print an `ExportNamedDeclaration` ESTree node to a `Doc` object.
 */
function printExportNamedDeclaration(node, options = {}) {
    const specifiers = node.specifiers;
    const specifiersPrinted = printDoc(indent(join([',', line], specifiers.map(function (x) {
        return printNode(x, options);
    }))), options);
    return ['export', space, '{', line, specifiersPrinted, (specifiersPrinted === '') ? empty : line, '}', ';'];
}
printExportNamedDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-export-named-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('get-field'), Symbol.for('specifiers'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('specifiers-printed'), [Symbol.for('~>'), Symbol.for('specifiers'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('line')], Symbol.for('_')], [Symbol.for('indent')], [Symbol.for('print-doc'), Symbol.for('options')]]], [Symbol.for('list'), 'export', Symbol.for('space'), '{', Symbol.for('line'), Symbol.for('specifiers-printed'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('specifiers-printed'), ''], Symbol.for('empty'), Symbol.for('line')], '}', ';']];
/**
 * Print an `ExportSpecifier` ESTree node to a `Doc` object.
 */
function printExportSpecifier(node, options = {}) {
    return printImportSpecifier(new estree_1.ImportSpecifier(node.local, node.exported), options);
}
printExportSpecifier.lispSource = [Symbol.for('define'), [Symbol.for('print-export-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-import-specifier'), [Symbol.for('new'), Symbol.for('ImportSpecifier'), [Symbol.for('get-field'), Symbol.for('local'), Symbol.for('node')], [Symbol.for('get-field'), Symbol.for('exported'), Symbol.for('node')]], Symbol.for('options')]];
/**
 * Print an `ExportAllDeclaration` ESTree node to a `Doc` object.
 */
function printExportAllDeclaration(node, options = {}) {
    return ['export', space, '*', space, 'from', space, printNode(node.source, options), ';'];
}
printExportAllDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-export-all-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'export', Symbol.for('space'), '*', Symbol.for('space'), 'from', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('source'), Symbol.for('node')], Symbol.for('options')], ';']];
/**
 * Print an `ObjectExpression` ESTree node to a `Doc` object.
 */
function printObjectExpression(node, options = {}) {
    const properties = node.properties;
    return ['{', (properties.length === 0) ? empty : [line, indent(join([',', line], properties.map(function (x) {
                return printNode(x, options);
            }))), line], '}'];
}
printObjectExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-object-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('properties'), [Symbol.for('get-field'), Symbol.for('properties'), Symbol.for('node')]], [Symbol.for('list'), '{', [Symbol.for('if'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('properties')], 0], Symbol.for('empty'), [Symbol.for('list'), Symbol.for('line'), [Symbol.for('~>'), Symbol.for('properties'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('line')], Symbol.for('_')], [Symbol.for('indent')]], Symbol.for('line')]], '}']];
/**
 * Print an `ObjectPattern` ESTree node to a `Doc` object.
 */
function printObjectPattern(node, options = {}) {
    return ['{', join([',', space], node.properties.map(function (prop) {
            return printAssignmentProperty(prop, options);
        })), '}'];
}
printObjectPattern.lispSource = [Symbol.for('define'), [Symbol.for('print-object-pattern'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), '{', [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('properties'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('prop')], [Symbol.for('print-assignment-property'), Symbol.for('prop'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], '}']];
/**
 * Print an `AssignmentProperty` ESTree node to a `Doc` object.
 */
function printAssignmentProperty(node, options = {}) {
    const options1 = Object.assign(Object.assign({}, options), { noImplicitAny: false });
    const key = node.key;
    let keyPrinted = printNode(key, options1);
    const keyPrintedStr = printDoc(keyPrinted, options1);
    const value = node.value;
    const valuePrinted = printNode(value, options1);
    const valuePrintedStr = printDoc(valuePrinted, options1);
    if (keyPrintedStr === valuePrintedStr) {
        return keyPrinted;
    }
    else {
        return [keyPrinted, ':', space, valuePrinted];
    }
}
printAssignmentProperty.lispSource = [Symbol.for('define'), [Symbol.for('print-assignment-property'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('options1'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-field'), Symbol.for('key'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('key-printed'), [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options1')]], [Symbol.for('define'), Symbol.for('key-printed-str'), [Symbol.for('print-doc'), Symbol.for('key-printed'), Symbol.for('options1')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('value-printed'), [Symbol.for('print-node'), Symbol.for('value'), Symbol.for('options1')]], [Symbol.for('define'), Symbol.for('value-printed-str'), [Symbol.for('print-doc'), Symbol.for('value-printed'), Symbol.for('options1')]], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('key-printed-str'), Symbol.for('value-printed-str')], Symbol.for('key-printed'), [Symbol.for('list'), Symbol.for('key-printed'), ':', Symbol.for('space'), Symbol.for('value-printed')]]];
/**
 * Print a `Property` ESTree node to a `Doc` object.
 */
function printProperty(node, options = {}) {
    const language = options['language'];
    const key = node.key;
    let keyPrinted = printNode(key, options);
    const value = node.value;
    const valuePrinted = printNode(value, options);
    if (node.computed) {
        keyPrinted = ['[', keyPrinted, (language === 'TypeScript') ? [space, 'as any'] : empty, ']'];
    }
    return [keyPrinted, ':', space, valuePrinted];
}
printProperty.lispSource = [Symbol.for('define'), [Symbol.for('print-property'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-field'), Symbol.for('key'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('key-printed'), [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('value-printed'), [Symbol.for('print-node'), Symbol.for('value'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('get-field'), Symbol.for('computed'), Symbol.for('node')], [Symbol.for('set!'), Symbol.for('key-printed'), [Symbol.for('list'), '[', Symbol.for('key-printed'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('list'), Symbol.for('space'), 'as any'], Symbol.for('empty')], ']']]], [Symbol.for('list'), Symbol.for('key-printed'), ':', Symbol.for('space'), Symbol.for('value-printed')]];
/**
 * Print a `Program` ESTree node to a `Doc` object.
 */
function printProgram(node, options = {}) {
    return join([line, line], node.body.map(function (x) {
        return printNode(x, options);
    }));
}
printProgram.lispSource = [Symbol.for('define'), [Symbol.for('print-program'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), Symbol.for('line'), Symbol.for('line')], Symbol.for('_')]]];
/**
 * Print a `SwitchStatement` ESTree node to a `Doc` object.
 */
function printSwitchStatement(node, options = {}) {
    const discriminant = node.discriminant;
    const discriminantPrinted = printNode(discriminant, options);
    const cases = node.cases;
    const casesPrinted = indent(join(line, cases.map(function (x) {
        return printNode(x, options);
    })));
    return ['switch', space, '(', discriminantPrinted, ')', space, '{', line, casesPrinted, line, '}'];
}
printSwitchStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-switch-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('discriminant'), [Symbol.for('get-field'), Symbol.for('discriminant'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('discriminant-printed'), [Symbol.for('print-node'), Symbol.for('discriminant'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('cases'), [Symbol.for('get-field'), Symbol.for('cases'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('cases-printed'), [Symbol.for('~>'), Symbol.for('cases'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), Symbol.for('line'), Symbol.for('_')], [Symbol.for('indent'), Symbol.for('_')]]], [Symbol.for('list'), 'switch', Symbol.for('space'), '(', Symbol.for('discriminant-printed'), ')', Symbol.for('space'), '{', Symbol.for('line'), Symbol.for('cases-printed'), Symbol.for('line'), '}']];
/**
 * Print a `SwitchCase` ESTree node to a `Doc` object.
 */
function printSwitchCase(node, options = {}) {
    const test = node.test;
    let testPrinted = test ? ['case', space, printNode(test, options)] : 'default';
    const consequent = node.consequent;
    const isBlockStatement = (consequent.length === 1) && consequent[0] && (0, estree_1.estreeTypeP)(consequent[0], 'BlockStatement');
    let consequentPrinted = consequent.map(function (x) {
        return printNode(x, options);
    });
    if (isBlockStatement) {
        consequentPrinted = [space, consequentPrinted[0]];
    }
    else {
        consequentPrinted = [line, indent(join(line, consequentPrinted))];
    }
    return [testPrinted, ':', consequentPrinted];
}
printSwitchCase.lispSource = [Symbol.for('define'), [Symbol.for('print-switch-case'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('cond'), [Symbol.for('test'), [Symbol.for('list'), 'case', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]]], [Symbol.for('else'), 'default']]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('get-field'), Symbol.for('consequent'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('is-block-statement'), [Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('consequent')], 1], [Symbol.for('first'), Symbol.for('consequent')], [Symbol.for('estree-type?'), [Symbol.for('first'), Symbol.for('consequent')], 'BlockStatement']]], [Symbol.for('define'), Symbol.for('consequent-printed'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('consequent')]], [Symbol.for('cond'), [Symbol.for('is-block-statement'), [Symbol.for('set!'), Symbol.for('consequent-printed'), [Symbol.for('list'), Symbol.for('space'), [Symbol.for('first'), Symbol.for('consequent-printed')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('consequent-printed'), [Symbol.for('list'), Symbol.for('line'), [Symbol.for('~>'), Symbol.for('consequent-printed'), [Symbol.for('join'), Symbol.for('line'), Symbol.for('_')], [Symbol.for('indent'), Symbol.for('_')]]]]]], [Symbol.for('list'), Symbol.for('test-printed'), ':', Symbol.for('consequent-printed')]];
/**
 * Print a `TSAsExpression` TSESTree node to a `Doc` object.
 */
function printTsAsExpression(node, options = {}) {
    const expression = node.expression;
    const expressionPrinted = printNode(expression, options);
    const typeAnnotation = node.typeAnnotation;
    const typeAnnotationPrinted = printTsType(typeAnnotation, options);
    return [expressionPrinted, space, 'as', space, typeAnnotationPrinted];
}
printTsAsExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-as-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression'), [Symbol.for('get-field'), Symbol.for('expression'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('expression-printed'), [Symbol.for('print-node'), Symbol.for('expression'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('type-annotation'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('type-annotation-printed'), [Symbol.for('print-ts-type'), Symbol.for('type-annotation'), Symbol.for('options')]], [Symbol.for('list'), Symbol.for('expression-printed'), Symbol.for('space'), 'as', Symbol.for('space'), Symbol.for('type-annotation-printed')]];
/**
 * Print TSESTree type to a `Doc` object.
 */
function printTsType(node, options = {}) {
    let type_ = (0, estree_1.estreeType)(node);
    if (printerMap.has(type_)) {
        return printNode(node, options);
    }
    else {
        return 'any';
    }
}
printTsType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('estree-type'), Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('printer-map'), Symbol.for('type_')], [Symbol.for('print-node'), Symbol.for('node'), Symbol.for('options')]], [Symbol.for('else'), 'any']]];
/**
 * Print a `TSAnyKeyword` TSESTree node to a `Doc` object.
 */
function printTsAnyKeyword(node, options = {}) {
    return 'any';
}
printTsAnyKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-any-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'any'];
/**
 * Print a `TSVoidKeyword` TSESTree node to a `Doc` object.
 */
function printTsVoidKeyword(node, options = {}) {
    return 'void';
}
printTsVoidKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-void-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'void'];
/**
 * Print a `TSUndefinedKeyword` TSESTree node to a `Doc` object.
 */
function printTsUndefinedKeyword(node, options = {}) {
    return 'undefined';
}
printTsUndefinedKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-undefined-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'undefined'];
/**
 * Print a `TSBooleanKeyword` TSESTree node to a `Doc` object.
 */
function printTsBooleanKeyword(node, options = {}) {
    return 'boolean';
}
printTsBooleanKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-boolean-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'boolean'];
/**
 * Print a `TSNumberKeyword` TSESTree node to a `Doc` object.
 */
function printTsNumberKeyword(node, options = {}) {
    return 'number';
}
printTsNumberKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-number-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'number'];
/**
 * Print a `TSStringKeyword` TSESTree node to a `Doc` object.
 */
function printTsStringKeyword(node, options = {}) {
    return 'string';
}
printTsStringKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-string-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'string'];
/**
 * Print a `TSArrayType` TSESTree node to a `Doc` object.
 */
function printTsArrayType(node, options = {}) {
    const elementType = node.elementType;
    let result = printNode(elementType, options);
    if ((0, estree_1.estreeTypeP)(elementType, 'TSUnionType')) {
        result = docWrap(result, options);
    }
    return [result, '[]'];
}
printTsArrayType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-array-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('element-type'), [Symbol.for('get-field'), Symbol.for('elementType'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('print-node'), Symbol.for('element-type'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('element-type'), 'TSUnionType'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('doc-wrap'), Symbol.for('result'), Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('result'), '[]']];
/**
 * Print a `TSTupleType` TSESTree node to a `Doc` object.
 */
function printTsTupleType(node, options = {}) {
    const elementTypes = node.elementTypes;
    return ['[', join([',', space], elementTypes.map(function (x) {
            return printNode(x, options);
        })), ']'];
}
printTsTupleType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-tuple-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('element-types'), [Symbol.for('get-field'), Symbol.for('elementTypes'), Symbol.for('node')]], [Symbol.for('list'), '[', [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('element-types')]], ']']];
/**
 * Print a `TSUnionType` TSESTree node to a `Doc` object.
 */
function printTsUnionType(node, options = {}) {
    return join([space, '|', space], node.types.map(function (x) {
        let result = printNode(x, options);
        if ((0, estree_1.estreeTypeP)(x, 'TSUnionType')) {
            result = docWrap(result, options);
        }
        return result;
    }));
}
printTsUnionType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-union-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('types'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('x'), 'TSUnionType'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('doc-wrap'), Symbol.for('result'), Symbol.for('options')]]], Symbol.for('result')], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), Symbol.for('space'), '|', Symbol.for('space')], Symbol.for('_')]]];
/**
 * Print a `TSFunctionType` TSESTree node to a `Doc` object.
 */
function printTsFunctionType(node, options = {}) {
    return ['(', join([',', space], node.params.map(function (x) {
            return printNode(x, options);
        })), ')', space, '=>', space, printNode(node.returnType, options)];
}
printTsFunctionType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-function-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), '(', [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], ')', Symbol.for('space'), '=>', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('node')], Symbol.for('options')]]];
/**
 * Print a `TSTypeAliasDeclaration` TSESTree node to a `Doc` object.
 */
function printTsTypeAliasDeclaration(node, options = {}) {
    return ['type', space, printNode(node.id, options), space, '=', space, printNode(node.typeAnnotation, options), ';'];
}
printTsTypeAliasDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-type-alias-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'type', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')], Symbol.for('options')], Symbol.for('space'), '=', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('node')], Symbol.for('options')], ';']];
/**
 * Print a `TSTypeAnnotation` TSESTree node to a `Doc` object.
 */
function printTsTypeAnnotation(node, options = {}) {
    return printNode(node.typeAnnotation, options);
}
printTsTypeAnnotation.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-type-annotation'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('node')], Symbol.for('options')]];
/**
 * Print a `TSLiteralType` TSESTree node to a `Doc` object.
 */
function printTsLiteralType(node, options = {}) {
    return printNode(node.literal, Object.assign(Object.assign({}, options), { noImplicitAny: false }));
}
printTsLiteralType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-literal-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('literal'), Symbol.for('node')], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]]];
/**
 * Print a `TSTypeReference` TSESTree node to a `Doc` object.
 */
function printTsTypeReference(node, options = {}) {
    const name = node.typeName;
    const params = node.typeParameters;
    return [printNode(name, Object.assign(Object.assign({}, options), { noImplicitAny: false })), params ? printNode(params, Object.assign(Object.assign({}, options), { noImplicitAny: false })) : empty];
}
printTsTypeReference.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-type-reference'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('get-field'), Symbol.for('typeName'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('get-field'), Symbol.for('typeParameters'), Symbol.for('node')]], [Symbol.for('list'), [Symbol.for('print-node'), Symbol.for('name'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]], [Symbol.for('if'), Symbol.for('params'), [Symbol.for('print-node'), Symbol.for('params'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]], Symbol.for('empty')]]];
/**
 * Print a `TSTypeParameterInstantiation` TSESTree node to a `Doc` object.
 */
function printTsTypeParameterInstantiation(node, options = {}) {
    const params = node.params;
    return ['<', join(',', params.map(function (x) {
            return printTsType(x, Object.assign(Object.assign({}, options), { noImplicitAny: false }));
        })), '>'];
}
printTsTypeParameterInstantiation.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-type-parameter-instantiation'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('node')]], [Symbol.for('list'), '<', [Symbol.for('~>'), Symbol.for('params'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-ts-type'), Symbol.for('x'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]]], Symbol.for('_')], [Symbol.for('join'), ',', Symbol.for('_')]], '>']];
/**
 * Print an `XRawJavaScript` ESTree extension node to a `Doc` object.
 */
function printXRawJavascript(node, options = {}) {
    let str = node.js;
    if (str.match(new RegExp('^function \\('))) {
        str = docWrap(str);
    }
    return str;
}
printXRawJavascript.lispSource = [Symbol.for('define'), [Symbol.for('print-x-raw-javascript'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('get-field'), Symbol.for('js'), Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^function \\('], Symbol.for('str')], [Symbol.for('set!'), Symbol.for('str'), [Symbol.for('doc-wrap'), Symbol.for('str')]]], Symbol.for('str')];
/**
 * Default printer.
 *
 * Returns the empty string.
 */
function defaultPrinter(node, options = {}) {
    return empty;
}
defaultPrinter.lispSource = [Symbol.for('define'), [Symbol.for('default-printer'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], Symbol.for('empty')];
/**
 * Mapping from node types to printer functions.
 */
const printerMap = new Map([['ArrayExpression', printArrayExpression], ['ArrayPattern', printArrayPattern], ['ArrowFunctionExpression', printArrowFunctionExpression], ['AssignmentExpression', printAssignmentExpression], ['AssignmentPattern', printAssignmentPattern], ['AwaitExpression', printAwaitExpression], ['BinaryExpression', printBinaryExpression], ['BlockStatement', printBlockStatement], ['BreakStatement', printBreakStatement], ['CallExpression', printCallExpression], ['ClassBody', printClassBody], ['ClassDeclaration', printClassDeclaration], ['ClassExpression', printClassExpression], ['ConditionalExpression', printConditionalExpression], ['ContinueStatement', printContinueStatement], ['DoWhileStatement', printDoWhileStatement], ['ExportAllDeclaration', printExportAllDeclaration], ['ExportNamedDeclaration', printExportNamedDeclaration], ['ExportSpecifier', printExportSpecifier], ['ExpressionStatement', printExpressionStatement], ['ForInStatement', printForInStatement], ['ForOfStatement', printForOfStatement], ['ForStatement', printForStatement], ['FunctionDeclaration', printFunctionDeclaration], ['FunctionExpression', printFunctionExpression], ['Identifier', printIdentifier], ['IfStatement', printIfStatement], ['ImportDeclaration', printImportDeclaration], ['ImportDefaultSpecifier', printImportDefaultSpecifier], ['ImportNamespaceSpecifier', printImportNamespaceSpecifier], ['ImportSpecifier', printImportSpecifier], ['Literal', printLiteral], ['LogicalExpression', printLogicalExpression], ['MemberExpression', printMemberExpression], ['MethodDefinition', printMethodDefinition], ['NewExpression', printNewExpression], ['ObjectExpression', printObjectExpression], ['ObjectPattern', printObjectPattern], ['Program', printProgram], ['Property', printProperty], ['PropertyDefinition', printPropertyDefinition], ['RestElement', printRestElement], ['ReturnStatement', printReturnStatement], ['SequenceExpression', printSequenceExpression], ['SpreadElement', printSpreadElement], ['SwitchStatement', printSwitchStatement], ['SwitchCase', printSwitchCase], ['TSAnyKeyword', printTsAnyKeyword], ['TSArrayType', printTsArrayType], ['TSAsExpression', printTsAsExpression], ['TSBooleanKeyword', printTsBooleanKeyword], ['TSFunctionType', printTsFunctionType], ['TSLiteralType', printTsLiteralType], ['TSNumberKeyword', printTsNumberKeyword], ['TSStringKeyword', printTsStringKeyword], ['TSTupleType', printTsTupleType], ['TSTypeAliasDeclaration', printTsTypeAliasDeclaration], ['TSTypeAnnotation', printTsTypeAnnotation], ['TSTypeParameterInstantiation', printTsTypeParameterInstantiation], ['TSTypeReference', printTsTypeReference], ['TSUndefinedKeyword', printTsUndefinedKeyword], ['TSUnionType', printTsUnionType], ['TSVoidKeyword', printTsVoidKeyword], ['TaggedTemplateExpression', printTaggedTemplateExpression], ['TemplateElement', printTemplateElement], ['TemplateLiteral', printTemplateLiteral], ['ThisExpression', printThisExpression], ['ThrowStatement', printThrowStatement], ['TryStatement', printTryStatement], ['UnaryExpression', printUnaryExpression], ['UpdateExpression', printUpdateExpression], ['VariableDeclaration', printVariableDeclaration], ['VariableDeclarator', printVariableDeclarator], ['WhileStatement', printWhileStatement], ['YieldExpression', printYieldExpression], ['XRawJavaScript', printXRawJavascript]]);
