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

import {
  BlockComment,
  CallExpression,
  ClassDeclaration,
  Comment,
  ImportSpecifier,
  LeadingComment,
  Node,
  TSAnyKeyword,
  TSArrayType,
  TSUnionType,
  TrailingComment,
  VariableDeclarator,
  estreep,
  estreeTypeP,
  estreeType
} from './estree';

import {
  rosep
} from './rose';

import {
  makeVisitor,
  visit
} from './visitor';

const [stringLength, length, findf, symbolp, booleanp, stringp, procedurep, arrayp, take, lastCdr]: any[] = ((): any => {
  function length_(lst: any): any {
    if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
      return ((): any => {
        function linkedListLength_(lst: any): any {
          let len: any = 0;
          let current: any = lst;
          while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.'))) {
            len = len + (lst.length - 2);
            current = current[current.length - 1];
          }
          return len;
        }
        return linkedListLength_;
      })()(lst);
    } else {
      return lst.length;
    }
  }
  function findf_(proc: any, lst: any, notFound: any = false): any {
    const idx: any = lst.findIndex(proc);
    if (idx >= 0) {
      return (lst as any)[idx];
    } else {
      return notFound;
    }
  }
  function symbolp_(obj: any): any {
    return typeof obj === 'symbol';
  }
  function booleanp_(obj: any): any {
    return typeof obj === 'boolean';
  }
  function stringp_(obj: any): any {
    return (typeof obj === 'string') || (obj instanceof String);
  }
  function procedurep_(obj: any): any {
    return obj instanceof Function;
  }
  function arrayp_(obj: any): any {
    return Array.isArray(obj);
  }
  function take_(lst: any, n: any): any {
    const n1: any = lst.length - n;
    if (n1 === 0) {
      return lst;
    } else {
      return lst.slice(0, -n1);
    }
  }
  function lastCdr_(lst: any): any {
    if (!Array.isArray(lst)) {
      return undefined;
    } else if (Array.isArray(lst) && (lst.length >= 3) && (lst[lst.length - 2] === Symbol.for('.'))) {
      let result: any = lst;
      while (Array.isArray(result) && (result.length >= 3) && (result[result.length - 2] === Symbol.for('.'))) {
        result = result[result.length - 1];
      }
      return result;
    } else {
      return [];
    }
  }
  function linkedListLength_(lst: any): any {
    let len: any = 0;
    let current: any = lst;
    while (Array.isArray(current) && (current.length >= 3) && (current[current.length - 2] === Symbol.for('.'))) {
      len = len + (lst.length - 2);
      current = current[current.length - 1];
    }
    return len;
  }
  return [length_, length_, findf_, symbolp_, booleanp_, stringp_, procedurep_, arrayp_, take_, lastCdr_];
})();

/**
 * `Doc` type.
 *
 * A `Doc` is a string, a `DocCommand`, or a list of `Doc`s.
 */
type Doc = string | Doc[] | DocCommand;

/**
 * `DocCommand` class.
 *
 * Used for implementing more complicated constructs,
 * such as `indent` and `group`.
 */
class DocCommand {
  type: any;

  args: any;

  constructor(type: any, ...args: any[]) {
    this.type = type;
    this.args = args;
  }
}

/**
 * Empty string.
 */
const empty: any = '';

/**
 * Space.
 */
const space: any = ' ';

/**
 * Newline.
 */
const line: any = '\n';

/**
 * `Doc` command `literalline`.
 */
const literalline: any = new DocCommand('literalline');

/**
 * `Doc` command `align`.
 */
function align(offset: any, doc: any, options: any = {}): any {
  return new DocCommand('align', offset, doc, options);
}

align.lispSource = [Symbol.for('define'), [Symbol.for('align'), Symbol.for('offset'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'align', Symbol.for('offset'), Symbol.for('doc'), Symbol.for('options')]];

/**
 * `Doc` command `indent`.
 */
function indent(doc: any, options: any = {}): any {
  return new DocCommand('indent', doc, options);
}

indent.lispSource = [Symbol.for('define'), [Symbol.for('indent'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'indent', Symbol.for('doc'), Symbol.for('options')]];

/**
 * `Doc` command `noindent`.
 */
function noindent(doc: any, options: any = {}): any {
  return new DocCommand('noindent', doc, options);
}

noindent.lispSource = [Symbol.for('define'), [Symbol.for('noindent'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'noindent', Symbol.for('doc'), Symbol.for('options')]];

/**
 * `Doc` command `join`.
 *
 * Join a list of documents with a separator.
 */
function join(sep: any, docs: any): any {
  let result: any = [];
  const _end: any = docs.length;
  for (let i: any = 0; i < _end; i++) {
    if (i !== 0) {
      result.push(sep);
    }
    result.push((docs as any)[i]);
  }
  return result;
}

join.lispSource = [Symbol.for('define'), [Symbol.for('join'), Symbol.for('sep'), Symbol.for('docs')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('docs')]]]], [Symbol.for('unless'), [Symbol.for('='), Symbol.for('i'), 0], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('sep')]], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('aget'), Symbol.for('docs'), Symbol.for('i')]]], Symbol.for('result')];

/**
 * `Doc` command `group`.
 *
 * Makes a document group.
 */
function group(doc: any, options: any = {}): any {
  return new DocCommand('group', doc, options);
}

group.lispSource = [Symbol.for('define'), [Symbol.for('group'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'group', Symbol.for('doc'), Symbol.for('options')]];

/**
 * Get the type of a `Doc` object.
 */
function docType(doc: any): any {
  if (typeof doc === 'string') {
    return 'string';
  } else if (Array.isArray(doc)) {
    return 'array';
  } else if (doc instanceof DocCommand) {
    return doc.type;
  } else {
    return 'undefined';
  }
}

docType.lispSource = [Symbol.for('define'), [Symbol.for('doc-type'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('string?'), Symbol.for('doc')], 'string'], [[Symbol.for('array?'), Symbol.for('doc')], 'array'], [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('get-field'), Symbol.for('type'), Symbol.for('doc')]], [Symbol.for('else'), 'undefined']]];

/**
 * Unwrap a `Doc` command.
 */
function docValue(doc: any): any {
  if (doc instanceof DocCommand) {
    return doc.args[0];
  } else {
    return doc;
  }
}

docValue.lispSource = [Symbol.for('define'), [Symbol.for('doc-value'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]]], [Symbol.for('else'), Symbol.for('doc')]]];

/**
 * Unwrap a `Doc` command and print it to a string.
 */
function docValueString(doc: any): any {
  return printDoc(docValue(doc));
}

docValueString.lispSource = [Symbol.for('define'), [Symbol.for('doc-value-string'), Symbol.for('doc')], [Symbol.for('print-doc'), [Symbol.for('doc-value'), Symbol.for('doc')]]];

/**
 * Whether a `Doc` object should break across multiple lines.
 */
function docShouldBreakP(doc: any): any {
  if (doc instanceof DocCommand) {
    return ((): any => {
      const arr: any = doc.args;
      return arr[arr.length - 1];
    })()['should-break'];
  } else {
    return false;
  }
}

docShouldBreakP.lispSource = [Symbol.for('define'), [Symbol.for('doc-should-break?'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('oget'), [Symbol.for('array-list-last'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], 'should-break']], [Symbol.for('else'), Symbol.for('#f')]]];

/**
 * Whether a `Doc` object contains any comments.
 */
function docHasCommentsP(doc: any): any {
  if (doc instanceof DocCommand) {
    return ((): any => {
      const arr: any = doc.args;
      return arr[arr.length - 1];
    })()['has-comments'];
  } else {
    return false;
  }
}

docHasCommentsP.lispSource = [Symbol.for('define'), [Symbol.for('doc-has-comments?'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('oget'), [Symbol.for('array-list-last'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], 'has-comments']], [Symbol.for('else'), Symbol.for('#f')]]];

/**
 * Wrap a `Doc` object in a pair of parentheses.
 */
function docWrap(doc: any, options: any = {}, settings: any = {}): any {
  const open: any = settings['open'] || '(';
  const close: any = settings['close'] || ')';
  const offset: any = stringLength(open);
  if (options['has-comments'] || docHasCommentsP(doc)) {
    return printDoc([open, line, align(offset, doc), line, close], options);
  } else {
    return printDoc([open, doc, close]);
  }
}

docWrap.lispSource = [Symbol.for('define'), [Symbol.for('doc-wrap'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('open'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('settings'), 'open'], '(']], [Symbol.for('define'), Symbol.for('close'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('settings'), 'close'], ')']], [Symbol.for('define'), Symbol.for('offset'), [Symbol.for('string-length'), Symbol.for('open')]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'has-comments'], [Symbol.for('doc-has-comments?'), Symbol.for('doc')]], [Symbol.for('print-doc'), [Symbol.for('list'), Symbol.for('open'), Symbol.for('line'), [Symbol.for('align'), Symbol.for('offset'), Symbol.for('doc')], Symbol.for('line'), Symbol.for('close')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('print-doc'), [Symbol.for('list'), Symbol.for('open'), Symbol.for('doc'), Symbol.for('close')]]]]];

/**
 * Print comments of an ESTree node and attach them
 * to a `Doc` object.
 */
function attachComments(result: any, node: any, options: any = {}): any {
  const commentsOption: any = options['comments'];
  const comments: any = node.comments;
  const code: any = docValueString(result);
  let leadingComments: any = '';
  let trailingComments: any = '';
  if ((commentsOption === false) || !comments || (comments.length === 0)) {
    return result;
  }
  const _end: any = length(comments);
  for (let i: any = 0; i < _end; i++) {
    const comment: any = (comments as any)[i];
    if (comment instanceof BlockComment) {
      let blockComment: any = makeBlockComment(comment.originalText);
      if ((i === (comments.length - 1)) && (code === '')) {
        blockComment = blockComment.replace(new RegExp('\\n*$'), '');
      }
      leadingComments = leadingComments + blockComment + (((code === '') || blockComment.match(new RegExp('\\n*$'))) ? empty : line);
    } else if (comment instanceof LeadingComment) {
      let leadingComment: any = makeLineComment(comment.originalText);
      if ((i === (comments.length - 1)) && (code === '')) {
        leadingComment = leadingComment.replace(new RegExp('\\n$'), '');
      }
      leadingComments = leadingComments + leadingComment + (((code === '') || leadingComment.match(new RegExp('\\n$'))) ? empty : line);
    } else if (comment instanceof TrailingComment) {
      const trailingComment: any = makeLineComment(comment.originalText);
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
function makeLineComment(text: any): any {
  const [, content, trailingNewlines]: any[] = text.match(new RegExp('^([\\s\\S]*?)([\\n]*)$'));
  return content.split('\n').map(function (x: any): any {
    return x.replace(new RegExp('^'), (x === '') ? '//' : '// ');
  }).join('\n') + trailingNewlines;
}

makeLineComment.lispSource = [Symbol.for('define'), [Symbol.for('make-line-comment'), Symbol.for('text')], [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('content'), Symbol.for('trailing-newlines')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^([\\s\\S]*?)([\\n]*)$'], Symbol.for('text')]], [Symbol.for('string-append'), [Symbol.for('~>'), Symbol.for('content'), [Symbol.for('string-split'), '\n'], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^'], Symbol.for('x'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('x'), ''], '//', '// ']]], Symbol.for('_')], [Symbol.for('string-join'), '\n']], Symbol.for('trailing-newlines')]];

/**
 * Make a block comment.
 */
function makeBlockComment(text: any): any {
  const [, content, trailingNewlines]: any[] = text.match(new RegExp('^([\\s\\S]*?)([\\n]*)$'));
  return '/**' + line + content.split('\n').map(function (x: any): any {
    return x.replace(new RegExp('^'), (x === '') ? ' *' : ' * ');
  }).join('\n') + line + ' */' + trailingNewlines;
}

makeBlockComment.lispSource = [Symbol.for('define'), [Symbol.for('make-block-comment'), Symbol.for('text')], [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('content'), Symbol.for('trailing-newlines')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^([\\s\\S]*?)([\\n]*)$'], Symbol.for('text')]], [Symbol.for('string-append'), '/**', Symbol.for('line'), [Symbol.for('~>'), Symbol.for('content'), [Symbol.for('string-split'), '\n'], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^'], Symbol.for('x'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('x'), ''], ' *', ' * ']]], Symbol.for('_')], [Symbol.for('string-join'), '\n']], Symbol.for('line'), ' */', Symbol.for('trailing-newlines')]];

/**
 * Whether an expression is "simple", i.e., does not
 * need to be wrapped in parentheses when printed.
 */
function estreeSimpleP(exp: any): any {
  return ['Literal', 'Identifier', 'ThisExpression', 'CallExpression', 'NewExpression', 'UnaryExpression', 'ArrayExpression', 'ObjectExpression', 'MemberExpression'].includes(estreeType(exp));
}

estreeSimpleP.lispSource = [Symbol.for('define'), [Symbol.for('estree-simple?'), Symbol.for('exp')], [Symbol.for('memq?'), [Symbol.for('estree-type'), Symbol.for('exp')], [Symbol.for('quote'), ['Literal', 'Identifier', 'ThisExpression', 'CallExpression', 'NewExpression', 'UnaryExpression', 'ArrayExpression', 'ObjectExpression', 'MemberExpression']]]];

/**
 * Whether an expression is "complex", i.e., needs
 * to be wrapped in parentheses when printed.
 */
function estreeComplexP(exp: any): any {
  return ['FunctionExpression', 'ArrowFunctionExpression', 'FunctionDeclaration', 'TSAsExpression'].includes(estreeType(exp));
}

estreeComplexP.lispSource = [Symbol.for('define'), [Symbol.for('estree-complex?'), Symbol.for('exp')], [Symbol.for('memq?'), [Symbol.for('estree-type'), Symbol.for('exp')], [Symbol.for('quote'), ['FunctionExpression', 'ArrowFunctionExpression', 'FunctionDeclaration', 'TSAsExpression']]]];

/**
 * Whether an expression is a string literal.
 */
function estreeStringLiteralP(exp: any): any {
  return estreeTypeP(exp, 'Literal') && (typeof exp.value === 'string');
}

estreeStringLiteralP.lispSource = [Symbol.for('define'), [Symbol.for('estree-string-literal?'), Symbol.for('exp')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('exp'), 'Literal'], [Symbol.for('string?'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('exp')]]]];

/**
 * Whether an ESTree node has any comments.
 */
function estreeHasCommentsP(node: any): any {
  return node.comments.length > 0;
}

estreeHasCommentsP.lispSource = [Symbol.for('define'), [Symbol.for('estree-has-comments?'), Symbol.for('node')], [Symbol.for('>'), [Symbol.for('array-list-length'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')]], 0]];

/**
 * Whether an ESTree node has any block comments.
 */
function estreeHasBlockCommentP(node: any): any {
  return findf(function (comment: any): any {
    return comment instanceof BlockComment;
  }, node.comments);
}

estreeHasBlockCommentP.lispSource = [Symbol.for('define'), [Symbol.for('estree-has-block-comment?'), Symbol.for('node')], [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('comment')], [Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('BlockComment')]], [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')]]];

/**
 * Whether an ESTree node has any leading comments.
 */
function estreeHasLeadingCommentP(node: any): any {
  return findf(function (comment: any): any {
    return comment instanceof LeadingComment;
  }, node.comments);
}

estreeHasLeadingCommentP.lispSource = [Symbol.for('define'), [Symbol.for('estree-has-leading-comment?'), Symbol.for('node')], [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('comment')], [Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('LeadingComment')]], [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')]]];

/**
 * Whether an ESTree node has any trailing comments.
 */
function estreeHasTrailingCommentP(node: any): any {
  return findf(function (comment: any): any {
    return comment instanceof TrailingComment;
  }, node.comments);
}

estreeHasTrailingCommentP.lispSource = [Symbol.for('define'), [Symbol.for('estree-has-trailing-comment?'), Symbol.for('node')], [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('comment')], [Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('TrailingComment')]], [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')]]];

/**
 * Print an ESTree node or an S-expression.
 */
function print(obj: any, options: any = {}): any {
  if (estreep(obj)) {
    return printEstree(obj, options);
  } else {
    return printSexp(obj, options);
  }
}

print.lispSource = [Symbol.for('define'), [Symbol.for('print'), Symbol.for('obj'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('cond'), [[Symbol.for('estree?'), Symbol.for('obj')], [Symbol.for('print-estree'), Symbol.for('obj'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('print-sexp'), Symbol.for('obj'), Symbol.for('options')]]]];

/**
 * Print an ESTree node.
 */
function printEstree(node: any, options: any = {}): any {
  return printToString(node, options);
}

printEstree.lispSource = [Symbol.for('define'), [Symbol.for('print-estree'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-to-string'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print a rose tree.
 */
function printRose(node: any, options: any = {}): any {
  return printSexp(node.getValue(), options);
}

printRose.lispSource = [Symbol.for('define'), [Symbol.for('print-rose'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-sexp'), [Symbol.for('send'), Symbol.for('node'), Symbol.for('get-value')], Symbol.for('options')]];

/**
 * Print an S-expression.
 */
function printSexp(exp: any, options: any = {}): any {
  return writeToString(exp, options);
}

printSexp.lispSource = [Symbol.for('define'), [Symbol.for('print-sexp'), Symbol.for('exp'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('write-to-string'), Symbol.for('exp'), Symbol.for('options')]];

/**
 * Print an S-expression as an expression
 * that can be evaluated.
 */
function printSexpAsExpression(exp: any, options: any = {}): any {
  return printSexp(exp, {
    ...options,
    quoteToplevel: true
  });
}

printSexpAsExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-sexp-as-expression'), Symbol.for('exp'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-sexp'), Symbol.for('exp'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'quoteToplevel', Symbol.for('#t')]]]];

/**
 * Print an S-expression to a string.
 */
function writeToString(obj: any, options: any = {}): any {
  let result: any = writeToDoc(obj, options);
  if (!options['doc']) {
    result = printDoc(result, options);
  }
  return result;
}

writeToString.lispSource = [Symbol.for('define'), [Symbol.for('write-to-string'), Symbol.for('obj'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('write-to-doc'), Symbol.for('obj'), Symbol.for('options')]], [Symbol.for('unless'), [Symbol.for('oget'), Symbol.for('options'), 'doc'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('print-doc'), Symbol.for('result'), Symbol.for('options')]]], Symbol.for('result')];

/**
 * Print an S-expression to a `Doc` object.
 */
function writeToDoc(obj: any, options: any = {}): any {
  const docOption: any = options['doc'];
  const prettyOption: any = options['pretty'];
  const quoteToplevelOption: any = options['quoteToplevel'];
  const visitor: any = makeVisitor([[rosep, function (obj: any): any {
    return writeToDoc(obj.getValue(), options);
  }], [symbolp, function (obj: any): any {
    return [quoteToplevelOption ? '\'' : empty, (obj === Symbol.for('.')) ? '.' : (obj.description as string)];
  }], [booleanp, function (obj: any): any {
    if (obj) {
      return '#t';
    } else {
      return '#f';
    }
  }], [stringp, function (obj: any): any {
    return ['"', join(literalline, obj.replace(new RegExp('\\\\', 'g'), '\\\\').replace(new RegExp('"', 'g'), '\\"').split(line)), '"'];
  }], [procedurep, function (obj: any): any {
    return '#<procedure>';
  }], [arrayp, function (obj: any): any {
    const op: any = obj[0];
    const spec: any = prettyOption && prettyPrintMap.get(op);
    let result: any = (spec instanceof Function) ? spec(obj, options) : (Number.isFinite(spec) ? prettyPrintWithOffset(spec, obj, options) : prettyPrintForm(obj, options));
    if (quoteToplevelOption) {
      result = ['\'', result];
    }
    return result;
  }], [function (...args: any[]): any {
    return true;
  }, function (obj: any): any {
    return obj + '';
  }]]);
  let result: any = visit(visitor, obj);
  return result;
}

writeToDoc.lispSource = [Symbol.for('define'), [Symbol.for('write-to-doc'), Symbol.for('obj'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('doc-option'), [Symbol.for('oget'), Symbol.for('options'), 'doc']], [Symbol.for('define'), Symbol.for('pretty-option'), [Symbol.for('oget'), Symbol.for('options'), 'pretty']], [Symbol.for('define'), Symbol.for('quote-toplevel-option'), [Symbol.for('oget'), Symbol.for('options'), 'quoteToplevel']], [Symbol.for('define'), Symbol.for('visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('rose?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('write-to-doc'), [Symbol.for('send'), Symbol.for('obj'), Symbol.for('get-value')], Symbol.for('options')]]]], [[Symbol.for('unquote'), Symbol.for('symbol?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('list'), [Symbol.for('if'), Symbol.for('quote-toplevel-option'), '\'', Symbol.for('empty')], [Symbol.for('if'), [Symbol.for('cons-dot?'), Symbol.for('obj')], '.', [Symbol.for('symbol->string'), Symbol.for('obj')]]]]]], [[Symbol.for('unquote'), Symbol.for('boolean?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('if'), Symbol.for('obj'), '#t', '#f']]]], [[Symbol.for('unquote'), Symbol.for('string?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('list'), '"', [Symbol.for('~>'), Symbol.for('obj'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\\\', 'g'], Symbol.for('_'), '\\\\'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '"', 'g'], Symbol.for('_'), '\\"'], [Symbol.for('string-split'), Symbol.for('line')], [Symbol.for('join'), Symbol.for('literalline'), Symbol.for('_')]], '"']]]], [[Symbol.for('unquote'), Symbol.for('procedure?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], '#<procedure>']]], [[Symbol.for('unquote'), Symbol.for('array?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('obj')]], [Symbol.for('define'), Symbol.for('spec'), [Symbol.for('and'), Symbol.for('pretty-option'), [Symbol.for('send'), Symbol.for('pretty-print-map'), Symbol.for('get'), Symbol.for('op')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('cond'), [[Symbol.for('procedure?'), Symbol.for('spec')], [Symbol.for('spec'), Symbol.for('obj'), Symbol.for('options')]], [[Symbol.for('number?'), Symbol.for('spec')], [Symbol.for('pretty-print-with-offset'), Symbol.for('spec'), Symbol.for('obj'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('pretty-print-form'), Symbol.for('obj'), Symbol.for('options')]]]], [Symbol.for('when'), Symbol.for('quote-toplevel-option'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '\'', Symbol.for('result')]]], Symbol.for('result')]]], [[Symbol.for('unquote'), [Symbol.for('const'), Symbol.for('#t')]], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('string-append'), Symbol.for('obj'), '']]]]]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('obj')]], Symbol.for('result')];

/**
 * Pretty-print a list expression.
 *
 * Helper function for `write-to-doc`.
 */
function prettyPrintForm(form: any, options: any): any {
  return ['(', join(' ', form.map(function (x: any): any {
    return writeToDoc(x, {
      ...options,
      quoteToplevel: false
    });
  })), ')'];
}

prettyPrintForm.lispSource = [Symbol.for('define'), [Symbol.for('pretty-print-form'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('list'), '(', [Symbol.for('join'), ' ', [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'quoteToplevel', Symbol.for('#f')]]]], Symbol.for('form')]], ')']];

/**
 * Pretty-print a list expression with an indentation offset.
 *
 * Helper function for `write-to-doc`.
 */
function prettyPrintWithOffset(offset: any, form: any, options: any): any {
  const prettyOption: any = options['pretty'];
  if (!prettyOption) {
    return writeToString(form, options);
  }
  if (!Array.isArray(form)) {
    return writeToString(form, options);
  }
  const op: any = form[0];
  const elements: any = form.map(function (x: any): any {
    return writeToDoc(x, {
      ...options,
      quoteToplevel: false
    });
  });
  const elements1: any = take(elements, offset + 1);
  const elements2: any = ((): any => {
    const n: any = offset + 1;
    if (n === 0) {
      return elements;
    } else {
      return elements.slice(n);
    }
  })();
  let result: any = [join(space, elements1), (elements2.length > 0) ? [line, indent(join(line, elements2))] : empty];
  result = ['(', result, ')'];
  return result;
}

prettyPrintWithOffset.lispSource = [Symbol.for('define'), [Symbol.for('pretty-print-with-offset'), Symbol.for('offset'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('pretty-option'), [Symbol.for('oget'), Symbol.for('options'), 'pretty']], [Symbol.for('unless'), Symbol.for('pretty-option'), [Symbol.for('return'), [Symbol.for('write-to-string'), Symbol.for('form'), Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('form')], [Symbol.for('return'), [Symbol.for('write-to-string'), Symbol.for('form'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('form')]], [Symbol.for('define'), Symbol.for('elements'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'quoteToplevel', Symbol.for('#f')]]]], Symbol.for('form')]], [Symbol.for('define'), Symbol.for('elements1'), [Symbol.for('take'), Symbol.for('elements'), [Symbol.for('+'), Symbol.for('offset'), 1]]], [Symbol.for('define'), Symbol.for('elements2'), [Symbol.for('drop'), Symbol.for('elements'), [Symbol.for('+'), Symbol.for('offset'), 1]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('list'), [Symbol.for('join'), Symbol.for('space'), Symbol.for('elements1')], [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('elements2')], 0], [Symbol.for('list'), Symbol.for('line'), [Symbol.for('indent'), [Symbol.for('join'), Symbol.for('line'), Symbol.for('elements2')]]], Symbol.for('empty')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '(', Symbol.for('result'), ')']], Symbol.for('result')];

/**
 * Pretty-print a `cond` expression.
 */
function prettyPrintCond(form: any, options: any): any {
  return ['(', writeToDoc(form[0], options), line, align(1, join(line, form.slice(1).map(function (x: any): any {
    return ['(', writeToDoc(x[0], options), line, align(1, join(line, x.slice(1).map(function (x1: any): any {
      return writeToDoc(x1, {
        ...options,
        quoteToplevel: false
      });
    }))), ')'];
  }))), ')'];
}

prettyPrintCond.lispSource = [Symbol.for('define'), [Symbol.for('pretty-print-cond'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('list'), '(', [Symbol.for('write-to-doc'), [Symbol.for('first'), Symbol.for('form')], Symbol.for('options')], Symbol.for('line'), [Symbol.for('align'), 1, [Symbol.for('join'), Symbol.for('line'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('list'), '(', [Symbol.for('write-to-doc'), [Symbol.for('first'), Symbol.for('x')], Symbol.for('options')], Symbol.for('line'), [Symbol.for('align'), 1, [Symbol.for('join'), Symbol.for('line'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x1')], [Symbol.for('write-to-doc'), Symbol.for('x1'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'quoteToplevel', Symbol.for('#f')]]]], [Symbol.for('rest'), Symbol.for('x')]]]], ')']], [Symbol.for('rest'), Symbol.for('form')]]]], ')']];

/**
 * Pretty-print an `if` expression.
 */
function prettyPrintIf(form: any, options: any): any {
  return ['(', join(' ', take(form, 2).map(function (x: any): any {
    return writeToDoc(x, options);
  })), line, align(4, join(line, form.slice(2).map(function (x: any): any {
    return writeToDoc(x, options);
  }))), ')'];
}

prettyPrintIf.lispSource = [Symbol.for('define'), [Symbol.for('pretty-print-if'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('list'), '(', [Symbol.for('join'), ' ', [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('take'), Symbol.for('form'), 2]]], Symbol.for('line'), [Symbol.for('align'), 4, [Symbol.for('join'), Symbol.for('line'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('drop'), Symbol.for('form'), 2]]]], ')']];

/**
 * Pretty-print a `module` expression.
 */
function prettyPrintModule(form: any, options: any): any {
  const noModuleFormOption: any = options['noModuleForm'];
  if (noModuleFormOption) {
    return join([line, line], form.slice(3).map(function (x: any): any {
      return writeToDoc(x, options);
    }));
  } else {
    return ['(', join(' ', take(form, 3).map(function (x: any): any {
      return writeToDoc(x, options);
    })), line, indent(join([line, line], form.slice(3).map(function (x: any): any {
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
const prettyPrintMap: any = new Map([[Symbol.for('as~>'), 2], [Symbol.for('begin'), 0], [Symbol.for('catch'), 2], [Symbol.for('class'), 1], [Symbol.for('cond'), prettyPrintCond], [Symbol.for('define'), 1], [Symbol.for('define/public'), 1], [Symbol.for('do'), 1], [Symbol.for('finally'), 0], [Symbol.for('fn'), 1], [Symbol.for('for'), 1], [Symbol.for('if'), prettyPrintIf], [Symbol.for('js/arrow'), 1], [Symbol.for('js/function'), 1], [Symbol.for('js/while'), 1], [Symbol.for('lambda'), 1], [Symbol.for('let*-values'), 1], [Symbol.for('let-values'), 1], [Symbol.for('module'), prettyPrintModule], [Symbol.for('provide'), 0], [Symbol.for('try'), 0], [Symbol.for('unless'), 1], [Symbol.for('when'), 1], [Symbol.for('while'), 1]] as any);

/**
 * Print a `Doc` object to a string.
 */
function printDoc(doc: any, options: any = {}): any {
  return printDocListToString(printDocToDocList(doc, options), options);
}

printDoc.lispSource = [Symbol.for('define'), [Symbol.for('print-doc'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('~>'), Symbol.for('doc'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('options')], [Symbol.for('print-doc-list-to-string'), Symbol.for('options')]]];

/**
 * Print a `Doc` object to a `Doc` list.
 */
function printDocToDocList(doc: any, options: any = {}): any {
  const dtype: any = docType(doc);
  if (dtype === 'string') {
    return join(line, doc.split(line));
  } else if (dtype === 'array') {
    let result: any = [];
    for (let x of doc) {
      const xResult: any = printDocToDocList(x, options);
      if (Array.isArray(xResult)) {
        result = [...result, ...xResult];
      } else {
        result.push(xResult);
      }
    }
    return result;
  } else if (dtype === 'align') {
    const args: any = doc.args;
    const offset: any = args[0];
    const contents: any = (Array.isArray(args) && (args.length >= 3) && (args[args.length - 2] === Symbol.for('.')) && ((): any => {
      const x: any = lastCdr(args);
      return Array.isArray(x) && (x.length === 0);
    })()) ? ((): any => {
      let i: any = 1;
      let result: any = args;
      while (i > 0) {
        if (Array.isArray(result) && (result.length === 3) && (result[1] === Symbol.for('.'))) {
          result = args[args.length - 1];
        } else {
          result = args.slice(1);
        }
        i--;
      }
      if (Array.isArray(result)) {
        result = result[0];
      }
      return result;
    })() : args[1];
    const contentsPrinted: any = printDocToDocList(contents, options).filter(function (x: any): any {
      return x !== empty;
    });
    let result: any = [];
    const indentation: any = ' '.repeat(offset);
    if (contentsPrinted.length > 0) {
      result.push(indentation);
    }
    const _end: any = contentsPrinted.length;
    for (let i: any = 0; i < _end; i++) {
      let current: any = (contentsPrinted as any)[i];
      const next: any = (i < (contentsPrinted.length - 1)) ? contentsPrinted[i + 1] : empty;
      result.push(current);
      if ((current === line) && (next !== line) && (next !== empty)) {
        result.push(indentation);
      }
    }
    return result;
  } else if (dtype === 'indent') {
    const args: any = doc.args;
    const contents: any = args[0];
    const offset: any = options['indent'] || 2;
    return printDocToDocList(new DocCommand('align', offset, contents, options), options);
  } else if (dtype === 'group') {
    const args: any = doc.args;
    const contents: any = args[0];
    const contentsPrinted: any = printDocToDocList(contents, options);
    return contentsPrinted;
  } else {
    return [doc];
  }
}

printDocToDocList.lispSource = [Symbol.for('define'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('dtype'), [Symbol.for('doc-type'), Symbol.for('doc')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('dtype'), 'string'], [Symbol.for('join'), Symbol.for('line'), [Symbol.for('string-split'), Symbol.for('doc'), Symbol.for('line')]]], [[Symbol.for('eq?'), Symbol.for('dtype'), 'array'], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('x-result'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('x-result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), Symbol.for('x-result')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('x-result')]]], Symbol.for('result')], [[Symbol.for('eq?'), Symbol.for('dtype'), 'align'], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('offset'), [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('contents'), [Symbol.for('second'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('contents-printed'), [Symbol.for('filter'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('empty')]]], [Symbol.for('print-doc-to-doc-list'), Symbol.for('contents'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('indentation'), [Symbol.for('string-repeat'), ' ', Symbol.for('offset')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('contents-printed')], 0], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('indentation')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('array-list-length'), Symbol.for('contents-printed')]]]], [Symbol.for('define'), Symbol.for('current'), [Symbol.for('aget'), Symbol.for('contents-printed'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('next'), [Symbol.for('if'), [Symbol.for('<'), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('contents-printed')], 1]], [Symbol.for('aget'), Symbol.for('contents-printed'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('empty')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('current')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('current'), Symbol.for('line')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('next'), Symbol.for('line')]], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('next'), Symbol.for('empty')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('indentation')]]], Symbol.for('result')], [[Symbol.for('eq?'), Symbol.for('dtype'), 'indent'], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('contents'), [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('offset'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), 'indent'], 2]], [Symbol.for('print-doc-to-doc-list'), [Symbol.for('new'), Symbol.for('DocCommand'), 'align', Symbol.for('offset'), Symbol.for('contents'), Symbol.for('options')], Symbol.for('options')]], [[Symbol.for('eq?'), Symbol.for('dtype'), 'group'], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('contents'), [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('contents-printed'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('contents'), Symbol.for('options')]], Symbol.for('contents-printed')], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('doc')]]]];

/**
 * Print a `Doc` list to a string.
 */
function printDocListToString(doc: any, options: any = {}): any {
  const dtype: any = docType(doc);
  if (dtype === 'string') {
    return doc;
  } else if (dtype === 'array') {
    return doc.map(function (x: any): any {
      return printDocListToString(x, options);
    }).join(empty);
  } else if (dtype === 'literalline') {
    return line;
  } else {
    return empty;
  }
}

printDocListToString.lispSource = [Symbol.for('define'), [Symbol.for('print-doc-list-to-string'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('dtype'), [Symbol.for('doc-type'), Symbol.for('doc')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('dtype'), 'string'], Symbol.for('doc')], [[Symbol.for('eq?'), Symbol.for('dtype'), 'array'], [Symbol.for('string-join'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-doc-list-to-string'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('doc')], Symbol.for('empty')]], [[Symbol.for('eq?'), Symbol.for('dtype'), 'literalline'], Symbol.for('line')], [Symbol.for('else'), Symbol.for('empty')]]];

/**
 * Print an ESTree node to a string.
 */
function printToString(node: any, options: any = {}): any {
  return printDoc(printNode(node, options), options);
}

printToString.lispSource = [Symbol.for('define'), [Symbol.for('print-to-string'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('node'), Symbol.for('options')], Symbol.for('options')]];

/**
 * Print an ESTree node to a `Doc` object.
 */
function printNode(node: any, options: any = {}): any {
  return visit(printVisitor, node, options);
}

printNode.lispSource = [Symbol.for('define'), [Symbol.for('print-node'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('visit'), Symbol.for('print-visitor'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Visitor function for printing ESTree nodes.
 */
function printVisitor(node: any, options: any): any {
  const type: any = estreeType(node);
  const comments: any = options['comments'];
  const printer: any = printerMap.get(type) || defaultPrinter;
  let result: any = printer(node, options);
  if (comments) {
    result = attachComments(result, node, options);
  }
  return result;
}

printVisitor.lispSource = [Symbol.for('define'), [Symbol.for('print-visitor'), Symbol.for('node'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('type'), [Symbol.for('estree-type'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('oget'), Symbol.for('options'), 'comments']], [Symbol.for('define'), Symbol.for('printer'), [Symbol.for('or'), [Symbol.for('hash-ref'), Symbol.for('printer-map'), Symbol.for('type')], Symbol.for('default-printer')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('printer'), Symbol.for('node'), Symbol.for('options')]], [Symbol.for('when'), Symbol.for('comments'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('attach-comments'), Symbol.for('result'), Symbol.for('node'), Symbol.for('options')]]], Symbol.for('result')];

/**
 * Print an `ExpressionStatement` ESTree node to a `Doc` object.
 */
function printExpressionStatement(node: any, options: any = {}): any {
  return [printNode(node.expression, options), ';'];
}

printExpressionStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-expression-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('expression'), Symbol.for('node')], Symbol.for('options')], ';']];

/**
 * Print a `ReturnStatement` ESTree node to a `Doc` object.
 */
function printReturnStatement(node: any, options: any = {}): any {
  const argument: any = node.argument;
  if (argument) {
    let argumentPrinted: any = printNode(argument, options);
    if (docShouldBreakP(argumentPrinted)) {
      argumentPrinted = ['(', line, indent(argumentPrinted), line, ')'];
    }
    return ['return', space, argumentPrinted, ';'];
  } else {
    return ['return', ';'];
  }
}

printReturnStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-return-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]], [Symbol.for('cond'), [Symbol.for('argument'), [Symbol.for('define'), Symbol.for('argument-printed'), [Symbol.for('print-node'), Symbol.for('argument'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('doc-should-break?'), Symbol.for('argument-printed')], [Symbol.for('set!'), Symbol.for('argument-printed'), [Symbol.for('list'), '(', Symbol.for('line'), [Symbol.for('indent'), Symbol.for('argument-printed')], Symbol.for('line'), ')'], Symbol.for('options')]], [Symbol.for('list'), 'return', Symbol.for('space'), Symbol.for('argument-printed'), ';']], [Symbol.for('else'), [Symbol.for('list'), 'return', ';']]]];

/**
 * Print a `YieldExpression` ESTree node to a `Doc` object.
 */
function printYieldExpression(node: any, options: any = {}): any {
  return ['yield', node.argument ? [space, printNode(node.argument, options)] : empty];
}

printYieldExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-yield-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'yield', [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')], [Symbol.for('list'), Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')], Symbol.for('options')]], Symbol.for('empty')]]];

/**
 * Print a `ThrowStatement` ESTree node to a `Doc` object.
 */
function printThrowStatement(node: any, options: any = {}): any {
  return ['throw', space, printNode(node.argument, options), ';'];
}

printThrowStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-throw-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'throw', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')], Symbol.for('options')], ';']];

/**
 * Print an `AwaitExpression` ESTree node to a `Doc` object.
 */
function printAwaitExpression(node: any, options: any = {}): any {
  return ['await', space, printNode(node.argument, options)];
}

printAwaitExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-await-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'await', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')], Symbol.for('options')]]];

/**
 * Print a `BreakStatement` ESTree node to a `Doc` object.
 */
function printBreakStatement(node: any, options: any = {}): any {
  return ['break', node.label ? [space, printNode(node.label, options)] : empty, ';'];
}

printBreakStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-break-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'break', [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('label'), Symbol.for('node')], [Symbol.for('list'), Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('label'), Symbol.for('node')], Symbol.for('options')]], Symbol.for('empty')], ';']];

/**
 * Print a `ContinueStatement` ESTree node to a `Doc` object.
 */
function printContinueStatement(node: any, options: any = {}): any {
  return ['continue', node.label ? [space, printNode(node.label, options)] : empty, ';'];
}

printContinueStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-continue-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'continue', [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('label'), Symbol.for('node')], [Symbol.for('list'), Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('label'), Symbol.for('node')], Symbol.for('options')]], Symbol.for('empty')], ';']];

/**
 * Print a `ThisExpression` ESTree node to a `Doc` object.
 */
function printThisExpression(node: any, options: any = {}): any {
  return 'this';
}

printThisExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-this-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'this'];

/**
 * Print an `Identifier` ESTree node to a `Doc` object.
 */
function printIdentifier(node: any, options: any = {}): any {
  const language: any = options['language'];
  const noImplicitAny: any = options['noImplicitAny'];
  let type_: any = node.typeAnnotation;
  if (noImplicitAny && !type_) {
    type_ = new TSAnyKeyword();
  }
  return [node.name, ((language === 'TypeScript') && type_) ? [node.optional ? '?:' : ':', space, printNode(type_, options)] : empty];
}

printIdentifier.lispSource = [Symbol.for('define'), [Symbol.for('print-identifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('no-implicit-any'), [Symbol.for('oget'), Symbol.for('options'), 'noImplicitAny']], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('no-implicit-any'), [Symbol.for('not'), Symbol.for('type_')]], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]], [Symbol.for('list'), [Symbol.for('get-field'), Symbol.for('name'), Symbol.for('node')], [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], Symbol.for('type_')], [Symbol.for('list'), [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('optional'), Symbol.for('node')], '?:', ':'], Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('type_'), Symbol.for('options')]], Symbol.for('empty')]]];

/**
 * Print a `Literal` ESTree node to a `Doc` object.
 */
function printLiteral(node: any, options: any = {}): any {
  const value: any = node.value;
  if (typeof value === 'string') {
    return printStringLiteral(node, options);
  } else if (value === true) {
    return 'true';
  } else if (value === false) {
    return 'false';
  } else if (value === null) {
    return 'null';
  } else if (value === undefined) {
    return 'undefined';
  } else {
    return value + '';
  }
}

printLiteral.lispSource = [Symbol.for('define'), [Symbol.for('print-literal'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('string?'), Symbol.for('value')], [Symbol.for('print-string-literal'), Symbol.for('node'), Symbol.for('options')]], [[Symbol.for('eq?'), Symbol.for('value'), Symbol.for('#t')], 'true'], [[Symbol.for('eq?'), Symbol.for('value'), Symbol.for('#f')], 'false'], [[Symbol.for('eq?'), Symbol.for('value'), Symbol.for('js/null')], 'null'], [[Symbol.for('eq?'), Symbol.for('value'), Symbol.for('undefined')], 'undefined'], [Symbol.for('else'), [Symbol.for('string-append'), Symbol.for('value'), '']]]];

/**
 * Print a string `Literal` ESTree node to a `Doc` object.
 *
 * Helper function for `print-literal`.
 */
function printStringLiteral(node: any, options: any = {}): any {
  let str: any = node.value.replace(new RegExp('\\\\', 'g'), '\\\\').replace(new RegExp('\'', 'g'), '\\\'').replace(new RegExp('\\n', 'g'), '\\n');
  return ['\'', str, '\''];
}

printStringLiteral.lispSource = [Symbol.for('define'), [Symbol.for('print-string-literal'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('_')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\\\', 'g'], Symbol.for('_'), '\\\\'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\'', 'g'], Symbol.for('_'), '\\\''], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\n', 'g'], Symbol.for('_'), '\\n']]], [Symbol.for('list'), '\'', Symbol.for('str'), '\'']];

/**
 * Print a template string.
 *
 * Helper function for `print-template-element` and
 * `print-template-literal`.
 */
function printTemplateString(str: any): any {
  return ['`', join(literalline, str.split(line)), '`'];
}

printTemplateString.lispSource = [Symbol.for('define'), [Symbol.for('print-template-string'), Symbol.for('str')], [Symbol.for('list'), '`', [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('string-split'), Symbol.for('line')], [Symbol.for('join'), Symbol.for('literalline'), Symbol.for('_')]], '`']];

/**
 * Print a `TemplateElement` ESTree node to a `Doc` object.
 */
function printTemplateElement(node: any, options: any = {}): any {
  let str: any = node.value.raw;
  return printTemplateString(str);
}

printTemplateElement.lispSource = [Symbol.for('define'), [Symbol.for('print-template-element'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('get-field'), Symbol.for('raw'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]]], [Symbol.for('print-template-string'), Symbol.for('str')]];

/**
 * Print a `TemplateLiteral` ESTree node to a `Doc` object.
 */
function printTemplateLiteral(node: any, options: any = {}): any {
  let str: any = node.quasis[0].value.raw;
  return printTemplateString(str);
}

printTemplateLiteral.lispSource = [Symbol.for('define'), [Symbol.for('print-template-literal'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('get-field'), Symbol.for('raw'), [Symbol.for('get-field'), Symbol.for('value'), [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('quasis'), Symbol.for('node')]]]]], [Symbol.for('print-template-string'), Symbol.for('str')]];

/**
 * Print a `TaggedTemplateExpression` ESTree node to a `Doc` object.
 */
function printTaggedTemplateExpression(node: any, options: any = {}): any {
  const tag: any = node.tag;
  const tagPrinted: any = printNode(tag, options);
  const quasi: any = node.quasi;
  const quasiPrinted: any = printNode(quasi, options);
  return [tagPrinted, quasiPrinted];
}

printTaggedTemplateExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-tagged-template-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('tag'), [Symbol.for('get-field'), Symbol.for('tag'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('tag-printed'), [Symbol.for('print-node'), Symbol.for('tag'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('quasi'), [Symbol.for('get-field'), Symbol.for('quasi'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('quasi-printed'), [Symbol.for('print-node'), Symbol.for('quasi'), Symbol.for('options')]], [Symbol.for('list'), Symbol.for('tag-printed'), Symbol.for('quasi-printed')]];

/**
 * Print a `UnaryExpression` ESTree node to a `Doc` object.
 */
function printUnaryExpression(node: any, options: any = {}): any {
  const prefix: any = node.prefix;
  const operator: any = node.operator;
  const operatorPrinted: any = operator;
  const argument: any = node.argument;
  let argumentPrinted: any = printNode(argument, options);
  if (!estreeSimpleP(argument)) {
    argumentPrinted = docWrap(argumentPrinted, options);
  }
  if (prefix) {
    return [operatorPrinted, ((operatorPrinted === 'delete') || (operatorPrinted === 'typeof')) ? space : empty, argumentPrinted];
  } else {
    return [argumentPrinted, operatorPrinted];
  }
}

printUnaryExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-unary-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('prefix'), [Symbol.for('get-field'), Symbol.for('prefix'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator-printed'), Symbol.for('operator')], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-field'), Symbol.for('argument'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('argument-printed'), [Symbol.for('print-node'), Symbol.for('argument'), Symbol.for('options')]], [Symbol.for('unless'), [Symbol.for('estree-simple?'), Symbol.for('argument')], [Symbol.for('set!'), Symbol.for('argument-printed'), [Symbol.for('doc-wrap'), Symbol.for('argument-printed'), Symbol.for('options')]]], [Symbol.for('cond'), [Symbol.for('prefix'), [Symbol.for('list'), Symbol.for('operator-printed'), [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('operator-printed'), 'delete'], [Symbol.for('eq?'), Symbol.for('operator-printed'), 'typeof']], Symbol.for('space'), Symbol.for('empty')], Symbol.for('argument-printed')]], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('argument-printed'), Symbol.for('operator-printed')]]]];

/**
 * Print a `BinaryExpression` ESTree node to a `Doc` object.
 */
function printBinaryExpression(node: any, options: any = {}): any {
  let type_: any = estreeType(node);
  const operator: any = node.operator;
  const operatorPrinted: any = operator;
  const left: any = node.left;
  let leftPrinted: any = printNode(left, options);
  let leftPrintedStr: any = docValueString(leftPrinted);
  const right: any = node.right;
  const rightPrinted: any = printNode(right, options);
  let rightPrintedStr: any = docValueString(rightPrinted);
  let shouldBreak: any = docShouldBreakP(leftPrinted) || docShouldBreakP(rightPrinted);
  const isMultilineStringLiteral: any = estreeStringLiteralP(left) && left.value.match('\\n$');
  const isMultilineBinaryExpression: any = !isMultilineStringLiteral && estreeTypeP(left, 'BinaryExpression') && estreeStringLiteralP(left.right) && left.right.value.match(new RegExp('\\n$'));
  const isMultilineString: any = isMultilineStringLiteral || isMultilineBinaryExpression;
  let result: any;
  if (!(estreeSimpleP(left) || (estreeTypeP(left, type_) && (left.operator === operator)))) {
    leftPrintedStr = docWrap(leftPrintedStr, {
      ...options,
      'has-comments': docHasCommentsP(leftPrinted)
    });
  }
  if (!(estreeSimpleP(right) || (estreeTypeP(right, type_) && (right.operator === operator) && ['+', '*', '&&', '||'].includes(operator)))) {
    rightPrintedStr = docWrap(rightPrintedStr, {
      ...options,
      'has-comments': docHasCommentsP(rightPrinted)
    });
  }
  if (shouldBreak) {
    result = ['(', line, align(1, leftPrintedStr), estreeHasTrailingCommentP(left) ? [line, align(1, operator)] : [space, operator], line, align(1, rightPrintedStr), line, ')'];
  } else if (isMultilineString) {
    result = [leftPrintedStr, space, operator, line, indent(rightPrintedStr)];
  } else {
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
function printLogicalExpression(node: any, options: any = {}): any {
  return printBinaryExpression(node, options);
}

printLogicalExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-logical-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-binary-expression'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print an `AssignmentExpression` ESTree node to a `Doc` object.
 */
function printAssignmentExpression(node: any, options: any = {}): any {
  // TODO: Break up statement if one of the sides have comments.
  const language: any = options['language'];
  const operator: any = node.operator;
  const operatorPrinted: any = operator;
  const left: any = node.left;
  let leftPrinted: any = printNode(left, options);
  const right: any = node.right;
  const rightPrinted: any = printNode(right, {
    ...options,
    noImplicitAny: false
  });
  let result: any = [leftPrinted, space, operator, docHasCommentsP(rightPrinted) ? [line, indent(rightPrinted)] : [space, rightPrinted]];
  if (estreeTypeP(left, 'ObjectPattern')) {
    result = docWrap(result, options);
  }
  return result;
}

printAssignmentExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-assignment-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('get-field'), Symbol.for('operator'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator-printed'), Symbol.for('operator')], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-printed'), [Symbol.for('print-node'), Symbol.for('left'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-printed'), [Symbol.for('print-node'), Symbol.for('right'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('left-printed'), Symbol.for('space'), Symbol.for('operator'), [Symbol.for('if'), [Symbol.for('doc-has-comments?'), Symbol.for('right-printed')], [Symbol.for('list'), Symbol.for('line'), [Symbol.for('indent'), Symbol.for('right-printed')]], [Symbol.for('list'), Symbol.for('space'), Symbol.for('right-printed')]]]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('left'), 'ObjectPattern'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('doc-wrap'), Symbol.for('result'), Symbol.for('options')]]], Symbol.for('result')];

/**
 * Print an `AssignmentPattern` ESTree node to a `Doc` object.
 */
function printAssignmentPattern(node: any, options: any = {}): any {
  return printNode(new VariableDeclarator(node.left, node.right), options);
}

printAssignmentPattern.lispSource = [Symbol.for('define'), [Symbol.for('print-assignment-pattern'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-node'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')], [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], Symbol.for('options')]];

/**
 * Print a `CallExpression` ESTree node to a `Doc` object.
 */
function printCallExpression(node: any, options: any = {}): any {
  const callee: any = node.callee;
  const calleeType: any = estreeType(callee);
  let calleePrinted: any = printNode(callee, options);
  const args: any = node.arguments;
  const argsPrinted: any = args.map(function (x: any): any {
    return printNode(x, options);
  });
  const optional: any = node.optional;
  if (estreeComplexP(callee)) {
    calleePrinted = docWrap(calleePrinted, options);
  }
  return [calleePrinted, optional ? '?.' : empty, '(', join([',', space], argsPrinted), ')'];
}

printCallExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-call-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('callee'), [Symbol.for('get-field'), Symbol.for('callee'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('callee-type'), [Symbol.for('estree-type'), Symbol.for('callee')]], [Symbol.for('define'), Symbol.for('callee-printed'), [Symbol.for('print-node'), Symbol.for('callee'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('arguments'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('args-printed'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('args')]], [Symbol.for('define'), Symbol.for('optional'), [Symbol.for('get-field'), Symbol.for('optional'), Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('estree-complex?'), Symbol.for('callee')], [Symbol.for('set!'), Symbol.for('callee-printed'), [Symbol.for('doc-wrap'), Symbol.for('callee-printed'), Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('callee-printed'), [Symbol.for('if'), Symbol.for('optional'), '?.', Symbol.for('empty')], '(', [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('args-printed')], ')']];

/**
 * Print a `SequenceExpression` ESTree node to a `Doc` object.
 */
function printSequenceExpression(node: any, options: any = {}): any {
  const expressions: any = node.expressions;
  const expressionsPrinted: any = expressions.map(function (x: any): any {
    return printNode(x, options);
  });
  let result: any;
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
function printBlockStatement(node: any, options: any = {}): any {
  const body: any = node.body;
  const bodyModified: any = ((): any => {
    if (node.comments && (body.length > 0)) {
      body[0].comments = [...node.comments, ...(body[0].comments || [])];
      node.comments = [];
    }
    return node.body;
  })();
  const bodyIndented: any = indent(join(line, bodyModified.map(function (x: any): any {
    return printNode(x, options);
  })));
  const bodyPrinted: any = printDoc(bodyIndented);
  return ['{', line, bodyIndented, (bodyPrinted === '') ? empty : line, '}'];
}

printBlockStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-block-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-modified'), [Symbol.for('begin'), [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')], [Symbol.for('>'), [Symbol.for('array-list-length'), Symbol.for('body')], 0]], [Symbol.for('set-field!'), Symbol.for('comments'), [Symbol.for('first'), Symbol.for('body')], [Symbol.for('append'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')], [Symbol.for('or'), [Symbol.for('get-field'), Symbol.for('comments'), [Symbol.for('first'), Symbol.for('body')]], [Symbol.for('quote'), []]]]], [Symbol.for('set!'), [Symbol.for('get-field'), Symbol.for('comments'), Symbol.for('node')], [Symbol.for('quote'), []]]], [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]]], [Symbol.for('define'), Symbol.for('body-indented'), [Symbol.for('indent'), [Symbol.for('~>'), Symbol.for('body-modified'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), Symbol.for('line'), Symbol.for('_')]]]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-doc'), Symbol.for('body-indented')]], [Symbol.for('list'), '{', Symbol.for('line'), Symbol.for('body-indented'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('body-printed'), ''], Symbol.for('empty'), Symbol.for('line')], '}']];

/**
 * Print a `MemberExpression` ESTree node to a `Doc` object.
 */
function printMemberExpression(node: any, options: any = {}): any {
  const language: any = options['language'];
  const object: any = node.object;
  const objectType: any = estreeType(object);
  let objectPrinted: any = printNode(object, options);
  const property: any = node.property;
  const propertyPrinted: any = printNode(property, options);
  const computed: any = node.computed;
  const optional: any = node.optional;
  if (!estreeSimpleP(object) || (objectType === 'ObjectExpression')) {
    // If the object expression is complicated, wrap it in
    // parentheses.
    objectPrinted = docWrap(objectPrinted, options);
  }
  if (computed) {
    // Kludge: prevent errors with expressions like
    // `x[y]`, where `y` is `any`-typed.
    // TODO: Move this code into the compiler.
    if ((language === 'TypeScript') && !['Literal', 'UnaryExpression', 'BinaryExpression'].includes(estreeType(property))) {
      objectPrinted = docWrap([objectPrinted, ' as any'], options);
    }
    return [objectPrinted, '[', propertyPrinted, ']'];
  } else {
    return [objectPrinted, optional ? '?.' : '.', propertyPrinted];
  }
}

printMemberExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-member-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('object'), [Symbol.for('get-field'), Symbol.for('object'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('object-type'), [Symbol.for('estree-type'), Symbol.for('object')]], [Symbol.for('define'), Symbol.for('object-printed'), [Symbol.for('print-node'), Symbol.for('object'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('property'), [Symbol.for('get-field'), Symbol.for('property'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('property-printed'), [Symbol.for('print-node'), Symbol.for('property'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('computed'), [Symbol.for('get-field'), Symbol.for('computed'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('optional'), [Symbol.for('get-field'), Symbol.for('optional'), Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('estree-simple?'), Symbol.for('object')]], [Symbol.for('eq?'), Symbol.for('object-type'), 'ObjectExpression']], [Symbol.for('set!'), Symbol.for('object-printed'), [Symbol.for('doc-wrap'), Symbol.for('object-printed'), Symbol.for('options')]]], [Symbol.for('cond'), [Symbol.for('computed'), [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('not'), [Symbol.for('memq?'), [Symbol.for('estree-type'), Symbol.for('property')], [Symbol.for('quote'), ['Literal', 'UnaryExpression', 'BinaryExpression']]]]], [Symbol.for('set!'), Symbol.for('object-printed'), [Symbol.for('doc-wrap'), [Symbol.for('list'), Symbol.for('object-printed'), ' as any'], Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('object-printed'), '[', Symbol.for('property-printed'), ']']], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('object-printed'), [Symbol.for('if'), Symbol.for('optional'), '?.', '.'], Symbol.for('property-printed')]]]];

/**
 * Print an `UpdateExpression` ESTree node to a `Doc` object.
 */
function printUpdateExpression(node: any, options: any = {}): any {
  return printUnaryExpression(node, options);
}

printUpdateExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-update-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-unary-expression'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print a `SpreadElement` ESTree node to a `Doc` object.
 */
function printSpreadElement(node: any, options: any = {}): any {
  const language: any = options['language'];
  const noImplicitAny: any = options['noImplicitAny'];
  const argument: any = node.argument;
  let argumentPrinted: any = printNode(argument, {
    ...options,
    noImplicitAny: false
  });
  let type_: any = node.typeAnnotation;
  if (noImplicitAny && !type_) {
    type_ = new TSArrayType(new TSAnyKeyword());
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
function printRestElement(node: any, options: any = {}): any {
  return printSpreadElement(node, options);
}

printRestElement.lispSource = [Symbol.for('define'), [Symbol.for('print-rest-element'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-spread-element'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print a function declaration or function expression to a
 * `Doc` object. Also handles arrow functions.
 */
function printFunction(node: any, options: any = {}, settings: any = {}): any {
  const language: any = options['language'];
  const arrow: any = settings['arrow'];
  const async_: any = node.async;
  const returnTypeSetting: any = settings['returnType'];
  const returnType: any = (typeof returnTypeSetting === 'string') ? returnTypeSetting : node.returnType;
  const returnTypePrinted: any = (typeof returnType === 'string') ? returnType : (returnType ? printTsType(returnType, options) : (async_ ? 'Promise<any>' : 'any'));
  return [async_ ? ['async', space] : empty, arrow ? empty : ['function', space], node.id ? printNode(node.id, options) : empty, '(', join([',', space], node.params.map(function (x: any): any {
    return printNode(x, {
      ...options,
      noImplicitAny: true
    });
  })), ')', ((language === 'TypeScript') && (returnTypePrinted !== '')) ? [':', space, returnTypePrinted] : empty, arrow ? [space, '=>', space] : space, printNode(node.body, options)];
}

printFunction.lispSource = [Symbol.for('define'), [Symbol.for('print-function'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]], [Symbol.for('settings'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('arrow'), [Symbol.for('oget'), Symbol.for('settings'), 'arrow']], [Symbol.for('define'), Symbol.for('async_'), [Symbol.for('get-field'), Symbol.for('async'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('return-type-setting'), [Symbol.for('oget'), Symbol.for('settings'), 'returnType']], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('if'), [Symbol.for('string?'), Symbol.for('return-type-setting')], Symbol.for('return-type-setting'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('node')]]], [Symbol.for('define'), Symbol.for('return-type-printed'), [Symbol.for('cond'), [[Symbol.for('string?'), Symbol.for('return-type')], Symbol.for('return-type')], [Symbol.for('return-type'), [Symbol.for('print-ts-type'), Symbol.for('return-type'), Symbol.for('options')]], [Symbol.for('async_'), 'Promise<any>'], [Symbol.for('else'), 'any']]], [Symbol.for('list'), [Symbol.for('if'), Symbol.for('async_'), [Symbol.for('list'), 'async', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('arrow'), Symbol.for('empty'), [Symbol.for('list'), 'function', Symbol.for('space')]], [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')], [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')], Symbol.for('options')], Symbol.for('empty')], '(', [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#t')]]]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], ')', [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('return-type-printed'), '']]], [Symbol.for('list'), ':', Symbol.for('space'), Symbol.for('return-type-printed')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('arrow'), [Symbol.for('list'), Symbol.for('space'), '=>', Symbol.for('space')], Symbol.for('space')], [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')], Symbol.for('options')]]];

/**
 * Print a `FunctionDeclaration` ESTree node to a `Doc` object.
 */
function printFunctionDeclaration(node: any, options: any = {}): any {
  return printFunction(node, options);
}

printFunctionDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-function-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-function'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print a `FunctionExpression` ESTree node to a `Doc` object.
 */
function printFunctionExpression(node: any, options: any = {}): any {
  return printFunction(node, options);
}

printFunctionExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-function-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-function'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print a `ArrowFunctionExpression` ESTree node to a `Doc` object.
 */
function printArrowFunctionExpression(node: any, options: any = {}): any {
  return printFunction(node, options, {
    arrow: true
  });
}

printArrowFunctionExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-arrow-function-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-function'), Symbol.for('node'), Symbol.for('options'), [Symbol.for('js-obj'), 'arrow', Symbol.for('#t')]]];

/**
 * Print a `VariableDeclaration` ESTree node to a `Doc` object.
 */
function printVariableDeclaration(node: any, options: any = {}): any {
  return [node.kind, space, join([',', space], node.declarations.map(function (x: any): any {
    return printNode(x, options);
  })), ';'];
}

printVariableDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-variable-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), [Symbol.for('get-field'), Symbol.for('kind'), Symbol.for('node')], Symbol.for('space'), [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('declarations'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], ';']];

/**
 * Print a `VariableDeclarator` ESTree node to a `Doc` object.
 */
function printVariableDeclarator(node: any, options: any = {}): any {
  const language: any = options['language'];
  const id: any = node.id;
  const idPrinted: any = printNode(id, {
    ...options,
    noImplicitAny: true
  });
  if (node.init) {
    const init: any = node.init;
    const initPrinted: any = printNode(init, {
      ...options,
      noImplicitAny: false
    });
    return [idPrinted, space, '=', docHasCommentsP(initPrinted) ? [line, indent(initPrinted)] : [space, initPrinted]];
  } else {
    return idPrinted;
  }
}

printVariableDeclarator.lispSource = [Symbol.for('define'), [Symbol.for('print-variable-declarator'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('id-printed'), [Symbol.for('print-node'), Symbol.for('id'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#t')]]]], [Symbol.for('cond'), [[Symbol.for('get-field'), Symbol.for('init'), Symbol.for('node')], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('init-printed'), [Symbol.for('print-node'), Symbol.for('init'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]]], [Symbol.for('list'), Symbol.for('id-printed'), Symbol.for('space'), '=', [Symbol.for('if'), [Symbol.for('doc-has-comments?'), Symbol.for('init-printed')], [Symbol.for('list'), Symbol.for('line'), [Symbol.for('indent'), Symbol.for('init-printed')]], [Symbol.for('list'), Symbol.for('space'), Symbol.for('init-printed')]]]], [Symbol.for('else'), Symbol.for('id-printed')]]];

/**
 * Print an `IfStatement` ESTree node to a `Doc` object.
 */
function printIfStatement(node: any, options: any = {}): any {
  const test: any = node.test;
  let testPrinted: any = printNode(test, options);
  const testPrintedStr: any = estreeTypeP(test, 'AssignmentExpression') ? docWrap(docValueString(testPrinted), options) : docValueString(testPrinted);
  const consequent: any = node.consequent;
  let consequentPrinted: any = printNode(consequent, options);
  const alternate: any = node.alternate;
  let result: any = 'if (' + testPrintedStr + ')' + (docShouldBreakP(consequentPrinted) ? line : space) + docValueString(consequentPrinted);
  if (alternate) {
    let alternatePrinted: any = printNode(alternate, options);
    result = result + ' else ' + docValueString(alternatePrinted);
  }
  return result;
}

printIfStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-if-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('test-printed-str'), [Symbol.for('if'), [Symbol.for('estree-type?'), Symbol.for('test'), 'AssignmentExpression'], [Symbol.for('doc-wrap'), [Symbol.for('doc-value-string'), Symbol.for('test-printed')], Symbol.for('options')], [Symbol.for('doc-value-string'), Symbol.for('test-printed')]]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('get-field'), Symbol.for('consequent'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('consequent-printed'), [Symbol.for('print-node'), Symbol.for('consequent'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('alternate'), [Symbol.for('get-field'), Symbol.for('alternate'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('string-append'), 'if (', Symbol.for('test-printed-str'), ')', [Symbol.for('if'), [Symbol.for('doc-should-break?'), Symbol.for('consequent-printed')], Symbol.for('line'), Symbol.for('space')], [Symbol.for('doc-value-string'), Symbol.for('consequent-printed')]]], [Symbol.for('when'), Symbol.for('alternate'), [Symbol.for('define'), Symbol.for('alternate-printed'), [Symbol.for('print-node'), Symbol.for('alternate'), Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('string-append'), Symbol.for('result'), ' else ', [Symbol.for('doc-value-string'), Symbol.for('alternate-printed')]]]], Symbol.for('result')];

/**
 * Print a `ConditionalExpression` ESTree node to a `Doc` object.
 */
function printConditionalExpression(node: any, options: any = {}): any {
  const test: any = node.test;
  let testPrinted: any = printNode(test, options);
  const consequent: any = node.consequent;
  let consequentPrinted: any = printNode(consequent, options);
  const alternate: any = node.alternate;
  let alternatePrinted: any = printNode(alternate, options);
  let result: any;
  if (!estreeSimpleP(test)) {
    testPrinted = docWrap(testPrinted, options);
  }
  if (!estreeSimpleP(consequent)) {
    consequentPrinted = docWrap(consequentPrinted, options);
  }
  if (!(estreeTypeP(alternate, 'SequenceExpression') || estreeSimpleP(alternate))) {
    alternatePrinted = docWrap(alternatePrinted, options);
  }
  return [testPrinted, space, '?', space, consequentPrinted, space, ':', space, alternatePrinted];
}

printConditionalExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-conditional-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('get-field'), Symbol.for('consequent'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('consequent-printed'), [Symbol.for('print-node'), Symbol.for('consequent'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('alternate'), [Symbol.for('get-field'), Symbol.for('alternate'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('alternate-printed'), [Symbol.for('print-node'), Symbol.for('alternate'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('unless'), [Symbol.for('estree-simple?'), Symbol.for('test')], [Symbol.for('set!'), Symbol.for('test-printed'), [Symbol.for('doc-wrap'), Symbol.for('test-printed'), Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('estree-simple?'), Symbol.for('consequent')], [Symbol.for('set!'), Symbol.for('consequent-printed'), [Symbol.for('doc-wrap'), Symbol.for('consequent-printed'), Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('estree-type?'), Symbol.for('alternate'), 'SequenceExpression'], [Symbol.for('estree-simple?'), Symbol.for('alternate')]], [Symbol.for('set!'), Symbol.for('alternate-printed'), [Symbol.for('doc-wrap'), Symbol.for('alternate-printed'), Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('test-printed'), Symbol.for('space'), '?', Symbol.for('space'), Symbol.for('consequent-printed'), Symbol.for('space'), ':', Symbol.for('space'), Symbol.for('alternate-printed')]];

/**
 * Print a `WhileStatement` ESTree node to a `Doc` object.
 */
function printWhileStatement(node: any, options: any = {}): any {
  const test: any = node.test;
  let testPrinted: any = printNode(test, options);
  const body: any = node.body;
  const bodyPrinted: any = printNode(body, options);
  if (estreeTypeP(test, 'AssignmentExpression')) {
    testPrinted = docWrap(testPrinted, options);
  }
  return ['while', space, '(', testPrinted, ')', space, bodyPrinted];
}

printWhileStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-while-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('test'), 'AssignmentExpression'], [Symbol.for('set!'), Symbol.for('test-printed'), [Symbol.for('doc-wrap'), Symbol.for('test-printed'), Symbol.for('options')]]], [Symbol.for('list'), 'while', Symbol.for('space'), '(', Symbol.for('test-printed'), ')', Symbol.for('space'), Symbol.for('body-printed')]];

/**
 * Print a `DoWhileStatement` ESTree node to a `Doc` object.
 */
function printDoWhileStatement(node: any, options: any = {}): any {
  const test: any = node.test;
  let testPrinted: any = printNode(test, options);
  const body: any = node.body;
  const bodyPrinted: any = printNode(body, options);
  if (estreeTypeP(test, 'AssignmentExpression')) {
    testPrinted = docWrap(testPrinted, options);
  }
  return ['do', space, bodyPrinted, space, 'while', space, '(', testPrinted, ')', ';'];
}

printDoWhileStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-do-while-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('test'), 'AssignmentExpression'], [Symbol.for('set!'), Symbol.for('test-printed'), [Symbol.for('doc-wrap'), Symbol.for('test-printed'), Symbol.for('options')]]], [Symbol.for('list'), 'do', Symbol.for('space'), Symbol.for('body-printed'), Symbol.for('space'), 'while', Symbol.for('space'), '(', Symbol.for('test-printed'), ')', ';']];

/**
 * Print a `ForStatement` ESTree node to a `Doc` object.
 */
function printForStatement(node: any, options: any = {}): any {
  const init: any = node.init;
  const initPrinted: any = printDoc(printNode(init, options), options).replace(new RegExp(';$'), '');
  const test: any = node.test;
  let testPrinted: any = printDoc(printNode(test, options), options);
  const update: any = node.update;
  const updatePrinted: any = printDoc(printNode(update, options), options).replace(new RegExp(';$'), '');
  const body: any = node.body;
  const bodyPrinted: any = printNode(body, options);
  return ['for', space, '(', initPrinted, ';', space, testPrinted, ';', space, updatePrinted, ')', space, bodyPrinted];
}

printForStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-for-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('get-field'), Symbol.for('init'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('init-printed'), [Symbol.for('~>'), [Symbol.for('print-node'), Symbol.for('init'), Symbol.for('options')], [Symbol.for('print-doc'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ';$'], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('update'), [Symbol.for('get-field'), Symbol.for('update'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('update-printed'), [Symbol.for('~>'), [Symbol.for('print-node'), Symbol.for('update'), Symbol.for('options')], [Symbol.for('print-doc'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ';$'], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('list'), 'for', Symbol.for('space'), '(', Symbol.for('init-printed'), ';', Symbol.for('space'), Symbol.for('test-printed'), ';', Symbol.for('space'), Symbol.for('update-printed'), ')', Symbol.for('space'), Symbol.for('body-printed')]];

/**
 * Print a `ForOfStatement` ESTree node to a `Doc` object.
 */
function printForOfStatement(node: any, options: any = {}): any {
  const language: any = options['language'];
  const left: any = node.left;
  let leftPrinted: any = printDoc(printNode(left, options), options).replace(new RegExp(';$'), '');
  const right: any = node.right;
  const rightPrinted: any = printNode(right, options);
  const body: any = node.body;
  const bodyPrinted: any = printNode(body, options);
  let resultStr: any;
  if (language === 'TypeScript') {
    leftPrinted = leftPrinted.replace(new RegExp(': any$'), '');
  }
  return ['for', space, '(', leftPrinted, space, 'of', space, rightPrinted, ')', space, bodyPrinted];
}

printForOfStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-for-of-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-printed'), [Symbol.for('~>'), [Symbol.for('print-node'), Symbol.for('left'), Symbol.for('options')], [Symbol.for('print-doc'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ';$'], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-printed'), [Symbol.for('print-node'), Symbol.for('right'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result-str')], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('set!'), Symbol.for('left-printed'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ': any$'], Symbol.for('left-printed'), '']]], [Symbol.for('list'), 'for', Symbol.for('space'), '(', Symbol.for('left-printed'), Symbol.for('space'), 'of', Symbol.for('space'), Symbol.for('right-printed'), ')', Symbol.for('space'), Symbol.for('body-printed')]];

/**
 * Print a `ForInStatement` ESTree node to a `Doc` object.
 */
function printForInStatement(node: any, options: any = {}): any {
  const language: any = options['language'];
  const left: any = node.left;
  let leftPrinted: any = printDoc(printNode(left, options), options).replace(new RegExp(';$'), '');
  const right: any = node.right;
  const rightPrinted: any = printNode(right, options);
  const body: any = node.body;
  const bodyPrinted: any = printNode(body, options);
  return ['for', space, '(', leftPrinted, space, 'in', space, rightPrinted, (language === 'TypeScript') ? ' as any[]' : empty, ')', space, '{', line, indent(bodyPrinted), line, '}'];
}

printForInStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-for-in-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-field'), Symbol.for('left'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-printed'), [Symbol.for('~>'), [Symbol.for('print-node'), Symbol.for('left'), Symbol.for('options')], [Symbol.for('print-doc'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ';$'], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-field'), Symbol.for('right'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-printed'), [Symbol.for('print-node'), Symbol.for('right'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('list'), 'for', Symbol.for('space'), '(', Symbol.for('left-printed'), Symbol.for('space'), 'in', Symbol.for('space'), Symbol.for('right-printed'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], ' as any[]', Symbol.for('empty')], ')', Symbol.for('space'), '{', Symbol.for('line'), [Symbol.for('indent'), Symbol.for('body-printed')], Symbol.for('line'), '}']];

/**
 * Print a `TryStatement` ESTree node to a `Doc` object.
 */
function printTryStatement(node: any, options: any = {}): any {
  const block: any = node.block;
  const blockPrinted: any = printNode(block, options);
  const handler: any = node.handler;
  const finalizer: any = node.finalizer;
  let result: any = ['try', space, blockPrinted];
  if (handler) {
    const handlerParam: any = handler.param;
    const handlerParamPrinted: any = handlerParam ? printNode(handlerParam, options) : false;
    const handlerBodyPrinted: any = printNode(handler.body, options);
    result = [...result, ...[space, 'catch', space], ...(handlerParam ? ['(', handlerParamPrinted, ')', space] : []), ...['{', line, indent(handlerBodyPrinted), line, '}']];
  }
  if (finalizer) {
    const finalizerPrinted: any = printNode(finalizer, options);
    result = [...result, ...[space, 'finally', space, finalizerPrinted]];
  }
  return result;
}

printTryStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-try-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('block'), [Symbol.for('get-field'), Symbol.for('block'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('block-printed'), [Symbol.for('print-node'), Symbol.for('block'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('handler'), [Symbol.for('get-field'), Symbol.for('handler'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('finalizer'), [Symbol.for('get-field'), Symbol.for('finalizer'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('list'), 'try', Symbol.for('space'), Symbol.for('block-printed')]], [Symbol.for('when'), Symbol.for('handler'), [Symbol.for('define'), Symbol.for('handler-param'), [Symbol.for('get-field'), Symbol.for('param'), Symbol.for('handler')]], [Symbol.for('define'), Symbol.for('handler-param-printed'), [Symbol.for('if'), Symbol.for('handler-param'), [Symbol.for('print-node'), Symbol.for('handler-param'), Symbol.for('options')], Symbol.for('#f')]], [Symbol.for('define'), Symbol.for('handler-body-printed'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('handler')], Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('space'), 'catch', Symbol.for('space')], [Symbol.for('if'), Symbol.for('handler-param'), [Symbol.for('list'), '(', Symbol.for('handler-param-printed'), ')', Symbol.for('space')], [Symbol.for('quote'), []]], [Symbol.for('list'), '{', Symbol.for('line'), [Symbol.for('indent'), Symbol.for('handler-body-printed')], Symbol.for('line'), '}']]]], [Symbol.for('when'), Symbol.for('finalizer'), [Symbol.for('define'), Symbol.for('finalizer-printed'), [Symbol.for('print-node'), Symbol.for('finalizer'), Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('space'), 'finally', Symbol.for('space'), Symbol.for('finalizer-printed')]]]], Symbol.for('result')];

/**
 * Print a `ClassDeclaration` ESTree node to a `Doc` object.
 */
function printClassDeclaration(node: any, options: any = {}): any {
  const id: any = node.id;
  const body: any = node.body;
  const bodyIndented: any = indent(printNode(body, options));
  const bodyPrinted: any = printDoc(bodyIndented, options);
  const superClass: any = node.superClass;
  return ['class', space, id ? [printNode(id, options), space] : empty, superClass ? ['extends', space, printNode(superClass, options), space] : empty, '{', line, bodyIndented, (bodyPrinted === '') ? empty : line, '}'];
}

printClassDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-class-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-indented'), [Symbol.for('indent'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-doc'), Symbol.for('body-indented'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('super-class'), [Symbol.for('get-field'), Symbol.for('superClass'), Symbol.for('node')]], [Symbol.for('list'), 'class', Symbol.for('space'), [Symbol.for('if'), Symbol.for('id'), [Symbol.for('list'), [Symbol.for('print-node'), Symbol.for('id'), Symbol.for('options')], Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('super-class'), [Symbol.for('list'), 'extends', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('super-class'), Symbol.for('options')], Symbol.for('space')], Symbol.for('empty')], '{', Symbol.for('line'), Symbol.for('body-indented'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('body-printed'), ''], Symbol.for('empty'), Symbol.for('line')], '}']];

/**
 * Print a `ClassExpression` ESTree node to a `Doc` object.
 */
function printClassExpression(node: any, options: any = {}): any {
  return printClassDeclaration(new ClassDeclaration(null, node.body, node.superClass), options);
}

printClassExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-class-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-class-declaration'), [Symbol.for('new'), Symbol.for('ClassDeclaration'), Symbol.for('js/null'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')], [Symbol.for('get-field'), Symbol.for('superClass'), Symbol.for('node')]], Symbol.for('options')]];

/**
 * Print a `ClassBody` ESTree node to a `Doc` object.
 */
function printClassBody(node: any, options: any = {}): any {
  return join([line, line], node.body.map(function (x: any): any {
    return printNode(x, options);
  }));
}

printClassBody.lispSource = [Symbol.for('define'), [Symbol.for('print-class-body'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('_')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), Symbol.for('line'), Symbol.for('line')], Symbol.for('_')]]];

/**
 * Print a `PropertyDefinition` ESTree node to a `Doc` object.
 */
function printPropertyDefinition(node: any, options: any = {}): any {
  const language: any = options['language'];
  const key: any = node.key;
  const value: any = node.value;
  const staticFlag: any = node.static;
  const accessibility: any = node.accessibility;
  return [((language === 'TypeScript') && (accessibility === 'private')) ? ['private', space] : empty, staticFlag ? ['static', space] : empty, printNode(key, options), (language === 'TypeScript') ? [':', space, 'any'] : empty, value ? [space, '=', space, printNode(value, options)] : empty, ';'];
}

printPropertyDefinition.lispSource = [Symbol.for('define'), [Symbol.for('print-property-definition'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-field'), Symbol.for('key'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('static-flag'), [Symbol.for('get-field'), Symbol.for('static'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('accessibility'), [Symbol.for('get-field'), Symbol.for('accessibility'), Symbol.for('node')]], [Symbol.for('list'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('eq?'), Symbol.for('accessibility'), 'private']], [Symbol.for('list'), 'private', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('static-flag'), [Symbol.for('list'), 'static', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options')], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('list'), ':', Symbol.for('space'), 'any'], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('value'), [Symbol.for('list'), Symbol.for('space'), '=', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('value'), Symbol.for('options')]], Symbol.for('empty')], ';']];

/**
 * Print a `MethodDefinition` ESTree node to a `Doc` object.
 */
function printMethodDefinition(node: any, options: any = {}): any {
  const language: any = options['language'];
  const key: any = node.key;
  let keyPrinted: any = printNode(key, options);
  const keyPrintedStr: any = printDoc(keyPrinted, options);
  const value: any = node.value;
  const valuePrinted: any = printDoc(printFunction(value, options, {
    returnType: (keyPrintedStr === 'constructor') ? '' : 'any'
  }), options).replace(new RegExp('^function '), '');
  const staticFlag: any = node.static;
  const computedFlag: any = node.computed;
  const generatorFlag: any = value.generator;
  const accessibility: any = node.accessibility;
  return [((language === 'TypeScript') && (accessibility === 'private')) ? ['private', space] : empty, staticFlag ? ['static', space] : empty, generatorFlag ? '*' : empty, computedFlag ? ['[', keyPrinted, ']'] : keyPrinted, valuePrinted];
}

printMethodDefinition.lispSource = [Symbol.for('define'), [Symbol.for('print-method-definition'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-field'), Symbol.for('key'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('key-printed'), [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('key-printed-str'), [Symbol.for('print-doc'), Symbol.for('key-printed'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('value-printed'), [Symbol.for('~>'), Symbol.for('value'), [Symbol.for('print-function'), Symbol.for('_'), Symbol.for('options'), [Symbol.for('js-obj'), 'returnType', [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('key-printed-str'), 'constructor'], '', 'any']]], [Symbol.for('print-doc'), Symbol.for('_'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^function '], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('static-flag'), [Symbol.for('get-field'), Symbol.for('static'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('computed-flag'), [Symbol.for('get-field'), Symbol.for('computed'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('generator-flag'), [Symbol.for('get-field'), Symbol.for('generator'), Symbol.for('value')]], [Symbol.for('define'), Symbol.for('accessibility'), [Symbol.for('get-field'), Symbol.for('accessibility'), Symbol.for('node')]], [Symbol.for('list'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('eq?'), Symbol.for('accessibility'), 'private']], [Symbol.for('list'), 'private', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('static-flag'), [Symbol.for('list'), 'static', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('generator-flag'), '*', Symbol.for('empty')], [Symbol.for('if'), Symbol.for('computed-flag'), [Symbol.for('list'), '[', Symbol.for('key-printed'), ']'], Symbol.for('key-printed')], Symbol.for('value-printed')]];

/**
 * Print an `ArrayExpression` ESTree node to a `Doc` object.
 */
function printArrayExpression(node: any, options: any = {}): any {
  const language: any = options['language'];
  const noImplicitAny: any = options['noImplicitAny'];
  let type_: any = node.typeAnnotation;
  const printedExpressions: any = [];
  let shouldBreak: any = false;
  let printedExp: any;
  let result: any;
  for (let exp of node.elements) {
    if (exp) {
      printedExp = printNode(exp, {
        ...options,
        noImplicitAny: false
      });
    } else {
      printedExp = empty;
    }
    printedExpressions.push(docValueString(printedExp));
    if (docShouldBreakP(printedExp)) {
      shouldBreak = true;
    }
  }
  if (shouldBreak) {
    result = ['[', line, join([',', line], printedExpressions.map(function (x: any): any {
      return align(1, x);
    })), line, ']'];
  } else {
    result = ['[', join([',', space], printedExpressions), ']'];
  }
  if (noImplicitAny && !type_) {
    type_ = new TSArrayType(new TSAnyKeyword());
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
function printArrayPattern(node: any, options: any = {}): any {
  return printArrayExpression(node, options);
}

printArrayPattern.lispSource = [Symbol.for('define'), [Symbol.for('print-array-pattern'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-array-expression'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print a `NewExpression` ESTree node to a `Doc` object.
 */
function printNewExpression(node: any, options: any = {}): any {
  return ['new', space, printNode(new CallExpression(node.callee, node.arguments), options)];
}

printNewExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-new-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'new', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('new'), Symbol.for('CallExpression'), [Symbol.for('get-field'), Symbol.for('callee'), Symbol.for('node')], [Symbol.for('get-field'), Symbol.for('arguments'), Symbol.for('node')]], Symbol.for('options')]]];

/**
 * Print an `ImportDeclaration` ESTree node to a `Doc` object.
 */
function printImportDeclaration(node: any, options: any = {}): any {
  const specifiers: any = node.specifiers;
  const source: any = node.source;
  if ((specifiers.length === 1) && !estreeTypeP(specifiers[0], 'ImportSpecifier')) {
    return ['import', space, printNode(specifiers[0], options), space, 'from', space, printNode(source, options), ';'];
  } else {
    return ['import', space, '{', line, indent(join([',', line], specifiers.map(function (x: any): any {
      return printNode(x, options);
    }))), line, '}', space, 'from', space, printNode(source, options), ';'];
  }
}

printImportDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-import-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('get-field'), Symbol.for('specifiers'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('source'), [Symbol.for('get-field'), Symbol.for('source'), Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('specifiers')], 1], [Symbol.for('not'), [Symbol.for('estree-type?'), [Symbol.for('first'), Symbol.for('specifiers')], 'ImportSpecifier']]], [Symbol.for('list'), 'import', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('first'), Symbol.for('specifiers')], Symbol.for('options')], Symbol.for('space'), 'from', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('source'), Symbol.for('options')], ';']], [Symbol.for('else'), [Symbol.for('list'), 'import', Symbol.for('space'), '{', Symbol.for('line'), [Symbol.for('~>'), Symbol.for('specifiers'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('line')], Symbol.for('_')], [Symbol.for('indent'), Symbol.for('_')]], Symbol.for('line'), '}', Symbol.for('space'), 'from', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('source'), Symbol.for('options')], ';']]]];

/**
 * Print an `ImportSpecifier` ESTree node to a `Doc` object.
 */
function printImportSpecifier(node: any, options: any = {}): any {
  const local: any = node.local;
  const localPrinted: any = printDoc(printNode(local, options), options);
  const imported: any = node.imported;
  const importedPrinted: any = printDoc(printNode(imported, options), options);
  if (localPrinted === importedPrinted) {
    return localPrinted;
  } else {
    return [localPrinted, space, 'as', space, importedPrinted];
  }
}

printImportSpecifier.lispSource = [Symbol.for('define'), [Symbol.for('print-import-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('local'), [Symbol.for('get-field'), Symbol.for('local'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('local-printed'), [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('local'), Symbol.for('options')], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('imported'), [Symbol.for('get-field'), Symbol.for('imported'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('imported-printed'), [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('imported'), Symbol.for('options')], Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('local-printed'), Symbol.for('imported-printed')], Symbol.for('local-printed')], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('local-printed'), Symbol.for('space'), 'as', Symbol.for('space'), Symbol.for('imported-printed')]]]];

/**
 * Print an `ImportDefaultSpecifier` ESTree node to a `Doc` object.
 */
function printImportDefaultSpecifier(node: any, options: any = {}): any {
  return printNode(node.local, options);
}

printImportDefaultSpecifier.lispSource = [Symbol.for('define'), [Symbol.for('print-import-default-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('local'), Symbol.for('node')], Symbol.for('options')]];

/**
 * Print an `ImportNamespaceSpecifier` ESTree node to a `Doc` object.
 */
function printImportNamespaceSpecifier(node: any, options: any = {}): any {
  return ['*', space, 'as', space, printNode(node.local, options)];
}

printImportNamespaceSpecifier.lispSource = [Symbol.for('define'), [Symbol.for('print-import-namespace-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), '*', Symbol.for('space'), 'as', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('local'), Symbol.for('node')], Symbol.for('options')]]];

/**
 * Print an `ExportNamedDeclaration` ESTree node to a `Doc` object.
 */
function printExportNamedDeclaration(node: any, options: any = {}): any {
  const specifiers: any = node.specifiers;
  const specifiersPrinted: any = printDoc(indent(join([',', line], specifiers.map(function (x: any): any {
    return printNode(x, options);
  }))), options);
  return ['export', space, '{', line, specifiersPrinted, (specifiersPrinted === '') ? empty : line, '}', ';'];
}

printExportNamedDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-export-named-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('get-field'), Symbol.for('specifiers'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('specifiers-printed'), [Symbol.for('~>'), Symbol.for('specifiers'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('line')], Symbol.for('_')], [Symbol.for('indent')], [Symbol.for('print-doc'), Symbol.for('options')]]], [Symbol.for('list'), 'export', Symbol.for('space'), '{', Symbol.for('line'), Symbol.for('specifiers-printed'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('specifiers-printed'), ''], Symbol.for('empty'), Symbol.for('line')], '}', ';']];

/**
 * Print an `ExportSpecifier` ESTree node to a `Doc` object.
 */
function printExportSpecifier(node: any, options: any = {}): any {
  return printImportSpecifier(new ImportSpecifier(node.local, node.exported), options);
}

printExportSpecifier.lispSource = [Symbol.for('define'), [Symbol.for('print-export-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-import-specifier'), [Symbol.for('new'), Symbol.for('ImportSpecifier'), [Symbol.for('get-field'), Symbol.for('local'), Symbol.for('node')], [Symbol.for('get-field'), Symbol.for('exported'), Symbol.for('node')]], Symbol.for('options')]];

/**
 * Print an `ExportAllDeclaration` ESTree node to a `Doc` object.
 */
function printExportAllDeclaration(node: any, options: any = {}): any {
  return ['export', space, '*', space, 'from', space, printNode(node.source, options), ';'];
}

printExportAllDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-export-all-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'export', Symbol.for('space'), '*', Symbol.for('space'), 'from', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('source'), Symbol.for('node')], Symbol.for('options')], ';']];

/**
 * Print an `ObjectExpression` ESTree node to a `Doc` object.
 */
function printObjectExpression(node: any, options: any = {}): any {
  const properties: any = node.properties;
  return ['{', (properties.length === 0) ? empty : [line, indent(join([',', line], properties.map(function (x: any): any {
    return printNode(x, options);
  }))), line], '}'];
}

printObjectExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-object-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('properties'), [Symbol.for('get-field'), Symbol.for('properties'), Symbol.for('node')]], [Symbol.for('list'), '{', [Symbol.for('if'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('properties')], 0], Symbol.for('empty'), [Symbol.for('list'), Symbol.for('line'), [Symbol.for('~>'), Symbol.for('properties'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('line')], Symbol.for('_')], [Symbol.for('indent')]], Symbol.for('line')]], '}']];

/**
 * Print an `ObjectPattern` ESTree node to a `Doc` object.
 */
function printObjectPattern(node: any, options: any = {}): any {
  return ['{', join([',', space], node.properties.map(function (prop: any): any {
    return printAssignmentProperty(prop, options);
  })), '}'];
}

printObjectPattern.lispSource = [Symbol.for('define'), [Symbol.for('print-object-pattern'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), '{', [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('properties'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('prop')], [Symbol.for('print-assignment-property'), Symbol.for('prop'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], '}']];

/**
 * Print an `AssignmentProperty` ESTree node to a `Doc` object.
 */
function printAssignmentProperty(node: any, options: any = {}): any {
  const options1: any = {
    ...options,
    noImplicitAny: false
  };
  const key: any = node.key;
  let keyPrinted: any = printNode(key, options1);
  const keyPrintedStr: any = printDoc(keyPrinted, options1);
  const value: any = node.value;
  const valuePrinted: any = printNode(value, options1);
  const valuePrintedStr: any = printDoc(valuePrinted, options1);
  if (keyPrintedStr === valuePrintedStr) {
    return keyPrinted;
  } else {
    return [keyPrinted, ':', space, valuePrinted];
  }
}

printAssignmentProperty.lispSource = [Symbol.for('define'), [Symbol.for('print-assignment-property'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('options1'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-field'), Symbol.for('key'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('key-printed'), [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options1')]], [Symbol.for('define'), Symbol.for('key-printed-str'), [Symbol.for('print-doc'), Symbol.for('key-printed'), Symbol.for('options1')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('value-printed'), [Symbol.for('print-node'), Symbol.for('value'), Symbol.for('options1')]], [Symbol.for('define'), Symbol.for('value-printed-str'), [Symbol.for('print-doc'), Symbol.for('value-printed'), Symbol.for('options1')]], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('key-printed-str'), Symbol.for('value-printed-str')], Symbol.for('key-printed'), [Symbol.for('list'), Symbol.for('key-printed'), ':', Symbol.for('space'), Symbol.for('value-printed')]]];

/**
 * Print a `Property` ESTree node to a `Doc` object.
 */
function printProperty(node: any, options: any = {}): any {
  const language: any = options['language'];
  const key: any = node.key;
  let keyPrinted: any = printNode(key, options);
  const value: any = node.value;
  const valuePrinted: any = printNode(value, options);
  if (node.computed) {
    keyPrinted = ['[', keyPrinted, (language === 'TypeScript') ? [space, 'as any'] : empty, ']'];
  }
  return [keyPrinted, ':', space, valuePrinted];
}

printProperty.lispSource = [Symbol.for('define'), [Symbol.for('print-property'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), 'language']], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-field'), Symbol.for('key'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('key-printed'), [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-field'), Symbol.for('value'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('value-printed'), [Symbol.for('print-node'), Symbol.for('value'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('get-field'), Symbol.for('computed'), Symbol.for('node')], [Symbol.for('set!'), Symbol.for('key-printed'), [Symbol.for('list'), '[', Symbol.for('key-printed'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language'), 'TypeScript'], [Symbol.for('list'), Symbol.for('space'), 'as any'], Symbol.for('empty')], ']']]], [Symbol.for('list'), Symbol.for('key-printed'), ':', Symbol.for('space'), Symbol.for('value-printed')]];

/**
 * Print a `Program` ESTree node to a `Doc` object.
 */
function printProgram(node: any, options: any = {}): any {
  return join([line, line], node.body.map(function (x: any): any {
    return printNode(x, options);
  }));
}

printProgram.lispSource = [Symbol.for('define'), [Symbol.for('print-program'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('body'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), Symbol.for('line'), Symbol.for('line')], Symbol.for('_')]]];

/**
 * Print a `SwitchStatement` ESTree node to a `Doc` object.
 */
function printSwitchStatement(node: any, options: any = {}): any {
  const discriminant: any = node.discriminant;
  const discriminantPrinted: any = printNode(discriminant, options);
  const cases: any = node.cases;
  const casesPrinted: any = indent(join(line, cases.map(function (x: any): any {
    return printNode(x, options);
  })));
  return ['switch', space, '(', discriminantPrinted, ')', space, '{', line, casesPrinted, line, '}'];
}

printSwitchStatement.lispSource = [Symbol.for('define'), [Symbol.for('print-switch-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('discriminant'), [Symbol.for('get-field'), Symbol.for('discriminant'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('discriminant-printed'), [Symbol.for('print-node'), Symbol.for('discriminant'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('cases'), [Symbol.for('get-field'), Symbol.for('cases'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('cases-printed'), [Symbol.for('~>'), Symbol.for('cases'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), Symbol.for('line'), Symbol.for('_')], [Symbol.for('indent'), Symbol.for('_')]]], [Symbol.for('list'), 'switch', Symbol.for('space'), '(', Symbol.for('discriminant-printed'), ')', Symbol.for('space'), '{', Symbol.for('line'), Symbol.for('cases-printed'), Symbol.for('line'), '}']];

/**
 * Print a `SwitchCase` ESTree node to a `Doc` object.
 */
function printSwitchCase(node: any, options: any = {}): any {
  const test: any = node.test;
  let testPrinted: any = test ? ['case', space, printNode(test, options)] : 'default';
  const consequent: any = node.consequent;
  const isBlockStatement: any = (consequent.length === 1) && consequent[0] && estreeTypeP(consequent[0], 'BlockStatement');
  let consequentPrinted: any = consequent.map(function (x: any): any {
    return printNode(x, options);
  });
  if (isBlockStatement) {
    consequentPrinted = [space, consequentPrinted[0]];
  } else {
    consequentPrinted = [line, indent(join(line, consequentPrinted))];
  }
  return [testPrinted, ':', consequentPrinted];
}

printSwitchCase.lispSource = [Symbol.for('define'), [Symbol.for('print-switch-case'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-field'), Symbol.for('test'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('cond'), [Symbol.for('test'), [Symbol.for('list'), 'case', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]]], [Symbol.for('else'), 'default']]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('get-field'), Symbol.for('consequent'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('is-block-statement'), [Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('consequent')], 1], [Symbol.for('first'), Symbol.for('consequent')], [Symbol.for('estree-type?'), [Symbol.for('first'), Symbol.for('consequent')], 'BlockStatement']]], [Symbol.for('define'), Symbol.for('consequent-printed'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('consequent')]], [Symbol.for('cond'), [Symbol.for('is-block-statement'), [Symbol.for('set!'), Symbol.for('consequent-printed'), [Symbol.for('list'), Symbol.for('space'), [Symbol.for('first'), Symbol.for('consequent-printed')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('consequent-printed'), [Symbol.for('list'), Symbol.for('line'), [Symbol.for('~>'), Symbol.for('consequent-printed'), [Symbol.for('join'), Symbol.for('line'), Symbol.for('_')], [Symbol.for('indent'), Symbol.for('_')]]]]]], [Symbol.for('list'), Symbol.for('test-printed'), ':', Symbol.for('consequent-printed')]];

/**
 * Print a `TSAsExpression` TSESTree node to a `Doc` object.
 */
function printTsAsExpression(node: any, options: any = {}): any {
  const expression: any = node.expression;
  const expressionPrinted: any = printNode(expression, options);
  const typeAnnotation: any = node.typeAnnotation;
  const typeAnnotationPrinted: any = printTsType(typeAnnotation, options);
  return [expressionPrinted, space, 'as', space, typeAnnotationPrinted];
}

printTsAsExpression.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-as-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('expression'), [Symbol.for('get-field'), Symbol.for('expression'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('expression-printed'), [Symbol.for('print-node'), Symbol.for('expression'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('type-annotation'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('type-annotation-printed'), [Symbol.for('print-ts-type'), Symbol.for('type-annotation'), Symbol.for('options')]], [Symbol.for('list'), Symbol.for('expression-printed'), Symbol.for('space'), 'as', Symbol.for('space'), Symbol.for('type-annotation-printed')]];

/**
 * Print TSESTree type to a `Doc` object.
 */
function printTsType(node: any, options: any = {}): any {
  let type_: any = estreeType(node);
  if (printerMap.has(type_)) {
    return printNode(node, options);
  } else {
    return 'any';
  }
}

printTsType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('estree-type'), Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('printer-map'), Symbol.for('type_')], [Symbol.for('print-node'), Symbol.for('node'), Symbol.for('options')]], [Symbol.for('else'), 'any']]];

/**
 * Print a `TSAnyKeyword` TSESTree node to a `Doc` object.
 */
function printTsAnyKeyword(node: any, options: any = {}): any {
  return 'any';
}

printTsAnyKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-any-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'any'];

/**
 * Print a `TSVoidKeyword` TSESTree node to a `Doc` object.
 */
function printTsVoidKeyword(node: any, options: any = {}): any {
  return 'void';
}

printTsVoidKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-void-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'void'];

/**
 * Print a `TSUndefinedKeyword` TSESTree node to a `Doc` object.
 */
function printTsUndefinedKeyword(node: any, options: any = {}): any {
  return 'undefined';
}

printTsUndefinedKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-undefined-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'undefined'];

/**
 * Print a `TSBooleanKeyword` TSESTree node to a `Doc` object.
 */
function printTsBooleanKeyword(node: any, options: any = {}): any {
  return 'boolean';
}

printTsBooleanKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-boolean-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'boolean'];

/**
 * Print a `TSNumberKeyword` TSESTree node to a `Doc` object.
 */
function printTsNumberKeyword(node: any, options: any = {}): any {
  return 'number';
}

printTsNumberKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-number-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'number'];

/**
 * Print a `TSStringKeyword` TSESTree node to a `Doc` object.
 */
function printTsStringKeyword(node: any, options: any = {}): any {
  return 'string';
}

printTsStringKeyword.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-string-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], 'string'];

/**
 * Print a `TSArrayType` TSESTree node to a `Doc` object.
 */
function printTsArrayType(node: any, options: any = {}): any {
  const elementType: any = node.elementType;
  let result: any = printNode(elementType, options);
  if (estreeTypeP(elementType, 'TSUnionType')) {
    result = docWrap(result, options);
  }
  return [result, '[]'];
}

printTsArrayType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-array-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('element-type'), [Symbol.for('get-field'), Symbol.for('elementType'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('print-node'), Symbol.for('element-type'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('element-type'), 'TSUnionType'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('doc-wrap'), Symbol.for('result'), Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('result'), '[]']];

/**
 * Print a `TSTupleType` TSESTree node to a `Doc` object.
 */
function printTsTupleType(node: any, options: any = {}): any {
  const elementTypes: any = node.elementTypes;
  return ['[', join([',', space], elementTypes.map(function (x: any): any {
    return printNode(x, options);
  })), ']'];
}

printTsTupleType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-tuple-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('element-types'), [Symbol.for('get-field'), Symbol.for('elementTypes'), Symbol.for('node')]], [Symbol.for('list'), '[', [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('element-types')]], ']']];

/**
 * Print a `TSUnionType` TSESTree node to a `Doc` object.
 */
function printTsUnionType(node: any, options: any = {}): any {
  return join([space, '|', space], node.types.map(function (x: any): any {
    let result: any = printNode(x, options);
    if (estreeTypeP(x, 'TSUnionType')) {
      result = docWrap(result, options);
    }
    return result;
  }));
}

printTsUnionType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-union-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('types'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('x'), 'TSUnionType'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('doc-wrap'), Symbol.for('result'), Symbol.for('options')]]], Symbol.for('result')], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), Symbol.for('space'), '|', Symbol.for('space')], Symbol.for('_')]]];

/**
 * Print a `TSFunctionType` TSESTree node to a `Doc` object.
 */
function printTsFunctionType(node: any, options: any = {}): any {
  return ['(', join([',', space], node.params.map(function (x: any): any {
    return printNode(x, options);
  })), ')', space, '=>', space, printNode(node.returnType, options)];
}

printTsFunctionType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-function-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), '(', [Symbol.for('~>'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], ')', Symbol.for('space'), '=>', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('returnType'), Symbol.for('node')], Symbol.for('options')]]];

/**
 * Print a `TSTypeAliasDeclaration` TSESTree node to a `Doc` object.
 */
function printTsTypeAliasDeclaration(node: any, options: any = {}): any {
  return ['type', space, printNode(node.id, options), space, '=', space, printNode(node.typeAnnotation, options), ';'];
}

printTsTypeAliasDeclaration.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-type-alias-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('list'), 'type', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('id'), Symbol.for('node')], Symbol.for('options')], Symbol.for('space'), '=', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('node')], Symbol.for('options')], ';']];

/**
 * Print a `TSTypeAnnotation` TSESTree node to a `Doc` object.
 */
function printTsTypeAnnotation(node: any, options: any = {}): any {
  return printNode(node.typeAnnotation, options);
}

printTsTypeAnnotation.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-type-annotation'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('typeAnnotation'), Symbol.for('node')], Symbol.for('options')]];

/**
 * Print a `TSLiteralType` TSESTree node to a `Doc` object.
 */
function printTsLiteralType(node: any, options: any = {}): any {
  return printNode(node.literal, {
    ...options,
    noImplicitAny: false
  });
}

printTsLiteralType.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-literal-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('print-node'), [Symbol.for('get-field'), Symbol.for('literal'), Symbol.for('node')], [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]]];

/**
 * Print a `TSTypeReference` TSESTree node to a `Doc` object.
 */
function printTsTypeReference(node: any, options: any = {}): any {
  const name: any = node.typeName;
  const params: any = node.typeParameters;
  return [printNode(name, {
    ...options,
    noImplicitAny: false
  }), params ? printNode(params, {
    ...options,
    noImplicitAny: false
  }) : empty];
}

printTsTypeReference.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-type-reference'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('get-field'), Symbol.for('typeName'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('get-field'), Symbol.for('typeParameters'), Symbol.for('node')]], [Symbol.for('list'), [Symbol.for('print-node'), Symbol.for('name'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]], [Symbol.for('if'), Symbol.for('params'), [Symbol.for('print-node'), Symbol.for('params'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]], Symbol.for('empty')]]];

/**
 * Print a `TSTypeParameterInstantiation` TSESTree node to a `Doc` object.
 */
function printTsTypeParameterInstantiation(node: any, options: any = {}): any {
  const params: any = node.params;
  return ['<', join(',', params.map(function (x: any): any {
    return printTsType(x, {
      ...options,
      noImplicitAny: false
    });
  })), '>'];
}

printTsTypeParameterInstantiation.lispSource = [Symbol.for('define'), [Symbol.for('print-ts-type-parameter-instantiation'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('get-field'), Symbol.for('params'), Symbol.for('node')]], [Symbol.for('list'), '<', [Symbol.for('~>'), Symbol.for('params'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-ts-type'), Symbol.for('x'), [Symbol.for('js-obj-append'), Symbol.for('options'), [Symbol.for('js-obj'), 'noImplicitAny', Symbol.for('#f')]]]], Symbol.for('_')], [Symbol.for('join'), ',', Symbol.for('_')]], '>']];

/**
 * Print an `XRawJavaScript` ESTree extension node to a `Doc` object.
 */
function printXRawJavascript(node: any, options: any = {}): any {
  let str: any = node.js;
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
function defaultPrinter(node: any, options: any = {}): any {
  return empty;
}

defaultPrinter.lispSource = [Symbol.for('define'), [Symbol.for('default-printer'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js-obj')]]], Symbol.for('empty')];

/**
 * Mapping from node types to printer functions.
 */
const printerMap: any = new Map([['ArrayExpression', printArrayExpression], ['ArrayPattern', printArrayPattern], ['ArrowFunctionExpression', printArrowFunctionExpression], ['AssignmentExpression', printAssignmentExpression], ['AssignmentPattern', printAssignmentPattern], ['AwaitExpression', printAwaitExpression], ['BinaryExpression', printBinaryExpression], ['BlockStatement', printBlockStatement], ['BreakStatement', printBreakStatement], ['CallExpression', printCallExpression], ['ClassBody', printClassBody], ['ClassDeclaration', printClassDeclaration], ['ClassExpression', printClassExpression], ['ConditionalExpression', printConditionalExpression], ['ContinueStatement', printContinueStatement], ['DoWhileStatement', printDoWhileStatement], ['ExportAllDeclaration', printExportAllDeclaration], ['ExportNamedDeclaration', printExportNamedDeclaration], ['ExportSpecifier', printExportSpecifier], ['ExpressionStatement', printExpressionStatement], ['ForInStatement', printForInStatement], ['ForOfStatement', printForOfStatement], ['ForStatement', printForStatement], ['FunctionDeclaration', printFunctionDeclaration], ['FunctionExpression', printFunctionExpression], ['Identifier', printIdentifier], ['IfStatement', printIfStatement], ['ImportDeclaration', printImportDeclaration], ['ImportDefaultSpecifier', printImportDefaultSpecifier], ['ImportNamespaceSpecifier', printImportNamespaceSpecifier], ['ImportSpecifier', printImportSpecifier], ['Literal', printLiteral], ['LogicalExpression', printLogicalExpression], ['MemberExpression', printMemberExpression], ['MethodDefinition', printMethodDefinition], ['NewExpression', printNewExpression], ['ObjectExpression', printObjectExpression], ['ObjectPattern', printObjectPattern], ['Program', printProgram], ['Property', printProperty], ['PropertyDefinition', printPropertyDefinition], ['RestElement', printRestElement], ['ReturnStatement', printReturnStatement], ['SequenceExpression', printSequenceExpression], ['SpreadElement', printSpreadElement], ['SwitchStatement', printSwitchStatement], ['SwitchCase', printSwitchCase], ['TSAnyKeyword', printTsAnyKeyword], ['TSArrayType', printTsArrayType], ['TSAsExpression', printTsAsExpression], ['TSBooleanKeyword', printTsBooleanKeyword], ['TSFunctionType', printTsFunctionType], ['TSLiteralType', printTsLiteralType], ['TSNumberKeyword', printTsNumberKeyword], ['TSStringKeyword', printTsStringKeyword], ['TSTupleType', printTsTupleType], ['TSTypeAliasDeclaration', printTsTypeAliasDeclaration], ['TSTypeAnnotation', printTsTypeAnnotation], ['TSTypeParameterInstantiation', printTsTypeParameterInstantiation], ['TSTypeReference', printTsTypeReference], ['TSUndefinedKeyword', printTsUndefinedKeyword], ['TSUnionType', printTsUnionType], ['TSVoidKeyword', printTsVoidKeyword], ['TaggedTemplateExpression', printTaggedTemplateExpression], ['TemplateElement', printTemplateElement], ['TemplateLiteral', printTemplateLiteral], ['ThisExpression', printThisExpression], ['ThrowStatement', printThrowStatement], ['TryStatement', printTryStatement], ['UnaryExpression', printUnaryExpression], ['UpdateExpression', printUpdateExpression], ['VariableDeclaration', printVariableDeclaration], ['VariableDeclarator', printVariableDeclarator], ['WhileStatement', printWhileStatement], ['YieldExpression', printYieldExpression], ['XRawJavaScript', printXRawJavascript]] as any);

export {
  printNode as printEstreeNode,
  printSexpAsExpression as printAsExpression,
  print,
  printEstree,
  printNode,
  printRose,
  printSexp,
  printSexpAsExpression,
  writeToString
};