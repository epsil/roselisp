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
  estreeType,
  getEstreeField
} from './estree';

import {
  syntaxToDatum,
  syntaxp
} from './rose';

import {
  makeVisitor,
  visit
} from './visitor';

import {
  force,
  thunkp
} from './thunk';

const [length, findf, symbolp, booleanp, undefinedp, jsNullP, stringp, procedurep, arrayp, take, lastCdr]: any[] = ((): any => {
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
  function undefinedp_(obj: any): any {
    return obj === undefined;
  }
  function jsNullP_(obj: any): any {
    return obj === null;
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
    return lst.slice(0, -(lst.length - n));
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
  return [length_, findf_, symbolp_, booleanp_, undefinedp_, jsNullP_, stringp_, procedurep_, arrayp_, take_, lastCdr_];
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

align.fsource = [Symbol.for('define'), [Symbol.for('align'), Symbol.for('offset'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'align', Symbol.for('offset'), Symbol.for('doc'), Symbol.for('options')]];

/**
 * `Doc` command `indent`.
 */
function indent(doc: any, options: any = {}): any {
  return new DocCommand('indent', doc, options);
}

indent.fsource = [Symbol.for('define'), [Symbol.for('indent'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'indent', Symbol.for('doc'), Symbol.for('options')]];

/**
 * `Doc` command `noindent`.
 */
function noindent(doc: any, options: any = {}): any {
  return new DocCommand('noindent', doc, options);
}

noindent.fsource = [Symbol.for('define'), [Symbol.for('noindent'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'noindent', Symbol.for('doc'), Symbol.for('options')]];

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

join.fsource = [Symbol.for('define'), [Symbol.for('join'), Symbol.for('sep'), Symbol.for('docs')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('js/length'), Symbol.for('docs')]]]], [Symbol.for('unless'), [Symbol.for('='), Symbol.for('i'), 0], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('sep')]], [Symbol.for('push-right!'), Symbol.for('result'), [Symbol.for('aget'), Symbol.for('docs'), Symbol.for('i')]]], Symbol.for('result')];

/**
 * `Doc` command `group`.
 *
 * Makes a document group.
 */
function group(doc: any, options: any = {}): any {
  return new DocCommand('group', doc, options);
}

group.fsource = [Symbol.for('define'), [Symbol.for('group'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('new'), Symbol.for('DocCommand'), 'group', Symbol.for('doc'), Symbol.for('options')]];

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

docType.fsource = [Symbol.for('define'), [Symbol.for('doc-type'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('string?'), Symbol.for('doc')], 'string'], [[Symbol.for('array?'), Symbol.for('doc')], 'array'], [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('get-field'), Symbol.for('type'), Symbol.for('doc')]], [Symbol.for('else'), 'undefined']]];

/**
 * Whether the type of a `Doc` object is `typ`.
 */
function docTypeP(doc: any, typ: any): any {
  return docType(doc) === typ;
}

docTypeP.fsource = [Symbol.for('define'), [Symbol.for('doc-type?'), Symbol.for('doc'), Symbol.for('typ')], [Symbol.for('eq?'), [Symbol.for('doc-type'), Symbol.for('doc')], Symbol.for('typ')]];

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

docValue.fsource = [Symbol.for('define'), [Symbol.for('doc-value'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('first'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]]], [Symbol.for('else'), Symbol.for('doc')]]];

/**
 * Unwrap a `Doc` command and print it to a string.
 */
function docValueString(doc: any): any {
  return printDoc(docValue(doc));
}

docValueString.fsource = [Symbol.for('define'), [Symbol.for('doc-value-string'), Symbol.for('doc')], [Symbol.for('print-doc'), [Symbol.for('doc-value'), Symbol.for('doc')]]];

/**
 * Whether a `Doc` object should break across multiple lines.
 */
function docShouldBreakP(doc: any): any {
  if (doc instanceof DocCommand) {
    return ((): any => {
      const arr: any = doc.args;
      return arr[arr.length - 1];
    })()['shouldBreak'];
  } else {
    return false;
  }
}

docShouldBreakP.fsource = [Symbol.for('define'), [Symbol.for('doc-should-break?'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('oget'), [Symbol.for('js/last'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], Symbol.for(':should-break')]], [Symbol.for('else'), false]]];

/**
 * Whether a `Doc` object contains any comments.
 */
function docHasCommentsP(doc: any): any {
  if (doc instanceof DocCommand) {
    return ((): any => {
      const arr: any = doc.args;
      return arr[arr.length - 1];
    })()['hasComments'];
  } else {
    return false;
  }
}

docHasCommentsP.fsource = [Symbol.for('define'), [Symbol.for('doc-has-comments?'), Symbol.for('doc')], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('doc'), Symbol.for('DocCommand')], [Symbol.for('oget'), [Symbol.for('js/last'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], Symbol.for(':has-comments')]], [Symbol.for('else'), false]]];

/**
 * Wrap a `Doc` object in a pair of parentheses.
 */
function docWrap(doc: any, options: any = {}, settings: any = {}): any {
  const open: any = settings['open'] || '(';
  const close: any = settings['close'] || ')';
  const offset: any = open.length;
  if (options['hasComments'] || docHasCommentsP(doc)) {
    return printDoc([open, line, align(offset, doc), line, close], options);
  } else {
    return printDoc([open, doc, close]);
  }
}

docWrap.fsource = [Symbol.for('define'), [Symbol.for('doc-wrap'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js/obj')]], [Symbol.for('settings'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('open'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('settings'), Symbol.for(':open')], '(']], [Symbol.for('define'), Symbol.for('close'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('settings'), Symbol.for(':close')], ')']], [Symbol.for('define'), Symbol.for('offset'), [Symbol.for('string-length'), Symbol.for('open')]], [Symbol.for('cond'), [[Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':has-comments')], [Symbol.for('doc-has-comments?'), Symbol.for('doc')]], [Symbol.for('print-doc'), [Symbol.for('list'), Symbol.for('open'), Symbol.for('line'), [Symbol.for('align'), Symbol.for('offset'), Symbol.for('doc')], Symbol.for('line'), Symbol.for('close')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('print-doc'), [Symbol.for('list'), Symbol.for('open'), Symbol.for('doc'), Symbol.for('close')]]]]];

/**
 * Print comments of an ESTree node and attach them
 * to a `Doc` object.
 */
function attachComments(result: any, node: any, options: any = {}): any {
  const commentsOption: any = options['comments'];
  const comments: any = getEstreeField('comments', node);
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
    shouldBreak: true,
    hasComments: true
  });
}

attachComments.fsource = [Symbol.for('define'), [Symbol.for('attach-comments'), Symbol.for('result'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('comments-option'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':comments')]], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('get-estree-field'), 'comments', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('code'), [Symbol.for('doc-value-string'), Symbol.for('result')]], [Symbol.for('define'), Symbol.for('leading-comments'), ''], [Symbol.for('define'), Symbol.for('trailing-comments'), ''], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('comments-option'), false], [Symbol.for('not'), Symbol.for('comments')], [Symbol.for('='), [Symbol.for('js/length'), Symbol.for('comments')], 0]], [Symbol.for('return'), Symbol.for('result')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('length'), Symbol.for('comments')]]]], [Symbol.for('define'), Symbol.for('comment'), [Symbol.for('aget'), Symbol.for('comments'), Symbol.for('i')]], [Symbol.for('cond'), [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('BlockComment')], [Symbol.for('define'), Symbol.for('block-comment'), [Symbol.for('make-block-comment'), [Symbol.for('get-field'), Symbol.for('original-text'), Symbol.for('comment')]]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('='), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('js/length'), Symbol.for('comments')], 1]], [Symbol.for('eq?'), Symbol.for('code'), '']], [Symbol.for('set!'), Symbol.for('block-comment'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\n*$'], Symbol.for('block-comment'), '']]], [Symbol.for('set!'), Symbol.for('leading-comments'), [Symbol.for('string-append'), Symbol.for('leading-comments'), Symbol.for('block-comment'), [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('code'), ''], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n*$'], Symbol.for('block-comment')]], Symbol.for('empty'), Symbol.for('line')]]]], [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('LeadingComment')], [Symbol.for('define'), Symbol.for('leading-comment'), [Symbol.for('make-line-comment'), [Symbol.for('get-field'), Symbol.for('original-text'), Symbol.for('comment')]]], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('='), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('js/length'), Symbol.for('comments')], 1]], [Symbol.for('eq?'), Symbol.for('code'), '']], [Symbol.for('set!'), Symbol.for('leading-comment'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\n$'], Symbol.for('leading-comment'), '']]], [Symbol.for('set!'), Symbol.for('leading-comments'), [Symbol.for('string-append'), Symbol.for('leading-comments'), Symbol.for('leading-comment'), [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('code'), ''], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n$'], Symbol.for('leading-comment')]], Symbol.for('empty'), Symbol.for('line')]]]], [[Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('TrailingComment')], [Symbol.for('define'), Symbol.for('trailing-comment'), [Symbol.for('make-line-comment'), [Symbol.for('get-field'), Symbol.for('original-text'), Symbol.for('comment')]]], [Symbol.for('set!'), Symbol.for('trailing-comments'), [Symbol.for('string-append'), Symbol.for('trailing-comments'), Symbol.for('space'), Symbol.for('trailing-comment')]]]]], [Symbol.for('group'), [Symbol.for('list'), Symbol.for('leading-comments'), Symbol.for('code'), Symbol.for('trailing-comments')], [Symbol.for('js/obj'), Symbol.for(':should-break'), true, Symbol.for(':has-comments'), true]]];

/**
 * Make a line comment.
 */
function makeLineComment(text: any): any {
  const [, content, trailingNewlines]: any[] = text.match(new RegExp('^([\\s\\S]*?)([\\n]*)$'));
  return content.split('\n').map(function (x: any): any {
    return x.replace(new RegExp('^'), (x === '') ? '//' : '// ');
  }).join('\n') + trailingNewlines;
}

makeLineComment.fsource = [Symbol.for('define'), [Symbol.for('make-line-comment'), Symbol.for('text')], [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('content'), Symbol.for('trailing-newlines')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^([\\s\\S]*?)([\\n]*)$'], Symbol.for('text')]], [Symbol.for('string-append'), [Symbol.for('~>'), Symbol.for('content'), [Symbol.for('string-split'), '\n'], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^'], Symbol.for('x'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('x'), ''], '//', '// ']]], Symbol.for('_')], [Symbol.for('string-join'), '\n']], Symbol.for('trailing-newlines')]];

/**
 * Make a block comment.
 */
function makeBlockComment(text: any): any {
  const [, content, trailingNewlines]: any[] = text.match(new RegExp('^([\\s\\S]*?)([\\n]*)$'));
  return '/**' + line + content.split('\n').map(function (x: any): any {
    return x.replace(new RegExp('^'), (x === '') ? ' *' : ' * ');
  }).join('\n') + line + ' */' + trailingNewlines;
}

makeBlockComment.fsource = [Symbol.for('define'), [Symbol.for('make-block-comment'), Symbol.for('text')], [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('content'), Symbol.for('trailing-newlines')], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^([\\s\\S]*?)([\\n]*)$'], Symbol.for('text')]], [Symbol.for('string-append'), '/**', Symbol.for('line'), [Symbol.for('~>'), Symbol.for('content'), [Symbol.for('string-split'), '\n'], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^'], Symbol.for('x'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('x'), ''], ' *', ' * ']]], Symbol.for('_')], [Symbol.for('string-join'), '\n']], Symbol.for('line'), ' */', Symbol.for('trailing-newlines')]];

/**
 * Whether an expression is "simple", i.e., does not
 * need to be wrapped in parentheses when printed.
 */
function estreeSimpleP(exp: any): any {
  return ['Literal', 'Identifier', 'ThisExpression', 'CallExpression', 'NewExpression', 'UnaryExpression', 'ArrayExpression', 'ObjectExpression', 'MemberExpression'].includes(estreeType(exp));
}

estreeSimpleP.fsource = [Symbol.for('define'), [Symbol.for('estree-simple?'), Symbol.for('exp')], [Symbol.for('memq?'), [Symbol.for('estree-type'), Symbol.for('exp')], [Symbol.for('quote'), ['Literal', 'Identifier', 'ThisExpression', 'CallExpression', 'NewExpression', 'UnaryExpression', 'ArrayExpression', 'ObjectExpression', 'MemberExpression']]]];

/**
 * Whether an expression is "complex", i.e., needs
 * to be wrapped in parentheses when printed.
 */
function estreeComplexP(exp: any): any {
  return ['FunctionExpression', 'ArrowFunctionExpression', 'FunctionDeclaration', 'TSAsExpression'].includes(estreeType(exp));
}

estreeComplexP.fsource = [Symbol.for('define'), [Symbol.for('estree-complex?'), Symbol.for('exp')], [Symbol.for('memq?'), [Symbol.for('estree-type'), Symbol.for('exp')], [Symbol.for('quote'), ['FunctionExpression', 'ArrowFunctionExpression', 'FunctionDeclaration', 'TSAsExpression']]]];

/**
 * Whether an expression is a string literal.
 */
function estreeStringLiteralP(exp: any): any {
  return estreeTypeP(exp, 'Literal') && (typeof getEstreeField('value', exp) === 'string');
}

estreeStringLiteralP.fsource = [Symbol.for('define'), [Symbol.for('estree-string-literal?'), Symbol.for('exp')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('exp'), 'Literal'], [Symbol.for('string?'), [Symbol.for('get-estree-field'), 'value', Symbol.for('exp')]]]];

/**
 * Whether an ESTree node has any comments.
 */
function estreeHasCommentsP(node: any): any {
  return getEstreeField('comments', node).length > 0;
}

estreeHasCommentsP.fsource = [Symbol.for('define'), [Symbol.for('estree-has-comments?'), Symbol.for('node')], [Symbol.for('>'), [Symbol.for('js/length'), [Symbol.for('get-estree-field'), 'comments', Symbol.for('node')]], 0]];

/**
 * Whether an ESTree node has any block comments.
 */
function estreeHasBlockCommentP(node: any): any {
  return findf(function (comment: any): any {
    return comment instanceof BlockComment;
  }, getEstreeField('comments', node));
}

estreeHasBlockCommentP.fsource = [Symbol.for('define'), [Symbol.for('estree-has-block-comment?'), Symbol.for('node')], [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('comment')], [Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('BlockComment')]], [Symbol.for('get-estree-field'), 'comments', Symbol.for('node')]]];

/**
 * Whether an ESTree node has any leading comments.
 */
function estreeHasLeadingCommentP(node: any): any {
  return findf(function (comment: any): any {
    return comment instanceof LeadingComment;
  }, getEstreeField('comments', node));
}

estreeHasLeadingCommentP.fsource = [Symbol.for('define'), [Symbol.for('estree-has-leading-comment?'), Symbol.for('node')], [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('comment')], [Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('LeadingComment')]], [Symbol.for('get-estree-field'), 'comments', Symbol.for('node')]]];

/**
 * Whether an ESTree node has any trailing comments.
 */
function estreeHasTrailingCommentP(node: any): any {
  return findf(function (comment: any): any {
    return comment instanceof TrailingComment;
  }, getEstreeField('comments', node));
}

estreeHasTrailingCommentP.fsource = [Symbol.for('define'), [Symbol.for('estree-has-trailing-comment?'), Symbol.for('node')], [Symbol.for('findf'), [Symbol.for('lambda'), [Symbol.for('comment')], [Symbol.for('is-a?'), Symbol.for('comment'), Symbol.for('TrailingComment')]], [Symbol.for('get-estree-field'), 'comments', Symbol.for('node')]]];

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

print.fsource = [Symbol.for('define'), [Symbol.for('print'), Symbol.for('obj'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('cond'), [[Symbol.for('estree?'), Symbol.for('obj')], [Symbol.for('print-estree'), Symbol.for('obj'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('print-sexp'), Symbol.for('obj'), Symbol.for('options')]]]];

/**
 * Print an ESTree node.
 */
function printEstree(node: any, options: any = {}): any {
  return printToString(node, addDefaultOptions(options));
}

printEstree.fsource = [Symbol.for('define'), [Symbol.for('print-estree'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-to-string'), Symbol.for('node'), [Symbol.for('add-default-options'), Symbol.for('options')]]];

/**
 * Print a syntax object.
 */
function printSyntax(node: any, options: any = {}): any {
  // TODO: Print comments.
  return printSexp(syntaxToDatum(node), options);
}

printSyntax.fsource = [Symbol.for('define'), [Symbol.for('print-syntax'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('~>'), [Symbol.for('syntax->datum'), Symbol.for('node')], [Symbol.for('print-sexp'), Symbol.for('_'), Symbol.for('options')]]];

/**
 * Print an S-expression.
 */
function printSexp(exp: any, options: any = {}): any {
  return writeToString(exp, options);
}

printSexp.fsource = [Symbol.for('define'), [Symbol.for('print-sexp'), Symbol.for('exp'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('write-to-string'), Symbol.for('exp'), Symbol.for('options')]];

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

printSexpAsExpression.fsource = [Symbol.for('define'), [Symbol.for('print-sexp-as-expression'), Symbol.for('exp'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-sexp'), Symbol.for('exp'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':quote-toplevel'), true]]]];

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

writeToString.fsource = [Symbol.for('define'), [Symbol.for('write-to-string'), Symbol.for('obj'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('write-to-doc'), Symbol.for('obj'), Symbol.for('options')]], [Symbol.for('unless'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':doc')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('print-doc'), Symbol.for('result'), Symbol.for('options')]]], Symbol.for('result')];

/**
 * Print an S-expression to a `Doc` object.
 */
function writeToDoc(obj: any, options: any = {}): any {
  const docOption: any = options['doc'];
  const prettyOption: any = options['pretty'];
  const quoteToplevelOption: any = options['quoteToplevel'];
  const visitor: any = makeVisitor([[syntaxp, function (obj: any): any {
    return writeToDoc(syntaxToDatum(obj), options);
  }], [symbolp, function (obj: any): any {
    return [quoteToplevelOption ? '\'' : empty, (obj === Symbol.for('.')) ? '.' : (obj.description as string)];
  }], [booleanp, function (obj: any): any {
    if (obj) {
      return '#t';
    } else {
      return '#f';
    }
  }], [undefinedp, function (obj: any): any {
    return '#u';
  }], [jsNullP, function (obj: any): any {
    return '#n';
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

writeToDoc.fsource = [Symbol.for('define'), [Symbol.for('write-to-doc'), Symbol.for('obj'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('doc-option'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':doc')]], [Symbol.for('define'), Symbol.for('pretty-option'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':pretty')]], [Symbol.for('define'), Symbol.for('quote-toplevel-option'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':quote-toplevel')]], [Symbol.for('define'), Symbol.for('visitor'), [Symbol.for('make-visitor'), [Symbol.for('quasiquote'), [[[Symbol.for('unquote'), Symbol.for('syntax?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('write-to-doc'), [Symbol.for('syntax->datum'), Symbol.for('obj')], Symbol.for('options')]]]], [[Symbol.for('unquote'), Symbol.for('symbol?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('list'), [Symbol.for('if'), Symbol.for('quote-toplevel-option'), '\'', Symbol.for('empty')], [Symbol.for('if'), [Symbol.for('cons-dot?'), Symbol.for('obj')], '.', [Symbol.for('symbol->string'), Symbol.for('obj')]]]]]], [[Symbol.for('unquote'), Symbol.for('boolean?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('if'), Symbol.for('obj'), '#t', '#f']]]], [[Symbol.for('unquote'), Symbol.for('undefined?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], '#u']]], [[Symbol.for('unquote'), Symbol.for('js/null?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], '#n']]], [[Symbol.for('unquote'), Symbol.for('string?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('list'), '"', [Symbol.for('~>'), Symbol.for('obj'), [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\\\', 'g'], Symbol.for('_'), '\\\\'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '"', 'g'], Symbol.for('_'), '\\"'], [Symbol.for('string-split'), Symbol.for('line')], [Symbol.for('join'), Symbol.for('literalline'), Symbol.for('_')]], '"']]]], [[Symbol.for('unquote'), Symbol.for('procedure?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], '#<procedure>']]], [[Symbol.for('unquote'), Symbol.for('array?')], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('obj')]], [Symbol.for('define'), Symbol.for('spec'), [Symbol.for('and'), Symbol.for('pretty-option'), [Symbol.for('send'), Symbol.for('pretty-print-map'), Symbol.for('get'), Symbol.for('op')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('cond'), [[Symbol.for('procedure?'), Symbol.for('spec')], [Symbol.for('spec'), Symbol.for('obj'), Symbol.for('options')]], [[Symbol.for('number?'), Symbol.for('spec')], [Symbol.for('pretty-print-with-offset'), Symbol.for('spec'), Symbol.for('obj'), Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('pretty-print-form'), Symbol.for('obj'), Symbol.for('options')]]]], [Symbol.for('when'), Symbol.for('quote-toplevel-option'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '\'', Symbol.for('result')]]], Symbol.for('result')]]], [[Symbol.for('unquote'), [Symbol.for('const'), true]], [Symbol.for('unquote'), [Symbol.for('lambda'), [Symbol.for('obj')], [Symbol.for('string-append'), Symbol.for('obj'), '']]]]]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('visit'), Symbol.for('visitor'), Symbol.for('obj')]], Symbol.for('result')];

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

prettyPrintForm.fsource = [Symbol.for('define'), [Symbol.for('pretty-print-form'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('list'), '(', [Symbol.for('join'), ' ', [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':quote-toplevel'), false]]]], Symbol.for('form')]], ')']];

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
  const elements2: any = elements.slice(offset + 1);
  let result: any = [join(space, elements1), (elements2.length > 0) ? [line, indent(join(line, elements2))] : empty];
  result = ['(', result, ')'];
  return result;
}

prettyPrintWithOffset.fsource = [Symbol.for('define'), [Symbol.for('pretty-print-with-offset'), Symbol.for('offset'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('pretty-option'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':pretty')]], [Symbol.for('unless'), Symbol.for('pretty-option'), [Symbol.for('return'), [Symbol.for('write-to-string'), Symbol.for('form'), Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('array?'), Symbol.for('form')], [Symbol.for('return'), [Symbol.for('write-to-string'), Symbol.for('form'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('op'), [Symbol.for('first'), Symbol.for('form')]], [Symbol.for('define'), Symbol.for('elements'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':quote-toplevel'), false]]]], Symbol.for('form')]], [Symbol.for('define'), Symbol.for('elements1'), [Symbol.for('take'), Symbol.for('elements'), [Symbol.for('+'), Symbol.for('offset'), 1]]], [Symbol.for('define'), Symbol.for('elements2'), [Symbol.for('drop'), Symbol.for('elements'), [Symbol.for('+'), Symbol.for('offset'), 1]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('list'), [Symbol.for('join'), Symbol.for('space'), Symbol.for('elements1')], [Symbol.for('if'), [Symbol.for('>'), [Symbol.for('js/length'), Symbol.for('elements2')], 0], [Symbol.for('list'), Symbol.for('line'), [Symbol.for('indent'), [Symbol.for('join'), Symbol.for('line'), Symbol.for('elements2')]]], Symbol.for('empty')]]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '(', Symbol.for('result'), ')']], Symbol.for('result')];

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

prettyPrintCond.fsource = [Symbol.for('define'), [Symbol.for('pretty-print-cond'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('list'), '(', [Symbol.for('write-to-doc'), [Symbol.for('first'), Symbol.for('form')], Symbol.for('options')], Symbol.for('line'), [Symbol.for('align'), 1, [Symbol.for('join'), Symbol.for('line'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('list'), '(', [Symbol.for('write-to-doc'), [Symbol.for('first'), Symbol.for('x')], Symbol.for('options')], Symbol.for('line'), [Symbol.for('align'), 1, [Symbol.for('join'), Symbol.for('line'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x1')], [Symbol.for('write-to-doc'), Symbol.for('x1'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':quote-toplevel'), false]]]], [Symbol.for('rest'), Symbol.for('x')]]]], ')']], [Symbol.for('rest'), Symbol.for('form')]]]], ')']];

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

prettyPrintIf.fsource = [Symbol.for('define'), [Symbol.for('pretty-print-if'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('list'), '(', [Symbol.for('join'), ' ', [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('take'), Symbol.for('form'), 2]]], Symbol.for('line'), [Symbol.for('align'), 4, [Symbol.for('join'), Symbol.for('line'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('drop'), Symbol.for('form'), 2]]]], ')']];

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

prettyPrintModule.fsource = [Symbol.for('define'), [Symbol.for('pretty-print-module'), Symbol.for('form'), Symbol.for('options')], [Symbol.for('define'), Symbol.for('no-module-form-option'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':no-module-form')]], [Symbol.for('cond'), [Symbol.for('no-module-form-option'), [Symbol.for('join'), [Symbol.for('list'), Symbol.for('line'), Symbol.for('line')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('drop'), Symbol.for('form'), 3]]]], [Symbol.for('else'), [Symbol.for('list'), '(', [Symbol.for('join'), ' ', [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('take'), Symbol.for('form'), 3]]], Symbol.for('line'), [Symbol.for('indent'), [Symbol.for('join'), [Symbol.for('list'), Symbol.for('line'), Symbol.for('line')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('write-to-doc'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('drop'), Symbol.for('form'), 3]]]], ')']]]];

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

printDoc.fsource = [Symbol.for('define'), [Symbol.for('print-doc'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('~>'), Symbol.for('doc'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('options')], [Symbol.for('print-doc-list-to-string'), Symbol.for('options')]]];

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

printDocToDocList.fsource = [Symbol.for('define'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('dtype'), [Symbol.for('doc-type'), Symbol.for('doc')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('dtype'), 'string'], [Symbol.for('join'), Symbol.for('line'), [Symbol.for('string-split'), Symbol.for('doc'), Symbol.for('line')]]], [[Symbol.for('eq?'), Symbol.for('dtype'), 'array'], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('x-result'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('if'), [Symbol.for('array?'), Symbol.for('x-result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), Symbol.for('x-result')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('x-result')]]], Symbol.for('result')], [[Symbol.for('eq?'), Symbol.for('dtype'), 'align'], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('offset'), [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('contents'), [Symbol.for('second'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('contents-printed'), [Symbol.for('filter'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('empty')]]], [Symbol.for('print-doc-to-doc-list'), Symbol.for('contents'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('indentation'), [Symbol.for('string-repeat'), ' ', Symbol.for('offset')]], [Symbol.for('when'), [Symbol.for('>'), [Symbol.for('js/length'), Symbol.for('contents-printed')], 0], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('indentation')]], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('js/length'), Symbol.for('contents-printed')]]]], [Symbol.for('define'), Symbol.for('current'), [Symbol.for('aget'), Symbol.for('contents-printed'), Symbol.for('i')]], [Symbol.for('define'), Symbol.for('next'), [Symbol.for('if'), [Symbol.for('<'), Symbol.for('i'), [Symbol.for('-'), [Symbol.for('js/length'), Symbol.for('contents-printed')], 1]], [Symbol.for('aget'), Symbol.for('contents-printed'), [Symbol.for('+'), Symbol.for('i'), 1]], Symbol.for('empty')]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('current')], [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('current'), Symbol.for('line')], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('next'), Symbol.for('line')]], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('next'), Symbol.for('empty')]]], [Symbol.for('push-right!'), Symbol.for('result'), Symbol.for('indentation')]]], Symbol.for('result')], [[Symbol.for('eq?'), Symbol.for('dtype'), 'indent'], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('contents'), [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('offset'), [Symbol.for('or'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':indent')], 2]], [Symbol.for('print-doc-to-doc-list'), [Symbol.for('new'), Symbol.for('DocCommand'), 'align', Symbol.for('offset'), Symbol.for('contents'), Symbol.for('options')], Symbol.for('options')]], [[Symbol.for('eq?'), Symbol.for('dtype'), 'group'], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-field'), Symbol.for('args'), Symbol.for('doc')]], [Symbol.for('define'), Symbol.for('contents'), [Symbol.for('first'), Symbol.for('args')]], [Symbol.for('define'), Symbol.for('contents-printed'), [Symbol.for('print-doc-to-doc-list'), Symbol.for('contents'), Symbol.for('options')]], Symbol.for('contents-printed')], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('doc')]]]];

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

printDocListToString.fsource = [Symbol.for('define'), [Symbol.for('print-doc-list-to-string'), Symbol.for('doc'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('dtype'), [Symbol.for('doc-type'), Symbol.for('doc')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('dtype'), 'string'], Symbol.for('doc')], [[Symbol.for('eq?'), Symbol.for('dtype'), 'array'], [Symbol.for('string-join'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-doc-list-to-string'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('doc')], Symbol.for('empty')]], [[Symbol.for('eq?'), Symbol.for('dtype'), 'literalline'], Symbol.for('line')], [Symbol.for('else'), Symbol.for('empty')]]];

/**
 * Print an ESTree node to a string.
 */
function printToString(node: any, options: any = {}): any {
  return printDoc(printNode(node, options), options);
}

printToString.fsource = [Symbol.for('define'), [Symbol.for('print-to-string'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('node'), Symbol.for('options')], Symbol.for('options')]];

/**
 * Print an ESTree node to a `Doc` object.
 */
function printNode(node: any, options: any = {}): any {
  return visit(printVisitor, node, options);
}

printNode.fsource = [Symbol.for('define'), [Symbol.for('print-node'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('visit'), Symbol.for('print-visitor'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Visitor function for printing ESTree nodes.
 */
function printVisitor(node: any, options: any): any {
  if (!node) {
    // Gracefully handle the case where `node` is `#n`
    // because it is used by some ESTree classes to
    // represent optional values.
    return empty;
  } else if (thunkp(node)) {
    // Handle thunks within ESTree trees.
    return printVisitor(force(node), options);
  } else {
    // Otherwise, if `node` is an ESTree node proper,
    // then inspect its type and call the
    // appropriate visitor.
    const type: any = estreeType(node);
    const comments: any = options['comments'];
    const printer: any = printerMap.get(type) || defaultPrinter;
    let result: any = printer(node, options);
    if (comments) {
      result = attachComments(result, node, options);
    }
    return result;
  }
}

printVisitor.fsource = [Symbol.for('define'), [Symbol.for('print-visitor'), Symbol.for('node'), Symbol.for('options')], [Symbol.for('cond'), [[Symbol.for('not'), Symbol.for('node')], Symbol.for('empty')], [[Symbol.for('thunk?'), Symbol.for('node')], [Symbol.for('print-visitor'), [Symbol.for('force'), Symbol.for('node')], Symbol.for('options')]], [Symbol.for('else'), [Symbol.for('define'), Symbol.for('type'), [Symbol.for('estree-type'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('comments'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':comments')]], [Symbol.for('define'), Symbol.for('printer'), [Symbol.for('or'), [Symbol.for('hash-ref'), Symbol.for('printer-map'), Symbol.for('type')], Symbol.for('default-printer')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('printer'), Symbol.for('node'), Symbol.for('options')]], [Symbol.for('when'), Symbol.for('comments'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('attach-comments'), Symbol.for('result'), Symbol.for('node'), Symbol.for('options')]]], Symbol.for('result')]]];

/**
 * Print an `ExpressionStatement` ESTree node to a `Doc` object.
 */
function printExpressionStatement(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  const expression: any = getEstreeField('expression', node);
  let expressionPrinted: any = printNode(expression, options);
  // Object expressions and object destructuring must be
  // wrapped in parentheses in order to produce a
  // syntactically correct program.
  if (estreeTypeP(expression, 'ObjectExpression') || (estreeTypeP(expression, 'AssignmentExpression') && estreeTypeP(getEstreeField('left', expression), 'ObjectPattern'))) {
    expressionPrinted = docWrap(expressionPrinted, options);
  }
  return [expressionPrinted, fsemicolon ? ';' : empty];
}

printExpressionStatement.fsource = [Symbol.for('define'), [Symbol.for('print-expression-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('define'), Symbol.for('expression'), [Symbol.for('get-estree-field'), 'expression', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('expression-printed'), [Symbol.for('print-node'), Symbol.for('expression'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('estree-type?'), Symbol.for('expression'), 'ObjectExpression'], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('expression'), 'AssignmentExpression'], [Symbol.for('estree-type?'), [Symbol.for('get-estree-field'), 'left', Symbol.for('expression')], 'ObjectPattern']]], [Symbol.for('set!'), Symbol.for('expression-printed'), [Symbol.for('doc-wrap'), Symbol.for('expression-printed'), Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('expression-printed'), [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]];

/**
 * Print a `ReturnStatement` ESTree node to a `Doc` object.
 */
function printReturnStatement(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  const argument: any = getEstreeField('argument', node);
  if (argument) {
    let argumentPrinted: any = printNode(argument, options);
    if (docShouldBreakP(argumentPrinted)) {
      argumentPrinted = ['(', line, indent(argumentPrinted), line, ')'];
    }
    return ['return', space, argumentPrinted, fsemicolon ? ';' : empty];
  } else {
    return ['return', fsemicolon ? ';' : empty];
  }
}

printReturnStatement.fsource = [Symbol.for('define'), [Symbol.for('print-return-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-estree-field'), 'argument', Symbol.for('node')]], [Symbol.for('cond'), [Symbol.for('argument'), [Symbol.for('define'), Symbol.for('argument-printed'), [Symbol.for('print-node'), Symbol.for('argument'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('doc-should-break?'), Symbol.for('argument-printed')], [Symbol.for('set!'), Symbol.for('argument-printed'), [Symbol.for('list'), '(', Symbol.for('line'), [Symbol.for('indent'), Symbol.for('argument-printed')], Symbol.for('line'), ')'], Symbol.for('options')]], [Symbol.for('list'), 'return', Symbol.for('space'), Symbol.for('argument-printed'), [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]], [Symbol.for('else'), [Symbol.for('list'), 'return', [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]]]];

/**
 * Print a `YieldExpression` ESTree node to a `Doc` object.
 */
function printYieldExpression(node: any, options: any = {}): any {
  return ['yield', getEstreeField('argument', node) ? [space, printNode(getEstreeField('argument', node), options)] : empty];
}

printYieldExpression.fsource = [Symbol.for('define'), [Symbol.for('print-yield-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('list'), 'yield', [Symbol.for('if'), [Symbol.for('get-estree-field'), 'argument', Symbol.for('node')], [Symbol.for('list'), Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'argument', Symbol.for('node')], Symbol.for('options')]], Symbol.for('empty')]]];

/**
 * Print a `ThrowStatement` ESTree node to a `Doc` object.
 */
function printThrowStatement(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  return ['throw', space, printNode(getEstreeField('argument', node), options), fsemicolon ? ';' : empty];
}

printThrowStatement.fsource = [Symbol.for('define'), [Symbol.for('print-throw-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('list'), 'throw', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'argument', Symbol.for('node')], Symbol.for('options')], [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]];

/**
 * Print an `AwaitExpression` ESTree node to a `Doc` object.
 */
function printAwaitExpression(node: any, options: any = {}): any {
  return ['await', space, printNode(getEstreeField('argument', node), options)];
}

printAwaitExpression.fsource = [Symbol.for('define'), [Symbol.for('print-await-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('list'), 'await', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'argument', Symbol.for('node')], Symbol.for('options')]]];

/**
 * Print a `BreakStatement` ESTree node to a `Doc` object.
 */
function printBreakStatement(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  return ['break', getEstreeField('label', node) ? [space, printNode(getEstreeField('label', node), options)] : empty, fsemicolon ? ';' : empty];
}

printBreakStatement.fsource = [Symbol.for('define'), [Symbol.for('print-break-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('list'), 'break', [Symbol.for('if'), [Symbol.for('get-estree-field'), 'label', Symbol.for('node')], [Symbol.for('list'), Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'label', Symbol.for('node')], Symbol.for('options')]], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]];

/**
 * Print a `ContinueStatement` ESTree node to a `Doc` object.
 */
function printContinueStatement(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  return ['continue', getEstreeField('label', node) ? [space, printNode(getEstreeField('label', node), options)] : empty, fsemicolon ? ';' : empty];
}

printContinueStatement.fsource = [Symbol.for('define'), [Symbol.for('print-continue-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('list'), 'continue', [Symbol.for('if'), [Symbol.for('get-estree-field'), 'label', Symbol.for('node')], [Symbol.for('list'), Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'label', Symbol.for('node')], Symbol.for('options')]], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]];

/**
 * Print a `ThisExpression` ESTree node to a `Doc` object.
 */
function printThisExpression(node: any, options: any = {}): any {
  return 'this';
}

printThisExpression.fsource = [Symbol.for('define'), [Symbol.for('print-this-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], 'this'];

/**
 * Print an `Identifier` ESTree node to a `Doc` object.
 */
function printIdentifier(node: any, options: any = {}): any {
  const language: any = options['language'];
  const noImplicitAny: any = options['noImplicitAny'];
  let type_: any = getEstreeField('typeAnnotation', node);
  if (noImplicitAny && !type_) {
    type_ = new TSAnyKeyword();
  }
  return [getEstreeField('name', node), ((language === 'typescript') && type_) ? [getEstreeField('optional', node) ? '?:' : ':', space, printNode(type_, options)] : empty];
}

printIdentifier.fsource = [Symbol.for('define'), [Symbol.for('print-identifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('no-implicit-any'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':no-implicit-any')]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('get-estree-field'), 'typeAnnotation', Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('no-implicit-any'), [Symbol.for('not'), Symbol.for('type_')]], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]], [Symbol.for('list'), [Symbol.for('get-estree-field'), 'name', Symbol.for('node')], [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'typescript'], Symbol.for('type_')], [Symbol.for('list'), [Symbol.for('if'), [Symbol.for('get-estree-field'), 'optional', Symbol.for('node')], '?:', ':'], Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('type_'), Symbol.for('options')]], Symbol.for('empty')]]];

/**
 * Print a `Literal` ESTree node to a `Doc` object.
 */
function printLiteral(node: any, options: any = {}): any {
  const value: any = getEstreeField('value', node);
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

printLiteral.fsource = [Symbol.for('define'), [Symbol.for('print-literal'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-estree-field'), 'value', Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('string?'), Symbol.for('value')], [Symbol.for('print-string-literal'), Symbol.for('node'), Symbol.for('options')]], [[Symbol.for('eq?'), Symbol.for('value'), true], 'true'], [[Symbol.for('eq?'), Symbol.for('value'), false], 'false'], [[Symbol.for('js/null?'), Symbol.for('value')], 'null'], [[Symbol.for('undefined?'), Symbol.for('value')], 'undefined'], [Symbol.for('else'), [Symbol.for('string-append'), Symbol.for('value'), '']]]];

/**
 * Print a string `Literal` ESTree node to a `Doc` object.
 *
 * Helper function for `print-literal`.
 */
function printStringLiteral(node: any, options: any = {}): any {
  let str: any = getEstreeField('value', node).replace(new RegExp('\\\\', 'g'), '\\\\').replace(new RegExp('\'', 'g'), '\\\'').replace(new RegExp('\\n', 'g'), '\\n');
  return ['\'', str, '\''];
}

printStringLiteral.fsource = [Symbol.for('define'), [Symbol.for('print-string-literal'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('get-estree-field'), 'value', Symbol.for('_')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\\\', 'g'], Symbol.for('_'), '\\\\'], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\'', 'g'], Symbol.for('_'), '\\\''], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '\\n', 'g'], Symbol.for('_'), '\\n']]], [Symbol.for('list'), '\'', Symbol.for('str'), '\'']];

/**
 * Print a template string.
 *
 * Helper function for `print-template-element` and
 * `print-template-literal`.
 */
function printTemplateString(str: any): any {
  return ['`', join(literalline, str.split(line)), '`'];
}

printTemplateString.fsource = [Symbol.for('define'), [Symbol.for('print-template-string'), Symbol.for('str')], [Symbol.for('list'), '`', [Symbol.for('~>'), Symbol.for('str'), [Symbol.for('string-split'), Symbol.for('line')], [Symbol.for('join'), Symbol.for('literalline'), Symbol.for('_')]], '`']];

/**
 * Print a `TemplateElement` ESTree node to a `Doc` object.
 */
function printTemplateElement(node: any, options: any = {}): any {
  let str: any = getEstreeField('raw', getEstreeField('value', node));
  return printTemplateString(str);
}

printTemplateElement.fsource = [Symbol.for('define'), [Symbol.for('print-template-element'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('get-estree-field'), 'raw', [Symbol.for('get-estree-field'), 'value', Symbol.for('node')]]], [Symbol.for('print-template-string'), Symbol.for('str')]];

/**
 * Print a `TemplateLiteral` ESTree node to a `Doc` object.
 */
function printTemplateLiteral(node: any, options: any = {}): any {
  let str: any = getEstreeField('raw', getEstreeField('value', getEstreeField('quasis', node)[0]));
  return printTemplateString(str);
}

printTemplateLiteral.fsource = [Symbol.for('define'), [Symbol.for('print-template-literal'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('get-estree-field'), 'quasis', Symbol.for('_')], [Symbol.for('first'), Symbol.for('_')], [Symbol.for('get-estree-field'), 'value', Symbol.for('_')], [Symbol.for('get-estree-field'), 'raw', Symbol.for('_')]]], [Symbol.for('print-template-string'), Symbol.for('str')]];

/**
 * Print a `TaggedTemplateExpression` ESTree node to a `Doc` object.
 */
function printTaggedTemplateExpression(node: any, options: any = {}): any {
  const tag: any = getEstreeField('tag', node);
  const tagPrinted: any = printNode(tag, options);
  const quasi: any = getEstreeField('quasi', node);
  const quasiPrinted: any = printNode(quasi, options);
  return [tagPrinted, quasiPrinted];
}

printTaggedTemplateExpression.fsource = [Symbol.for('define'), [Symbol.for('print-tagged-template-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('tag'), [Symbol.for('get-estree-field'), 'tag', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('tag-printed'), [Symbol.for('print-node'), Symbol.for('tag'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('quasi'), [Symbol.for('get-estree-field'), 'quasi', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('quasi-printed'), [Symbol.for('print-node'), Symbol.for('quasi'), Symbol.for('options')]], [Symbol.for('list'), Symbol.for('tag-printed'), Symbol.for('quasi-printed')]];

/**
 * Print a `UnaryExpression` ESTree node to a `Doc` object.
 */
function printUnaryExpression(node: any, options: any = {}): any {
  const prefix: any = getEstreeField('prefix', node);
  const operator: any = getEstreeField('operator', node);
  const operatorPrinted: any = operator;
  const argument: any = getEstreeField('argument', node);
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

printUnaryExpression.fsource = [Symbol.for('define'), [Symbol.for('print-unary-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('prefix'), [Symbol.for('get-estree-field'), 'prefix', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('get-estree-field'), 'operator', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator-printed'), Symbol.for('operator')], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-estree-field'), 'argument', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('argument-printed'), [Symbol.for('print-node'), Symbol.for('argument'), Symbol.for('options')]], [Symbol.for('unless'), [Symbol.for('estree-simple?'), Symbol.for('argument')], [Symbol.for('set!'), Symbol.for('argument-printed'), [Symbol.for('doc-wrap'), Symbol.for('argument-printed'), Symbol.for('options')]]], [Symbol.for('cond'), [Symbol.for('prefix'), [Symbol.for('list'), Symbol.for('operator-printed'), [Symbol.for('if'), [Symbol.for('or'), [Symbol.for('eq?'), Symbol.for('operator-printed'), 'delete'], [Symbol.for('eq?'), Symbol.for('operator-printed'), 'typeof']], Symbol.for('space'), Symbol.for('empty')], Symbol.for('argument-printed')]], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('argument-printed'), Symbol.for('operator-printed')]]]];

/**
 * Print a `BinaryExpression` ESTree node to a `Doc` object.
 */
function printBinaryExpression(node: any, options: any = {}): any {
  let type_: any = estreeType(node);
  const operator: any = getEstreeField('operator', node);
  const operatorPrinted: any = operator;
  const left: any = getEstreeField('left', node);
  let leftPrinted: any = printNode(left, options);
  let leftPrintedStr: any = docValueString(leftPrinted);
  const right: any = getEstreeField('right', node);
  const rightPrinted: any = printNode(right, options);
  let rightPrintedStr: any = docValueString(rightPrinted);
  let shouldBreak: any = docShouldBreakP(leftPrinted) || docShouldBreakP(rightPrinted);
  const isMultilineStringLiteral: any = estreeStringLiteralP(left) && getEstreeField('value', left).match('\\n$');
  const isMultilineBinaryExpression: any = !isMultilineStringLiteral && estreeTypeP(left, 'BinaryExpression') && estreeStringLiteralP(getEstreeField('right', left)) && getEstreeField('value', getEstreeField('right', left)).match(new RegExp('\\n$'));
  const isMultilineString: any = isMultilineStringLiteral || isMultilineBinaryExpression;
  let result: any;
  if (!(estreeSimpleP(left) || (estreeTypeP(left, type_) && (getEstreeField('operator', left) === operator)))) {
    leftPrintedStr = docWrap(leftPrintedStr, {
      ...options,
      hasComments: docHasCommentsP(leftPrinted)
    });
  }
  if (!(estreeSimpleP(right) || (estreeTypeP(right, type_) && (getEstreeField('operator', right) === operator) && ['+', '*', '&&', '||'].includes(operator)))) {
    rightPrintedStr = docWrap(rightPrintedStr, {
      ...options,
      hasComments: docHasCommentsP(rightPrinted)
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
    shouldBreak
  });
}

printBinaryExpression.fsource = [Symbol.for('define'), [Symbol.for('print-binary-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('estree-type'), Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('get-estree-field'), 'operator', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator-printed'), Symbol.for('operator')], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-estree-field'), 'left', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-printed'), [Symbol.for('print-node'), Symbol.for('left'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('left-printed-str'), [Symbol.for('doc-value-string'), Symbol.for('left-printed')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-estree-field'), 'right', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-printed'), [Symbol.for('print-node'), Symbol.for('right'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('right-printed-str'), [Symbol.for('doc-value-string'), Symbol.for('right-printed')]], [Symbol.for('define'), Symbol.for('should-break'), [Symbol.for('or'), [Symbol.for('doc-should-break?'), Symbol.for('left-printed')], [Symbol.for('doc-should-break?'), Symbol.for('right-printed')]]], [Symbol.for('define'), Symbol.for('is-multiline-string-literal'), [Symbol.for('and'), [Symbol.for('estree-string-literal?'), Symbol.for('left')], [Symbol.for('regexp-match'), '\\n$', [Symbol.for('get-estree-field'), 'value', Symbol.for('left')]]]], [Symbol.for('define'), Symbol.for('is-multiline-binary-expression'), [Symbol.for('and'), [Symbol.for('not'), Symbol.for('is-multiline-string-literal')], [Symbol.for('estree-type?'), Symbol.for('left'), 'BinaryExpression'], [Symbol.for('estree-string-literal?'), [Symbol.for('get-estree-field'), 'right', Symbol.for('left')]], [Symbol.for('regexp-match'), [Symbol.for('regexp'), '\\n$'], [Symbol.for('~>'), Symbol.for('left'), [Symbol.for('get-estree-field'), 'right', Symbol.for('_')], [Symbol.for('get-estree-field'), 'value', Symbol.for('_')]]]]], [Symbol.for('define'), Symbol.for('is-multiline-string'), [Symbol.for('or'), Symbol.for('is-multiline-string-literal'), Symbol.for('is-multiline-binary-expression')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('estree-simple?'), Symbol.for('left')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('left'), Symbol.for('type_')], [Symbol.for('eq?'), [Symbol.for('get-estree-field'), 'operator', Symbol.for('left')], Symbol.for('operator')]]], [Symbol.for('set!'), Symbol.for('left-printed-str'), [Symbol.for('doc-wrap'), Symbol.for('left-printed-str'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':has-comments'), [Symbol.for('doc-has-comments?'), Symbol.for('left-printed')]]]]]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('estree-simple?'), Symbol.for('right')], [Symbol.for('and'), [Symbol.for('estree-type?'), Symbol.for('right'), Symbol.for('type_')], [Symbol.for('eq?'), [Symbol.for('get-estree-field'), 'operator', Symbol.for('right')], Symbol.for('operator')], [Symbol.for('memq?'), Symbol.for('operator'), [Symbol.for('quote'), ['+', '*', '&&', '||']]]]], [Symbol.for('set!'), Symbol.for('right-printed-str'), [Symbol.for('doc-wrap'), Symbol.for('right-printed-str'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':has-comments'), [Symbol.for('doc-has-comments?'), Symbol.for('right-printed')]]]]]], [Symbol.for('cond'), [Symbol.for('should-break'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '(', Symbol.for('line'), [Symbol.for('align'), 1, Symbol.for('left-printed-str')], [Symbol.for('if'), [Symbol.for('estree-has-trailing-comment?'), Symbol.for('left')], [Symbol.for('list'), Symbol.for('line'), [Symbol.for('align'), 1, Symbol.for('operator')]], [Symbol.for('list'), Symbol.for('space'), Symbol.for('operator')]], Symbol.for('line'), [Symbol.for('align'), 1, Symbol.for('right-printed-str')], Symbol.for('line'), ')']]], [Symbol.for('is-multiline-string'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('left-printed-str'), Symbol.for('space'), Symbol.for('operator'), Symbol.for('line'), [Symbol.for('indent'), Symbol.for('right-printed-str')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('left-printed-str'), Symbol.for('space'), Symbol.for('operator'), Symbol.for('space'), Symbol.for('right-printed-str')]]]], [Symbol.for('group'), Symbol.for('result'), [Symbol.for('js/obj'), Symbol.for(':should-break'), Symbol.for('should-break')]]];

/**
 * Print a `LogicalExpression` ESTree node to a `Doc` object.
 */
function printLogicalExpression(node: any, options: any = {}): any {
  return printBinaryExpression(node, options);
}

printLogicalExpression.fsource = [Symbol.for('define'), [Symbol.for('print-logical-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-binary-expression'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print an `AssignmentExpression` ESTree node to a `Doc` object.
 */
function printAssignmentExpression(node: any, options: any = {}): any {
  // TODO: Break up statement if one of the sides have comments.
  const language: any = options['language'];
  const operator: any = getEstreeField('operator', node);
  const operatorPrinted: any = operator;
  const left: any = getEstreeField('left', node);
  let leftPrinted: any = printNode(left, options);
  const right: any = getEstreeField('right', node);
  const rightPrinted: any = printNode(right, {
    ...options,
    noImplicitAny: false
  });
  let result: any = [leftPrinted, space, operator, docHasCommentsP(rightPrinted) ? [line, indent(rightPrinted)] : [space, rightPrinted]];
  return result;
}

printAssignmentExpression.fsource = [Symbol.for('define'), [Symbol.for('print-assignment-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('operator'), [Symbol.for('get-estree-field'), 'operator', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('operator-printed'), Symbol.for('operator')], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-estree-field'), 'left', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-printed'), [Symbol.for('print-node'), Symbol.for('left'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-estree-field'), 'right', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-printed'), [Symbol.for('print-node'), Symbol.for('right'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':no-implicit-any'), false]]]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('left-printed'), Symbol.for('space'), Symbol.for('operator'), [Symbol.for('if'), [Symbol.for('doc-has-comments?'), Symbol.for('right-printed')], [Symbol.for('list'), Symbol.for('line'), [Symbol.for('indent'), Symbol.for('right-printed')]], [Symbol.for('list'), Symbol.for('space'), Symbol.for('right-printed')]]]], Symbol.for('result')];

/**
 * Print an `AssignmentPattern` ESTree node to a `Doc` object.
 */
function printAssignmentPattern(node: any, options: any = {}): any {
  return printNode(new VariableDeclarator(getEstreeField('left', node), getEstreeField('right', node)), options);
}

printAssignmentPattern.fsource = [Symbol.for('define'), [Symbol.for('print-assignment-pattern'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-node'), [Symbol.for('new'), Symbol.for('VariableDeclarator'), [Symbol.for('get-estree-field'), 'left', Symbol.for('node')], [Symbol.for('get-estree-field'), 'right', Symbol.for('node')]], Symbol.for('options')]];

/**
 * Print a `CallExpression` ESTree node to a `Doc` object.
 */
function printCallExpression(node: any, options: any = {}): any {
  const callee: any = getEstreeField('callee', node);
  const calleeType: any = estreeType(callee);
  let calleePrinted: any = printNode(callee, options);
  const args: any = getEstreeField('arguments', node);
  const argsPrinted: any = args.map(function (x: any): any {
    return printNode(x, options);
  });
  const optional: any = getEstreeField('optional', node);
  if (estreeComplexP(callee)) {
    calleePrinted = docWrap(calleePrinted, options);
  }
  return [calleePrinted, optional ? '?.' : empty, '(', join([',', space], argsPrinted), ')'];
}

printCallExpression.fsource = [Symbol.for('define'), [Symbol.for('print-call-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('callee'), [Symbol.for('get-estree-field'), 'callee', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('callee-type'), [Symbol.for('estree-type'), Symbol.for('callee')]], [Symbol.for('define'), Symbol.for('callee-printed'), [Symbol.for('print-node'), Symbol.for('callee'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('args'), [Symbol.for('get-estree-field'), 'arguments', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('args-printed'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('args')]], [Symbol.for('define'), Symbol.for('optional'), [Symbol.for('get-estree-field'), 'optional', Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('estree-complex?'), Symbol.for('callee')], [Symbol.for('set!'), Symbol.for('callee-printed'), [Symbol.for('doc-wrap'), Symbol.for('callee-printed'), Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('callee-printed'), [Symbol.for('if'), Symbol.for('optional'), '?.', Symbol.for('empty')], '(', [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('args-printed')], ')']];

/**
 * Print a `SequenceExpression` ESTree node to a `Doc` object.
 */
function printSequenceExpression(node: any, options: any = {}): any {
  const expressions: any = getEstreeField('expressions', node);
  const expressionsPrinted: any = expressions.map(function (x: any): any {
    return printNode(x, options);
  });
  let result: any;
  result = join([',', space], expressionsPrinted);
  // (when (> (js/length expressions) 1)
  //   (set! result
  //         (doc-wrap result options)))
  return result;
}

printSequenceExpression.fsource = [Symbol.for('define'), [Symbol.for('print-sequence-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('expressions'), [Symbol.for('get-estree-field'), 'expressions', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('expressions-printed'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('expressions')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('expressions-printed')]], Symbol.for('result')];

/**
 * Print a `BlockStatement` ESTree node to a `Doc` object.
 */
function printBlockStatement(node: any, options: any = {}): any {
  const body: any = getEstreeField('body', node);
  // FIXME: Kludge, this code belongs in the compiler.
  const bodyModified: any = ((): any => {
    if (getEstreeField('comments', node) && (body.length > 0)) {
      body[0].comments = [...getEstreeField('comments', node), ...(getEstreeField('comments', body[0]) || [])];
      node.comments = [];
    }
    return getEstreeField('body', node);
  })();
  const bodyIndented: any = indent(join(line, bodyModified.map(function (x: any): any {
    return printNode(x, options);
  })));
  const bodyPrinted: any = printDoc(bodyIndented);
  return ['{', line, bodyIndented, (bodyPrinted === '') ? empty : line, '}'];
}

printBlockStatement.fsource = [Symbol.for('define'), [Symbol.for('print-block-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-estree-field'), 'body', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-modified'), [Symbol.for('begin'), [Symbol.for('when'), [Symbol.for('and'), [Symbol.for('get-estree-field'), 'comments', Symbol.for('node')], [Symbol.for('>'), [Symbol.for('js/length'), Symbol.for('body')], 0]], [Symbol.for('set-field!'), Symbol.for('comments'), [Symbol.for('first'), Symbol.for('body')], [Symbol.for('append'), [Symbol.for('get-estree-field'), 'comments', Symbol.for('node')], [Symbol.for('or'), [Symbol.for('get-estree-field'), 'comments', [Symbol.for('first'), Symbol.for('body')]], [Symbol.for('quote'), []]]]], [Symbol.for('set-field!'), Symbol.for('comments'), Symbol.for('node'), [Symbol.for('quote'), []]]], [Symbol.for('get-estree-field'), 'body', Symbol.for('node')]]], [Symbol.for('define'), Symbol.for('body-indented'), [Symbol.for('indent'), [Symbol.for('~>'), Symbol.for('body-modified'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), Symbol.for('line'), Symbol.for('_')]]]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-doc'), Symbol.for('body-indented')]], [Symbol.for('list'), '{', Symbol.for('line'), Symbol.for('body-indented'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('body-printed'), ''], Symbol.for('empty'), Symbol.for('line')], '}']];

/**
 * Print a `MemberExpression` ESTree node to a `Doc` object.
 */
function printMemberExpression(node: any, options: any = {}): any {
  const language: any = options['language'];
  const object: any = getEstreeField('object', node);
  let objectPrinted: any = printNode(object, options);
  const property: any = getEstreeField('property', node);
  const propertyPrinted: any = printNode(property, options);
  const computed: any = getEstreeField('computed', node);
  const optional: any = getEstreeField('optional', node);
  if (!estreeSimpleP(object) || estreeTypeP(object, 'ObjectExpression')) {
    // If the object expression is complicated, wrap it in
    // parentheses.
    objectPrinted = docWrap(objectPrinted, options);
  }
  if (computed) {
    return [objectPrinted, optional ? '?.' : '', '[', propertyPrinted, ']'];
  } else {
    return [objectPrinted, optional ? '?.' : '.', propertyPrinted];
  }
}

printMemberExpression.fsource = [Symbol.for('define'), [Symbol.for('print-member-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('object'), [Symbol.for('get-estree-field'), 'object', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('object-printed'), [Symbol.for('print-node'), Symbol.for('object'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('property'), [Symbol.for('get-estree-field'), 'property', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('property-printed'), [Symbol.for('print-node'), Symbol.for('property'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('computed'), [Symbol.for('get-estree-field'), 'computed', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('optional'), [Symbol.for('get-estree-field'), 'optional', Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('or'), [Symbol.for('not'), [Symbol.for('estree-simple?'), Symbol.for('object')]], [Symbol.for('estree-type?'), Symbol.for('object'), 'ObjectExpression']], [Symbol.for('set!'), Symbol.for('object-printed'), [Symbol.for('doc-wrap'), Symbol.for('object-printed'), Symbol.for('options')]]], [Symbol.for('cond'), [Symbol.for('computed'), [Symbol.for('list'), Symbol.for('object-printed'), [Symbol.for('if'), Symbol.for('optional'), '?.', ''], '[', Symbol.for('property-printed'), ']']], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('object-printed'), [Symbol.for('if'), Symbol.for('optional'), '?.', '.'], Symbol.for('property-printed')]]]];

/**
 * Print an `UpdateExpression` ESTree node to a `Doc` object.
 */
function printUpdateExpression(node: any, options: any = {}): any {
  return printUnaryExpression(node, options);
}

printUpdateExpression.fsource = [Symbol.for('define'), [Symbol.for('print-update-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-unary-expression'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print a `SpreadElement` ESTree node to a `Doc` object.
 */
function printSpreadElement(node: any, options: any = {}): any {
  const language: any = options['language'];
  const noImplicitAny: any = options['noImplicitAny'];
  const argument: any = getEstreeField('argument', node);
  let argumentPrinted: any = printNode(argument, {
    ...options,
    noImplicitAny: false
  });
  let type_: any = getEstreeField('typeAnnotation', node);
  if (noImplicitAny && !type_) {
    type_ = new TSArrayType(new TSAnyKeyword());
  }
  if (!estreeSimpleP(argument)) {
    argumentPrinted = docWrap(argumentPrinted, options);
  }
  return ['...', argumentPrinted, ((language === 'typescript') && type_) ? [':', space, printNode(type_, options)] : empty];
}

printSpreadElement.fsource = [Symbol.for('define'), [Symbol.for('print-spread-element'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('no-implicit-any'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':no-implicit-any')]], [Symbol.for('define'), Symbol.for('argument'), [Symbol.for('get-estree-field'), 'argument', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('argument-printed'), [Symbol.for('print-node'), Symbol.for('argument'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':no-implicit-any'), false]]]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('get-estree-field'), 'typeAnnotation', Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('no-implicit-any'), [Symbol.for('not'), Symbol.for('type_')]], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('new'), Symbol.for('TSArrayType'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]], [Symbol.for('unless'), [Symbol.for('estree-simple?'), Symbol.for('argument')], [Symbol.for('set!'), Symbol.for('argument-printed'), [Symbol.for('doc-wrap'), Symbol.for('argument-printed'), Symbol.for('options')]]], [Symbol.for('list'), '...', Symbol.for('argument-printed'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'typescript'], Symbol.for('type_')], [Symbol.for('list'), ':', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('type_'), Symbol.for('options')]], Symbol.for('empty')]]];

/**
 * Print a `RestElement` ESTree node to a `Doc` object.
 */
function printRestElement(node: any, options: any = {}): any {
  return printSpreadElement(node, options);
}

printRestElement.fsource = [Symbol.for('define'), [Symbol.for('print-rest-element'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-spread-element'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print a function declaration or function expression to a
 * `Doc` object. Also handles arrow functions.
 */
function printFunction(node: any, options: any = {}, settings: any = {}): any {
  const language: any = options['language'];
  const arrow: any = settings['arrow'];
  const async_: any = getEstreeField('async', node);
  const returnTypeSetting: any = settings['returnType'];
  const returnType: any = (typeof returnTypeSetting === 'string') ? returnTypeSetting : getEstreeField('returnType', node);
  const returnTypePrinted: any = (typeof returnType === 'string') ? returnType : (returnType ? printTsType(returnType, options) : (async_ ? 'Promise<any>' : 'any'));
  return [async_ ? ['async', space] : empty, arrow ? empty : ['function', space], getEstreeField('id', node) ? printNode(getEstreeField('id', node), options) : empty, '(', join([',', space], getEstreeField('params', node).map(function (x: any): any {
    return printNode(x, {
      ...options,
      noImplicitAny: true
    });
  })), ')', ((language === 'typescript') && (returnTypePrinted !== '')) ? [':', space, returnTypePrinted] : empty, arrow ? [space, '=>', space] : space, printNode(getEstreeField('body', node), options)];
}

printFunction.fsource = [Symbol.for('define'), [Symbol.for('print-function'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]], [Symbol.for('settings'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('arrow'), [Symbol.for('oget'), Symbol.for('settings'), Symbol.for(':arrow')]], [Symbol.for('define'), Symbol.for('async_'), [Symbol.for('get-estree-field'), 'async', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('return-type-setting'), [Symbol.for('oget'), Symbol.for('settings'), Symbol.for(':return-type')]], [Symbol.for('define'), Symbol.for('return-type'), [Symbol.for('if'), [Symbol.for('string?'), Symbol.for('return-type-setting')], Symbol.for('return-type-setting'), [Symbol.for('get-estree-field'), 'returnType', Symbol.for('node')]]], [Symbol.for('define'), Symbol.for('return-type-printed'), [Symbol.for('cond'), [[Symbol.for('string?'), Symbol.for('return-type')], Symbol.for('return-type')], [Symbol.for('return-type'), [Symbol.for('print-ts-type'), Symbol.for('return-type'), Symbol.for('options')]], [Symbol.for('async_'), 'Promise<any>'], [Symbol.for('else'), 'any']]], [Symbol.for('list'), [Symbol.for('if'), Symbol.for('async_'), [Symbol.for('list'), 'async', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('arrow'), Symbol.for('empty'), [Symbol.for('list'), 'function', Symbol.for('space')]], [Symbol.for('if'), [Symbol.for('get-estree-field'), 'id', Symbol.for('node')], [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'id', Symbol.for('node')], Symbol.for('options')], Symbol.for('empty')], '(', [Symbol.for('~>'), [Symbol.for('get-estree-field'), 'params', Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':no-implicit-any'), true]]]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], ')', [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'typescript'], [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('return-type-printed'), '']]], [Symbol.for('list'), ':', Symbol.for('space'), Symbol.for('return-type-printed')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('arrow'), [Symbol.for('list'), Symbol.for('space'), '=>', Symbol.for('space')], Symbol.for('space')], [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'body', Symbol.for('node')], Symbol.for('options')]]];

/**
 * Print a `FunctionDeclaration` ESTree node to a `Doc` object.
 */
function printFunctionDeclaration(node: any, options: any = {}): any {
  return printFunction(node, options);
}

printFunctionDeclaration.fsource = [Symbol.for('define'), [Symbol.for('print-function-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-function'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print a `FunctionExpression` ESTree node to a `Doc` object.
 */
function printFunctionExpression(node: any, options: any = {}): any {
  return printFunction(node, options);
}

printFunctionExpression.fsource = [Symbol.for('define'), [Symbol.for('print-function-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-function'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print a `ArrowFunctionExpression` ESTree node to a `Doc` object.
 */
function printArrowFunctionExpression(node: any, options: any = {}): any {
  return printFunction(node, options, {
    arrow: true
  });
}

printArrowFunctionExpression.fsource = [Symbol.for('define'), [Symbol.for('print-arrow-function-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-function'), Symbol.for('node'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':arrow'), true]]];

/**
 * Print a `VariableDeclaration` ESTree node to a `Doc` object.
 */
function printVariableDeclaration(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  return [getEstreeField('kind', node), space, join([',', space], getEstreeField('declarations', node).map(function (x: any): any {
    return printNode(x, options);
  })), fsemicolon ? ';' : empty];
}

printVariableDeclaration.fsource = [Symbol.for('define'), [Symbol.for('print-variable-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('list'), [Symbol.for('get-estree-field'), 'kind', Symbol.for('node')], Symbol.for('space'), [Symbol.for('~>'), [Symbol.for('get-estree-field'), 'declarations', Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]];

/**
 * Print a `VariableDeclarator` ESTree node to a `Doc` object.
 */
function printVariableDeclarator(node: any, options: any = {}): any {
  const language: any = options['language'];
  const id: any = getEstreeField('id', node);
  const idPrinted: any = printNode(id, {
    ...options,
    noImplicitAny: true
  });
  if (getEstreeField('init', node)) {
    const init: any = getEstreeField('init', node);
    const initPrinted: any = printNode(init, {
      ...options,
      noImplicitAny: false
    });
    return [idPrinted, space, '=', docHasCommentsP(initPrinted) ? [line, indent(initPrinted)] : [space, initPrinted]];
  } else {
    return idPrinted;
  }
}

printVariableDeclarator.fsource = [Symbol.for('define'), [Symbol.for('print-variable-declarator'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('get-estree-field'), 'id', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('id-printed'), [Symbol.for('print-node'), Symbol.for('id'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':no-implicit-any'), true]]]], [Symbol.for('cond'), [[Symbol.for('get-estree-field'), 'init', Symbol.for('node')], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('get-estree-field'), 'init', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('init-printed'), [Symbol.for('print-node'), Symbol.for('init'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':no-implicit-any'), false]]]], [Symbol.for('list'), Symbol.for('id-printed'), Symbol.for('space'), '=', [Symbol.for('if'), [Symbol.for('doc-has-comments?'), Symbol.for('init-printed')], [Symbol.for('list'), Symbol.for('line'), [Symbol.for('indent'), Symbol.for('init-printed')]], [Symbol.for('list'), Symbol.for('space'), Symbol.for('init-printed')]]]], [Symbol.for('else'), Symbol.for('id-printed')]]];

/**
 * Print an `IfStatement` ESTree node to a `Doc` object.
 */
function printIfStatement(node: any, options: any = {}): any {
  const test: any = getEstreeField('test', node);
  let testPrinted: any = printNode(test, options);
  let testPrintedStr: any = docValueString(testPrinted);
  // It is customary to wrap assignment expressions
  // in an extra set of parentheses when used as a
  // condition, as this helps to distinguish them
  // from comparisons (`((x = y))` vs. `(x === y)`).
  if (estreeTypeP(test, 'AssignmentExpression')) {
    testPrintedStr = docWrap(testPrinted, options);
  }
  const consequent: any = getEstreeField('consequent', node);
  let consequentPrinted: any = printNode(consequent, options);
  const alternate: any = getEstreeField('alternate', node);
  let result: any = 'if (' + testPrintedStr + ')' + (docShouldBreakP(consequentPrinted) ? line : space) + docValueString(consequentPrinted);
  if (alternate) {
    let alternatePrinted: any = printNode(alternate, options);
    result = result + ' else ' + docValueString(alternatePrinted);
  }
  return result;
}

printIfStatement.fsource = [Symbol.for('define'), [Symbol.for('print-if-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-estree-field'), 'test', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('test-printed-str'), [Symbol.for('doc-value-string'), Symbol.for('test-printed')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('test'), 'AssignmentExpression'], [Symbol.for('set!'), Symbol.for('test-printed-str'), [Symbol.for('doc-wrap'), Symbol.for('test-printed'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('get-estree-field'), 'consequent', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('consequent-printed'), [Symbol.for('print-node'), Symbol.for('consequent'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('alternate'), [Symbol.for('get-estree-field'), 'alternate', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('string-append'), 'if (', Symbol.for('test-printed-str'), ')', [Symbol.for('if'), [Symbol.for('doc-should-break?'), Symbol.for('consequent-printed')], Symbol.for('line'), Symbol.for('space')], [Symbol.for('doc-value-string'), Symbol.for('consequent-printed')]]], [Symbol.for('when'), Symbol.for('alternate'), [Symbol.for('define'), Symbol.for('alternate-printed'), [Symbol.for('print-node'), Symbol.for('alternate'), Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('string-append'), Symbol.for('result'), ' else ', [Symbol.for('doc-value-string'), Symbol.for('alternate-printed')]]]], Symbol.for('result')];

/**
 * Print a `ConditionalExpression` ESTree node to a `Doc` object.
 */
function printConditionalExpression(node: any, options: any = {}): any {
  const test: any = getEstreeField('test', node);
  let testPrinted: any = printNode(test, options);
  const consequent: any = getEstreeField('consequent', node);
  let consequentPrinted: any = printNode(consequent, options);
  const alternate: any = getEstreeField('alternate', node);
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

printConditionalExpression.fsource = [Symbol.for('define'), [Symbol.for('print-conditional-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-estree-field'), 'test', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('get-estree-field'), 'consequent', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('consequent-printed'), [Symbol.for('print-node'), Symbol.for('consequent'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('alternate'), [Symbol.for('get-estree-field'), 'alternate', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('alternate-printed'), [Symbol.for('print-node'), Symbol.for('alternate'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('unless'), [Symbol.for('estree-simple?'), Symbol.for('test')], [Symbol.for('set!'), Symbol.for('test-printed'), [Symbol.for('doc-wrap'), Symbol.for('test-printed'), Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('estree-simple?'), Symbol.for('consequent')], [Symbol.for('set!'), Symbol.for('consequent-printed'), [Symbol.for('doc-wrap'), Symbol.for('consequent-printed'), Symbol.for('options')]]], [Symbol.for('unless'), [Symbol.for('or'), [Symbol.for('estree-type?'), Symbol.for('alternate'), 'SequenceExpression'], [Symbol.for('estree-simple?'), Symbol.for('alternate')]], [Symbol.for('set!'), Symbol.for('alternate-printed'), [Symbol.for('doc-wrap'), Symbol.for('alternate-printed'), Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('test-printed'), Symbol.for('space'), '?', Symbol.for('space'), Symbol.for('consequent-printed'), Symbol.for('space'), ':', Symbol.for('space'), Symbol.for('alternate-printed')]];

/**
 * Print a `WhileStatement` ESTree node to a `Doc` object.
 */
function printWhileStatement(node: any, options: any = {}): any {
  const test: any = getEstreeField('test', node);
  let testPrinted: any = printNode(test, options);
  const body: any = getEstreeField('body', node);
  const bodyPrinted: any = printNode(body, options);
  if (estreeTypeP(test, 'AssignmentExpression')) {
    testPrinted = docWrap(testPrinted, options);
  }
  return ['while', space, '(', testPrinted, ')', space, bodyPrinted];
}

printWhileStatement.fsource = [Symbol.for('define'), [Symbol.for('print-while-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-estree-field'), 'test', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-estree-field'), 'body', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('test'), 'AssignmentExpression'], [Symbol.for('set!'), Symbol.for('test-printed'), [Symbol.for('doc-wrap'), Symbol.for('test-printed'), Symbol.for('options')]]], [Symbol.for('list'), 'while', Symbol.for('space'), '(', Symbol.for('test-printed'), ')', Symbol.for('space'), Symbol.for('body-printed')]];

/**
 * Print a `DoWhileStatement` ESTree node to a `Doc` object.
 */
function printDoWhileStatement(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  const test: any = getEstreeField('test', node);
  let testPrinted: any = printNode(test, options);
  const body: any = getEstreeField('body', node);
  const bodyPrinted: any = printNode(body, options);
  if (estreeTypeP(test, 'AssignmentExpression')) {
    testPrinted = docWrap(testPrinted, options);
  }
  return ['do', space, bodyPrinted, space, 'while', space, '(', testPrinted, ')', fsemicolon ? ';' : empty];
}

printDoWhileStatement.fsource = [Symbol.for('define'), [Symbol.for('print-do-while-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-estree-field'), 'test', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-estree-field'), 'body', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('test'), 'AssignmentExpression'], [Symbol.for('set!'), Symbol.for('test-printed'), [Symbol.for('doc-wrap'), Symbol.for('test-printed'), Symbol.for('options')]]], [Symbol.for('list'), 'do', Symbol.for('space'), Symbol.for('body-printed'), Symbol.for('space'), 'while', Symbol.for('space'), '(', Symbol.for('test-printed'), ')', [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]];

/**
 * Print a `ForStatement` ESTree node to a `Doc` object.
 */
function printForStatement(node: any, options: any = {}): any {
  const init: any = getEstreeField('init', node);
  const initPrinted: any = printNode(init, {
    ...options,
    fsemicolon: false
  });
  const test: any = getEstreeField('test', node);
  let testPrinted: any = printDoc(printNode(test, options), options);
  const update: any = getEstreeField('update', node);
  const updatePrinted: any = printNode(update, {
    ...options,
    fsemicolon: false
  });
  const body: any = getEstreeField('body', node);
  const bodyPrinted: any = printNode(body, options);
  return ['for', space, '(', initPrinted, ';', (testPrinted === empty) ? empty : space, testPrinted, ';', (updatePrinted === empty) ? empty : space, updatePrinted, ')', space, bodyPrinted];
}

printForStatement.fsource = [Symbol.for('define'), [Symbol.for('print-for-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('init'), [Symbol.for('get-estree-field'), 'init', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('init-printed'), [Symbol.for('print-node'), Symbol.for('init'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':fsemicolon'), false]]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-estree-field'), 'test', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('update'), [Symbol.for('get-estree-field'), 'update', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('update-printed'), [Symbol.for('print-node'), Symbol.for('update'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':fsemicolon'), false]]]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-estree-field'), 'body', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('list'), 'for', Symbol.for('space'), '(', Symbol.for('init-printed'), ';', [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('test-printed'), Symbol.for('empty')], Symbol.for('empty'), Symbol.for('space')], Symbol.for('test-printed'), ';', [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('update-printed'), Symbol.for('empty')], Symbol.for('empty'), Symbol.for('space')], Symbol.for('update-printed'), ')', Symbol.for('space'), Symbol.for('body-printed')]];

/**
 * Print a `ForOfStatement` ESTree node to a `Doc` object.
 */
function printForOfStatement(node: any, options: any = {}): any {
  const language: any = options['language'];
  const left: any = getEstreeField('left', node);
  let leftPrinted: any = printNode(left, {
    ...options,
    fsemicolon: false
  });
  const right: any = getEstreeField('right', node);
  const rightPrinted: any = printNode(right, options);
  const body: any = getEstreeField('body', node);
  const bodyPrinted: any = printNode(body, options);
  let resultStr: any;
  // FIXME: Kludge.
  if (language === 'typescript') {
    leftPrinted = printDoc(leftPrinted).replace(new RegExp(': any$'), '');
  }
  return ['for', space, '(', leftPrinted, space, 'of', space, rightPrinted, ')', space, bodyPrinted];
}

printForOfStatement.fsource = [Symbol.for('define'), [Symbol.for('print-for-of-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-estree-field'), 'left', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-printed'), [Symbol.for('print-node'), Symbol.for('left'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':fsemicolon'), false]]]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-estree-field'), 'right', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-printed'), [Symbol.for('print-node'), Symbol.for('right'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-estree-field'), 'body', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('result-str')], [Symbol.for('when'), [Symbol.for('eq?'), Symbol.for('language'), 'typescript'], [Symbol.for('set!'), Symbol.for('left-printed'), [Symbol.for('~>'), Symbol.for('left-printed'), [Symbol.for('print-doc'), Symbol.for('_')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ': any$'], Symbol.for('_'), '']]]], [Symbol.for('list'), 'for', Symbol.for('space'), '(', Symbol.for('left-printed'), Symbol.for('space'), 'of', Symbol.for('space'), Symbol.for('right-printed'), ')', Symbol.for('space'), Symbol.for('body-printed')]];

/**
 * Print a `ForInStatement` ESTree node to a `Doc` object.
 */
function printForInStatement(node: any, options: any = {}): any {
  const language: any = options['language'];
  const left: any = getEstreeField('left', node);
  let leftPrinted: any = printDoc(printNode(left, options), options).replace(new RegExp(';$'), '');
  const right: any = getEstreeField('right', node);
  const rightPrinted: any = printNode(right, options);
  const body: any = getEstreeField('body', node);
  const bodyPrinted: any = printNode(body, options);
  return ['for', space, '(', leftPrinted, space, 'in', space, rightPrinted, (language === 'typescript') ? ' as any[]' : empty, ')', space, bodyPrinted];
}

printForInStatement.fsource = [Symbol.for('define'), [Symbol.for('print-for-in-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('left'), [Symbol.for('get-estree-field'), 'left', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('left-printed'), [Symbol.for('~>'), [Symbol.for('print-node'), Symbol.for('left'), Symbol.for('options')], [Symbol.for('print-doc'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), ';$'], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('right'), [Symbol.for('get-estree-field'), 'right', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('right-printed'), [Symbol.for('print-node'), Symbol.for('right'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-estree-field'), 'body', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]], [Symbol.for('list'), 'for', Symbol.for('space'), '(', Symbol.for('left-printed'), Symbol.for('space'), 'in', Symbol.for('space'), Symbol.for('right-printed'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language'), 'typescript'], ' as any[]', Symbol.for('empty')], ')', Symbol.for('space'), Symbol.for('body-printed')]];

/**
 * Print a `TryStatement` ESTree node to a `Doc` object.
 */
function printTryStatement(node: any, options: any = {}): any {
  const block: any = getEstreeField('block', node);
  const blockPrinted: any = printNode(block, options);
  const handler: any = getEstreeField('handler', node);
  const finalizer: any = getEstreeField('finalizer', node);
  let result: any = ['try', space, blockPrinted];
  if (handler) {
    const handlerParam: any = getEstreeField('param', handler);
    const handlerParamPrinted: any = handlerParam ? printNode(handlerParam, options) : false;
    const handlerBodyPrinted: any = printNode(getEstreeField('body', handler), options);
    result = [...result, ...[space, 'catch', space], ...(handlerParam ? ['(', handlerParamPrinted, ')', space] : []), ...handlerBodyPrinted];
  }
  if (finalizer) {
    const finalizerPrinted: any = printNode(finalizer, options);
    result = [...result, ...[space, 'finally', space, finalizerPrinted]];
  }
  return result;
}

printTryStatement.fsource = [Symbol.for('define'), [Symbol.for('print-try-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('block'), [Symbol.for('get-estree-field'), 'block', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('block-printed'), [Symbol.for('print-node'), Symbol.for('block'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('handler'), [Symbol.for('get-estree-field'), 'handler', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('finalizer'), [Symbol.for('get-estree-field'), 'finalizer', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('list'), 'try', Symbol.for('space'), Symbol.for('block-printed')]], [Symbol.for('when'), Symbol.for('handler'), [Symbol.for('define'), Symbol.for('handler-param'), [Symbol.for('get-estree-field'), 'param', Symbol.for('handler')]], [Symbol.for('define'), Symbol.for('handler-param-printed'), [Symbol.for('if'), Symbol.for('handler-param'), [Symbol.for('print-node'), Symbol.for('handler-param'), Symbol.for('options')], false]], [Symbol.for('define'), Symbol.for('handler-body-printed'), [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'body', Symbol.for('handler')], Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('space'), 'catch', Symbol.for('space')], [Symbol.for('if'), Symbol.for('handler-param'), [Symbol.for('list'), '(', Symbol.for('handler-param-printed'), ')', Symbol.for('space')], [Symbol.for('quote'), []]], Symbol.for('handler-body-printed')]]], [Symbol.for('when'), Symbol.for('finalizer'), [Symbol.for('define'), Symbol.for('finalizer-printed'), [Symbol.for('print-node'), Symbol.for('finalizer'), Symbol.for('options')]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), [Symbol.for('list'), Symbol.for('space'), 'finally', Symbol.for('space'), Symbol.for('finalizer-printed')]]]], Symbol.for('result')];

/**
 * Print a `ClassDeclaration` ESTree node to a `Doc` object.
 */
function printClassDeclaration(node: any, options: any = {}): any {
  const id: any = getEstreeField('id', node);
  const body: any = getEstreeField('body', node);
  const bodyIndented: any = indent(printNode(body, options));
  const bodyPrinted: any = printDoc(bodyIndented, options);
  const superClass: any = getEstreeField('superClass', node);
  return ['class', space, id ? [printNode(id, options), space] : empty, superClass ? ['extends', space, printNode(superClass, options), space] : empty, '{', line, bodyIndented, (bodyPrinted === '') ? empty : line, '}'];
}

printClassDeclaration.fsource = [Symbol.for('define'), [Symbol.for('print-class-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('id'), [Symbol.for('get-estree-field'), 'id', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body'), [Symbol.for('get-estree-field'), 'body', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('body-indented'), [Symbol.for('indent'), [Symbol.for('print-node'), Symbol.for('body'), Symbol.for('options')]]], [Symbol.for('define'), Symbol.for('body-printed'), [Symbol.for('print-doc'), Symbol.for('body-indented'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('super-class'), [Symbol.for('get-estree-field'), 'superClass', Symbol.for('node')]], [Symbol.for('list'), 'class', Symbol.for('space'), [Symbol.for('if'), Symbol.for('id'), [Symbol.for('list'), [Symbol.for('print-node'), Symbol.for('id'), Symbol.for('options')], Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('super-class'), [Symbol.for('list'), 'extends', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('super-class'), Symbol.for('options')], Symbol.for('space')], Symbol.for('empty')], '{', Symbol.for('line'), Symbol.for('body-indented'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('body-printed'), ''], Symbol.for('empty'), Symbol.for('line')], '}']];

/**
 * Print a `ClassExpression` ESTree node to a `Doc` object.
 */
function printClassExpression(node: any, options: any = {}): any {
  return printClassDeclaration(new ClassDeclaration(null, getEstreeField('body', node), getEstreeField('superClass', node)), options);
}

printClassExpression.fsource = [Symbol.for('define'), [Symbol.for('print-class-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-class-declaration'), [Symbol.for('new'), Symbol.for('ClassDeclaration'), null, [Symbol.for('get-estree-field'), 'body', Symbol.for('node')], [Symbol.for('get-estree-field'), 'superClass', Symbol.for('node')]], Symbol.for('options')]];

/**
 * Print a `ClassBody` ESTree node to a `Doc` object.
 */
function printClassBody(node: any, options: any = {}): any {
  return join([line, line], getEstreeField('body', node).map(function (x: any): any {
    return printNode(x, options);
  }));
}

printClassBody.fsource = [Symbol.for('define'), [Symbol.for('print-class-body'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('~>'), Symbol.for('node'), [Symbol.for('get-estree-field'), 'body', Symbol.for('_')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), Symbol.for('line'), Symbol.for('line')], Symbol.for('_')]]];

/**
 * Print a `PropertyDefinition` ESTree node to a `Doc` object.
 */
function printPropertyDefinition(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  const language: any = options['language'];
  const key: any = getEstreeField('key', node);
  const value: any = getEstreeField('value', node);
  const staticFlag: any = getEstreeField('static', node);
  const accessibility: any = getEstreeField('accessibility', node);
  return [((language === 'typescript') && (accessibility === 'private')) ? ['private', space] : empty, staticFlag ? ['static', space] : empty, printNode(key, options), (language === 'typescript') ? [':', space, 'any'] : empty, value ? [space, '=', space, printNode(value, options)] : empty, fsemicolon ? ';' : empty];
}

printPropertyDefinition.fsource = [Symbol.for('define'), [Symbol.for('print-property-definition'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-estree-field'), 'key', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-estree-field'), 'value', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('static-flag'), [Symbol.for('get-estree-field'), 'static', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('accessibility'), [Symbol.for('get-estree-field'), 'accessibility', Symbol.for('node')]], [Symbol.for('list'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'typescript'], [Symbol.for('eq?'), Symbol.for('accessibility'), 'private']], [Symbol.for('list'), 'private', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('static-flag'), [Symbol.for('list'), 'static', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options')], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language'), 'typescript'], [Symbol.for('list'), ':', Symbol.for('space'), 'any'], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('value'), [Symbol.for('list'), Symbol.for('space'), '=', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('value'), Symbol.for('options')]], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]];

/**
 * Print a `MethodDefinition` ESTree node to a `Doc` object.
 */
function printMethodDefinition(node: any, options: any = {}): any {
  const language: any = options['language'];
  const key: any = getEstreeField('key', node);
  let keyPrinted: any = printNode(key, options);
  const keyPrintedStr: any = printDoc(keyPrinted, options);
  const value: any = getEstreeField('value', node);
  const valuePrinted: any = printDoc(printFunction(value, options, {
    returnType: (keyPrintedStr === 'constructor') ? '' : 'any'
  }), options).replace(new RegExp('^function '), '');
  const staticFlag: any = getEstreeField('static', node);
  const computedFlag: any = getEstreeField('computed', node);
  const generatorFlag: any = getEstreeField('generator', value);
  const accessibility: any = getEstreeField('accessibility', node);
  return [((language === 'typescript') && (accessibility === 'private')) ? ['private', space] : empty, staticFlag ? ['static', space] : empty, generatorFlag ? '*' : empty, computedFlag ? ['[', keyPrinted, ']'] : keyPrinted, valuePrinted];
}

printMethodDefinition.fsource = [Symbol.for('define'), [Symbol.for('print-method-definition'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-estree-field'), 'key', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('key-printed'), [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('key-printed-str'), [Symbol.for('print-doc'), Symbol.for('key-printed'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-estree-field'), 'value', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('value-printed'), [Symbol.for('~>'), Symbol.for('value'), [Symbol.for('print-function'), Symbol.for('_'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':return-type'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('key-printed-str'), 'constructor'], '', 'any']]], [Symbol.for('print-doc'), Symbol.for('_'), Symbol.for('options')], [Symbol.for('regexp-replace'), [Symbol.for('regexp'), '^function '], Symbol.for('_'), '']]], [Symbol.for('define'), Symbol.for('static-flag'), [Symbol.for('get-estree-field'), 'static', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('computed-flag'), [Symbol.for('get-estree-field'), 'computed', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('generator-flag'), [Symbol.for('get-estree-field'), 'generator', Symbol.for('value')]], [Symbol.for('define'), Symbol.for('accessibility'), [Symbol.for('get-estree-field'), 'accessibility', Symbol.for('node')]], [Symbol.for('list'), [Symbol.for('if'), [Symbol.for('and'), [Symbol.for('eq?'), Symbol.for('language'), 'typescript'], [Symbol.for('eq?'), Symbol.for('accessibility'), 'private']], [Symbol.for('list'), 'private', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('static-flag'), [Symbol.for('list'), 'static', Symbol.for('space')], Symbol.for('empty')], [Symbol.for('if'), Symbol.for('generator-flag'), '*', Symbol.for('empty')], [Symbol.for('if'), Symbol.for('computed-flag'), [Symbol.for('list'), '[', Symbol.for('key-printed'), ']'], Symbol.for('key-printed')], Symbol.for('value-printed')]];

/**
 * Print an `ArrayExpression` ESTree node to a `Doc` object.
 */
function printArrayExpression(node: any, options: any = {}): any {
  const language: any = options['language'];
  const noImplicitAny: any = options['noImplicitAny'];
  let type_: any = getEstreeField('typeAnnotation', node);
  const printedExpressions: any = [];
  let shouldBreak: any = false;
  let printedExp: any;
  let result: any;
  for (let exp of getEstreeField('elements', node)) {
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
  if (type_ && (language === 'typescript')) {
    result = [...result, ...[':', space, printNode(type_, options)]];
  }
  if (shouldBreak) {
    result = group(result, {
      shouldBreak
    });
  }
  return result;
}

printArrayExpression.fsource = [Symbol.for('define'), [Symbol.for('print-array-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('no-implicit-any'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':no-implicit-any')]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('get-estree-field'), 'typeAnnotation', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('printed-expressions'), [Symbol.for('quote'), []]], [Symbol.for('define'), Symbol.for('should-break'), false], [Symbol.for('define'), Symbol.for('printed-exp')], [Symbol.for('define'), Symbol.for('result')], [Symbol.for('for'), [[Symbol.for('exp'), [Symbol.for('get-estree-field'), 'elements', Symbol.for('node')]]], [Symbol.for('if'), Symbol.for('exp'), [Symbol.for('set!'), Symbol.for('printed-exp'), [Symbol.for('print-node'), Symbol.for('exp'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':no-implicit-any'), false]]]], [Symbol.for('set!'), Symbol.for('printed-exp'), Symbol.for('empty')]], [Symbol.for('push-right!'), Symbol.for('printed-expressions'), [Symbol.for('doc-value-string'), Symbol.for('printed-exp')]], [Symbol.for('when'), [Symbol.for('doc-should-break?'), Symbol.for('printed-exp')], [Symbol.for('set!'), Symbol.for('should-break'), true]]], [Symbol.for('cond'), [Symbol.for('should-break'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '[', Symbol.for('line'), [Symbol.for('~>'), Symbol.for('printed-expressions'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('align'), 1, Symbol.for('x')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('line')], Symbol.for('_')]], Symbol.for('line'), ']']]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('list'), '[', [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('printed-expressions')], ']']]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('no-implicit-any'), [Symbol.for('not'), Symbol.for('type_')]], [Symbol.for('set!'), Symbol.for('type_'), [Symbol.for('new'), Symbol.for('TSArrayType'), [Symbol.for('new'), Symbol.for('TSAnyKeyword')]]]], [Symbol.for('when'), [Symbol.for('and'), Symbol.for('type_'), [Symbol.for('eq?'), Symbol.for('language'), 'typescript']], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('append'), Symbol.for('result'), [Symbol.for('list'), ':', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('type_'), Symbol.for('options')]]]]], [Symbol.for('when'), Symbol.for('should-break'), [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('group'), Symbol.for('result'), [Symbol.for('js/obj'), Symbol.for(':should-break'), Symbol.for('should-break')]]]], Symbol.for('result')];

/**
 * Print an `ArrayPattern` ESTree node to a `Doc` object.
 */
function printArrayPattern(node: any, options: any = {}): any {
  return printArrayExpression(node, options);
}

printArrayPattern.fsource = [Symbol.for('define'), [Symbol.for('print-array-pattern'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-array-expression'), Symbol.for('node'), Symbol.for('options')]];

/**
 * Print a `NewExpression` ESTree node to a `Doc` object.
 */
function printNewExpression(node: any, options: any = {}): any {
  return ['new', space, printNode(new CallExpression(getEstreeField('callee', node), getEstreeField('arguments', node)), options)];
}

printNewExpression.fsource = [Symbol.for('define'), [Symbol.for('print-new-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('list'), 'new', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('new'), Symbol.for('CallExpression'), [Symbol.for('get-estree-field'), 'callee', Symbol.for('node')], [Symbol.for('get-estree-field'), 'arguments', Symbol.for('node')]], Symbol.for('options')]]];

/**
 * Print an `ImportDeclaration` ESTree node to a `Doc` object.
 */
function printImportDeclaration(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  const specifiers: any = getEstreeField('specifiers', node);
  const source: any = getEstreeField('source', node);
  if ((specifiers.length === 1) && !estreeTypeP(specifiers[0], 'ImportSpecifier')) {
    return ['import', space, printNode(specifiers[0], options), space, 'from', space, printNode(source, options), fsemicolon ? ';' : empty];
  } else {
    return ['import', space, '{', line, indent(join([',', line], specifiers.map(function (x: any): any {
      return printNode(x, options);
    }))), line, '}', space, 'from', space, printNode(source, options), fsemicolon ? ';' : empty];
  }
}

printImportDeclaration.fsource = [Symbol.for('define'), [Symbol.for('print-import-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('get-estree-field'), 'specifiers', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('source'), [Symbol.for('get-estree-field'), 'source', Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('='), [Symbol.for('js/length'), Symbol.for('specifiers')], 1], [Symbol.for('not'), [Symbol.for('estree-type?'), [Symbol.for('first'), Symbol.for('specifiers')], 'ImportSpecifier']]], [Symbol.for('list'), 'import', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('first'), Symbol.for('specifiers')], Symbol.for('options')], Symbol.for('space'), 'from', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('source'), Symbol.for('options')], [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]], [Symbol.for('else'), [Symbol.for('list'), 'import', Symbol.for('space'), '{', Symbol.for('line'), [Symbol.for('~>'), Symbol.for('specifiers'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('line')], Symbol.for('_')], [Symbol.for('indent'), Symbol.for('_')]], Symbol.for('line'), '}', Symbol.for('space'), 'from', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('source'), Symbol.for('options')], [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]]]];

/**
 * Print an `ImportSpecifier` ESTree node to a `Doc` object.
 */
function printImportSpecifier(node: any, options: any = {}): any {
  const local: any = getEstreeField('local', node);
  const localPrinted: any = printDoc(printNode(local, options), options);
  const imported: any = getEstreeField('imported', node);
  const importedPrinted: any = printDoc(printNode(imported, options), options);
  if (localPrinted === importedPrinted) {
    return localPrinted;
  } else {
    return [localPrinted, space, 'as', space, importedPrinted];
  }
}

printImportSpecifier.fsource = [Symbol.for('define'), [Symbol.for('print-import-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('local'), [Symbol.for('get-estree-field'), 'local', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('local-printed'), [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('local'), Symbol.for('options')], Symbol.for('options')]], [Symbol.for('define'), Symbol.for('imported'), [Symbol.for('get-estree-field'), 'imported', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('imported-printed'), [Symbol.for('print-doc'), [Symbol.for('print-node'), Symbol.for('imported'), Symbol.for('options')], Symbol.for('options')]], [Symbol.for('cond'), [[Symbol.for('eq?'), Symbol.for('local-printed'), Symbol.for('imported-printed')], Symbol.for('local-printed')], [Symbol.for('else'), [Symbol.for('list'), Symbol.for('local-printed'), Symbol.for('space'), 'as', Symbol.for('space'), Symbol.for('imported-printed')]]]];

/**
 * Print an `ImportDefaultSpecifier` ESTree node to a `Doc` object.
 */
function printImportDefaultSpecifier(node: any, options: any = {}): any {
  return printNode(getEstreeField('local', node), options);
}

printImportDefaultSpecifier.fsource = [Symbol.for('define'), [Symbol.for('print-import-default-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'local', Symbol.for('node')], Symbol.for('options')]];

/**
 * Print an `ImportNamespaceSpecifier` ESTree node to a `Doc` object.
 */
function printImportNamespaceSpecifier(node: any, options: any = {}): any {
  return ['*', space, 'as', space, printNode(getEstreeField('local', node), options)];
}

printImportNamespaceSpecifier.fsource = [Symbol.for('define'), [Symbol.for('print-import-namespace-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('list'), '*', Symbol.for('space'), 'as', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'local', Symbol.for('node')], Symbol.for('options')]]];

/**
 * Print an `ExportNamedDeclaration` ESTree node to a `Doc` object.
 */
function printExportNamedDeclaration(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  const specifiers: any = getEstreeField('specifiers', node);
  const specifiersPrinted: any = printDoc(indent(join([',', line], specifiers.map(function (x: any): any {
    return printNode(x, options);
  }))), options);
  return ['export', space, '{', line, specifiersPrinted, (specifiersPrinted === '') ? empty : line, '}', fsemicolon ? ';' : empty];
}

printExportNamedDeclaration.fsource = [Symbol.for('define'), [Symbol.for('print-export-named-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('define'), Symbol.for('specifiers'), [Symbol.for('get-estree-field'), 'specifiers', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('specifiers-printed'), [Symbol.for('~>'), Symbol.for('specifiers'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('line')], Symbol.for('_')], [Symbol.for('indent')], [Symbol.for('print-doc'), Symbol.for('options')]]], [Symbol.for('list'), 'export', Symbol.for('space'), '{', Symbol.for('line'), Symbol.for('specifiers-printed'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('specifiers-printed'), ''], Symbol.for('empty'), Symbol.for('line')], '}', [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]];

/**
 * Print an `ExportSpecifier` ESTree node to a `Doc` object.
 */
function printExportSpecifier(node: any, options: any = {}): any {
  return printImportSpecifier(new ImportSpecifier(getEstreeField('local', node), getEstreeField('exported', node)), options);
}

printExportSpecifier.fsource = [Symbol.for('define'), [Symbol.for('print-export-specifier'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-import-specifier'), [Symbol.for('new'), Symbol.for('ImportSpecifier'), [Symbol.for('get-estree-field'), 'local', Symbol.for('node')], [Symbol.for('get-estree-field'), 'exported', Symbol.for('node')]], Symbol.for('options')]];

/**
 * Print an `ExportAllDeclaration` ESTree node to a `Doc` object.
 */
function printExportAllDeclaration(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  return ['export', space, '*', space, 'from', space, printNode(getEstreeField('source', node), options), fsemicolon ? ';' : empty];
}

printExportAllDeclaration.fsource = [Symbol.for('define'), [Symbol.for('print-export-all-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('list'), 'export', Symbol.for('space'), '*', Symbol.for('space'), 'from', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'source', Symbol.for('node')], Symbol.for('options')], [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]];

/**
 * Print an `ObjectExpression` ESTree node to a `Doc` object.
 */
function printObjectExpression(node: any, options: any = {}): any {
  const properties: any = getEstreeField('properties', node);
  return ['{', (properties.length === 0) ? empty : [line, indent(join([',', line], properties.map(function (x: any): any {
    return printNode(x, options);
  }))), line], '}'];
}

printObjectExpression.fsource = [Symbol.for('define'), [Symbol.for('print-object-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('properties'), [Symbol.for('get-estree-field'), 'properties', Symbol.for('node')]], [Symbol.for('list'), '{', [Symbol.for('if'), [Symbol.for('='), [Symbol.for('js/length'), Symbol.for('properties')], 0], Symbol.for('empty'), [Symbol.for('list'), Symbol.for('line'), [Symbol.for('~>'), Symbol.for('properties'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('line')], Symbol.for('_')], [Symbol.for('indent')]], Symbol.for('line')]], '}']];

/**
 * Print an `ObjectPattern` ESTree node to a `Doc` object.
 */
function printObjectPattern(node: any, options: any = {}): any {
  return ['{', join([',', space], getEstreeField('properties', node).map(function (prop: any): any {
    return printAssignmentProperty(prop, options);
  })), '}'];
}

printObjectPattern.fsource = [Symbol.for('define'), [Symbol.for('print-object-pattern'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('list'), '{', [Symbol.for('~>'), [Symbol.for('get-estree-field'), 'properties', Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('prop')], [Symbol.for('print-assignment-property'), Symbol.for('prop'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], '}']];

/**
 * Print an `AssignmentProperty` ESTree node to a `Doc` object.
 */
function printAssignmentProperty(node: any, options: any = {}): any {
  const options1: any = {
    ...options,
    noImplicitAny: false
  };
  const key: any = getEstreeField('key', node);
  let keyPrinted: any = printNode(key, options1);
  const keyPrintedStr: any = printDoc(keyPrinted, options1);
  const value: any = getEstreeField('value', node);
  const valuePrinted: any = printNode(value, options1);
  const valuePrintedStr: any = printDoc(valuePrinted, options1);
  if (keyPrintedStr === valuePrintedStr) {
    return keyPrinted;
  } else {
    return [keyPrinted, ':', space, valuePrinted];
  }
}

printAssignmentProperty.fsource = [Symbol.for('define'), [Symbol.for('print-assignment-property'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('options1'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':no-implicit-any'), false]]], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-estree-field'), 'key', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('key-printed'), [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options1')]], [Symbol.for('define'), Symbol.for('key-printed-str'), [Symbol.for('print-doc'), Symbol.for('key-printed'), Symbol.for('options1')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-estree-field'), 'value', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('value-printed'), [Symbol.for('print-node'), Symbol.for('value'), Symbol.for('options1')]], [Symbol.for('define'), Symbol.for('value-printed-str'), [Symbol.for('print-doc'), Symbol.for('value-printed'), Symbol.for('options1')]], [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('key-printed-str'), Symbol.for('value-printed-str')], Symbol.for('key-printed'), [Symbol.for('list'), Symbol.for('key-printed'), ':', Symbol.for('space'), Symbol.for('value-printed')]]];

/**
 * Print a `Property` ESTree node to a `Doc` object.
 */
function printProperty(node: any, options: any = {}): any {
  const language: any = options['language'];
  const key: any = getEstreeField('key', node);
  let keyPrinted: any = printNode(key, options);
  const value: any = getEstreeField('value', node);
  const computed: any = getEstreeField('computed', node);
  const shorthand: any = getEstreeField('shorthand', node);
  if (computed) {
    keyPrinted = ['[', keyPrinted, (language === 'typescript') ? [space, 'as any'] : empty, ']'];
  }
  return [keyPrinted, shorthand ? empty : [':', space, printNode(value, options)]];
}

printProperty.fsource = [Symbol.for('define'), [Symbol.for('print-property'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('language'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':language')]], [Symbol.for('define'), Symbol.for('key'), [Symbol.for('get-estree-field'), 'key', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('key-printed'), [Symbol.for('print-node'), Symbol.for('key'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('value'), [Symbol.for('get-estree-field'), 'value', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('computed'), [Symbol.for('get-estree-field'), 'computed', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('shorthand'), [Symbol.for('get-estree-field'), 'shorthand', Symbol.for('node')]], [Symbol.for('when'), Symbol.for('computed'), [Symbol.for('set!'), Symbol.for('key-printed'), [Symbol.for('list'), '[', Symbol.for('key-printed'), [Symbol.for('if'), [Symbol.for('eq?'), Symbol.for('language'), 'typescript'], [Symbol.for('list'), Symbol.for('space'), 'as any'], Symbol.for('empty')], ']']]], [Symbol.for('list'), Symbol.for('key-printed'), [Symbol.for('if'), Symbol.for('shorthand'), Symbol.for('empty'), [Symbol.for('list'), ':', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('value'), Symbol.for('options')]]]]];

/**
 * Print a `Program` ESTree node to a `Doc` object.
 */
function printProgram(node: any, options: any = {}): any {
  return join([line, line], getEstreeField('body', node).map(function (x: any): any {
    return printNode(x, options);
  }));
}

printProgram.fsource = [Symbol.for('define'), [Symbol.for('print-program'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('~>'), [Symbol.for('get-estree-field'), 'body', Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), Symbol.for('line'), Symbol.for('line')], Symbol.for('_')]]];

/**
 * Print a `SwitchStatement` ESTree node to a `Doc` object.
 */
function printSwitchStatement(node: any, options: any = {}): any {
  const discriminant: any = getEstreeField('discriminant', node);
  const discriminantPrinted: any = printNode(discriminant, options);
  const cases: any = getEstreeField('cases', node);
  const casesPrinted: any = indent(join(line, cases.map(function (x: any): any {
    return printNode(x, options);
  })));
  return ['switch', space, '(', discriminantPrinted, ')', space, '{', line, casesPrinted, line, '}'];
}

printSwitchStatement.fsource = [Symbol.for('define'), [Symbol.for('print-switch-statement'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('discriminant'), [Symbol.for('get-estree-field'), 'discriminant', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('discriminant-printed'), [Symbol.for('print-node'), Symbol.for('discriminant'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('cases'), [Symbol.for('get-estree-field'), 'cases', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('cases-printed'), [Symbol.for('~>'), Symbol.for('cases'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), Symbol.for('line'), Symbol.for('_')], [Symbol.for('indent'), Symbol.for('_')]]], [Symbol.for('list'), 'switch', Symbol.for('space'), '(', Symbol.for('discriminant-printed'), ')', Symbol.for('space'), '{', Symbol.for('line'), Symbol.for('cases-printed'), Symbol.for('line'), '}']];

/**
 * Print a `SwitchCase` ESTree node to a `Doc` object.
 */
function printSwitchCase(node: any, options: any = {}): any {
  const test: any = getEstreeField('test', node);
  let testPrinted: any = test ? ['case', space, printNode(test, options)] : 'default';
  const consequent: any = getEstreeField('consequent', node);
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

printSwitchCase.fsource = [Symbol.for('define'), [Symbol.for('print-switch-case'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('test'), [Symbol.for('get-estree-field'), 'test', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('test-printed'), [Symbol.for('cond'), [Symbol.for('test'), [Symbol.for('list'), 'case', Symbol.for('space'), [Symbol.for('print-node'), Symbol.for('test'), Symbol.for('options')]]], [Symbol.for('else'), 'default']]], [Symbol.for('define'), Symbol.for('consequent'), [Symbol.for('get-estree-field'), 'consequent', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('is-block-statement'), [Symbol.for('and'), [Symbol.for('='), [Symbol.for('js/length'), Symbol.for('consequent')], 1], [Symbol.for('first'), Symbol.for('consequent')], [Symbol.for('estree-type?'), [Symbol.for('first'), Symbol.for('consequent')], 'BlockStatement']]], [Symbol.for('define'), Symbol.for('consequent-printed'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('consequent')]], [Symbol.for('cond'), [Symbol.for('is-block-statement'), [Symbol.for('set!'), Symbol.for('consequent-printed'), [Symbol.for('list'), Symbol.for('space'), [Symbol.for('first'), Symbol.for('consequent-printed')]]]], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('consequent-printed'), [Symbol.for('list'), Symbol.for('line'), [Symbol.for('~>'), Symbol.for('consequent-printed'), [Symbol.for('join'), Symbol.for('line'), Symbol.for('_')], [Symbol.for('indent'), Symbol.for('_')]]]]]], [Symbol.for('list'), Symbol.for('test-printed'), ':', Symbol.for('consequent-printed')]];

/**
 * Print a `TSAsExpression` TSESTree node to a `Doc` object.
 */
function printTsAsExpression(node: any, options: any = {}): any {
  const expression: any = getEstreeField('expression', node);
  let expressionPrinted: any = printNode(expression, options);
  const typeAnnotation: any = getEstreeField('typeAnnotation', node);
  const typeAnnotationPrinted: any = printTsType(typeAnnotation, options);
  return [expressionPrinted, space, 'as', space, typeAnnotationPrinted];
}

printTsAsExpression.fsource = [Symbol.for('define'), [Symbol.for('print-ts-as-expression'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('expression'), [Symbol.for('get-estree-field'), 'expression', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('expression-printed'), [Symbol.for('print-node'), Symbol.for('expression'), Symbol.for('options')]], [Symbol.for('define'), Symbol.for('type-annotation'), [Symbol.for('get-estree-field'), 'typeAnnotation', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('type-annotation-printed'), [Symbol.for('print-ts-type'), Symbol.for('type-annotation'), Symbol.for('options')]], [Symbol.for('list'), Symbol.for('expression-printed'), Symbol.for('space'), 'as', Symbol.for('space'), Symbol.for('type-annotation-printed')]];

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

printTsType.fsource = [Symbol.for('define'), [Symbol.for('print-ts-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('type_'), [Symbol.for('estree-type'), Symbol.for('node')]], [Symbol.for('cond'), [[Symbol.for('hash-has-key?'), Symbol.for('printer-map'), Symbol.for('type_')], [Symbol.for('print-node'), Symbol.for('node'), Symbol.for('options')]], [Symbol.for('else'), 'any']]];

/**
 * Print a `TSAnyKeyword` TSESTree node to a `Doc` object.
 */
function printTsAnyKeyword(node: any, options: any = {}): any {
  return 'any';
}

printTsAnyKeyword.fsource = [Symbol.for('define'), [Symbol.for('print-ts-any-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], 'any'];

/**
 * Print a `TSVoidKeyword` TSESTree node to a `Doc` object.
 */
function printTsVoidKeyword(node: any, options: any = {}): any {
  return 'void';
}

printTsVoidKeyword.fsource = [Symbol.for('define'), [Symbol.for('print-ts-void-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], 'void'];

/**
 * Print a `TSUndefinedKeyword` TSESTree node to a `Doc` object.
 */
function printTsUndefinedKeyword(node: any, options: any = {}): any {
  return 'undefined';
}

printTsUndefinedKeyword.fsource = [Symbol.for('define'), [Symbol.for('print-ts-undefined-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], 'undefined'];

/**
 * Print a `TSBooleanKeyword` TSESTree node to a `Doc` object.
 */
function printTsBooleanKeyword(node: any, options: any = {}): any {
  return 'boolean';
}

printTsBooleanKeyword.fsource = [Symbol.for('define'), [Symbol.for('print-ts-boolean-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], 'boolean'];

/**
 * Print a `TSNumberKeyword` TSESTree node to a `Doc` object.
 */
function printTsNumberKeyword(node: any, options: any = {}): any {
  return 'number';
}

printTsNumberKeyword.fsource = [Symbol.for('define'), [Symbol.for('print-ts-number-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], 'number'];

/**
 * Print a `TSStringKeyword` TSESTree node to a `Doc` object.
 */
function printTsStringKeyword(node: any, options: any = {}): any {
  return 'string';
}

printTsStringKeyword.fsource = [Symbol.for('define'), [Symbol.for('print-ts-string-keyword'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], 'string'];

/**
 * Print a `TSArrayType` TSESTree node to a `Doc` object.
 */
function printTsArrayType(node: any, options: any = {}): any {
  const elementType: any = getEstreeField('elementType', node);
  let result: any = printNode(elementType, options);
  if (estreeTypeP(elementType, 'TSUnionType')) {
    result = docWrap(result, options);
  }
  return [result, '[]'];
}

printTsArrayType.fsource = [Symbol.for('define'), [Symbol.for('print-ts-array-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('element-type'), [Symbol.for('get-estree-field'), 'elementType', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('print-node'), Symbol.for('element-type'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('element-type'), 'TSUnionType'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('doc-wrap'), Symbol.for('result'), Symbol.for('options')]]], [Symbol.for('list'), Symbol.for('result'), '[]']];

/**
 * Print a `TSTupleType` TSESTree node to a `Doc` object.
 */
function printTsTupleType(node: any, options: any = {}): any {
  const elementTypes: any = getEstreeField('elementTypes', node);
  return ['[', join([',', space], elementTypes.map(function (x: any): any {
    return printNode(x, options);
  })), ']'];
}

printTsTupleType.fsource = [Symbol.for('define'), [Symbol.for('print-ts-tuple-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('element-types'), [Symbol.for('get-estree-field'), 'elementTypes', Symbol.for('node')]], [Symbol.for('list'), '[', [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('element-types')]], ']']];

/**
 * Print a `TSUnionType` TSESTree node to a `Doc` object.
 */
function printTsUnionType(node: any, options: any = {}): any {
  return join([space, '|', space], getEstreeField('types', node).map(function (x: any): any {
    let result: any = printNode(x, options);
    if (estreeTypeP(x, 'TSUnionType')) {
      result = docWrap(result, options);
    }
    return result;
  }));
}

printTsUnionType.fsource = [Symbol.for('define'), [Symbol.for('print-ts-union-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('~>'), [Symbol.for('get-estree-field'), 'types', Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('result'), [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], [Symbol.for('when'), [Symbol.for('estree-type?'), Symbol.for('x'), 'TSUnionType'], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('doc-wrap'), Symbol.for('result'), Symbol.for('options')]]], Symbol.for('result')], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), Symbol.for('space'), '|', Symbol.for('space')], Symbol.for('_')]]];

/**
 * Print a `TSFunctionType` TSESTree node to a `Doc` object.
 */
function printTsFunctionType(node: any, options: any = {}): any {
  return ['(', join([',', space], getEstreeField('params', node).map(function (x: any): any {
    return printNode(x, options);
  })), ')', space, '=>', space, printNode(getEstreeField('returnType', node), options)];
}

printTsFunctionType.fsource = [Symbol.for('define'), [Symbol.for('print-ts-function-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('list'), '(', [Symbol.for('~>'), [Symbol.for('get-estree-field'), 'params', Symbol.for('node')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-node'), Symbol.for('x'), Symbol.for('options')]], Symbol.for('_')], [Symbol.for('join'), [Symbol.for('list'), ',', Symbol.for('space')], Symbol.for('_')]], ')', Symbol.for('space'), '=>', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'returnType', Symbol.for('node')], Symbol.for('options')]]];

/**
 * Print a `TSTypeAliasDeclaration` TSESTree node to a `Doc` object.
 */
function printTsTypeAliasDeclaration(node: any, options: any = {}): any {
  const fsemicolon: any = options['fsemicolon'];
  return ['type', space, printNode(getEstreeField('id', node), options), space, '=', space, printNode(getEstreeField('typeAnnotation', node), options), fsemicolon ? ';' : empty];
}

printTsTypeAliasDeclaration.fsource = [Symbol.for('define'), [Symbol.for('print-ts-type-alias-declaration'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('fsemicolon'), [Symbol.for('oget'), Symbol.for('options'), Symbol.for(':fsemicolon')]], [Symbol.for('list'), 'type', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'id', Symbol.for('node')], Symbol.for('options')], Symbol.for('space'), '=', Symbol.for('space'), [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'typeAnnotation', Symbol.for('node')], Symbol.for('options')], [Symbol.for('if'), Symbol.for('fsemicolon'), ';', Symbol.for('empty')]]];

/**
 * Print a `TSTypeAnnotation` TSESTree node to a `Doc` object.
 */
function printTsTypeAnnotation(node: any, options: any = {}): any {
  return printNode(getEstreeField('typeAnnotation', node), options);
}

printTsTypeAnnotation.fsource = [Symbol.for('define'), [Symbol.for('print-ts-type-annotation'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'typeAnnotation', Symbol.for('node')], Symbol.for('options')]];

/**
 * Print a `TSLiteralType` TSESTree node to a `Doc` object.
 */
function printTsLiteralType(node: any, options: any = {}): any {
  return printNode(getEstreeField('literal', node), {
    ...options,
    noImplicitAny: false
  });
}

printTsLiteralType.fsource = [Symbol.for('define'), [Symbol.for('print-ts-literal-type'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('print-node'), [Symbol.for('get-estree-field'), 'literal', Symbol.for('node')], [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':no-implicit-any'), false]]]];

/**
 * Print a `TSTypeReference` TSESTree node to a `Doc` object.
 */
function printTsTypeReference(node: any, options: any = {}): any {
  const name: any = getEstreeField('typeName', node);
  const params: any = getEstreeField('typeParameters', node);
  return [printNode(name, {
    ...options,
    noImplicitAny: false
  }), params ? printNode(params, {
    ...options,
    noImplicitAny: false
  }) : empty];
}

printTsTypeReference.fsource = [Symbol.for('define'), [Symbol.for('print-ts-type-reference'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('name'), [Symbol.for('get-estree-field'), 'typeName', Symbol.for('node')]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('get-estree-field'), 'typeParameters', Symbol.for('node')]], [Symbol.for('list'), [Symbol.for('print-node'), Symbol.for('name'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':no-implicit-any'), false]]], [Symbol.for('if'), Symbol.for('params'), [Symbol.for('print-node'), Symbol.for('params'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':no-implicit-any'), false]]], Symbol.for('empty')]]];

/**
 * Print a `TSTypeParameterInstantiation` TSESTree node to a `Doc` object.
 */
function printTsTypeParameterInstantiation(node: any, options: any = {}): any {
  const params: any = getEstreeField('params', node);
  return ['<', join(',', params.map(function (x: any): any {
    return printTsType(x, {
      ...options,
      noImplicitAny: false
    });
  })), '>'];
}

printTsTypeParameterInstantiation.fsource = [Symbol.for('define'), [Symbol.for('print-ts-type-parameter-instantiation'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('params'), [Symbol.for('get-estree-field'), 'params', Symbol.for('node')]], [Symbol.for('list'), '<', [Symbol.for('~>'), Symbol.for('params'), [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('print-ts-type'), Symbol.for('x'), [Symbol.for('js/obj-append'), Symbol.for('options'), [Symbol.for('js/obj'), Symbol.for(':no-implicit-any'), false]]]], Symbol.for('_')], [Symbol.for('join'), ',', Symbol.for('_')]], '>']];

/**
 * Print an `XRawJavaScript` ESTree extension node to a `Doc` object.
 */
function printXRawJavascript(node: any, options: any = {}): any {
  let str: any = getEstreeField('js', node);
  if (str.match(new RegExp('^function \\('))) {
    str = docWrap(str);
  }
  return str;
}

printXRawJavascript.fsource = [Symbol.for('define'), [Symbol.for('print-x-raw-javascript'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], [Symbol.for('define'), Symbol.for('str'), [Symbol.for('get-estree-field'), 'js', Symbol.for('node')]], [Symbol.for('when'), [Symbol.for('regexp-match'), [Symbol.for('regexp'), '^function \\('], Symbol.for('str')], [Symbol.for('set!'), Symbol.for('str'), [Symbol.for('doc-wrap'), Symbol.for('str')]]], Symbol.for('str')];

/**
 * Default printer.
 *
 * Returns the empty string.
 */
function defaultPrinter(node: any, options: any = {}): any {
  return empty;
}

defaultPrinter.fsource = [Symbol.for('define'), [Symbol.for('default-printer'), Symbol.for('node'), [Symbol.for('options'), [Symbol.for('js/obj')]]], Symbol.for('empty')];

/**
 * Add `default-options` to an options object.
 */
function addDefaultOptions(options: any): any {
  return {
    ...defaultOptions,
    ...options
  };
}

addDefaultOptions.fsource = [Symbol.for('define'), [Symbol.for('add-default-options'), Symbol.for('options')], [Symbol.for('js/obj-append'), Symbol.for('default-options'), Symbol.for('options')]];

/**
 * Default printing options.
 */
const defaultOptions: any = {
  fsemicolon: true
};

/**
 * Mapping from node types to printer functions.
 */
const printerMap: any = new Map([['ArrayExpression', printArrayExpression], ['ArrayPattern', printArrayPattern], ['ArrowFunctionExpression', printArrowFunctionExpression], ['AssignmentExpression', printAssignmentExpression], ['AssignmentPattern', printAssignmentPattern], ['AwaitExpression', printAwaitExpression], ['BinaryExpression', printBinaryExpression], ['BlockStatement', printBlockStatement], ['BreakStatement', printBreakStatement], ['CallExpression', printCallExpression], ['ClassBody', printClassBody], ['ClassDeclaration', printClassDeclaration], ['ClassExpression', printClassExpression], ['ConditionalExpression', printConditionalExpression], ['ContinueStatement', printContinueStatement], ['DoWhileStatement', printDoWhileStatement], ['ExportAllDeclaration', printExportAllDeclaration], ['ExportNamedDeclaration', printExportNamedDeclaration], ['ExportSpecifier', printExportSpecifier], ['ExpressionStatement', printExpressionStatement], ['ForInStatement', printForInStatement], ['ForOfStatement', printForOfStatement], ['ForStatement', printForStatement], ['FunctionDeclaration', printFunctionDeclaration], ['FunctionExpression', printFunctionExpression], ['Identifier', printIdentifier], ['IfStatement', printIfStatement], ['ImportDeclaration', printImportDeclaration], ['ImportDefaultSpecifier', printImportDefaultSpecifier], ['ImportNamespaceSpecifier', printImportNamespaceSpecifier], ['ImportSpecifier', printImportSpecifier], ['Literal', printLiteral], ['LogicalExpression', printLogicalExpression], ['MemberExpression', printMemberExpression], ['MethodDefinition', printMethodDefinition], ['NewExpression', printNewExpression], ['ObjectExpression', printObjectExpression], ['ObjectPattern', printObjectPattern], ['Program', printProgram], ['Property', printProperty], ['PropertyDefinition', printPropertyDefinition], ['RestElement', printRestElement], ['ReturnStatement', printReturnStatement], ['SequenceExpression', printSequenceExpression], ['SpreadElement', printSpreadElement], ['SwitchStatement', printSwitchStatement], ['SwitchCase', printSwitchCase], ['TSAnyKeyword', printTsAnyKeyword], ['TSArrayType', printTsArrayType], ['TSAsExpression', printTsAsExpression], ['TSBooleanKeyword', printTsBooleanKeyword], ['TSFunctionType', printTsFunctionType], ['TSLiteralType', printTsLiteralType], ['TSNumberKeyword', printTsNumberKeyword], ['TSStringKeyword', printTsStringKeyword], ['TSTupleType', printTsTupleType], ['TSTypeAliasDeclaration', printTsTypeAliasDeclaration], ['TSTypeAnnotation', printTsTypeAnnotation], ['TSTypeParameterInstantiation', printTsTypeParameterInstantiation], ['TSTypeReference', printTsTypeReference], ['TSUndefinedKeyword', printTsUndefinedKeyword], ['TSUnionType', printTsUnionType], ['TSVoidKeyword', printTsVoidKeyword], ['TaggedTemplateExpression', printTaggedTemplateExpression], ['TemplateElement', printTemplateElement], ['TemplateLiteral', printTemplateLiteral], ['ThisExpression', printThisExpression], ['ThrowStatement', printThrowStatement], ['TryStatement', printTryStatement], ['UnaryExpression', printUnaryExpression], ['UpdateExpression', printUpdateExpression], ['VariableDeclaration', printVariableDeclaration], ['VariableDeclarator', printVariableDeclarator], ['WhileStatement', printWhileStatement], ['YieldExpression', printYieldExpression], ['XRawJavaScript', printXRawJavascript]] as any);

export {
  printSyntax as printRose,
  printNode as printEstreeNode,
  printSexpAsExpression as printAsExpression,
  print,
  printEstree,
  printNode,
  printSyntax,
  printSexp,
  printSexpAsExpression,
  writeToString
};