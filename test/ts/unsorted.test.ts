/**
 * # Various unsorted tests
 *
 * This file functions as an "inbox" for incoming tests.
 */

import {
  assertEqual,
  testRepl,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('To do', function (): any {
});

describe('sqrt', function (): any {
  xit('(sqrt 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('sqrt'), 4], 2]);
  });
  return xit('(compile \'(sqrt 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('sqrt'), 4]]], 'Math.sqrt(4);']);
  });
});

describe('expt', function (): any {
  xit('(expt 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('expt'), 2, 3], 8]);
  });
  return xit('(compile \'(expt 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('expt'), 2, 3]]], '2 ** 3;']);
  });
});

describe('pow', function (): any {
  xit('(pow 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('pow'), 2, 3], 8]);
  });
  return xit('(compile \'(pow 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('pow'), 2, 3]]], '2 ** 3;']);
  });
});

describe('js/**', function (): any {
  xit('(js/** 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('js/**'), 2, 3], 8]);
  });
  return xit('(compile \'(js/** 2 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/**'), 2, 3]]], '2 ** 3;']);
  });
});

describe('js/eval', function (): any {
  it('(js/eval "1 + 1;")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/eval'), '1 + 1;'], 2]);
  });
  it('(compile \'(js/eval "1 + 1;"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/eval'), '1 + 1;']]], 'eval(\'1 + 1;\');']);
  });
  it('(compile \'(js/eval "1  +  1;"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/eval'), '1  +  1;']]], 'eval(\'1  +  1;\');']);
  });
  it('(compile \'(js/eval "1 + 1;") :to "javascript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/eval'), '1 + 1;']], Symbol.for(':to'), 'javascript'], 'eval(\'1 + 1;\');']);
  });
  return it('(compile \'(js/eval "1 + 1;") :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/eval'), '1 + 1;']], Symbol.for(':to'), 'typescript'], 'eval(\'1 + 1;\');']);
  });
});

describe('js/raw', function (): any {
  it('(js/raw "1 + 1;")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/raw'), '1 + 1;'], 2]);
  });
  it('(compile \'(js/raw "1 + 1;"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/raw'), '1 + 1;']]], '1 + 1;']);
  });
  it('(compile \'(js/raw "1  +  1;"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/raw'), '1  +  1;']]], '1  +  1;']);
  });
  it('(compile \'(js/raw "1 + 1;") :to "javascript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/raw'), '1 + 1;']], Symbol.for(':to'), 'javascript'], '1 + 1;']);
  });
  return it('(compile \'(js/raw "1 + 1;") :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/raw'), '1 + 1;']], Symbol.for(':to'), 'typescript'], '1 + 1;']);
  });
});

describe('interpret', function (): any {
  it('(interpret 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('interpret'), 1], 1]);
  });
  it('(interpret \'\'foo)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('interpret'), [Symbol.for('quote'), [Symbol.for('quote'), Symbol.for('foo')]]], [Symbol.for('quote'), Symbol.for('foo')]]);
  });
  it('(interpret \'(second \'(1 . (2 . ()))) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('interpret'), [Symbol.for('quote'), [Symbol.for('second'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]]], Symbol.for(':fdottedlists'), true], 2]);
  });
  it('(compile \'(module m scheme (interpret 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('interpret'), 1]]]], 'import {\n' +
      '  interpret\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'interpret(1);']);
  });
  it('(compile \'(module m scheme (eval 1)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('eval'), 1]]]], 'import {\n' +
      '  interpret\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'interpret(1);']);
  });
  return it('(compile \'(module m scheme (js/eval "1;")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('js/eval'), '1;']]]], 'eval(\'1;\');']);
  });
});

describe('Dot', function (): any {
  it('\'.', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('quote'), Symbol.for('.')], [Symbol.for('quote'), Symbol.for('.')]]);
  });
  it('(array-ref \'(1 . 2) 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('array-ref'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]], 1], [Symbol.for('quote'), Symbol.for('.')]]);
  });
  return it('(compile \'.)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), Symbol.for('.')]], 'Symbol.for(\'.\');']);
  });
});

describe('Assignment operators', function (): any {
  xit('(compile \'(js/+= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/+='), Symbol.for('x'), Symbol.for('y')]]], 'x += y;']);
  });
  xit('(compile \'(js/-= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/-='), Symbol.for('x'), Symbol.for('y')]]], 'x -= y;']);
  });
  xit('(compile \'(js/*= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/*='), Symbol.for('x'), Symbol.for('y')]]], 'x *= y;']);
  });
  xit('(compile \'(js//= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js//='), Symbol.for('x'), Symbol.for('y')]]], 'x /= y;']);
  });
  xit('(compile \'(js/^= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/^='), Symbol.for('x'), Symbol.for('y')]]], 'x ^= y;']);
  });
  xit('(compile \'(js/&= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/&='), Symbol.for('x'), Symbol.for('y')]]], 'x &= y;']);
  });
  xit('(compile \'(js/|= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/|='), Symbol.for('x'), Symbol.for('y')]]], 'x |= y;']);
  });
  xit('(compile \'(js/<<= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/<<='), Symbol.for('x'), Symbol.for('y')]]], 'x <<= y;']);
  });
  xit('(compile \'(js/>>= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>>='), Symbol.for('x'), Symbol.for('y')]]], 'x >>= y;']);
  });
  return xit('(compile \'(js/>>>= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>>>='), Symbol.for('x'), Symbol.for('y')]]], 'x >>>= y;']);
  });
});

describe('js/iife', function (): any {
  return xit('(compile \'(js/iife (js/arrow (x . y) (+ x (first y))) (list* a b)) :as \'statement)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), [Symbol.for('first'), Symbol.for('y')]]], [Symbol.for('list*'), Symbol.for('a'), Symbol.for('b')]]], Symbol.for(':as'), [Symbol.for('quote'), Symbol.for('statement')]], 'let x = a;\n' +
      '\n' +
      'let y = b;\n' +
      '\n' +
      'x + y[0];']);
  });
});

describe('parse', function (): any {
  it('(parse "foo")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('parse'), 'foo'], [Symbol.for('quote'), Symbol.for('foo')]]);
  });
  it('(parse "(foo)")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('parse'), '(foo)'], [Symbol.for('quote'), [Symbol.for('foo')]]]);
  });
  return xit('(parse "foo;" :as \'javascript)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('parse'), 'foo;', Symbol.for(':as'), [Symbol.for('quote'), Symbol.for('javascript')]], [Symbol.for('js/obj'), 'type', 'Program', 'body', [Symbol.for('list'), [Symbol.for('js/obj'), 'type', 'ExpressionStatement', 'expression', [Symbol.for('js/obj'), 'type', 'Identifier', 'name', 'foo']]]]]);
  });
});

describe('interpret', function (): any {
  return xit('(interpret \'(length \'(1 . ())) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('interpret'), [Symbol.for('quote'), [Symbol.for('length'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]]], Symbol.for(':fdottedlists'), true], 1]);
  });
});

describe('gensym', function (): any {
  return xit('(compile `(module m scheme (define ,(gensym "length") length)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quasiquote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('unquote'), [Symbol.for('gensym'), 'length']], Symbol.for('length')]]]], 'import {\n' +
      '  length\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'let length1 = length;']);
  });
});

describe('define-syntax', function (): any {
  return xit('(compile \'(module m scheme (define x 1) (define-syntax (foo x) (syntax (begin (define x 2) x))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define-syntax'), [Symbol.for('foo'), Symbol.for('x')], [Symbol.for('syntax'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 2], Symbol.for('x')]]], [Symbol.for('foo')]]]], 'import {\n' +
      '  datumToSyntax\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'let x = 1;\n' +
      '\n' +
      'function foo(x) {\n' +
      '  return datumToSyntax(false, [Symbol.for(\'begin\'), [Symbol.for(\'define\'), Symbol.for(\'x\'), 2], Symbol.for(\'x\')]);\n' +
      '}\n' +
      '\n' +
      'foo.ftype = [Symbol.for(\'macro->\'), Symbol.for(\'Syntax\'), Symbol.for(\'Syntax\')];\n' +
      '\n' +
      'let x1 = 2;\n' +
      '\n' +
      'x1;']);
  });
});

describe('Dotted lists', function (): any {
  xit('\'(1 . 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('quote'), [1, Symbol.for('.'), 2]], [Symbol.for('quote'), [1, Symbol.for('.'), 2]]]);
  });
  xit('\'(1 . ())', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('quote'), [1, Symbol.for('.'), []]], [Symbol.for('quote'), [1]]]);
  });
  xit('\'(1 . (2 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]], [Symbol.for('quote'), [1, 2]]]);
  });
  xit('\'(1 . (2 . 3))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), 3]]], [Symbol.for('quote'), [1, 2, Symbol.for('.'), 3]]]);
  });
  xit('(dotted-list? \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], false]);
  });
  xit('(dotted-list? \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('dotted-list?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], false]);
  });
  xit('(dotted-pair? \'(1 . ()))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]], false]);
  });
  xit('(dotted-pair? \'(1 . (2 . ())))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('dotted-pair?'), [Symbol.for('quote'), [1, Symbol.for('.'), [2, Symbol.for('.'), []]]]], false]);
  });
  xit('(compile \'(module m scheme \'(1 . ())) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('quote'), [1, Symbol.for('.'), []]]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  normalizeList\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'normalizeList([1, Symbol.for(\'.\'), []);']);
  });
  return xit('(compile \'(module m scheme (define (normalize-list x) \'(1 . x))) :fdottedlists #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('normalize-list'), Symbol.for('x')], [Symbol.for('quote'), [1, Symbol.for('.'), Symbol.for('x')]]]]], Symbol.for(':fdottedlists'), true], 'import {\n' +
      '  normalizeList1\n' +
      '} from \'roselisp\';\n' +
      '\n' +
      'function normalizeList(x) {\n' +
      '  normalizeList1([1, Symbol.for(\'.\'), x);\n' +
      '}']);
  });
});