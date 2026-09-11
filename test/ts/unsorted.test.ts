/**
 * # Unsorted tests
 *
 * This file functions as an "inbox" for incoming tests, as well as
 * an "outbox" for legacy tests that can be deleted.
 */

import {
  assertEqual,
  testRepl,
  testMacro
} from './test-util';

import {
  tcall,
  trampoline
} from '../../src/ts/unsorted';

testMacro.ftype = 'macro';

describe('To do', function (): any {
});

describe('Avoiding IIFEs', function (): any {
  return xit('(compile \'(define x (begin y z)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), [Symbol.for('begin'), Symbol.for('y'), Symbol.for('z')]]]], 'let y;\n' +
      '\n' +
      'let x = z;']);
  });
});

describe('declare', function (): any {
  return xit('(compile \'(begin (define (my-plus x y) (+ x y 0)) (declare my-plus (compiler-macro (macro (x y) `(+ ,x ,y)))) (define x (my-plus 1 2))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define'), [Symbol.for('my-plus'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y'), 0]], [Symbol.for('declare'), Symbol.for('my-plus'), [Symbol.for('compiler-macro'), [Symbol.for('macro'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('quasiquote'), [Symbol.for('+'), [Symbol.for('unquote'), Symbol.for('x')], [Symbol.for('unquote'), Symbol.for('y')]]]]]], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('my-plus'), 1, 2]]]]], 'function myPlus(x, y) {\n' +
      '  return x + y + 0;\n' +
      '}\n' +
      '\n' +
      'myPlus.compilerMacro = (() => {\n' +
      '  let f = function (exp, env) {\n' +
      '    let [x, y] = exp.slice(1);\n' +
      '    return [Symbol.for(\'+\'), x, y];\n' +
      '  };\n' +
      '  f.ftype = \'macro\';\n' +
      '  return f;\n' +
      '})();\n' +
      '\n' +
      'let x = 1 + 2;']);
  });
});

describe('define-subst', function (): any {
  return xit('(compile \'(begin (define-subst (my-plus x y) (+ x y)) (define x (my-plus 1 2))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('xit>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('begin'), [Symbol.for('define-subst'), [Symbol.for('my-plus'), Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('define'), Symbol.for('x'), [Symbol.for('my-plus'), 1, 2]]]]], 'function myPlus(x, y) {\n' +
      '  return x + y;\n' +
      '}\n' +
      '\n' +
      'myPlus.compilerMacro = (() => {\n' +
      '  let f = function (exp, env) {\n' +
      '    let [x, y] = exp.slice(1);\n' +
      '    return [Symbol.for(\'+\'), x, y];\n' +
      '  };\n' +
      '  f.ftype = \'macro\';\n' +
      '  return f;\n' +
      '})();\n' +
      '\n' +
      'let x = 1 + 2;']);
  });
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

describe('trampoline', function (): any {
  it('(trampoline (fn (x) x) 1)', function (): any {
    return assertEqual(trampoline(function (x: any): any {
      return x;
    }, 1), 1);
  });
  function add(x: any, y: any): any {
    return x + y;
  }
  function fibonacci(n: any): any {
    if (n < 2) {
      return n;
    } else {
      return tcall(add, tcall(fibonacci, n - 1), tcall(fibonacci, n - 2));
    }
  }
  it('(trampoline fibonacci 0)', function (): any {
    return assertEqual(trampoline(fibonacci, 0), 0);
  });
  it('(trampoline fibonacci 1)', function (): any {
    return assertEqual(trampoline(fibonacci, 1), 1);
  });
  it('(trampoline fibonacci 2)', function (): any {
    return assertEqual(trampoline(fibonacci, 2), 1);
  });
  it('(trampoline fibonacci 3)', function (): any {
    return assertEqual(trampoline(fibonacci, 3), 2);
  });
  it('(trampoline fibonacci 4)', function (): any {
    return assertEqual(trampoline(fibonacci, 4), 3);
  });
  it('(trampoline fibonacci 5)', function (): any {
    return assertEqual(trampoline(fibonacci, 5), 5);
  });
  it('(trampoline fibonacci 6)', function (): any {
    return assertEqual(trampoline(fibonacci, 6), 8);
  });
  it('(trampoline fibonacci 7)', function (): any {
    return assertEqual(trampoline(fibonacci, 7), 13);
  });
  it('(trampoline fibonacci 8)', function (): any {
    return assertEqual(trampoline(fibonacci, 8), 21);
  });
  it('(trampoline fibonacci 9)', function (): any {
    return assertEqual(trampoline(fibonacci, 9), 34);
  });
  it('(trampoline fibonacci 10)', function (): any {
    return assertEqual(trampoline(fibonacci, 10), 55);
  });
  function sub(x: any, y: any): any {
    return x - y;
  }
  function sequence(n: any): any {
    if (n < 2) {
      return n;
    } else {
      return tcall(sub, tcall(sequence, n - 1), tcall(sequence, n - 2));
    }
  }
  it('(trampoline sequence 0)', function (): any {
    return assertEqual(trampoline(sequence, 0), 0);
  });
  it('(trampoline sequence 1)', function (): any {
    return assertEqual(trampoline(sequence, 1), 1);
  });
  it('(trampoline sequence 2)', function (): any {
    return assertEqual(trampoline(sequence, 2), 1);
  });
  it('(trampoline sequence 3)', function (): any {
    return assertEqual(trampoline(sequence, 3), 0);
  });
  it('(trampoline sequence 4)', function (): any {
    return assertEqual(trampoline(sequence, 4), -1);
  });
  it('(trampoline sequence 5)', function (): any {
    return assertEqual(trampoline(sequence, 5), -1);
  });
  it('(trampoline sequence 6)', function (): any {
    return assertEqual(trampoline(sequence, 6), 0);
  });
  it('(trampoline sequence 7)', function (): any {
    return assertEqual(trampoline(sequence, 7), 1);
  });
  it('(trampoline sequence 8)', function (): any {
    return assertEqual(trampoline(sequence, 8), 1);
  });
  it('(trampoline sequence 9)', function (): any {
    return assertEqual(trampoline(sequence, 9), 0);
  });
  return it('(trampoline sequence 10)', function (): any {
    return assertEqual(trampoline(sequence, 10), -1);
  });
});