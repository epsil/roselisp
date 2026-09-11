/**
 * # JavaScript
 *
 * Tests of JavaScript constructs.
 */

import {
  testRepl,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('js/is-NaN', function (): any {
  it('(js/is-NaN NaN)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/is-NaN'), Symbol.for('NaN')], true]);
  });
  it('(js/is-NaN 0)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/is-NaN'), 0], false]);
  });
  return it('(compile \'(js/is-NaN x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/is-NaN'), Symbol.for('x')]]], 'isNaN(x);']);
  });
});

describe('js/[]', function (): any {
  return it('(compile \'(js/[] x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/[]'), Symbol.for('x'), Symbol.for('y')]]], 'x[y];']);
  });
});

describe('js/var', function (): any {
  it('(compile \'(js/var x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/var'), Symbol.for('x')]]], 'var x;']);
  });
  it('(compile \'(js/var x 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/var'), Symbol.for('x'), 0]]], 'var x = 0;']);
  });
  return it('(compile \'(js/var x 0 y 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/var'), Symbol.for('x'), 0, Symbol.for('y'), 0]]], 'var x = 0, y = 0;']);
  });
});

describe('js/let', function (): any {
  it('(compile \'(js/let x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/let'), Symbol.for('x')]]], 'let x;']);
  });
  it('(compile \'(js/let x 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/let'), Symbol.for('x'), 0]]], 'let x = 0;']);
  });
  return it('(compile \'(js/let x 0 y 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/let'), Symbol.for('x'), 0, Symbol.for('y'), 0]]], 'let x = 0, y = 0;']);
  });
});

describe('js/const', function (): any {
  it('(compile \'(js/const x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/const'), Symbol.for('x')]]], 'const x;']);
  });
  it('(compile \'(js/const x 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/const'), Symbol.for('x'), 0]]], 'const x = 0;']);
  });
  return it('(compile \'(js/const x 0 y 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/const'), Symbol.for('x'), 0, Symbol.for('y'), 0]]], 'const x = 0, y = 0;']);
  });
});

describe('js/function', function (): any {
  it('(compile \'(js/function () 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [], 0]]], 'function () {\n' +
      '  return 0;\n' +
      '};']);
  });
  it('(compile \'(js/function () : Number 0) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [], Symbol.for(':'), Symbol.for('Number'), 0]], Symbol.for(':to'), 'typescript'], 'function (): number {\n' +
      '  return 0;\n' +
      '};']);
  });
  it('(compile \'(js/function () :name foo 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [], Symbol.for(':name'), Symbol.for('foo'), 0]]], 'function foo() {\n' +
      '  return 0;\n' +
      '}']);
  });
  return it('(compile \'(js/function () : Number :name foo 0) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/function'), [], Symbol.for(':'), Symbol.for('Number'), Symbol.for(':name'), Symbol.for('foo'), 0]], Symbol.for(':to'), 'typescript'], 'function foo(): number {\n' +
      '  return 0;\n' +
      '}']);
  });
});

describe('js/arrow', function (): any {
  it('(compile \'(js/arrow () 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [], 0]]], '() => {\n' +
      '  return 0;\n' +
      '};']);
  });
  it('(compile \'(js/arrow () : Number 0) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [], Symbol.for(':'), Symbol.for('Number'), 0]], Symbol.for(':to'), 'typescript'], '(): number => {\n' +
      '  return 0;\n' +
      '};']);
  });
  it('(compile \'(js/arrow () :name foo 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [], Symbol.for(':name'), Symbol.for('foo'), 0]]], 'let foo = () => {\n' +
      '  return 0;\n' +
      '};']);
  });
  return it('(compile \'(js/arrow () : Number :name foo 0) :to "typescript")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/arrow'), [], Symbol.for(':'), Symbol.for('Number'), Symbol.for(':name'), Symbol.for('foo'), 0]], Symbol.for(':to'), 'typescript'], 'let foo: any = (): number => {\n' +
      '  return 0;\n' +
      '};']);
  });
});

describe('js/arrow?', function (): any {
  it('(js/arrow? (js/arrow (x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/arrow?'), [Symbol.for('js/arrow'), [Symbol.for('x')], Symbol.for('x')]], true]);
  });
  return it('(js/arrow? (lambda (x) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/arrow?'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]], false]);
  });
});

describe('js/=>', function (): any {
  return it('(compile \'(js/=> () 0))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/=>'), [], 0]]], '() => {\n' +
      '  return 0;\n' +
      '};']);
  });
});

describe('js/iife', function (): any {
  it('(compile \'(js/iife (js/arrow (x y) (+ x y)) (list 1 2)) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('list'), 1, 2]]], Symbol.for(':as'), 'expression'], '((x, y) => {\n' +
      '  return x + y;\n' +
      '})(1, 2)']);
  });
  it('(compile \'(js/iife (js/arrow (x . y) (+ x (first y))) (list* a b)) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), [Symbol.for('first'), Symbol.for('y')]]], [Symbol.for('list*'), Symbol.for('a'), Symbol.for('b')]]], Symbol.for(':as'), 'expression'], '((x, ...y) => {\n' +
      '  return x + y[0];\n' +
      '})(a, ...b)']);
  });
  it('(compile \'(js/iife (js/arrow (x y) (+ x y)) (list 1 2)) :as "statement")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('list'), 1, 2]]], Symbol.for(':as'), 'statement'], 'let x = 1;\n' +
      '\n' +
      'let y = 2;\n' +
      '\n' +
      'x + y;']);
  });
  it('(compile \'(js/iife (js/arrow (x . y) (+ x (first y))) (list a b c)) :as "statement")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), [Symbol.for('first'), Symbol.for('y')]]], [Symbol.for('list'), Symbol.for('a'), Symbol.for('b'), Symbol.for('c')]]], Symbol.for(':as'), 'statement'], 'let y = [b, c];\n' +
      '\n' +
      'a + y[0];']);
  });
  return it('(compile \'(js/iife (js/arrow (x y) (+ x y)) (list 1 2)) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/iife'), [Symbol.for('js/arrow'), [Symbol.for('x'), Symbol.for('y')], [Symbol.for('+'), Symbol.for('x'), Symbol.for('y')]], [Symbol.for('list'), 1, 2]]], Symbol.for(':as'), 'return'], 'let x = 1;\n' +
      '\n' +
      'let y = 2;\n' +
      '\n' +
      'return x + y;']);
  });
});

describe('js/()', function (): any {
  it('(compile \'(js/() x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/()'), Symbol.for('x')]]], 'x();']);
  });
  it('(compile \'(js/() x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/()'), Symbol.for('x'), Symbol.for('y')]]], 'x(y);']);
  });
  return it('(compile \'(js/() x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/()'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x(y, z);']);
  });
});

describe('js/=', function (): any {
  it('(compile \'(js/= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), Symbol.for('x'), Symbol.for('y')]]], 'x = y;']);
  });
  it('(compile \'(js/= (aget x i) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('aget'), Symbol.for('x'), Symbol.for('i')], Symbol.for('y')]]], 'x[i] = y;']);
  });
  it('(compile \'(js/= (list-ref x i) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list-ref'), Symbol.for('x'), Symbol.for('i')], Symbol.for('y')]]], 'x[i] = y;']);
  });
  it('(compile \'(js/= \'(x y) z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('quote'), [Symbol.for('x'), Symbol.for('y')]], Symbol.for('z')]]], '[x, y] = z;']);
  });
  it('(compile \'(js/= \'((x) y) z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('quote'), [[Symbol.for('x')], Symbol.for('y')]], Symbol.for('z')]]], '[[x], y] = z;']);
  });
  it('(compile \'(js/= \'(x . y) z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('quote'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')]], Symbol.for('z')]]], '[x, ...y] = z;']);
  });
  it('(compile \'(module m scheme (js/= \'(length) x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('js/='), [Symbol.for('quote'), [Symbol.for('length')]], Symbol.for('x')]]]], '[length] = x;']);
  });
  it('(compile \'(js/= (list x y) z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z')]]], '[x, y] = z;']);
  });
  it('(compile \'(js/= (list #f y) z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list'), false, Symbol.for('y')], Symbol.for('z')]]], '[, y] = z;']);
  });
  it('(compile \'(js/= (list* x) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list*'), Symbol.for('x')], Symbol.for('y')]]], 'x = y;']);
  });
  it('(compile \'(js/= (list* x y) z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list*'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z')]]], '[x, ...y] = z;']);
  });
  it('(compile \'(js/= (list* #f x y) z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list*'), false, Symbol.for('x'), Symbol.for('y')], Symbol.for('z')]]], '[, x, ...y] = z;']);
  });
  it('(compile \'(js/= (values x y) z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('values'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z')]]], '[x, y] = z;']);
  });
  it('(compile \'(js/= (js/obj x x) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('js/obj'), Symbol.for('x'), Symbol.for('x')], Symbol.for('y')]]], '({x} = y);']);
  });
  it('(compile \'(js/= (js/obj x y) z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('js/obj'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z')]]], '({x: y} = z);']);
  });
  it('(compile \'(js/= (set! x) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('set!'), Symbol.for('x')], Symbol.for('y')]]], 'x = y;']);
  });
  it('(compile \'(js/= (aset! x i) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('aset!'), Symbol.for('x'), Symbol.for('i')], Symbol.for('y')]]], 'x[i] = y;']);
  });
  it('(compile \'(js/= (list-set! x i) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('list-set!'), Symbol.for('x'), Symbol.for('i')], Symbol.for('y')]]], 'x[i] = y;']);
  });
  it('(compile \'(js/= (oset! x "y") z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('oset!'), Symbol.for('x'), 'y'], Symbol.for('z')]]], 'x[\'y\'] = z;']);
  });
  it('(compile \'(js/= (set!-values (x)) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('set!-values'), [Symbol.for('x')]], Symbol.for('y')]]], '[x] = y;']);
  });
  it('(compile \'(js/= (set!-fields (x)) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('set!-fields'), [Symbol.for('x')]], Symbol.for('y')]]], '({x} = y);']);
  });
  it('(compile \'(js/= (define x) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('define'), Symbol.for('x')], Symbol.for('y')]]], 'let x = y;']);
  });
  it('(compile \'(js/= (define-values (x)) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('define-values'), [Symbol.for('x')]], Symbol.for('y')]]], 'let [x] = y;']);
  });
  it('(compile \'(js/= (define-values (_ x)) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('x')]], Symbol.for('y')]]], 'let [, x] = y;']);
  });
  it('(compile \'(js/= (define-values (_ __ x) :hole-marker __) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('__'), Symbol.for('x')], Symbol.for(':hole-marker'), Symbol.for('__')], Symbol.for('y')]]], 'let [_, , x] = y;']);
  });
  return it('(compile \'(js/= (define-fields (x)) y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/='), [Symbol.for('define-fields'), [Symbol.for('x')]], Symbol.for('y')]]], 'let {x} = y;']);
  });
});

describe('js/,', function (): any {
  it('(compile \'(js/, x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/,'), Symbol.for('x')]]], 'x;']);
  });
  it('(compile \'(js/, x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/,'), Symbol.for('x'), Symbol.for('y')]]], 'x, y;']);
  });
  return it('(compile \'(js/, x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/,'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x, y, z;']);
  });
});

describe('js/;', function (): any {
  it('(compile \'(js/; x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/;'), Symbol.for('x')]]], 'x;']);
  });
  it('(compile \'(js/; x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/;'), Symbol.for('x'), Symbol.for('y')]]], 'x;\n' +
      '\n' +
      'y;']);
  });
  return it('(compile \'(js/; x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/;'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x;\n' +
      '\n' +
      'y;\n' +
      '\n' +
      'z;']);
  });
});

describe('js/block', function (): any {
  it('(compile \'(js/block x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/block'), Symbol.for('x')]]], '{\n' +
      '  x;\n' +
      '}']);
  });
  it('(compile \'(js/block x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/block'), Symbol.for('x'), Symbol.for('y')]]], '{\n' +
      '  x;\n' +
      '  y;\n' +
      '}']);
  });
  return it('(compile \'(js/block x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/block'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '{\n' +
      '  x;\n' +
      '  y;\n' +
      '  z;\n' +
      '}']);
  });
});

describe('js/{}', function (): any {
  it('(compile \'(js/{} x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/{}'), Symbol.for('x')]]], '{\n' +
      '  x;\n' +
      '}']);
  });
  it('(compile \'(js/{} x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/{}'), Symbol.for('x'), Symbol.for('y')]]], '{\n' +
      '  x;\n' +
      '  y;\n' +
      '}']);
  });
  return it('(compile \'(js/{} x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/{}'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '{\n' +
      '  x;\n' +
      '  y;\n' +
      '  z;\n' +
      '}']);
  });
});

describe('js/?', function (): any {
  it('(compile \'(js/? x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y')]]], 'x ? y : undefined;']);
  });
  it('(compile \'(js/? x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x ? y : z;']);
  });
  it('(compile \'(js/? x y (js/? z w)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y'), [Symbol.for('js/?'), Symbol.for('z'), Symbol.for('w')]]]], 'x ? y : (z ? w : undefined);']);
  });
  it('(compile \'(js/? x y z) :as "statement")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'statement'], 'x ? y : z;']);
  });
  it('(compile \'(js/? x y z) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'return'], 'return x ? y : z;']);
  });
  return it('(compile \'(js/? x y z) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'expression'], 'x ? y : z']);
  });
});

describe('js/if', function (): any {
  it('(compile \'(js/if x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y')]]], 'if (x) {\n' +
      '  y;\n' +
      '}']);
  });
  it('(compile \'(js/if x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'if (x) {\n' +
      '  y;\n' +
      '} else {\n' +
      '  z;\n' +
      '}']);
  });
  it('(compile \'(js/if x y (js/if z w)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y'), [Symbol.for('js/if'), Symbol.for('z'), Symbol.for('w')]]]], 'if (x) {\n' +
      '  y;\n' +
      '} else if (z) {\n' +
      '  w;\n' +
      '}']);
  });
  it('(compile \'(js/if x y z) :as "statement")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'statement'], 'if (x) {\n' +
      '  y;\n' +
      '} else {\n' +
      '  z;\n' +
      '}']);
  });
  it('(compile \'(js/if x y z) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'return'], 'if (x) {\n' +
      '  return y;\n' +
      '} else {\n' +
      '  return z;\n' +
      '}']);
  });
  return it('(compile \'(js/if x y z) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('it>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/if'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]], Symbol.for(':as'), 'expression'], '(() => {\n' +
      '  if (x) {\n' +
      '    return y;\n' +
      '  } else {\n' +
      '    return z;\n' +
      '  }\n' +
      '})()']);
  });
});

describe('js/switch', function (): any {
  it('(let* ((x "foo") (y "bar")) (js/switch x (case "foo" (set! y "baz") (break)) (default (set! y "quux"))) y)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let*'), [[Symbol.for('x'), 'foo'], [Symbol.for('y'), 'bar']], [Symbol.for('js/switch'), Symbol.for('x'), [Symbol.for('case'), 'foo', [Symbol.for('set!'), Symbol.for('y'), 'baz'], [Symbol.for('break')]], [Symbol.for('default'), [Symbol.for('set!'), Symbol.for('y'), 'quux']]], Symbol.for('y')], 'baz']);
  });
  it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/switch'), Symbol.for('x'), [Symbol.for('case'), 'foo', [Symbol.for('display'), 'foo'], [Symbol.for('break')]], [Symbol.for('default'), [Symbol.for('display'), 'bar']]]]], 'switch (x) {\n' +
      '  case \'foo\': {\n' +
      '    console.log(\'foo\');\n' +
      '    break;\n' +
      '  }\n' +
      '  default: {\n' +
      '    console.log(\'bar\');\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/switch'), Symbol.for('x'), [Symbol.for('case'), 'foo', [Symbol.for('display'), 'foo'], [Symbol.for('break')]], [Symbol.for('default'), [Symbol.for('display'), 'bar']]]], Symbol.for(':as'), 'return'], 'switch (x) {\n' +
      '  case \'foo\': {\n' +
      '    return console.log(\'foo\');\n' +
      '    break;\n' +
      '  }\n' +
      '  default: {\n' +
      '    return console.log(\'bar\');\n' +
      '  }\n' +
      '}']);
  });
  it('(compile \'(js/switch x (case "foo" (display "foo")) (default (display "bar"))) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/switch'), Symbol.for('x'), [Symbol.for('case'), 'foo', [Symbol.for('display'), 'foo']], [Symbol.for('default'), [Symbol.for('display'), 'bar']]]], Symbol.for(':as'), 'return'], 'switch (x) {\n' +
      '  case \'foo\': {\n' +
      '    console.log(\'foo\');\n' +
      '  }\n' +
      '  default: {\n' +
      '    return console.log(\'bar\');\n' +
      '  }\n' +
      '}']);
  });
  return it('(compile \'(js/switch x (case "foo" (display "foo") (break)) (default (display "bar"))) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/switch'), Symbol.for('x'), [Symbol.for('case'), 'foo', [Symbol.for('display'), 'foo'], [Symbol.for('break')]], [Symbol.for('default'), [Symbol.for('display'), 'bar']]]], Symbol.for(':as'), 'expression'], '(() => {\n' +
      '  switch (x) {\n' +
      '    case \'foo\': {\n' +
      '      return console.log(\'foo\');\n' +
      '      break;\n' +
      '    }\n' +
      '    default: {\n' +
      '      return console.log(\'bar\');\n' +
      '    }\n' +
      '  }\n' +
      '})()']);
  });
});

describe('js/!', function (): any {
  it('(js/! #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/!'), false], true]);
  });
  it('(js/! #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/!'), true], false]);
  });
  return it('(compile \'(js/! x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/!'), Symbol.for('x')]]], '!x;']);
  });
});

describe('js/&&', function (): any {
  it('(js/&&)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/&&')], true]);
  });
  it('(js/&& #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/&&'), true], true]);
  });
  it('(js/&& #t #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/&&'), true, true], true]);
  });
  it('(js/&& #t #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/&&'), true, false], false]);
  });
  it('(funcall js/&&)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/&&')], true]);
  });
  it('(funcall js/&& #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/&&'), true], true]);
  });
  it('(funcall js/&& #t #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/&&'), true, true], true]);
  });
  it('(funcall js/&& #t #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/&&'), true, false], false]);
  });
  it('(compile \'(js/&& x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/&&'), Symbol.for('x'), Symbol.for('y')]]], 'x && y;']);
  });
  return it('(compile \'(js/&& x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/&&'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x && y && z;']);
  });
});

describe('js/||', function (): any {
  it('(js/||)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/||')], false]);
  });
  it('(js/|| #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/||'), true], true]);
  });
  it('(js/|| #t #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/||'), true, true], true]);
  });
  it('(js/|| #t #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/||'), true, false], true]);
  });
  it('(funcall js/||)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/||')], false]);
  });
  it('(funcall js/|| #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/||'), true], true]);
  });
  it('(funcall js/|| #t #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/||'), true, true], true]);
  });
  it('(funcall js/|| #t #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/||'), true, false], true]);
  });
  it('(compile \'(js/|| x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/||'), Symbol.for('x'), Symbol.for('y')]]], 'x || y;']);
  });
  return it('(compile \'(js/|| x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/||'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], 'x || y || z;']);
  });
});

describe('js/op', function (): any {
  it('(js/op ! #t)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/op'), Symbol.for('!'), true], false]);
  });
  it('(js/op && #t #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/op'), Symbol.for('&&'), true, false], false]);
  });
  it('(js/op || #t #f)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/op'), Symbol.for('||'), true, false], true]);
  });
  it('(compile \'(js/op ! x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/op'), Symbol.for('!'), Symbol.for('x')]]], '!x;']);
  });
  it('(compile \'(js/op ~ x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/op'), Symbol.for('~'), Symbol.for('x')]]], '~x;']);
  });
  it('(compile \'(js/op & x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/op'), Symbol.for('&'), Symbol.for('x'), Symbol.for('y')]]], 'x & y;']);
  });
  it('(compile \'(js/op && x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/op'), Symbol.for('&&'), Symbol.for('x'), Symbol.for('y')]]], 'x && y;']);
  });
  return it('(compile \'(js/op || x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/op'), Symbol.for('||'), Symbol.for('x'), Symbol.for('y')]]], 'x || y;']);
  });
});

describe('js/while', function (): any {
  it('(compile \'(js/while (< (length result) 3) (display result)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/while'), [Symbol.for('<'), [Symbol.for('length'), Symbol.for('result')], 3], [Symbol.for('display'), Symbol.for('result')]]]], 'while (result.length < 3) {\n' +
      '  console.log(result);\n' +
      '}']);
  });
  return it('(compile \'(js/while (begin (set! x (- x 1)) (> x 0)) (display x)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/while'), [Symbol.for('begin'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('-'), Symbol.for('x'), 1]], [Symbol.for('>'), Symbol.for('x'), 0]], [Symbol.for('display'), Symbol.for('x')]]]], 'while (x--, x > 0) {\n' +
      '  console.log(x);\n' +
      '}']);
  });
});

describe('js/do-while', function (): any {
  it('(compile \'(js/do-while ((display result)) (< (length result) 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/do-while'), [[Symbol.for('display'), Symbol.for('result')]], [Symbol.for('<'), [Symbol.for('length'), Symbol.for('result')], 3]]]], 'do {\n' +
      '  console.log(result);\n' +
      '} while (result.length < 3);']);
  });
  return it('(compile \'(js/do-while ((foo) (display result)) (< (length result) 3)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/do-while'), [[Symbol.for('foo')], [Symbol.for('display'), Symbol.for('result')]], [Symbol.for('<'), [Symbol.for('length'), Symbol.for('result')], 3]]]], 'do {\n' +
      '  foo();\n' +
      '  console.log(result);\n' +
      '} while (result.length < 3);']);
  });
});

describe('js/for', function (): any {
  it('(let ((result 0)) (js/for ((i 0) (< i 10) (+ i 1)) (set! result (+ result 2))) result)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('result'), 0]], [Symbol.for('js/for'), [[Symbol.for('i'), 0], [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('+'), Symbol.for('result'), 2]]], Symbol.for('result')], 20]);
  });
  it('(compile \'(js/for ((i 0) (< i 10) (+ i 1)) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[Symbol.for('i'), 0], [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('foo')]]]], 'for (let i = 0; i < 10; i++) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(compile \'(js/for ((set! i 0) (< i 10) (+ i 1)) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[Symbol.for('set!'), Symbol.for('i'), 0], [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('foo')]]]], 'for (i = 0; i < 10; i++) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(compile \'(js/for ((define i 0) (< i 10) (+ i 1)) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[Symbol.for('define'), Symbol.for('i'), 0], [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('foo')]]]], 'for (let i = 0; i < 10; i++) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(compile \'(js/for ((begin (set! i 0) (set! j 0)) (and (< i 10) (< j 10)) (begin (set! i (+ i 1)) (set! j (+ j 1)))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[Symbol.for('begin'), [Symbol.for('set!'), Symbol.for('i'), 0], [Symbol.for('set!'), Symbol.for('j'), 0]], [Symbol.for('and'), [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('<'), Symbol.for('j'), 10]], [Symbol.for('begin'), [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('set!'), Symbol.for('j'), [Symbol.for('+'), Symbol.for('j'), 1]]]], [Symbol.for('foo')]]]], 'for (i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(compile \'(js/for ((js/define i 0 j 0) (and (< i 10) (< j 10)) (begin (set! i (+ i 1)) (set! j (+ j 1)))) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[Symbol.for('js/define'), Symbol.for('i'), 0, Symbol.for('j'), 0], [Symbol.for('and'), [Symbol.for('<'), Symbol.for('i'), 10], [Symbol.for('<'), Symbol.for('j'), 10]], [Symbol.for('begin'), [Symbol.for('set!'), Symbol.for('i'), [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('set!'), Symbol.for('j'), [Symbol.for('+'), Symbol.for('j'), 1]]]], [Symbol.for('foo')]]]], 'for (let i = 0, j = 0; (i < 10) && (j < 10); i++, j++) {\n' +
      '  foo();\n' +
      '}']);
  });
  it('(let (result) (js/for (() () ()) (set! result 1) (break)) result)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('result')], [Symbol.for('js/for'), [[], [], []], [Symbol.for('set!'), Symbol.for('result'), 1], [Symbol.for('break')]], Symbol.for('result')], 1]);
  });
  it('(let (result) (js/for (#u #u #u) (set! result 1) (break)) result)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [Symbol.for('result')], [Symbol.for('js/for'), [undefined, undefined, undefined], [Symbol.for('set!'), Symbol.for('result'), 1], [Symbol.for('break')]], Symbol.for('result')], 1]);
  });
  it('(compile \'(js/for (() () ()) (break)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [[], [], []], [Symbol.for('break')]]]], 'for (;;) {\n' +
      '  break;\n' +
      '}']);
  });
  it('(compile \'(js/for (#f #f #f) (break)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [false, false, false], [Symbol.for('break')]]]], 'for (;;) {\n' +
      '  break;\n' +
      '}']);
  });
  return it('(compile \'(js/for (#u #u #u) (break)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for'), [undefined, undefined, undefined], [Symbol.for('break')]]]], 'for (;;) {\n' +
      '  break;\n' +
      '}']);
  });
});

describe('js/for-in', function (): any {
  return it('(compile \'(js/for-in ((i obj)) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for-in'), [[Symbol.for('i'), Symbol.for('obj')]], [Symbol.for('foo')]]]], 'for (let i in obj) {\n' +
      '  foo();\n' +
      '}']);
  });
});

describe('js/for-of', function (): any {
  return it('(compile \'(js/for-of ((i lst)) (foo)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/for-of'), [[Symbol.for('i'), Symbol.for('lst')]], [Symbol.for('foo')]]]], 'for (let i of lst) {\n' +
      '  foo();\n' +
      '}']);
  });
});

describe('js/.', function (): any {
  it('(let ((obj (js/obj "foo" "bar"))) (js/. obj foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('foo')]], 'bar']);
  });
  it('(let ((obj (js/obj "foo" "bar"))) (js/. obj "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/.'), Symbol.for('obj'), 'foo']], 'bar']);
  });
  it('(let ((obj (js/obj "foo" (js/obj "bar" "baz")))) (js/. obj foo bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj'), 'bar', 'baz']]]], [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('foo'), Symbol.for('bar')]], 'baz']);
  });
  it('(let ((obj (js/obj "foo" (js/obj "bar" "baz")))) (js/. (js/. obj foo) bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj'), 'bar', 'baz']]]], [Symbol.for('js/.'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('foo')], Symbol.for('bar')]], 'baz']);
  });
  it('(compile \'(js/. obj prop))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('prop')]]], 'obj.prop;']);
  });
  it('(compile \'(js/. obj "prop"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), 'prop']]], 'obj[\'prop\'];']);
  });
  it('(compile \'(js/. obj :foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for(':foo')]]], 'obj.foo;']);
  });
  it('(compile \'(js/. obj :foo-bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for(':foo-bar')]]], 'obj.fooBar;']);
  });
  it('(compile \'(js/. obj \'foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo')]]]], 'obj.foo;']);
  });
  it('(compile \'(js/. obj \'foo-bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), [Symbol.for('quote'), Symbol.for('foo-bar')]]]], 'obj.fooBar;']);
  });
  it('(compile \'(js/. obj prop1 prop2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('prop1'), Symbol.for('prop2')]]], 'obj.prop1.prop2;']);
  });
  return it('(compile \'(js/. (js/. obj prop1) prop2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/.'), [Symbol.for('js/.'), Symbol.for('obj'), Symbol.for('prop1')], Symbol.for('prop2')]]], 'obj.prop1.prop2;']);
  });
});

describe('js/?.', function (): any {
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('foo')]], 'bar']);
  });
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj "foo"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/?.'), Symbol.for('obj'), 'foo']], 'bar']);
  });
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj quux))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('quux')]], undefined]);
  });
  it('(let ((obj (js/obj "foo" "bar"))) ((js/?. obj quux)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [[Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('quux')]]], undefined]);
  });
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. obj quux wobble))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('quux'), Symbol.for('wobble')]], undefined]);
  });
  it('(let ((obj (js/obj "foo" "bar"))) (js/?. (js/?. obj quux) wobble))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/?.'), [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('quux')], Symbol.for('wobble')]], undefined]);
  });
  it('(compile \'(js/?. obj prop))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('prop')]]], 'obj?.prop;']);
  });
  it('(compile \'(js/?. obj prop1 prop2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('prop1'), Symbol.for('prop2')]]], 'obj?.prop1?.prop2;']);
  });
  it('(compile \'(js/?. (js/?. obj prop1) prop2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?.'), [Symbol.for('js/?.'), Symbol.for('obj'), Symbol.for('prop1')], Symbol.for('prop2')]]], 'obj?.prop1?.prop2;']);
  });
  it('(compile \'(js/?. (js/?. (js/?. obj) prop1) prop2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/?.'), [Symbol.for('js/?.'), [Symbol.for('js/?.'), Symbol.for('obj')], Symbol.for('prop1')], Symbol.for('prop2')]]], 'obj?.prop1?.prop2;']);
  });
  it('(compile \'(define x (js/?. foo bar)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), [Symbol.for('js/?.'), Symbol.for('foo'), Symbol.for('bar')]]]], 'let x = foo?.bar;']);
  });
  it('(compile \'(define x ((js/?. foo bar) baz)))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), [[Symbol.for('js/?.'), Symbol.for('foo'), Symbol.for('bar')], Symbol.for('baz')]]]], 'let x = foo?.bar(baz);']);
  });
  return it('(compile \'(define x (js/?. foo (bar))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('define'), Symbol.for('x'), [Symbol.for('js/?.'), Symbol.for('foo'), [Symbol.for('bar')]]]]], 'let x = foo?.(bar);']);
  });
});

describe('js/obj', function (): any {
  it('(js/obj)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/obj')], [Symbol.for('js/obj')]]);
  });
  it('(js/obj "foo" "bar")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/obj'), 'foo', 'bar'], [Symbol.for('js/obj'), 'foo', 'bar']]);
  });
  it('(js/obj "foo" 1 "bar" 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/obj'), 'foo', 1, 'bar', 2], [Symbol.for('js/obj'), 'foo', 1, 'bar', 2]]);
  });
  it('(compile \'(js/obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj')]]], '({});']);
  });
  it('(compile \'(js/obj "foo" foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', Symbol.for('foo')]]], '({\n' +
      '  foo\n' +
      '});']);
  });
  it('(compile \'(js/obj "foo" "bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 'bar']]], '({\n' +
      '  foo: \'bar\'\n' +
      '});']);
  });
  it('(compile \'(js/obj foo foo))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), Symbol.for('foo'), Symbol.for('foo')]]], '({\n' +
      '  [foo]: foo\n' +
      '});']);
  });
  it('(compile \'(js/obj foo "bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), Symbol.for('foo'), 'bar']]], '({\n' +
      '  [foo]: \'bar\'\n' +
      '});']);
  });
  it('(compile \'(js/obj \'foo "bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), [Symbol.for('quote'), Symbol.for('foo')], 'bar']]], '({\n' +
      '  foo: \'bar\'\n' +
      '});']);
  });
  it('(compile \'(js/obj :foo "bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), Symbol.for(':foo'), 'bar']]], '({\n' +
      '  foo: \'bar\'\n' +
      '});']);
  });
  it('(compile \'(js/obj "foo-bar" "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo-bar', 'baz']]], '({\n' +
      '  \'foo-bar\': \'baz\'\n' +
      '});']);
  });
  it('(compile \'(js/obj foo-bar "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), Symbol.for('foo-bar'), 'baz']]], '({\n' +
      '  [fooBar]: \'baz\'\n' +
      '});']);
  });
  it('(compile \'(js/obj \'foo-bar "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), [Symbol.for('quote'), Symbol.for('foo-bar')], 'baz']]], '({\n' +
      '  fooBar: \'baz\'\n' +
      '});']);
  });
  it('(compile \'(js/obj :foo-bar "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), Symbol.for(':foo-bar'), 'baz']]], '({\n' +
      '  fooBar: \'baz\'\n' +
      '});']);
  });
  it('(compile \'(js/obj "foo bar" "baz"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo bar', 'baz']]], '({\n' +
      '  \'foo bar\': \'baz\'\n' +
      '});']);
  });
  it('(compile \'(js/obj "foo bar" baz))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo bar', Symbol.for('baz')]]], '({\n' +
      '  \'foo bar\': baz\n' +
      '});']);
  });
  it('(compile \'(js/obj "foo bar" \'baz))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo bar', [Symbol.for('quote'), Symbol.for('baz')]]]], '({\n' +
      '  \'foo bar\': Symbol.for(\'baz\')\n' +
      '});']);
  });
  it('(compile \'(js/obj "foo bar" :baz))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo bar', Symbol.for(':baz')]]], '({\n' +
      '  \'foo bar\': Symbol.for(\':baz\')\n' +
      '});']);
  });
  it('(compile \'(js/obj "foo" 1 "bar" 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 1, 'bar', 2]]], '({\n' +
      '  foo: 1,\n' +
      '  bar: 2\n' +
      '});']);
  });
  it('(compile \'(js/obj "foo" foo "bar" bar))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', Symbol.for('foo'), 'bar', Symbol.for('bar')]]], '({\n' +
      '  foo,\n' +
      '  bar\n' +
      '});']);
  });
  it('(compile \'(js/obj "foo" (js/obj "bar" "baz")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj'), 'bar', 'baz']]]], '({\n' +
      '  foo: {\n' +
      '    bar: \'baz\'\n' +
      '  }\n' +
      '});']);
  });
  it('(compile \'(js/obj "foo" (js/obj "foo" "foo") "bar" (js/obj "bar" "bar")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj'), 'foo', 'foo'], 'bar', [Symbol.for('js/obj'), 'bar', 'bar']]]], '({\n' +
      '  foo: {\n' +
      '    foo: \'foo\'\n' +
      '  },\n' +
      '  bar: {\n' +
      '    bar: \'bar\'\n' +
      '  }\n' +
      '});']);
  });
  it('(compile \'(js/obj "foo" (js/obj) "bar" (js/obj "bar" "bar") "baz" (js/obj "baz" "baz")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', [Symbol.for('js/obj')], 'bar', [Symbol.for('js/obj'), 'bar', 'bar'], 'baz', [Symbol.for('js/obj'), 'baz', 'baz']]]], '({\n' +
      '  foo: {},\n' +
      '  bar: {\n' +
      '    bar: \'bar\'\n' +
      '  },\n' +
      '  baz: {\n' +
      '    baz: \'baz\'\n' +
      '  }\n' +
      '});']);
  });
  it('(compile \'(js/obj) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj')]], Symbol.for(':as'), 'expression'], '{}']);
  });
  it('(compile \'(js/obj "foo" "bar") :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 'bar']], Symbol.for(':as'), 'expression'], '{\n' +
      '  foo: \'bar\'\n' +
      '}']);
  });
  it('(compile \'(js/obj "foo" 1 "bar" 2) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 1, 'bar', 2]], Symbol.for(':as'), 'expression'], '{\n' +
      '  foo: 1,\n' +
      '  bar: 2\n' +
      '}']);
  });
  it('(compile \'(js/obj) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj')]], Symbol.for(':as'), 'return'], 'return {};']);
  });
  it('(compile \'(js/obj "foo" "bar") :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 'bar']], Symbol.for(':as'), 'return'], 'return {\n' +
      '  foo: \'bar\'\n' +
      '};']);
  });
  return it('(compile \'(js/obj "foo" 1 "bar" 2) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj'), 'foo', 1, 'bar', 2]], Symbol.for(':as'), 'return'], 'return {\n' +
      '  foo: 1,\n' +
      '  bar: 2\n' +
      '};']);
  });
});

describe('js/obj?', function (): any {
  return it('(compile \'(js/obj? x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj?'), Symbol.for('x')]]], '(x !== null) && (typeof x === \'object\');']);
  });
});

describe('js/obj-append', function (): any {
  return it('(compile \'(js/obj-append obj (js/obj "foo" "bar")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/obj-append'), Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]]], '({\n' +
      '  ...obj,\n' +
      '  foo: \'bar\'\n' +
      '});']);
  });
});

describe('js/keys', function (): any {
  it('(js/keys (js/obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/keys'), [Symbol.for('js/obj')]], [Symbol.for('quote'), []]]);
  });
  it('(js/keys (js/obj "foo" "bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/keys'), [Symbol.for('js/obj'), 'foo', 'bar']], [Symbol.for('quote'), ['foo']]]);
  });
  it('(js/keys (js/obj "foo" "bar" "baz" "quux"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/keys'), [Symbol.for('js/obj'), 'foo', 'bar', 'baz', 'quux']], [Symbol.for('quote'), ['foo', 'baz']]]);
  });
  return it('(compile \'(js/keys x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/keys'), Symbol.for('x')]]], 'Object.keys(x);']);
  });
});

describe('js/in', function (): any {
  it('(let ((obj (js/obj "foo" "bar"))) (js/in "foo" obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('obj'), [Symbol.for('js/obj'), 'foo', 'bar']]], [Symbol.for('js/in'), 'foo', Symbol.for('obj')]], true]);
  });
  return it('(compile \'(js/in "foo" obj))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/in'), 'foo', Symbol.for('obj')]]], '\'foo\' in obj;']);
  });
});

describe('js/delete', function (): any {
  return it('(compile \'(js/delete x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/delete'), Symbol.for('x')]]], 'delete x;']);
  });
});

describe('js/try', function (): any {
  it('(js/try (/ 1 2) (catch e (display "there was an error")) (finally (display "finally")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/try'), [Symbol.for('/'), 1, 2], [Symbol.for('catch'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'finally']]], 0.5]);
  });
  it('(js/try (/ 1 3) (/ 1 2) (catch e (display "there was an error")) (finally (display "finally")))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/try'), [Symbol.for('/'), 1, 3], [Symbol.for('/'), 1, 2], [Symbol.for('catch'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'finally']]], 0.5]);
  });
  it('(compile \'(js/try))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try')]]], 'try {\n' +
      '}']);
  });
  it('(compile \'(js/try (set! x (/ 2 1))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]]]]], 'try {\n' +
      '  x = 2 / 1;\n' +
      '}']);
  });
  it('(compile \'(js/try (set! x (/ 2 1)) (finally (display "cleanup"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], 'try {\n' +
      '  x = 2 / 1;\n' +
      '} finally {\n' +
      '  console.log(\'cleanup\');\n' +
      '}']);
  });
  it('(compile \'(js/try (/ 1 2) (catch e (display "there was an error")) (finally (display "finally"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('/'), 1, 2], [Symbol.for('catch'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'finally']]]]], 'try {\n' +
      '  1 / 2;\n' +
      '} catch (e) {\n' +
      '  console.log(\'there was an error\');\n' +
      '} finally {\n' +
      '  console.log(\'finally\');\n' +
      '}']);
  });
  it('(compile \'(js/try (set! x (/ 2 1)) (catch _ (display "there was an error")) (finally (display "cleanup"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('catch'), Symbol.for('_'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], 'try {\n' +
      '  x = 2 / 1;\n' +
      '} catch {\n' +
      '  console.log(\'there was an error\');\n' +
      '} finally {\n' +
      '  console.log(\'cleanup\');\n' +
      '}']);
  });
  it('(compile \'(js/try (set! x (/ 2 1)) (catch e (display "there was an error")) (finally (display "cleanup"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('catch'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], 'try {\n' +
      '  x = 2 / 1;\n' +
      '} catch (e) {\n' +
      '  console.log(\'there was an error\');\n' +
      '} finally {\n' +
      '  console.log(\'cleanup\');\n' +
      '}']);
  });
  return it('(compile \'(js/try (set! x (/ 2 1)) (catch e (display "there was an error")) (finally (display "cleanup"))))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('catch'), Symbol.for('e'), [Symbol.for('display'), 'there was an error']], [Symbol.for('finally'), [Symbol.for('display'), 'cleanup']]]]], 'try {\n' +
      '  x = 2 / 1;\n' +
      '} catch (e) {\n' +
      '  console.log(\'there was an error\');\n' +
      '} finally {\n' +
      '  console.log(\'cleanup\');\n' +
      '}']);
  });
});

describe('js/+', function (): any {
  it('(js/+)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+')], 0]);
  });
  it('(js/+ 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 1], 1]);
  });
  it('(js/+ 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 1, 2], 3]);
  });
  it('(js/+ 2 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 2, 2], 4]);
  });
  it('(js/+ 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 1, 2, 3], 6]);
  });
  it('(js/+ 1 2 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 1, 2, 4], 7]);
  });
  it('(js/+ (js/+ 1 1) (js/+ 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), [Symbol.for('js/+'), 1, 1], [Symbol.for('js/+'), 1, 1]], 4]);
  });
  it('(let ((x 2)) (js/+ x x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('let'), [[Symbol.for('x'), 2]], [Symbol.for('js/+'), Symbol.for('x'), Symbol.for('x')]], 4]);
  });
  it('(js/+ 1 "")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/+'), 1, ''], '1']);
  });
  it('(compile \'(js/+ 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/+'), 1, 1]]], '1 + 1;']);
  });
  return it('(compile \'(js/+ 1 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/+'), 1, 1, 1]]], '1 + 1 + 1;']);
  });
});

describe('js/-', function (): any {
  it('(js/-)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/-')], 0]);
  });
  it('(js/- 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/-'), 1], -1]);
  });
  it('(js/- 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/-'), 1, 2], -1]);
  });
  it('(js/- 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/-'), 1, 2, 3], -4]);
  });
  it('(js/- 1 2 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/-'), 1, 2, 4], -5]);
  });
  it('(compile \'(js/- 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/-'), 1]]], '-1;']);
  });
  it('(compile \'(js/- 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/-'), 1, 1]]], '1 - 1;']);
  });
  return it('(compile \'(js/- 1 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/-'), 1, 1, 1]]], '1 - 1 - 1;']);
  });
});

describe('js/*', function (): any {
  it('(js/*)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/*')], 1]);
  });
  it('(js/* 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/*'), 1], 1]);
  });
  it('(js/* 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/*'), 1, 2], 2]);
  });
  it('(js/* 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/*'), 1, 2, 3], 6]);
  });
  it('(js/* 1 2 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/*'), 1, 2, 4], 8]);
  });
  it('(compile \'(js/* 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/*'), 1, 1]]], '1 * 1;']);
  });
  return it('(compile \'(js/* 1 1 1))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/*'), 1, 1, 1]]], '1 * 1 * 1;']);
  });
});

describe('js//', function (): any {
  it('(js//)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js//')], undefined]);
  });
  it('(js// 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js//'), 1], 1]);
  });
  it('(js// 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js//'), 1, 2], 0.5]);
  });
  it('(js// 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js//'), 1, 2, 3], [Symbol.for('js//'), 1, 2, 3]]);
  });
  it('(/ 1 2 4)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('/'), 1, 2, 4], 0.125]);
  });
  it('(compile \'(js// 1 2))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js//'), 1, 2]]], '1 / 2;']);
  });
  return it('(compile \'(js// 1 2 4))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js//'), 1, 2, 4]]], '1 / 2 / 4;']);
  });
});

describe('js/<', function (): any {
  it('(js/< 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<'), 1, 2], true]);
  });
  it('(js/< 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<'), 2, 1], false]);
  });
  it('(js/< 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<'), 1, 2, 3], true]);
  });
  it('(js/< 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<'), 2, 1, 3], false]);
  });
  it('(js/< 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<'), 1, 3, 2], false]);
  });
  it('(funcall js/< 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<'), 1, 2], true]);
  });
  it('(funcall js/< 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<'), 2, 1], false]);
  });
  it('(funcall js/< 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<'), 1, 2, 3], true]);
  });
  it('(funcall js/< 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<'), 2, 1, 3], false]);
  });
  it('(funcall js/< 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<'), 1, 3, 2], false]);
  });
  it('(compile \'(js/< x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/<'), Symbol.for('x'), Symbol.for('y')]]], 'x < y;']);
  });
  return it('(compile \'(js/< x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/<'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x < y) && (y < z);']);
  });
});

describe('js/<=', function (): any {
  it('(js/<= 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 1, 2], true]);
  });
  it('(js/<= 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 2, 1], false]);
  });
  it('(js/<= 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 1, 2, 3], true]);
  });
  it('(js/<= 1 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 1, 1, 2], true]);
  });
  it('(js/<= 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 2, 1, 3], false]);
  });
  it('(js/<= 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/<='), 1, 3, 2], false]);
  });
  it('(funcall js/<= 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 1, 2], true]);
  });
  it('(funcall js/<= 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 2, 1], false]);
  });
  it('(funcall js/<= 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 1, 2, 3], true]);
  });
  it('(funcall js/<= 1 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 1, 1, 2], true]);
  });
  it('(funcall js/<= 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 2, 1, 3], false]);
  });
  it('(funcall js/<= 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/<='), 1, 3, 2], false]);
  });
  it('(compile \'(js/<= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/<='), Symbol.for('x'), Symbol.for('y')]]], 'x <= y;']);
  });
  return it('(compile \'(js/<= x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/<='), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x <= y) && (y <= z);']);
  });
});

describe('js/>', function (): any {
  it('(js/> 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 2, 1], true]);
  });
  it('(js/> 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 1, 2], false]);
  });
  it('(js/> 3 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 3, 2, 1], true]);
  });
  it('(js/> 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 1, 2, 3], false]);
  });
  it('(js/> 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 2, 1, 3], false]);
  });
  it('(js/> 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>'), 1, 3, 2], false]);
  });
  it('(funcall js/> 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 2, 1], true]);
  });
  it('(funcall js/> 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 1, 2], false]);
  });
  it('(funcall js/> 3 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 3, 2, 1], true]);
  });
  it('(funcall js/> 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 1, 2, 3], false]);
  });
  it('(funcall js/> 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 2, 1, 3], false]);
  });
  it('(funcall js/> 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>'), 1, 3, 2], false]);
  });
  it('(compile \'(js/> x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>'), Symbol.for('x'), Symbol.for('y')]]], 'x > y;']);
  });
  return it('(compile \'(js/> x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x > y) && (y > z);']);
  });
});

describe('js/>=', function (): any {
  it('(js/>= 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 2, 1], true]);
  });
  it('(js/>= 2 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 2, 2], true]);
  });
  it('(js/>= 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 1, 2], false]);
  });
  it('(js/>= 3 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 3, 2, 1], true]);
  });
  it('(js/>= 3 2 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 3, 2, 2], true]);
  });
  it('(js/>= 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 1, 2, 3], false]);
  });
  it('(js/>= 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 2, 1, 3], false]);
  });
  it('(js/>= 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('js/>='), 1, 3, 2], false]);
  });
  it('(funcall js/>= 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 2, 1], true]);
  });
  it('(funcall js/>= 2 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 2, 2], true]);
  });
  it('(funcall js/>= 1 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 1, 2], false]);
  });
  it('(funcall js/>= 3 2 1)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 3, 2, 1], true]);
  });
  it('(funcall js/>= 3 2 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 3, 2, 2], true]);
  });
  it('(funcall js/>= 1 2 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 1, 2, 3], false]);
  });
  it('(funcall js/>= 2 1 3)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 2, 1, 3], false]);
  });
  it('(funcall js/>= 1 3 2)', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('funcall'), Symbol.for('js/>='), 1, 3, 2], false]);
  });
  it('(compile \'(js/>= x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>='), Symbol.for('x'), Symbol.for('y')]]], 'x >= y;']);
  });
  return it('(compile \'(js/>= x y z))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/>='), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]]], '(x >= y) && (y >= z);']);
  });
});

describe('js/%', function (): any {
  return it('(compile \'(js/% x y))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/%'), Symbol.for('x'), Symbol.for('y')]]], 'x % y;']);
  });
});

describe('js/abs', function (): any {
  return it('(compile \'(js/abs x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/abs'), Symbol.for('x')]]], 'Math.abs(x);']);
  });
});

describe('js/tag', function (): any {
  return it('(compile \'(js/tag foo "bar"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/tag'), Symbol.for('foo'), 'bar']]], 'foo`bar`;']);
  });
});

describe('js/rename', function (): any {
  return it('(compile \'(js/rename ((x y)) x))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/rename'), [[Symbol.for('x'), Symbol.for('y')]], Symbol.for('x')]]], 'y;']);
  });
});

describe('js/statement-or-expression', function (): any {
  it('(compile \'(js/statement-or-expression :statement 1 :expression 2) :as "statement")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), 1, Symbol.for(':expression'), 2]], Symbol.for(':as'), 'statement'], '1;']);
  });
  it('(compile \'(js/statement-or-expression :statement 1 :expression 2) :as "expression")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), 1, Symbol.for(':expression'), 2]], Symbol.for(':as'), 'expression'], '2']);
  });
  it('(compile \'(js/statement-or-expression :statement 1 :expression 2 :return 3) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), 1, Symbol.for(':expression'), 2, Symbol.for(':return'), 3]], Symbol.for(':as'), 'return'], 'return 3;']);
  });
  it('(compile \'(js/statement-or-expression :statement 1 :expression 2) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), 1, Symbol.for(':expression'), 2]], Symbol.for(':as'), 'return'], 'return 2;']);
  });
  return it('(compile \'(js/statement-or-expression :statement 1) :as "return")', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/statement-or-expression'), Symbol.for(':statement'), 1]], Symbol.for(':as'), 'return'], 'return 1;']);
  });
});

describe('js/raw', function (): any {
  it('(compile \'(js/raw "1"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/raw'), '1']]], '1']);
  });
  return it('(compile \'(js/raw "function I(x) { return x; }"))', function (): any {
    return testRepl([Symbol.for('roselisp'), Symbol.for('>'), [Symbol.for('compile'), [Symbol.for('quote'), [Symbol.for('js/raw'), 'function I(x) { return x; }']]], 'function I(x) { return x; }']);
  });
});