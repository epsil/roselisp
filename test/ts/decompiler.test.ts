import {
  I
} from '../../src/ts/combinators';

import {
  decompile
} from '../../src/ts/language';

import {
  writeToString
} from '../../src/ts/printer';

import {
  sexp
} from '../../src/ts/sexp';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('decompile', (): any => {
  it('(decompile "true;")', (): any => assertEqual(decompile('true;'), true));
  it('(decompile "false")', (): any => assertEqual(decompile('false'), false));
  it('(decompile "const foo = undefined;")', (): any => assertEqual(decompile('const foo = undefined;'), [Symbol.for('define'), Symbol.for('foo'), Symbol.for('undefined')]));
  it('(decompile "const foo = null;")', (): any => assertEqual(decompile('const foo = null;'), [Symbol.for('define'), Symbol.for('foo'), Symbol.for('js/null')]));
  it('(decompile "const foo = this;")', (): any => assertEqual(decompile('const foo = this;'), [Symbol.for('define'), Symbol.for('foo'), Symbol.for('this')]));
  it('(decompile "0")', (): any => assertEqual(decompile('0'), 0));
  it('(decompile "1")', (): any => assertEqual(decompile('1'), 1));
  it('(decompile "-1")', (): any => assertEqual(decompile('-1'), -1));
  it('(decompile "\'foo\'")', (): any => assertEqual(decompile('\'foo\''), 'foo'));
  it('(decompile "const foo = `bar`;")', (): any => assertEqual(decompile('const foo = `bar`;'), [Symbol.for('define'), Symbol.for('foo'), 'bar']));
  it('(decompile "const foo = bar`baz`;")', (): any => assertEqual(decompile('const foo = bar`baz`;'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('js/tag'), Symbol.for('bar'), 'baz']]));
  it(`(decompile "const foo = \`bar
\\\\\`baz\`;")`, (): any => assertEqual(decompile(`const foo = \`bar
\\\`baz\`;`), [Symbol.for('define'), Symbol.for('foo'), `bar
\`baz`]));
  it('(decompile "/foo/")', (): any => assertEqual(decompile('/foo/'), [Symbol.for('js/regexp'), 'foo']));
  it('(decompile "/foo/g")', (): any => assertEqual(decompile('/foo/g'), [Symbol.for('js/regexp'), 'foo', 'g']));
  xit('(decompile "const exp = /.*/;")', (): any => assertEqual(decompile('const exp = /.*/;'), [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('regexp'), '.*']]));
  it('(decompile "const regexp = /.*/;")', (): any => assertEqual(decompile('const regexp = /.*/;'), [Symbol.for('define'), Symbol.for('regexp'), [Symbol.for('js/regexp'), '.*']]));
  it('(decompile "const exp = /.*/;")', (): any => assertEqual(decompile('const exp = /.*/;'), [Symbol.for('define'), Symbol.for('exp'), [Symbol.for('js/regexp'), '.*']]));
  it('(decompile "[]")', (): any => assertEqual(decompile('[]'), [Symbol.for('list')]));
  it('(decompile "const foo = [];")', (): any => assertEqual(decompile('const foo = [];'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('list')]]));
  it('(decompile "[1, 2, 3]")', (): any => assertEqual(decompile('[1, 2, 3]'), [Symbol.for('list'), 1, 2, 3]));
  it('(decompile "[...x]")', (): any => assertEqual(decompile('[...x]'), [Symbol.for('append'), Symbol.for('x')]));
  it('(decompile "[x, ...y]")', (): any => assertEqual(decompile('[x, ...y]'), [Symbol.for('append'), [Symbol.for('list'), Symbol.for('x')], Symbol.for('y')]));
  it('(decompile "x[0]")', (): any => assertEqual(decompile('x[0]'), [Symbol.for('list-ref'), Symbol.for('x'), 0]));
  it('(decompile "x[0](a, b)")', (): any => assertEqual(decompile('x[0](a, b)'), [[Symbol.for('list-ref'), Symbol.for('x'), 0], Symbol.for('a'), Symbol.for('b')]));
  it('(decompile "x[0][1]")', (): any => assertEqual(decompile('x[0][1]'), [Symbol.for('list-ref'), Symbol.for('x'), 0, 1]));
  it('(decompile "x[len]")', (): any => assertEqual(decompile('x[len]'), [Symbol.for('oget'), Symbol.for('x'), Symbol.for('len')]));
  it('(decompile "x[len - 1]")', (): any => assertEqual(decompile('x[len - 1]'), [Symbol.for('oget'), Symbol.for('x'), [Symbol.for('-'), Symbol.for('len'), 1]]));
  it('(decompile "x[0] = 1")', (): any => assertEqual(decompile('x[0] = 1'), [Symbol.for('list-set!'), Symbol.for('x'), 0, 1]));
  it('(decompile "x[\'foo\'] = 1")', (): any => assertEqual(decompile('x[\'foo\'] = 1'), [Symbol.for('oset!'), Symbol.for('x'), 'foo', 1]));
  it('(decompile "!foo")', (): any => assertEqual(decompile('!foo'), [Symbol.for('not'), Symbol.for('foo')]));
  it('(decompile "1 + 2")', (): any => assertEqual(decompile('1 + 2'), [Symbol.for('+'), 1, 2]));
  it('(decompile "1 + 2 + 3")', (): any => assertEqual(decompile('1 + 2 + 3'), [Symbol.for('+'), 1, 2, 3]));
  it('(decompile "1 + \'\'")', (): any => assertEqual(decompile('1 + \'\''), [Symbol.for('string-append'), 1, '']));
  it('(decompile "1 + 2 + \'\'")', (): any => assertEqual(decompile('1 + 2 + \'\''), [Symbol.for('string-append'), 1, 2, '']));
  it('(decompile "-1")', (): any => assertEqual(decompile('-1'), -1));
  it('(decompile "-(1)")', (): any => assertEqual(decompile('-(1)'), -1));
  it('(decompile "-x")', (): any => assertEqual(decompile('-x'), [Symbol.for('-'), Symbol.for('x')]));
  it('(decompile "1 - 2")', (): any => assertEqual(decompile('1 - 2'), [Symbol.for('-'), 1, 2]));
  it('(decompile "1 - 2 - 3")', (): any => assertEqual(decompile('1 - 2 - 3'), [Symbol.for('-'), 1, 2, 3]));
  it('(decompile "1 - (2 - 3)")', (): any => assertEqual(decompile('1 - (2 - 3)'), [Symbol.for('-'), 1, [Symbol.for('-'), 2, 3]]));
  it('(decompile "1 * 2")', (): any => assertEqual(decompile('1 * 2'), [Symbol.for('*'), 1, 2]));
  it('(decompile "1 * 2 * 3")', (): any => assertEqual(decompile('1 * 2 * 3'), [Symbol.for('*'), 1, 2, 3]));
  it('(decompile "1 / 2")', (): any => assertEqual(decompile('1 / 2'), [Symbol.for('/'), 1, 2]));
  xit('(decompile "1 / 2 / 3")', (): any => assertEqual(decompile('1 / 2 / 3'), [Symbol.for('/'), 1, 2, 3]));
  it('(decompile "x && y")', (): any => assertEqual(decompile('x && y'), [Symbol.for('and'), Symbol.for('x'), Symbol.for('y')]));
  it('(decompile "x && y && z")', (): any => assertEqual(decompile('x && y && z'), [Symbol.for('and'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]));
  it('(decompile "typeof x === \'number\' && typeof y === \'number\'")', (): any => assertEqual(decompile('typeof x === \'number\' && typeof y === \'number\''), [Symbol.for('and'), [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('x')], 'number'], [Symbol.for('eq?'), [Symbol.for('type-of'), Symbol.for('y')], 'number']]));
  it('(decompile "x || y")', (): any => assertEqual(decompile('x || y'), [Symbol.for('or'), Symbol.for('x'), Symbol.for('y')]));
  it('(decompile "x || y || z")', (): any => assertEqual(decompile('x || y || z'), [Symbol.for('or'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]));
  it('(decompile "x === y")', (): any => assertEqual(decompile('x === y'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('y')]));
  it('(decompile "x !== y")', (): any => assertEqual(decompile('x !== y'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('x'), Symbol.for('y')]]));
  it('(decompile "x in y")', (): any => assertEqual(decompile('x in y'), [Symbol.for('js/in'), Symbol.for('x'), Symbol.for('y')]));
  it('(decompile "x instanceof y")', (): any => assertEqual(decompile('x instanceof y'), [Symbol.for('is-a?'), Symbol.for('x'), Symbol.for('y')]));
  it('(decompile "typeof x")', (): any => assertEqual(decompile('typeof x'), [Symbol.for('type-of'), Symbol.for('x')]));
  it('(decompile "foo(bar);")', (): any => assertEqual(decompile('foo(bar);'), [Symbol.for('foo'), Symbol.for('bar')]));
  it('(decompile "foo(bar);")', (): any => assertEqual(decompile('foo(bar);'), [Symbol.for('foo'), Symbol.for('bar')]));
  it('(decompile "foo(bar);" :module #t)', (): any => assertEqual(decompile('foo(bar);', Symbol.for(':module'), true), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('foo'), Symbol.for('bar')]]));
  it('(decompile "foo(bar);" :module #t)', (): any => assertEqual(decompile('foo(bar);', Symbol.for(':module'), true), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('foo'), Symbol.for('bar')]]));
  it('(decompile "foo(\'bar\');")', (): any => assertEqual(decompile('foo(\'bar\');'), [Symbol.for('foo'), 'bar']));
  it('(decompile "foo(\'bar\');")', (): any => assertEqual(decompile('foo(\'bar\');'), [Symbol.for('foo'), 'bar']));
  it('(decompile "foo(\'bar\', \'baz\');")', (): any => assertEqual(decompile('foo(\'bar\', \'baz\');'), [Symbol.for('foo'), 'bar', 'baz']));
  it('(decompile "foo(\'bar\', \'baz\');")', (): any => assertEqual(decompile('foo(\'bar\', \'baz\');'), [Symbol.for('foo'), 'bar', 'baz']));
  it('(decompile "foo(1, 2, 3);")', (): any => assertEqual(decompile('foo(1, 2, 3);'), [Symbol.for('foo'), 1, 2, 3]));
  it('(decompile "foo(...args);")', (): any => assertEqual(decompile('foo(...args);'), [Symbol.for('apply'), Symbol.for('foo'), Symbol.for('args')]));
  it('(decompile "foo(x, ...args);")', (): any => assertEqual(decompile('foo(x, ...args);'), [Symbol.for('apply'), Symbol.for('foo'), Symbol.for('x'), Symbol.for('args')]));
  it('(decompile "foo(...args, x);")', (): any => assertEqual(decompile('foo(...args, x);'), [Symbol.for('apply'), Symbol.for('foo'), [Symbol.for('append'), Symbol.for('args'), [Symbol.for('list'), Symbol.for('x')]]]));
  it('(decompile "foo(x, ...args, y);")', (): any => assertEqual(decompile('foo(x, ...args, y);'), [Symbol.for('apply'), Symbol.for('foo'), [Symbol.for('append'), [Symbol.for('list'), Symbol.for('x')], Symbol.for('args'), [Symbol.for('list'), Symbol.for('y')]]]));
  xit(`(decompile "// comment
foo(bar);")`, (): any => assertEqual(decompile(`// comment
foo(bar);`), [Symbol.for('foo'), Symbol.for('bar')]));
  it('(decompile "x = 1;")', (): any => assertEqual(decompile('x = 1;'), [Symbol.for('set!'), Symbol.for('x'), 1]));
  it('(decompile "x += 1;")', (): any => assertEqual(decompile('x += 1;'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('+'), Symbol.for('x'), 1]]));
  it('(decompile "x -= 1;")', (): any => assertEqual(decompile('x -= 1;'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('-'), Symbol.for('x'), 1]]));
  it('(decompile "x *= 1;")', (): any => assertEqual(decompile('x *= 1;'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('*'), Symbol.for('x'), 1]]));
  it('(decompile "x /= 1;")', (): any => assertEqual(decompile('x /= 1;'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), Symbol.for('x'), 1]]));
  it('(decompile "x ||= y;")', (): any => assertEqual(decompile('x ||= y;'), [Symbol.for('js/op'), Symbol.for('||='), Symbol.for('x'), Symbol.for('y')]));
  it('(decompile "x &&= y;")', (): any => assertEqual(decompile('x &&= y;'), [Symbol.for('js/op'), Symbol.for('&&='), Symbol.for('x'), Symbol.for('y')]));
  it('(decompile "x ??= y;")', (): any => assertEqual(decompile('x ??= y;'), [Symbol.for('js/op'), Symbol.for('??='), Symbol.for('x'), Symbol.for('y')]));
  it('(decompile "let x = 1;")', (): any => assertEqual(decompile('let x = 1;'), [Symbol.for('define'), Symbol.for('x'), 1]));
  it('(decompile "let x = undefined;")', (): any => assertEqual(decompile('let x = undefined;'), [Symbol.for('define'), Symbol.for('x'), Symbol.for('undefined')]));
  it('(decompile "let x;")', (): any => assertEqual(decompile('let x;'), [Symbol.for('define'), Symbol.for('x')]));
  it('(decompile "let x = 1, y = 2;")', (): any => assertEqual(decompile('let x = 1, y = 2;'), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define'), Symbol.for('y'), 2]]));
  it(`(decompile "let x = 1, y = 2;
let z = 3;")`, (): any => assertEqual(decompile(`let x = 1, y = 2;
let z = 3;`), [Symbol.for('begin'), [Symbol.for('define'), Symbol.for('x'), 1], [Symbol.for('define'), Symbol.for('y'), 2], [Symbol.for('define'), Symbol.for('z'), 3]]));
  it('(decompile "const x = 1")', (): any => assertEqual(decompile('const x = 1'), [Symbol.for('define'), Symbol.for('x'), 1]));
  it('(decompile "let [x] = arr;")', (): any => assertEqual(decompile('let [x] = arr;'), [Symbol.for('define-values'), [Symbol.for('x')], Symbol.for('arr')]));
  it('(decompile "let [x, y] = arr;")', (): any => assertEqual(decompile('let [x, y] = arr;'), [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('y')], Symbol.for('arr')]));
  it('(decompile "[x, y] = arr;")', (): any => assertEqual(decompile('[x, y] = arr;'), [Symbol.for('set!-values'), [Symbol.for('x'), Symbol.for('y')], Symbol.for('arr')]));
  it('(decompile "let [x, ...y] = arr;")', (): any => assertEqual(decompile('let [x, ...y] = arr;'), [Symbol.for('define-values'), [Symbol.for('x'), Symbol.for('.'), Symbol.for('y')], Symbol.for('arr')]));
  it('(decompile "let [, y] = arr;")', (): any => assertEqual(decompile('let [, y] = arr;'), [Symbol.for('define-values'), [Symbol.for('_'), Symbol.for('y')], Symbol.for('arr')]));
  it('(decompile "let {x, y} = obj;")', (): any => assertEqual(decompile('let {x, y} = obj;'), [Symbol.for('define-fields'), [Symbol.for('x'), Symbol.for('y')], Symbol.for('obj')]));
  it('(decompile "({x, y} = obj);")', (): any => assertEqual(decompile('({x, y} = obj);'), [Symbol.for('set!-fields'), [Symbol.for('x'), Symbol.for('y')], Symbol.for('obj')]));
  it('(decompile "let {x: y, z} = obj;")', (): any => assertEqual(decompile('let {x: y, z} = obj;'), [Symbol.for('define-fields'), [[Symbol.for('x'), Symbol.for('y')], Symbol.for('z')], Symbol.for('obj')]));
  it('(decompile "x.y;")', (): any => assertEqual(decompile('x.y;'), [Symbol.for('get-field'), Symbol.for('y'), Symbol.for('x')]));
  it('(decompile "x.len")', (): any => assertEqual(decompile('x.len'), [Symbol.for('get-field'), Symbol.for('len'), Symbol.for('x')]));
  it('(decompile "x.length")', (): any => assertEqual(decompile('x.length'), [Symbol.for('length'), Symbol.for('x')]));
  xit('(decompile "let length = x.length;")', (): any => assertEqual(decompile('let length = x.length;'), [Symbol.for('define'), Symbol.for('length'), [Symbol.for('js/length'), Symbol.for('x')]]));
  it('(decompile "x?.y;")', (): any => assertEqual(decompile('x?.y;'), [Symbol.for('js/?.'), Symbol.for('x'), Symbol.for('y')]));
  it('(decompile "x?.y();")', (): any => assertEqual(decompile('x?.y();'), [[Symbol.for('js/?.'), Symbol.for('x'), Symbol.for('y')]]));
  it('(decompile "x?.y(z);")', (): any => assertEqual(decompile('x?.y(z);'), [[Symbol.for('js/?.'), Symbol.for('x'), Symbol.for('y')], Symbol.for('z')]));
  it('(decompile "foo()?.y;")', (): any => assertEqual(decompile('foo()?.y;'), [Symbol.for('js/?.'), [Symbol.for('foo')], Symbol.for('y')]));
  it('(decompile "x?.[0];")', (): any => assertEqual(decompile('x?.[0];'), [Symbol.for('list-ref'), [Symbol.for('js/?.'), Symbol.for('x')], 0]));
  it('(decompile "x?.[y];")', (): any => assertEqual(decompile('x?.[y];'), [Symbol.for('oget'), [Symbol.for('js/?.'), Symbol.for('x')], Symbol.for('y')]));
  it('(decompile "x.y = z;")', (): any => assertEqual(decompile('x.y = z;'), [Symbol.for('set-field!'), Symbol.for('y'), Symbol.for('x'), Symbol.for('z')]));
  it('(decompile "x.y();")', (): any => assertEqual(decompile('x.y();'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('y')]));
  it('(decompile "x?.y();")', (): any => assertEqual(decompile('x?.y();'), [[Symbol.for('js/?.'), Symbol.for('x'), Symbol.for('y')]]));
  it('(decompile "x()?.y();")', (): any => assertEqual(decompile('x()?.y();'), [[Symbol.for('js/?.'), [Symbol.for('x')], Symbol.for('y')]]));
  it('(decompile "x.y(z);")', (): any => assertEqual(decompile('x.y(z);'), [Symbol.for('send'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]));
  it('(decompile "x.y(...z);")', (): any => assertEqual(decompile('x.y(...z);'), [Symbol.for('send/apply'), Symbol.for('x'), Symbol.for('y'), Symbol.for('z')]));
  it(`(decompile "function I(x) {
  return x;}")`, (): any => assertEqual(decompile(`function I(x) {
  return x;}`), [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], Symbol.for('x')]));
  it(`(decompile "function I(x) {
  foo();
  return x;
}")`, (): any => assertEqual(decompile(`function I(x) {
  foo();
  return x;
}`), [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], [Symbol.for('foo')], Symbol.for('x')]));
  it(`(decompile "function I(x: any, y?: any) {
  return x;
}" :from 'typescript)`, (): any => assertEqual(decompile(`function I(x: any, y?: any) {
  return x;
}`, Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('define'), [Symbol.for('I'), [Symbol.for('x'), Symbol.for(':'), Symbol.for('Any')], [Symbol.for('y'), Symbol.for('undefined')]], Symbol.for('x')]));
  it(`(decompile "function I(x: any, y: any = true) {
  return x;
}" :from 'typescript)`, (): any => assertEqual(decompile(`function I(x: any, y: any = true) {
  return x;
}`, Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('define'), [Symbol.for('I'), [Symbol.for('x'), Symbol.for(':'), Symbol.for('Any')], [Symbol.for('y'), Symbol.for(':'), Symbol.for('Any'), true]], Symbol.for('x')]));
  it(`(decompile "function I(x: number, y: number = 1) {
  return x;
}" :from 'typescript)`, (): any => assertEqual(decompile(`function I(x: number, y: number = 1) {
  return x;
}`, Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('define'), [Symbol.for('I'), [Symbol.for('x'), Symbol.for(':'), Symbol.for('Number')], [Symbol.for('y'), Symbol.for(':'), Symbol.for('Number'), 1]], Symbol.for('x')]));
  it(`(decompile "function foo(x = 1) {
  return x;
}")`, (): any => assertEqual(decompile(`function foo(x = 1) {
  return x;
}`), [Symbol.for('define'), [Symbol.for('foo'), [Symbol.for('x'), 1]], Symbol.for('x')]));
  it(`(decompile "function foo(...args) {
  return args;
}")`, (): any => assertEqual(decompile(`function foo(...args) {
  return args;
}`), [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('.'), Symbol.for('args')], Symbol.for('args')]));
  it(`(decompile "function I(x) {
  if (x) {
    return x;
  } else {
    return false;
  }
}")`, (): any => assertEqual(decompile(`function I(x) {
  if (x) {
    return x;
  } else {
    return false;
  }
}`), [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], [Symbol.for('if'), Symbol.for('x'), Symbol.for('x'), false]]));
  it(`(decompile "function I(x) {
  if (x) {
    return x;
  } else if (false) {
    return false;
  } else {
    return false;
  }
}")`, (): any => assertEqual(decompile(`function I(x) {
  if (x) {
    return x;
  } else if (false) {
    return false;
  } else {
    return false;
  }
}`), [Symbol.for('define'), [Symbol.for('I'), Symbol.for('x')], [Symbol.for('cond'), [Symbol.for('x'), Symbol.for('x')], [false, false], [Symbol.for('else'), false]]]));
  it(`(decompile "let I = function (x) {
  return x;
};")`, (): any => assertEqual(decompile(`let I = function (x) {
  return x;
};`), [Symbol.for('define'), Symbol.for('I'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]));
  it(`(decompile "let foo = function (...args) {
  return args;
};")`, (): any => assertEqual(decompile(`let foo = function (...args) {
  return args;
};`), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('lambda'), Symbol.for('args'), Symbol.for('args')]]));
  it(`(decompile "let I = (x) => {
  return x;
};")`, (): any => assertEqual(decompile(`let I = (x) => {
  return x;
};`), [Symbol.for('define'), Symbol.for('I'), [Symbol.for('js/arrow'), [Symbol.for('x')], Symbol.for('x')]]));
  it(`(decompile "let I = (x: any) => {
  return x;
};" :from 'typescript)`, (): any => assertEqual(decompile(`let I = (x: any) => {
  return x;
};`, Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('define'), Symbol.for('I'), [Symbol.for('js/arrow'), [[Symbol.for('x'), Symbol.for(':'), Symbol.for('Any')]], Symbol.for('x')]]));
  it(`(decompile "if (true) {
  foo('bar');
}")`, (): any => assertEqual(decompile(`if (true) {
  foo('bar');
}`), [Symbol.for('when'), true, [Symbol.for('foo'), 'bar']]));
  it(`(decompile "if (!foo) {
  bar('baz');
}")`, (): any => assertEqual(decompile(`if (!foo) {
  bar('baz');
}`), [Symbol.for('unless'), Symbol.for('foo'), [Symbol.for('bar'), 'baz']]));
  it(`(decompile "if (true) {
  foo('bar');
} else {
  bar('baz');
}")`, (): any => assertEqual(decompile(`if (true) {
  foo('bar');
} else {
  bar('baz');
}`), [Symbol.for('if'), true, [Symbol.for('foo'), 'bar'], [Symbol.for('bar'), 'baz']]));
  it(`(decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  bar('baz');
}")`, (): any => assertEqual(decompile(`if (x) {
  if (y) {
    foo('bar');
  }
} else {
  bar('baz');
}`), [Symbol.for('if'), Symbol.for('x'), [Symbol.for('when'), Symbol.for('y'), [Symbol.for('foo'), 'bar']], [Symbol.for('bar'), 'baz']]));
  it(`(decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (z) {
    bar('baz');
  }
}")`, (): any => assertEqual(decompile(`if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (z) {
    bar('baz');
  }
}`), [Symbol.for('cond'), [Symbol.for('x'), [Symbol.for('when'), Symbol.for('y'), [Symbol.for('foo'), 'bar']]], [Symbol.for('z'), [Symbol.for('bar'), 'baz']]]));
  it(`(decompile "if (x) {
  foo();
  bar();
} else if (y) {
  baz();
  quux();
}")`, (): any => assertEqual(decompile(`if (x) {
  foo();
  bar();
} else if (y) {
  baz();
  quux();
}`), [Symbol.for('cond'), [Symbol.for('x'), [Symbol.for('foo')], [Symbol.for('bar')]], [Symbol.for('y'), [Symbol.for('baz')], [Symbol.for('quux')]]]));
  it(`(decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (z) {
    bar('baz');
  } else {
    baz('quux');
  }
}")`, (): any => assertEqual(decompile(`if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (z) {
    bar('baz');
  } else {
    baz('quux');
  }
}`), [Symbol.for('cond'), [Symbol.for('x'), [Symbol.for('when'), Symbol.for('y'), [Symbol.for('foo'), 'bar']]], [Symbol.for('z'), [Symbol.for('bar'), 'baz']], [Symbol.for('else'), [Symbol.for('baz'), 'quux']]]));
  it(`(decompile "if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (!z) {
    bar('baz');
  } else {
    baz('quux');
  }
}")`, (): any => assertEqual(decompile(`if (x) {
  if (y) {
    foo('bar');
  }
} else {
  if (!z) {
    bar('baz');
  } else {
    baz('quux');
  }
}`), [Symbol.for('cond'), [Symbol.for('x'), [Symbol.for('when'), Symbol.for('y'), [Symbol.for('foo'), 'bar']]], [[Symbol.for('not'), Symbol.for('z')], [Symbol.for('bar'), 'baz']], [Symbol.for('else'), [Symbol.for('baz'), 'quux']]]));
  it(`(decompile "if (x) {
  foo('bar');
} else if (y) {
  bar('baz');
} else {
  baz('quux');
}")`, (): any => assertEqual(decompile(`if (x) {
  foo('bar');
} else if (y) {
  bar('baz');
} else {
  baz('quux');
}`), [Symbol.for('cond'), [Symbol.for('x'), [Symbol.for('foo'), 'bar']], [Symbol.for('y'), [Symbol.for('bar'), 'baz']], [Symbol.for('else'), [Symbol.for('baz'), 'quux']]]));
  it(`(decompile "if (x) {
  foo();
} else {
  bar();
  baz();
}")`, (): any => assertEqual(decompile(`if (x) {
  foo();
} else {
  bar();
  baz();
}`), [Symbol.for('cond'), [Symbol.for('x'), [Symbol.for('foo')]], [Symbol.for('else'), [Symbol.for('bar')], [Symbol.for('baz')]]]));
  it('(decompile "let x = true ? foo : bar;")', (): any => assertEqual(decompile('let x = true ? foo : bar;'), [Symbol.for('define'), Symbol.for('x'), [Symbol.for('if'), true, Symbol.for('foo'), Symbol.for('bar')]]));
  it('(decompile "let x = 1 ? foo : 2 ? bar : baz")', (): any => assertEqual(decompile('let x = 1 ? foo : 2 ? bar : baz'), [Symbol.for('define'), Symbol.for('x'), [Symbol.for('cond'), [1, Symbol.for('foo')], [2, Symbol.for('bar')], [Symbol.for('else'), Symbol.for('baz')]]]));
  it(`(decompile "while (foo) {
  bar();}")`, (): any => assertEqual(decompile(`while (foo) {
  bar();}`), [Symbol.for('do'), [], [[Symbol.for('not'), Symbol.for('foo')]], [Symbol.for('bar')]]));
  it(`(decompile "do {
  bar();
} while (foo);")`, (): any => assertEqual(decompile(`do {
  bar();
} while (foo);`), [Symbol.for('js/do-while'), [[Symbol.for('bar')]], Symbol.for('foo')]));
  it(`(decompile "do {
  bar();
  baz();
} while (foo);")`, (): any => assertEqual(decompile(`do {
  bar();
  baz();
} while (foo);`), [Symbol.for('js/do-while'), [[Symbol.for('bar')], [Symbol.for('baz')]], Symbol.for('foo')]));
  it(`(decompile "for (let i = 0; i < 10; i++) {
  foo();
  break;
}")`, (): any => assertEqual(decompile(`for (let i = 0; i < 10; i++) {
  foo();
  break;
}`), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, 10]]], [Symbol.for('foo')], [Symbol.for('break')]]));
  it(`(decompile "for (i = 0; i < arr.length; i++) {
  foo();
}")`, (): any => assertEqual(decompile(`for (i = 0; i < arr.length; i++) {
  foo();
}`), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 0, [Symbol.for('length'), Symbol.for('arr')]]]], [Symbol.for('foo')]]));
  it(`(decompile "for (let i = 10; i > 0; i--) {
  foo();
}")`, (): any => assertEqual(decompile(`for (let i = 10; i > 0; i--) {
  foo();
}`), [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), 10, 0, -1]]], [Symbol.for('foo')]]));
  it(`(decompile "for (let i = 0, j = 0; i < 10; i++, j++) {
  foo();
  break bar;
}")`, (): any => assertEqual(decompile(`for (let i = 0, j = 0; i < 10; i++, j++) {
  foo();
  break bar;
}`), [Symbol.for('do'), [[Symbol.for('i'), 0, [Symbol.for('+'), Symbol.for('i'), 1]], [Symbol.for('j'), 0, [Symbol.for('+'), Symbol.for('j'), 1]]], [[Symbol.for('not'), [Symbol.for('<'), Symbol.for('i'), 10]]], [Symbol.for('foo')], [Symbol.for('break'), Symbol.for('bar')]]));
  it(`(decompile "for (let x of foo) {
  bar();
  continue;
}")`, (): any => assertEqual(decompile(`for (let x of foo) {
  bar();
  continue;
}`), [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('foo')]], [Symbol.for('bar')], [Symbol.for('continue')]]));
  it(`(decompile "for (const [name, value] of entries) {
  result.insert(value, [name]);
}")`, (): any => assertEqual(decompile(`for (const [name, value] of entries) {
  result.insert(value, [name]);
}`), [Symbol.for('for'), [[Symbol.for('x'), Symbol.for('entries')]], [Symbol.for('define-values'), [Symbol.for('name'), Symbol.for('value')], Symbol.for('x')], [Symbol.for('send'), Symbol.for('result'), Symbol.for('insert'), Symbol.for('value'), [Symbol.for('list'), Symbol.for('name')]]]));
  it(`(decompile "for (const [name, value] of x) {
  result.insert(value, [name]);
}")`, (): any => assertEqual(decompile(`for (const [name, value] of x) {
  result.insert(value, [name]);
}`), [Symbol.for('for'), [[Symbol.for('x1'), Symbol.for('x')]], [Symbol.for('define-values'), [Symbol.for('name'), Symbol.for('value')], Symbol.for('x1')], [Symbol.for('send'), Symbol.for('result'), Symbol.for('insert'), Symbol.for('value'), [Symbol.for('list'), Symbol.for('name')]]]));
  it(`(decompile "for (let x in foo) {
  bar();
}")`, (): any => assertEqual(decompile(`for (let x in foo) {
  bar();
}`), [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('js/keys'), Symbol.for('foo')]]], [Symbol.for('bar')]]));
  it('(decompile "new Foo();")', (): any => assertEqual(decompile('new Foo();'), [Symbol.for('new'), Symbol.for('Foo')]));
  it('(decompile "new Foo(\'bar\');")', (): any => assertEqual(decompile('new Foo(\'bar\');'), [Symbol.for('new'), Symbol.for('Foo'), 'bar']));
  it('(decompile "new Foo(...args);")', (): any => assertEqual(decompile('new Foo(...args);'), [Symbol.for('apply'), Symbol.for('new'), Symbol.for('Foo'), Symbol.for('args')]));
  it('(decompile "new Foo(x, ...args);")', (): any => assertEqual(decompile('new Foo(x, ...args);'), [Symbol.for('apply'), Symbol.for('new'), Symbol.for('Foo'), Symbol.for('x'), Symbol.for('args')]));
  it('(decompile "delete x")', (): any => assertEqual(decompile('delete x'), [Symbol.for('js/delete'), Symbol.for('x')]));
  it('(decompile "throw new Error(\'An error\')")', (): any => assertEqual(decompile('throw new Error(\'An error\')'), [Symbol.for('throw'), [Symbol.for('new'), Symbol.for('Error'), 'An error']]));
  xit(`(decompile "try {
}")`, (): any => assertEqual(decompile(`try {
}`), [Symbol.for('try')]));
  it(`(decompile "try {
  x = 2 / 1;
} finally {
  foo();
}")`, (): any => assertEqual(decompile(`try {
  x = 2 / 1;
} finally {
  foo();
}`), [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('finally'), [Symbol.for('foo')]]]));
  it(`(decompile "try {
  x = 2 / 1;
} catch {
  foo();
}")`, (): any => assertEqual(decompile(`try {
  x = 2 / 1;
} catch {
  foo();
}`), [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('_'), [Symbol.for('foo')]]]));
  it(`(decompile "try {
  x = 2 / 1;
} catch (e) {
  foo();
} finally {
  bar();
}")`, (): any => assertEqual(decompile(`try {
  x = 2 / 1;
} catch (e) {
  foo();
} finally {
  bar();
}`), [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('x'), [Symbol.for('/'), 2, 1]], [Symbol.for('catch'), Symbol.for('Object'), Symbol.for('e'), [Symbol.for('foo')]], [Symbol.for('finally'), [Symbol.for('bar')]]]));
  it(`(decompile "const I = async function (x) {
  return x;
};")`, (): any => assertEqual(decompile(`const I = async function (x) {
  return x;
};`), [Symbol.for('define'), Symbol.for('I'), [Symbol.for('async'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]));
  it(`(decompile "async function I(x) {
  return x;
}" :module #t)`, (): any => assertEqual(decompile(`async function I(x) {
  return x;
}`, Symbol.for(':module'), true), [Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), Symbol.for('I'), [Symbol.for('async'), [Symbol.for('lambda'), [Symbol.for('x')], Symbol.for('x')]]]]));
  it('(decompile "import \'foo\';")', (): any => assertEqual(decompile('import \'foo\';'), [Symbol.for('require'), 'foo']));
  it('(decompile "import * as foo from \'bar\';")', (): any => assertEqual(decompile('import * as foo from \'bar\';'), [Symbol.for('require'), Symbol.for('foo'), 'bar']));
  it('(decompile "import { foo } from \'bar\';")', (): any => assertEqual(decompile('import { foo } from \'bar\';'), [Symbol.for('require'), [Symbol.for('only-in'), 'bar', Symbol.for('foo')]]));
  it('(decompile "import { foo as bar } from \'baz\';")', (): any => assertEqual(decompile('import { foo as bar } from \'baz\';'), [Symbol.for('require'), [Symbol.for('only-in'), 'baz', [Symbol.for('foo'), Symbol.for('bar')]]]));
  it('(decompile "export {};")', (): any => assertEqual(decompile('export {};'), [Symbol.for('provide')]));
  it(`(decompile "export {
  foo
};")`, (): any => assertEqual(decompile(`export {
  foo
};`), [Symbol.for('provide'), Symbol.for('foo')]));
  it(`(decompile "export {
  foo as bar
};")`, (): any => assertEqual(decompile(`export {
  foo as bar
};`), [Symbol.for('provide'), [Symbol.for('rename-out'), [Symbol.for('foo'), Symbol.for('bar')]]]));
  it('(decompile "export * from \'foo\';")', (): any => assertEqual(decompile('export * from \'foo\';'), [Symbol.for('provide'), [Symbol.for('all-from-out'), 'foo']]));
  it('(decompile "({});")', (): any => assertEqual(decompile('({});'), [Symbol.for('js/obj')]));
  it('(decompile "({ foo: bar });")', (): any => assertEqual(decompile('({ foo: bar });'), [Symbol.for('js/obj'), 'foo', Symbol.for('bar')]));
  xit('(decompile "({ [foo]: bar });")', (): any => assertEqual(decompile('({ [foo]: bar });'), [Symbol.for('js/obj'), Symbol.for('foo'), Symbol.for('bar')]));
  it('(decompile "const foo = {};")', (): any => assertEqual(decompile('const foo = {};'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('js/obj')]]));
  it('(decompile "const foo = { bar: true };")', (): any => assertEqual(decompile('const foo = { bar: true };'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('js/obj'), 'bar', true]]));
  it('(decompile "const foo = { ...{ bar: true } };")', (): any => assertEqual(decompile('const foo = { ...{ bar: true } };'), [Symbol.for('define'), Symbol.for('foo'), [Symbol.for('js/obj-append'), [Symbol.for('js/obj'), 'bar', true]]]));
  it(`(decompile "class Foo {
}")`, (): any => assertEqual(decompile(`class Foo {
}`), [Symbol.for('define-class'), Symbol.for('Foo'), []]));
  it(`(decompile "class Foo extends Bar {
}")`, (): any => assertEqual(decompile(`class Foo extends Bar {
}`), [Symbol.for('define-class'), Symbol.for('Foo'), [Symbol.for('Bar')]]));
  it(`(decompile "class Foo {
  bar = 1;
}")`, (): any => assertEqual(decompile(`class Foo {
  bar = 1;
}`), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('bar'), 1]]));
  it(`(decompile "class Foo {
  bar() {
    return 1;
  }
}")`, (): any => assertEqual(decompile(`class Foo {
  bar() {
    return 1;
  }
}`), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), [Symbol.for('bar')], 1]]));
  it(`(decompile "class Foo {
  bar;

  constructor() {
    this.bar = 1;
  }
}")`, (): any => assertEqual(decompile(`class Foo {
  bar;

  constructor() {
    this.bar = 1;
  }
}`), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('bar')], [Symbol.for('define'), [Symbol.for('constructor')], [Symbol.for('set-field!'), Symbol.for('bar'), Symbol.for('this'), 1]]]));
  it(`(decompile "class Foo extends Bar {
  constructor() {
    super();
  }
}")`, (): any => assertEqual(decompile(`class Foo extends Bar {
  constructor() {
    super();
  }
}`), [Symbol.for('define-class'), Symbol.for('Foo'), [Symbol.for('Bar')], [Symbol.for('define'), [Symbol.for('constructor')], [Symbol.for('super')]]]));
  it(`(decompile "class Foo extends Bar {
  constructor(x) {
    super(x);
  }
}")`, (): any => assertEqual(decompile(`class Foo extends Bar {
  constructor(x) {
    super(x);
  }
}`), [Symbol.for('define-class'), Symbol.for('Foo'), [Symbol.for('Bar')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('x')], [Symbol.for('super'), Symbol.for('x')]]]));
  it(`(decompile "class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  *generator() {
    for (let x of this.arr) {
      yield x;
    }
  }
}")`, (): any => assertEqual(decompile(`class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  *generator() {
    for (let x of this.arr) {
      yield x;
    }
  }
}`), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('arr')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('arr')], [Symbol.for('set-field!'), Symbol.for('arr'), Symbol.for('this'), Symbol.for('arr')]], [Symbol.for('define/generator'), [Symbol.for('generator')], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('get-field'), Symbol.for('arr'), Symbol.for('this')]]], [Symbol.for('yield'), Symbol.for('x')]]]]));
  it(`(decompile "class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  *[Symbol.iterator]() {
    for (let x of this.arr) {
      yield x;
    }
  }
}")`, (): any => assertEqual(decompile(`class Foo {
  arr;

  constructor(arr) {
    this.arr = arr;
  }

  *[Symbol.iterator]() {
    for (let x of this.arr) {
      yield x;
    }
  }
}`), [Symbol.for('define-class'), Symbol.for('Foo'), [], [Symbol.for('define/public'), Symbol.for('arr')], [Symbol.for('define'), [Symbol.for('constructor'), Symbol.for('arr')], [Symbol.for('set-field!'), Symbol.for('arr'), Symbol.for('this'), Symbol.for('arr')]], [Symbol.for('define/generator'), [[Symbol.for('get-field'), Symbol.for('iterator'), Symbol.for('Symbol')]], [Symbol.for('for'), [[Symbol.for('x'), [Symbol.for('get-field'), Symbol.for('arr'), Symbol.for('this')]]], [Symbol.for('yield'), Symbol.for('x')]]]]));
  it('(decompile "foo as any" :from \'typescript)', (): any => assertEqual(decompile('foo as any', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('Any')]));
  it('(decompile "foo as number" :from \'typescript)', (): any => assertEqual(decompile('foo as number', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('Number')]));
  it('(decompile "foo as boolean" :from \'typescript)', (): any => assertEqual(decompile('foo as boolean', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('Boolean')]));
  it('(decompile "foo as true" :from \'typescript)', (): any => assertEqual(decompile('foo as true', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('True')]));
  it('(decompile "foo as MyClass" :from \'typescript)', (): any => assertEqual(decompile('foo as MyClass', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('ann'), Symbol.for('foo'), Symbol.for('MyClass')]));
  it('(decompile "foo as MyClass<Any>" :from \'typescript\')', (): any => assertEqual(decompile('foo as MyClass<Any>', Symbol.for(':from'), Symbol.for('typescript\'')), [Symbol.for('ann'), Symbol.for('foo'), [Symbol.for('MyClass'), Symbol.for('Any')]]));
  it('(decompile "foo as any[]" :from \'typescript)', (): any => assertEqual(decompile('foo as any[]', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('ann'), Symbol.for('foo'), [Symbol.for('Listof'), Symbol.for('Any')]]));
  it('(decompile "foo as [any]" :from \'typescript)', (): any => assertEqual(decompile('foo as [any]', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('ann'), Symbol.for('foo'), [Symbol.for('List'), Symbol.for('Any')]]));
  it('(decompile "foo as [number, any]" :from \'typescript)', (): any => assertEqual(decompile('foo as [number, any]', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('ann'), Symbol.for('foo'), [Symbol.for('List'), Symbol.for('Number'), Symbol.for('Any')]]));
  it('(decompile "foo as number | string" :from \'typescript)', (): any => assertEqual(decompile('foo as number | string', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('ann'), Symbol.for('foo'), [Symbol.for('U'), Symbol.for('Number'), Symbol.for('String')]]));
  it('(decompile "foo as string | undefined" :from \'typescript)', (): any => assertEqual(decompile('foo as string | undefined', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('ann'), Symbol.for('foo'), [Symbol.for('U'), Symbol.for('String'), Symbol.for('Undefined')]]));
  it('(decompile "foo as (a: any) => void" :from \'typescript)', (): any => assertEqual(decompile('foo as (a: any) => void', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('ann'), Symbol.for('foo'), [Symbol.for('->'), Symbol.for('Any'), Symbol.for('Void')]]));
  return it('(decompile "type NN = number;" :from \'typescript)', (): any => assertEqual(decompile('type NN = number;', Symbol.for(':from'), Symbol.for('typescript')), [Symbol.for('define-type'), Symbol.for('NN'), Symbol.for('Number')]));
});