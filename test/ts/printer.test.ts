import {
  ArrowFunctionExpression,
  AssignmentExpression,
  AssignmentPattern,
  BinaryExpression,
  BlockStatement,
  CallExpression,
  ExportAllDeclaration,
  ExpressionStatement,
  ForStatement,
  FunctionExpression,
  Identifier,
  IfStatement,
  LeadingComment,
  Literal,
  LogicalExpression,
  MemberExpression,
  ObjectExpression,
  ReturnStatement,
  TSAnyKeyword,
  TSAsExpression,
  TSFunctionType,
  TSNumberKeyword,
  TSTypeAliasDeclaration,
  TSTypeAnnotation,
  TSTypeParameterInstantiation,
  TSTypeReference,
  TaggedTemplateExpression,
  TemplateElement,
  TemplateLiteral,
  TrailingComment,
  VariableDeclaration,
  VariableDeclarator,
  WhileStatement
} from '../../src/ts/estree';

import {
  printEstree,
  writeToString
} from '../../src/ts/printer';

import {
  s,
  sexp
} from '../../src/ts/sexp';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('print-estree', (): any => {
  it('foo', (): any => assertEqual(printEstree(new Identifier('foo')), 'foo'));
  it('foo, leading comment', (): any => assertEqual(printEstree(new Identifier('foo').addComment(new LeadingComment('comment')), {
    comments: true
  }), `// comment
foo`));
  it('foo, multi-line comment', (): any => assertEqual(printEstree(new Identifier('foo').addComment(new LeadingComment(`multi-line
comment`)), {
    comments: true
  }), `// multi-line
// comment
foo`));
  it('function (x) { return x; }', (): any => assertEqual(printEstree(new FunctionExpression([new Identifier('x')], new BlockStatement([new ReturnStatement(new Identifier('x'))]))), `function (x) {
  return x;
}`));
  it('foo()', (): any => assertEqual(printEstree(new CallExpression(new Identifier('foo'))), 'foo()'));
  it('foo(1)', (): any => assertEqual(printEstree(new CallExpression(new Identifier('foo'), [new Literal(1)])), 'foo(1)'));
  it('foo(1, 2)', (): any => assertEqual(printEstree(new CallExpression(new Identifier('foo'), [new Literal(1), new Literal(2)])), 'foo(1, 2)'));
  it('foo.bar()', (): any => assertEqual(printEstree(new CallExpression(new MemberExpression(new Identifier('foo'), new Identifier('bar')), [])), 'foo.bar()'));
  it('({}).foo()', (): any => assertEqual(printEstree(new CallExpression(new MemberExpression(new ObjectExpression(), new Identifier('foo')), [])), '({}).foo()'));
  it('a + b', (): any => assertEqual(printEstree(new BinaryExpression('+', new Identifier('a'), new Identifier('b'))), 'a + b'));
  it('a + b, leading comment', (): any => assertEqual(printEstree(new BinaryExpression('+', new Identifier('a').addComment(new LeadingComment('comment')), new Identifier('b')), {
    comments: true
  }), `(
 // comment
 a +
 b
)`));
  it('(a + b) + c, leading comment', (): any => assertEqual(printEstree(new BinaryExpression('+', new BinaryExpression('+', new Identifier('a'), new Identifier('b')).addComment(new LeadingComment('comment')), new Identifier('c')), {
    comments: true
  }), `(
 // comment
 a + b +
 c
)`));
  it('(a + b) + c, trailing comment', (): any => assertEqual(printEstree(new BinaryExpression('+', new BinaryExpression('+', new Identifier('a'), new Identifier('b')).addComment(new TrailingComment('comment')), new Identifier('c')), {
    comments: true
  }), `(
 a + b // comment
 +
 c
)`));
  it('a + b + c', (): any => assertEqual(printEstree(new BinaryExpression('+', new BinaryExpression('+', new Identifier('a'), new Identifier('b')), new Identifier('c'))), 'a + b + c'));
  xit('a + b + c + d, leading comments', (): any => assertEqual(printEstree(new BinaryExpression('+', new BinaryExpression('+', new BinaryExpression('+', new Identifier('a'), new Identifier('b')), new Identifier('c')), new Identifier('d'))), 'a + b + c'));
  it('a < b', (): any => assertEqual(printEstree(new BinaryExpression('<', new Identifier('a'), new Identifier('b'))), 'a < b'));
  it('a < b < c', (): any => assertEqual(printEstree(new BinaryExpression('<', new BinaryExpression('<', new Identifier('a'), new Identifier('b')), new Identifier('c'))), 'a < b < c'));
  it('a && b', (): any => assertEqual(printEstree(new LogicalExpression('&&', new Identifier('a'), new Identifier('b'))), 'a && b'));
  it('a || b', (): any => assertEqual(printEstree(new LogicalExpression('||', new Identifier('a'), new Identifier('b'))), 'a || b'));
  it('if (x) { x = 1; }', (): any => assertEqual(printEstree(new IfStatement(new Identifier('x'), new BlockStatement([new ExpressionStatement(new AssignmentExpression('=', new Identifier('x'), new Literal(1)))]))), `if (x) {
  x = 1;
}`));
  it('if ((x = 1)) { x = 1; }', (): any => assertEqual(printEstree(new IfStatement(new AssignmentExpression('=', new Identifier('x'), new Literal(1)), new BlockStatement([new ExpressionStatement(new AssignmentExpression('=', new Identifier('x'), new Literal(1)))]))), `if ((x = 1)) {
  x = 1;
}`));
  it('while (x) { x = 1; }', (): any => assertEqual(printEstree(new WhileStatement(new Identifier('x'), new BlockStatement([new ExpressionStatement(new AssignmentExpression('=', new Identifier('x'), new Literal(1)))]))), `while (x) {
  x = 1;
}`));
  it('while ((x = 1)) { x = 1; }', (): any => assertEqual(printEstree(new WhileStatement(new AssignmentExpression('=', new Identifier('x'), new Literal(1)), new BlockStatement([new ExpressionStatement(new AssignmentExpression('=', new Identifier('x'), new Literal(1)))]))), `while ((x = 1)) {
  x = 1;
}`));
  it('for (i = 0; i < 10; i = i + 1) { x = 1; }', (): any => assertEqual(printEstree(new ForStatement(new AssignmentExpression('=', new Identifier('i'), new Literal(0)), new BinaryExpression('<', new Identifier('i'), new Literal(10)), new AssignmentExpression('=', new Identifier('i'), new BinaryExpression('+', new Identifier('i'), new Literal(1))), new BlockStatement([new ExpressionStatement(new AssignmentExpression('=', new Identifier('x'), new Literal(1)))]))), `for (i = 0; i < 10; i = i + 1) {
  x = 1;
}`));
  it('(print-estree (new ReturnStatement (new Literal 0)))', (): any => assertEqual(printEstree(new ReturnStatement(new Literal(0))), 'return 0;'));
  it('return ( ... );', (): any => assertEqual(printEstree(new ReturnStatement(new Literal(0).addComment(new LeadingComment('comment'))), {
    comments: true
  }), `return (
  // comment
  0
);`));
  it('const x: number = 1;', (): any => assertEqual(printEstree(new TSTypeAliasDeclaration(new Identifier('X'), new TSNumberKeyword()), {
    to: 'typescript'
  }), 'type X = number;'));
  it('1 as number', (): any => assertEqual(printEstree(new TSAsExpression(new Literal(1), new TSNumberKeyword()), {
    to: 'typescript'
  }), '1 as number'));
  it('x as any', (): any => assertEqual(printEstree(new TSAsExpression(new Identifier('x'), new TSAnyKeyword()), {
    to: 'typescript'
  }), 'x as any'));
  it('x as Foo', (): any => assertEqual(printEstree(new TSAsExpression(new Identifier('x'), new TSTypeReference(new Identifier('Foo'))), {
    to: 'typescript'
  }), 'x as Foo'));
  it('x as Promise<any>', (): any => assertEqual(printEstree(new TSAsExpression(new Identifier('x'), new TSTypeReference(new Identifier('Promise'), new TSTypeParameterInstantiation([new TSAnyKeyword()]))), {
    to: 'typescript'
  }), 'x as Promise<any>'));
  it('const x: number = 1;', (): any => assertEqual(printEstree(new VariableDeclaration([new VariableDeclarator(new Identifier('x').setType(new TSNumberKeyword()), new Literal(1))], 'const'), {
    to: 'typescript'
  }), 'const x: number = 1;'));
  it('function (x: number): number { return x; }', (): any => assertEqual(printEstree(new FunctionExpression([new Identifier('x').setType(new TSNumberKeyword())], new BlockStatement([new ReturnStatement(new Identifier('x'))])).setType(new TSNumberKeyword()), {
    to: 'typescript'
  }), `function (x: number): number {
  return x;
}`));
  it('function (x: number = 1): number { return x; }', (): any => assertEqual(printEstree(new FunctionExpression([new AssignmentPattern(new Identifier('x').setType(new TSNumberKeyword()), new Literal(1))], new BlockStatement([new ReturnStatement(new Identifier('x'))])).setType(new TSNumberKeyword()), {
    to: 'typescript'
  }), `function (x: number = 1): number {
  return x;
}`));
  it('function (x: number = y): number { return x; }', (): any => assertEqual(printEstree(new FunctionExpression([new AssignmentPattern(new Identifier('x').setType(new TSNumberKeyword()), new Identifier('y'))], new BlockStatement([new ReturnStatement(new Identifier('x'))])).setType(new TSNumberKeyword()), {
    to: 'typescript'
  }), `function (x: number = y): number {
  return x;
}`));
  it('function (x: number): number { return x; }', (): any => assertEqual(printEstree(new ArrowFunctionExpression([new Identifier('x').setType(new TSNumberKeyword())], new BlockStatement([new ReturnStatement(new Identifier('x'))])).setType(new TSNumberKeyword()), {
    to: 'typescript'
  }), `(x: number): number => {
  return x;
}`));
  it('const f: (a: any) => any = (x: any): any => { return x; };', (): any => assertEqual(printEstree(new VariableDeclaration([new VariableDeclarator(new Identifier('f').setType(new TSFunctionType([new Identifier('a').setType(new TSAnyKeyword())], new TSTypeAnnotation(new TSAnyKeyword()))), new ArrowFunctionExpression([new Identifier('x')], new BlockStatement([new ReturnStatement(new Identifier('x'))])))], 'const'), {
    to: 'typescript'
  }), `const f: (a: any) => any = (x: any): any => {
  return x;
};`));
  it('`foo`', (): any => assertEqual(printEstree(new TemplateLiteral([new TemplateElement(true, 'foo')]), {
    to: 'typescript'
  }), '`foo`'));
  it(`\`foo
bar\``, (): any => assertEqual(printEstree(new TemplateLiteral([new TemplateElement(true, `foo
bar`)]), {
    to: 'typescript'
  }), `\`foo
bar\``));
  it(`\`foo
\\\`bar\``, (): any => assertEqual(printEstree(new TemplateLiteral([new TemplateElement(true, `foo
\`bar`)]), {
    to: 'typescript'
  }), `\`foo
\\\`bar\``));
  it(`function (): any { return \`foo
bar\`; }`, (): any => assertEqual(printEstree(new FunctionExpression([], new BlockStatement([new ReturnStatement(new TemplateLiteral([new TemplateElement(true, `foo
bar`)]))])).setType(new TSAnyKeyword()), {
    to: 'typescript'
  }), `function (): any {
  return \`foo
bar\`;
}`));
  it('foo`bar`', (): any => assertEqual(printEstree(new TaggedTemplateExpression(new Identifier('foo'), new TemplateLiteral([new TemplateElement(true, 'bar')])), {
    to: 'typescript'
  }), 'foo`bar`'));
  it(`foo\`bar
baz\``, (): any => assertEqual(printEstree(new TaggedTemplateExpression(new Identifier('foo'), new TemplateLiteral([new TemplateElement(true, `bar
baz`)])), {
    to: 'typescript'
  }), `foo\`bar
baz\``));
  it(`function (): any { return foo\`bar
baz\`; }`, (): any => assertEqual(printEstree(new FunctionExpression([], new BlockStatement([new ReturnStatement(new TaggedTemplateExpression(new Identifier('foo'), new TemplateLiteral([new TemplateElement(true, `bar
baz`)])))])).setType(new TSAnyKeyword()), {
    to: 'typescript'
  }), `function (): any {
  return foo\`bar
baz\`;
}`));
  it(`function (): any { return function (): any { return foo\`bar
baz\`; }; }`, (): any => assertEqual(printEstree(new FunctionExpression([], new BlockStatement([new ReturnStatement(new FunctionExpression([], new BlockStatement([new ReturnStatement(new TaggedTemplateExpression(new Identifier('foo'), new TemplateLiteral([new TemplateElement(true, `bar
baz`)])))])).setType(new TSAnyKeyword()))])).setType(new TSAnyKeyword()), {
    to: 'typescript'
  }), `function (): any {
  return function (): any {
    return foo\`bar
baz\`;
  };
}`));
  return it('export * from "foo";', (): any => assertEqual(printEstree(new ExportAllDeclaration(new Literal('foo')), {
    to: 'javascript'
  }), 'export * from \'foo\';'));
});

describe('write-to-string', (): any => {
  it('(write-to-string 1)', (): any => assertEqual(writeToString(1), '1'));
  it('(write-to-string \'foo)', (): any => assertEqual(writeToString(Symbol.for('foo')), 'foo'));
  it('(write-to-string "foo")', (): any => assertEqual(writeToString('foo'), '"foo"'));
  it('(write-to-string "foo")', (): any => assertEqual(writeToString('foo'), '"foo"'));
  it('(write-to-string "foo\\\\bar")', (): any => assertEqual(writeToString('foo\\bar'), '"foo\\\\bar"'));
  it('(write-to-string "\\\\")', (): any => assertEqual(writeToString('\\'), '"\\\\"'));
  it('(write-to-string "foo\\"bar")', (): any => assertEqual(writeToString('foo"bar'), '"foo\\"bar"'));
  it('(write-to-string \'())', (): any => assertEqual(writeToString([]), '()'));
  it('(write-to-string \'(1 . 2))', (): any => assertEqual(writeToString([1, Symbol.for('.'), 2]), '(1 . 2)'));
  it(`(write-to-string '(begin "foo
bar") (js/obj :pretty #t))`, (): any => assertEqual(writeToString([Symbol.for('begin'), `foo
bar`], {
    pretty: true
  }), `(begin
  "foo
bar")`));
  it('(write-to-string \'(begin "\\"foo bar\\"") (js/obj :pretty #t))', (): any => assertEqual(writeToString([Symbol.for('begin'), '"foo bar"'], {
    pretty: true
  }), `(begin
  "\\"foo bar\\"")`));
  it('(write-to-string 1)', (): any => assertEqual(writeToString(1), '1'));
  it('(write-to-string \'(foo))', (): any => assertEqual(writeToString([Symbol.for('foo')]), '(foo)'));
  it('(write-to-string \'(foo bar))', (): any => assertEqual(writeToString([Symbol.for('foo'), Symbol.for('bar')]), '(foo bar)'));
  it('(write-to-string \'(foo "bar"))', (): any => assertEqual(writeToString([Symbol.for('foo'), 'bar']), '(foo "bar")'));
  it('(write-to-string \'("foo" "bar"))', (): any => assertEqual(writeToString(['foo', 'bar']), '("foo" "bar")'));
  it('(write-to-string \'(foo (bar)))', (): any => assertEqual(writeToString([Symbol.for('foo'), [Symbol.for('bar')]]), '(foo (bar))'));
  it('(write-to-string \'(foo (bar (baz))))', (): any => assertEqual(writeToString([Symbol.for('foo'), [Symbol.for('bar'), [Symbol.for('baz')]]]), '(foo (bar (baz)))'));
  it('(write-to-string \'(begin (foo) (bar)) (js/obj :pretty #t))', (): any => assertEqual(writeToString([Symbol.for('begin'), [Symbol.for('foo')], [Symbol.for('bar')]], {
    pretty: true
  }), `(begin
  (foo)
  (bar))`));
  it('(write-to-string \'(begin (foo (bar)) (bar (baz))) (js/obj :pretty #t))', (): any => assertEqual(writeToString([Symbol.for('begin'), [Symbol.for('foo'), [Symbol.for('bar')]], [Symbol.for('bar'), [Symbol.for('baz')]]], {
    pretty: true
  }), `(begin
  (foo (bar))
  (bar (baz)))`));
  it('(write-to-string \'(cond (foo (bar)) (bar (baz))) (js/obj :pretty #t))', (): any => assertEqual(writeToString([Symbol.for('cond'), [Symbol.for('foo'), [Symbol.for('bar')]], [Symbol.for('bar'), [Symbol.for('baz')]]], {
    pretty: true
  }), `(cond
 (foo
  (bar))
 (bar
  (baz)))`));
  it('(write-to-string \'(if foo bar baz) (js/obj :pretty #t))', (): any => assertEqual(writeToString([Symbol.for('if'), Symbol.for('foo'), Symbol.for('bar'), Symbol.for('baz')], {
    pretty: true
  }), `(if foo
    bar
    baz)`));
  it('(write-to-string \'(when foo bar) (js/obj :pretty #t))', (): any => assertEqual(writeToString([Symbol.for('when'), Symbol.for('foo'), Symbol.for('bar')], {
    pretty: true
  }), `(when foo
  bar)`));
  it('(write-to-string \'(unless foo bar) (js/obj :pretty #t))', (): any => assertEqual(writeToString([Symbol.for('unless'), Symbol.for('foo'), Symbol.for('bar')], {
    pretty: true
  }), `(unless foo
  bar)`));
  it('(write-to-string \'(define (foo x) x) (js/obj :pretty #t))', (): any => assertEqual(writeToString([Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')], {
    pretty: true
  }), `(define (foo x)
  x)`));
  it('(write-to-string \'(module m scheme (define (foo x) x) (define (bar y) y)) (js/obj :pretty #t))', (): any => assertEqual(writeToString([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('define'), [Symbol.for('bar'), Symbol.for('y')], Symbol.for('y')]], {
    pretty: true
  }), `(module m scheme
  (define (foo x)
    x)

  (define (bar y)
    y))`));
  return it('(write-to-string \'(module m scheme (define (foo x) x) (define (bar y) y)) (js/obj :no-module-form #t :pretty #t))', (): any => assertEqual(writeToString([Symbol.for('module'), Symbol.for('m'), Symbol.for('scheme'), [Symbol.for('define'), [Symbol.for('foo'), Symbol.for('x')], Symbol.for('x')], [Symbol.for('define'), [Symbol.for('bar'), Symbol.for('y')], Symbol.for('y')]], {
    noModuleForm: true,
    pretty: true
  }), `(define (foo x)
  x)

(define (bar y)
  y)`));
});