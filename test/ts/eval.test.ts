/**
 * # Evaluation tests
 */

import {
  ArrayExpression,
  ArrayPattern,
  ArrowFunctionExpression,
  AssignmentExpression,
  BinaryExpression,
  BlockStatement,
  BreakStatement,
  CallExpression,
  CatchClause,
  ClassBody,
  ClassDeclaration,
  ClassExpression,
  ConditionalExpression,
  ContinueStatement,
  ExpressionStatement,
  ForOfStatement,
  ForStatement,
  FunctionDeclaration,
  FunctionExpression,
  Identifier,
  IfStatement,
  Literal,
  LogicalExpression,
  MemberExpression,
  MethodDefinition,
  NewExpression,
  ObjectExpression,
  Property,
  PropertyDefinition,
  RestElement,
  ReturnStatement,
  SpreadElement,
  ThisExpression,
  ThrowStatement,
  TryStatement,
  UnaryExpression,
  VariableDeclaration,
  VariableDeclarator,
  WhileStatement,
} from '../../src/ts/estree';

import { eval_, evalEstree, evalRose } from '../../src/ts/eval';

import {
  __,
  LispEnvironment,
  langEnvironment,
  lisp1Environment,
  evalLisp,
} from '../../src/ts/language';

import { wrapSexpInRose } from '../../src/ts/rose';

import { sexp } from '../../src/ts/sexp';

import { assertEqual, testMacro } from './test-util';

testMacro.ftype = 'macro';

describe('eval_', function (): any {
  it('(eval_ #t lisp-1-environment)', function (): any {
    return assertEqual(eval_(true, lisp1Environment), true);
  });
  it('(eval_ (js/tag sexp "true") lisp-1-environment)', function (): any {
    return assertEqual(eval_(sexp`true`, lisp1Environment), true);
  });
  it('(eval_ (js/tag sexp "t") lisp-1-environment)', function (): any {
    return assertEqual(eval_(sexp`t`, lisp1Environment), true);
  });
  it('(eval_ (js/tag sexp "#t") lisp-1-environment)', function (): any {
    return assertEqual(eval_(sexp`#t`, lisp1Environment), true);
  });
  it('(eval_ #f lisp-1-environment)', function (): any {
    return assertEqual(eval_(false, lisp1Environment), false);
  });
  it('(eval_ (js/tag sexp "false") lisp-1-environment)', function (): any {
    return assertEqual(eval_(sexp`false`, lisp1Environment), false);
  });
  return it('(eval_ (js/tag sexp "#f") lisp-1-environment)', function (): any {
    return assertEqual(eval_(sexp`#f`, lisp1Environment), false);
  });
});

describe('eval-rose', function (): any {
  it('(eval-rose (wrap-sexp-in-rose #t) lisp-1-environment)', function (): any {
    return assertEqual(evalRose(wrapSexpInRose(true), lisp1Environment), true);
  });
  return it('(eval-rose (wrap-sexp-in-rose #f) lisp-1-environment)', function (): any {
    return assertEqual(
      evalRose(wrapSexpInRose(false), lisp1Environment),
      false
    );
  });
});

describe('eval-estree', function (): any {
  it('#t', function (): any {
    return assertEqual(evalEstree(new Literal(true), lisp1Environment), true);
  });
  it('#f', function (): any {
    return assertEqual(evalEstree(new Literal(false), lisp1Environment), false);
  });
  it('0', function (): any {
    return assertEqual(evalEstree(new Literal(0), lisp1Environment), 0);
  });
  it('1', function (): any {
    return assertEqual(evalEstree(new Literal(1), lisp1Environment), 1);
  });
  it('2', function (): any {
    return assertEqual(evalEstree(new Literal(2), lisp1Environment), 2);
  });
  it('foo', function (): any {
    return assertEqual(
      evalEstree(new Identifier('foo'), lisp1Environment),
      undefined
    );
  });
  it('{}', function (): any {
    return assertEqual(
      evalEstree(new ObjectExpression(), lisp1Environment),
      {}
    );
  });
  it("{ foo: 'bar' }", function (): any {
    return assertEqual(
      evalEstree(
        new ObjectExpression([
          new Property(new Identifier('foo'), new Literal('bar')),
        ]),
        lisp1Environment
      ),
      {
        foo: 'bar',
      }
    );
  });
  it("{ const foo = { bar: 'baz' }; {...foo}; }", function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclarator(
            new Identifier('foo'),
            new ObjectExpression([
              new Property(new Identifier('bar'), new Literal('baz')),
            ])
          ),
          new ObjectExpression([new SpreadElement(new Identifier('foo'))]),
        ]),
        lisp1Environment
      ),
      {
        bar: 'baz',
      }
    );
  });
  it('!false', function (): any {
    return assertEqual(
      evalEstree(
        new UnaryExpression('!', false, new Literal(false)),
        lisp1Environment
      ),
      true
    );
  });
  it('{ let x = 0; ++x; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclarator(new Identifier('x'), new Literal(0)),
          new UnaryExpression('++', true, new Identifier('x')),
        ]),
        lisp1Environment
      ),
      1
    );
  });
  it('{ let x = 0; x++; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclarator(new Identifier('x'), new Literal(0)),
          new UnaryExpression('++', false, new Identifier('x')),
        ]),
        lisp1Environment
      ),
      0
    );
  });
  it('{ let x = 0; --x; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclarator(new Identifier('x'), new Literal(0)),
          new UnaryExpression('--', true, new Identifier('x')),
        ]),
        lisp1Environment
      ),
      -1
    );
  });
  it('{ let x = 0; x--; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclarator(new Identifier('x'), new Literal(0)),
          new UnaryExpression('--', false, new Identifier('x')),
        ]),
        lisp1Environment
      ),
      0
    );
  });
  it('1 < 2', function (): any {
    return assertEqual(
      evalEstree(
        new BinaryExpression('<', new Literal(1), new Literal(2)),
        lisp1Environment
      ),
      true
    );
  });
  it('1 > 2', function (): any {
    return assertEqual(
      evalEstree(
        new BinaryExpression('>', new Literal(1), new Literal(2)),
        lisp1Environment
      ),
      false
    );
  });
  it('1 + 2', function (): any {
    return assertEqual(
      evalEstree(
        new BinaryExpression('+', new Literal(1), new Literal(2)),
        lisp1Environment
      ),
      3
    );
  });
  it('1 - 2', function (): any {
    return assertEqual(
      evalEstree(
        new BinaryExpression('-', new Literal(1), new Literal(2)),
        lisp1Environment
      ),
      -1
    );
  });
  it('1 * 2', function (): any {
    return assertEqual(
      evalEstree(
        new BinaryExpression('*', new Literal(1), new Literal(2)),
        lisp1Environment
      ),
      2
    );
  });
  it('4 / 2', function (): any {
    return assertEqual(
      evalEstree(
        new BinaryExpression('/', new Literal(4), new Literal(2)),
        lisp1Environment
      ),
      2
    );
  });
  it('false && false', function (): any {
    return assertEqual(
      evalEstree(
        new LogicalExpression('&&', new Literal(false), new Literal(false)),
        lisp1Environment
      ),
      false
    );
  });
  it('false && true', function (): any {
    return assertEqual(
      evalEstree(
        new LogicalExpression('&&', new Literal(false), new Literal(true)),
        lisp1Environment
      ),
      false
    );
  });
  it('true && false', function (): any {
    return assertEqual(
      evalEstree(
        new LogicalExpression('&&', new Literal(true), new Literal(false)),
        lisp1Environment
      ),
      false
    );
  });
  it('true && true', function (): any {
    return assertEqual(
      evalEstree(
        new LogicalExpression('&&', new Literal(true), new Literal(true)),
        lisp1Environment
      ),
      true
    );
  });
  it('false || false', function (): any {
    return assertEqual(
      evalEstree(
        new LogicalExpression('||', new Literal(false), new Literal(false)),
        lisp1Environment
      ),
      false
    );
  });
  it('false || true', function (): any {
    return assertEqual(
      evalEstree(
        new LogicalExpression('||', new Literal(false), new Literal(true)),
        lisp1Environment
      ),
      true
    );
  });
  it('true || false', function (): any {
    return assertEqual(
      evalEstree(
        new LogicalExpression('||', new Literal(true), new Literal(false)),
        lisp1Environment
      ),
      true
    );
  });
  it('true || true', function (): any {
    return assertEqual(
      evalEstree(
        new LogicalExpression('||', new Literal(true), new Literal(true)),
        lisp1Environment
      ),
      true
    );
  });
  it('{ const x = 1; x; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration(
            [new VariableDeclarator(new Identifier('x'), new Literal(1))],
            'const'
          ),
          new Identifier('x'),
        ]),
        lisp1Environment
      ),
      1
    );
  });
  it('{ x = 1; x; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new ExpressionStatement(
            new AssignmentExpression('=', new Identifier('x'), new Literal(1))
          ),
          new Identifier('x'),
        ]),
        lisp1Environment
      ),
      1
    );
  });
  it('{ ([x] = [1]); x; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new ExpressionStatement(
            new AssignmentExpression(
              '=',
              new ArrayPattern([new Identifier('x')]),
              new ArrayExpression([new Literal(1)])
            )
          ),
          new Identifier('x'),
        ]),
        lisp1Environment
      ),
      1
    );
  });
  it("{ const x = {}; x.foo = 'bar'; x.foo; }", function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclarator(new Identifier('x'), new ObjectExpression()),
          new ExpressionStatement(
            new AssignmentExpression(
              '=',
              new MemberExpression(new Identifier('x'), new Identifier('foo')),
              new Literal('bar')
            )
          ),
          new MemberExpression(new Identifier('x'), new Identifier('foo')),
        ]),
        lisp1Environment
      ),
      'bar'
    );
  });
  it('{ 1; }', function (): any {
    return assertEqual(
      evalEstree(new BlockStatement([new Literal(1)]), lisp1Environment),
      1
    );
  });
  it('{ let x = true; if (x) { x = false; } x; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclarator(new Identifier('x'), new Literal(true)),
          new IfStatement(
            new Identifier('x'),
            new BlockStatement([
              new AssignmentExpression(
                '=',
                new Identifier('x'),
                new Literal(false)
              ),
            ])
          ),
          new Identifier('x'),
        ]),
        lisp1Environment
      ),
      false
    );
  });
  it('true ? 1 : 2', function (): any {
    return assertEqual(
      evalEstree(
        new ConditionalExpression(
          new Literal(true),
          new Literal(1),
          new Literal(2)
        ),
        lisp1Environment
      ),
      1
    );
  });
  it('false ? 1 : 2', function (): any {
    return assertEqual(
      evalEstree(
        new ConditionalExpression(
          new Literal(false),
          new Literal(1),
          new Literal(2)
        ),
        lisp1Environment
      ),
      2
    );
  });
  it('{ let x = true; while (x) { x = false; } x; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration([
            new VariableDeclarator(new Identifier('x'), new Literal(true)),
          ]),
          new WhileStatement(
            new Identifier('x'),
            new BlockStatement([
              new ExpressionStatement(
                new AssignmentExpression(
                  '=',
                  new Identifier('x'),
                  new Literal(false)
                )
              ),
            ])
          ),
          new Identifier('x'),
        ]),
        lisp1Environment
      ),
      false
    );
  });
  it('{ let x = true; while (x) { break; } x; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration([
            new VariableDeclarator(new Identifier('x'), new Literal(true)),
          ]),
          new WhileStatement(
            new Identifier('x'),
            new BlockStatement([new BreakStatement()])
          ),
          new Identifier('x'),
        ]),
        lisp1Environment
      ),
      true
    );
  });
  it('{ let x = 0; let i = 0; for (i = 0; i < 10; i = i + 1) { x = i; } x; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration(
            [
              new VariableDeclarator(new Identifier('x'), new Literal(0)),
              new VariableDeclarator(new Identifier('i'), new Literal(0)),
            ],
            'const'
          ),
          new ForStatement(
            new AssignmentExpression('=', new Identifier('i'), new Literal(0)),
            new BinaryExpression('<', new Identifier('i'), new Literal(10)),
            new AssignmentExpression(
              '=',
              new Identifier('i'),
              new BinaryExpression('+', new Identifier('i'), new Literal(1))
            ),
            new BlockStatement([
              new ExpressionStatement(
                new AssignmentExpression(
                  '=',
                  new Identifier('x'),
                  new Identifier('i')
                )
              ),
            ])
          ),
          new Identifier('x'),
        ]),
        lisp1Environment
      ),
      9
    );
  });
  it('{ let x = 0; let i = 0; for (i = 0; i < 10; i = i + 1) { if (i === 5) { break; } x = i; } x; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration(
            [
              new VariableDeclarator(new Identifier('x'), new Literal(0)),
              new VariableDeclarator(new Identifier('i'), new Literal(0)),
            ],
            'const'
          ),
          new ForStatement(
            new AssignmentExpression('=', new Identifier('i'), new Literal(0)),
            new BinaryExpression('<', new Identifier('i'), new Literal(10)),
            new AssignmentExpression(
              '=',
              new Identifier('i'),
              new BinaryExpression('+', new Identifier('i'), new Literal(1))
            ),
            new BlockStatement([
              new IfStatement(
                new BinaryExpression(
                  '===',
                  new Identifier('i'),
                  new Literal(5)
                ),
                new BlockStatement([new BreakStatement()])
              ),
              new ExpressionStatement(
                new AssignmentExpression(
                  '=',
                  new Identifier('x'),
                  new Identifier('i')
                )
              ),
            ])
          ),
          new Identifier('x'),
        ]),
        lisp1Environment
      ),
      4
    );
  });
  it('{ let x = 0; let i = 0; for (i = 0; i < 10; i = i + 1) { if (i === 9) { continue; } x = i; } x; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration(
            [
              new VariableDeclarator(new Identifier('x'), new Literal(0)),
              new VariableDeclarator(new Identifier('i'), new Literal(0)),
            ],
            'const'
          ),
          new ForStatement(
            new AssignmentExpression('=', new Identifier('i'), new Literal(0)),
            new BinaryExpression('<', new Identifier('i'), new Literal(10)),
            new AssignmentExpression(
              '=',
              new Identifier('i'),
              new BinaryExpression('+', new Identifier('i'), new Literal(1))
            ),
            new BlockStatement([
              new IfStatement(
                new BinaryExpression(
                  '===',
                  new Identifier('i'),
                  new Literal(9)
                ),
                new BlockStatement([new ContinueStatement()])
              ),
              new ExpressionStatement(
                new AssignmentExpression(
                  '=',
                  new Identifier('x'),
                  new Identifier('i')
                )
              ),
            ])
          ),
          new Identifier('x'),
        ]),
        lisp1Environment
      ),
      8
    );
  });
  it('{ let lst = []; for (let x of [1, 2, 3]) { lst.push(x); } lst; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('lst'),
                new ArrayExpression()
              ),
            ],
            'let'
          ),
          new ForOfStatement(
            new VariableDeclaration(
              [new VariableDeclarator(new Identifier('x'))],
              'let'
            ),
            new ArrayExpression([
              new Literal(1),
              new Literal(2),
              new Literal(3),
            ]),
            new BlockStatement([
              new CallExpression(
                new MemberExpression(
                  new Identifier('lst'),
                  new Identifier('push')
                ),
                [new Identifier('x')]
              ),
            ])
          ),
          new Identifier('lst'),
        ]),
        lisp1Environment
      ),
      [1, 2, 3]
    );
  });
  it('{ let lst = []; for (let x of [1, 2, 3]) { if (x === 2) { continue; } lst.push(x); } lst; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('lst'),
                new ArrayExpression()
              ),
            ],
            'let'
          ),
          new ForOfStatement(
            new VariableDeclaration(
              [new VariableDeclarator(new Identifier('x'))],
              'let'
            ),
            new ArrayExpression([
              new Literal(1),
              new Literal(2),
              new Literal(3),
            ]),
            new BlockStatement([
              new IfStatement(
                new BinaryExpression(
                  '===',
                  new Identifier('x'),
                  new Literal(2)
                ),
                new BlockStatement([new ContinueStatement()])
              ),
              new CallExpression(
                new MemberExpression(
                  new Identifier('lst'),
                  new Identifier('push')
                ),
                [new Identifier('x')]
              ),
            ])
          ),
          new Identifier('lst'),
        ]),
        lisp1Environment
      ),
      [1, 3]
    );
  });
  it('I(1)', function (): any {
    return assertEqual(
      evalEstree(
        new CallExpression(new Identifier('I'), [new Literal(1)]),
        new LispEnvironment([
          [
            Symbol.for('I'),
            function (x: any): any {
              return x;
            },
            'function',
          ],
        ])
      ),
      1
    );
  });
  it('(<fn> 1)', function (): any {
    return assertEqual(
      evalEstree(
        new FunctionExpression(
          [new Identifier('x')],
          new BlockStatement([new ReturnStatement(new Identifier('x'))])
        ),
        lisp1Environment
      )(1),
      1
    );
  });
  it('(<fn-with-rest-element> 1)', function (): any {
    return assertEqual(
      evalEstree(
        new FunctionExpression(
          [new RestElement(new Identifier('x'))],
          new BlockStatement([new ReturnStatement(new Identifier('x'))])
        ),
        lisp1Environment
      )(1),
      [1]
    );
  });
  it('(<arrow-fn> 1)', function (): any {
    return assertEqual(
      evalEstree(
        new ArrowFunctionExpression(
          [new Identifier('x')],
          new BlockStatement([new ReturnStatement(new Identifier('x'))])
        ),
        lisp1Environment
      )(1),
      1
    );
  });
  it('{ function I(x) { ... } I(1); }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new FunctionDeclaration(
            new Identifier('I'),
            [new Identifier('x')],
            new BlockStatement([new ReturnStatement(new Identifier('x'))])
          ),
          new CallExpression(new Identifier('I'), [new Literal(1)]),
        ]),
        lisp1Environment
      ),
      1
    );
  });
  it('{ try { throw new Error(); } catch (err) { } 1; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new TryStatement(
            new BlockStatement([
              new ThrowStatement(new NewExpression(new Identifier('Error'))),
            ]),
            new CatchClause(new Identifier('err'), new BlockStatement([]))
          ),
          new Literal(1),
        ]),
        langEnvironment
      ),
      1
    );
  });
  it('{ const Foo = class {}; const bar = new Foo(); bar instanceof Foo; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('Foo'),
                new ClassExpression()
              ),
            ],
            'const'
          ),
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('bar'),
                new NewExpression(new Identifier('Foo'))
              ),
            ],
            'const'
          ),
          new BinaryExpression(
            'instanceof',
            new Identifier('bar'),
            new Identifier('Foo')
          ),
        ]),
        langEnvironment
      ),
      true
    );
  });
  it('{ const Foo = class {}; const Bar = class extends Foo {}; const baz = new Bar(); baz instanceof Foo; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('Foo'),
                new ClassExpression()
              ),
            ],
            'const'
          ),
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('Bar'),
                new ClassExpression(new ClassBody(), new Identifier('Foo'))
              ),
            ],
            'const'
          ),
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('baz'),
                new NewExpression(new Identifier('Bar'))
              ),
            ],
            'const'
          ),
          new BinaryExpression(
            'instanceof',
            new Identifier('baz'),
            new Identifier('Foo')
          ),
        ]),
        langEnvironment
      ),
      true
    );
  });
  it("{ const Foo = class { bar = 'baz'; }; const quux = new Foo(); quux.bar; }", function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('Foo'),
                new ClassExpression(
                  new ClassBody([
                    new PropertyDefinition(
                      new Identifier('bar'),
                      new Literal('baz')
                    ),
                  ])
                )
              ),
            ],
            'const'
          ),
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('quux'),
                new NewExpression(new Identifier('Foo'))
              ),
            ],
            'const'
          ),
          new MemberExpression(new Identifier('quux'), new Identifier('bar')),
        ]),
        langEnvironment
      ),
      'baz'
    );
  });
  it("{ const Foo = class { bar() { return 'baz'; } }; const quux = new Foo(); quux.bar(); }", function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('Foo'),
                new ClassExpression(
                  new ClassBody([
                    new MethodDefinition(
                      new Identifier('bar'),
                      new FunctionExpression(
                        [],
                        new BlockStatement([
                          new ReturnStatement(new Literal('baz')),
                        ])
                      )
                    ),
                  ])
                )
              ),
            ],
            'const'
          ),
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('quux'),
                new NewExpression(new Identifier('Foo'))
              ),
            ],
            'const'
          ),
          new CallExpression(
            new MemberExpression(new Identifier('quux'), new Identifier('bar')),
            []
          ),
        ]),
        langEnvironment
      ),
      'baz'
    );
  });
  it('{ class Foo {} const bar = new Foo(); bar instanceof Foo; }', function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new ClassDeclaration(new Identifier('Foo')),
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('bar'),
                new NewExpression(new Identifier('Foo'))
              ),
            ],
            'const'
          ),
          new BinaryExpression(
            'instanceof',
            new Identifier('bar'),
            new Identifier('Foo')
          ),
        ]),
        langEnvironment
      ),
      true
    );
  });
  it("{ class Foo { bar = 'bar'; baz() { return this.bar; } } const quux = new Foo(); quux.baz(); }", function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new ClassDeclaration(
            new Identifier('Foo'),
            new ClassBody([
              new PropertyDefinition(new Identifier('bar'), new Literal('baz')),
              new MethodDefinition(
                new Identifier('baz'),
                new FunctionExpression(
                  [],
                  new BlockStatement([
                    new ReturnStatement(
                      new MemberExpression(
                        new ThisExpression(),
                        new Identifier('bar')
                      )
                    ),
                  ])
                )
              ),
            ])
          ),
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('quux'),
                new NewExpression(new Identifier('Foo'))
              ),
            ],
            'const'
          ),
          new CallExpression(
            new MemberExpression(new Identifier('quux'), new Identifier('baz')),
            []
          ),
        ]),
        langEnvironment
      ),
      'baz'
    );
  });
  return it("{ class Foo { bar; constructor(bar) { this.bar = bar; } } const quux = new Foo('bar'); quux.bar; }", function (): any {
    return assertEqual(
      evalEstree(
        new BlockStatement([
          new ClassDeclaration(
            new Identifier('Foo'),
            new ClassBody([
              new PropertyDefinition(new Identifier('bar')),
              new MethodDefinition(
                new Identifier('constructor'),
                new FunctionExpression(
                  [new Identifier('bar')],
                  new BlockStatement([
                    new ExpressionStatement(
                      new AssignmentExpression(
                        '=',
                        new MemberExpression(
                          new ThisExpression(),
                          new Identifier('bar')
                        ),
                        new Identifier('bar')
                      )
                    ),
                  ])
                )
              ),
            ])
          ),
          new VariableDeclaration(
            [
              new VariableDeclarator(
                new Identifier('quux'),
                new NewExpression(new Identifier('Foo'), [new Literal('bar')])
              ),
            ],
            'const'
          ),
          new MemberExpression(new Identifier('quux'), new Identifier('bar')),
        ]),
        langEnvironment
      ),
      'bar'
    );
  });
});
