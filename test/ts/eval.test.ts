/**
 * # Evaluation tests
 *
 * ## To do
 *
 * ...
 */

import * as chai from 'chai';

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

import { assertEqual } from './test-util';

describe('eval_', function (): any {
  it('true', function (): any {
    assertEqual(eval_(true, lisp1Environment), true);
    assertEqual(eval_(sexp`true`, lisp1Environment), true);
    assertEqual(eval_(sexp`t`, lisp1Environment), true);
    return assertEqual(eval_(sexp`#t`, lisp1Environment), true);
  });
  return it('false', function (): any {
    assertEqual(eval_(false, lisp1Environment), false);
    assertEqual(eval_(sexp`false`, lisp1Environment), false);
    return assertEqual(eval_(sexp`#f`, lisp1Environment), false);
  });
});

describe('eval-rose', function (): any {
  it('true', function (): any {
    return assertEqual(evalRose(wrapSexpInRose(true), lisp1Environment), true);
  });
  return it('false', function (): any {
    return assertEqual(
      evalRose(wrapSexpInRose(false), lisp1Environment),
      false
    );
  });
});

describe('eval-estree', function (): any {
  describe('boolean values', function (): any {
    it('#t', function (): any {
      return assertEqual(evalEstree(new Literal(true), lisp1Environment), true);
    });
    return it('#f', function (): any {
      return assertEqual(
        evalEstree(new Literal(false), lisp1Environment),
        false
      );
    });
  });
  describe('numbers', function (): any {
    it('0', function (): any {
      return assertEqual(evalEstree(new Literal(0), lisp1Environment), 0);
    });
    it('1', function (): any {
      return assertEqual(evalEstree(new Literal(1), lisp1Environment), 1);
    });
    return it('2', function (): any {
      return assertEqual(evalEstree(new Literal(2), lisp1Environment), 2);
    });
  });
  describe('variables', function (): any {
    return it('foo', function (): any {
      return assertEqual(
        evalEstree(new Identifier('foo'), lisp1Environment),
        undefined
      );
    });
  });
  describe('object expressions', function (): any {
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
    return it("{ const foo = { bar: 'baz' }; {...foo}; }", function (): any {
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
  });
  describe('unary expressions', function (): any {
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
    return it('{ let x = 0; x--; }', function (): any {
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
  });
  describe('binary expressions', function (): any {
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
    return it('4 / 2', function (): any {
      return assertEqual(
        evalEstree(
          new BinaryExpression('/', new Literal(4), new Literal(2)),
          lisp1Environment
        ),
        2
      );
    });
  });
  describe('logical expressions', function (): any {
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
    return it('true || true', function (): any {
      return assertEqual(
        evalEstree(
          new LogicalExpression('||', new Literal(true), new Literal(true)),
          lisp1Environment
        ),
        true
      );
    });
  });
  describe('assignment', function (): any {
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
    return it("{ const x = {}; x.foo = 'bar'; x.foo; }", function (): any {
      return assertEqual(
        evalEstree(
          new BlockStatement([
            new VariableDeclarator(new Identifier('x'), new ObjectExpression()),
            new ExpressionStatement(
              new AssignmentExpression(
                '=',
                new MemberExpression(
                  new Identifier('x'),
                  new Identifier('foo')
                ),
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
  });
  describe('blocks', function (): any {
    return it('{ 1; }', function (): any {
      return assertEqual(
        evalEstree(new BlockStatement([new Literal(1)]), lisp1Environment),
        1
      );
    });
  });
  describe('if statements', function (): any {
    return it('{ let x = true; if (x) { x = false; } x; }', function (): any {
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
  });
  describe('conditional expressions', function (): any {
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
    return it('false ? 1 : 2', function (): any {
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
  });
  describe('while loops', function (): any {
    // TODO: Test `continue`.
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
    return it('{ let x = true; while (x) { break; } x; }', function (): any {
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
  });
  describe('for loops', function (): any {
    // TODO: Test `continue`.
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
              new AssignmentExpression(
                '=',
                new Identifier('i'),
                new Literal(0)
              ),
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
              new AssignmentExpression(
                '=',
                new Identifier('i'),
                new Literal(0)
              ),
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
    return it('{ let x = 0; let i = 0; for (i = 0; i < 10; i = i + 1) { if (i === 9) { continue; } x = i; } x; }', function (): any {
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
              new AssignmentExpression(
                '=',
                new Identifier('i'),
                new Literal(0)
              ),
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
  });
  describe('for...of loops', function (): any {
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
    return it('{ let lst = []; for (let x of [1, 2, 3]) { if (x === 2) { continue; } lst.push(x); } lst; }', function (): any {
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
  });
  describe('function calls', function (): any {
    return it('I(1)', function (): any {
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
  });
  describe('function expressions', function (): any {
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
    return it('(<arrow-fn> 1)', function (): any {
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
  });
  describe('function definitions', function (): any {
    return it('{ function I(x) { ... } I(1); }', function (): any {
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
  });
  describe('try...catch', function (): any {
    return it('{ try { throw new Error(); } catch (err) { } 1; }', function (): any {
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
  });
  describe('class expressions', function (): any {
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
    return it("{ const Foo = class { bar() { return 'baz'; } }; const quux = new Foo(); quux.bar(); }", function (): any {
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
              new MemberExpression(
                new Identifier('quux'),
                new Identifier('bar')
              ),
              []
            ),
          ]),
          langEnvironment
        ),
        'baz'
      );
    });
  });
  return describe('class declarations', function (): any {
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
                new PropertyDefinition(
                  new Identifier('bar'),
                  new Literal('baz')
                ),
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
              new MemberExpression(
                new Identifier('quux'),
                new Identifier('baz')
              ),
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
});
