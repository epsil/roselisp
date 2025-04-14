import * as chai from 'chai';

import {
  case_,
  caseEq_,
  threadAs_,
  threadFirst_,
  threadLast_,
} from '../../src/ts/macros';

import {
  LispEnvironment,
  macroexpand,
  macroexpand1,
  macroexpandAll,
  makeLisp,
} from '../../src/ts';

import { assertEqual, testRepl } from './test-util';

const [lastCdr]: any[] = ((): any => {
  function lastCdr_(lst: any): any {
    if (!Array.isArray(lst)) {
      return undefined;
    } else if (
      Array.isArray(lst) &&
      lst.length >= 3 &&
      lst[lst.length - 2] === Symbol.for('.')
    ) {
      let result: any = lst;
      while (
        Array.isArray(result) &&
        result.length >= 3 &&
        result[result.length - 2] === Symbol.for('.')
      ) {
        result = result[result.length - 1];
      }
      return result;
    } else {
      return [];
    }
  }
  return [lastCdr_];
})();

describe('macroexpand', function (): any {
  it('(foo bar)', function (): any {
    return assertEqual(
      macroexpand(
        [Symbol.for('foo'), Symbol.for('bar')],
        new LispEnvironment([
          [
            Symbol.for('foo'),
            function (exp: any, env: any): any {
              return [Symbol.for('baz')];
            },
            'macro',
          ],
        ])
      ),
      [[Symbol.for('baz')], true]
    );
  });
  it('(+ 1 1)', function (): any {
    return assertEqual(
      macroexpand([Symbol.for('+'), 1, 1], new LispEnvironment()),
      [[Symbol.for('+'), 1, 1], false]
    );
  });
  it('(~> "a b c d" ...)', function (): any {
    return assertEqual(
      macroexpand1(
        [
          Symbol.for('~>'),
          'a b c d',
          Symbol.for('.toUpperCase'),
          [Symbol.for('.replace'), 'A', 'X'],
          [Symbol.for('.split'), ' '],
          Symbol.for('first'),
        ],
        makeLisp()
      ),
      [
        [
          Symbol.for('as~>'),
          'a b c d',
          Symbol.for('_'),
          [Symbol.for('.toUpperCase'), Symbol.for('_')],
          [Symbol.for('.replace'), Symbol.for('_'), 'A', 'X'],
          [Symbol.for('.split'), Symbol.for('_'), ' '],
          [Symbol.for('first'), Symbol.for('_')],
        ],
        true,
      ]
    );
  });
  it('(~>> foo)', function (): any {
    return assertEqual(
      macroexpand1([Symbol.for('~>>'), Symbol.for('foo')], makeLisp()),
      [[Symbol.for('as~>'), Symbol.for('foo'), Symbol.for('_')], true]
    );
  });
  it('(~>> foo (bar))', function (): any {
    return assertEqual(
      macroexpand1(
        [Symbol.for('~>>'), Symbol.for('foo'), [Symbol.for('bar')]],
        makeLisp()
      ),
      [
        [
          Symbol.for('as~>'),
          Symbol.for('foo'),
          Symbol.for('_'),
          [Symbol.for('bar'), Symbol.for('_')],
        ],
        true,
      ]
    );
  });
  return it('(~>> (range) ...)', function (): any {
    return assertEqual(
      macroexpand1(
        [
          Symbol.for('~>>'),
          [Symbol.for('range')],
          [
            Symbol.for('map'),
            [
              Symbol.for('fn'),
              [Symbol.for('x')],
              [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')],
            ],
          ],
          [Symbol.for('filter'), Symbol.for('even?')],
          [Symbol.for('take'), 10],
          [Symbol.for('reduce'), Symbol.for('+')],
        ],
        makeLisp()
      ),
      [
        [
          Symbol.for('as~>'),
          [Symbol.for('range')],
          Symbol.for('_'),
          [
            Symbol.for('map'),
            [
              Symbol.for('fn'),
              [Symbol.for('x')],
              [Symbol.for('*'), Symbol.for('x'), Symbol.for('x')],
            ],
            Symbol.for('_'),
          ],
          [Symbol.for('filter'), Symbol.for('even?'), Symbol.for('_')],
          [Symbol.for('take'), 10, Symbol.for('_')],
          [Symbol.for('reduce'), Symbol.for('+'), Symbol.for('_')],
        ],
        true,
      ]
    );
  });
});

describe('macroexpand-all', function (): any {
  it('(~> ...)', function (): any {
    return assertEqual(
      macroexpandAll(
        [
          Symbol.for('~>'),
          'a b c d',
          Symbol.for('.toUpperCase'),
          [Symbol.for('.replace'), 'A', 'X'],
          [Symbol.for('.split'), ' '],
          Symbol.for('first'),
        ],
        makeLisp()
      ),
      [
        Symbol.for('first'),
        [
          Symbol.for('.split'),
          [
            Symbol.for('.replace'),
            [Symbol.for('.toUpperCase'), 'a b c d'],
            'A',
            'X',
          ],
          ' ',
        ],
      ]
    );
  });
  xit('(begin (~> ...))', function (): any {
    return assertEqual(
      macroexpandAll(
        [
          Symbol.for('begin'),
          [
            Symbol.for('~>'),
            'a b c d',
            Symbol.for('.toUpperCase'),
            [Symbol.for('.replace'), 'A', 'X'],
            [Symbol.for('.split'), ' '],
            Symbol.for('first'),
          ],
        ],
        makeLisp()
      ),
      [
        Symbol.for('begin'),
        [
          Symbol.for('first'),
          [
            Symbol.for('.split'),
            [
              Symbol.for('.replace'),
              [Symbol.for('.toUpperCase'), 'a b c d'],
              'A',
              'X',
            ],
            ' ',
          ],
        ],
      ]
    );
  });
  return xit('(begin (~> ... (~> ...) ...))', function (): any {
    return assertEqual(
      macroexpandAll(
        [
          Symbol.for('begin'),
          [
            Symbol.for('~>'),
            'a b c d',
            Symbol.for('.toUpperCase'),
            [
              Symbol.for('.replace'),
              'A',
              [Symbol.for('~>'), 'x', [Symbol.for('.toUpperCase')]],
            ],
            [Symbol.for('.split'), ' '],
            Symbol.for('first'),
          ],
        ],
        makeLisp()
      ),
      [
        Symbol.for('begin'),
        [
          Symbol.for('first'),
          [
            Symbol.for('.split'),
            [
              Symbol.for('.replace'),
              [Symbol.for('.toUpperCase'), 'a b c d'],
              'A',
              [Symbol.for('.toUpperCase'), 'x'],
            ],
            ' ',
          ],
        ],
      ]
    );
  });
});

describe('as~>', function (): any {
  it('(as~> x _)', function (): any {
    return assertEqual(
      threadAs_([Symbol.for('as~>'), Symbol.for('x'), Symbol.for('_')]),
      Symbol.for('x')
    );
  });
  it('(as~> x _ (foo))', function (): any {
    return assertEqual(
      threadAs_([
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('foo')],
      ]),
      [Symbol.for('begin'), Symbol.for('x'), [Symbol.for('foo')]]
    );
  });
  it('(as~> x _ (foo) (bar))', function (): any {
    return assertEqual(
      threadAs_([
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('foo')],
        [Symbol.for('bar')],
      ]),
      [
        Symbol.for('begin'),
        Symbol.for('x'),
        [Symbol.for('foo')],
        [Symbol.for('bar')],
      ]
    );
  });
  it('(as~> x _ (+ _ 1))', function (): any {
    return assertEqual(
      threadAs_([
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('+'), Symbol.for('_'), 1],
      ]),
      [Symbol.for('+'), Symbol.for('x'), 1]
    );
  });
  it('(as~> x _ (+ _ _))', function (): any {
    return assertEqual(
      threadAs_([
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('+'), Symbol.for('_'), Symbol.for('_')],
      ]),
      [
        Symbol.for('let'),
        [[Symbol.for('_'), Symbol.for('x')]],
        [
          Symbol.for('set!'),
          Symbol.for('_'),
          [Symbol.for('+'), Symbol.for('_'), Symbol.for('_')],
        ],
        Symbol.for('_'),
      ]
    );
  });
  it('(as~> x _ (+ _ _))', function (): any {
    return assertEqual(
      threadAs_([
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('+'), Symbol.for('_'), 1],
        [Symbol.for('+'), Symbol.for('_'), 1],
      ]),
      [Symbol.for('+'), [Symbol.for('+'), Symbol.for('x'), 1], 1]
    );
  });
  return it('(as~> x _ (+ _ _))', function (): any {
    return assertEqual(
      threadAs_([
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('+'), Symbol.for('_'), 1],
        [Symbol.for('+'), Symbol.for('_'), Symbol.for('_')],
      ]),
      [
        Symbol.for('let'),
        [[Symbol.for('_'), [Symbol.for('+'), Symbol.for('x'), 1]]],
        [
          Symbol.for('set!'),
          Symbol.for('_'),
          [Symbol.for('+'), Symbol.for('_'), Symbol.for('_')],
        ],
        Symbol.for('_'),
      ]
    );
  });
});

describe('~>', function (): any {
  it('(~> x foo)', function (): any {
    return assertEqual(
      threadFirst_([Symbol.for('~>'), Symbol.for('x'), Symbol.for('foo')]),
      [
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('foo'), Symbol.for('_')],
      ]
    );
  });
  it('(~> x (foo))', function (): any {
    return assertEqual(
      threadFirst_([Symbol.for('~>'), Symbol.for('x'), [Symbol.for('foo')]]),
      [
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('foo'), Symbol.for('_')],
      ]
    );
  });
  it('(~> x (foo _))', function (): any {
    return assertEqual(
      threadFirst_([
        Symbol.for('~>'),
        Symbol.for('x'),
        [Symbol.for('foo'), Symbol.for('_')],
      ]),
      [
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('foo'), Symbol.for('_')],
      ]
    );
  });
  return it('(~> x :hole-marker * (foo *))', function (): any {
    return assertEqual(
      threadFirst_([
        Symbol.for('~>'),
        Symbol.for('x'),
        Symbol.for(':hole-marker'),
        Symbol.for('*'),
        [Symbol.for('foo'), Symbol.for('*')],
      ]),
      [
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('*'),
        [Symbol.for('foo'), Symbol.for('*')],
      ]
    );
  });
});

describe('~>>', function (): any {
  it('(~>> x foo)', function (): any {
    return assertEqual(
      threadLast_([Symbol.for('~>>'), Symbol.for('x'), Symbol.for('foo')]),
      [
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('foo'), Symbol.for('_')],
      ]
    );
  });
  it('(~>> x (foo))', function (): any {
    return assertEqual(
      threadLast_([Symbol.for('~>>'), Symbol.for('x'), [Symbol.for('foo')]]),
      [
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('foo'), Symbol.for('_')],
      ]
    );
  });
  it('(~>> x (foo _))', function (): any {
    return assertEqual(
      threadLast_([
        Symbol.for('~>>'),
        Symbol.for('x'),
        [Symbol.for('foo'), Symbol.for('_')],
      ]),
      [
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('_'),
        [Symbol.for('foo'), Symbol.for('_')],
      ]
    );
  });
  return it('(~>> x :hole-marker * (foo *))', function (): any {
    return assertEqual(
      threadLast_([
        Symbol.for('~>>'),
        Symbol.for('x'),
        Symbol.for(':hole-marker'),
        Symbol.for('*'),
        [Symbol.for('foo'), Symbol.for('*')],
      ]),
      [
        Symbol.for('as~>'),
        Symbol.for('x'),
        Symbol.for('*'),
        [Symbol.for('foo'), Symbol.for('*')],
      ]
    );
  });
});

describe('case/eq', function (): any {
  it('(case/eq x (("foo") foo) (else bar))', function (): any {
    return assertEqual(
      caseEq_([
        Symbol.for('case/eq'),
        Symbol.for('x'),
        [['foo'], Symbol.for('foo')],
        [Symbol.for('else'), Symbol.for('bar')],
      ]),
      [
        Symbol.for('js/switch'),
        Symbol.for('x'),
        [
          Symbol.for('case'),
          [Symbol.for('quote'), 'foo'],
          Symbol.for('foo'),
          [Symbol.for('break')],
        ],
        [Symbol.for('default'), Symbol.for('bar')],
      ]
    );
  });
  it('(case/eq x (("foo" "bar") foo) (else baz))', function (): any {
    return assertEqual(
      caseEq_([
        Symbol.for('case/eq'),
        Symbol.for('x'),
        [['foo', 'bar'], Symbol.for('foo')],
        [Symbol.for('else'), Symbol.for('baz')],
      ]),
      [
        Symbol.for('cond'),
        [
          [
            Symbol.for('member?'),
            Symbol.for('x'),
            [Symbol.for('quote'), ['foo', 'bar']],
          ],
          Symbol.for('foo'),
        ],
        [Symbol.for('else'), Symbol.for('baz')],
      ]
    );
  });
  return it('(case/eq x (("foo" "bar") foo) (else baz))', function (): any {
    const actual: any = caseEq_([
      Symbol.for('case/eq'),
      [Symbol.for('get-field'), Symbol.for('prop'), Symbol.for('x')],
      [['foo', 'bar'], Symbol.for('foo')],
      [Symbol.for('else'), Symbol.for('baz')],
    ]);
    const resultVar: any = (
      Array.isArray(actual) &&
      actual.length >= 3 &&
      actual[actual.length - 2] === Symbol.for('.') &&
      ((): any => {
        const x: any = lastCdr(actual);
        return Array.isArray(x) && x.length === 0;
      })()
        ? ((): any => {
            let i: any = 1;
            let result: any = actual;
            while (i > 0) {
              if (
                Array.isArray(result) &&
                result.length === 3 &&
                result[1] === Symbol.for('.')
              ) {
                result = actual[actual.length - 1];
              } else {
                result = actual.slice(1);
              }
              i--;
            }
            if (Array.isArray(result)) {
              result = result[0];
            }
            return result;
          })()
        : actual[1]
    )[0][0];
    const expected: any = [
      Symbol.for('let'),
      [
        [
          resultVar,
          [Symbol.for('get-field'), Symbol.for('prop'), Symbol.for('x')],
        ],
      ],
      [
        Symbol.for('cond'),
        [
          [
            Symbol.for('member?'),
            resultVar,
            [Symbol.for('quote'), ['foo', 'bar']],
          ],
          Symbol.for('foo'),
        ],
        [Symbol.for('else'), Symbol.for('baz')],
      ],
    ];
    return assertEqual(actual, expected);
  });
});

describe('case', function (): any {
  it('(case x (("foo") foo) (else bar))', function (): any {
    return assertEqual(
      case_([
        Symbol.for('case'),
        Symbol.for('x'),
        [['foo'], Symbol.for('foo')],
        [Symbol.for('else'), Symbol.for('bar')],
      ]),
      [
        Symbol.for('case/eq'),
        Symbol.for('x'),
        [['foo'], Symbol.for('foo')],
        [Symbol.for('else'), Symbol.for('bar')],
      ]
    );
  });
  it('(case x ((("foo")) foo) (else bar))', function (): any {
    return assertEqual(
      case_([
        Symbol.for('case'),
        Symbol.for('x'),
        [[['foo']], Symbol.for('foo')],
        [Symbol.for('else'), Symbol.for('bar')],
      ]),
      [
        Symbol.for('cond'),
        [
          [
            Symbol.for('member?'),
            Symbol.for('x'),
            [Symbol.for('quote'), [['foo']]],
            Symbol.for('equal?'),
          ],
          Symbol.for('foo'),
        ],
        [Symbol.for('else'), Symbol.for('bar')],
      ]
    );
  });
  return it("> (case 'foo ((foo) 1))", function (): any {
    return testRepl([
      Symbol.for('roselisp'),
      Symbol.for('>'),
      [
        Symbol.for('case'),
        [Symbol.for('quote'), Symbol.for('foo')],
        [[Symbol.for('foo')], 1],
      ],
      1,
    ]);
  });
});
