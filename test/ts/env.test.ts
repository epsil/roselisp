import {
  Environment,
  EnvironmentComposition,
  EnvironmentPipe,
  EnvironmentStack,
  JavaScriptEnvironment,
  LispEnvironment,
  ThunkedEnvironment,
  TypedEnvironment,
  extendEnvironment,
} from '../../src/ts/env';

import { thunk } from '../../src/ts/thunk';

import { assertEqual, testMacro } from './test-util';

describe('Environment', function (): any {
  it('find-frame', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.findFrame(Symbol.for('foo')), env);
  });
  it('find-frame, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.findFrame(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('find-frame, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.findFrame(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('find-frame, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        function filter(x: any): any {
          return false;
        }
        return env.findFrame(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('find-frame, filter option, parent stack', function (): any {
    return assertEqual(
      ((): any => {
        const env1: any = new LispEnvironment([
          [Symbol.for('foo'), 'baz', Symbol.for('Any')],
        ]);
        const env2: any = new LispEnvironment([
          [Symbol.for('bar'), 'baz', Symbol.for('Any')],
        ]);
        const env: any = new Environment([], new EnvironmentStack(env1, env2));
        function filter(x: any): any {
          return x !== env2;
        }
        return env.findFrame(Symbol.for('bar'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('find-frame, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment(
          [[Symbol.for('foo'), 'bar']],
          new Environment([[Symbol.for('foo'), 'baz']])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.findFrame(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.get(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.get(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.get(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        function filter(x: any): any {
          return false;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment(
          [[Symbol.for('foo'), 'bar']],
          new Environment([[Symbol.for('foo'), 'baz']])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-value', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.getValue(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get-local', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.getLocal(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get-local, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.getLocal(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get-local, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.getLocal(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get-local, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        function filter(x: any): any {
          return false;
        }
        return env.getLocal(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-tuple', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.getTuple(Symbol.for('foo'));
      })(),
      ['bar', true]
    );
  });
  it('get-tuple, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.getTuple(Symbol.for('quux'));
      })(),
      [undefined, false]
    );
  });
  it('get-tuple, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.getTuple(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      [false, false]
    );
  });
  it('get-tuple, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        function filter(x: any): any {
          return false;
        }
        return env.getTuple(Symbol.for('quux'), {
          filter: filter,
        });
      })(),
      [undefined, false]
    );
  });
  it('get-tuple, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment(
          [[Symbol.for('foo'), 'bar']],
          new Environment([[Symbol.for('foo'), 'baz']])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.getTuple(Symbol.for('quux'), {
          filter: filter,
        });
      })(),
      [undefined, false]
    );
  });
  it('get-local-tuple', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.getLocalTuple(Symbol.for('foo'));
      })(),
      ['bar', true]
    );
  });
  it('get-local-tuple, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.getLocalTuple(Symbol.for('quux'));
      })(),
      [undefined, false]
    );
  });
  it('get-local-tuple, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.getLocalTuple(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      [false, false]
    );
  });
  it('get-local-tuple, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        function filter(x: any): any {
          return false;
        }
        return env.getLocalTuple(Symbol.for('quux'), {
          filter: filter,
        });
      })(),
      [undefined, false]
    );
  });
  it('has', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.has(Symbol.for('foo'));
      })(),
      true
    );
  });
  it('has, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.has(Symbol.for('quux'));
      })(),
      false
    );
  });
  it('has, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        function filter(x: any): any {
          return false;
        }
        return env.has(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      false
    );
  });
  it('has, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment(
          [[Symbol.for('foo'), 'bar']],
          new Environment([[Symbol.for('foo'), 'baz']])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.has(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      false
    );
  });
  it('has-local', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.hasLocal(Symbol.for('foo'));
      })(),
      true
    );
  });
  it('has-local, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        return env.hasLocal(Symbol.for('quux'));
      })(),
      false
    );
  });
  it('has-local, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
        function filter(x: any): any {
          return false;
        }
        return env.hasLocal(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      false
    );
  });
  it('set', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment();
        env.set(Symbol.for('foo'), 'bar');
        return env.get(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('set-entry', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment();
        env.setEntry([Symbol.for('foo'), 'bar']);
        return env.get(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('set-local', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new Environment();
        env.setLocal(Symbol.for('foo'), 'bar');
        return env.getLocal(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  return it('set, mutate existing value in parent environment', function (): any {
    const parent: any = new Environment([[Symbol.for('foo'), 'bar']]);
    const env: any = extendEnvironment(new Environment(), parent);
    env.set(Symbol.for('foo'), 'quux');
    assertEqual(parent.get(Symbol.for('foo')), 'quux');
    assertEqual(env.getLocal(Symbol.for('foo')), undefined);
    return assertEqual(env.get(Symbol.for('foo')), 'quux');
  });
});

describe('TypedEnvironment', function (): any {
  it('get', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.get(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.get(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.get(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment(
          [[Symbol.for('foo'), 'bar', Symbol.for('Any')]],
          new TypedEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-value', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getValue(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get-value, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getValue(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get-value, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getValue(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get-value, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.getValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-value, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment(
          [[Symbol.for('foo'), 'bar', Symbol.for('Any')]],
          new TypedEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.getValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-typed-value', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getTypedValue(Symbol.for('foo'));
      })(),
      ['bar', Symbol.for('Any')]
    );
  });
  it('get-typed-value, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getTypedValue(Symbol.for('quux'));
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
  it('get-typed-value, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getTypedValue(Symbol.for('quux'), {
          notFound: [false, Symbol.for('Undefined')],
        });
      })(),
      [false, Symbol.for('Undefined')]
    );
  });
  it('get-typed-value, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.getTypedValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
  it('get-typed-value, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment(
          [[Symbol.for('foo'), 'bar', Symbol.for('Any')]],
          new TypedEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.getTypedValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
  it('get-local', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getLocal(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get-local, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getLocal(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get-local, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getLocal(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get-local, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.getLocal(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-type', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getType(Symbol.for('foo'));
      })(),
      Symbol.for('Any')
    );
  });
  it('get-type, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getType(Symbol.for('quux'));
      })(),
      Symbol.for('Undefined')
    );
  });
  it('get-type, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getType(Symbol.for('quux'), {
          notFound: Symbol.for('Any'),
        });
      })(),
      Symbol.for('Any')
    );
  });
  it('get-type, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.getType(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      Symbol.for('Undefined')
    );
  });
  it('get-type, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment(
          [[Symbol.for('foo'), 'bar', Symbol.for('Any')]],
          new TypedEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.getType(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      Symbol.for('Undefined')
    );
  });
  it('has', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.has(Symbol.for('foo'));
      })(),
      true
    );
  });
  it('has, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.has(Symbol.for('quux'));
      })(),
      false
    );
  });
  it('has, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.has(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      false
    );
  });
  it('has, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment(
          [[Symbol.for('foo'), 'bar', Symbol.for('Any')]],
          new TypedEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.has(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      false
    );
  });
  it('has-local', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.hasLocal(Symbol.for('foo'));
      })(),
      true
    );
  });
  it('has-local, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.hasLocal(Symbol.for('quux'));
      })(),
      false
    );
  });
  it('has-local, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.hasLocal(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      false
    );
  });
  it('set', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment();
        env.set(Symbol.for('foo'), 'bar', Symbol.for('Any'));
        return env.get(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('set-entry', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment();
        env.setEntry([Symbol.for('foo'), ['bar', Symbol.for('Any')]]);
        return env.get(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  xit('set-local', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new TypedEnvironment();
        env.setLocal(Symbol.for('foo'), 'bar', Symbol.for('Any'));
        return env.getLocal(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  return it('set, mutate existing value in parent environment', function (): any {
    const parent: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', Symbol.for('Any')],
    ]);
    const env: any = extendEnvironment(new TypedEnvironment(), parent);
    env.set(Symbol.for('foo'), 'quux', Symbol.for('Any'));
    assertEqual(parent.get(Symbol.for('foo')), 'quux');
    assertEqual(env.getLocal(Symbol.for('foo')), undefined);
    return assertEqual(env.get(Symbol.for('foo')), 'quux');
  });
});

describe('LispEnvironment', function (): any {
  it('get', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.get(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.get(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.get(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment(
          [[Symbol.for('foo'), 'bar', Symbol.for('Any')]],
          new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-value', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getValue(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get-value, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getValue(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get-value, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getValue(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get-value, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.getValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-value, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment(
          [[Symbol.for('foo'), 'bar', Symbol.for('Any')]],
          new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.getValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-typed-value', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getTypedValue(Symbol.for('foo'));
      })(),
      ['bar', Symbol.for('Any')]
    );
  });
  it('get-typed-value, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getTypedValue(Symbol.for('quux'));
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
  it('get-typed-value, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getTypedValue(Symbol.for('quux'), {
          notFound: [false, Symbol.for('Undefined')],
        });
      })(),
      [false, Symbol.for('Undefined')]
    );
  });
  it('get-typed-value, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.getTypedValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
  it('get-typed-value, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment(
          [[Symbol.for('foo'), 'bar', Symbol.for('Any')]],
          new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.getTypedValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
  it('get-local', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getLocal(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get-local, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getLocal(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get-local, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getLocal(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get-local, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.getLocal(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-type', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getType(Symbol.for('foo'));
      })(),
      Symbol.for('Any')
    );
  });
  it('get-type, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getType(Symbol.for('quux'));
      })(),
      Symbol.for('Undefined')
    );
  });
  it('get-type, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.getType(Symbol.for('quux'), {
          notFound: Symbol.for('Any'),
        });
      })(),
      Symbol.for('Any')
    );
  });
  it('get-type, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.getType(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      Symbol.for('Undefined')
    );
  });
  it('get-type, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment(
          [[Symbol.for('foo'), 'bar', Symbol.for('Any')]],
          new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.getType(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      Symbol.for('Undefined')
    );
  });
  it('has', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.has(Symbol.for('foo'));
      })(),
      true
    );
  });
  it('has, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.has(Symbol.for('quux'));
      })(),
      false
    );
  });
  it('has, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.has(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      false
    );
  });
  it('has, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment(
          [[Symbol.for('foo'), 'bar', Symbol.for('Any')]],
          new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.has(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      false
    );
  });
  it('has-local', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.hasLocal(Symbol.for('foo'));
      })(),
      true
    );
  });
  it('has-local, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        return env.hasLocal(Symbol.for('quux'));
      })(),
      false
    );
  });
  it('has-local, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.hasLocal(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      false
    );
  });
  it('set', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment();
        env.set(Symbol.for('foo'), 'bar', Symbol.for('Any'));
        return env.get(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('set-entry', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment();
        env.setEntry([Symbol.for('foo'), ['bar', Symbol.for('Any')]]);
        return env.get(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  xit('set-local', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new LispEnvironment();
        env.setLocal(Symbol.for('foo'), 'bar', Symbol.for('Any'));
        return env.getLocal(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  return it('set, mutate existing value in parent environment', function (): any {
    const parent: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', Symbol.for('Any')],
    ]);
    const env: any = extendEnvironment(new LispEnvironment(), parent);
    env.set(Symbol.for('foo'), 'quux', Symbol.for('Any'));
    assertEqual(parent.get(Symbol.for('foo')), 'quux');
    assertEqual(env.getLocal(Symbol.for('foo')), undefined);
    return assertEqual(env.get(Symbol.for('foo')), 'quux');
  });
});

describe('EnvironmentStack', function (): any {
  it('get', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        return env.get(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        return env.get(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        return env.get(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return false;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get, multiple environments, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env1: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        const env2: any = new LispEnvironment([
          [Symbol.for('foo'), 'baz', Symbol.for('Any')],
        ]);
        const env: any = new EnvironmentStack(env1, env2);
        function filter(x: any): any {
          return x !== env1;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-value', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        return env.getValue(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get-value, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        return env.getValue(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get-value, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        return env.getValue(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get-value, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return false;
        }
        return env.getValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-value, multiple environments, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env1: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        const env2: any = new LispEnvironment([
          [Symbol.for('foo'), 'baz', Symbol.for('Any')],
        ]);
        const env: any = new EnvironmentStack(env1, env2);
        function filter(x: any): any {
          return x !== env1;
        }
        return env.getValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-typed-value', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        return env.getTypedValue(Symbol.for('foo'));
      })(),
      ['bar', Symbol.for('Any')]
    );
  });
  it('get-typed-value 2', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]),
          new EnvironmentStack(
            new LispEnvironment([
              [Symbol.for('bar'), 'bar', Symbol.for('Any')],
            ]),
            new JavaScriptEnvironment()
          )
        );
        return env.getTypedValue(Symbol.for('foo'));
      })(),
      ['bar', Symbol.for('Any')]
    );
  });
  it('get-typed-value, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        return env.getTypedValue(Symbol.for('quux'));
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
  it('get-typed-value, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        return env.getTypedValue(Symbol.for('quux'), {
          notFound: [false, Symbol.for('Undefined')],
        });
      })(),
      [false, Symbol.for('Undefined')]
    );
  });
  it('get-typed-value, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentStack(
          new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]])
        );
        function filter(x: any): any {
          return false;
        }
        return env.getTypedValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
  it('get-typed-value, multiple environments, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env1: any = new LispEnvironment([
          [Symbol.for('foo'), 'bar', Symbol.for('Any')],
        ]);
        const env2: any = new LispEnvironment([
          [Symbol.for('foo'), 'baz', Symbol.for('Any')],
        ]);
        const env: any = new EnvironmentStack(env1, env2);
        function filter(x: any): any {
          return x !== env1;
        }
        return env.getTypedValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
  it('set, one environment', function (): any {
    const env1: any = new LispEnvironment();
    const env: any = new EnvironmentStack(env1);
    env.set(Symbol.for('foo'), 'bar', Symbol.for('Any'));
    assertEqual(env.get(Symbol.for('foo')), 'bar');
    return assertEqual(env1.get(Symbol.for('foo')), 'bar');
  });
  it('set, two environments, previously defined in second', function (): any {
    const env1: any = new LispEnvironment();
    const env2: any = new LispEnvironment([
      [Symbol.for('foo'), 'foo', Symbol.for('Any')],
    ]);
    const env: any = new EnvironmentStack(env1, env2);
    env.set(Symbol.for('foo'), 'bar', Symbol.for('Any'));
    assertEqual(env.get(Symbol.for('foo')), 'bar');
    assertEqual(env1.get(Symbol.for('foo')), undefined);
    return assertEqual(env2.get(Symbol.for('foo')), 'bar');
  });
  return it('set-entry, two environments, previously defined in second', function (): any {
    const env1: any = new LispEnvironment();
    const env2: any = new LispEnvironment([
      [Symbol.for('foo'), 'foo', Symbol.for('Any')],
    ]);
    const env: any = new EnvironmentStack(env1, env2);
    env.setEntry([Symbol.for('foo'), ['bar', Symbol.for('Any')]]);
    assertEqual(env.get(Symbol.for('foo')), 'bar');
    assertEqual(env1.get(Symbol.for('foo')), 'bar');
    return assertEqual(env2.get(Symbol.for('foo')), 'foo');
  });
});

describe('EnvironmentPipe', function (): any {
  it('get', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentPipe(
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ])
        );
        return env.get(Symbol.for('foo'));
      })(),
      Symbol.for('baz')
    );
  });
  it('get, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentPipe(
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ])
        );
        return env.get(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentPipe(
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ])
        );
        return env.get(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env1: any = new LispEnvironment([
          [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
        ]);
        const env2: any = new LispEnvironment([
          [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
        ]);
        const env: any = new EnvironmentPipe(env1, env2);
        function filter(x: any): any {
          return x !== env1;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-value', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentPipe(
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ])
        );
        return env.getValue(Symbol.for('foo'));
      })(),
      Symbol.for('baz')
    );
  });
  it('get-value, notexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentPipe(
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ])
        );
        return env.getValue(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get-value, notexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentPipe(
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ])
        );
        return env.getValue(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get-value, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentPipe(
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ])
        );
        function filter(x: any): any {
          return false;
        }
        return env.getValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-typed-value', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentPipe(
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ])
        );
        return env.getTypedValue(Symbol.for('foo'));
      })(),
      [Symbol.for('baz'), Symbol.for('Any')]
    );
  });
  it('get-typed-value, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentPipe(
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ])
        );
        return env.getTypedValue(Symbol.for('quux'));
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
  it('get-typed-value, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentPipe(
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ])
        );
        return env.getTypedValue(Symbol.for('quux'), {
          notFound: [false, Symbol.for('Undefined')],
        });
      })(),
      [false, Symbol.for('Undefined')]
    );
  });
  return it('get-typed-value, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env1: any = new LispEnvironment([
          [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
        ]);
        const env2: any = new LispEnvironment([
          [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
        ]);
        const env: any = new EnvironmentPipe(env1, env2);
        function filter(x: any): any {
          return x !== env1;
        }
        return env.getTypedValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
});

describe('EnvironmentComposition', function (): any {
  it('get', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentComposition(
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ])
        );
        return env.get(Symbol.for('foo'));
      })(),
      Symbol.for('baz')
    );
  });
  it('get, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentComposition(
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ])
        );
        return env.get(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentComposition(
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ])
        );
        return env.get(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env1: any = new LispEnvironment([
          [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
        ]);
        const env2: any = new LispEnvironment([
          [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
        ]);
        const env: any = new EnvironmentComposition(env2, env1);
        function filter(x: any): any {
          return x !== env1;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-value', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentComposition(
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ])
        );
        return env.getValue(Symbol.for('foo'));
      })(),
      Symbol.for('baz')
    );
  });
  it('get-value, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentComposition(
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ])
        );
        return env.getValue(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get-value, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentComposition(
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ])
        );
        return env.getValue(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get-value, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env1: any = new LispEnvironment([
          [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
        ]);
        const env2: any = new LispEnvironment([
          [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
        ]);
        const env: any = new EnvironmentComposition(env2, env1);
        function filter(x: any): any {
          return x !== env1;
        }
        return env.getValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-typed-value', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentComposition(
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ])
        );
        return env.getTypedValue(Symbol.for('foo'));
      })(),
      [Symbol.for('baz'), Symbol.for('Any')]
    );
  });
  it('get-typed-value, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentComposition(
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ])
        );
        return env.getTypedValue(Symbol.for('quux'));
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
  it('get-typed-value, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new EnvironmentComposition(
          new LispEnvironment([
            [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
          ]),
          new LispEnvironment([
            [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
          ])
        );
        return env.getTypedValue(Symbol.for('quux'), {
          notFound: [false, Symbol.for('Undefined')],
        });
      })(),
      [false, Symbol.for('Undefined')]
    );
  });
  return it('get-typed-value, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env1: any = new LispEnvironment([
          [Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')],
        ]);
        const env2: any = new LispEnvironment([
          [Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')],
        ]);
        const env: any = new EnvironmentComposition(env2, env1);
        function filter(x: any): any {
          return x !== env1;
        }
        return env.getTypedValue(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      [undefined, Symbol.for('Undefined')]
    );
  });
});

describe('ThunkedEnvironment', function (): any {
  it('get', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new ThunkedEnvironment([
          [
            Symbol.for('foo'),
            thunk(function (): any {
              return 'bar';
            }),
            Symbol.for('Any'),
          ],
        ]);
        return env.get(Symbol.for('foo'));
      })(),
      'bar'
    );
  });
  it('get, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new ThunkedEnvironment([
          [
            Symbol.for('foo'),
            thunk(function (): any {
              return 'bar';
            }),
            Symbol.for('Any'),
          ],
        ]);
        return env.get(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new ThunkedEnvironment([
          [
            Symbol.for('foo'),
            thunk(function (): any {
              return 'bar';
            }),
            Symbol.for('Any'),
          ],
        ]);
        return env.get(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new ThunkedEnvironment([
          [
            Symbol.for('foo'),
            thunk(function (): any {
              return 'bar';
            }),
            Symbol.for('Any'),
          ],
        ]);
        function filter(x: any): any {
          return false;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get, parent environment, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new ThunkedEnvironment(
          [
            [
              Symbol.for('foo'),
              thunk(function (): any {
                return 'bar';
              }),
              Symbol.for('Any'),
            ],
          ],
          new ThunkedEnvironment([
            [
              Symbol.for('foo'),
              thunk(function (): any {
                return 'baz';
              }),
              Symbol.for('Any'),
            ],
          ])
        );
        function filter(x: any): any {
          return x !== env;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('has-thunk, true', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new ThunkedEnvironment([
          [
            Symbol.for('foo'),
            thunk(function (): any {
              return 'foo';
            }),
            Symbol.for('Any'),
          ],
        ]);
        return env.hasThunk(Symbol.for('foo'));
      })(),
      true
    );
  });
  it('has-thunk, parent environment, true', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new ThunkedEnvironment(
          [
            [
              Symbol.for('foo'),
              thunk(function (): any {
                return 'foo';
              }),
              Symbol.for('Any'),
            ],
          ],
          new ThunkedEnvironment([
            [
              Symbol.for('bar'),
              thunk(function (): any {
                return 'bar';
              }),
              Symbol.for('Any'),
            ],
          ])
        );
        return env.hasThunk(Symbol.for('bar'));
      })(),
      true
    );
  });
  it('has-thunk, false', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new ThunkedEnvironment([
          [
            Symbol.for('foo'),
            thunk(function (): any {
              return 'foo';
            }),
            Symbol.for('Any'),
          ],
          [Symbol.for('bar'), 'bar', Symbol.for('Any')],
        ]);
        return env.hasThunk(Symbol.for('bar'));
      })(),
      false
    );
  });
  it('has-local-thunk, true', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new ThunkedEnvironment(
          [
            [
              Symbol.for('foo'),
              thunk(function (): any {
                return 'foo';
              }),
              Symbol.for('Any'),
            ],
          ],
          new ThunkedEnvironment([
            [
              Symbol.for('bar'),
              thunk(function (): any {
                return 'bar';
              }),
              Symbol.for('Any'),
            ],
          ])
        );
        return env.hasLocalThunk(Symbol.for('foo'));
      })(),
      true
    );
  });
  return it('has-local-thunk, false', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new ThunkedEnvironment(
          [
            [
              Symbol.for('foo'),
              thunk(function (): any {
                return 'foo';
              }),
              Symbol.for('Any'),
            ],
          ],
          new ThunkedEnvironment([
            [
              Symbol.for('bar'),
              thunk(function (): any {
                return 'bar';
              }),
              Symbol.for('Any'),
            ],
          ])
        );
        return env.hasLocalThunk(Symbol.for('bar'));
      })(),
      false
    );
  });
});

describe('JavaScriptEnvironment', function (): any {
  it('get', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        return env.get(Symbol.for('Map'));
      })(),
      Map
    );
  });
  it('get, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        return env.get(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        return env.get(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        function filter(x: any): any {
          return false;
        }
        return env.get(Symbol.for('foo'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('get-local', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        return env.getLocal(Symbol.for('Map'));
      })(),
      Map
    );
  });
  it('get-local, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        return env.getLocal(Symbol.for('quux'));
      })(),
      undefined
    );
  });
  it('get-local, nonexistant binding, notFound option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        return env.getLocal(Symbol.for('quux'), {
          notFound: false,
        });
      })(),
      false
    );
  });
  it('get-local, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        function filter(x: any): any {
          return false;
        }
        return env.getLocal(Symbol.for('Map'), {
          filter: filter,
        });
      })(),
      undefined
    );
  });
  it('has', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        return env.has(Symbol.for('Map'));
      })(),
      true
    );
  });
  it('has, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        return env.has(Symbol.for('quux'));
      })(),
      false
    );
  });
  it('has, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        function filter(x: any): any {
          return false;
        }
        return env.has(Symbol.for('Map'), {
          filter: filter,
        });
      })(),
      false
    );
  });
  it('has-local', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        return env.hasLocal(Symbol.for('Map'));
      })(),
      true
    );
  });
  it('has-local, nonexistant binding', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        return env.hasLocal(Symbol.for('quux'));
      })(),
      false
    );
  });
  return it('has-local, filter option', function (): any {
    return assertEqual(
      ((): any => {
        const env: any = new JavaScriptEnvironment();
        function filter(x: any): any {
          return false;
        }
        return env.hasLocal(Symbol.for('Map'), {
          filter: filter,
        });
      })(),
      false
    );
  });
});
