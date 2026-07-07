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

import { assertEqual } from './test-util';

describe('Environment', function (): any {
  it('find-frame', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.findFrame(Symbol.for('foo')), env);
  });
  it('find-frame, nonexistant binding', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.findFrame(Symbol.for('quux')), undefined);
  });
  it('find-frame, nonexistant binding, notFound option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.findFrame(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('find-frame, filter option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.findFrame(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  it('get, nonexistant binding', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.get(Symbol.for('quux')), undefined);
  });
  it('get, nonexistant binding, notFound option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.get(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get, filter option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.get(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-value', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.getValue(Symbol.for('foo')), 'bar');
  });
  it('get-local', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.getLocal(Symbol.for('foo')), 'bar');
  });
  it('get-local, nonexistant binding', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.getLocal(Symbol.for('quux')), undefined);
  });
  it('get-local, nonexistant binding, notFound option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.getLocal(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get-local, filter option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.getLocal(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-tuple', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.getTuple(Symbol.for('foo')), ['bar', true]);
  });
  it('get-tuple, nonexistant binding', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.getTuple(Symbol.for('quux')), [undefined, false]);
  });
  it('get-tuple, nonexistant binding, notFound option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.getTuple(Symbol.for('quux'), {
        notFound: false,
      }),
      [false, false]
    );
  });
  it('get-tuple, filter option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.getTuple(Symbol.for('quux'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      [undefined, false]
    );
  });
  it('get-local-tuple', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.getLocalTuple(Symbol.for('foo')), ['bar', true]);
  });
  it('get-local-tuple, nonexistant binding', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.getLocalTuple(Symbol.for('quux')), [
      undefined,
      false,
    ]);
  });
  it('get-local-tuple, nonexistant binding, notFound option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.getLocalTuple(Symbol.for('quux'), {
        notFound: false,
      }),
      [false, false]
    );
  });
  it('get-local-tuple, filter option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.getLocalTuple(Symbol.for('quux'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      [undefined, false]
    );
  });
  it('has', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.has(Symbol.for('foo')), true);
  });
  it('has, nonexistant binding', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.has(Symbol.for('quux')), false);
  });
  it('has, filter option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.has(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      false
    );
  });
  it('has-local', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.hasLocal(Symbol.for('foo')), true);
  });
  it('has-local, nonexistant binding', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.hasLocal(Symbol.for('quux')), false);
  });
  it('has-local, filter option', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(
      env.hasLocal(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      false
    );
  });
  it('set', function (): any {
    const env: any = new Environment();
    env.set(Symbol.for('foo'), 'bar');
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  it('set-entry', function (): any {
    const env: any = new Environment();
    env.setEntry([Symbol.for('foo'), 'bar']);
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  it('set-local', function (): any {
    const env: any = new Environment();
    env.setLocal(Symbol.for('foo'), 'bar');
    return assertEqual(env.getLocal(Symbol.for('foo')), 'bar');
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
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  it('get, nonexistant binding', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.get(Symbol.for('quux')), undefined);
  });
  it('get, nonexistant binding, notFound option', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.get(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get, filter option', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.get(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-value', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getValue(Symbol.for('foo')), 'bar');
  });
  it('get-value, nonexistant binding', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getValue(Symbol.for('quux')), undefined);
  });
  it('get-value, nonexistant binding, notFound option', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getValue(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get-value, filter option', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getValue(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-typed-value', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getTypedValue(Symbol.for('foo')), [
      'bar',
      'variable',
    ]);
  });
  it('get-typed-value, nonexistant binding', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getTypedValue(Symbol.for('quux')), [
      undefined,
      'undefined',
    ]);
  });
  it('get-typed-value, nonexistant binding, notFound option', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getTypedValue(Symbol.for('quux'), {
        notFound: [false, 'undefined'],
      }),
      [false, 'undefined']
    );
  });
  it('get-typed-value, filter option', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getTypedValue(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      [undefined, 'undefined']
    );
  });
  it('get-local', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getLocal(Symbol.for('foo')), 'bar');
  });
  it('get-local, nonexistant binding', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getLocal(Symbol.for('quux')), undefined);
  });
  it('get-local, nonexistant binding, notFound option', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getLocal(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get-local, filter option', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getLocal(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-type', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getType(Symbol.for('foo')), 'variable');
  });
  it('get-type, nonexistant binding', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getType(Symbol.for('quux')), 'undefined');
  });
  it('get-type, filter option', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getType(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      'undefined'
    );
  });
  it('has', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.has(Symbol.for('foo')), true);
  });
  it('has, nonexistant binding', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.has(Symbol.for('quux')), false);
  });
  it('has, filter option', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.has(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      false
    );
  });
  it('has-local', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.hasLocal(Symbol.for('foo')), true);
  });
  it('has-local, nonexistant binding', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.hasLocal(Symbol.for('quux')), false);
  });
  it('has-local, filter option', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.hasLocal(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      false
    );
  });
  it('set', function (): any {
    const env: any = new TypedEnvironment();
    env.set(Symbol.for('foo'), 'bar', 'variable');
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  it('set-entry', function (): any {
    const env: any = new TypedEnvironment();
    env.setEntry([Symbol.for('foo'), ['bar', 'variable']]);
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  xit('set-local', function (): any {
    const env: any = new TypedEnvironment();
    env.setLocal(Symbol.for('foo'), 'bar', 'variable');
    return assertEqual(env.getLocal(Symbol.for('foo')), 'bar');
  });
  return it('set, mutate existing value in parent environment', function (): any {
    const parent: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    const env: any = extendEnvironment(new TypedEnvironment(), parent);
    env.set(Symbol.for('foo'), 'quux', 'variable');
    assertEqual(parent.get(Symbol.for('foo')), 'quux');
    assertEqual(env.getLocal(Symbol.for('foo')), undefined);
    return assertEqual(env.get(Symbol.for('foo')), 'quux');
  });
});

describe('LispEnvironment', function (): any {
  it('get', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  it('get, nonexistant binding', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.get(Symbol.for('quux')), undefined);
  });
  it('get, nonexistant binding, notFound option', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.get(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get, filter option', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.get(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-value', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getValue(Symbol.for('foo')), 'bar');
  });
  it('get-value, nonexistant binding', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getValue(Symbol.for('quux')), undefined);
  });
  it('get-value, nonexistant binding, notFound option', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getValue(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get-value, filter option', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getValue(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-typed-value', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getTypedValue(Symbol.for('foo')), [
      'bar',
      'variable',
    ]);
  });
  it('get-typed-value, nonexistant binding', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getTypedValue(Symbol.for('quux')), [
      undefined,
      'undefined',
    ]);
  });
  it('get-typed-value, nonexistant binding, notFound option', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getTypedValue(Symbol.for('quux'), {
        notFound: [false, 'undefined'],
      }),
      [false, 'undefined']
    );
  });
  it('get-typed-value, filter option', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getTypedValue(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      [undefined, 'undefined']
    );
  });
  it('get-local', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getLocal(Symbol.for('foo')), 'bar');
  });
  it('get-local, nonexistant binding', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getLocal(Symbol.for('quux')), undefined);
  });
  it('get-local, nonexistant binding, notFound option', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getLocal(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get-local, filter option', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getLocal(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-type', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getType(Symbol.for('foo')), 'variable');
  });
  it('get-type, nonexistant binding', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getType(Symbol.for('quux')), 'undefined');
  });
  it('get-type, filter option', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.getType(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      'undefined'
    );
  });
  it('has', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.has(Symbol.for('foo')), true);
  });
  it('has, nonexistant binding', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.has(Symbol.for('quux')), false);
  });
  it('has, filter option', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.has(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      false
    );
  });
  it('has-local', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.hasLocal(Symbol.for('foo')), true);
  });
  it('has-local, nonexistant binding', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.hasLocal(Symbol.for('quux')), false);
  });
  it('has-local, filter option', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(
      env.hasLocal(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      false
    );
  });
  it('set', function (): any {
    const env: any = new LispEnvironment();
    env.set(Symbol.for('foo'), 'bar', 'variable');
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  it('set-entry', function (): any {
    const env: any = new LispEnvironment();
    env.setEntry([Symbol.for('foo'), ['bar', 'variable']]);
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  xit('set-local', function (): any {
    const env: any = new LispEnvironment();
    env.setLocal(Symbol.for('foo'), 'bar', 'variable');
    return assertEqual(env.getLocal(Symbol.for('foo')), 'bar');
  });
  return it('set, mutate existing value in parent environment', function (): any {
    const parent: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    const env: any = extendEnvironment(new LispEnvironment(), parent);
    env.set(Symbol.for('foo'), 'quux', 'variable');
    assertEqual(parent.get(Symbol.for('foo')), 'quux');
    assertEqual(env.getLocal(Symbol.for('foo')), undefined);
    return assertEqual(env.get(Symbol.for('foo')), 'quux');
  });
});

describe('EnvironmentStack', function (): any {
  it('get', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  it('get, nonexistant binding', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(env.get(Symbol.for('quux')), undefined);
  });
  it('get, nonexistant binding, notFound option', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(
      env.get(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get, filter option', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(
      env.get(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-value', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(env.getValue(Symbol.for('foo')), 'bar');
  });
  it('get-value, nonexistant binding', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(env.getValue(Symbol.for('quux')), undefined);
  });
  it('get-value, nonexistant binding, notFound option', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(
      env.getValue(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get-value, filter option', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(
      env.getValue(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-typed-value', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(env.getTypedValue(Symbol.for('foo')), [
      'bar',
      'variable',
    ]);
  });
  it('get-typed-value 2', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']]),
      new EnvironmentStack(
        new LispEnvironment([[Symbol.for('bar'), 'bar', 'variable']]),
        new JavaScriptEnvironment()
      )
    );
    return assertEqual(env.getTypedValue(Symbol.for('foo')), [
      'bar',
      'variable',
    ]);
  });
  it('get-typed-value, nonexistant binding', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(env.getTypedValue(Symbol.for('quux')), [
      undefined,
      'undefined',
    ]);
  });
  it('get-typed-value, nonexistant binding, notFound option', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(
      env.getTypedValue(Symbol.for('quux'), {
        notFound: [false, 'undefined'],
      }),
      [false, 'undefined']
    );
  });
  it('get-typed-value, filter option', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(
      env.getTypedValue(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      [undefined, 'undefined']
    );
  });
  it('set, one environment', function (): any {
    const env1: any = new LispEnvironment();
    const env: any = new EnvironmentStack(env1);
    env.set(Symbol.for('foo'), 'bar', 'variable');
    assertEqual(env.get(Symbol.for('foo')), 'bar');
    return assertEqual(env1.get(Symbol.for('foo')), 'bar');
  });
  it('set, two environments, previously defined in second', function (): any {
    const env1: any = new LispEnvironment();
    const env2: any = new LispEnvironment([
      [Symbol.for('foo'), 'foo', 'variable'],
    ]);
    const env: any = new EnvironmentStack(env1, env2);
    env.set(Symbol.for('foo'), 'bar', 'variable');
    assertEqual(env.get(Symbol.for('foo')), 'bar');
    assertEqual(env1.get(Symbol.for('foo')), undefined);
    return assertEqual(env2.get(Symbol.for('foo')), 'bar');
  });
  return it('set-entry, two environments, previously defined in second', function (): any {
    const env1: any = new LispEnvironment();
    const env2: any = new LispEnvironment([
      [Symbol.for('foo'), 'foo', 'variable'],
    ]);
    const env: any = new EnvironmentStack(env1, env2);
    env.setEntry([Symbol.for('foo'), ['bar', 'variable']]);
    assertEqual(env.get(Symbol.for('foo')), 'bar');
    assertEqual(env1.get(Symbol.for('foo')), 'bar');
    return assertEqual(env2.get(Symbol.for('foo')), 'foo');
  });
});

describe('EnvironmentPipe', function (): any {
  it('get', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(env.get(Symbol.for('foo')), Symbol.for('baz'));
  });
  it('get, nonexistant binding', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(env.get(Symbol.for('quux')), undefined);
  });
  it('get, nonexistant binding, notFound option', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(
      env.get(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get, filter option', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(
      env.get(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-value', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(env.getValue(Symbol.for('foo')), Symbol.for('baz'));
  });
  it('get-value, notexistant binding', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(env.getValue(Symbol.for('quux')), undefined);
  });
  it('get-value, notexistant binding, notFound option', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(
      env.getValue(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get-value, filter option', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(
      env.getValue(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-typed-value', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(env.getTypedValue(Symbol.for('foo')), [
      Symbol.for('baz'),
      'variable',
    ]);
  });
  it('get-typed-value, nonexistant binding', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(env.getTypedValue(Symbol.for('quux')), [
      undefined,
      'undefined',
    ]);
  });
  it('get-typed-value, nonexistant binding, notFound option', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(
      env.getTypedValue(Symbol.for('quux'), {
        notFound: [false, 'undefined'],
      }),
      [false, 'undefined']
    );
  });
  return it('get-typed-value, filter option', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(
      env.getTypedValue(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      [undefined, 'undefined']
    );
  });
});

describe('EnvironmentComposition', function (): any {
  it('get', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(env.get(Symbol.for('foo')), Symbol.for('baz'));
  });
  it('get, nonexistant binding', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(env.get(Symbol.for('quux')), undefined);
  });
  it('get, nonexistant binding, notFound option', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(
      env.get(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get, filter option', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(
      env.get(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-value', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(env.getValue(Symbol.for('foo')), Symbol.for('baz'));
  });
  it('get-value, nonexistant binding', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(env.getValue(Symbol.for('quux')), undefined);
  });
  it('get-value, nonexistant binding, notFound option', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(
      env.getValue(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get-value, filter option', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(
      env.getValue(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-typed-value', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(env.getTypedValue(Symbol.for('foo')), [
      Symbol.for('baz'),
      'variable',
    ]);
  });
  it('get-typed-value, nonexistant binding', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(env.getTypedValue(Symbol.for('quux')), [
      undefined,
      'undefined',
    ]);
  });
  it('get-typed-value, nonexistant binding, notFound option', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(
      env.getTypedValue(Symbol.for('quux'), {
        notFound: [false, 'undefined'],
      }),
      [false, 'undefined']
    );
  });
  return it('get-typed-value, filter option', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(
      env.getTypedValue(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      [undefined, 'undefined']
    );
  });
});

describe('ThunkedEnvironment', function (): any {
  it('get', function (): any {
    const env: any = new ThunkedEnvironment([
      [
        Symbol.for('foo'),
        thunk(function (): any {
          return 'bar';
        }),
        'variable',
      ],
    ]);
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  it('get, nonexistant binding', function (): any {
    const env: any = new ThunkedEnvironment([
      [
        Symbol.for('foo'),
        thunk(function (): any {
          return 'bar';
        }),
        'variable',
      ],
    ]);
    return assertEqual(env.get(Symbol.for('quux')), undefined);
  });
  it('get, nonexistant binding, notFound option', function (): any {
    const env: any = new ThunkedEnvironment([
      [
        Symbol.for('foo'),
        thunk(function (): any {
          return 'bar';
        }),
        'variable',
      ],
    ]);
    return assertEqual(
      env.get(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  return it('get, filter option', function (): any {
    const env: any = new ThunkedEnvironment([
      [
        Symbol.for('foo'),
        thunk(function (): any {
          return 'bar';
        }),
        'variable',
      ],
    ]);
    return assertEqual(
      env.get(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
});

describe('JavaScriptEnvironment', function (): any {
  it('get', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.get(Symbol.for('Map')), Map);
  });
  it('get, nonexistant binding', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.get(Symbol.for('quux')), undefined);
  });
  it('get, nonexistant binding, notFound option', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(
      env.get(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get, filter option', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(
      env.get(Symbol.for('foo'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('get-local', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.getLocal(Symbol.for('Map')), Map);
  });
  it('get-local, nonexistant binding', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.getLocal(Symbol.for('quux')), undefined);
  });
  it('get-local, nonexistant binding, notFound option', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(
      env.getLocal(Symbol.for('quux'), {
        notFound: false,
      }),
      false
    );
  });
  it('get-local, filter option', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(
      env.getLocal(Symbol.for('Map'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      undefined
    );
  });
  it('has', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.has(Symbol.for('Map')), true);
  });
  it('has, nonexistant binding', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.has(Symbol.for('quux')), false);
  });
  it('has, filter option', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(
      env.has(Symbol.for('Map'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      false
    );
  });
  it('has-local', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.hasLocal(Symbol.for('Map')), true);
  });
  it('has-local, nonexistant binding', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.hasLocal(Symbol.for('quux')), false);
  });
  return it('has-local, filter option', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(
      env.hasLocal(Symbol.for('Map'), {
        filter: function (x: any): any {
          return false;
        },
      }),
      false
    );
  });
});
