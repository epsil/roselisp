import {
  Environment,
  EnvironmentComposition,
  EnvironmentPipe,
  EnvironmentStack,
  JavaScriptEnvironment,
  LispEnvironment,
  PromiseEnvironment,
  TypedEnvironment,
  extendEnvironment
} from '../../src/ts/env';

import {
  InternalPromise
} from '../../src/ts/thunk';

import {
  assertEqual,
  testMacro
} from './test-util';

testMacro.ftype = 'macro';

describe('Environment', (): any => {
  it('find-frame', (): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.findFrame(Symbol.for('foo')), env);
  });
  it('find-frame, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.findFrame(Symbol.for('quux'));
  })(), undefined));
  it('find-frame, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.findFrame(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('find-frame, filter option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    function filter(x: any): any {
      return false;
    }
    return env.findFrame(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('find-frame, filter option, parent stack', (): any => assertEqual(((): any => {
    const env1: any = new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]);
    const env2: any = new LispEnvironment([[Symbol.for('bar'), 'baz', Symbol.for('Any')]]);
    const env: any = new Environment([], new EnvironmentStack(env1, env2));
    function filter(x: any): any {
      return x !== env2;
    }
    return env.findFrame(Symbol.for('bar'), {
      filter
    });
  })(), undefined));
  it('find-frame, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']], new Environment([[Symbol.for('foo'), 'baz']]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.findFrame(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.get(Symbol.for('foo'));
  })(), 'bar'));
  it('get, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.get(Symbol.for('quux'));
  })(), undefined));
  it('get, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.get(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get, filter option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    function filter(x: any): any {
      return false;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']], new Environment([[Symbol.for('foo'), 'baz']]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-value', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.getValue(Symbol.for('foo'));
  })(), 'bar'));
  it('get-local', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.getLocal(Symbol.for('foo'));
  })(), 'bar'));
  it('get-local, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.getLocal(Symbol.for('quux'));
  })(), undefined));
  it('get-local, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.getLocal(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get-local, filter option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    function filter(x: any): any {
      return false;
    }
    return env.getLocal(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-tuple', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.getTuple(Symbol.for('foo'));
  })(), ['bar', true]));
  it('get-tuple, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.getTuple(Symbol.for('quux'));
  })(), [undefined, false]));
  it('get-tuple, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.getTuple(Symbol.for('quux'), {
      notFound: false
    });
  })(), [false, false]));
  it('get-tuple, filter option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    function filter(x: any): any {
      return false;
    }
    return env.getTuple(Symbol.for('quux'), {
      filter
    });
  })(), [undefined, false]));
  it('get-tuple, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']], new Environment([[Symbol.for('foo'), 'baz']]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.getTuple(Symbol.for('quux'), {
      filter
    });
  })(), [undefined, false]));
  it('get-local-tuple', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.getLocalTuple(Symbol.for('foo'));
  })(), ['bar', true]));
  it('get-local-tuple, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.getLocalTuple(Symbol.for('quux'));
  })(), [undefined, false]));
  it('get-local-tuple, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.getLocalTuple(Symbol.for('quux'), {
      notFound: false
    });
  })(), [false, false]));
  it('get-local-tuple, filter option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    function filter(x: any): any {
      return false;
    }
    return env.getLocalTuple(Symbol.for('quux'), {
      filter
    });
  })(), [undefined, false]));
  it('has?', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.hasp(Symbol.for('foo'));
  })(), true));
  it('has?, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.hasp(Symbol.for('quux'));
  })(), false));
  it('has?, filter option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    function filter(x: any): any {
      return false;
    }
    return env.hasp(Symbol.for('foo'), {
      filter
    });
  })(), false));
  it('has?, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']], new Environment([[Symbol.for('foo'), 'baz']]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.hasp(Symbol.for('foo'), {
      filter
    });
  })(), false));
  it('has-local?', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.hasLocalP(Symbol.for('foo'));
  })(), true));
  it('has-local?, parent environment', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'foo']], new Environment([[Symbol.for('bar'), 'bar']]));
    return env.hasLocalP(Symbol.for('bar'));
  })(), false));
  it('has-local?, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return env.hasLocalP(Symbol.for('quux'));
  })(), false));
  it('has-local?, filter option', (): any => assertEqual(((): any => {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    function filter(x: any): any {
      return false;
    }
    return env.hasLocalP(Symbol.for('foo'), {
      filter
    });
  })(), false));
  it('set!', (): any => assertEqual(((): any => {
    const env: any = new Environment();
    env.setx(Symbol.for('foo'), 'bar');
    return env.get(Symbol.for('foo'));
  })(), 'bar'));
  it('set-entry!', (): any => assertEqual(((): any => {
    const env: any = new Environment();
    env.setEntryX([Symbol.for('foo'), 'bar']);
    return env.get(Symbol.for('foo'));
  })(), 'bar'));
  it('set-local!', (): any => assertEqual(((): any => {
    const env: any = new Environment();
    env.setLocalX(Symbol.for('foo'), 'bar');
    return env.getLocal(Symbol.for('foo'));
  })(), 'bar'));
  return it('set!, mutate existing value in parent environment', (): any => {
    const parent: any = new Environment([[Symbol.for('foo'), 'bar']]);
    const env: any = extendEnvironment(new Environment(), parent);
    env.setx(Symbol.for('foo'), 'quux');
    assertEqual(parent.get(Symbol.for('foo')), 'quux');
    assertEqual(env.getLocal(Symbol.for('foo')), undefined);
    return assertEqual(env.get(Symbol.for('foo')), 'quux');
  });
});

describe('TypedEnvironment', (): any => {
  it('get', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.get(Symbol.for('foo'));
  })(), 'bar'));
  it('get, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.get(Symbol.for('quux'));
  })(), undefined));
  it('get, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.get(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]], new TypedEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-value', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getValue(Symbol.for('foo'));
  })(), 'bar'));
  it('get-value, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getValue(Symbol.for('quux'));
  })(), undefined));
  it('get-value, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getValue(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get-value, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.getValue(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-value, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]], new TypedEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.getValue(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-typed-value', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getTypedValue(Symbol.for('foo'));
  })(), ['bar', Symbol.for('Any')]));
  it('get-typed-value, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getTypedValue(Symbol.for('quux'));
  })(), [undefined, Symbol.for('Undefined')]));
  it('get-typed-value, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getTypedValue(Symbol.for('quux'), {
      notFound: [false, Symbol.for('Undefined')]
    });
  })(), [false, Symbol.for('Undefined')]));
  it('get-typed-value, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.getTypedValue(Symbol.for('foo'), {
      filter
    });
  })(), [undefined, Symbol.for('Undefined')]));
  it('get-typed-value, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]], new TypedEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.getTypedValue(Symbol.for('foo'), {
      filter
    });
  })(), [undefined, Symbol.for('Undefined')]));
  it('get-local', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getLocal(Symbol.for('foo'));
  })(), 'bar'));
  it('get-local, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getLocal(Symbol.for('quux'));
  })(), undefined));
  it('get-local, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getLocal(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get-local, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.getLocal(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-type', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getType(Symbol.for('foo'));
  })(), Symbol.for('Any')));
  it('get-type, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getType(Symbol.for('quux'));
  })(), Symbol.for('Undefined')));
  it('get-type, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getType(Symbol.for('quux'), {
      notFound: Symbol.for('Any')
    });
  })(), Symbol.for('Any')));
  it('get-type, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.getType(Symbol.for('foo'), {
      filter
    });
  })(), Symbol.for('Undefined')));
  it('get-type, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]], new TypedEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.getType(Symbol.for('foo'), {
      filter
    });
  })(), Symbol.for('Undefined')));
  it('has?', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.hasp(Symbol.for('foo'));
  })(), true));
  it('has?, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.hasp(Symbol.for('quux'));
  })(), false));
  it('has?, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.hasp(Symbol.for('foo'), {
      filter
    });
  })(), false));
  it('has?, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]], new TypedEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.hasp(Symbol.for('foo'), {
      filter
    });
  })(), false));
  it('has-local?', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.hasLocalP(Symbol.for('foo'));
  })(), true));
  it('has-local?, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.hasLocalP(Symbol.for('quux'));
  })(), false));
  it('has-local?, filter option', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.hasLocalP(Symbol.for('foo'), {
      filter
    });
  })(), false));
  it('set!', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment();
    env.setx(Symbol.for('foo'), 'bar', Symbol.for('Any'));
    return env.get(Symbol.for('foo'));
  })(), 'bar'));
  it('set-entry!', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment();
    env.setEntryX([Symbol.for('foo'), ['bar', Symbol.for('Any')]]);
    return env.get(Symbol.for('foo'));
  })(), 'bar'));
  xit('set-local!', (): any => assertEqual(((): any => {
    const env: any = new TypedEnvironment();
    env.setLocalX(Symbol.for('foo'), 'bar', Symbol.for('Any'));
    return env.getLocal(Symbol.for('foo'));
  })(), 'bar'));
  return it('set!, mutate existing value in parent environment', (): any => {
    const parent: any = new TypedEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    const env: any = extendEnvironment(new TypedEnvironment(), parent);
    env.setx(Symbol.for('foo'), 'quux', Symbol.for('Any'));
    assertEqual(parent.get(Symbol.for('foo')), 'quux');
    assertEqual(env.getLocal(Symbol.for('foo')), undefined);
    return assertEqual(env.get(Symbol.for('foo')), 'quux');
  });
});

describe('LispEnvironment', (): any => {
  it('get', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.get(Symbol.for('foo'));
  })(), 'bar'));
  it('get, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.get(Symbol.for('quux'));
  })(), undefined));
  it('get, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.get(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get, filter option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]], new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-value', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getValue(Symbol.for('foo'));
  })(), 'bar'));
  it('get-value, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getValue(Symbol.for('quux'));
  })(), undefined));
  it('get-value, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getValue(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get-value, filter option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.getValue(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-value, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]], new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.getValue(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-typed-value', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getTypedValue(Symbol.for('foo'));
  })(), ['bar', Symbol.for('Any')]));
  it('get-typed-value, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getTypedValue(Symbol.for('quux'));
  })(), [undefined, Symbol.for('Undefined')]));
  it('get-typed-value, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getTypedValue(Symbol.for('quux'), {
      notFound: [false, Symbol.for('Undefined')]
    });
  })(), [false, Symbol.for('Undefined')]));
  it('get-typed-value, filter option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.getTypedValue(Symbol.for('foo'), {
      filter
    });
  })(), [undefined, Symbol.for('Undefined')]));
  it('get-typed-value, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]], new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.getTypedValue(Symbol.for('foo'), {
      filter
    });
  })(), [undefined, Symbol.for('Undefined')]));
  it('get-local', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getLocal(Symbol.for('foo'));
  })(), 'bar'));
  it('get-local, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getLocal(Symbol.for('quux'));
  })(), undefined));
  it('get-local, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getLocal(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get-local, filter option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.getLocal(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-type', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getType(Symbol.for('foo'));
  })(), Symbol.for('Any')));
  it('get-type, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getType(Symbol.for('quux'));
  })(), Symbol.for('Undefined')));
  it('get-type, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.getType(Symbol.for('quux'), {
      notFound: Symbol.for('Any')
    });
  })(), Symbol.for('Any')));
  it('get-type, filter option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.getType(Symbol.for('foo'), {
      filter
    });
  })(), Symbol.for('Undefined')));
  it('get-type, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]], new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.getType(Symbol.for('foo'), {
      filter
    });
  })(), Symbol.for('Undefined')));
  it('has?', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.hasp(Symbol.for('foo'));
  })(), true));
  it('has?, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.hasp(Symbol.for('quux'));
  })(), false));
  it('has?, filter option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.hasp(Symbol.for('foo'), {
      filter
    });
  })(), false));
  it('has?, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]], new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.hasp(Symbol.for('foo'), {
      filter
    });
  })(), false));
  it('has-local?', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.hasLocalP(Symbol.for('foo'));
  })(), true));
  it('has-local?, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    return env.hasLocalP(Symbol.for('quux'));
  })(), false));
  it('has-local?, filter option', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.hasLocalP(Symbol.for('foo'), {
      filter
    });
  })(), false));
  it('set!', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment();
    env.setx(Symbol.for('foo'), 'bar', Symbol.for('Any'));
    return env.get(Symbol.for('foo'));
  })(), 'bar'));
  it('set-entry!', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment();
    env.setEntryX([Symbol.for('foo'), ['bar', Symbol.for('Any')]]);
    return env.get(Symbol.for('foo'));
  })(), 'bar'));
  xit('set-local!', (): any => assertEqual(((): any => {
    const env: any = new LispEnvironment();
    env.setLocalX(Symbol.for('foo'), 'bar', Symbol.for('Any'));
    return env.getLocal(Symbol.for('foo'));
  })(), 'bar'));
  return it('set!, mutate existing value in parent environment', (): any => {
    const parent: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    const env: any = extendEnvironment(new LispEnvironment(), parent);
    env.setx(Symbol.for('foo'), 'quux', Symbol.for('Any'));
    assertEqual(parent.get(Symbol.for('foo')), 'quux');
    assertEqual(env.getLocal(Symbol.for('foo')), undefined);
    return assertEqual(env.get(Symbol.for('foo')), 'quux');
  });
});

describe('EnvironmentStack', (): any => {
  it('get', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    return env.get(Symbol.for('foo'));
  })(), 'bar'));
  it('get, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    return env.get(Symbol.for('quux'));
  })(), undefined));
  it('get, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    return env.get(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get, filter option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    function filter(x: any): any {
      return false;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get, multiple environments, filter option', (): any => assertEqual(((): any => {
    const env1: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    const env2: any = new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]);
    const env: any = new EnvironmentStack(env1, env2);
    function filter(x: any): any {
      return x !== env1;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-value', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    return env.getValue(Symbol.for('foo'));
  })(), 'bar'));
  it('get-value, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    return env.getValue(Symbol.for('quux'));
  })(), undefined));
  it('get-value, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    return env.getValue(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get-value, filter option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    function filter(x: any): any {
      return false;
    }
    return env.getValue(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-value, multiple environments, filter option', (): any => assertEqual(((): any => {
    const env1: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    const env2: any = new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]);
    const env: any = new EnvironmentStack(env1, env2);
    function filter(x: any): any {
      return x !== env1;
    }
    return env.getValue(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-typed-value', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    return env.getTypedValue(Symbol.for('foo'));
  })(), ['bar', Symbol.for('Any')]));
  it('get-typed-value 2', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]), new EnvironmentStack(new LispEnvironment([[Symbol.for('bar'), 'bar', Symbol.for('Any')]]), new JavaScriptEnvironment()));
    return env.getTypedValue(Symbol.for('foo'));
  })(), ['bar', Symbol.for('Any')]));
  it('get-typed-value, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    return env.getTypedValue(Symbol.for('quux'));
  })(), [undefined, Symbol.for('Undefined')]));
  it('get-typed-value, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    return env.getTypedValue(Symbol.for('quux'), {
      notFound: [false, Symbol.for('Undefined')]
    });
  })(), [false, Symbol.for('Undefined')]));
  it('get-typed-value, filter option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]));
    function filter(x: any): any {
      return false;
    }
    return env.getTypedValue(Symbol.for('foo'), {
      filter
    });
  })(), [undefined, Symbol.for('Undefined')]));
  it('get-typed-value, multiple environments, filter option', (): any => assertEqual(((): any => {
    const env1: any = new LispEnvironment([[Symbol.for('foo'), 'bar', Symbol.for('Any')]]);
    const env2: any = new LispEnvironment([[Symbol.for('foo'), 'baz', Symbol.for('Any')]]);
    const env: any = new EnvironmentStack(env1, env2);
    function filter(x: any): any {
      return x !== env1;
    }
    return env.getTypedValue(Symbol.for('foo'), {
      filter
    });
  })(), [undefined, Symbol.for('Undefined')]));
  it('set!, one environment', (): any => {
    const env1: any = new LispEnvironment();
    const env: any = new EnvironmentStack(env1);
    env.setx(Symbol.for('foo'), 'bar', Symbol.for('Any'));
    assertEqual(env.get(Symbol.for('foo')), 'bar');
    return assertEqual(env1.get(Symbol.for('foo')), 'bar');
  });
  it('set!, two environments, previously defined in second', (): any => {
    const env1: any = new LispEnvironment();
    const env2: any = new LispEnvironment([[Symbol.for('foo'), 'foo', Symbol.for('Any')]]);
    const env: any = new EnvironmentStack(env1, env2);
    env.setx(Symbol.for('foo'), 'bar', Symbol.for('Any'));
    assertEqual(env.get(Symbol.for('foo')), 'bar');
    assertEqual(env1.get(Symbol.for('foo')), undefined);
    return assertEqual(env2.get(Symbol.for('foo')), 'bar');
  });
  it('set-entry!, two environments, previously defined in second', (): any => {
    const env1: any = new LispEnvironment();
    const env2: any = new LispEnvironment([[Symbol.for('foo'), 'foo', Symbol.for('Any')]]);
    const env: any = new EnvironmentStack(env1, env2);
    env.setEntryX([Symbol.for('foo'), ['bar', Symbol.for('Any')]]);
    assertEqual(env.get(Symbol.for('foo')), 'bar');
    assertEqual(env1.get(Symbol.for('foo')), 'bar');
    return assertEqual(env2.get(Symbol.for('foo')), 'foo');
  });
  it('has-promise?', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')]]));
    return env.hasPromiseP(Symbol.for('foo'));
  })(), true));
  return it('has-local-promise?', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentStack(new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')]]));
    return env.hasLocalPromiseP(Symbol.for('foo'));
  })(), true));
});

describe('EnvironmentPipe', (): any => {
  it('get', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentPipe(new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]));
    return env.get(Symbol.for('foo'));
  })(), Symbol.for('baz')));
  it('get, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentPipe(new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]));
    return env.get(Symbol.for('quux'));
  })(), undefined));
  it('get, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentPipe(new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]));
    return env.get(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get, filter option', (): any => assertEqual(((): any => {
    const env1: any = new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]);
    const env2: any = new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]);
    const env: any = new EnvironmentPipe(env1, env2);
    function filter(x: any): any {
      return x !== env1;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-value', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentPipe(new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]));
    return env.getValue(Symbol.for('foo'));
  })(), Symbol.for('baz')));
  it('get-value, notexistant binding', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentPipe(new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]));
    return env.getValue(Symbol.for('quux'));
  })(), undefined));
  it('get-value, notexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentPipe(new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]));
    return env.getValue(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get-value, filter option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentPipe(new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]));
    function filter(x: any): any {
      return false;
    }
    return env.getValue(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-typed-value', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentPipe(new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]));
    return env.getTypedValue(Symbol.for('foo'));
  })(), [Symbol.for('baz'), Symbol.for('Any')]));
  it('get-typed-value, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentPipe(new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]));
    return env.getTypedValue(Symbol.for('quux'));
  })(), [undefined, Symbol.for('Undefined')]));
  it('get-typed-value, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentPipe(new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]));
    return env.getTypedValue(Symbol.for('quux'), {
      notFound: [false, Symbol.for('Undefined')]
    });
  })(), [false, Symbol.for('Undefined')]));
  return it('get-typed-value, filter option', (): any => assertEqual(((): any => {
    const env1: any = new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]);
    const env2: any = new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]);
    const env: any = new EnvironmentPipe(env1, env2);
    function filter(x: any): any {
      return x !== env1;
    }
    return env.getTypedValue(Symbol.for('foo'), {
      filter
    });
  })(), [undefined, Symbol.for('Undefined')]));
});

describe('EnvironmentComposition', (): any => {
  it('get', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentComposition(new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]));
    return env.get(Symbol.for('foo'));
  })(), Symbol.for('baz')));
  it('get, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentComposition(new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]));
    return env.get(Symbol.for('quux'));
  })(), undefined));
  it('get, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentComposition(new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]));
    return env.get(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get, filter option', (): any => assertEqual(((): any => {
    const env1: any = new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]);
    const env2: any = new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]);
    const env: any = new EnvironmentComposition(env2, env1);
    function filter(x: any): any {
      return x !== env1;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-value', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentComposition(new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]));
    return env.getValue(Symbol.for('foo'));
  })(), Symbol.for('baz')));
  it('get-value, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentComposition(new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]));
    return env.getValue(Symbol.for('quux'));
  })(), undefined));
  it('get-value, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentComposition(new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]));
    return env.getValue(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get-value, filter option', (): any => assertEqual(((): any => {
    const env1: any = new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]);
    const env2: any = new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]);
    const env: any = new EnvironmentComposition(env2, env1);
    function filter(x: any): any {
      return x !== env1;
    }
    return env.getValue(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-typed-value', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentComposition(new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]));
    return env.getTypedValue(Symbol.for('foo'));
  })(), [Symbol.for('baz'), Symbol.for('Any')]));
  it('get-typed-value, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentComposition(new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]));
    return env.getTypedValue(Symbol.for('quux'));
  })(), [undefined, Symbol.for('Undefined')]));
  it('get-typed-value, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new EnvironmentComposition(new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]), new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]));
    return env.getTypedValue(Symbol.for('quux'), {
      notFound: [false, Symbol.for('Undefined')]
    });
  })(), [false, Symbol.for('Undefined')]));
  return it('get-typed-value, filter option', (): any => assertEqual(((): any => {
    const env1: any = new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), Symbol.for('Any')]]);
    const env2: any = new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), Symbol.for('Any')]]);
    const env: any = new EnvironmentComposition(env2, env1);
    function filter(x: any): any {
      return x !== env1;
    }
    return env.getTypedValue(Symbol.for('foo'), {
      filter
    });
  })(), [undefined, Symbol.for('Undefined')]));
});

describe('PromiseEnvironment', (): any => {
  it('get', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')]]);
    return env.get(Symbol.for('foo'));
  })(), 'foo'));
  it('get, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')]]);
    return env.get(Symbol.for('quux'));
  })(), undefined));
  it('get, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')]]);
    return env.get(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get, filter option', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')]]);
    function filter(x: any): any {
      return false;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get, parent environment, filter option', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')]], new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF1: any = (): any => {
        if (promiseF1.forced) {
          return promiseF1.value;
        } else {
          promiseF1.forced = undefined;
          promiseF1.value = 'bar';
          promiseF1.forced = true;
          return promiseF1.value;
        }
      };
      promiseF1.value = undefined as any;
      promiseF1.forced = false as any;
      promiseF1.ftype = 'thunk';
      return promiseF1;
    })()), Symbol.for('Any')]]));
    function filter(x: any): any {
      return x !== env;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('has-promise?, true', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')]]);
    return env.hasPromiseP(Symbol.for('foo'));
  })(), true));
  it('has-promise?, parent environment, true', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')]], new PromiseEnvironment([[Symbol.for('bar'), new InternalPromise(((): any => {
      const promiseF1: any = (): any => {
        if (promiseF1.forced) {
          return promiseF1.value;
        } else {
          promiseF1.forced = undefined;
          promiseF1.value = 'bar';
          promiseF1.forced = true;
          return promiseF1.value;
        }
      };
      promiseF1.value = undefined as any;
      promiseF1.forced = false as any;
      promiseF1.ftype = 'thunk';
      return promiseF1;
    })()), Symbol.for('Any')]]));
    return env.hasPromiseP(Symbol.for('bar'));
  })(), true));
  it('has-promise?, false', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')], [Symbol.for('bar'), 'bar', Symbol.for('Any')]]);
    return env.hasPromiseP(Symbol.for('bar'));
  })(), false));
  it('has-local-promise?, true', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')]], new PromiseEnvironment([[Symbol.for('bar'), new InternalPromise(((): any => {
      const promiseF1: any = (): any => {
        if (promiseF1.forced) {
          return promiseF1.value;
        } else {
          promiseF1.forced = undefined;
          promiseF1.value = 'bar';
          promiseF1.forced = true;
          return promiseF1.value;
        }
      };
      promiseF1.value = undefined as any;
      promiseF1.forced = false as any;
      promiseF1.ftype = 'thunk';
      return promiseF1;
    })()), Symbol.for('Any')]]));
    return env.hasLocalPromiseP(Symbol.for('foo'));
  })(), true));
  it('has-local-promise?, false', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = 'foo';
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })()), Symbol.for('Any')]], new PromiseEnvironment([[Symbol.for('bar'), new InternalPromise(((): any => {
      const promiseF1: any = (): any => {
        if (promiseF1.forced) {
          return promiseF1.value;
        } else {
          promiseF1.forced = undefined;
          promiseF1.value = 'bar';
          promiseF1.forced = true;
          return promiseF1.value;
        }
      };
      promiseF1.value = undefined as any;
      promiseF1.forced = false as any;
      promiseF1.ftype = 'thunk';
      return promiseF1;
    })()), Symbol.for('Any')]]));
    return env.hasLocalPromiseP(Symbol.for('bar'));
  })(), false));
  it('get-type', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), 1, Symbol.for('Number')]]);
    return env.getType(Symbol.for('foo'));
  })(), Symbol.for('Number')));
  it('get-type, promise', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), 1, new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = Symbol.for('Number');
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })())]]);
    return env.getType(Symbol.for('foo'));
  })(), Symbol.for('Number')));
  it('get-local-type', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), 1, Symbol.for('Number')]]);
    return env.getLocalType(Symbol.for('foo'));
  })(), Symbol.for('Number')));
  return it('get-local-type, promise', (): any => assertEqual(((): any => {
    const env: any = new PromiseEnvironment([[Symbol.for('foo'), 1, new InternalPromise(((): any => {
      const promiseF: any = (): any => {
        if (promiseF.forced) {
          return promiseF.value;
        } else {
          promiseF.forced = undefined;
          promiseF.value = Symbol.for('Number');
          promiseF.forced = true;
          return promiseF.value;
        }
      };
      promiseF.value = undefined as any;
      promiseF.forced = false as any;
      promiseF.ftype = 'thunk';
      return promiseF;
    })())]]);
    return env.getLocalType(Symbol.for('foo'));
  })(), Symbol.for('Number')));
});

describe('JavaScriptEnvironment', (): any => {
  it('get', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    return env.get(Symbol.for('Map'));
  })(), Map));
  it('get, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    return env.get(Symbol.for('quux'));
  })(), undefined));
  it('get, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    return env.get(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get, filter option', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    function filter(x: any): any {
      return false;
    }
    return env.get(Symbol.for('foo'), {
      filter
    });
  })(), undefined));
  it('get-local', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    return env.getLocal(Symbol.for('Map'));
  })(), Map));
  it('get-local, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    return env.getLocal(Symbol.for('quux'));
  })(), undefined));
  it('get-local, nonexistant binding, notFound option', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    return env.getLocal(Symbol.for('quux'), {
      notFound: false
    });
  })(), false));
  it('get-local, filter option', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    function filter(x: any): any {
      return false;
    }
    return env.getLocal(Symbol.for('Map'), {
      filter
    });
  })(), undefined));
  it('has?', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    return env.hasp(Symbol.for('Map'));
  })(), true));
  it('has?, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    return env.hasp(Symbol.for('quux'));
  })(), false));
  it('has?, filter option', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    function filter(x: any): any {
      return false;
    }
    return env.hasp(Symbol.for('Map'), {
      filter
    });
  })(), false));
  it('has-local?', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    return env.hasLocalP(Symbol.for('Map'));
  })(), true));
  it('has-local?, nonexistant binding', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    return env.hasLocalP(Symbol.for('quux'));
  })(), false));
  return it('has-local?, filter option', (): any => assertEqual(((): any => {
    const env: any = new JavaScriptEnvironment();
    function filter(x: any): any {
      return false;
    }
    return env.hasLocalP(Symbol.for('Map'), {
      filter
    });
  })(), false));
});