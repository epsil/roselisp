import * as chai from 'chai';

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
  it('get', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.get(Symbol.for('foo')), 'bar');
  });
  it('get-value', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.getValue(Symbol.for('foo')), 'bar');
  });
  it('get-local', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.getLocal(Symbol.for('foo')), 'bar');
  });
  it('has', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.has(Symbol.for('foo')), true);
  });
  it('has-local', function (): any {
    const env: any = new Environment([[Symbol.for('foo'), 'bar']]);
    return assertEqual(env.hasLocal(Symbol.for('foo')), true);
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
  it('get-value', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getValue(Symbol.for('foo')), 'bar');
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
  xit('get-local', function (): any { // ???
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getLocal(Symbol.for('foo')), 'bar');
  });
  it('get-type', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getType(Symbol.for('foo')), 'variable');
  });
  it('has', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.has(Symbol.for('foo')), true);
  });
  xit('has-local', function (): any {
    const env: any = new TypedEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.hasLocal(Symbol.for('foo')), true);
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
  it('get-value', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getValue(Symbol.for('foo')), 'bar');
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
  xit('get-local', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getLocal(Symbol.for('foo')), 'bar');
  });
  it('get-type', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.getType(Symbol.for('foo')), 'variable');
  });
  it('has', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.has(Symbol.for('foo')), true);
  });
  xit('has-local', function (): any {
    const env: any = new LispEnvironment([
      [Symbol.for('foo'), 'bar', 'variable'],
    ]);
    return assertEqual(env.hasLocal(Symbol.for('foo')), true);
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
  it('get-value', function (): any {
    const env: any = new EnvironmentStack(
      new LispEnvironment([[Symbol.for('foo'), 'bar', 'variable']])
    );
    return assertEqual(env.getValue(Symbol.for('foo')), 'bar');
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
  it('get-value', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(env.getValue(Symbol.for('foo')), Symbol.for('baz'));
  });
  return it('get-typed-value', function (): any {
    const env: any = new EnvironmentPipe(
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']]),
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']])
    );
    return assertEqual(env.getTypedValue(Symbol.for('foo')), [
      Symbol.for('baz'),
      'variable',
    ]);
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
  it('get-value', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(env.getValue(Symbol.for('foo')), Symbol.for('baz'));
  });
  return it('get-typed-value', function (): any {
    const env: any = new EnvironmentComposition(
      new LispEnvironment([[Symbol.for('bar'), Symbol.for('baz'), 'variable']]),
      new LispEnvironment([[Symbol.for('foo'), Symbol.for('bar'), 'variable']])
    );
    return assertEqual(env.getTypedValue(Symbol.for('foo')), [
      Symbol.for('baz'),
      'variable',
    ]);
  });
});

describe('ThunkedEnvironment', function (): any {
  return it('get', function (): any {
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
});

describe('JavaScriptEnvironment', function (): any {
  it('get', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.get(Symbol.for('Map')), Map);
  });
  xit('get-local', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.getLocal(Symbol.for('Map')), Map);
  });
  it('has', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.has(Symbol.for('Map')), true);
  });
  return xit('has-local', function (): any {
    const env: any = new JavaScriptEnvironment();
    return assertEqual(env.hasLocal(Symbol.for('Map')), true);
  });
});
