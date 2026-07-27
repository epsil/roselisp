// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Environment
 *
 * Environment class.
 *
 * ## Description
 *
 * This file defines the `Environment` class used for implementing
 * the language environment.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

import {
  lookupJsValue
} from './lookup';

import {
  force,
  Thunk,
  thunkp
} from './thunk';

/**
 * Environment.
 *
 * In its simplest form, an environment is a mapping from keys to
 * values, with a pointer to an (optional) parent environment.
 * Subclasses of this class, like {@link TypedEnvironment}, might add
 * additional semantics, but the core idea is the same.
 */
class Environment {
  /**
   * The table of bindings.
   */
  table: any = new Map();

  /**
   * The parent environment, if any.
   */
  parent: any;

  /**
   * Create a new environment.
   *
   * The environment may be initialized with `entries`,
   * which is a list of `(key value)` tuples. The
   * parent environment may be specified with `parent`.
   */
  constructor(entries: any = [], parent: any = undefined) {
    this.addEntriesX(entries);
    this.parent = parent;
  }

  /**
   * Add a list of entries `((key value) ...)` to the environment.
   */
  addEntriesX(entries: any): any {
    for (let entry of entries) {
      this.setEntryX(entry);
    }
    return this;
  }

  /**
   * Clone the environment.
   *
   * Creates a new environment object with its own table.
   * The `parent` pointer, however, points to the same
   * environment as the original environment.
   */
  clone(): any {
    let env: any = new (this.constructor as any)();
    env.table = new Map(this.table);
    env.parent = this.parent;
    return env;
  }

  /**
   * Clone the environment deeply.
   *
   * Like `.clone()`, but the `parent` environment is
   * cloned too.
   */
  cloneDeep(): any {
    let env: any = this.clone();
    const parent: any = env.parent;
    if (parent) {
      env.parent = parent.cloneDeep();
    }
    return env;
  }

  /**
   * Combine two environments.
   *
   * Returns a new environment.
   */
  combine(env: any): any {
    // FIXME: Does not support combining an untyped environment
    // with a typed one.
    const entries: any = [...this.entries(), ...env.entries()];
    let result: any = new (this.constructor as any)(entries, this.parent);
    return result;
  }

  /**
   * Combine one environment into another.
   *
   * Changes the current environment.
   */
  combineInto(env: any): any {
    this.addEntriesX(env.entries());
    return this;
  }

  /**
   * Return a list of table entries.
   * Each entry is a tuple `(key value)`.
   */
  entries(): any {
    return [...this.table.entries()];
  }

  /**
   * Find an environment frame binding `key`.
   */
  findFrame(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const filter: any = options['filter'];
    const parent: any = this.parent;
    if (filter && !filter(this)) {
      return notFound;
    } else if (this.hasLocalP(key)) {
      return this;
    } else if (parent) {
      return parent.findFrame(key, options);
    } else {
      return notFound;
    }
  }

  /**
   * Delete the binding for `key`, if any.
   */
  deletex(key: any): any {
    let env: any = this.findFrame(key);
    if (env) {
      env.deleteLocalX(key);
    }
    return this;
  }

  /**
   * Delete the local binding for `key`, if any.
   */
  deleteLocalX(key: any): any {
    this.table.delete(key);
    return this;
  }

  /**
   * Get the value of `key`, or `#u`
   * if there is no binding.
   */
  get(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const inheritedOptions: any = {
      ...options,
      notFound: false
    };
    let [value, found]: any[] = this.getTuple(key, inheritedOptions);
    if (found) {
      return value;
    } else {
      return notFound;
    }
  }

  /**
   * Get the entry of `key`, which is a list `(key binding)`.
   */
  getEntry(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const inheritedOptions: any = {
      ...options,
      notFound: false
    };
    let [value, found]: any[] = this.getTuple(key, options);
    if (found) {
      return [key, value];
    } else {
      return notFound;
    }
  }

  /**
   * Get the environment frames of an environment.
   *
   * Returns a list of `Environment` objects that are accessible by
   * following the `parent` pointer. The list also includes the
   * current environment, which is the first element in the list.
   */
  getFrames(options: any = {}): any {
    const filter: any = options['filter'];
    const offset: any = options['offset'] || 0;
    let frames: any = [];
    let env: any = this;
    while (env && !frames.includes(env) && !(filter && !filter(env))) {
      frames.push(env);
      env = env.parent;
    }
    if (offset === 0) {
      return frames;
    } else {
      return frames.slice(offset);
    }
  }

  /**
   * Get the binding for `key` as a tuple `(value found)`.
   */
  getTuple(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const inheritedOptions: any = {
      ...options,
      notFound: false
    };
    let env: any = this.findFrame(key, inheritedOptions);
    if (env) {
      return env.getLocalTuple(key, options);
    } else {
      return [notFound, false];
    }
  }

  /**
   * Get the binding defined by the current environment frame,
   * if any.
   */
  getLocal(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const inheritedOptions: any = {
      ...options,
      notFound: false
    };
    let [value, found]: any[] = this.getLocalTuple(key, inheritedOptions);
    if (found) {
      return value;
    } else {
      return notFound;
    }
  }

  /**
   * Get the binding defined by the current environment frame,
   * if any, as a tuple `(binding found)`.
   */
  getLocalTuple(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const filter: any = options['filter'];
    if (filter && !filter(this)) {
      return [notFound, false];
    }
    let found: any = this.hasLocalP(key);
    const value: any = found ? this.table.get(key) : notFound;
    return [value, found];
  }

  /**
   * Get the parent environment, if any.
   */
  getParent(): any {
    return this.parent;
  }

  /**
   * Get the value of `key`.
   */
  getValue(key: any, options: any = {}): any {
    // Alias for `.get`.
    return this.get(key, options);
  }

  /**
   * Whether `key` is bound in the environment,
   * or in a parent environment.
   */
  hasp(key: any, options: any = {}): any {
    let env: any = this.findFrame(key, options);
    if (env) {
      return true;
    } else {
      return false;
    }
  }

  /**
   * Whether `key` is bound in the current environment frame.
   */
  hasLocalP(key: any, options: any = {}): any {
    const filter: any = options['filter'];
    if (filter && !filter(this)) {
      return false;
    } else {
      return this.table.has(key);
    }
  }

  /**
   * Map a function over the environment.
   */
  map(f: any): any {
    return new (this.constructor as any)(this.entries().map(f), this.parent);
  }

  /**
   * Set `key` to `value` in the environment.
   */
  setx(key: any, value: any): any {
    let env: any = this.findFrame(key, {
      notFound: this
    });
    return env.setLocalX(key, value);
  }

  /**
   * Add an entry to the current environment frame.
   */
  setEntryX(entry: any): any {
    let [key, binding]: any[] = entry;
    return this.setLocalX(key, binding);
  }

  /**
   * Set `key` to `value` in the current environment frame.
   */
  setLocalX(key: any, value: any): any {
    const table: any = this.table;
    table.set(key, value);
    return this;
  }

  /**
   * Set the parent environment.
   */
  setParentX(parent: any): any {
    this.parent = parent;
    return this;
  }

  /**
   * Set `key` to `value` in the environment.
   */
  setValueX(key: any, value: any): any {
    // Alias for `.set`.
    return this.setx(key, value);
  }
}

/**
 * Typed environment.
 *
 * Each value is tagged with a "type" like `'variable'`
 * or `'macro'`.
 */
class TypedEnvironment extends Environment {
  /**
   * Get the binding defined by the current environment frame,
   * if any.
   */
  get(key: any, options: any = {}): any {
    return this.getUntypedValue(key, options);
  }

  /**
   * Get the local binding defined by the current environment frame,
   * if any.
   */
  getLocal(key: any, options: any = {}): any {
    return this.getUntypedLocalValue(key, options);
  }

  /**
   * Get the type of `key`. If there is no binding,
   * return `Undefined`.
   */
  getType(key: any, options: any = {}): any {
    const notFound: any = options['notFound'] || Symbol.for('Undefined');
    const inheritedOptions: any = {
      ...options,
      notFound: [undefined, notFound]
    };
    const [, typ]: any[] = this.getTypedValue(key, inheritedOptions);
    return typ;
  }

  /**
   * Get the local type of `key`. If there is no binding,
   * return `Undefined`.
   */
  getLocalType(key: any, options: any = {}): any {
    const notFound: any = options['notFound'] || Symbol.for('Undefined');
    const inheritedOptions: any = {
      ...options,
      notFound: [undefined, notFound]
    };
    const [, typ]: any[] = this.getTypedLocalValue(key, inheritedOptions);
    return typ;
  }

  /**
   * Get the typed value of `key`, which is a tuple
   * `(value type)`.
   */
  getTypedValue(key: any, options: any = {
    notFound: [undefined, Symbol.for('Undefined')]
  }): any {
    // The same as `super.get`, except that
    // `notFound` defaults to `(#u Undefined)`.
    return super.get(key, {
      ...options,
      notFound: options['notFound'] || [undefined, Symbol.for('Undefined')]
    });
  }

  getTypedLocalValue(key: any, options: any = {
    notFound: [undefined, Symbol.for('Undefined')]
  }): any {
    // The same as `super.get-local`, except that
    // `not-found` defaults to `(#u Undefined)`.
    return super.getLocal(key, {
      ...options,
      notFound: options['notFound'] || [undefined, Symbol.for('Undefined')]
    });
  }

  /**
   * Get the untyped value of `key`.
   */
  getUntypedValue(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const inheritedOptions: any = {
      ...options,
      notFound: [undefined, Symbol.for('Undefined')]
    };
    const [value, typ]: any[] = this.getTypedValue(key, inheritedOptions);
    if (typ === Symbol.for('Undefined')) {
      return notFound;
    } else {
      return value;
    }
  }

  /**
   * Get the untyped local value of `key`.
   */
  getUntypedLocalValue(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const inheritedOptions: any = {
      ...options,
      notFound: [undefined, Symbol.for('Undefined')]
    };
    const [value, typ]: any[] = this.getTypedLocalValue(key, inheritedOptions);
    if (typ === Symbol.for('Undefined')) {
      return notFound;
    } else {
      return value;
    }
  }

  /**
   * Set `key` to `value` with type `type` in the environment.
   */
  setx(key: any, value: any, type: any = Symbol.for('Any')): any {
    // Alias for `.set-typed-value`.
    return this.setTypedValueX(key, value, type);
  }

  /**
   * Add an entry `(key value type)` or `(key (value type))`
   * to the environment.
   */
  setEntryX(entry: any): any {
    if (entry.length === 3) {
      const [key, value, type]: any[] = entry;
      this.setLocalX(key, value, type);
    } else {
      let [key, binding]: any[] = entry;
      const [value, type]: any[] = binding;
      this.setLocalX(key, value, type);
    }
    return this;
  }

  /**
   * Set `key` to `value` with type `type` in
   * the current environment frame.
   */
  setLocalX(key: any, value: any, type: any = Symbol.for('Any')): any {
    return super.setLocalX(key, [value, type]);
  }

  /**
   * Set the type of `key` to `typ`.
   * If there is no existing binding,
   * creates a new binding where the value is `#u`.
   */
  setTypeX(key: any, typ: any, options: any = {}): any {
    let val: any = this.get(key, options);
    return this.setx(key, val, typ);
  }

  /**
   * Set the local type of `key` to `typ`.
   * If there is no existing local binding,
   * creates a new binding where the value is `#u`.
   */
  setLocalTypeX(key: any, typ: any, options: any = {}): any {
    let val: any = this.getLocal(key, options);
    return this.setLocalX(key, val, typ);
  }

  /**
   * Set `key` to `value` with type `type` in
   * the current environment frame.
   */
  setTypedValueX(key: any, value: any, type: any = Symbol.for('Any')): any {
    let env: any = this.findFrame(key, {
      notFound: this
    });
    if (env instanceof TypedEnvironment) {
      return env.setLocalX(key, value, type);
    } else {
      return key.setLocalX(value);
    }
  }

  /**
   * Set `key` to `value` with type `type` in
   * the current environment frame.
   */
  setValueX(key: any, value: any, type: any = Symbol.for('Any')): any {
    // Alias for `.set`.
    return this.setx(key, value, type);
  }
}

/**
 * Thunked environment.
 *
 * A typed environment storing thunks that are forced upon request.
 */
class ThunkedEnvironment extends TypedEnvironment {
  /**
   * Get the binding defined by the current environment frame,
   * if any, as a tuple `(binding found)`.
   */
  getLocalTuple(key: any, options: any = {}): any {
    let tuple: any = this.getUnforcedLocalTuple(key, options);
    let [binding, found]: any[] = tuple;
    if (found) {
      let [val, typ]: any[] = binding;
      if (thunkp(val)) {
        val = force(val);
        this.setLocalX(key, val, typ);
        binding = [val, typ];
        tuple = [binding, found];
      }
    }
    return tuple;
  }

  /**
   * Like `get-tuple`, but does not force any thunks.
   */
  getUnforcedTuple(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const filter: any = options['filter'];
    if (filter && !filter(this)) {
      return [notFound, false];
    } else if (this.hasLocalP(key, options)) {
      return this.getUnforcedLocalTuple(key, options);
    } else {
      const inheritedOptions: any = {
        ...options,
        offset: 1
      };
      for (let frame of this.getFrames(inheritedOptions)) {
        if (frame.hasLocalP(key, options)) {
          return (frame instanceof ThunkedEnvironment) ? frame.getUnforcedLocalTuple(key, options) : frame.getLocalTuple(key, options);
        }
      }
      return [notFound, false];
    }
  }

  /**
   * Like `get-local-tuple`, but does not force any thunks.
   */
  getUnforcedLocalTuple(key: any, options: any = {}): any {
    return super.getLocalTuple(key, options);
  }

  /**
   * Get the type of `key`. If there is no binding,
   * return `Undefined`.
   */
  getType(key: any, options: any = {}): any {
    // Obtain the type without forcing the thunk.
    const notFound: any = options['notFound'] || Symbol.for('Undefined');
    let tuple: any = this.getUnforcedTuple(key, options);
    let [binding, found]: any[] = tuple;
    if (found) {
      const [, typ]: any[] = binding;
      return typ;
    } else {
      return notFound;
    }
  }

  /**
   * Get the local type of `key`. If there is no binding,
   * return `Undefined`.
   */
  getLocalType(key: any, options: any = {}): any {
    // Obtain the type without forcing the thunk.
    const notFound: any = options['notFound'] || Symbol.for('Undefined');
    let tuple: any = this.getUnforcedLocalTuple(key, options);
    let [binding, found]: any[] = tuple;
    if (found) {
      const [, typ]: any[] = binding;
      return typ;
    } else {
      return notFound;
    }
  }

  /**
   * Whether `key` is bound to a thunk.
   */
  hasThunkP(key: any, options: any = {}): any {
    // Obtain the type without forcing the thunk.
    let tuple: any = this.getUnforcedTuple(key, options);
    let [binding, found]: any[] = tuple;
    if (found) {
      let [val]: any[] = binding;
      return thunkp(val);
    } else {
      return false;
    }
  }

  /**
   * Whether `key` is locally bound to a thunk.
   */
  hasLocalThunkP(key: any, options: any = {}): any {
    // Obtain the type without forcing the thunk.
    let tuple: any = this.getUnforcedLocalTuple(key, options);
    let [binding, found]: any[] = tuple;
    if (found) {
      let [val]: any[] = binding;
      return thunkp(val);
    } else {
      return false;
    }
  }

  /**
   * Set the type of `key` to `typ`.
   * Does not force any thunks.
   */
  setTypeX(key: any, typ: any, options: any = {}): any {
    const inheritedOptions: any = {
      ...options,
      notFound: [undefined, Symbol.for('Undefined')]
    };
    // Obtain the type without forcing the thunk.
    let tuple: any = this.getUnforcedTuple(key, inheritedOptions);
    let [binding]: any[] = tuple;
    let [val]: any[] = binding;
    return this.setx(key, val, typ);
  }

  /**
   * Set the local type of `key` to `typ`.
   * Does not force any thunks.
   */
  setLocalTypeX(key: any, typ: any, options: any = {}): any {
    const inheritedOptions: any = {
      ...options,
      notFound: [undefined, Symbol.for('Undefined')]
    };
    // Obtain the type without forcing the thunk.
    let tuple: any = this.getUnforcedLocalTuple(key, inheritedOptions);
    let [binding]: any[] = tuple;
    let [val]: any[] = binding;
    return this.setLocalX(key, val, typ);
  }
}

/**
 * Lisp environment.
 *
 * A typed, thunked environment.
 */
class LispEnvironment extends ThunkedEnvironment {
}

/**
 * Environment class for stacking environments.
 *
 * Each stacked environment is treated like a frame in the containing
 * environment. The environment at the top of the stack---i.e., the
 * first element of the underlying array---is tried first, with the
 * other environments serving as parent environments.
 */
class EnvironmentStack extends TypedEnvironment {
  /**
   * Environment stack.
   *
   * `stack[0]` is the top of the stack, i.e., the environment
   * that is tried first.
   */
  stack: any = [];

  /**
   * Create an environment stack.
   *
   * `args` is a list of environments. `args[0]` becomes the top of
   * the stack, i.e., the environment that is tried first.
   */
  constructor(...args: any[]) {
    super();
    this.stack = args;
  }

  /**
   * Clone the environment stack.
   */
  clone(): any {
    let env: any = super.clone();
    env.stack = [...this.stack];
    return env;
  }

  /**
   * Find an environment frame binding `key`.
   */
  findFrame(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const inheritedOptions: any = {
      ...options,
      notFound: false
    };
    let env: any = this.findLocalFrame(key, inheritedOptions);
    if (!env) {
      const parent: any = this.parent;
      if (parent) {
        env = parent.findFrame(key, inheritedOptions);
      }
    }
    if (env) {
      return env;
    } else {
      return notFound;
    }
  }

  /**
   * Find a local environment frame binding `key`.
   */
  findLocalFrame(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const filter: any = options['filter'];
    if (filter && !filter(this)) {
      return notFound;
    }
    const inheritedOptions: any = {
      ...options,
      notFound: false
    };
    let result: any = notFound;
    const environments: any = this.stack;
    for (let env of environments) {
      if (filter && !filter(env)) {
        break;
      }
      const frame: any = env.findFrame(key, inheritedOptions);
      if (frame) {
        result = frame;
        break;
      }
    }
    return result;
  }

  /**
   * Get all the environment frames of all the environments
   * on the stack.
   */
  getFrames(options: any = {}): any {
    const filter: any = options['filter'];
    let frames: any = [];
    const offset: any = options['offset'] || 0;
    const environments: any = this.stack;
    const parent: any = this.parent;
    const inheritedOptions: any = {
      ...options,
      offset: 0
    };
    if (parent) {
      environments.push(parent);
    }
    for (let env of environments) {
      for (let f of env.getFrames(inheritedOptions)) {
        if (!frames.includes(f)) {
          if (f instanceof EnvironmentStack) {
            frames = [...frames, ...f.getFrames(inheritedOptions)];
          } else {
            frames.push(f);
          }
        }
      }
    }
    if (offset === 0) {
      return frames;
    } else {
      return frames.slice(offset);
    }
  }

  /**
   * Get the binding for `key` as a tuple `(value found)`.
   */
  getTuple(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    let env: any = this.findFrame(key, options);
    if (env) {
      return env.getTuple(key, options);
    } else {
      return [notFound, false];
    }
  }

  /**
   * Get the local binding for `key` as a tuple `(value found)`.
   */
  getLocalTuple(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    let env: any = this.findLocalFrame(key, options);
    if (env) {
      return env.getTuple(key, options);
    } else {
      return [notFound, false];
    }
  }

  /**
   * Whether the stack contains an environment that binds `key`.
   */
  hasLocalP(key: any): any {
    // This could have been implemented in terms of
    // `find-local-frame`, but the following is faster since it
    // doesn't concern itself with the finer details of which
    // particular frame of which particular environment contains the
    // binding.
    let result: any = false;
    for (let env of this.stack) {
      if (env.hasp(key)) {
        result = true;
        break;
      }
    }
    return result;
  }

  /**
   * Whether the environment stack contains a
   * particular environment.
   */
  hasEnvironmentP(env: any): any {
    if (env === this) {
      return true;
    }
    for (let x of this.stack) {
      if (x === env) {
        return true;
      } else if ((x instanceof EnvironmentStack) && x.hasEnvironmentP(env)) {
        return true;
      }
    }
    if (this.parent && (this.parent instanceof EnvironmentStack)) {
      return this.parent.hasEnvironmentP(env);
    } else {
      return false;
    }
  }

  /**
   * Map a function over the environment stack.
   */
  map(f: any): any {
    return new EnvironmentStack(...this.stack.map(function (env: any): any {
      return env.map(f);
    }));
  }

  /**
   * Set `key` to `value` in the first
   * environment in the stack.
   */
  setLocalX(key: any, value: any, type: any = Symbol.for('Any')): any {
    let env: any = this.stack[0];
    if (env) {
      if (env instanceof TypedEnvironment) {
        env.setLocalX(key, value, type);
      } else {
        key.setLocalX(value);
      }
    }
    return this;
  }
}

/**
 * Compose environments left-to-right.
 */
class EnvironmentPipe extends TypedEnvironment {
  environments: any = [];

  /**
   * Create an environment pipe.
   */
  constructor(...args: any[]) {
    super();
    this.environments = args;
  }

  /**
   * Get the binding for `key` as a tuple `(value found)`.
   */
  getTuple(key: any, options: any = {}): any {
    return this.getLocalTuple(key, options);
  }

  /**
   * Get the local binding for `key` as a tuple `(value found)`.
   */
  getLocalTuple(key: any, options: any = {}): any {
    const notFound: any = options['filter'];
    const filter: any = options['filter'];
    if (filter && !filter(this)) {
      return [notFound, false];
    }
    let currentKey: any = key;
    let lastKey: any = key;
    let lastEnv: any;
    let found: any = true;
    for (let env of this.environments) {
      if (filter && !filter(env)) {
        break;
      }
      if (env.hasp(currentKey)) {
        lastEnv = env;
        lastKey = currentKey;
        currentKey = env.get(currentKey);
      } else {
        found = false;
        break;
      }
    }
    if (found && lastEnv) {
      return lastEnv.getTuple(lastKey);
    } else if (this.parent) {
      return this.parent.getTuple(currentKey);
    } else {
      return [notFound, false];
    }
  }

  /**
   * Whether `key` is bound in the environment,
   * or in a parent environment.
   */
  hasp(key: any, options: any = {}): any {
    let [value, found]: any[] = this.getTuple(key, options);
    return found;
  }

  /**
   * Whether `key` is bound in the current environment frame.
   */
  hasLocalP(key: any, options: any = {}): any {
    let [value, found]: any[] = this.getLocalTuple(key, options);
    return found;
  }
}

/**
 * Compose environments right-to-left.
 */
class EnvironmentComposition extends EnvironmentPipe {
  /**
   * Create an environment composition.
   */
  constructor(...args: any[]) {
    super(...args.reverse());
  }
}

/**
 * Dynamic environment, retrieving values
 * on the basis of some lookup function.
 */
class DynamicEnvironment extends TypedEnvironment {
  /**
   * Lookup function.
   */
  lookupF: any;

  /**
   * Typing function.
   */
  typingF: any;

  /**
   * Create a dynamic environment.
   */
  constructor(lookupF: any, typingF: any = function (...args: any[]): any {
    return Symbol.for('Any');
  }) {
    super();
    this.lookupF = lookupF;
    this.typingF = typingF;
  }

  /**
   * Get the binding defined by the dynamic environment,
   * if any, as a tuple `(binding found)`.
   */
  getLocalTuple(key: any, options: any = {}): any {
    const notFound: any = options['notFound'];
    const filter: any = options['filter'];
    if (filter && !filter(this)) {
      return [notFound, false];
    } else {
      let [value, found]: any[] = this.lookupF(key);
      if (found) {
        return [[value, this.typingF(value)], true];
      } else {
        return [notFound, false];
      }
    }
  }

  /**
   * Whether `key` is bound by the dynamic environment.
   */
  hasLocalP(key: any, options: any = {}): any {
    let [, found]: any[] = this.getLocalTuple(key, options);
    return found;
  }
}

/**
 * JavaScript environment.
 *
 * A dynamic environment that looks up JavaScript values.
 */
class JavaScriptEnvironment extends DynamicEnvironment {
  constructor() {
    super(lookupJsValue);
  }
}

/**
 * Pointer to the current environment.
 * Used by {@link currentEnvironment}.
 */
let currentEnvironmentPointer: any = undefined;

/**
 * Return the current environment.
 */
function currentEnvironment_(): any {
  return currentEnvironmentPointer;
}

currentEnvironment_.fsource = [Symbol.for('define'), [Symbol.for('current-environment_')], Symbol.for('current-environment-pointer')];

/**
 * Return an empty environment.
 */
function emptyEnvironment(): any {
  return new LispEnvironment();
}

emptyEnvironment.fsource = [Symbol.for('define'), [Symbol.for('empty-environment')], [Symbol.for('new'), Symbol.for('LispEnvironment')]];

/**
 * Return the default environment.
 *
 * The default environment is defined as follows: use
 * the current environment if there is one, and if not,
 * use the empty environment.
 */
function defaultEnvironment(): any {
  return currentEnvironment_() || emptyEnvironment();
}

defaultEnvironment.fsource = [Symbol.for('define'), [Symbol.for('default-environment')], [Symbol.for('or'), [Symbol.for('current-environment_')], [Symbol.for('empty-environment')]]];

/**
 * Run `f` with `currentEnvironmentPointer` bound to `env`.
 * This makes the current environment available through the
 * function {@link currentEnvironment}. The original value
 * of `currentEnvironmentPointer` is restored afterwards.
 */
function withEnvironment(env: any, f: any): any {
  // TODO: It would be much faster to implement this as
  // a macro.
  let result: any = undefined;
  const tmp: any = currentEnvironmentPointer;
  try {
    currentEnvironmentPointer = env;
    result = f();
  } finally {
    currentEnvironmentPointer = tmp;
  }
  return result;
}

withEnvironment.fsource = [Symbol.for('define'), [Symbol.for('with-environment'), Symbol.for('env'), Symbol.for('f')], [Symbol.for('define'), Symbol.for('result'), undefined], [Symbol.for('define'), Symbol.for('tmp'), Symbol.for('current-environment-pointer')], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('current-environment-pointer'), Symbol.for('env')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f')]], [Symbol.for('finally'), [Symbol.for('set!'), Symbol.for('current-environment-pointer'), Symbol.for('tmp')]]], Symbol.for('result')];

/**
 * Make an environment.
 */
function makeEnvironment(variables: any = undefined, parent: any = undefined, isLisp2: any = false): any {
  return new LispEnvironment(variables, parent);
}

makeEnvironment.fsource = [Symbol.for('define'), [Symbol.for('make-environment'), [Symbol.for('variables'), undefined], [Symbol.for('parent'), undefined], [Symbol.for('is-lisp-2'), false]], [Symbol.for('new'), Symbol.for('LispEnvironment'), Symbol.for('variables'), Symbol.for('parent')]];

/**
 * Extend the environment `env` with `parent` as its parent
 * environment. This means that if a variable, function or macro
 * is not defined in `env`, it is looked up in `parent` instead.
 *
 * If `env` already has a parent environment, `parent` becomes the
 * parent of that environment.  An environment is effectively a
 * linked list of environment frames. This function simply creates
 * a linked list containing of the frames of both environments,
 * in such a way that no frame is listed more than once.
 */
function extendEnvironment(env: any, parent: any): any {
  // Create an array where each environment frame is
  // listed only once. This prevents cycles when the array
  // is converted to a linked list, which is how we tie
  // the two environments together.
  let frames: any = [];
  for (let frame of environmentFrames(env)) {
    if (!frames.includes(frame)) {
      frames.push(frame);
    }
  }
  for (let frame of environmentFrames(parent)) {
    if (!frames.includes(frame)) {
      frames.push(frame);
    }
  }
  linkEnvironmentFrames(frames);
  return env;
}

extendEnvironment.fsource = [Symbol.for('define'), [Symbol.for('extend-environment'), Symbol.for('env'), Symbol.for('parent')], [Symbol.for('define'), Symbol.for('frames'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('frame'), [Symbol.for('environment-frames'), Symbol.for('env')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('frame'), Symbol.for('frames')], [Symbol.for('push-right!'), Symbol.for('frames'), Symbol.for('frame')]]], [Symbol.for('for'), [[Symbol.for('frame'), [Symbol.for('environment-frames'), Symbol.for('parent')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('frame'), Symbol.for('frames')], [Symbol.for('push-right!'), Symbol.for('frames'), Symbol.for('frame')]]], [Symbol.for('link-environment-frames'), Symbol.for('frames')], Symbol.for('env')];

/**
 * Return an array of the frames in an environment.
 */
function environmentFrames(env: any): any {
  let frames: any = [env];
  if (env.parent) {
    return [...frames, ...environmentFrames(env.parent)];
  } else {
    return frames;
  }
}

environmentFrames.fsource = [Symbol.for('define'), [Symbol.for('environment-frames'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('frames'), [Symbol.for('list'), Symbol.for('env')]], [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('parent'), Symbol.for('env')], [Symbol.for('append'), Symbol.for('frames'), [Symbol.for('environment-frames'), [Symbol.for('get-field'), Symbol.for('parent'), Symbol.for('env')]]], Symbol.for('frames')]];

/**
 * Convert an array of environment frames to a linked list
 * by modifying each frame's `parent` property to point to
 * the next frame in the array.
 */
function linkEnvironmentFrames(frames: any): any {
  let firstFrame: any = undefined;
  const _start: any = frames.length - 1;
  for (let i: any = _start; i > -1; i--) {
    const frame: any = (frames as any)[i];
    frame.parent = firstFrame;
    firstFrame = frame;
  }
  return firstFrame;
}

linkEnvironmentFrames.fsource = [Symbol.for('define'), [Symbol.for('link-environment-frames'), Symbol.for('frames')], [Symbol.for('define'), Symbol.for('first-frame'), undefined], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), [Symbol.for('-'), [Symbol.for('js/length'), Symbol.for('frames')], 1], -1, -1]]], [Symbol.for('define'), Symbol.for('frame'), [Symbol.for('aget'), Symbol.for('frames'), Symbol.for('i')]], [Symbol.for('set-field!'), Symbol.for('parent'), Symbol.for('frame'), Symbol.for('first-frame')], [Symbol.for('set!'), Symbol.for('first-frame'), Symbol.for('frame')]], Symbol.for('first-frame')];

/**
 * Prefix a set of bindings.
 */
function prefixBindings(prefix: any, bindings: any): any {
  function prefixBinding(binding: any): any {
    return [Symbol.for(prefix + (binding[0].description as string)), ...binding.slice(1)];
  }
  prefixBinding.fsource = [Symbol.for('define'), [Symbol.for('prefix-binding'), Symbol.for('binding')], [Symbol.for('~>'), [Symbol.for('first'), Symbol.for('binding')], [Symbol.for('symbol->string'), Symbol.for('_')], [Symbol.for('string-append'), Symbol.for('prefix'), Symbol.for('_')], [Symbol.for('string->symbol'), Symbol.for('_')], [Symbol.for('append'), [Symbol.for('list'), Symbol.for('_')], [Symbol.for('rest'), Symbol.for('binding')]]]];
  return bindings.map(function (x: any): any {
    return prefixBinding(x);
  });
}

prefixBindings.fsource = [Symbol.for('define'), [Symbol.for('prefix-bindings'), Symbol.for('prefix'), Symbol.for('bindings')], [Symbol.for('define'), [Symbol.for('prefix-binding'), Symbol.for('binding')], [Symbol.for('~>'), [Symbol.for('first'), Symbol.for('binding')], [Symbol.for('symbol->string'), Symbol.for('_')], [Symbol.for('string-append'), Symbol.for('prefix'), Symbol.for('_')], [Symbol.for('string->symbol'), Symbol.for('_')], [Symbol.for('append'), [Symbol.for('list'), Symbol.for('_')], [Symbol.for('rest'), Symbol.for('binding')]]]], [Symbol.for('map'), Symbol.for('prefix-binding'), Symbol.for('bindings')]];

export {
  currentEnvironment_ as currentEnvironment,
  DynamicEnvironment,
  Environment,
  EnvironmentComposition,
  EnvironmentPipe,
  EnvironmentStack,
  JavaScriptEnvironment,
  LispEnvironment,
  ThunkedEnvironment,
  TypedEnvironment,
  currentEnvironmentPointer,
  currentEnvironment_,
  defaultEnvironment,
  emptyEnvironment,
  environmentFrames,
  extendEnvironment,
  linkEnvironmentFrames,
  makeEnvironment,
  prefixBindings,
  withEnvironment
};