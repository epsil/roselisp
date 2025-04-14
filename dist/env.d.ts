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
/**
 * Environment.
 *
 * In its simplest form, an environment is a mapping from keys to
 * values, with a pointer to an (optional) parent environment.
 * Subclasses of this class, like {@link TypedEnvironment}, might add
 * additional semantics, but the core idea is the same.
 */
declare class Environment {
    /**
     * The table of bindings.
     */
    table: any;
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
    constructor(entries?: any, parent?: any);
    /**
     * Add a list of entries `((key value) ...)` to the environment.
     */
    addEntries(entries: any): any;
    /**
     * Clone the environment.
     *
     * Creates a new environment object with its own table.
     * The `parent` pointer, however, points to the same
     * environment as the original environment.
     */
    clone(): any;
    /**
     * Clone the environment deeply.
     *
     * Like `.clone()`, but the `parent` environment is
     * cloned too.
     */
    cloneDeep(): any;
    /**
     * Combine two environments.
     *
     * Returns a new environment.
     */
    combine(env: any): any;
    /**
     * Combine one environment into another.
     *
     * Changes the current environment.
     */
    combineInto(env: any): any;
    /**
     * Return a list of table entries.
     * Each entry is a tuple `(key value)`.
     */
    entries(): any;
    /**
     * Find an environment frame binding `key`.
     * Returns `not-found` if not found.
     */
    findFrame(key: any, options?: any): any;
    /**
     * Delete the binding for `key`, if any.
     */
    delete(key: any): any;
    /**
     * Delete the local binding for `key`, if any.
     */
    deleteLocal(key: any): any;
    /**
     * Get the value of `key`, or `undefined`
     * if there is no binding.
     */
    get(key: any, notFound?: any): any;
    /**
     * Get the entry of `key`, which is a list `(key binding)`.
     * If there is no binding, `not-found` is returned.
     */
    getEntry(key: any, notFound?: any): any;
    /**
     * Get the environment frames of an environment.
     *
     * Returns a list of `Environment` objects that are accessible by
     * following the `parent` pointer. The list also includes the
     * current environment, which is the first element in the list.
     */
    getFrames(options?: any): any;
    /**
     * Get the binding for `key` as a tuple `(value found)`.
     */
    getTuple(key: any): any;
    /**
     * Get the binding defined by the current environment frame,
     * if any.
     */
    getLocal(key: any, notFound?: any): any;
    /**
     * Get the binding defined by the current environment frame,
     * if any, as a tuple `(binding found)`.
     */
    getLocalTuple(key: any): any;
    /**
     * Get the parent environment, if any.
     */
    getParent(): any;
    /**
     * Get the value of `key`, or `not-found`
     * if there is no binding.
     */
    getValue(key: any, notFound?: any): any;
    /**
     * Whether `key` is bound in the environment,
     * or in a parent environment.
     */
    has(key: any): any;
    /**
     * Whether `key` is bound in the current environment frame.
     */
    hasLocal(key: any): any;
    /**
     * Map a function over the environment.
     */
    map(f: any): any;
    /**
     * Set `key` to `value` in the environment.
     */
    set(key: any, value: any): any;
    /**
     * Add an entry to the current environment frame.
     */
    setEntry(entry: any): any;
    /**
     * Set `key` to `value` in the current environment frame.
     */
    setLocal(key: any, value: any): any;
    /**
     * Set the parent environment.
     */
    setParent(parent: any): any;
    /**
     * Set `key` to `value` in the environment.
     */
    setValue(key: any, value: any): any;
}
/**
 * Typed environment.
 *
 * Each value is tagged with a "type" like `'variable'`
 * or `'macro'`.
 */
declare class TypedEnvironment extends Environment {
    /**
     * Get the binding defined by the current environment frame,
     * if any.
     */
    get(key: any, notFound?: any): any;
    /**
     * Get the local binding defined by the current environment frame,
     * if any.
     */
    getLocal(key: any, notFound?: any): any;
    /**
     * Get the type of `key`. If there is no binding,
     * return `"undefined"`.
     */
    getType(key: any): any;
    /**
     * Get the typed value of `key`, which is a tuple
     * `(value type)`. If there is no binding, return
     * `not-found`.
     */
    getTypedValue(key: any, notFound?: any): any;
    getTypedLocalValue(key: any, notFound?: any): any;
    /**
     * Get the untyped value of `key`. If there is no binding,
     * return `not-found`.
     */
    getUntypedValue(key: any, notFound?: any): any;
    /**
     * Get the untyped local value of `key`. If there is no binding,
     * return `not-found`.
     */
    getUntypedLocalValue(key: any, notFound?: any): any;
    /**
     * Set `key` to `value` with type `type` in the environment.
     */
    set(key: any, value: any, type?: any): any;
    /**
     * Add an entry `(key value type)` or `(key (value type))`
     * to the environment.
     */
    setEntry(entry: any): any;
    /**
     * Set `key` to `value` with type `type` in
     * the current environment frame.
     */
    setLocal(key: any, value: any, type?: any): any;
    /**
     * Set `key` to `value` with type `type` in
     * the current environment frame.
     */
    setTypedValue(key: any, value: any, type?: any): any;
    /**
     * Set `key` to `value` with type `type` in
     * the current environment frame.
     */
    setValue(key: any, value: any, type?: any): any;
}
/**
 * Thunked environment.
 *
 * A typed environment storing thunks that are forced upon request.
 */
declare class ThunkedEnvironment extends TypedEnvironment {
    /**
     * Get the binding defined by the current environment frame,
     * if any, as a tuple `(binding found)`.
     */
    getLocalTuple(key: any): any;
    getUnforcedTuple(key: any): any;
    getUnforcedLocalTuple(key: any): any;
    /**
     * Get the type of `key`. If there is no binding,
     * return `"undefined"`.
     */
    getType(key: any): any;
}
/**
 * Lisp environment.
 *
 * A typed, thunked environment.
 */
declare class LispEnvironment extends ThunkedEnvironment {
}
/**
 * Environment class for stacking environments.
 *
 * Each stacked environment is treated like a frame in the containing
 * environment. The environment at the top of the stack---i.e., the
 * first element of the underlying array---is tried first, with the
 * other environments serving as parent environments.
 */
declare class EnvironmentStack extends TypedEnvironment {
    /**
     * Environment stack.
     *
     * `stack[0]` is the top of the stack, i.e., the environment
     * that is tried first.
     */
    stack: any;
    /**
     * Create an environment stack.
     *
     * `args` is a list of environments. `args[0]` becomes the top of
     * the stack, i.e., the environment that is tried first.
     */
    constructor(...args: any[]);
    /**
     * Clone the environment stack.
     */
    clone(): any;
    /**
     * Find an environment frame binding `key`.
     * Returns `not-found` if not found.
     */
    findFrame(key: any, options?: any): any;
    /**
     * Find a local environment frame binding `key`.
     * Returns `not-found` if not found.
     */
    findLocalFrame(key: any, options?: any): any;
    /**
     * Get all the environment frames of all the environments
     * on the stack.
     */
    getFrames(options?: any): any;
    /**
     * Get the binding for `key` as a tuple `(value found)`.
     */
    getTuple(key: any): any;
    /**
     * Get the local binding for `key` as a tuple `(value found)`.
     */
    getLocalTuple(key: any): any;
    /**
     * Whether the stack contains an environment that binds `key`.
     */
    hasLocal(key: any): any;
    /**
     * Whether the environment stack contains a
     * particular environment.
     */
    hasEnvironment(env: any): any;
    /**
     * Map a function over the environment stack.
     */
    map(f: any): any;
    /**
     * Set `key` to `value` in the first
     * environment in the stack.
     */
    setLocal(key: any, value: any, type?: any): any;
}
/**
 * Compose environments left-to-right.
 */
declare class EnvironmentPipe extends TypedEnvironment {
    environments: any;
    /**
     * Create an environment pipe.
     */
    constructor(...args: any[]);
    /**
     * Get the binding for `key` as a tuple `(value found)`.
     */
    getTuple(key: any): any;
    /**
     * Get the local binding for `key` as a tuple `(value found)`.
     */
    getLocalTuple(key: any): any;
    has(key: any): any;
    hasLocal(key: any): any;
}
/**
 * Compose environments right-to-left.
 */
declare class EnvironmentComposition extends EnvironmentPipe {
    /**
     * Create an environment composition.
     */
    constructor(...args: any[]);
}
/**
 * Dynamic environment, retrieving values
 * on the basis of some lookup function.
 */
declare class DynamicEnvironment extends TypedEnvironment {
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
    constructor(lookupF: any, typingF?: any);
    /**
     * Get the binding defined by the dynamic environment,
     * if any, as a tuple `(binding found)`.
     */
    getLocalTuple(key: any): any;
    /**
     * Whether `key` is bound by the dynamic environment.
     */
    hasLocal(key: any): any;
}
/**
 * JavaScript environment.
 *
 * A dynamic environment that looks up JavaScript values.
 */
declare class JavaScriptEnvironment extends DynamicEnvironment {
    constructor();
}
/**
 * Pointer to the current environment.
 * Used by {@link currentEnvironment}.
 */
declare let currentEnvironmentPointer: any;
/**
 * Return the current environment.
 */
declare function currentEnvironment_(): any;
declare namespace currentEnvironment_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return an empty environment.
 */
declare function emptyEnvironment(): any;
declare namespace emptyEnvironment {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the default environment.
 *
 * The default environment is defined as follows: use
 * the current environment if there is one, and if not,
 * use the empty environment.
 */
declare function defaultEnvironment(): any;
declare namespace defaultEnvironment {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
/**
 * Run `f` with `currentEnvironmentPointer` bound to `env`.
 * This makes the current environment available through the
 * function {@link currentEnvironment}. The original value
 * of `currentEnvironmentPointer` is restored afterwards.
 */
declare function withEnvironment(env: any, f: any): any;
declare namespace withEnvironment {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Make an environment.
 */
declare function makeEnvironment(variables?: any, parent?: any, isLisp2?: any): any;
declare namespace makeEnvironment {
    var lispSource: (symbol | (symbol | symbol[])[])[];
}
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
declare function extendEnvironment(env: any, parent: any): any;
declare namespace extendEnvironment {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[] | (symbol | symbol[])[][])[])[];
}
/**
 * Return an array of the frames in an environment.
 */
declare function environmentFrames(env: any): any;
declare namespace environmentFrames {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
/**
 * Convert an array of environment frames to a linked list
 * by modifying each frame's `parent` property to point to
 * the next frame in the array.
 */
declare function linkEnvironmentFrames(frames: any): any;
declare namespace linkEnvironmentFrames {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[] | (symbol | (number | symbol | (number | symbol | symbol[])[])[])[][])[])[];
}
/**
 * Prefix a set of bindings.
 */
declare function prefixBindings(prefix: any, bindings: any): any;
declare namespace prefixBindings {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
}
export { currentEnvironment_ as currentEnvironment, DynamicEnvironment, Environment, EnvironmentComposition, EnvironmentPipe, EnvironmentStack, JavaScriptEnvironment, LispEnvironment, ThunkedEnvironment, TypedEnvironment, currentEnvironmentPointer, currentEnvironment_, defaultEnvironment, emptyEnvironment, environmentFrames, extendEnvironment, linkEnvironmentFrames, makeEnvironment, prefixBindings, withEnvironment };
