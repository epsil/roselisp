"use strict";
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.withEnvironment = exports.prefixBindings = exports.makeEnvironment = exports.linkEnvironmentFrames = exports.extendEnvironment = exports.environmentFrames = exports.emptyEnvironment = exports.defaultEnvironment = exports.currentEnvironment_ = exports.currentEnvironmentPointer = exports.TypedEnvironment = exports.ThunkedEnvironment = exports.LispEnvironment = exports.JavaScriptEnvironment = exports.EnvironmentStack = exports.EnvironmentPipe = exports.EnvironmentComposition = exports.Environment = exports.DynamicEnvironment = exports.currentEnvironment = void 0;
const lookup_1 = require("./lookup");
const thunk_1 = require("./thunk");
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
     * Create a new environment.
     *
     * The environment may be initialized with `entries`,
     * which is a list of `(key value)` tuples. The
     * parent environment may be specified with `parent`.
     */
    constructor(entries = [], parent = undefined) {
        /**
         * The table of bindings.
         */
        this.table = new Map();
        this.addEntries(entries);
        this.parent = parent;
    }
    /**
     * Add a list of entries `((key value) ...)` to the environment.
     */
    addEntries(entries) {
        for (let entry of entries) {
            this.setEntry(entry);
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
    clone() {
        let env = new this.constructor();
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
    cloneDeep() {
        let env = this.clone();
        const parent = env.parent;
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
    combine(env) {
        // FIXME: Does not support combining an untyped environment
        // with a typed one.
        const entries = [...this.entries(), ...env.entries()];
        let result = new this.constructor(entries, this.parent);
        return result;
    }
    /**
     * Combine one environment into another.
     *
     * Changes the current environment.
     */
    combineInto(env) {
        this.addEntries(env.entries());
        return this;
    }
    /**
     * Return a list of table entries.
     * Each entry is a tuple `(key value)`.
     */
    entries() {
        return [...this.table.entries()];
    }
    /**
     * Find an environment frame binding `key`.
     * Returns `not-found` if not found.
     */
    findFrame(key, options = {}) {
        const notFound = options['notFound'];
        const filter = options['filter'];
        let env = this;
        while (env && ((filter && !filter(env)) || !env.hasLocal(key))) {
            env = env.parent;
        }
        if (env) {
            return env;
        }
        else {
            return notFound;
        }
    }
    /**
     * Delete the binding for `key`, if any.
     */
    delete(key) {
        let env = this.findFrame(key);
        if (env) {
            env.deleteLocal(key);
        }
        return this;
    }
    /**
     * Delete the local binding for `key`, if any.
     */
    deleteLocal(key) {
        this.table.delete(key);
        return this;
    }
    /**
     * Get the value of `key`, or `undefined`
     * if there is no binding.
     */
    get(key, notFound = undefined) {
        let [value, found] = this.getTuple(key);
        if (found) {
            return value;
        }
        else {
            return notFound;
        }
    }
    /**
     * Get the entry of `key`, which is a list `(key binding)`.
     * If there is no binding, `not-found` is returned.
     */
    getEntry(key, notFound = undefined) {
        let [value, found] = this.getTuple(key);
        if (found) {
            return [key, value];
        }
        else {
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
    getFrames(options = {}) {
        const offset = options['offset'] || 0;
        let frames = [];
        let env = this;
        while (env && !frames.includes(env)) {
            frames.push(env);
            env = env.parent;
        }
        if (offset === 0) {
            return frames;
        }
        else {
            return frames.slice(offset);
        }
    }
    /**
     * Get the binding for `key` as a tuple `(value found)`.
     */
    getTuple(key) {
        let env = this.findFrame(key);
        if (env) {
            return env.getLocalTuple(key);
        }
        else {
            return [undefined, false];
        }
    }
    /**
     * Get the binding defined by the current environment frame,
     * if any.
     */
    getLocal(key, notFound = undefined) {
        let [value, found] = this.getLocalTuple(key);
        if (found) {
            return value;
        }
        else {
            return notFound;
        }
    }
    /**
     * Get the binding defined by the current environment frame,
     * if any, as a tuple `(binding found)`.
     */
    getLocalTuple(key) {
        let found = this.hasLocal(key);
        const value = found ? this.table.get(key) : undefined;
        return [value, found];
    }
    /**
     * Get the parent environment, if any.
     */
    getParent() {
        return this.parent;
    }
    /**
     * Get the value of `key`, or `not-found`
     * if there is no binding.
     */
    getValue(key, notFound = undefined) {
        // Alias for `.get`.
        return this.get(key, notFound);
    }
    /**
     * Whether `key` is bound in the environment,
     * or in a parent environment.
     */
    has(key) {
        let env = this.findFrame(key);
        if (env) {
            return true;
        }
        else {
            return false;
        }
    }
    /**
     * Whether `key` is bound in the current environment frame.
     */
    hasLocal(key) {
        return this.table.has(key);
    }
    /**
     * Map a function over the environment.
     */
    map(f) {
        return new this.constructor(this.entries().map(f), this.parent);
    }
    /**
     * Set `key` to `value` in the environment.
     */
    set(key, value) {
        let env = this.findFrame(key, {
            notFound: this
        });
        return env.setLocal(key, value);
    }
    /**
     * Add an entry to the current environment frame.
     */
    setEntry(entry) {
        let [key, binding] = entry;
        return this.setLocal(key, binding);
    }
    /**
     * Set `key` to `value` in the current environment frame.
     */
    setLocal(key, value) {
        const table = this.table;
        table.set(key, value);
        return this;
    }
    /**
     * Set the parent environment.
     */
    setParent(parent) {
        this.parent = parent;
        return this;
    }
    /**
     * Set `key` to `value` in the environment.
     */
    setValue(key, value) {
        // Alias for `.set`.
        return this.set(key, value);
    }
}
exports.Environment = Environment;
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
    get(key, notFound = undefined) {
        return this.getUntypedValue(key, notFound);
    }
    /**
     * Get the local binding defined by the current environment frame,
     * if any.
     */
    getLocal(key, notFound = undefined) {
        return this.getUntypedLocalValue(key, notFound);
    }
    /**
     * Get the type of `key`. If there is no binding,
     * return `"undefined"`.
     */
    getType(key) {
        const [, typ] = this.getTypedValue(key);
        return typ;
    }
    /**
     * Get the typed value of `key`, which is a tuple
     * `(value type)`. If there is no binding, return
     * `not-found`.
     */
    getTypedValue(key, notFound = [undefined, 'undefined']) {
        // The same as `super.get`, except that
        // `not-found` defaults to `(undefined "undefined")`.
        return super.get(key, notFound);
    }
    getTypedLocalValue(key, notFound = [undefined, 'undefined']) {
        // The same as `super.get-local`, except that
        // `not-found` defaults to `(undefined "undefined")`.
        return super.getLocal(key, notFound);
    }
    /**
     * Get the untyped value of `key`. If there is no binding,
     * return `not-found`.
     */
    getUntypedValue(key, notFound = undefined) {
        const [value, typ] = this.getTypedValue(key);
        if (typ === 'undefined') {
            return notFound;
        }
        else {
            return value;
        }
    }
    /**
     * Get the untyped local value of `key`. If there is no binding,
     * return `not-found`.
     */
    getUntypedLocalValue(key, notFound = undefined) {
        const [value, typ] = this.getTypedLocalValue(key);
        if (typ === 'undefined') {
            return notFound;
        }
        else {
            return value;
        }
    }
    /**
     * Set `key` to `value` with type `type` in the environment.
     */
    set(key, value, type = 'variable') {
        // Alias for `.set-typed-value`.
        return this.setTypedValue(key, value, type);
    }
    /**
     * Add an entry `(key value type)` or `(key (value type))`
     * to the environment.
     */
    setEntry(entry) {
        if (entry.length === 3) {
            const [key, value, type] = entry;
            this.setLocal(key, value, type);
        }
        else {
            let [key, binding] = entry;
            const [value, type] = binding;
            this.setLocal(key, value, type);
        }
        return this;
    }
    /**
     * Set `key` to `value` with type `type` in
     * the current environment frame.
     */
    setLocal(key, value, type = 'variable') {
        return super.setLocal(key, [value, type]);
    }
    /**
     * Set `key` to `value` with type `type` in
     * the current environment frame.
     */
    setTypedValue(key, value, type = 'variable') {
        let env = this.findFrame(key, {
            notFound: this
        });
        if (env instanceof TypedEnvironment) {
            return env.setLocal(key, value, type);
        }
        else {
            return key.setLocal(value);
        }
    }
    /**
     * Set `key` to `value` with type `type` in
     * the current environment frame.
     */
    setValue(key, value, type = 'variable') {
        // Alias for `.set`.
        return this.set(key, value, type);
    }
}
exports.TypedEnvironment = TypedEnvironment;
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
    getLocalTuple(key) {
        let tuple = this.getUnforcedLocalTuple(key);
        let [binding, found] = tuple;
        if (found) {
            let [val, typ] = binding;
            if ((0, thunk_1.thunkp)(val)) {
                val = (0, thunk_1.force)(val);
                this.setLocal(key, val, typ);
                binding = [val, typ];
                tuple = [binding, found];
            }
        }
        return tuple;
    }
    getUnforcedTuple(key) {
        if (this.hasLocal(key)) {
            return this.getUnforcedLocalTuple(key);
        }
        else {
            for (let frame of this.getFrames({
                offset: 1
            })) {
                if (frame.hasLocal(key)) {
                    return (frame instanceof ThunkedEnvironment) ? frame.getUnforcedLocalTuple(key) : frame.getLocalTuple(key);
                }
            }
            return [undefined, false];
        }
    }
    getUnforcedLocalTuple(key) {
        return super.getLocalTuple(key);
    }
    /**
     * Get the type of `key`. If there is no binding,
     * return `"undefined"`.
     */
    getType(key) {
        // Obtain the type without forcing the thunk.
        let tuple = this.getUnforcedTuple(key);
        let [binding, found] = tuple;
        if (found) {
            const [, typ] = binding;
            return typ;
        }
        else {
            return 'undefined';
        }
    }
}
exports.ThunkedEnvironment = ThunkedEnvironment;
/**
 * Lisp environment.
 *
 * A typed, thunked environment.
 */
class LispEnvironment extends ThunkedEnvironment {
}
exports.LispEnvironment = LispEnvironment;
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
     * Create an environment stack.
     *
     * `args` is a list of environments. `args[0]` becomes the top of
     * the stack, i.e., the environment that is tried first.
     */
    constructor(...args) {
        super();
        /**
         * Environment stack.
         *
         * `stack[0]` is the top of the stack, i.e., the environment
         * that is tried first.
         */
        this.stack = [];
        this.stack = args;
    }
    /**
     * Clone the environment stack.
     */
    clone() {
        let env = super.clone();
        env.stack = [...this.stack];
        return env;
    }
    /**
     * Find an environment frame binding `key`.
     * Returns `not-found` if not found.
     */
    findFrame(key, options = {}) {
        const notFound = options['notFound'];
        const inheritedOptions = Object.assign(Object.assign({}, options), { notFound: false });
        let env = this.findLocalFrame(key, inheritedOptions);
        if (!env) {
            const parent = this.parent;
            if (parent) {
                env = parent.findFrame(key, inheritedOptions);
            }
        }
        if (env) {
            return env;
        }
        else {
            return notFound;
        }
    }
    /**
     * Find a local environment frame binding `key`.
     * Returns `not-found` if not found.
     */
    findLocalFrame(key, options = {}) {
        const notFound = options['notFound'];
        const filter = options['filter'];
        const inheritedOptions = Object.assign(Object.assign({}, options), { notFound: false });
        let result = notFound;
        const environments = this.stack;
        for (let env of environments) {
            const frame = env.findFrame(key, inheritedOptions);
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
    getFrames(options = {}) {
        let frames = [];
        const offset = options['offset'] || 0;
        const environments = this.stack;
        const parent = this.parent;
        if (parent) {
            environments.push(parent);
        }
        for (let env of environments) {
            for (let f of env.getFrames()) {
                if (!frames.includes(f)) {
                    if (f instanceof EnvironmentStack) {
                        frames = [...frames, ...f.getFrames()];
                    }
                    else {
                        frames.push(f);
                    }
                }
            }
        }
        if (offset === 0) {
            return frames;
        }
        else {
            return frames.slice(offset);
        }
    }
    /**
     * Get the binding for `key` as a tuple `(value found)`.
     */
    getTuple(key) {
        let env = this.findFrame(key);
        if (env) {
            return env.getTuple(key);
        }
        else {
            return [undefined, false];
        }
    }
    /**
     * Get the local binding for `key` as a tuple `(value found)`.
     */
    getLocalTuple(key) {
        let env = this.findLocalFrame(key);
        if (env) {
            return env.getTuple(key);
        }
        else {
            return [undefined, false];
        }
    }
    /**
     * Whether the stack contains an environment that binds `key`.
     */
    hasLocal(key) {
        // This could have been implemented in terms of
        // `find-local-frame`, but the following is faster since it
        // doesn't concern itself with the finer details of which
        // particular frame of which particular environment contains the
        // binding.
        let result = false;
        for (let env of this.stack) {
            if (env.has(key)) {
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
    hasEnvironment(env) {
        if (env === this) {
            return true;
        }
        for (let x of this.stack) {
            if (x === env) {
                return true;
            }
            else if ((x instanceof EnvironmentStack) && x.hasEnvironment(env)) {
                return true;
            }
        }
        if (this.parent && (this.parent instanceof EnvironmentStack)) {
            return this.parent.hasEnvironment(env);
        }
        else {
            return false;
        }
    }
    /**
     * Map a function over the environment stack.
     */
    map(f) {
        return new EnvironmentStack(...this.stack.map(function (env) {
            return env.map(f);
        }));
    }
    /**
     * Set `key` to `value` in the first
     * environment in the stack.
     */
    setLocal(key, value, type = 'variable') {
        let env = this.stack[0];
        if (env) {
            if (env instanceof TypedEnvironment) {
                env.setLocal(key, value, type);
            }
            else {
                key.setLocal(value);
            }
        }
        return this;
    }
}
exports.EnvironmentStack = EnvironmentStack;
/**
 * Compose environments left-to-right.
 */
class EnvironmentPipe extends TypedEnvironment {
    /**
     * Create an environment pipe.
     */
    constructor(...args) {
        super();
        this.environments = [];
        this.environments = args;
    }
    /**
     * Get the binding for `key` as a tuple `(value found)`.
     */
    getTuple(key) {
        return this.getLocalTuple(key);
    }
    /**
     * Get the local binding for `key` as a tuple `(value found)`.
     */
    getLocalTuple(key) {
        let currentKey = key;
        let lastKey = key;
        let lastEnv;
        let found = true;
        for (let env of this.environments) {
            if (env.has(currentKey)) {
                lastEnv = env;
                lastKey = currentKey;
                currentKey = env.get(currentKey);
            }
            else {
                found = false;
                break;
            }
        }
        if (found) {
            return lastEnv.getTuple(lastKey);
        }
        else if (this.parent) {
            return this.parent.getTuple(currentKey);
        }
        else {
            return [undefined, false];
        }
    }
    has(key) {
        let [value, found] = this.getTuple(key);
        return found;
    }
    hasLocal(key) {
        let [value, found] = this.getLocalTuple(key);
        return found;
    }
}
exports.EnvironmentPipe = EnvironmentPipe;
/**
 * Compose environments right-to-left.
 */
class EnvironmentComposition extends EnvironmentPipe {
    /**
     * Create an environment composition.
     */
    constructor(...args) {
        super(...args.reverse());
    }
}
exports.EnvironmentComposition = EnvironmentComposition;
/**
 * Dynamic environment, retrieving values
 * on the basis of some lookup function.
 */
class DynamicEnvironment extends TypedEnvironment {
    /**
     * Create a dynamic environment.
     */
    constructor(lookupF, typingF = function (...args) {
        return 'variable';
    }) {
        super();
        this.lookupF = lookupF;
        this.typingF = typingF;
    }
    /**
     * Get the binding defined by the dynamic environment,
     * if any, as a tuple `(binding found)`.
     */
    getLocalTuple(key) {
        let [value, found] = this.lookupF(key);
        if (found) {
            return [[value, this.typingF(value)], true];
        }
        else {
            return [undefined, false];
        }
    }
    /**
     * Whether `key` is bound by the dynamic environment.
     */
    hasLocal(key) {
        let [, found] = this.getLocalTuple(key);
        return found;
    }
}
exports.DynamicEnvironment = DynamicEnvironment;
/**
 * JavaScript environment.
 *
 * A dynamic environment that looks up JavaScript values.
 */
class JavaScriptEnvironment extends DynamicEnvironment {
    constructor() {
        super(lookup_1.lookupJsValue);
    }
}
exports.JavaScriptEnvironment = JavaScriptEnvironment;
/**
 * Pointer to the current environment.
 * Used by {@link currentEnvironment}.
 */
let currentEnvironmentPointer = undefined;
exports.currentEnvironmentPointer = currentEnvironmentPointer;
/**
 * Return the current environment.
 */
function currentEnvironment_() {
    return currentEnvironmentPointer;
}
exports.currentEnvironment = currentEnvironment_;
exports.currentEnvironment_ = currentEnvironment_;
currentEnvironment_.lispSource = [Symbol.for('define'), [Symbol.for('current-environment_')], Symbol.for('current-environment-pointer')];
/**
 * Return an empty environment.
 */
function emptyEnvironment() {
    return new LispEnvironment();
}
exports.emptyEnvironment = emptyEnvironment;
emptyEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('empty-environment')], [Symbol.for('new'), Symbol.for('LispEnvironment')]];
/**
 * Return the default environment.
 *
 * The default environment is defined as follows: use
 * the current environment if there is one, and if not,
 * use the empty environment.
 */
function defaultEnvironment() {
    return currentEnvironment_() || emptyEnvironment();
}
exports.defaultEnvironment = defaultEnvironment;
defaultEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('default-environment')], [Symbol.for('or'), [Symbol.for('current-environment_')], [Symbol.for('empty-environment')]]];
/**
 * Run `f` with `currentEnvironmentPointer` bound to `env`.
 * This makes the current environment available through the
 * function {@link currentEnvironment}. The original value
 * of `currentEnvironmentPointer` is restored afterwards.
 */
function withEnvironment(env, f) {
    // TODO: It would be much faster to implement this as
    // a macro.
    let result = undefined;
    const tmp = currentEnvironmentPointer;
    try {
        exports.currentEnvironmentPointer = currentEnvironmentPointer = env;
        result = f();
    }
    finally {
        exports.currentEnvironmentPointer = currentEnvironmentPointer = tmp;
    }
    return result;
}
exports.withEnvironment = withEnvironment;
withEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('with-environment'), Symbol.for('env'), Symbol.for('f')], [Symbol.for('define'), Symbol.for('result'), Symbol.for('undefined')], [Symbol.for('define'), Symbol.for('tmp'), Symbol.for('current-environment-pointer')], [Symbol.for('try'), [Symbol.for('set!'), Symbol.for('current-environment-pointer'), Symbol.for('env')], [Symbol.for('set!'), Symbol.for('result'), [Symbol.for('f')]], [Symbol.for('finally'), [Symbol.for('set!'), Symbol.for('current-environment-pointer'), Symbol.for('tmp')]]], Symbol.for('result')];
/**
 * Make an environment.
 */
function makeEnvironment(variables = undefined, parent = undefined, isLisp2 = false) {
    return new LispEnvironment(variables, parent);
}
exports.makeEnvironment = makeEnvironment;
makeEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('make-environment'), [Symbol.for('variables'), Symbol.for('undefined')], [Symbol.for('parent'), Symbol.for('undefined')], [Symbol.for('is-lisp-2'), Symbol.for('#f')]], [Symbol.for('new'), Symbol.for('LispEnvironment'), Symbol.for('variables'), Symbol.for('parent')]];
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
function extendEnvironment(env, parent) {
    // Create an array where each environment frame is
    // listed only once. This prevents cycles when the array
    // is converted to a linked list, which is how we tie
    // the two environments together.
    let frames = [];
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
exports.extendEnvironment = extendEnvironment;
extendEnvironment.lispSource = [Symbol.for('define'), [Symbol.for('extend-environment'), Symbol.for('env'), Symbol.for('parent')], [Symbol.for('define'), Symbol.for('frames'), [Symbol.for('quote'), []]], [Symbol.for('for'), [[Symbol.for('frame'), [Symbol.for('environment-frames'), Symbol.for('env')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('frame'), Symbol.for('frames')], [Symbol.for('push-right!'), Symbol.for('frames'), Symbol.for('frame')]]], [Symbol.for('for'), [[Symbol.for('frame'), [Symbol.for('environment-frames'), Symbol.for('parent')]]], [Symbol.for('unless'), [Symbol.for('memq?'), Symbol.for('frame'), Symbol.for('frames')], [Symbol.for('push-right!'), Symbol.for('frames'), Symbol.for('frame')]]], [Symbol.for('link-environment-frames'), Symbol.for('frames')], Symbol.for('env')];
/**
 * Return an array of the frames in an environment.
 */
function environmentFrames(env) {
    let frames = [env];
    if (env.parent) {
        return [...frames, ...environmentFrames(env.parent)];
    }
    else {
        return frames;
    }
}
exports.environmentFrames = environmentFrames;
environmentFrames.lispSource = [Symbol.for('define'), [Symbol.for('environment-frames'), Symbol.for('env')], [Symbol.for('define'), Symbol.for('frames'), [Symbol.for('list'), Symbol.for('env')]], [Symbol.for('if'), [Symbol.for('get-field'), Symbol.for('parent'), Symbol.for('env')], [Symbol.for('append'), Symbol.for('frames'), [Symbol.for('environment-frames'), [Symbol.for('get-field'), Symbol.for('parent'), Symbol.for('env')]]], Symbol.for('frames')]];
/**
 * Convert an array of environment frames to a linked list
 * by modifying each frame's `parent` property to point to
 * the next frame in the array.
 */
function linkEnvironmentFrames(frames) {
    let firstFrame = undefined;
    const _start = frames.length - 1;
    for (let i = _start; i > -1; i--) {
        const frame = frames[i];
        frame.parent = firstFrame;
        firstFrame = frame;
    }
    return firstFrame;
}
exports.linkEnvironmentFrames = linkEnvironmentFrames;
linkEnvironmentFrames.lispSource = [Symbol.for('define'), [Symbol.for('link-environment-frames'), Symbol.for('frames')], [Symbol.for('define'), Symbol.for('first-frame'), Symbol.for('undefined')], [Symbol.for('for'), [[Symbol.for('i'), [Symbol.for('range'), [Symbol.for('-'), [Symbol.for('array-list-length'), Symbol.for('frames')], 1], -1, -1]]], [Symbol.for('define'), Symbol.for('frame'), [Symbol.for('aget'), Symbol.for('frames'), Symbol.for('i')]], [Symbol.for('set-field!'), Symbol.for('parent'), Symbol.for('frame'), Symbol.for('first-frame')], [Symbol.for('set!'), Symbol.for('first-frame'), Symbol.for('frame')]], Symbol.for('first-frame')];
/**
 * Prefix a set of bindings.
 */
function prefixBindings(prefix, bindings) {
    return bindings.map(function (x) {
        const prefixedSym = Symbol.for(prefix + x[0].description);
        return [prefixedSym, ...x.slice(1)];
    });
}
exports.prefixBindings = prefixBindings;
prefixBindings.lispSource = [Symbol.for('define'), [Symbol.for('prefix-bindings'), Symbol.for('prefix'), Symbol.for('bindings')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('define'), Symbol.for('prefixed-sym'), [Symbol.for('string->symbol'), [Symbol.for('string-append'), Symbol.for('prefix'), [Symbol.for('symbol->string'), [Symbol.for('first'), Symbol.for('x')]]]]], [Symbol.for('append'), [Symbol.for('list'), Symbol.for('prefixed-sym')], [Symbol.for('rest'), Symbol.for('x')]]], Symbol.for('bindings')]];
