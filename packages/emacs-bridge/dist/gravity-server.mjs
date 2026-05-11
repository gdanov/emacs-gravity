
import { createRequire as __createRequire } from "module";
import { fileURLToPath as __fileURLToPath } from "url";
import { dirname as __dirnameFn } from "path";
const require = __createRequire(import.meta.url);
const __filename = __fileURLToPath(import.meta.url);
const __dirname = __dirnameFn(__filename);

var __defProp = Object.defineProperty;
var __export = (target, all3) => {
  for (var name in all3)
    __defProp(target, name, { get: all3[name], enumerable: true });
};

// src/gravity-server.ts
import { createServer } from "net";
import { unlinkSync as unlinkSync2 } from "fs";
import { dirname } from "path";

// ../../node_modules/effect/dist/Pipeable.js
var pipeArguments = (self, args2) => {
  switch (args2.length) {
    case 0:
      return self;
    case 1:
      return args2[0](self);
    case 2:
      return args2[1](args2[0](self));
    case 3:
      return args2[2](args2[1](args2[0](self)));
    case 4:
      return args2[3](args2[2](args2[1](args2[0](self))));
    case 5:
      return args2[4](args2[3](args2[2](args2[1](args2[0](self)))));
    case 6:
      return args2[5](args2[4](args2[3](args2[2](args2[1](args2[0](self))))));
    case 7:
      return args2[6](args2[5](args2[4](args2[3](args2[2](args2[1](args2[0](self)))))));
    case 8:
      return args2[7](args2[6](args2[5](args2[4](args2[3](args2[2](args2[1](args2[0](self))))))));
    case 9:
      return args2[8](args2[7](args2[6](args2[5](args2[4](args2[3](args2[2](args2[1](args2[0](self)))))))));
    default: {
      let ret = self;
      for (let i = 0, len = args2.length; i < len; i++) {
        ret = args2[i](ret);
      }
      return ret;
    }
  }
};
var Prototype = {
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var Class = /* @__PURE__ */ (function() {
  function PipeableBase() {
  }
  PipeableBase.prototype = Prototype;
  return PipeableBase;
})();

// ../../node_modules/effect/dist/Function.js
var dual = function(arity, body) {
  if (typeof arity === "function") {
    return function() {
      return arity(arguments) ? body.apply(this, arguments) : (self) => body(self, ...arguments);
    };
  }
  switch (arity) {
    case 0:
    case 1:
      throw new RangeError(`Invalid arity ${arity}`);
    case 2:
      return function(a, b) {
        if (arguments.length >= 2) {
          return body(a, b);
        }
        return function(self) {
          return body(self, a);
        };
      };
    case 3:
      return function(a, b, c) {
        if (arguments.length >= 3) {
          return body(a, b, c);
        }
        return function(self) {
          return body(self, a, b);
        };
      };
    default:
      return function() {
        if (arguments.length >= arity) {
          return body.apply(this, arguments);
        }
        const args2 = arguments;
        return function(self) {
          return body(self, ...args2);
        };
      };
  }
};
var identity = (a) => a;
var constant = (value) => () => value;
var constTrue = /* @__PURE__ */ constant(true);
var constFalse = /* @__PURE__ */ constant(false);
var constUndefined = /* @__PURE__ */ constant(void 0);
var constVoid = constUndefined;
function pipe(a, ...args2) {
  return pipeArguments(a, args2);
}

// ../../node_modules/effect/dist/internal/equal.js
var getAllObjectKeys = (obj) => {
  const keys = new Set(Reflect.ownKeys(obj));
  if (obj.constructor === Object) return keys;
  if (obj instanceof Error) {
    keys.delete("stack");
  }
  const proto = Object.getPrototypeOf(obj);
  let current = proto;
  while (current !== null && current !== Object.prototype) {
    const ownKeys = Reflect.ownKeys(current);
    for (let i = 0; i < ownKeys.length; i++) {
      keys.add(ownKeys[i]);
    }
    current = Object.getPrototypeOf(current);
  }
  if (keys.has("constructor") && typeof obj.constructor === "function" && proto === obj.constructor.prototype) {
    keys.delete("constructor");
  }
  return keys;
};
var byReferenceInstances = /* @__PURE__ */ new WeakSet();

// ../../node_modules/effect/dist/Predicate.js
function isString(input) {
  return typeof input === "string";
}
function isNumber(input) {
  return typeof input === "number";
}
function isBigInt(input) {
  return typeof input === "bigint";
}
function isFunction(input) {
  return typeof input === "function";
}
function isNotUndefined(input) {
  return input !== void 0;
}
function isObjectKeyword(input) {
  return typeof input === "object" && input !== null || isFunction(input);
}
var hasProperty = /* @__PURE__ */ dual(2, (self, property) => isObjectKeyword(self) && property in self);
var isTagged = /* @__PURE__ */ dual(2, (self, tag) => hasProperty(self, "_tag") && self["_tag"] === tag);
function isIterable(input) {
  return hasProperty(input, Symbol.iterator) || isString(input);
}

// ../../node_modules/effect/dist/Hash.js
var symbol = "~effect/interfaces/Hash";
var hash = (self) => {
  switch (typeof self) {
    case "number":
      return number(self);
    case "bigint":
      return string(self.toString(10));
    case "boolean":
      return string(String(self));
    case "symbol":
      return string(String(self));
    case "string":
      return string(self);
    case "undefined":
      return string("undefined");
    case "function":
    case "object": {
      if (self === null) {
        return string("null");
      } else if (self instanceof Date) {
        return string(self.toISOString());
      } else if (self instanceof RegExp) {
        return string(self.toString());
      } else {
        if (byReferenceInstances.has(self)) {
          return random(self);
        }
        if (hashCache.has(self)) {
          return hashCache.get(self);
        }
        const h = withVisitedTracking(self, () => {
          if (isHash(self)) {
            return self[symbol]();
          } else if (typeof self === "function") {
            return random(self);
          } else if (Array.isArray(self)) {
            return array(self);
          } else if (self instanceof Map) {
            return hashMap(self);
          } else if (self instanceof Set) {
            return hashSet(self);
          }
          return structure(self);
        });
        hashCache.set(self, h);
        return h;
      }
    }
    default:
      throw new Error(`BUG: unhandled typeof ${typeof self} - please report an issue at https://github.com/Effect-TS/effect/issues`);
  }
};
var random = (self) => {
  if (!randomHashCache.has(self)) {
    randomHashCache.set(self, number(Math.floor(Math.random() * Number.MAX_SAFE_INTEGER)));
  }
  return randomHashCache.get(self);
};
var combine = /* @__PURE__ */ dual(2, (self, b) => self * 53 ^ b);
var optimize = (n) => n & 3221225471 | n >>> 1 & 1073741824;
var isHash = (u) => hasProperty(u, symbol);
var number = (n) => {
  if (n !== n) {
    return string("NaN");
  }
  if (n === Infinity) {
    return string("Infinity");
  }
  if (n === -Infinity) {
    return string("-Infinity");
  }
  let h = n | 0;
  if (h !== n) {
    h ^= n * 4294967295;
  }
  while (n > 4294967295) {
    h ^= n /= 4294967295;
  }
  return optimize(h);
};
var string = (str) => {
  let h = 5381, i = str.length;
  while (i) {
    h = h * 33 ^ str.charCodeAt(--i);
  }
  return optimize(h);
};
var structureKeys = (o, keys) => {
  let h = 12289;
  for (const key of keys) {
    h ^= combine(hash(key), hash(o[key]));
  }
  return optimize(h);
};
var structure = (o) => structureKeys(o, getAllObjectKeys(o));
var iterableWith = (seed, f) => (iter) => {
  let h = seed;
  for (const element of iter) {
    h ^= f(element);
  }
  return optimize(h);
};
var array = /* @__PURE__ */ iterableWith(6151, hash);
var hashMap = /* @__PURE__ */ iterableWith(/* @__PURE__ */ string("Map"), ([k, v]) => combine(hash(k), hash(v)));
var hashSet = /* @__PURE__ */ iterableWith(/* @__PURE__ */ string("Set"), hash);
var randomHashCache = /* @__PURE__ */ new WeakMap();
var hashCache = /* @__PURE__ */ new WeakMap();
var visitedObjects = /* @__PURE__ */ new WeakSet();
function withVisitedTracking(obj, fn3) {
  if (visitedObjects.has(obj)) {
    return string("[Circular]");
  }
  visitedObjects.add(obj);
  const result3 = fn3();
  visitedObjects.delete(obj);
  return result3;
}

// ../../node_modules/effect/dist/Equal.js
var symbol2 = "~effect/interfaces/Equal";
function equals() {
  if (arguments.length === 1) {
    return (self) => compareBoth(self, arguments[0]);
  }
  return compareBoth(arguments[0], arguments[1]);
}
function compareBoth(self, that) {
  if (self === that) return true;
  if (self == null || that == null) return false;
  const selfType = typeof self;
  if (selfType !== typeof that) {
    return false;
  }
  if (selfType === "number" && self !== self && that !== that) {
    return true;
  }
  if (selfType !== "object" && selfType !== "function") {
    return false;
  }
  if (byReferenceInstances.has(self) || byReferenceInstances.has(that)) {
    return false;
  }
  return withCache(self, that, compareObjects);
}
function withVisitedTracking2(self, that, fn3) {
  const hasLeft = visitedLeft.has(self);
  const hasRight = visitedRight.has(that);
  if (hasLeft && hasRight) {
    return true;
  }
  if (hasLeft || hasRight) {
    return false;
  }
  visitedLeft.add(self);
  visitedRight.add(that);
  const result3 = fn3();
  visitedLeft.delete(self);
  visitedRight.delete(that);
  return result3;
}
var visitedLeft = /* @__PURE__ */ new WeakSet();
var visitedRight = /* @__PURE__ */ new WeakSet();
function compareObjects(self, that) {
  if (hash(self) !== hash(that)) {
    return false;
  } else if (self instanceof Date) {
    if (!(that instanceof Date)) return false;
    return self.toISOString() === that.toISOString();
  } else if (self instanceof RegExp) {
    if (!(that instanceof RegExp)) return false;
    return self.toString() === that.toString();
  }
  const selfIsEqual = isEqual(self);
  const thatIsEqual = isEqual(that);
  if (selfIsEqual !== thatIsEqual) return false;
  const bothEquals = selfIsEqual && thatIsEqual;
  if (typeof self === "function" && !bothEquals) {
    return false;
  }
  return withVisitedTracking2(self, that, () => {
    if (bothEquals) {
      return self[symbol2](that);
    } else if (Array.isArray(self)) {
      if (!Array.isArray(that) || self.length !== that.length) {
        return false;
      }
      return compareArrays(self, that);
    } else if (self instanceof Map) {
      if (!(that instanceof Map) || self.size !== that.size) {
        return false;
      }
      return compareMaps(self, that);
    } else if (self instanceof Set) {
      if (!(that instanceof Set) || self.size !== that.size) {
        return false;
      }
      return compareSets(self, that);
    }
    return compareRecords(self, that);
  });
}
function withCache(self, that, f) {
  let selfMap = equalityCache.get(self);
  if (!selfMap) {
    selfMap = /* @__PURE__ */ new WeakMap();
    equalityCache.set(self, selfMap);
  } else if (selfMap.has(that)) {
    return selfMap.get(that);
  }
  const result3 = f(self, that);
  selfMap.set(that, result3);
  let thatMap = equalityCache.get(that);
  if (!thatMap) {
    thatMap = /* @__PURE__ */ new WeakMap();
    equalityCache.set(that, thatMap);
  }
  thatMap.set(self, result3);
  return result3;
}
var equalityCache = /* @__PURE__ */ new WeakMap();
function compareArrays(self, that) {
  for (let i = 0; i < self.length; i++) {
    if (!compareBoth(self[i], that[i])) {
      return false;
    }
  }
  return true;
}
function compareRecords(self, that) {
  const selfKeys = getAllObjectKeys(self);
  const thatKeys = getAllObjectKeys(that);
  if (selfKeys.size !== thatKeys.size) {
    return false;
  }
  for (const key of selfKeys) {
    if (!thatKeys.has(key) || !compareBoth(self[key], that[key])) {
      return false;
    }
  }
  return true;
}
function makeCompareMap(keyEquivalence, valueEquivalence) {
  return function compareMaps2(self, that) {
    for (const [selfKey, selfValue] of self) {
      let found = false;
      for (const [thatKey, thatValue] of that) {
        if (keyEquivalence(selfKey, thatKey) && valueEquivalence(selfValue, thatValue)) {
          found = true;
          break;
        }
      }
      if (!found) {
        return false;
      }
    }
    return true;
  };
}
var compareMaps = /* @__PURE__ */ makeCompareMap(compareBoth, compareBoth);
function makeCompareSet(equivalence) {
  return function compareSets2(self, that) {
    for (const selfValue of self) {
      let found = false;
      for (const thatValue of that) {
        if (equivalence(selfValue, thatValue)) {
          found = true;
          break;
        }
      }
      if (!found) {
        return false;
      }
    }
    return true;
  };
}
var compareSets = /* @__PURE__ */ makeCompareSet(compareBoth);
var isEqual = (u) => hasProperty(u, symbol2);
var asEquivalence = () => equals;
var byReferenceUnsafe = (obj) => {
  byReferenceInstances.add(obj);
  return obj;
};

// ../../node_modules/effect/dist/Redactable.js
var symbolRedactable = /* @__PURE__ */ Symbol.for("~effect/Inspectable/redactable");
var isRedactable = (u) => hasProperty(u, symbolRedactable);
function redact(u) {
  if (isRedactable(u)) return getRedacted(u);
  return u;
}
function getRedacted(redactable) {
  return redactable[symbolRedactable](globalThis[currentFiberTypeId]?.services ?? emptyServiceMap);
}
var currentFiberTypeId = "~effect/Fiber/currentFiber";
var emptyServiceMap = {
  "~effect/ServiceMap": {},
  mapUnsafe: /* @__PURE__ */ new Map(),
  pipe() {
    return pipeArguments(this, arguments);
  }
};

// ../../node_modules/effect/dist/Formatter.js
function format(input, options) {
  const space = options?.space ?? 0;
  const seen = /* @__PURE__ */ new WeakSet();
  const gap = !space ? "" : typeof space === "number" ? " ".repeat(space) : space;
  const ind = (d) => gap.repeat(d);
  const wrap = (v, body) => {
    const ctor = v?.constructor;
    return ctor && ctor !== Object.prototype.constructor && ctor.name ? `${ctor.name}(${body})` : body;
  };
  const ownKeys = (o) => {
    try {
      return Reflect.ownKeys(o);
    } catch {
      return ["[ownKeys threw]"];
    }
  };
  function recur(v, d = 0) {
    if (Array.isArray(v)) {
      if (seen.has(v)) return CIRCULAR;
      seen.add(v);
      if (!gap || v.length <= 1) return `[${v.map((x) => recur(x, d)).join(",")}]`;
      const inner = v.map((x) => recur(x, d + 1)).join(",\n" + ind(d + 1));
      return `[
${ind(d + 1)}${inner}
${ind(d)}]`;
    }
    if (v instanceof Date) return formatDate(v);
    if (!options?.ignoreToString && hasProperty(v, "toString") && typeof v["toString"] === "function" && v["toString"] !== Object.prototype.toString && v["toString"] !== Array.prototype.toString) {
      const s = safeToString(v);
      if (v instanceof Error && v.cause) {
        return `${s} (cause: ${recur(v.cause, d)})`;
      }
      return s;
    }
    if (typeof v === "string") return JSON.stringify(v);
    if (typeof v === "number" || v == null || typeof v === "boolean" || typeof v === "symbol") return String(v);
    if (typeof v === "bigint") return String(v) + "n";
    if (typeof v === "object" || typeof v === "function") {
      if (seen.has(v)) return CIRCULAR;
      seen.add(v);
      if (symbolRedactable in v) return format(getRedacted(v));
      if (Symbol.iterator in v) {
        return `${v.constructor.name}(${recur(Array.from(v), d)})`;
      }
      const keys = ownKeys(v);
      if (!gap || keys.length <= 1) {
        const body2 = `{${keys.map((k) => `${formatPropertyKey(k)}:${recur(v[k], d)}`).join(",")}}`;
        return wrap(v, body2);
      }
      const body = `{
${keys.map((k) => `${ind(d + 1)}${formatPropertyKey(k)}: ${recur(v[k], d + 1)}`).join(",\n")}
${ind(d)}}`;
      return wrap(v, body);
    }
    return String(v);
  }
  return recur(input, 0);
}
var CIRCULAR = "[Circular]";
function formatPropertyKey(name) {
  return typeof name === "string" ? JSON.stringify(name) : String(name);
}
function formatDate(date) {
  try {
    return date.toISOString();
  } catch {
    return "Invalid Date";
  }
}
function safeToString(input) {
  try {
    const s = input.toString();
    return typeof s === "string" ? s : String(s);
  } catch {
    return "[toString threw]";
  }
}
function formatJson(input, options) {
  let cache = [];
  const out = JSON.stringify(input, (_key, value) => typeof value === "object" && value !== null ? cache.includes(value) ? void 0 : cache.push(value) && redact(value) : value, options?.space);
  cache = void 0;
  return out;
}

// ../../node_modules/effect/dist/Inspectable.js
var NodeInspectSymbol = /* @__PURE__ */ Symbol.for("nodejs.util.inspect.custom");
var toJson = (input) => {
  try {
    if (hasProperty(input, "toJSON") && isFunction(input["toJSON"]) && input["toJSON"].length === 0) {
      return input.toJSON();
    } else if (Array.isArray(input)) {
      return input.map(toJson);
    }
  } catch {
    return "[toJSON threw]";
  }
  return redact(input);
};
var toStringUnknown = (u, whitespace = 2) => {
  if (typeof u === "string") {
    return u;
  }
  try {
    return typeof u === "object" ? stringifyCircular(u, whitespace) : String(u);
  } catch {
    return String(u);
  }
};
var stringifyCircular = (obj, whitespace) => {
  let cache = [];
  const retVal = JSON.stringify(obj, (_key, value) => typeof value === "object" && value !== null ? cache.includes(value) ? void 0 : cache.push(value) && redact(value) : value, whitespace);
  cache = void 0;
  return retVal;
};
var BaseProto = {
  toJSON() {
    return toJson(this);
  },
  [NodeInspectSymbol]() {
    return this.toJSON();
  },
  toString() {
    return format(this.toJSON());
  }
};
var Class2 = class {
  /**
   * Node.js custom inspection method.
   *
   * @since 2.0.0
   */
  [NodeInspectSymbol]() {
    return this.toJSON();
  }
  /**
   * Returns a formatted string representation of this object.
   *
   * @since 2.0.0
   */
  toString() {
    return format(this.toJSON());
  }
};

// ../../node_modules/effect/dist/Utils.js
var GenKindTypeId = "~effect/Utils/GenKind";
var GenKindImpl = class {
  value;
  constructor(value) {
    this.value = value;
  }
  get _F() {
    return identity;
  }
  get _R() {
    return (_) => _;
  }
  get _O() {
    return (_) => _;
  }
  get _E() {
    return (_) => _;
  }
  [GenKindTypeId] = GenKindTypeId;
  [Symbol.iterator]() {
    return new SingleShotGen(this);
  }
};
var SingleShotGen = class _SingleShotGen {
  called = false;
  self;
  constructor(self) {
    this.self = self;
  }
  /**
   * @since 2.0.0
   */
  next(a) {
    return this.called ? {
      value: a,
      done: true
    } : (this.called = true, {
      value: this.self,
      done: false
    });
  }
  /**
   * @since 2.0.0
   */
  [Symbol.iterator]() {
    return new _SingleShotGen(this.self);
  }
};
var InternalTypeId = "~effect/Effect/internal";
var standard = {
  [InternalTypeId]: (body) => {
    return body();
  }
};
var forced = {
  [InternalTypeId]: (body) => {
    try {
      return body();
    } finally {
    }
  }
};
var isNotOptimizedAway = /* @__PURE__ */ standard[InternalTypeId](() => new Error().stack)?.includes(InternalTypeId) === true;
var internalCall = isNotOptimizedAway ? standard[InternalTypeId] : forced[InternalTypeId];
var genConstructor = function* () {
}.constructor;

// ../../node_modules/effect/dist/internal/core.js
var EffectTypeId = `~effect/Effect`;
var ExitTypeId = `~effect/Exit`;
var effectVariance = {
  _A: identity,
  _E: identity,
  _R: identity
};
var identifier = `${EffectTypeId}/identifier`;
var args = `${EffectTypeId}/args`;
var evaluate = `${EffectTypeId}/evaluate`;
var contA = `${EffectTypeId}/successCont`;
var contE = `${EffectTypeId}/failureCont`;
var contAll = `${EffectTypeId}/ensureCont`;
var Yield = /* @__PURE__ */ Symbol.for("effect/Effect/Yield");
var PipeInspectableProto = {
  pipe() {
    return pipeArguments(this, arguments);
  },
  toJSON() {
    return {
      ...this
    };
  },
  toString() {
    return format(this.toJSON(), {
      ignoreToString: true,
      space: 2
    });
  },
  [NodeInspectSymbol]() {
    return this.toJSON();
  }
};
var StructuralProto = {
  [symbol]() {
    return structureKeys(this, Object.keys(this));
  },
  [symbol2](that) {
    const selfKeys = Object.keys(this);
    const thatKeys = Object.keys(that);
    if (selfKeys.length !== thatKeys.length) return false;
    for (let i = 0; i < selfKeys.length; i++) {
      if (selfKeys[i] !== thatKeys[i] && !equals(this[selfKeys[i]], that[selfKeys[i]])) {
        return false;
      }
    }
    return true;
  }
};
var YieldableProto = {
  [Symbol.iterator]() {
    return new SingleShotGen(this);
  }
};
var YieldableErrorProto = {
  ...YieldableProto,
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var EffectProto = {
  [EffectTypeId]: effectVariance,
  ...PipeInspectableProto,
  [Symbol.iterator]() {
    return new SingleShotGen(this);
  },
  asEffect() {
    return this;
  },
  toJSON() {
    return {
      _id: "Effect",
      op: this[identifier],
      ...args in this ? {
        args: this[args]
      } : void 0
    };
  }
};
var isEffect = (u) => hasProperty(u, EffectTypeId);
var isExit = (u) => hasProperty(u, ExitTypeId);
var CauseTypeId = "~effect/Cause";
var CauseReasonTypeId = "~effect/Cause/Reason";
var isCause = (self) => hasProperty(self, CauseTypeId);
var CauseImpl = class {
  [CauseTypeId];
  reasons;
  constructor(failures) {
    this[CauseTypeId] = CauseTypeId;
    this.reasons = failures;
  }
  pipe() {
    return pipeArguments(this, arguments);
  }
  toJSON() {
    return {
      _id: "Cause",
      failures: this.reasons.map((f) => f.toJSON())
    };
  }
  toString() {
    return `Cause(${format(this.reasons)})`;
  }
  [NodeInspectSymbol]() {
    return this.toJSON();
  }
  [symbol2](that) {
    return isCause(that) && this.reasons.length === that.reasons.length && this.reasons.every((e, i) => equals(e, that.reasons[i]));
  }
  [symbol]() {
    return array(this.reasons);
  }
};
var annotationsMap = /* @__PURE__ */ new WeakMap();
var ReasonBase = class {
  [CauseReasonTypeId];
  annotations;
  _tag;
  constructor(_tag, annotations, originalError) {
    this[CauseReasonTypeId] = CauseReasonTypeId;
    this._tag = _tag;
    if (annotations !== constEmptyAnnotations && typeof originalError === "object" && originalError !== null && annotations.size > 0) {
      const prevAnnotations = annotationsMap.get(originalError);
      if (prevAnnotations) {
        annotations = new Map([...prevAnnotations, ...annotations]);
      }
      annotationsMap.set(originalError, annotations);
    }
    this.annotations = annotations;
  }
  annotate(annotations, options) {
    if (annotations.mapUnsafe.size === 0) return this;
    const newAnnotations = new Map(this.annotations);
    annotations.mapUnsafe.forEach((value, key) => {
      if (options?.overwrite !== true && newAnnotations.has(key)) return;
      newAnnotations.set(key, value);
    });
    const self = Object.assign(Object.create(Object.getPrototypeOf(this)), this);
    self.annotations = newAnnotations;
    return self;
  }
  pipe() {
    return pipeArguments(this, arguments);
  }
  toString() {
    return format(this);
  }
  [NodeInspectSymbol]() {
    return this.toString();
  }
};
var constEmptyAnnotations = /* @__PURE__ */ new Map();
var Fail = class extends ReasonBase {
  error;
  constructor(error, annotations = constEmptyAnnotations) {
    super("Fail", annotations, error);
    this.error = error;
  }
  toString() {
    return `Fail(${format(this.error)})`;
  }
  toJSON() {
    return {
      _tag: "Fail",
      error: this.error
    };
  }
  [symbol2](that) {
    return isFailReason(that) && equals(this.error, that.error) && equals(this.annotations, that.annotations);
  }
  [symbol]() {
    return combine(string(this._tag))(combine(hash(this.error))(hash(this.annotations)));
  }
};
var causeFromReasons = (reasons) => new CauseImpl(reasons);
var causeEmpty = /* @__PURE__ */ new CauseImpl([]);
var causeFail = (error) => new CauseImpl([new Fail(error)]);
var Die = class extends ReasonBase {
  defect;
  constructor(defect, annotations = constEmptyAnnotations) {
    super("Die", annotations, defect);
    this.defect = defect;
  }
  toString() {
    return `Die(${format(this.defect)})`;
  }
  toJSON() {
    return {
      _tag: "Die",
      defect: this.defect
    };
  }
  [symbol2](that) {
    return isDieReason(that) && equals(this.defect, that.defect) && equals(this.annotations, that.annotations);
  }
  [symbol]() {
    return combine(string(this._tag))(combine(hash(this.defect))(hash(this.annotations)));
  }
};
var causeDie = (defect) => new CauseImpl([new Die(defect)]);
var causeAnnotate = /* @__PURE__ */ dual((args2) => isCause(args2[0]), (self, annotations, options) => {
  if (annotations.mapUnsafe.size === 0) return self;
  return new CauseImpl(self.reasons.map((f) => f.annotate(annotations, options)));
});
var isFailReason = (self) => self._tag === "Fail";
var isDieReason = (self) => self._tag === "Die";
var isInterruptReason = (self) => self._tag === "Interrupt";
function defaultEvaluate(_fiber) {
  return exitDie(`Effect.evaluate: Not implemented`);
}
var makePrimitiveProto = (options) => ({
  ...EffectProto,
  [identifier]: options.op,
  [evaluate]: options[evaluate] ?? defaultEvaluate,
  [contA]: options[contA],
  [contE]: options[contE],
  [contAll]: options[contAll]
});
var makePrimitive = (options) => {
  const Proto3 = makePrimitiveProto(options);
  return function() {
    const self = Object.create(Proto3);
    self[args] = options.single === false ? arguments : arguments[0];
    return self;
  };
};
var makeExit = (options) => {
  const Proto3 = {
    ...makePrimitiveProto(options),
    [ExitTypeId]: ExitTypeId,
    _tag: options.op,
    get [options.prop]() {
      return this[args];
    },
    toString() {
      return `${options.op}(${format(this[args])})`;
    },
    toJSON() {
      return {
        _id: "Exit",
        _tag: options.op,
        [options.prop]: this[args]
      };
    },
    [symbol2](that) {
      return isExit(that) && that._tag === this._tag && equals(this[args], that[args]);
    },
    [symbol]() {
      return combine(string(options.op), hash(this[args]));
    }
  };
  return function(value) {
    const self = Object.create(Proto3);
    self[args] = value;
    return self;
  };
};
var exitSucceed = /* @__PURE__ */ makeExit({
  op: "Success",
  prop: "value",
  [evaluate](fiber3) {
    const cont = fiber3.getCont(contA);
    return cont ? cont[contA](this[args], fiber3, this) : fiber3.yieldWith(this);
  }
});
var StackTraceKey = {
  key: "effect/Cause/StackTrace"
};
var InterruptorStackTrace = {
  key: "effect/Cause/InterruptorStackTrace"
};
var exitFailCause = /* @__PURE__ */ makeExit({
  op: "Failure",
  prop: "cause",
  [evaluate](fiber3) {
    let cause = this[args];
    let annotated = false;
    if (fiber3.currentStackFrame) {
      cause = causeAnnotate(cause, {
        mapUnsafe: /* @__PURE__ */ new Map([[StackTraceKey.key, fiber3.currentStackFrame]])
      });
      annotated = true;
    }
    let cont = fiber3.getCont(contE);
    while (fiber3.interruptible && fiber3._interruptedCause && cont) {
      cont = fiber3.getCont(contE);
    }
    return cont ? cont[contE](cause, fiber3, annotated ? void 0 : this) : fiber3.yieldWith(annotated ? this : exitFailCause(cause));
  }
});
var exitFail = (e) => exitFailCause(causeFail(e));
var exitDie = (defect) => exitFailCause(causeDie(defect));
var withFiber = /* @__PURE__ */ makePrimitive({
  op: "WithFiber",
  [evaluate](fiber3) {
    return this[args](fiber3);
  }
});
var YieldableError = /* @__PURE__ */ (function() {
  class YieldableError2 extends globalThis.Error {
    asEffect() {
      return exitFail(this);
    }
  }
  Object.assign(YieldableError2.prototype, YieldableErrorProto);
  return YieldableError2;
})();
var Error2 = /* @__PURE__ */ (function() {
  const plainArgsSymbol = /* @__PURE__ */ Symbol.for("effect/Data/Error/plainArgs");
  return class Base extends YieldableError {
    constructor(args2) {
      super(args2?.message, args2?.cause ? {
        cause: args2.cause
      } : void 0);
      if (args2) {
        Object.assign(this, args2);
        Object.defineProperty(this, plainArgsSymbol, {
          value: args2,
          enumerable: false
        });
      }
    }
    toJSON() {
      return {
        ...this[plainArgsSymbol],
        ...this
      };
    }
  };
})();
var TaggedError = (tag) => {
  class Base extends Error2 {
    _tag = tag;
  }
  ;
  Base.prototype.name = tag;
  return Base;
};
var NoSuchElementErrorTypeId = "~effect/Cause/NoSuchElementError";
var isNoSuchElementError = (u) => hasProperty(u, NoSuchElementErrorTypeId);
var NoSuchElementError = class extends (/* @__PURE__ */ TaggedError("NoSuchElementError")) {
  [NoSuchElementErrorTypeId] = NoSuchElementErrorTypeId;
  constructor(message) {
    super({
      message
    });
  }
};
var DoneTypeId = "~effect/Cause/Done";
var isDone = (u) => hasProperty(u, DoneTypeId);
var DoneVoid = {
  [DoneTypeId]: DoneTypeId,
  _tag: "Done",
  value: void 0
};
var Done = (value) => {
  if (value === void 0) return DoneVoid;
  return {
    [DoneTypeId]: DoneTypeId,
    _tag: "Done",
    value
  };
};
var doneVoid = /* @__PURE__ */ exitFail(DoneVoid);
var done = (value) => {
  if (value === void 0) return doneVoid;
  return exitFail(Done(value));
};

// ../../node_modules/effect/dist/internal/option.js
var TypeId = "~effect/data/Option";
var CommonProto = {
  [TypeId]: {
    _A: (_) => _
  },
  ...PipeInspectableProto,
  ...YieldableProto
};
var SomeProto = /* @__PURE__ */ Object.assign(/* @__PURE__ */ Object.create(CommonProto), {
  _tag: "Some",
  _op: "Some",
  [symbol2](that) {
    return isOption(that) && isSome(that) && equals(this.value, that.value);
  },
  [symbol]() {
    return combine(hash(this._tag))(hash(this.value));
  },
  toString() {
    return `some(${format(this.value)})`;
  },
  toJSON() {
    return {
      _id: "Option",
      _tag: this._tag,
      value: toJson(this.value)
    };
  },
  asEffect() {
    return exitSucceed(this.value);
  }
});
var NoneHash = /* @__PURE__ */ hash("None");
var NoneProto = /* @__PURE__ */ Object.assign(/* @__PURE__ */ Object.create(CommonProto), {
  _tag: "None",
  _op: "None",
  [symbol2](that) {
    return isOption(that) && isNone(that);
  },
  [symbol]() {
    return NoneHash;
  },
  toString() {
    return `none()`;
  },
  toJSON() {
    return {
      _id: "Option",
      _tag: this._tag
    };
  },
  asEffect() {
    return exitFail(new NoSuchElementError());
  }
});
var isOption = (input) => hasProperty(input, TypeId);
var isNone = (fa) => fa._tag === "None";
var isSome = (fa) => fa._tag === "Some";
var none = /* @__PURE__ */ Object.create(NoneProto);
var some = (value) => {
  const a = Object.create(SomeProto);
  a.value = value;
  return a;
};

// ../../node_modules/effect/dist/internal/result.js
var TypeId2 = "~effect/data/Result";
var CommonProto2 = {
  [TypeId2]: {
    /* v8 ignore next 2 */
    _A: (_) => _,
    _E: (_) => _
  },
  ...PipeInspectableProto,
  ...YieldableProto
};
var SuccessProto = /* @__PURE__ */ Object.assign(/* @__PURE__ */ Object.create(CommonProto2), {
  _tag: "Success",
  _op: "Success",
  [symbol2](that) {
    return isResult(that) && isSuccess(that) && equals(this.success, that.success);
  },
  [symbol]() {
    return combine(hash(this._tag))(hash(this.success));
  },
  toString() {
    return `success(${format(this.success)})`;
  },
  toJSON() {
    return {
      _id: "Result",
      _tag: this._tag,
      value: toJson(this.success)
    };
  },
  asEffect() {
    return exitSucceed(this.success);
  }
});
var FailureProto = /* @__PURE__ */ Object.assign(/* @__PURE__ */ Object.create(CommonProto2), {
  _tag: "Failure",
  _op: "Failure",
  [symbol2](that) {
    return isResult(that) && isFailure(that) && equals(this.failure, that.failure);
  },
  [symbol]() {
    return combine(hash(this._tag))(hash(this.failure));
  },
  toString() {
    return `failure(${format(this.failure)})`;
  },
  toJSON() {
    return {
      _id: "Result",
      _tag: this._tag,
      failure: toJson(this.failure)
    };
  },
  asEffect() {
    return exitFail(this.failure);
  }
});
var isResult = (input) => hasProperty(input, TypeId2);
var isFailure = (result3) => result3._tag === "Failure";
var isSuccess = (result3) => result3._tag === "Success";
var fail = (failure) => {
  const a = Object.create(FailureProto);
  a.failure = failure;
  return a;
};
var succeed = (success) => {
  const a = Object.create(SuccessProto);
  a.success = success;
  return a;
};

// ../../node_modules/effect/dist/Order.js
function make(compare) {
  return (self, that) => self === that ? 0 : compare(self, that);
}
var Number2 = /* @__PURE__ */ make((self, that) => {
  if (globalThis.Number.isNaN(self) && globalThis.Number.isNaN(that)) return 0;
  if (globalThis.Number.isNaN(self)) return -1;
  if (globalThis.Number.isNaN(that)) return 1;
  return self < that ? -1 : 1;
});
var mapInput = /* @__PURE__ */ dual(2, (self, f) => make((b1, b2) => self(f(b1), f(b2))));
var isGreaterThan = (O) => dual(2, (self, that) => O(self, that) === 1);

// ../../node_modules/effect/dist/Option.js
var none2 = () => none;
var some2 = some;

// ../../node_modules/effect/dist/Result.js
var succeed2 = succeed;
var fail2 = fail;
var isFailure2 = isFailure;

// ../../node_modules/effect/dist/Filter.js
var apply = (filter4, input, ...args2) => {
  const result3 = filter4(input, ...args2);
  if (result3 === true) return succeed2(input);
  if (result3 === false) return fail2(input);
  return result3;
};
var composePassthrough = /* @__PURE__ */ dual(2, (left, right) => (input) => {
  const leftOut = left(input);
  if (isFailure2(leftOut)) return fail2(input);
  const rightOut = right(leftOut.success);
  if (isFailure2(rightOut)) return fail2(input);
  return rightOut;
});

// ../../node_modules/effect/dist/internal/array.js
var isArrayNonEmpty = (self) => self.length > 0;

// ../../node_modules/effect/dist/Iterable.js
var filter = /* @__PURE__ */ dual(2, (self, predicate) => ({
  [Symbol.iterator]() {
    const iterator = self[Symbol.iterator]();
    let i = 0;
    return {
      next() {
        let result3 = iterator.next();
        while (!result3.done) {
          if (predicate(result3.value, i++)) {
            return {
              done: false,
              value: result3.value
            };
          }
          result3 = iterator.next();
        }
        return {
          done: true,
          value: void 0
        };
      }
    };
  }
}));

// ../../node_modules/effect/dist/Array.js
var Array2 = globalThis.Array;
var fromIterable = (collection) => Array2.isArray(collection) ? collection : Array2.from(collection);
var appendAll = /* @__PURE__ */ dual(2, (self, that) => fromIterable(self).concat(fromIterable(that)));
var isArray = Array2.isArray;
var isReadonlyArrayNonEmpty = isArrayNonEmpty;
function isOutOfBounds(i, as3) {
  return i < 0 || i >= as3.length;
}
var getUnsafe = /* @__PURE__ */ dual(2, (self, index) => {
  const i = Math.floor(index);
  if (isOutOfBounds(i, self)) {
    throw new Error(`Index out of bounds: ${i}`);
  }
  return self[i];
});
var headNonEmpty = /* @__PURE__ */ getUnsafe(0);
var tailNonEmpty = (self) => self.slice(1);
var unionWith = /* @__PURE__ */ dual(3, (self, that, isEquivalent) => {
  const a = fromIterable(self);
  const b = fromIterable(that);
  if (isReadonlyArrayNonEmpty(a)) {
    if (isReadonlyArrayNonEmpty(b)) {
      const dedupe = dedupeWith(isEquivalent);
      return dedupe(appendAll(a, b));
    }
    return a;
  }
  return b;
});
var union = /* @__PURE__ */ dual(2, (self, that) => unionWith(self, that, asEquivalence()));
var partitionMap = /* @__PURE__ */ dual(2, (self, f) => {
  const failures = [];
  const successes = [];
  const as3 = fromIterable(self);
  for (let i = 0; i < as3.length; i++) {
    const e = f(as3[i], i);
    if (isFailure2(e)) {
      failures.push(e.failure);
    } else {
      successes.push(e.success);
    }
  }
  return [failures, successes];
});
var dedupeWith = /* @__PURE__ */ dual(2, (self, isEquivalent) => {
  const input = fromIterable(self);
  if (isReadonlyArrayNonEmpty(input)) {
    const out = [headNonEmpty(input)];
    const rest = tailNonEmpty(input);
    for (const r of rest) {
      if (out.every((a) => !isEquivalent(r, a))) {
        out.push(r);
      }
    }
    return out;
  }
  return [];
});

// ../../node_modules/effect/dist/Duration.js
var TypeId3 = "~effect/time/Duration";
var bigint0 = /* @__PURE__ */ BigInt(0);
var bigint1e3 = /* @__PURE__ */ BigInt(1e3);
var DURATION_REGEXP = /^(-?\d+(?:\.\d+)?)\s+(nanos?|micros?|millis?|seconds?|minutes?|hours?|days?|weeks?)$/;
var fromInputUnsafe = (input) => {
  if (isDuration(input)) return input;
  if (isNumber(input)) return millis(input);
  if (isBigInt(input)) return nanos(input);
  if (Array.isArray(input) && input.length === 2 && input.every(isNumber)) {
    if (Number.isNaN(input[0]) || Number.isNaN(input[1])) {
      return zero;
    }
    if (input[0] === -Infinity || input[1] === -Infinity) {
      return negativeInfinity;
    }
    if (input[0] === Infinity || input[1] === Infinity) {
      return infinity;
    }
    return nanos(BigInt(Math.round(input[0] * 1e9)) + BigInt(Math.round(input[1])));
  }
  if (isString(input)) {
    const match4 = DURATION_REGEXP.exec(input);
    if (match4) {
      const [_, valueStr, unit] = match4;
      const value = Number(valueStr);
      switch (unit) {
        case "nano":
        case "nanos":
          return nanos(BigInt(valueStr));
        case "micro":
        case "micros":
          return micros(BigInt(valueStr));
        case "milli":
        case "millis":
          return millis(value);
        case "second":
        case "seconds":
          return seconds(value);
        case "minute":
        case "minutes":
          return minutes(value);
        case "hour":
        case "hours":
          return hours(value);
        case "day":
        case "days":
          return days(value);
        case "week":
        case "weeks":
          return weeks(value);
      }
    }
  }
  throw new Error(`Invalid Input: ${input}`);
};
var zeroDurationValue = {
  _tag: "Millis",
  millis: 0
};
var infinityDurationValue = {
  _tag: "Infinity"
};
var negativeInfinityDurationValue = {
  _tag: "NegativeInfinity"
};
var DurationProto = {
  [TypeId3]: TypeId3,
  [symbol]() {
    return structure(this.value);
  },
  [symbol2](that) {
    return isDuration(that) && equals2(this, that);
  },
  toString() {
    switch (this.value._tag) {
      case "Infinity":
        return "Infinity";
      case "NegativeInfinity":
        return "-Infinity";
      case "Nanos":
        return `${this.value.nanos} nanos`;
      case "Millis":
        return `${this.value.millis} millis`;
    }
  },
  toJSON() {
    switch (this.value._tag) {
      case "Millis":
        return {
          _id: "Duration",
          _tag: "Millis",
          millis: this.value.millis
        };
      case "Nanos":
        return {
          _id: "Duration",
          _tag: "Nanos",
          nanos: String(this.value.nanos)
        };
      case "Infinity":
        return {
          _id: "Duration",
          _tag: "Infinity"
        };
      case "NegativeInfinity":
        return {
          _id: "Duration",
          _tag: "NegativeInfinity"
        };
    }
  },
  [NodeInspectSymbol]() {
    return this.toJSON();
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var make2 = (input) => {
  const duration = Object.create(DurationProto);
  if (isNumber(input)) {
    if (isNaN(input) || input === 0 || Object.is(input, -0)) {
      duration.value = zeroDurationValue;
    } else if (!Number.isFinite(input)) {
      duration.value = input > 0 ? infinityDurationValue : negativeInfinityDurationValue;
    } else if (!Number.isInteger(input)) {
      duration.value = {
        _tag: "Nanos",
        nanos: BigInt(Math.round(input * 1e6))
      };
    } else {
      duration.value = {
        _tag: "Millis",
        millis: input
      };
    }
  } else if (input === bigint0) {
    duration.value = zeroDurationValue;
  } else {
    duration.value = {
      _tag: "Nanos",
      nanos: input
    };
  }
  return duration;
};
var isDuration = (u) => hasProperty(u, TypeId3);
var zero = /* @__PURE__ */ make2(0);
var infinity = /* @__PURE__ */ make2(Infinity);
var negativeInfinity = /* @__PURE__ */ make2(-Infinity);
var nanos = (nanos2) => make2(nanos2);
var micros = (micros2) => make2(micros2 * bigint1e3);
var millis = (millis2) => make2(millis2);
var seconds = (seconds2) => make2(seconds2 * 1e3);
var minutes = (minutes2) => make2(minutes2 * 6e4);
var hours = (hours2) => make2(hours2 * 36e5);
var days = (days2) => make2(days2 * 864e5);
var weeks = (weeks2) => make2(weeks2 * 6048e5);
var toMillis = (self) => match(fromInputUnsafe(self), {
  onMillis: identity,
  onNanos: (nanos2) => Number(nanos2) / 1e6,
  onInfinity: () => Infinity,
  onNegativeInfinity: () => -Infinity
});
var toNanosUnsafe = (self) => {
  switch (self.value._tag) {
    case "Infinity":
    case "NegativeInfinity":
      throw new Error("Cannot convert infinite duration to nanos");
    case "Nanos":
      return self.value.nanos;
    case "Millis":
      return BigInt(Math.round(self.value.millis * 1e6));
  }
};
var match = /* @__PURE__ */ dual(2, (self, options) => {
  switch (self.value._tag) {
    case "Millis":
      return options.onMillis(self.value.millis);
    case "Nanos":
      return options.onNanos(self.value.nanos);
    case "Infinity":
      return options.onInfinity();
    case "NegativeInfinity":
      return (options.onNegativeInfinity ?? options.onInfinity)();
  }
});
var matchPair = /* @__PURE__ */ dual(3, (self, that, options) => {
  if (self.value._tag === "Infinity" || self.value._tag === "NegativeInfinity" || that.value._tag === "Infinity" || that.value._tag === "NegativeInfinity") return options.onInfinity(self, that);
  if (self.value._tag === "Millis") {
    return that.value._tag === "Millis" ? options.onMillis(self.value.millis, that.value.millis) : options.onNanos(toNanosUnsafe(self), that.value.nanos);
  } else {
    return options.onNanos(self.value.nanos, toNanosUnsafe(that));
  }
});
var Equivalence = (self, that) => matchPair(self, that, {
  onMillis: (self2, that2) => self2 === that2,
  onNanos: (self2, that2) => self2 === that2,
  onInfinity: (self2, that2) => self2.value._tag === that2.value._tag
});
var subtract = /* @__PURE__ */ dual(2, (self, that) => matchPair(self, that, {
  onMillis: (self2, that2) => make2(self2 - that2),
  onNanos: (self2, that2) => make2(self2 - that2),
  onInfinity: (self2, that2) => {
    const s = self2.value._tag;
    const t = that2.value._tag;
    if (s === "Infinity") return t === "Infinity" ? zero : infinity;
    if (s === "NegativeInfinity") return t === "NegativeInfinity" ? zero : negativeInfinity;
    return t === "Infinity" ? negativeInfinity : infinity;
  }
}));
var equals2 = /* @__PURE__ */ dual(2, (self, that) => Equivalence(self, that));

// ../../node_modules/effect/dist/ServiceMap.js
var ServiceMap_exports = {};
__export(ServiceMap_exports, {
  Reference: () => Reference,
  Service: () => Service,
  add: () => add,
  addOrOmit: () => addOrOmit,
  empty: () => empty,
  get: () => get,
  getOption: () => getOption,
  getOrElse: () => getOrElse,
  getOrUndefined: () => getOrUndefined,
  getReferenceUnsafe: () => getReferenceUnsafe,
  getUnsafe: () => getUnsafe2,
  isReference: () => isReference,
  isService: () => isService,
  isServiceMap: () => isServiceMap,
  make: () => make3,
  makeUnsafe: () => makeUnsafe,
  merge: () => merge,
  mergeAll: () => mergeAll,
  omit: () => omit,
  pick: () => pick
});
var ServiceTypeId = "~effect/ServiceMap/Service";
var Service = function() {
  const prevLimit = Error.stackTraceLimit;
  Error.stackTraceLimit = 2;
  const err = new Error();
  Error.stackTraceLimit = prevLimit;
  function KeyClass() {
  }
  const self = KeyClass;
  Object.setPrototypeOf(self, ServiceProto);
  Object.defineProperty(self, "stack", {
    get() {
      return err.stack;
    }
  });
  if (arguments.length > 0) {
    self.key = arguments[0];
    if (arguments[1]?.defaultValue) {
      self[ReferenceTypeId] = ReferenceTypeId;
      self.defaultValue = arguments[1].defaultValue;
    }
    return self;
  }
  return function(key, options) {
    self.key = key;
    if (options?.make) {
      ;
      self.make = options.make;
    }
    return self;
  };
};
var ServiceProto = {
  [ServiceTypeId]: {
    _Service: (_) => _,
    _Identifier: (_) => _
  },
  ...PipeInspectableProto,
  ...YieldableProto,
  toJSON() {
    return {
      _id: "Service",
      key: this.key,
      stack: this.stack
    };
  },
  asEffect() {
    const fn3 = this.asEffect = constant(withFiber((fiber3) => exitSucceed(get(fiber3.services, this))));
    return fn3();
  },
  of(self) {
    return self;
  },
  serviceMap(self) {
    return make3(this, self);
  },
  use(f) {
    return withFiber((fiber3) => f(get(fiber3.services, this)));
  },
  useSync(f) {
    return withFiber((fiber3) => exitSucceed(f(get(fiber3.services, this))));
  }
};
var ReferenceTypeId = "~effect/ServiceMap/Reference";
var TypeId4 = "~effect/ServiceMap";
var makeUnsafe = (mapUnsafe) => {
  const self = Object.create(Proto);
  self.mapUnsafe = mapUnsafe;
  return self;
};
var Proto = {
  ...PipeInspectableProto,
  [TypeId4]: {
    _Services: (_) => _
  },
  toJSON() {
    return {
      _id: "ServiceMap",
      services: Array.from(this.mapUnsafe).map(([key, value]) => ({
        key,
        value
      }))
    };
  },
  [symbol2](that) {
    if (!isServiceMap(that) || this.mapUnsafe.size !== that.mapUnsafe.size) return false;
    for (const k of this.mapUnsafe.keys()) {
      if (!that.mapUnsafe.has(k) || !equals(this.mapUnsafe.get(k), that.mapUnsafe.get(k))) {
        return false;
      }
    }
    return true;
  },
  [symbol]() {
    return number(this.mapUnsafe.size);
  }
};
var isServiceMap = (u) => hasProperty(u, TypeId4);
var isService = (u) => hasProperty(u, ServiceTypeId);
var isReference = (u) => hasProperty(u, ReferenceTypeId);
var empty = () => emptyServiceMap2;
var emptyServiceMap2 = /* @__PURE__ */ makeUnsafe(/* @__PURE__ */ new Map());
var make3 = (key, service3) => makeUnsafe(/* @__PURE__ */ new Map([[key.key, service3]]));
var add = /* @__PURE__ */ dual(3, (self, key, service3) => {
  const map3 = new Map(self.mapUnsafe);
  map3.set(key.key, service3);
  return makeUnsafe(map3);
});
var addOrOmit = /* @__PURE__ */ dual(3, (self, key, service3) => {
  const map3 = new Map(self.mapUnsafe);
  if (service3._tag === "None") {
    map3.delete(key.key);
  } else {
    map3.set(key.key, service3.value);
  }
  return makeUnsafe(map3);
});
var getOrElse = /* @__PURE__ */ dual(3, (self, key, orElse) => {
  if (self.mapUnsafe.has(key.key)) {
    return self.mapUnsafe.get(key.key);
  }
  return isReference(key) ? getDefaultValue(key) : orElse();
});
var getOrUndefined = /* @__PURE__ */ dual(2, (self, key) => self.mapUnsafe.get(key.key));
var getUnsafe2 = /* @__PURE__ */ dual(2, (self, service3) => {
  if (!self.mapUnsafe.has(service3.key)) {
    if (ReferenceTypeId in service3) return getDefaultValue(service3);
    throw serviceNotFoundError(service3);
  }
  return self.mapUnsafe.get(service3.key);
});
var get = getUnsafe2;
var getReferenceUnsafe = (self, service3) => {
  if (!self.mapUnsafe.has(service3.key)) {
    return getDefaultValue(service3);
  }
  return self.mapUnsafe.get(service3.key);
};
var defaultValueCacheKey = "~effect/ServiceMap/defaultValue";
var getDefaultValue = (ref) => {
  if (defaultValueCacheKey in ref) {
    return ref[defaultValueCacheKey];
  }
  return ref[defaultValueCacheKey] = ref.defaultValue();
};
var serviceNotFoundError = (service3) => {
  const error = new Error(`Service not found${service3.key ? `: ${String(service3.key)}` : ""}`);
  if (service3.stack) {
    const lines = service3.stack.split("\n");
    if (lines.length > 2) {
      const afterAt = lines[2].match(/at (.*)/);
      if (afterAt) {
        error.message = error.message + ` (defined at ${afterAt[1]})`;
      }
    }
  }
  if (error.stack) {
    const lines = error.stack.split("\n");
    lines.splice(1, 3);
    error.stack = lines.join("\n");
  }
  return error;
};
var getOption = /* @__PURE__ */ dual(2, (self, service3) => {
  if (self.mapUnsafe.has(service3.key)) {
    return some2(self.mapUnsafe.get(service3.key));
  }
  return isReference(service3) ? some2(getDefaultValue(service3)) : none2();
});
var merge = /* @__PURE__ */ dual(2, (self, that) => {
  if (self.mapUnsafe.size === 0) return that;
  if (that.mapUnsafe.size === 0) return self;
  const map3 = new Map(self.mapUnsafe);
  that.mapUnsafe.forEach((value, key) => map3.set(key, value));
  return makeUnsafe(map3);
});
var mergeAll = (...ctxs) => {
  const map3 = /* @__PURE__ */ new Map();
  for (let i = 0; i < ctxs.length; i++) {
    ctxs[i].mapUnsafe.forEach((value, key) => {
      map3.set(key, value);
    });
  }
  return makeUnsafe(map3);
};
var pick = (...services3) => (self) => {
  const map3 = /* @__PURE__ */ new Map();
  const keySet = new Set(services3.map((key) => key.key));
  self.mapUnsafe.forEach((value, key) => {
    if (keySet.has(key)) {
      map3.set(key, value);
    }
  });
  return makeUnsafe(map3);
};
var omit = (...keys) => (self) => {
  const map3 = new Map(self.mapUnsafe);
  for (let i = 0; i < keys.length; i++) {
    map3.delete(keys[i].key);
  }
  return makeUnsafe(map3);
};
var Reference = Service;

// ../../node_modules/effect/dist/Scheduler.js
var Scheduler = /* @__PURE__ */ Reference("effect/Scheduler", {
  defaultValue: () => new MixedScheduler()
});
var setImmediate2 = "setImmediate" in globalThis ? (f) => {
  const timer = globalThis.setImmediate(f);
  return () => globalThis.clearImmediate(timer);
} : (f) => {
  const timer = setTimeout(f, 0);
  return () => clearTimeout(timer);
};
var PriorityBuckets = class {
  buckets = [];
  scheduleTask(task, priority) {
    const buckets = this.buckets;
    const len = buckets.length;
    let bucket;
    let index = 0;
    for (; index < len; index++) {
      if (buckets[index][0] > priority) break;
      bucket = buckets[index];
    }
    if (bucket && bucket[0] === priority) {
      bucket[1].push(task);
    } else if (index === len) {
      buckets.push([priority, [task]]);
    } else {
      buckets.splice(index, 0, [priority, [task]]);
    }
  }
  drain() {
    const buckets = this.buckets;
    this.buckets = [];
    return buckets;
  }
};
var MixedScheduler = class {
  tasks = /* @__PURE__ */ new PriorityBuckets();
  running = void 0;
  executionMode;
  setImmediate;
  constructor(executionMode = "async", setImmediateFn = setImmediate2) {
    this.executionMode = executionMode;
    this.setImmediate = setImmediateFn;
  }
  /**
   * @since 2.0.0
   */
  scheduleTask(task, priority) {
    this.tasks.scheduleTask(task, priority);
    if (this.running === void 0) {
      this.running = this.setImmediate(this.afterScheduled);
    }
  }
  /**
   * @since 2.0.0
   */
  afterScheduled = () => {
    this.running = void 0;
    this.runTasks();
  };
  /**
   * @since 2.0.0
   */
  runTasks() {
    const buckets = this.tasks.drain();
    for (let i = 0; i < buckets.length; i++) {
      const toRun = buckets[i][1];
      for (let j = 0; j < toRun.length; j++) {
        toRun[j]();
      }
    }
  }
  /**
   * @since 2.0.0
   */
  shouldYield(fiber3) {
    return fiber3.currentOpCount >= fiber3.maxOpsBeforeYield;
  }
  /**
   * @since 2.0.0
   */
  flush() {
    while (this.tasks.buckets.length > 0) {
      if (this.running !== void 0) {
        this.running();
        this.running = void 0;
      }
      this.runTasks();
    }
  }
};
var MaxOpsBeforeYield = /* @__PURE__ */ Reference("effect/Scheduler/MaxOpsBeforeYield", {
  defaultValue: () => 2048
});

// ../../node_modules/effect/dist/Tracer.js
var ParentSpanKey = "effect/Tracer/ParentSpan";
var ParentSpan = class extends (/* @__PURE__ */ Service()(ParentSpanKey)) {
};
var make4 = (options) => options;
var DisablePropagation = /* @__PURE__ */ Reference("effect/Tracer/DisablePropagation", {
  defaultValue: constFalse
});
var CurrentTraceLevel = /* @__PURE__ */ Reference("effect/Tracer/CurrentTraceLevel", {
  defaultValue: () => "Info"
});
var MinimumTraceLevel = /* @__PURE__ */ Reference("effect/Tracer/MinimumTraceLevel", {
  defaultValue: () => "All"
});
var TracerKey = "effect/Tracer";
var Tracer = /* @__PURE__ */ Reference(TracerKey, {
  defaultValue: () => make4({
    span: (options) => new NativeSpan(options)
  })
});
var NativeSpan = class {
  _tag = "Span";
  spanId;
  traceId = "native";
  sampled;
  name;
  parent;
  annotations;
  links;
  startTime;
  kind;
  status;
  attributes;
  events = [];
  constructor(options) {
    this.name = options.name;
    this.parent = options.parent;
    this.annotations = options.annotations;
    this.links = options.links;
    this.startTime = options.startTime;
    this.kind = options.kind;
    this.sampled = options.sampled;
    this.status = {
      _tag: "Started",
      startTime: options.startTime
    };
    this.attributes = /* @__PURE__ */ new Map();
    this.traceId = options.parent?.traceId ?? randomHexString(32);
    this.spanId = randomHexString(16);
  }
  end(endTime, exit3) {
    this.status = {
      _tag: "Ended",
      endTime,
      exit: exit3,
      startTime: this.status.startTime
    };
  }
  attribute(key, value) {
    this.attributes.set(key, value);
  }
  event(name, startTime, attributes) {
    this.events.push([name, startTime, attributes ?? {}]);
  }
  addLinks(links) {
    this.links.push(...links);
  }
};
var randomHexString = /* @__PURE__ */ (function() {
  const characters = "abcdef0123456789";
  const charactersLength = characters.length;
  return function(length) {
    let result3 = "";
    for (let i = 0; i < length; i++) {
      result3 += characters.charAt(Math.floor(Math.random() * charactersLength));
    }
    return result3;
  };
})();

// ../../node_modules/effect/dist/References.js
var CurrentConcurrency = /* @__PURE__ */ Reference("effect/References/CurrentConcurrency", {
  defaultValue: () => "unbounded"
});
var CurrentStackFrame = /* @__PURE__ */ Reference("effect/References/CurrentStackFrame", {
  defaultValue: constUndefined
});
var TracerEnabled = /* @__PURE__ */ Reference("effect/References/TracerEnabled", {
  defaultValue: constTrue
});
var TracerTimingEnabled = /* @__PURE__ */ Reference("effect/References/TracerTimingEnabled", {
  defaultValue: constTrue
});
var TracerSpanAnnotations = /* @__PURE__ */ Reference("effect/References/TracerSpanAnnotations", {
  defaultValue: () => ({})
});
var TracerSpanLinks = /* @__PURE__ */ Reference("effect/References/TracerSpanLinks", {
  defaultValue: () => []
});
var CurrentLogAnnotations = /* @__PURE__ */ Reference("effect/References/CurrentLogAnnotations", {
  defaultValue: () => ({})
});
var CurrentLogLevel = /* @__PURE__ */ Reference("effect/References/CurrentLogLevel", {
  defaultValue: () => "Info"
});
var MinimumLogLevel = /* @__PURE__ */ Reference("effect/References/MinimumLogLevel", {
  defaultValue: () => "Info"
});
var CurrentLogSpans = /* @__PURE__ */ Reference("effect/References/CurrentLogSpans", {
  defaultValue: () => []
});

// ../../node_modules/effect/dist/internal/metric.js
var FiberRuntimeMetricsKey = "effect/observability/Metric/FiberRuntimeMetricsKey";

// ../../node_modules/effect/dist/internal/tracer.js
var addSpanStackTrace = (options) => {
  if (options?.captureStackTrace === false) {
    return options;
  } else if (options?.captureStackTrace !== void 0 && typeof options.captureStackTrace !== "boolean") {
    return options;
  }
  const limit = Error.stackTraceLimit;
  Error.stackTraceLimit = 3;
  const traceError = new Error();
  Error.stackTraceLimit = limit;
  return {
    ...options,
    captureStackTrace: spanCleaner(() => traceError.stack)
  };
};
var makeStackCleaner = (line) => (stack) => {
  let cache;
  return () => {
    if (cache !== void 0) return cache;
    const trace = stack();
    if (!trace) return void 0;
    const lines = trace.split("\n");
    if (lines[line] !== void 0) {
      cache = lines[line].trim();
      return cache;
    }
  };
};
var spanCleaner = /* @__PURE__ */ makeStackCleaner(3);

// ../../node_modules/effect/dist/internal/version.js
var version = "dev";

// ../../node_modules/effect/dist/internal/effect.js
var Interrupt = class extends ReasonBase {
  fiberId;
  constructor(fiberId3, annotations = constEmptyAnnotations) {
    super("Interrupt", annotations, "Interrupted");
    this.fiberId = fiberId3;
  }
  toString() {
    return `Interrupt(${this.fiberId})`;
  }
  toJSON() {
    return {
      _tag: "Interrupt",
      fiberId: this.fiberId
    };
  }
  [symbol2](that) {
    return isInterruptReason(that) && this.fiberId === that.fiberId && this.annotations === that.annotations;
  }
  [symbol]() {
    return combine(string(`${this._tag}:${this.fiberId}`))(random(this.annotations));
  }
};
var causeInterrupt = (fiberId3) => new CauseImpl([new Interrupt(fiberId3)]);
var findFail = (self) => {
  const reason = self.reasons.find(isFailReason);
  return reason ? succeed2(reason) : fail2(self);
};
var findError = (self) => {
  for (let i = 0; i < self.reasons.length; i++) {
    const reason = self.reasons[i];
    if (reason._tag === "Fail") {
      return succeed2(reason.error);
    }
  }
  return fail2(self);
};
var findDefect = (self) => {
  const reason = self.reasons.find(isDieReason);
  return reason ? succeed2(reason.defect) : fail2(self);
};
var hasInterrupts = (self) => self.reasons.some(isInterruptReason);
var causeFilterInterruptors = (self) => {
  let interruptors;
  for (let i = 0; i < self.reasons.length; i++) {
    const f = self.reasons[i];
    if (f._tag !== "Interrupt") continue;
    interruptors ??= /* @__PURE__ */ new Set();
    if (f.fiberId !== void 0) {
      interruptors.add(f.fiberId);
    }
  }
  return interruptors ? succeed2(interruptors) : fail2(self);
};
var causeCombine = /* @__PURE__ */ dual(2, (self, that) => {
  if (self.reasons.length === 0) {
    return that;
  } else if (that.reasons.length === 0) {
    return self;
  }
  const newCause = new CauseImpl(union(self.reasons, that.reasons));
  return equals(self, newCause) ? self : newCause;
});
var causePartition = (self) => {
  const obj = {
    Fail: [],
    Die: [],
    Interrupt: []
  };
  for (let i = 0; i < self.reasons.length; i++) {
    obj[self.reasons[i]._tag].push(self.reasons[i]);
  }
  return obj;
};
var causeSquash = (self) => {
  const partitioned = causePartition(self);
  if (partitioned.Fail.length > 0) {
    return partitioned.Fail[0].error;
  } else if (partitioned.Die.length > 0) {
    return partitioned.Die[0].defect;
  } else if (partitioned.Interrupt.length > 0) {
    return new globalThis.Error("All fibers interrupted without error");
  }
  return new globalThis.Error("Empty cause");
};
var causePrettyErrors = (self) => {
  const errors = [];
  const interrupts = [];
  if (self.reasons.length === 0) return errors;
  const prevStackLimit = Error.stackTraceLimit;
  Error.stackTraceLimit = 1;
  for (const failure of self.reasons) {
    if (failure._tag === "Interrupt") {
      interrupts.push(failure);
      continue;
    }
    errors.push(causePrettyError(failure._tag === "Die" ? failure.defect : failure.error, failure.annotations));
  }
  if (errors.length === 0) {
    const cause = new Error("The fiber was interrupted by:");
    cause.name = "InterruptCause";
    cause.stack = interruptCauseStack(cause, interrupts);
    const error = new globalThis.Error("All fibers interrupted without error", {
      cause
    });
    error.name = "InterruptError";
    error.stack = `${error.name}: ${error.message}`;
    errors.push(causePrettyError(error, interrupts[0].annotations));
  }
  ;
  Error.stackTraceLimit = prevStackLimit;
  return errors;
};
var causePrettyError = (original, annotations) => {
  const kind = typeof original;
  let error;
  if (original && kind === "object") {
    error = new globalThis.Error(causePrettyMessage(original), {
      cause: original.cause ? causePrettyError(original.cause) : void 0
    });
    if (typeof original.name === "string") {
      error.name = original.name;
    }
    if (typeof original.stack === "string") {
      error.stack = cleanErrorStack(original.stack, error, annotations);
    } else {
      const stack = `${error.name}: ${error.message}`;
      error.stack = annotations ? addStackAnnotations(stack, annotations) : stack;
    }
    for (const key of Object.keys(original)) {
      if (!(key in error)) {
        ;
        error[key] = original[key];
      }
    }
  } else {
    error = new globalThis.Error(!original ? `Unknown error: ${original}` : kind === "string" ? original : formatJson(original));
  }
  return error;
};
var causePrettyMessage = (u) => {
  if (typeof u.message === "string") {
    return u.message;
  } else if (typeof u.toString === "function" && u.toString !== Object.prototype.toString && u.toString !== Array.prototype.toString) {
    try {
      return u.toString();
    } catch {
    }
  }
  return formatJson(u);
};
var locationRegExp = /\((.*)\)/g;
var cleanErrorStack = (stack, error, annotations) => {
  const message = `${error.name}: ${error.message}`;
  const lines = (stack.startsWith(message) ? stack.slice(message.length) : stack).split("\n");
  const out = [message];
  for (let i = 1; i < lines.length; i++) {
    if (/(?:Generator\.next|~effect\/Effect)/.test(lines[i])) {
      break;
    }
    out.push(lines[i]);
  }
  return annotations ? addStackAnnotations(out.join("\n"), annotations) : out.join("\n");
};
var addStackAnnotations = (stack, annotations) => {
  const frame = annotations?.get(StackTraceKey.key);
  if (frame) {
    stack = `${stack}
${currentStackTrace(frame)}`;
  }
  return stack;
};
var interruptCauseStack = (error, interrupts) => {
  const out = [`${error.name}: ${error.message}`];
  for (const current of interrupts) {
    const fiberId3 = current.fiberId !== void 0 ? `#${current.fiberId}` : "unknown";
    const frame = current.annotations.get(InterruptorStackTrace.key);
    out.push(`    at fiber (${fiberId3})`);
    if (frame) out.push(currentStackTrace(frame));
  }
  return out.join("\n");
};
var currentStackTrace = (frame) => {
  const out = [];
  let current = frame;
  let i = 0;
  while (current && i < 10) {
    const stack = current.stack();
    if (stack) {
      const locationMatchAll = stack.matchAll(locationRegExp);
      let match4 = false;
      for (const [, location] of locationMatchAll) {
        match4 = true;
        out.push(`    at ${current.name} (${location})`);
      }
      if (!match4) {
        out.push(`    at ${current.name} (${stack.replace(/^at /, "")})`);
      }
    } else {
      out.push(`    at ${current.name}`);
    }
    current = current.parent;
    i++;
  }
  return out.join("\n");
};
var causePretty = (cause) => causePrettyErrors(cause).map((e) => e.cause ? `${e.stack} {
${renderErrorCause(e.cause, "  ")}
}` : e.stack).join("\n");
var renderErrorCause = (cause, prefix) => {
  const lines = cause.stack.split("\n");
  let stack = `${prefix}[cause]: ${lines[0]}`;
  for (let i = 1, len = lines.length; i < len; i++) {
    stack += `
${prefix}${lines[i]}`;
  }
  if (cause.cause) {
    stack += ` {
${renderErrorCause(cause.cause, `${prefix}  `)}
${prefix}}`;
  }
  return stack;
};
var FiberTypeId = `~effect/Fiber/${version}`;
var fiberVariance = {
  _A: identity,
  _E: identity
};
var fiberIdStore = {
  id: 0
};
var getCurrentFiber = () => globalThis[currentFiberTypeId];
var keepAlive = /* @__PURE__ */ (() => {
  let count = 0;
  let running = void 0;
  return {
    increment() {
      count++;
      running ??= globalThis.setInterval(constVoid, 2147483647);
    },
    decrement() {
      count--;
      if (count === 0 && running !== void 0) {
        globalThis.clearInterval(running);
        running = void 0;
      }
    }
  };
})();
var FiberImpl = class {
  constructor(services3, interruptible3 = true) {
    this[FiberTypeId] = fiberVariance;
    this.setServices(services3);
    this.id = ++fiberIdStore.id;
    this.currentOpCount = 0;
    this.currentLoopCount = 0;
    this.interruptible = interruptible3;
    this._stack = [];
    this._observers = [];
    this._exit = void 0;
    this._children = void 0;
    this._interruptedCause = void 0;
    this._yielded = void 0;
  }
  [FiberTypeId];
  id;
  interruptible;
  currentOpCount;
  currentLoopCount;
  _stack;
  _observers;
  _exit;
  _currentExit;
  _children;
  _interruptedCause;
  _yielded;
  // set in setServices
  services;
  currentScheduler;
  currentTracerContext;
  currentSpan;
  currentLogLevel;
  minimumLogLevel;
  currentStackFrame;
  runtimeMetrics;
  maxOpsBeforeYield;
  getRef(ref) {
    return getReferenceUnsafe(this.services, ref);
  }
  addObserver(cb) {
    if (this._exit) {
      cb(this._exit);
      return constVoid;
    }
    this._observers.push(cb);
    return () => {
      const index = this._observers.indexOf(cb);
      if (index >= 0) {
        this._observers.splice(index, 1);
      }
    };
  }
  interruptUnsafe(fiberId3, annotations) {
    if (this._exit) {
      return;
    }
    let cause = causeInterrupt(fiberId3);
    if (this.currentStackFrame) {
      cause = causeAnnotate(cause, make3(StackTraceKey, this.currentStackFrame));
    }
    if (annotations) {
      cause = causeAnnotate(cause, annotations);
    }
    this._interruptedCause = this._interruptedCause ? causeCombine(this._interruptedCause, cause) : cause;
    if (this.interruptible) {
      this.evaluate(failCause(this._interruptedCause));
    }
  }
  pollUnsafe() {
    return this._exit;
  }
  evaluate(effect2) {
    this.runtimeMetrics?.recordFiberStart(this.services);
    if (this._exit) {
      return;
    } else if (this._yielded !== void 0) {
      const yielded = this._yielded;
      this._yielded = void 0;
      yielded();
    }
    const exit3 = this.runLoop(effect2);
    if (exit3 === Yield) {
      return;
    }
    const interruptChildren = fiberMiddleware.interruptChildren && fiberMiddleware.interruptChildren(this);
    if (interruptChildren !== void 0) {
      return this.evaluate(flatMap(interruptChildren, () => exit3));
    }
    this._exit = exit3;
    this.runtimeMetrics?.recordFiberEnd(this.services, this._exit);
    for (let i = 0; i < this._observers.length; i++) {
      this._observers[i](exit3);
    }
    this._observers.length = 0;
  }
  runLoop(effect2) {
    const prevFiber = globalThis[currentFiberTypeId];
    globalThis[currentFiberTypeId] = this;
    let yielding = false;
    let current = effect2;
    this.currentOpCount = 0;
    const currentLoop = ++this.currentLoopCount;
    try {
      while (true) {
        this.currentOpCount++;
        if (!yielding && this.currentScheduler.shouldYield(this)) {
          yielding = true;
          const prev = current;
          current = flatMap(yieldNow, () => prev);
        }
        current = this.currentTracerContext ? this.currentTracerContext(current, this) : current[evaluate](this);
        if (currentLoop !== this.currentLoopCount) {
          return Yield;
        } else if (current === Yield) {
          const yielded = this._yielded;
          if (ExitTypeId in yielded) {
            this._yielded = void 0;
            return yielded;
          }
          return Yield;
        }
      }
    } catch (error) {
      if (!hasProperty(current, evaluate)) {
        return exitDie(`Fiber.runLoop: Not a valid effect: ${String(current)}`);
      }
      return this.runLoop(exitDie(error));
    } finally {
      ;
      globalThis[currentFiberTypeId] = prevFiber;
    }
  }
  getCont(symbol3) {
    while (true) {
      const op = this._stack.pop();
      if (!op) return void 0;
      const cont = op[contAll] && op[contAll](this);
      if (cont) {
        ;
        cont[symbol3] = cont;
        return cont;
      }
      if (op[symbol3]) return op;
    }
  }
  yieldWith(value) {
    this._yielded = value;
    return Yield;
  }
  children() {
    return this._children ??= /* @__PURE__ */ new Set();
  }
  pipe() {
    return pipeArguments(this, arguments);
  }
  setServices(services3) {
    this.services = services3;
    this.currentScheduler = this.getRef(Scheduler);
    this.currentSpan = services3.mapUnsafe.get(ParentSpanKey);
    this.currentLogLevel = this.getRef(CurrentLogLevel);
    this.minimumLogLevel = this.getRef(MinimumLogLevel);
    this.currentStackFrame = services3.mapUnsafe.get(CurrentStackFrame.key);
    this.maxOpsBeforeYield = this.getRef(MaxOpsBeforeYield);
    this.runtimeMetrics = services3.mapUnsafe.get(FiberRuntimeMetricsKey);
    const currentTracer = services3.mapUnsafe.get(TracerKey);
    this.currentTracerContext = currentTracer ? currentTracer["context"] : void 0;
  }
  get currentSpanLocal() {
    return this.currentSpan?._tag === "Span" ? this.currentSpan : void 0;
  }
};
var fiberMiddleware = {
  interruptChildren: void 0
};
var fiberStackAnnotations = (fiber3) => {
  if (!fiber3.currentStackFrame) return void 0;
  const annotations = /* @__PURE__ */ new Map();
  annotations.set(StackTraceKey.key, fiber3.currentStackFrame);
  return makeUnsafe(annotations);
};
var fiberInterruptChildren = (fiber3) => {
  if (fiber3._children === void 0 || fiber3._children.size === 0) {
    return void 0;
  }
  return fiberInterruptAll(fiber3._children);
};
var fiberAwait = (self) => {
  const impl = self;
  if (impl._exit) return succeed3(impl._exit);
  return callback((resume) => {
    if (impl._exit) return resume(succeed3(impl._exit));
    return sync(self.addObserver((exit3) => resume(succeed3(exit3))));
  });
};
var fiberAwaitAll = (self) => callback((resume) => {
  const iter = self[Symbol.iterator]();
  const exits = [];
  let cancel = void 0;
  function loop() {
    let result3 = iter.next();
    while (!result3.done) {
      if (result3.value._exit) {
        exits.push(result3.value._exit);
        result3 = iter.next();
        continue;
      }
      cancel = result3.value.addObserver((exit3) => {
        exits.push(exit3);
        loop();
      });
      return;
    }
    resume(succeed3(exits));
  }
  loop();
  return sync(() => cancel?.());
});
var fiberInterrupt = (self) => withFiber((fiber3) => fiberInterruptAs(self, fiber3.id));
var fiberInterruptAs = /* @__PURE__ */ dual(2, (self, fiberId3) => withFiber((parent) => {
  self.interruptUnsafe(fiberId3, fiberStackAnnotations(parent));
  return asVoid(fiberAwait(self));
}));
var fiberInterruptAll = (fibers) => withFiber((parent) => {
  const annotations = fiberStackAnnotations(parent);
  for (const fiber3 of fibers) {
    fiber3.interruptUnsafe(parent.id, annotations);
  }
  return asVoid(fiberAwaitAll(fibers));
});
var succeed3 = exitSucceed;
var failCause = exitFailCause;
var fail3 = exitFail;
var sync = /* @__PURE__ */ makePrimitive({
  op: "Sync",
  [evaluate](fiber3) {
    const value = this[args]();
    const cont = fiber3.getCont(contA);
    return cont ? cont[contA](value, fiber3) : fiber3.yieldWith(exitSucceed(value));
  }
});
var suspend = /* @__PURE__ */ makePrimitive({
  op: "Suspend",
  [evaluate](_fiber) {
    return this[args]();
  }
});
var fromYieldable = (yieldable) => yieldable.asEffect();
var fromOption2 = fromYieldable;
var fromResult = fromYieldable;
var fromNullishOr = (value) => value == null ? fail3(new NoSuchElementError()) : succeed3(value);
var yieldNowWith = /* @__PURE__ */ makePrimitive({
  op: "Yield",
  [evaluate](fiber3) {
    let resumed = false;
    fiber3.currentScheduler.scheduleTask(() => {
      if (resumed) return;
      fiber3.evaluate(exitVoid);
    }, this[args] ?? 0);
    return fiber3.yieldWith(() => {
      resumed = true;
    });
  }
});
var yieldNow = /* @__PURE__ */ yieldNowWith(0);
var succeedSome = (a) => succeed3(some2(a));
var succeedNone = /* @__PURE__ */ succeed3(/* @__PURE__ */ none2());
var failCauseSync = (evaluate2) => suspend(() => failCause(internalCall(evaluate2)));
var die = (defect) => exitDie(defect);
var failSync = (error) => suspend(() => fail3(internalCall(error)));
var void_ = /* @__PURE__ */ succeed3(void 0);
var try_ = (options) => suspend(() => {
  try {
    return succeed3(internalCall(options.try));
  } catch (err) {
    return fail3(internalCall(() => options.catch(err)));
  }
});
var promise = (evaluate2) => callbackOptions(function(resume, signal) {
  internalCall(() => evaluate2(signal)).then((a) => resume(succeed3(a)), (e) => resume(die(e)));
}, evaluate2.length !== 0);
var tryPromise = (options) => {
  const f = typeof options === "function" ? options : options.try;
  const catcher = typeof options === "function" ? (cause) => new UnknownError(cause, "An error occurred in Effect.tryPromise") : options.catch;
  return callbackOptions(function(resume, signal) {
    try {
      internalCall(() => f(signal)).then((a) => resume(succeed3(a)), (e) => resume(fail3(internalCall(() => catcher(e)))));
    } catch (err) {
      resume(fail3(internalCall(() => catcher(err))));
    }
  }, eval.length !== 0);
};
var withFiberId = (f) => withFiber((fiber3) => f(fiber3.id));
var fiber = /* @__PURE__ */ withFiber(succeed3);
var fiberId = /* @__PURE__ */ withFiberId(succeed3);
var callbackOptions = /* @__PURE__ */ makePrimitive({
  op: "Async",
  single: false,
  [evaluate](fiber3) {
    const register = internalCall(() => this[args][0].bind(fiber3.currentScheduler));
    let resumed = false;
    let yielded = false;
    const controller = this[args][1] ? new AbortController() : void 0;
    const onCancel = register((effect2) => {
      if (resumed) return;
      resumed = true;
      if (yielded) {
        fiber3.evaluate(effect2);
      } else {
        yielded = effect2;
      }
    }, controller?.signal);
    if (yielded !== false) return yielded;
    yielded = true;
    keepAlive.increment();
    fiber3._yielded = () => {
      resumed = true;
      keepAlive.decrement();
    };
    if (controller === void 0 && onCancel === void 0) {
      return Yield;
    }
    fiber3._stack.push(asyncFinalizer(() => {
      resumed = true;
      controller?.abort();
      return onCancel ?? exitVoid;
    }));
    return Yield;
  }
});
var asyncFinalizer = /* @__PURE__ */ makePrimitive({
  op: "AsyncFinalizer",
  [contAll](fiber3) {
    if (fiber3.interruptible) {
      fiber3.interruptible = false;
      fiber3._stack.push(setInterruptibleTrue);
    }
  },
  [contE](cause, _fiber) {
    return hasInterrupts(cause) ? flatMap(this[args](), () => failCause(cause)) : failCause(cause);
  }
});
var callback = (register) => callbackOptions(register, register.length >= 2);
var never = /* @__PURE__ */ callback(constVoid);
var gen = (...args2) => suspend(() => fromIteratorUnsafe(args2.length === 1 ? args2[0]() : args2[1].call(args2[0].self)));
var fnUntraced = (body, ...pipeables) => {
  return pipeables.length === 0 ? function() {
    return suspend(() => fromIteratorUnsafe(body.apply(this, arguments)));
  } : function() {
    let effect2 = suspend(() => fromIteratorUnsafe(body.apply(this, arguments)));
    for (let i = 0; i < pipeables.length; i++) {
      effect2 = pipeables[i](effect2, ...arguments);
    }
    return effect2;
  };
};
var fnStackCleaner = /* @__PURE__ */ makeStackCleaner(2);
var fn = function() {
  const nameFirst = typeof arguments[0] === "string";
  const name = nameFirst ? arguments[0] : "Effect.fn";
  const spanOptions = nameFirst ? arguments[1] : void 0;
  const prevLimit = globalThis.Error.stackTraceLimit;
  globalThis.Error.stackTraceLimit = 2;
  const defError = new globalThis.Error();
  globalThis.Error.stackTraceLimit = prevLimit;
  if (nameFirst) {
    return (body, ...pipeables) => makeFn(name, body, defError, pipeables, nameFirst, spanOptions);
  }
  return makeFn(name, arguments[0], defError, Array.prototype.slice.call(arguments, 1), nameFirst, spanOptions);
};
var makeFn = (name, bodyOrOptions, defError, pipeables, addSpan, spanOptions) => {
  const body = typeof bodyOrOptions === "function" ? bodyOrOptions : pipeables.pop().bind(bodyOrOptions.self);
  return function(...args2) {
    let result3 = suspend(() => {
      const iter = body.apply(this, arguments);
      return isEffect(iter) ? iter : fromIteratorUnsafe(iter);
    });
    for (let i = 0; i < pipeables.length; i++) {
      result3 = pipeables[i](result3, ...args2);
    }
    if (!isEffect(result3)) {
      return result3;
    }
    const prevLimit = globalThis.Error.stackTraceLimit;
    globalThis.Error.stackTraceLimit = 2;
    const callError = new globalThis.Error();
    globalThis.Error.stackTraceLimit = prevLimit;
    return updateService(addSpan ? useSpan(name, spanOptions, (span2) => provideParentSpan(result3, span2)) : result3, CurrentStackFrame, (prev) => ({
      name,
      stack: fnStackCleaner(() => callError.stack),
      parent: {
        name: `${name} (definition)`,
        stack: fnStackCleaner(() => defError.stack),
        parent: prev
      }
    }));
  };
};
var fnUntracedEager = (body, ...pipeables) => pipeables.length === 0 ? function() {
  return fromIteratorEagerUnsafe(() => body.apply(this, arguments));
} : function() {
  let effect2 = fromIteratorEagerUnsafe(() => body.apply(this, arguments));
  for (const pipeable of pipeables) {
    effect2 = pipeable(effect2);
  }
  return effect2;
};
var fromIteratorEagerUnsafe = (evaluate2) => {
  try {
    const iterator = evaluate2();
    let value = void 0;
    while (true) {
      const state = iterator.next(value);
      if (state.done) {
        return succeed3(state.value);
      }
      const yieldable = state.value;
      const effect2 = yieldable.asEffect();
      const primitive = effect2;
      if (primitive && primitive._tag === "Success") {
        value = primitive.value;
        continue;
      } else if (primitive && primitive._tag === "Failure") {
        return effect2;
      } else {
        let isFirstExecution = true;
        return suspend(() => {
          if (isFirstExecution) {
            isFirstExecution = false;
            return flatMap(effect2, (value2) => fromIteratorUnsafe(iterator, value2));
          } else {
            return suspend(() => fromIteratorUnsafe(evaluate2()));
          }
        });
      }
    }
  } catch (error) {
    return die(error);
  }
};
var fromIteratorUnsafe = /* @__PURE__ */ makePrimitive({
  op: "Iterator",
  single: false,
  [contA](value, fiber3) {
    const iter = this[args][0];
    while (true) {
      const state = iter.next(value);
      if (state.done) return succeed3(state.value);
      const eff = state.value.asEffect();
      if (!effectIsExit(eff)) {
        fiber3._stack.push(this);
        return eff;
      } else if (eff._tag === "Failure") {
        return eff;
      }
      value = eff.value;
    }
  },
  [evaluate](fiber3) {
    return this[contA](this[args][1], fiber3);
  }
});
var as = /* @__PURE__ */ dual(2, (self, value) => {
  const b = succeed3(value);
  return flatMap(self, (_) => b);
});
var asSome = (self) => map(self, some2);
var flip = (self) => matchEffect(self, {
  onFailure: succeed3,
  onSuccess: fail3
});
var andThen = /* @__PURE__ */ dual(2, (self, f) => flatMap(self, (a) => isEffect(f) ? f : internalCall(() => f(a))));
var tap = /* @__PURE__ */ dual(2, (self, f) => flatMap(self, (a) => as(isEffect(f) ? f : internalCall(() => f(a)), a)));
var asVoid = (self) => flatMap(self, (_) => exitVoid);
var sandbox = (self) => catchCause(self, fail3);
var raceAll = (all3, options) => withFiber((parent) => callback((resume) => {
  const effects = fromIterable(all3);
  const len = effects.length;
  let doneCount = 0;
  let done4 = false;
  const fibers = /* @__PURE__ */ new Set();
  const failures = [];
  const onExit3 = (exit3, fiber3, i) => {
    doneCount++;
    if (exit3._tag === "Failure") {
      failures.push(...exit3.cause.reasons);
      if (doneCount >= len) {
        resume(failCause(causeFromReasons(failures)));
      }
      return;
    }
    const isWinner = !done4;
    done4 = true;
    resume(fibers.size === 0 ? exit3 : flatMap(uninterruptible(fiberInterruptAll(fibers)), () => exit3));
    if (isWinner && options?.onWinner) {
      options.onWinner({
        fiber: fiber3,
        index: i,
        parentFiber: parent
      });
    }
  };
  for (let i = 0; i < len; i++) {
    const fiber3 = forkUnsafe(parent, effects[i], true, true, false);
    fibers.add(fiber3);
    fiber3.addObserver((exit3) => {
      fibers.delete(fiber3);
      onExit3(exit3, fiber3, i);
    });
    if (done4) break;
  }
  return fiberInterruptAll(fibers);
}));
var raceAllFirst = (all3, options) => withFiber((parent) => callback((resume) => {
  let done4 = false;
  const fibers = /* @__PURE__ */ new Set();
  const onExit3 = (exit3) => {
    done4 = true;
    resume(fibers.size === 0 ? exit3 : flatMap(uninterruptible(fiberInterruptAll(fibers)), () => exit3));
  };
  let i = 0;
  for (const effect2 of all3) {
    if (done4) break;
    const index = i++;
    const fiber3 = forkUnsafe(parent, effect2, true, true, false);
    fibers.add(fiber3);
    fiber3.addObserver((exit3) => {
      fibers.delete(fiber3);
      const isWinner = !done4;
      onExit3(exit3);
      if (isWinner && options?.onWinner) {
        options.onWinner({
          fiber: fiber3,
          index,
          parentFiber: parent
        });
      }
    });
  }
  return fiberInterruptAll(fibers);
}));
var race = /* @__PURE__ */ dual((args2) => isEffect(args2[1]), (self, that, options) => raceAll([self, that], options));
var raceFirst = /* @__PURE__ */ dual((args2) => isEffect(args2[1]), (self, that, options) => raceAllFirst([self, that], options));
var flatMap = /* @__PURE__ */ dual(2, (self, f) => {
  const onSuccess = Object.create(OnSuccessProto);
  onSuccess[args] = self;
  onSuccess[contA] = f.length !== 1 ? (a) => f(a) : f;
  return onSuccess;
});
var OnSuccessProto = /* @__PURE__ */ makePrimitiveProto({
  op: "OnSuccess",
  [evaluate](fiber3) {
    fiber3._stack.push(this);
    return this[args];
  }
});
var matchCauseEffectEager = /* @__PURE__ */ dual(2, (self, options) => {
  if (effectIsExit(self)) {
    return self._tag === "Success" ? options.onSuccess(self.value) : options.onFailure(self.cause);
  }
  return matchCauseEffect(self, options);
});
var effectIsExit = (effect2) => ExitTypeId in effect2;
var flatMapEager = /* @__PURE__ */ dual(2, (self, f) => {
  if (effectIsExit(self)) {
    return self._tag === "Success" ? f(self.value) : self;
  }
  return flatMap(self, f);
});
var flatten = (self) => flatMap(self, identity);
var map = /* @__PURE__ */ dual(2, (self, f) => flatMap(self, (a) => succeed3(internalCall(() => f(a)))));
var mapEager = /* @__PURE__ */ dual(2, (self, f) => effectIsExit(self) ? exitMap(self, f) : map(self, f));
var mapErrorEager = /* @__PURE__ */ dual(2, (self, f) => effectIsExit(self) ? exitMapError(self, f) : mapError2(self, f));
var mapBothEager = /* @__PURE__ */ dual(2, (self, options) => effectIsExit(self) ? exitMapBoth(self, options) : mapBoth(self, options));
var catchEager = /* @__PURE__ */ dual(2, (self, f) => {
  if (effectIsExit(self)) {
    if (self._tag === "Success") return self;
    const error = findError(self.cause);
    if (isFailure2(error)) return self;
    return f(error.success);
  }
  return catch_(self, f);
});
var exitIsSuccess = (self) => self._tag === "Success";
var exitFilterCause = (self) => self._tag === "Failure" ? succeed2(self.cause) : fail2(self);
var exitVoid = /* @__PURE__ */ exitSucceed(void 0);
var exitMap = /* @__PURE__ */ dual(2, (self, f) => self._tag === "Success" ? exitSucceed(f(self.value)) : self);
var exitMapError = /* @__PURE__ */ dual(2, (self, f) => {
  if (self._tag === "Success") return self;
  const error = findError(self.cause);
  if (isFailure2(error)) return self;
  return exitFail(f(error.success));
});
var exitMapBoth = /* @__PURE__ */ dual(2, (self, options) => {
  if (self._tag === "Success") return exitSucceed(options.onSuccess(self.value));
  const error = findError(self.cause);
  if (isFailure2(error)) return self;
  return exitFail(options.onFailure(error.success));
});
var exitAsVoidAll = (exits) => {
  const failures = [];
  for (const exit3 of exits) {
    if (exit3._tag === "Failure") {
      failures.push(...exit3.cause.reasons);
    }
  }
  return failures.length === 0 ? exitVoid : exitFailCause(causeFromReasons(failures));
};
var service = fromYieldable;
var serviceOption = (service3) => withFiber((fiber3) => succeed3(getOption(fiber3.services, service3)));
var serviceOptional = (service3) => withFiber((fiber3) => fiber3.services.mapUnsafe.has(service3.key) ? succeed3(getUnsafe2(fiber3.services, service3)) : fail3(new NoSuchElementError()));
var updateServices = /* @__PURE__ */ dual(2, (self, f) => withFiber((fiber3) => {
  const prev = fiber3.services;
  const nextServices = f(prev);
  if (prev === nextServices) return self;
  fiber3.setServices(nextServices);
  const newServices = /* @__PURE__ */ new Map();
  for (const [key, value] of fiber3.services.mapUnsafe) {
    if (!prev.mapUnsafe.has(key) || value !== prev.mapUnsafe.get(key)) {
      newServices.set(key, value);
    }
  }
  return onExitPrimitive(self, () => {
    const map3 = new Map(fiber3.services.mapUnsafe);
    for (const [key, value] of newServices) {
      if (value !== map3.get(key)) continue;
      if (prev.mapUnsafe.has(key)) {
        map3.set(key, prev.mapUnsafe.get(key));
      } else {
        map3.delete(key);
      }
    }
    fiber3.setServices(makeUnsafe(map3));
    return void 0;
  });
}));
var updateService = /* @__PURE__ */ dual(3, (self, service3, f) => withFiber((fiber3) => {
  const prev = getUnsafe2(fiber3.services, service3);
  const next = f(prev);
  if (prev === next) return self;
  fiber3.setServices(add(fiber3.services, service3, next));
  return onExit(self, () => sync(() => fiber3.setServices(add(fiber3.services, service3, prev))));
}));
var services = () => getServiceMap;
var getServiceMap = /* @__PURE__ */ withFiber((fiber3) => succeed3(fiber3.services));
var servicesWith = (f) => withFiber((fiber3) => f(fiber3.services));
var provideServices = /* @__PURE__ */ dual(2, (self, services3) => {
  if (effectIsExit(self)) return self;
  return updateServices(self, merge(services3));
});
var provideService = function() {
  if (arguments.length === 1) {
    return dual(2, (self, impl) => provideServiceImpl(self, arguments[0], impl));
  }
  return dual(3, (self, service3, impl) => provideServiceImpl(self, service3, impl)).apply(this, arguments);
};
var provideServiceImpl = (self, service3, implementation) => withFiber((fiber3) => {
  const prev = getOption(fiber3.services, service3);
  if (prev._tag === "Some" && prev.value === implementation) return self;
  fiber3.setServices(add(fiber3.services, service3, implementation));
  return onExit(self, () => sync(() => fiber3.setServices(addOrOmit(fiber3.services, service3, prev))));
});
var provideServiceEffect = /* @__PURE__ */ dual(3, (self, service3, acquire) => flatMap(acquire, (implementation) => provideService(self, service3, implementation)));
var withConcurrency = /* @__PURE__ */ provideService(CurrentConcurrency);
var zip = /* @__PURE__ */ dual((args2) => isEffect(args2[1]), (self, that, options) => zipWith(self, that, (a, a2) => [a, a2], options));
var zipWith = /* @__PURE__ */ dual((args2) => isEffect(args2[1]), (self, that, f, options) => options?.concurrent ? map(all([self, that], {
  concurrency: 2
}), ([a, a2]) => internalCall(() => f(a, a2))) : flatMap(self, (a) => map(that, (a2) => internalCall(() => f(a, a2)))));
var filterOrFail = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, filter4, orFailWith) => filterOrElse(self, filter4, orFailWith ? (a) => fail3(orFailWith(a)) : () => fail3(new NoSuchElementError())));
var when = /* @__PURE__ */ dual(2, (self, condition) => flatMap(condition, (pass) => pass ? asSome(self) : succeedNone));
var replicate = /* @__PURE__ */ dual(2, (self, n) => Array.from({
  length: n
}, () => self));
var replicateEffect = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, n, options) => all(replicate(self, n), options));
var forever = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, options) => whileLoop({
  while: constTrue,
  body: constant(options?.disableYield ? self : flatMap(self, (_) => yieldNow)),
  step: constVoid
}));
var catchCause = /* @__PURE__ */ dual(2, (self, f) => {
  const onFailure = Object.create(OnFailureProto);
  onFailure[args] = self;
  onFailure[contE] = f.length !== 1 ? (cause) => f(cause) : f;
  return onFailure;
});
var OnFailureProto = /* @__PURE__ */ makePrimitiveProto({
  op: "OnFailure",
  [evaluate](fiber3) {
    fiber3._stack.push(this);
    return this[args];
  }
});
var catchCauseIf = /* @__PURE__ */ dual(3, (self, filter4, f) => catchCause(self, (cause) => {
  const eb = apply(filter4, cause);
  return !isFailure2(eb) ? internalCall(() => f(eb.success, cause)) : failCause(eb.failure);
}));
var catch_ = /* @__PURE__ */ dual(2, (self, f) => catchCauseIf(self, findError, (e) => f(e)));
var catchNoSuchElement = (self) => matchEffect(self, {
  onFailure: (error) => isNoSuchElementError(error) ? succeedNone : fail3(error),
  onSuccess: succeedSome
});
var catchDefect = /* @__PURE__ */ dual(2, (self, f) => catchCauseIf(self, findDefect, f));
var tapCause = /* @__PURE__ */ dual(2, (self, f) => catchCause(self, (cause) => andThen(internalCall(() => f(cause)), failCause(cause))));
var tapCauseIf = /* @__PURE__ */ dual(3, (self, filter4, f) => catchCauseIf(self, (cause) => {
  const result3 = apply(filter4, cause);
  return isFailure2(result3) ? fail2(cause) : result3;
}, (failure, cause) => andThen(internalCall(() => f(failure, cause)), failCause(cause))));
var tapError = /* @__PURE__ */ dual(2, (self, f) => tapCauseIf(self, findError, (e) => f(e)));
var tapErrorTag = /* @__PURE__ */ dual(3, (self, k, f) => {
  const predicate = Array.isArray(k) ? (e) => hasProperty(e, "_tag") && k.includes(e._tag) : isTagged(k);
  return tapError(self, (error) => predicate(error) ? f(error) : void_);
});
var tapDefect = /* @__PURE__ */ dual(2, (self, f) => tapCauseIf(self, findDefect, (_) => f(_)));
var catchIf = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, filter4, f, orElse) => catchCause(self, (cause) => {
  const error = findError(cause);
  if (isFailure2(error)) return failCause(error.failure);
  const result3 = apply(filter4, error.success);
  if (isFailure2(result3)) {
    return orElse ? internalCall(() => orElse(result3.failure)) : failCause(cause);
  }
  return internalCall(() => f(result3.success));
}));
var catchTag = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, k, f, orElse) => {
  const pred = Array.isArray(k) ? (e) => hasProperty(e, "_tag") && k.includes(e._tag) : isTagged(k);
  return catchIf(self, pred, f, orElse);
});
var catchTags = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, cases, orElse) => {
  let keys;
  return catchIf(self, (e) => {
    keys ??= Object.keys(cases);
    return hasProperty(e, "_tag") && isString(e["_tag"]) && keys.includes(e["_tag"]) ? succeed2(e) : fail2(e);
  }, (e) => internalCall(() => cases[e["_tag"]](e)), orElse);
});
var catchReason = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, errorTag, reasonTag, f, orElse) => catchIf(self, (e) => isTagged(e, errorTag) && hasProperty(e, "reason"), (e) => {
  const reason = e.reason;
  if (isTagged(reason, reasonTag)) return f(reason);
  return orElse ? internalCall(() => orElse(reason)) : fail3(e);
}));
var catchReasons = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, errorTag, cases, orElse) => {
  let keys;
  return catchIf(self, (e) => isTagged(e, errorTag) && hasProperty(e, "reason") && hasProperty(e.reason, "_tag") && isString(e.reason._tag), (e) => {
    const reason = e.reason;
    keys ??= Object.keys(cases);
    if (keys.includes(reason._tag)) {
      return internalCall(() => cases[reason._tag](reason));
    }
    return orElse ? internalCall(() => orElse(reason)) : fail3(e);
  });
});
var unwrapReason = /* @__PURE__ */ dual(2, (self, errorTag) => catchIf(self, (e) => {
  if (isTagged(e, errorTag) && hasProperty(e, "reason")) {
    return succeed2(e.reason);
  }
  return fail2(e);
}, fail3));
var mapError2 = /* @__PURE__ */ dual(2, (self, f) => catch_(self, (error) => failSync(() => f(error))));
var mapBoth = /* @__PURE__ */ dual(2, (self, options) => matchEffect(self, {
  onFailure: (e) => failSync(() => options.onFailure(e)),
  onSuccess: (a) => sync(() => options.onSuccess(a))
}));
var orDie = (self) => catch_(self, die);
var orElseSucceed = /* @__PURE__ */ dual(2, (self, f) => catch_(self, (_) => sync(f)));
var eventually = (self) => catch_(self, (_) => flatMap(yieldNow, () => eventually(self)));
var ignore = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, options) => {
  if (!options?.log) {
    return matchEffect(self, {
      onFailure: (_) => void_,
      onSuccess: (_) => void_
    });
  }
  const logEffect = logWithLevel(options.log === true ? void 0 : options.log);
  return matchCauseEffect(self, {
    onFailure(cause) {
      const failure = findFail(cause);
      return isFailure2(failure) ? failCause(failure.failure) : logEffect(cause);
    },
    onSuccess: (_) => void_
  });
});
var ignoreCause = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, options) => {
  if (!options?.log) {
    return matchCauseEffect(self, {
      onFailure: (_) => void_,
      onSuccess: (_) => void_
    });
  }
  const logEffect = logWithLevel(options.log === true ? void 0 : options.log);
  return matchCauseEffect(self, {
    onFailure: logEffect,
    onSuccess: (_) => void_
  });
});
var option = (self) => match2(self, {
  onFailure: none2,
  onSuccess: some2
});
var result = (self) => matchEager(self, {
  onFailure: fail2,
  onSuccess: succeed2
});
var matchCauseEffect = /* @__PURE__ */ dual(2, (self, options) => {
  const primitive = Object.create(OnSuccessAndFailureProto);
  primitive[args] = self;
  primitive[contA] = options.onSuccess.length !== 1 ? (a) => options.onSuccess(a) : options.onSuccess;
  primitive[contE] = options.onFailure.length !== 1 ? (cause) => options.onFailure(cause) : options.onFailure;
  return primitive;
});
var OnSuccessAndFailureProto = /* @__PURE__ */ makePrimitiveProto({
  op: "OnSuccessAndFailure",
  [evaluate](fiber3) {
    fiber3._stack.push(this);
    return this[args];
  }
});
var matchCause = /* @__PURE__ */ dual(2, (self, options) => matchCauseEffect(self, {
  onFailure: (cause) => sync(() => options.onFailure(cause)),
  onSuccess: (value) => sync(() => options.onSuccess(value))
}));
var matchEffect = /* @__PURE__ */ dual(2, (self, options) => matchCauseEffect(self, {
  onFailure: (cause) => {
    const fail5 = cause.reasons.find(isFailReason);
    return fail5 ? internalCall(() => options.onFailure(fail5.error)) : failCause(cause);
  },
  onSuccess: options.onSuccess
}));
var match2 = /* @__PURE__ */ dual(2, (self, options) => matchEffect(self, {
  onFailure: (error) => sync(() => options.onFailure(error)),
  onSuccess: (value) => sync(() => options.onSuccess(value))
}));
var matchEager = /* @__PURE__ */ dual(2, (self, options) => {
  if (effectIsExit(self)) {
    if (self._tag === "Success") return exitSucceed(options.onSuccess(self.value));
    const error = findError(self.cause);
    if (isFailure2(error)) return self;
    return exitSucceed(options.onFailure(error.success));
  }
  return match2(self, options);
});
var matchCauseEager = /* @__PURE__ */ dual(2, (self, options) => {
  if (effectIsExit(self)) {
    if (self._tag === "Success") return exitSucceed(options.onSuccess(self.value));
    return exitSucceed(options.onFailure(self.cause));
  }
  return matchCause(self, options);
});
var exit = (self) => effectIsExit(self) ? exitSucceed(self) : exitPrimitive(self);
var exitPrimitive = /* @__PURE__ */ makePrimitive({
  op: "Exit",
  [evaluate](fiber3) {
    fiber3._stack.push(this);
    return this[args];
  },
  [contA](value, _, exit3) {
    return succeed3(exit3 ?? exitSucceed(value));
  },
  [contE](cause, _, exit3) {
    return succeed3(exit3 ?? exitFailCause(cause));
  }
});
var isFailure3 = /* @__PURE__ */ matchEager({
  onFailure: () => true,
  onSuccess: () => false
});
var isSuccess3 = /* @__PURE__ */ matchEager({
  onFailure: () => false,
  onSuccess: () => true
});
var delay = /* @__PURE__ */ dual(2, (self, duration) => andThen(sleep(duration), self));
var timeoutOrElse = /* @__PURE__ */ dual(2, (self, options) => raceFirst(self, flatMap(sleep(options.duration), options.onTimeout)));
var timeout = /* @__PURE__ */ dual(2, (self, duration) => timeoutOrElse(self, {
  duration,
  onTimeout: () => fail3(new TimeoutError())
}));
var timeoutOption = /* @__PURE__ */ dual(2, (self, duration) => raceFirst(asSome(self), as(sleep(duration), none2())));
var timed = (self) => clockWith((clock) => {
  const start = clock.currentTimeNanosUnsafe();
  return map(self, (a) => [nanos(clock.currentTimeNanosUnsafe() - start), a]);
});
var ScopeTypeId = "~effect/Scope";
var ScopeCloseableTypeId = "~effect/Scope/Closeable";
var scopeTag = /* @__PURE__ */ Service("effect/Scope");
var scopeClose = (self, exit_) => suspend(() => scopeCloseUnsafe(self, exit_) ?? void_);
var scopeCloseUnsafe = (self, exit_) => {
  if (self.state._tag === "Closed") return;
  const closed = {
    _tag: "Closed",
    exit: exit_
  };
  if (self.state._tag === "Empty") {
    self.state = closed;
    return;
  }
  const {
    finalizers
  } = self.state;
  self.state = closed;
  if (finalizers.size === 0) {
    return;
  } else if (finalizers.size === 1) {
    return finalizers.values().next().value(exit_);
  }
  return scopeCloseFinalizers(self, finalizers, exit_);
};
var scopeCloseFinalizers = /* @__PURE__ */ fnUntraced(function* (self, finalizers, exit_) {
  let exits = [];
  const fibers = [];
  const arr = Array.from(finalizers.values());
  const parent = getCurrentFiber();
  for (let i = arr.length - 1; i >= 0; i--) {
    const finalizer = arr[i];
    if (self.strategy === "sequential") {
      exits.push(yield* exit(finalizer(exit_)));
    } else {
      fibers.push(forkUnsafe(parent, finalizer(exit_), true, true, "inherit"));
    }
  }
  if (fibers.length > 0) {
    exits = yield* fiberAwaitAll(fibers);
  }
  return yield* exitAsVoidAll(exits);
});
var scopeForkUnsafe = (scope3, finalizerStrategy) => {
  const newScope = scopeMakeUnsafe(finalizerStrategy);
  if (scope3.state._tag === "Closed") {
    newScope.state = scope3.state;
    return newScope;
  }
  const key = {};
  scopeAddFinalizerUnsafe(scope3, key, (exit3) => scopeClose(newScope, exit3));
  scopeAddFinalizerUnsafe(newScope, key, (_) => sync(() => scopeRemoveFinalizerUnsafe(scope3, key)));
  return newScope;
};
var scopeAddFinalizerExit = (scope3, finalizer) => {
  return suspend(() => {
    if (scope3.state._tag === "Closed") {
      return finalizer(scope3.state.exit);
    }
    scopeAddFinalizerUnsafe(scope3, {}, finalizer);
    return void_;
  });
};
var scopeAddFinalizerUnsafe = (scope3, key, finalizer) => {
  if (scope3.state._tag === "Empty") {
    scope3.state = {
      _tag: "Open",
      finalizers: /* @__PURE__ */ new Map([[key, finalizer]])
    };
  } else if (scope3.state._tag === "Open") {
    scope3.state.finalizers.set(key, finalizer);
  }
};
var scopeRemoveFinalizerUnsafe = (scope3, key) => {
  if (scope3.state._tag === "Open") {
    scope3.state.finalizers.delete(key);
  }
};
var scopeMakeUnsafe = (finalizerStrategy = "sequential") => ({
  [ScopeCloseableTypeId]: ScopeCloseableTypeId,
  [ScopeTypeId]: ScopeTypeId,
  strategy: finalizerStrategy,
  state: constScopeEmpty
});
var constScopeEmpty = {
  _tag: "Empty"
};
var scope = /* @__PURE__ */ scopeTag.asEffect();
var provideScope = /* @__PURE__ */ provideService(scopeTag);
var scoped = (self) => withFiber((fiber3) => {
  const prev = getOption(fiber3.services, scopeTag);
  const scope3 = scopeMakeUnsafe();
  fiber3.setServices(add(fiber3.services, scopeTag, scope3));
  return onExitPrimitive(self, (exit3) => {
    fiber3.setServices(addOrOmit(fiber3.services, scopeTag, prev));
    return scopeCloseUnsafe(scope3, exit3);
  });
});
var scopedWith = (f) => suspend(() => {
  const scope3 = scopeMakeUnsafe();
  return onExit(f(scope3), (exit3) => suspend(() => scopeCloseUnsafe(scope3, exit3) ?? void_));
});
var acquireRelease = (acquire, release) => uninterruptible(flatMap(scope, (scope3) => tap(acquire, (a) => scopeAddFinalizerExit(scope3, (exit3) => internalCall(() => release(a, exit3))))));
var addFinalizer = (finalizer) => flatMap(scope, (scope3) => servicesWith((services3) => scopeAddFinalizerExit(scope3, (exit3) => provideServices(finalizer(exit3), services3))));
var onExitPrimitive = /* @__PURE__ */ makePrimitive({
  op: "OnExit",
  single: false,
  [evaluate](fiber3) {
    fiber3._stack.push(this);
    return this[args][0];
  },
  [contAll](fiber3) {
    if (fiber3.interruptible && this[args][2] !== true) {
      fiber3._stack.push(setInterruptibleTrue);
      fiber3.interruptible = false;
    }
  },
  [contA](value, _, exit3) {
    exit3 ??= exitSucceed(value);
    const eff = this[args][1](exit3);
    return eff ? flatMap(eff, (_2) => exit3) : exit3;
  },
  [contE](cause, _, exit3) {
    exit3 ??= exitFailCause(cause);
    const eff = this[args][1](exit3);
    return eff ? flatMap(eff, (_2) => exit3) : exit3;
  }
});
var onExit = /* @__PURE__ */ dual(2, onExitPrimitive);
var ensuring = /* @__PURE__ */ dual(2, (self, finalizer) => onExit(self, (_) => finalizer));
var onExitIf = /* @__PURE__ */ dual(3, (self, filter4, f) => onExit(self, (exit3) => {
  const pass = apply(filter4, exit3);
  return isFailure2(pass) ? void_ : f(pass.success, exit3);
}));
var onError = /* @__PURE__ */ dual(2, (self, f) => onExitIf(self, exitFilterCause, f));
var onErrorIf = /* @__PURE__ */ dual(3, (self, filter4, f) => onExitIf(self, (exit3) => {
  if (exit3._tag !== "Failure") return fail2(exit3);
  return apply(filter4, exit3.cause);
}, (eb, exit3) => f(eb, exit3.cause)));
var onInterrupt = /* @__PURE__ */ dual(2, (self, finalizer) => onErrorIf(causeFilterInterruptors, finalizer)(self));
var acquireUseRelease = (acquire, use, release) => uninterruptibleMask((restore) => flatMap(acquire, (a) => onExitPrimitive(restore(use(a)), (exit3) => release(a, exit3), true)));
var cachedInvalidateWithTTL = /* @__PURE__ */ dual(2, (self, ttl) => sync(() => {
  const ttlMillis = toMillis(fromInputUnsafe(ttl));
  const isFinite = Number.isFinite(ttlMillis);
  const latch = makeLatchUnsafe(false);
  let expiresAt = 0;
  let running = false;
  let exit3;
  const wait = flatMap(latch.await, () => exit3);
  return [withFiber((fiber3) => {
    const now = isFinite ? fiber3.getRef(ClockRef).currentTimeMillisUnsafe() : 0;
    if (running || now < expiresAt) return exit3 ?? wait;
    running = true;
    latch.closeUnsafe();
    exit3 = void 0;
    return onExit(self, (exit_) => sync(() => {
      running = false;
      expiresAt = now + ttlMillis;
      exit3 = exit_;
      latch.openUnsafe();
    }));
  }), sync(() => {
    expiresAt = 0;
    latch.closeUnsafe();
    exit3 = void 0;
  })];
}));
var cachedWithTTL = /* @__PURE__ */ dual(2, (self, timeToLive) => map(cachedInvalidateWithTTL(self, timeToLive), (tuple) => tuple[0]));
var cached = (self) => cachedWithTTL(self, infinity);
var interrupt = /* @__PURE__ */ withFiber((fiber3) => failCause(causeInterrupt(fiber3.id)));
var uninterruptible = (self) => withFiber((fiber3) => {
  if (!fiber3.interruptible) return self;
  fiber3.interruptible = false;
  fiber3._stack.push(setInterruptibleTrue);
  return self;
});
var setInterruptible = /* @__PURE__ */ makePrimitive({
  op: "SetInterruptible",
  [contAll](fiber3) {
    fiber3.interruptible = this[args];
    if (fiber3._interruptedCause && fiber3.interruptible) {
      return () => failCause(fiber3._interruptedCause);
    }
  }
});
var setInterruptibleTrue = /* @__PURE__ */ setInterruptible(true);
var setInterruptibleFalse = /* @__PURE__ */ setInterruptible(false);
var interruptible = (self) => withFiber((fiber3) => {
  if (fiber3.interruptible) return self;
  fiber3.interruptible = true;
  fiber3._stack.push(setInterruptibleFalse);
  if (fiber3._interruptedCause) return failCause(fiber3._interruptedCause);
  return self;
});
var uninterruptibleMask = (f) => withFiber((fiber3) => {
  if (!fiber3.interruptible) return f(identity);
  fiber3.interruptible = false;
  fiber3._stack.push(setInterruptibleTrue);
  return f(interruptible);
});
var interruptibleMask = (f) => withFiber((fiber3) => {
  if (fiber3.interruptible) return f(identity);
  fiber3.interruptible = true;
  fiber3._stack.push(setInterruptibleFalse);
  return f(uninterruptible);
});
var all = (arg, options) => {
  if (isIterable(arg)) {
    return options?.mode === "result" ? forEach(arg, result, options) : forEach(arg, identity, options);
  } else if (options?.discard) {
    return options.mode === "result" ? forEach(Object.values(arg), result, options) : forEach(Object.values(arg), identity, options);
  }
  return suspend(() => {
    const out = {};
    return as(forEach(Object.entries(arg), ([key, effect2]) => map(options?.mode === "result" ? result(effect2) : effect2, (value) => {
      out[key] = value;
    }), {
      discard: true,
      concurrency: options?.concurrency
    }), out);
  });
};
var partition = /* @__PURE__ */ dual((args2) => isIterable(args2[0]) && !isEffect(args2[0]), (elements, f, options) => map(forEach(elements, (a, i) => result(f(a, i)), options), (results) => partitionMap(results, identity)));
var whileLoop = /* @__PURE__ */ makePrimitive({
  op: "While",
  [contA](value, fiber3) {
    this[args].step(value);
    if (this[args].while()) {
      fiber3._stack.push(this);
      return this[args].body();
    }
    return exitVoid;
  },
  [evaluate](fiber3) {
    if (this[args].while()) {
      fiber3._stack.push(this);
      return this[args].body();
    }
    return exitVoid;
  }
});
var forEach = /* @__PURE__ */ dual((args2) => typeof args2[1] === "function", (iterable, f, options) => withFiber((parent) => {
  const concurrencyOption = options?.concurrency === "inherit" ? parent.getRef(CurrentConcurrency) : options?.concurrency ?? 1;
  const concurrency = concurrencyOption === "unbounded" ? Number.POSITIVE_INFINITY : Math.max(1, concurrencyOption);
  if (concurrency === 1) {
    return forEachSequential(iterable, f, options);
  }
  const items = fromIterable(iterable);
  let length = items.length;
  if (length === 0) {
    return options?.discard ? void_ : succeed3([]);
  }
  const out = options?.discard ? void 0 : new Array(length);
  let index = 0;
  const annotations = fiberStackAnnotations(parent);
  return callback((resume) => {
    const fibers = /* @__PURE__ */ new Set();
    const failures = [];
    let failed = false;
    let inProgress = 0;
    let doneCount = 0;
    let pumping = false;
    let interrupted = false;
    function pump() {
      pumping = true;
      while (inProgress < concurrency && index < length) {
        const currentIndex = index;
        const item = items[currentIndex];
        index++;
        inProgress++;
        try {
          const child = forkUnsafe(parent, f(item, currentIndex), true, true, "inherit");
          fibers.add(child);
          child.addObserver((exit3) => {
            if (interrupted) {
              return;
            }
            fibers.delete(child);
            if (exit3._tag === "Failure") {
              if (!failed) {
                failed = true;
                length = index;
                failures.push(...exit3.cause.reasons);
                fibers.forEach((fiber3) => fiber3.interruptUnsafe(parent.id, annotations));
              } else {
                for (const f2 of exit3.cause.reasons) {
                  if (f2._tag === "Interrupt") continue;
                  failures.push(f2);
                }
              }
            } else if (out !== void 0) {
              out[currentIndex] = exit3.value;
            }
            doneCount++;
            inProgress--;
            if (doneCount === length) {
              resume(failures.length > 0 ? exitFailCause(causeFromReasons(failures)) : succeed3(out));
            } else if (!pumping && !failed && inProgress < concurrency) {
              pump();
            }
          });
        } catch (err) {
          failed = true;
          length = index;
          failures.push(new Die(err));
          fibers.forEach((fiber3) => fiber3.interruptUnsafe(parent.id, annotations));
        }
      }
      pumping = false;
    }
    pump();
    return suspend(() => {
      interrupted = true;
      index = length;
      return fiberInterruptAll(fibers);
    });
  });
}));
var forEachSequential = (iterable, f, options) => suspend(() => {
  const out = options?.discard ? void 0 : [];
  const iterator = iterable[Symbol.iterator]();
  let state = iterator.next();
  let index = 0;
  return as(whileLoop({
    while: () => !state.done,
    body: () => f(state.value, index++),
    step: (b) => {
      if (out) out.push(b);
      state = iterator.next();
    }
  }), out);
});
var filterOrElse = /* @__PURE__ */ dual(3, (self, filter4, orElse) => flatMap(self, (a) => {
  const result3 = apply(filter4, a);
  return isFailure2(result3) ? orElse(result3.failure) : succeed3(result3.success);
}));
var filter2 = /* @__PURE__ */ dual((args2) => isIterable(args2[0]) && !isEffect(args2[0]), (elements, filter4, options) => suspend(() => {
  const out = [];
  return as(forEach(elements, (a, i) => {
    const result3 = filter4(a, i);
    if (typeof result3 === "boolean") {
      if (result3) out.push(a);
      return void_;
    }
    if (!isEffect(result3)) {
      if (!isFailure2(result3)) {
        out.push(result3.success);
      }
      return void_;
    }
    return map(result3, (r) => {
      if (typeof r === "boolean") {
        if (r) out.push(a);
      } else if (!isFailure2(r)) {
        out.push(r.success);
      }
    });
  }, {
    discard: true,
    concurrency: options?.concurrency
  }), out);
}));
var forkChild = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, options) => withFiber((fiber3) => {
  interruptChildrenPatch();
  return succeed3(forkUnsafe(fiber3, self, options?.startImmediately, false, options?.uninterruptible ?? false));
}));
var forkUnsafe = (parent, effect2, immediate = false, daemon = false, uninterruptible3 = false) => {
  const interruptible3 = uninterruptible3 === "inherit" ? parent.interruptible : !uninterruptible3;
  const child = new FiberImpl(parent.services, interruptible3);
  if (immediate) {
    child.evaluate(effect2);
  } else {
    parent.currentScheduler.scheduleTask(() => child.evaluate(effect2), 0);
  }
  if (!daemon && !child._exit) {
    parent.children().add(child);
    child.addObserver(() => parent._children.delete(child));
  }
  return child;
};
var forkDetach = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, options) => withFiber((fiber3) => succeed3(forkUnsafe(fiber3, self, options?.startImmediately, true, options?.uninterruptible))));
var awaitAllChildren = (self) => withFiber((fiber3) => {
  const initialChildren = fiber3._children && fromIterable(fiber3._children);
  return onExit(self, (_) => {
    let children = fiber3._children;
    if (children === void 0 || children.size === 0) {
      return void_;
    } else if (initialChildren) {
      children = filter(children, (child) => !initialChildren.includes(child));
    }
    return asVoid(fiberAwaitAll(children));
  });
});
var forkIn = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, scope3, options) => withFiber((parent) => {
  const fiber3 = forkUnsafe(parent, self, options?.startImmediately, true, options?.uninterruptible);
  if (!fiber3._exit) {
    if (scope3.state._tag !== "Closed") {
      const key = {};
      const finalizer = () => withFiberId((interruptor) => interruptor === fiber3.id ? void_ : fiberInterrupt(fiber3));
      scopeAddFinalizerUnsafe(scope3, key, finalizer);
      fiber3.addObserver(() => scopeRemoveFinalizerUnsafe(scope3, key));
    } else {
      fiber3.interruptUnsafe(parent.id, fiberStackAnnotations(parent));
    }
  }
  return succeed3(fiber3);
}));
var forkScoped = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, options) => flatMap(scope, (scope3) => forkIn(self, scope3, options)));
var runForkWith = (services3) => (effect2, options) => {
  const scheduler = options?.scheduler || !services3.mapUnsafe.has(Scheduler.key) && new MixedScheduler();
  const fiber3 = new FiberImpl(scheduler ? add(services3, Scheduler, scheduler) : services3, options?.uninterruptible !== true);
  fiber3.evaluate(effect2);
  if (fiber3._exit) return fiber3;
  if (options?.signal) {
    if (options.signal.aborted) {
      fiber3.interruptUnsafe();
    } else {
      const abort = () => fiber3.interruptUnsafe();
      options.signal.addEventListener("abort", abort, {
        once: true
      });
      fiber3.addObserver(() => options.signal.removeEventListener("abort", abort));
    }
  }
  return fiber3;
};
var runFork = /* @__PURE__ */ runForkWith(/* @__PURE__ */ empty());
var runCallbackWith = (services3) => {
  const runFork3 = runForkWith(services3);
  return (effect2, options) => {
    const fiber3 = runFork3(effect2, options);
    if (options?.onExit) {
      fiber3.addObserver(options.onExit);
    }
    return (interruptor) => {
      return fiber3.interruptUnsafe(interruptor);
    };
  };
};
var runCallback = /* @__PURE__ */ runCallbackWith(/* @__PURE__ */ empty());
var runPromiseExitWith = (services3) => {
  const runFork3 = runForkWith(services3);
  return (effect2, options) => {
    const fiber3 = runFork3(effect2, options);
    return new Promise((resolve) => {
      fiber3.addObserver((exit3) => resolve(exit3));
    });
  };
};
var runPromiseExit = /* @__PURE__ */ runPromiseExitWith(/* @__PURE__ */ empty());
var runPromiseWith = (services3) => {
  const runPromiseExit3 = runPromiseExitWith(services3);
  return (effect2, options) => runPromiseExit3(effect2, options).then((exit3) => {
    if (exit3._tag === "Failure") {
      throw causeSquash(exit3.cause);
    }
    return exit3.value;
  });
};
var runPromise = /* @__PURE__ */ runPromiseWith(/* @__PURE__ */ empty());
var runSyncExitWith = (services3) => {
  const runFork3 = runForkWith(services3);
  return (effect2) => {
    if (effectIsExit(effect2)) return effect2;
    const scheduler = new MixedScheduler("sync");
    const fiber3 = runFork3(effect2, {
      scheduler
    });
    scheduler.flush();
    return fiber3._exit ?? exitDie(fiber3);
  };
};
var runSyncExit = /* @__PURE__ */ runSyncExitWith(/* @__PURE__ */ empty());
var runSyncWith = (services3) => {
  const runSyncExit3 = runSyncExitWith(services3);
  return (effect2) => {
    const exit3 = runSyncExit3(effect2);
    if (exit3._tag === "Failure") throw causeSquash(exit3.cause);
    return exit3.value;
  };
};
var runSync = /* @__PURE__ */ runSyncWith(/* @__PURE__ */ empty());
var succeedTrue = /* @__PURE__ */ succeed3(true);
var succeedFalse = /* @__PURE__ */ succeed3(false);
var Latch = class {
  waiters = [];
  scheduled = false;
  isOpen;
  constructor(isOpen) {
    this.isOpen = isOpen;
  }
  scheduleUnsafe(fiber3) {
    if (this.scheduled || this.waiters.length === 0) {
      return succeedTrue;
    }
    this.scheduled = true;
    fiber3.currentScheduler.scheduleTask(this.flushWaiters, 0);
    return succeedTrue;
  }
  flushWaiters = () => {
    this.scheduled = false;
    const waiters = this.waiters;
    this.waiters = [];
    for (let i = 0; i < waiters.length; i++) {
      waiters[i](exitVoid);
    }
  };
  open = /* @__PURE__ */ withFiber((fiber3) => {
    if (this.isOpen) return succeedFalse;
    this.isOpen = true;
    return this.scheduleUnsafe(fiber3);
  });
  release = /* @__PURE__ */ withFiber((fiber3) => this.open ? succeedFalse : this.scheduleUnsafe(fiber3));
  openUnsafe() {
    if (this.isOpen) return false;
    this.isOpen = true;
    this.flushWaiters();
    return true;
  }
  await = /* @__PURE__ */ callback((resume) => {
    if (this.isOpen) {
      return resume(void_);
    }
    this.waiters.push(resume);
    return sync(() => {
      const index = this.waiters.indexOf(resume);
      if (index !== -1) {
        this.waiters.splice(index, 1);
      }
    });
  });
  closeUnsafe() {
    if (!this.isOpen) return false;
    this.isOpen = false;
    return true;
  }
  close = /* @__PURE__ */ sync(() => this.closeUnsafe());
  whenOpen = (self) => andThen(this.await, self);
};
var makeLatchUnsafe = (open) => new Latch(open ?? false);
var tracer = /* @__PURE__ */ withFiber((fiber3) => succeed3(fiber3.getRef(Tracer)));
var withTracer = /* @__PURE__ */ dual(2, (effect2, tracer3) => provideService(effect2, Tracer, tracer3));
var withTracerEnabled = /* @__PURE__ */ provideService(TracerEnabled);
var withTracerTiming = /* @__PURE__ */ provideService(TracerTimingEnabled);
var bigint02 = /* @__PURE__ */ BigInt(0);
var NoopSpanProto = {
  _tag: "Span",
  spanId: "noop",
  traceId: "noop",
  sampled: false,
  status: {
    _tag: "Ended",
    startTime: bigint02,
    endTime: bigint02,
    exit: exitVoid
  },
  attributes: /* @__PURE__ */ new Map(),
  links: [],
  kind: "internal",
  attribute() {
  },
  event() {
  },
  end() {
  },
  addLinks() {
  }
};
var noopSpan = (options) => Object.assign(Object.create(NoopSpanProto), options);
var filterDisablePropagation = (span2) => {
  if (span2) {
    return get(span2.annotations, DisablePropagation) ? span2._tag === "Span" ? filterDisablePropagation(span2.parent) : void 0 : span2;
  }
};
var makeSpanUnsafe = (fiber3, name, options) => {
  const disablePropagation = !fiber3.getRef(TracerEnabled) || options?.annotations && get(options.annotations, DisablePropagation);
  const parent = options?.parent ?? (options?.root ? void 0 : filterDisablePropagation(fiber3.currentSpan));
  let span2;
  if (disablePropagation) {
    span2 = noopSpan({
      name,
      parent,
      annotations: add(options?.annotations ?? empty(), DisablePropagation, true)
    });
  } else {
    const tracer3 = fiber3.getRef(Tracer);
    const clock = fiber3.getRef(ClockRef);
    const timingEnabled = fiber3.getRef(TracerTimingEnabled);
    const annotationsFromEnv = fiber3.getRef(TracerSpanAnnotations);
    const linksFromEnv = fiber3.getRef(TracerSpanLinks);
    const level = options?.level ?? fiber3.getRef(CurrentTraceLevel);
    const links = options?.links !== void 0 ? [...linksFromEnv, ...options.links] : linksFromEnv.slice();
    span2 = tracer3.span({
      name,
      parent,
      annotations: options?.annotations ?? empty(),
      links,
      startTime: timingEnabled ? clock.currentTimeNanosUnsafe() : 0n,
      kind: options?.kind ?? "internal",
      root: options?.root ?? options?.parent === void 0,
      sampled: options?.sampled ?? (parent?.sampled === false ? false : !isLogLevelGreaterThan(fiber3.getRef(MinimumTraceLevel), level))
    });
    for (const [key, value] of Object.entries(annotationsFromEnv)) {
      span2.attribute(key, value);
    }
    if (options?.attributes !== void 0) {
      for (const [key, value] of Object.entries(options.attributes)) {
        span2.attribute(key, value);
      }
    }
  }
  return span2;
};
var makeSpan = (name, options) => withFiber((fiber3) => succeed3(makeSpanUnsafe(fiber3, name, options)));
var makeSpanScoped = (name, options) => uninterruptible(withFiber((fiber3) => {
  const scope3 = getUnsafe2(fiber3.services, scopeTag);
  const span2 = makeSpanUnsafe(fiber3, name, options ?? {});
  const clock = fiber3.getRef(ClockRef);
  const timingEnabled = fiber3.getRef(TracerTimingEnabled);
  return as(scopeAddFinalizerExit(scope3, (exit3) => endSpan(span2, exit3, clock, timingEnabled)), span2);
}));
var withSpanScoped = function() {
  const dataFirst = typeof arguments[0] !== "string";
  const name = dataFirst ? arguments[1] : arguments[0];
  const options = addSpanStackTrace(dataFirst ? arguments[2] : arguments[1]);
  if (dataFirst) {
    const self = arguments[0];
    return flatMap(makeSpanScoped(name, options), (span2) => withParentSpan(self, span2, options));
  }
  return (self) => flatMap(makeSpanScoped(name, options), (span2) => withParentSpan(self, span2, options));
};
var provideSpanStackFrame = (name, stack) => {
  stack = typeof stack === "function" ? stack : constUndefined;
  return updateService(CurrentStackFrame, (parent) => ({
    name,
    stack,
    parent
  }));
};
var spanAnnotations = /* @__PURE__ */ TracerSpanAnnotations.asEffect();
var spanLinks = /* @__PURE__ */ TracerSpanLinks.asEffect();
var linkSpans = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, span2, attributes = {}) => {
  const spans = Array.isArray(span2) ? span2 : [span2];
  const links = spans.map((span3) => ({
    span: span3,
    attributes
  }));
  return updateService(self, TracerSpanLinks, (current) => [...current, ...links]);
});
var endSpan = (span2, exit3, clock, timingEnabled) => sync(() => {
  if (span2.status._tag === "Ended") return;
  span2.end(timingEnabled ? clock.currentTimeNanosUnsafe() : bigint02, exit3);
});
var useSpan = (name, ...args2) => {
  const options = args2.length === 1 ? void 0 : args2[0];
  const evaluate2 = args2[args2.length - 1];
  return withFiber((fiber3) => {
    const span2 = makeSpanUnsafe(fiber3, name, options);
    const clock = fiber3.getRef(ClockRef);
    return onExit(internalCall(() => evaluate2(span2)), (exit3) => sync(() => {
      if (span2.status._tag === "Ended") return;
      span2.end(clock.currentTimeNanosUnsafe(), exit3);
    }));
  });
};
var provideParentSpan = /* @__PURE__ */ provideService(ParentSpan);
var withParentSpan = function() {
  const dataFirst = isEffect(arguments[0]);
  const span2 = dataFirst ? arguments[1] : arguments[0];
  let options = dataFirst ? arguments[2] : arguments[1];
  let provideStackFrame = identity;
  if (span2._tag === "Span") {
    options = addSpanStackTrace(options);
    provideStackFrame = provideSpanStackFrame(span2.name, options?.captureStackTrace);
  }
  if (dataFirst) {
    return provideParentSpan(provideStackFrame(arguments[0]), span2);
  }
  return (self) => provideParentSpan(provideStackFrame(self), span2);
};
var withSpan = function() {
  const dataFirst = typeof arguments[0] !== "string";
  const name = dataFirst ? arguments[1] : arguments[0];
  const traceOptions = addSpanStackTrace(arguments[2]);
  if (dataFirst) {
    const self = arguments[0];
    return useSpan(name, arguments[2], (span2) => withParentSpan(self, span2, traceOptions));
  }
  const fnArg = typeof arguments[1] === "function" ? arguments[1] : void 0;
  const options = fnArg ? void 0 : arguments[1];
  return (self, ...args2) => useSpan(name, fnArg ? fnArg(...args2) : options, (span2) => withParentSpan(self, span2, traceOptions));
};
var annotateSpans = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (effect2, ...args2) => updateService(effect2, TracerSpanAnnotations, (annotations) => {
  const newAnnotations = {
    ...annotations
  };
  if (args2.length === 1) {
    Object.assign(newAnnotations, args2[0]);
  } else {
    newAnnotations[args2[0]] = args2[1];
  }
  return newAnnotations;
}));
var annotateCurrentSpan = (...args2) => withFiber((fiber3) => {
  const span2 = fiber3.currentSpanLocal;
  if (span2) {
    if (args2.length === 1) {
      for (const [key, value] of Object.entries(args2[0])) {
        span2.attribute(key, value);
      }
    } else {
      span2.attribute(args2[0], args2[1]);
    }
  }
  return void_;
});
var currentSpan = /* @__PURE__ */ withFiber((fiber3) => {
  const span2 = fiber3.currentSpanLocal;
  return span2 ? succeed3(span2) : fail3(new NoSuchElementError());
});
var currentParentSpan = /* @__PURE__ */ serviceOptional(ParentSpan);
var ClockRef = /* @__PURE__ */ Reference("effect/Clock", {
  defaultValue: () => new ClockImpl()
});
var MAX_TIMER_MILLIS = 2 ** 31 - 1;
var ClockImpl = class {
  currentTimeMillisUnsafe() {
    return Date.now();
  }
  currentTimeMillis = /* @__PURE__ */ sync(() => this.currentTimeMillisUnsafe());
  currentTimeNanosUnsafe() {
    return processOrPerformanceNow();
  }
  currentTimeNanos = /* @__PURE__ */ sync(() => this.currentTimeNanosUnsafe());
  sleep(duration) {
    const millis2 = toMillis(duration);
    if (millis2 <= 0) return yieldNow;
    return callback((resume) => {
      if (millis2 > MAX_TIMER_MILLIS) return;
      const handle = setTimeout(() => resume(void_), millis2);
      return sync(() => clearTimeout(handle));
    });
  }
};
var performanceNowNanos = /* @__PURE__ */ (function() {
  const bigint1e6 = /* @__PURE__ */ BigInt(1e6);
  if (typeof performance === "undefined" || typeof performance.now === "undefined") {
    return () => BigInt(Date.now()) * bigint1e6;
  } else if (typeof performance.timeOrigin === "number" && performance.timeOrigin === 0) {
    return () => BigInt(Math.round(performance.now() * 1e6));
  }
  const origin = /* @__PURE__ */ BigInt(/* @__PURE__ */ Date.now()) * bigint1e6 - /* @__PURE__ */ BigInt(/* @__PURE__ */ Math.round(/* @__PURE__ */ performance.now() * 1e6));
  return () => origin + BigInt(Math.round(performance.now() * 1e6));
})();
var processOrPerformanceNow = /* @__PURE__ */ (function() {
  const processHrtime = typeof process === "object" && "hrtime" in process && typeof process.hrtime.bigint === "function" ? process.hrtime : void 0;
  if (!processHrtime) {
    return performanceNowNanos;
  }
  const origin = /* @__PURE__ */ performanceNowNanos() - /* @__PURE__ */ processHrtime.bigint();
  return () => origin + processHrtime.bigint();
})();
var clockWith = (f) => withFiber((fiber3) => f(fiber3.getRef(ClockRef)));
var sleep = (duration) => clockWith((clock) => clock.sleep(fromInputUnsafe(duration)));
var currentTimeMillis = /* @__PURE__ */ clockWith((clock) => clock.currentTimeMillis);
var TimeoutErrorTypeId = "~effect/Cause/TimeoutError";
var TimeoutError = class extends (/* @__PURE__ */ TaggedError("TimeoutError")) {
  [TimeoutErrorTypeId] = TimeoutErrorTypeId;
  constructor(message) {
    super({
      message
    });
  }
};
var IllegalArgumentErrorTypeId = "~effect/Cause/IllegalArgumentError";
var IllegalArgumentError = class extends (/* @__PURE__ */ TaggedError("IllegalArgumentError")) {
  [IllegalArgumentErrorTypeId] = IllegalArgumentErrorTypeId;
  constructor(message) {
    super({
      message
    });
  }
};
var ExceededCapacityErrorTypeId = "~effect/Cause/ExceededCapacityError";
var ExceededCapacityError = class extends (/* @__PURE__ */ TaggedError("ExceededCapacityError")) {
  [ExceededCapacityErrorTypeId] = ExceededCapacityErrorTypeId;
  constructor(message) {
    super({
      message
    });
  }
};
var UnknownErrorTypeId = "~effect/Cause/UnknownError";
var UnknownError = class extends (/* @__PURE__ */ TaggedError("UnknownError")) {
  [UnknownErrorTypeId] = UnknownErrorTypeId;
  constructor(cause, message) {
    super({
      message,
      cause
    });
  }
};
var ConsoleRef = /* @__PURE__ */ Reference("effect/Console/CurrentConsole", {
  defaultValue: () => globalThis.console
});
var logLevelToOrder = (level) => {
  switch (level) {
    case "All":
      return Number.MIN_SAFE_INTEGER;
    case "Fatal":
      return 5e4;
    case "Error":
      return 4e4;
    case "Warn":
      return 3e4;
    case "Info":
      return 2e4;
    case "Debug":
      return 1e4;
    case "Trace":
      return 0;
    case "None":
      return Number.MAX_SAFE_INTEGER;
  }
};
var LogLevelOrder = /* @__PURE__ */ mapInput(Number2, logLevelToOrder);
var isLogLevelGreaterThan = /* @__PURE__ */ isGreaterThan(LogLevelOrder);
var CurrentLoggers = /* @__PURE__ */ Reference("effect/Loggers/CurrentLoggers", {
  defaultValue: () => /* @__PURE__ */ new Set([defaultLogger, tracerLogger])
});
var LogToStderr = /* @__PURE__ */ Reference("effect/Logger/LogToStderr", {
  defaultValue: constFalse
});
var LoggerTypeId = "~effect/Logger";
var LoggerProto = {
  [LoggerTypeId]: {
    _Message: identity,
    _Output: identity
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var loggerMake = (log2) => {
  const self = Object.create(LoggerProto);
  self.log = log2;
  return self;
};
var formatLabel = (key) => key.replace(/[\s="]/g, "_");
var formatLogSpan = (self, now) => {
  const label = formatLabel(self[0]);
  return `${label}=${now - self[1]}ms`;
};
var logWithLevel = (level) => (...message) => {
  let cause = void 0;
  for (let i = 0, len = message.length; i < len; i++) {
    const msg = message[i];
    if (isCause(msg)) {
      if (cause) {
        ;
        message.splice(i, 1);
      } else {
        message = message.slice(0, i).concat(message.slice(i + 1));
      }
      cause = cause ? causeFromReasons(cause.reasons.concat(msg.reasons)) : msg;
      i--;
    }
  }
  if (cause === void 0) {
    cause = causeEmpty;
  }
  return withFiber((fiber3) => {
    const logLevel = level ?? fiber3.currentLogLevel;
    if (isLogLevelGreaterThan(fiber3.minimumLogLevel, logLevel)) {
      return void_;
    }
    const clock = fiber3.getRef(ClockRef);
    const loggers = fiber3.getRef(CurrentLoggers);
    if (loggers.size > 0) {
      const date = new Date(clock.currentTimeMillisUnsafe());
      for (const logger of loggers) {
        logger.log({
          cause,
          fiber: fiber3,
          date,
          logLevel,
          message
        });
      }
    }
    return void_;
  });
};
var colors = {
  bold: "1",
  red: "31",
  green: "32",
  yellow: "33",
  blue: "34",
  cyan: "36",
  white: "37",
  gray: "90",
  black: "30",
  bgBrightRed: "101"
};
var logLevelColors = {
  None: [],
  All: [],
  Trace: [colors.gray],
  Debug: [colors.blue],
  Info: [colors.green],
  Warn: [colors.yellow],
  Error: [colors.red],
  Fatal: [colors.bgBrightRed, colors.black]
};
var defaultDateFormat = (date) => `${date.getHours().toString().padStart(2, "0")}:${date.getMinutes().toString().padStart(2, "0")}:${date.getSeconds().toString().padStart(2, "0")}.${date.getMilliseconds().toString().padStart(3, "0")}`;
var hasProcessStdout = typeof process === "object" && process !== null && typeof process.stdout === "object" && process.stdout !== null;
var processStdoutIsTTY = hasProcessStdout && process.stdout.isTTY === true;
var hasProcessStdoutOrDeno = hasProcessStdout || "Deno" in globalThis;
var defaultLogger = /* @__PURE__ */ loggerMake(({
  cause,
  date,
  fiber: fiber3,
  logLevel,
  message
}) => {
  const message_ = Array.isArray(message) ? message.slice() : [message];
  if (cause.reasons.length > 0) {
    message_.unshift(causePretty(cause));
  }
  const now = date.getTime();
  const spans = fiber3.getRef(CurrentLogSpans);
  let spanString = "";
  for (const span2 of spans) {
    spanString += ` ${formatLogSpan(span2, now)}`;
  }
  const annotations = fiber3.getRef(CurrentLogAnnotations);
  if (Object.keys(annotations).length > 0) {
    message_.push(annotations);
  }
  const console = fiber3.getRef(ConsoleRef);
  const log2 = fiber3.getRef(LogToStderr) ? console.error : console.log;
  log2(`[${defaultDateFormat(date)}] ${logLevel.toUpperCase()} (#${fiber3.id})${spanString}:`, ...message_);
});
var tracerLogger = /* @__PURE__ */ loggerMake(({
  cause,
  fiber: fiber3,
  logLevel,
  message
}) => {
  const clock = fiber3.getRef(ClockRef);
  const annotations = fiber3.getRef(CurrentLogAnnotations);
  const span2 = fiber3.currentSpan;
  if (span2 === void 0 || span2._tag === "ExternalSpan") return;
  const attributes = {};
  for (const [key, value] of Object.entries(annotations)) {
    attributes[key] = value;
  }
  attributes["effect.fiberId"] = fiber3.id;
  attributes["effect.logLevel"] = logLevel.toUpperCase();
  if (cause.reasons.length > 0) {
    attributes["effect.cause"] = causePretty(cause);
  }
  span2.event(toStringUnknown(Array.isArray(message) && message.length === 1 ? message[0] : message), clock.currentTimeNanosUnsafe(), attributes);
});
function interruptChildrenPatch() {
  fiberMiddleware.interruptChildren ??= fiberInterruptChildren;
}
var undefined_ = /* @__PURE__ */ succeed3(void 0);

// ../../node_modules/effect/dist/Cause.js
var findError2 = findError;
var isDone2 = isDone;
var done2 = done;

// ../../node_modules/effect/dist/Effect.js
var Effect_exports = {};
__export(Effect_exports, {
  Transaction: () => Transaction,
  YieldableClass: () => YieldableClass,
  acquireRelease: () => acquireRelease2,
  acquireUseRelease: () => acquireUseRelease2,
  addFinalizer: () => addFinalizer2,
  all: () => all2,
  andThen: () => andThen2,
  annotateCurrentSpan: () => annotateCurrentSpan2,
  annotateLogs: () => annotateLogs,
  annotateSpans: () => annotateSpans2,
  as: () => as2,
  asSome: () => asSome2,
  asVoid: () => asVoid2,
  atomic: () => atomic,
  atomicWith: () => atomicWith,
  awaitAllChildren: () => awaitAllChildren2,
  cached: () => cached2,
  cachedInvalidateWithTTL: () => cachedInvalidateWithTTL2,
  cachedWithTTL: () => cachedWithTTL2,
  callback: () => callback2,
  catch: () => catch_3,
  catchCause: () => catchCause3,
  catchCauseIf: () => catchCauseIf2,
  catchDefect: () => catchDefect2,
  catchEager: () => catchEager2,
  catchIf: () => catchIf2,
  catchNoSuchElement: () => catchNoSuchElement2,
  catchReason: () => catchReason2,
  catchReasons: () => catchReasons2,
  catchTag: () => catchTag3,
  catchTags: () => catchTags2,
  clockWith: () => clockWith2,
  currentParentSpan: () => currentParentSpan2,
  currentSpan: () => currentSpan2,
  delay: () => delay2,
  die: () => die2,
  effectify: () => effectify,
  ensuring: () => ensuring2,
  eventually: () => eventually2,
  exit: () => exit2,
  fail: () => fail4,
  failCause: () => failCause2,
  failCauseSync: () => failCauseSync2,
  failSync: () => failSync2,
  fiber: () => fiber2,
  fiberId: () => fiberId2,
  filter: () => filter3,
  filterOrElse: () => filterOrElse2,
  filterOrFail: () => filterOrFail2,
  flatMap: () => flatMap3,
  flatMapEager: () => flatMapEager2,
  flatten: () => flatten2,
  flip: () => flip2,
  fn: () => fn2,
  fnUntraced: () => fnUntraced2,
  fnUntracedEager: () => fnUntracedEager2,
  forEach: () => forEach2,
  forever: () => forever3,
  forkChild: () => forkChild2,
  forkDetach: () => forkDetach2,
  forkIn: () => forkIn2,
  forkScoped: () => forkScoped2,
  fromNullishOr: () => fromNullishOr2,
  fromOption: () => fromOption3,
  fromResult: () => fromResult2,
  fromYieldable: () => fromYieldable2,
  gen: () => gen2,
  ignore: () => ignore2,
  ignoreCause: () => ignoreCause2,
  interrupt: () => interrupt2,
  interruptible: () => interruptible2,
  interruptibleMask: () => interruptibleMask2,
  isEffect: () => isEffect2,
  isFailure: () => isFailure4,
  isSuccess: () => isSuccess5,
  linkSpans: () => linkSpans2,
  log: () => log,
  logDebug: () => logDebug,
  logError: () => logError,
  logFatal: () => logFatal,
  logInfo: () => logInfo,
  logTrace: () => logTrace,
  logWarning: () => logWarning,
  logWithLevel: () => logWithLevel2,
  makeSpan: () => makeSpan2,
  makeSpanScoped: () => makeSpanScoped2,
  map: () => map2,
  mapBoth: () => mapBoth2,
  mapBothEager: () => mapBothEager2,
  mapEager: () => mapEager2,
  mapError: () => mapError3,
  mapErrorEager: () => mapErrorEager2,
  match: () => match3,
  matchCause: () => matchCause2,
  matchCauseEager: () => matchCauseEager2,
  matchCauseEffect: () => matchCauseEffect2,
  matchCauseEffectEager: () => matchCauseEffectEager2,
  matchEager: () => matchEager2,
  matchEffect: () => matchEffect3,
  never: () => never2,
  onError: () => onError2,
  onErrorIf: () => onErrorIf2,
  onExit: () => onExit2,
  onExitIf: () => onExitIf2,
  onExitPrimitive: () => onExitPrimitive2,
  onInterrupt: () => onInterrupt2,
  option: () => option2,
  orDie: () => orDie3,
  orElseSucceed: () => orElseSucceed2,
  partition: () => partition2,
  promise: () => promise2,
  provide: () => provide4,
  provideService: () => provideService2,
  provideServiceEffect: () => provideServiceEffect2,
  provideServices: () => provideServices2,
  race: () => race2,
  raceAll: () => raceAll2,
  raceAllFirst: () => raceAllFirst2,
  raceFirst: () => raceFirst2,
  repeat: () => repeat2,
  repeatOrElse: () => repeatOrElse2,
  replicate: () => replicate2,
  replicateEffect: () => replicateEffect2,
  request: () => request2,
  requestUnsafe: () => requestUnsafe2,
  result: () => result2,
  retry: () => retry2,
  retryOrElse: () => retryOrElse2,
  retryTransaction: () => retryTransaction,
  runCallback: () => runCallback2,
  runCallbackWith: () => runCallbackWith2,
  runFork: () => runFork2,
  runForkWith: () => runForkWith2,
  runPromise: () => runPromise2,
  runPromiseExit: () => runPromiseExit2,
  runPromiseExitWith: () => runPromiseExitWith2,
  runPromiseWith: () => runPromiseWith2,
  runSync: () => runSync2,
  runSyncExit: () => runSyncExit2,
  runSyncExitWith: () => runSyncExitWith2,
  runSyncWith: () => runSyncWith2,
  sandbox: () => sandbox2,
  satisfiesErrorType: () => satisfiesErrorType2,
  satisfiesServicesType: () => satisfiesServicesType2,
  satisfiesSuccessType: () => satisfiesSuccessType2,
  schedule: () => schedule,
  scheduleFrom: () => scheduleFrom2,
  scope: () => scope2,
  scoped: () => scoped2,
  scopedWith: () => scopedWith2,
  service: () => service2,
  serviceOption: () => serviceOption2,
  services: () => services2,
  servicesWith: () => servicesWith2,
  sleep: () => sleep2,
  spanAnnotations: () => spanAnnotations2,
  spanLinks: () => spanLinks2,
  succeed: () => succeed5,
  succeedNone: () => succeedNone2,
  succeedSome: () => succeedSome2,
  suspend: () => suspend2,
  sync: () => sync3,
  tap: () => tap2,
  tapCause: () => tapCause2,
  tapCauseIf: () => tapCauseIf2,
  tapDefect: () => tapDefect2,
  tapError: () => tapError2,
  tapErrorTag: () => tapErrorTag2,
  timed: () => timed2,
  timeout: () => timeout2,
  timeoutOption: () => timeoutOption2,
  timeoutOrElse: () => timeoutOrElse2,
  tracer: () => tracer2,
  track: () => track,
  trackDefects: () => trackDefects,
  trackDuration: () => trackDuration,
  trackErrors: () => trackErrors,
  trackSuccesses: () => trackSuccesses,
  transaction: () => transaction,
  transactionWith: () => transactionWith,
  try: () => try_2,
  tryPromise: () => tryPromise2,
  undefined: () => undefined_2,
  uninterruptible: () => uninterruptible2,
  uninterruptibleMask: () => uninterruptibleMask2,
  unwrapReason: () => unwrapReason2,
  updateService: () => updateService3,
  updateServices: () => updateServices2,
  useSpan: () => useSpan2,
  void: () => void_2,
  when: () => when2,
  whileLoop: () => whileLoop2,
  withConcurrency: () => withConcurrency2,
  withExecutionPlan: () => withExecutionPlan2,
  withFiber: () => withFiber2,
  withLogSpan: () => withLogSpan,
  withLogger: () => withLogger,
  withParentSpan: () => withParentSpan3,
  withSpan: () => withSpan3,
  withSpanScoped: () => withSpanScoped2,
  withTracer: () => withTracer2,
  withTracerEnabled: () => withTracerEnabled2,
  withTracerTiming: () => withTracerTiming2,
  yieldNow: () => yieldNow2,
  yieldNowWith: () => yieldNowWith2,
  zip: () => zip2,
  zipWith: () => zipWith2
});

// ../../node_modules/effect/dist/Exit.js
var isSuccess4 = exitIsSuccess;

// ../../node_modules/effect/dist/Layer.js
var Layer_exports = {};
__export(Layer_exports, {
  CurrentMemoMap: () => CurrentMemoMap,
  build: () => build,
  buildWithMemoMap: () => buildWithMemoMap,
  buildWithScope: () => buildWithScope,
  catch: () => catch_2,
  catchCause: () => catchCause2,
  catchTag: () => catchTag2,
  effect: () => effect,
  effectDiscard: () => effectDiscard,
  effectServices: () => effectServices,
  empty: () => empty3,
  flatMap: () => flatMap2,
  fresh: () => fresh,
  fromBuild: () => fromBuild,
  fromBuildMemo: () => fromBuildMemo,
  isLayer: () => isLayer,
  launch: () => launch,
  makeMemoMap: () => makeMemoMap,
  makeMemoMapUnsafe: () => makeMemoMapUnsafe,
  merge: () => merge2,
  mergeAll: () => mergeAll2,
  mock: () => mock,
  orDie: () => orDie2,
  parentSpan: () => parentSpan,
  provide: () => provide2,
  provideMerge: () => provideMerge,
  satisfiesErrorType: () => satisfiesErrorType,
  satisfiesServicesType: () => satisfiesServicesType,
  satisfiesSuccessType: () => satisfiesSuccessType,
  span: () => span,
  succeed: () => succeed4,
  succeedServices: () => succeedServices,
  sync: () => sync2,
  syncServices: () => syncServices,
  unwrap: () => unwrap,
  updateService: () => updateService2,
  withParentSpan: () => withParentSpan2,
  withSpan: () => withSpan2
});

// ../../node_modules/effect/dist/Deferred.js
var TypeId5 = "~effect/Deferred";
var DeferredProto = {
  [TypeId5]: {
    _A: identity,
    _E: identity
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var makeUnsafe2 = () => {
  const self = Object.create(DeferredProto);
  self.resumes = void 0;
  self.effect = void 0;
  return self;
};
var _await = (self) => callback((resume) => {
  if (self.effect) return resume(self.effect);
  self.resumes ??= [];
  self.resumes.push(resume);
  return sync(() => {
    const index = self.resumes.indexOf(resume);
    self.resumes.splice(index, 1);
  });
});
var completeWith = /* @__PURE__ */ dual(2, (self, effect2) => sync(() => doneUnsafe(self, effect2)));
var done3 = completeWith;
var doneUnsafe = (self, effect2) => {
  if (self.effect) return false;
  self.effect = effect2;
  if (self.resumes) {
    for (let i = 0; i < self.resumes.length; i++) {
      self.resumes[i](effect2);
    }
    self.resumes = void 0;
  }
  return true;
};

// ../../node_modules/effect/dist/Scope.js
var Scope = scopeTag;
var makeUnsafe3 = scopeMakeUnsafe;
var provide = provideScope;
var forkUnsafe2 = scopeForkUnsafe;
var close = scopeClose;

// ../../node_modules/effect/dist/Layer.js
var TypeId6 = "~effect/Layer";
var MemoMapTypeId = "~effect/Layer/MemoMap";
var isLayer = (u) => hasProperty(u, TypeId6);
var LayerProto = {
  [TypeId6]: {
    _ROut: identity,
    _E: identity,
    _RIn: identity
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var fromBuildUnsafe = (build2) => {
  const self = Object.create(LayerProto);
  self.build = build2;
  return self;
};
var fromBuild = (build2) => fromBuildUnsafe((memoMap, scope3) => {
  const layerScope = forkUnsafe2(scope3);
  return onExit(build2(memoMap, layerScope), (exit3) => exit3._tag === "Failure" ? close(layerScope, exit3) : void_);
});
var fromBuildMemo = (build2) => {
  const self = fromBuild((memoMap, scope3) => memoMap.getOrElseMemoize(self, scope3, build2));
  return self;
};
var MemoMapImpl = class {
  get [MemoMapTypeId]() {
    return MemoMapTypeId;
  }
  map = /* @__PURE__ */ new Map();
  getOrElseMemoize(layer, scope3, build2) {
    if (this.map.has(layer)) {
      const entry2 = this.map.get(layer);
      entry2.observers++;
      return andThen(scopeAddFinalizerExit(scope3, (exit3) => entry2.finalizer(exit3)), entry2.effect);
    }
    const layerScope = makeUnsafe3();
    const deferred = makeUnsafe2();
    const entry = {
      observers: 1,
      effect: _await(deferred),
      finalizer: (exit3) => suspend(() => {
        entry.observers--;
        if (entry.observers === 0) {
          this.map.delete(layer);
          return close(layerScope, exit3);
        }
        return void_;
      })
    };
    this.map.set(layer, entry);
    return scopeAddFinalizerExit(scope3, entry.finalizer).pipe(flatMap(() => build2(this, layerScope)), onExit((exit3) => {
      entry.effect = exit3;
      return done3(deferred, exit3);
    }));
  }
};
var makeMemoMapUnsafe = () => new MemoMapImpl();
var makeMemoMap = /* @__PURE__ */ sync(makeMemoMapUnsafe);
var CurrentMemoMap = class extends (/* @__PURE__ */ Service()("effect/Layer/CurrentMemoMap")) {
  static getOrCreate = /* @__PURE__ */ getOrElse(this, makeMemoMapUnsafe);
};
var buildWithMemoMap = /* @__PURE__ */ dual(3, (self, memoMap, scope3) => provideService(map(self.build(memoMap, scope3), add(CurrentMemoMap, memoMap)), CurrentMemoMap, memoMap));
var build = (self) => withFiber((fiber3) => buildWithMemoMap(self, CurrentMemoMap.getOrCreate(fiber3.services), getUnsafe2(fiber3.services, Scope)));
var buildWithScope = /* @__PURE__ */ dual(2, (self, scope3) => withFiber((fiber3) => buildWithMemoMap(self, CurrentMemoMap.getOrCreate(fiber3.services), scope3)));
var succeed4 = function() {
  if (arguments.length === 1) {
    return (resource) => succeedServices(make3(arguments[0], resource));
  }
  return succeedServices(make3(arguments[0], arguments[1]));
};
var succeedServices = (services3) => fromBuildUnsafe(constant(succeed3(services3)));
var empty3 = /* @__PURE__ */ succeedServices(/* @__PURE__ */ empty());
var sync2 = function() {
  if (arguments.length === 1) {
    return (evaluate2) => syncServices(() => make3(arguments[0], evaluate2()));
  }
  return syncServices(() => make3(arguments[0], arguments[1]()));
};
var syncServices = (evaluate2) => fromBuildMemo(constant(sync(evaluate2)));
var effect = function() {
  if (arguments.length === 1) {
    return (effect2) => effectImpl(arguments[0], effect2);
  }
  return effectImpl(arguments[0], arguments[1]);
};
var effectImpl = (service3, effect2) => effectServices(map(effect2, (value) => make3(service3, value)));
var effectServices = (effect2) => fromBuildMemo((_, scope3) => provide(effect2, scope3));
var effectDiscard = (effect2) => effectServices(as(effect2, empty()));
var unwrap = (self) => {
  const service3 = Service("effect/Layer/unwrap");
  return flatMap2(effect(service3)(self), get(service3));
};
var mergeAllEffect = (layers, memoMap, scope3) => {
  const parentScope = forkUnsafe2(scope3, "parallel");
  return forEach(layers, (layer) => layer.build(memoMap, forkUnsafe2(parentScope, "sequential")), {
    concurrency: layers.length
  }).pipe(map((services3) => mergeAll(...services3)));
};
var mergeAll2 = (...layers) => fromBuild((memoMap, scope3) => mergeAllEffect(layers, memoMap, scope3));
var merge2 = /* @__PURE__ */ dual(2, (self, that) => mergeAll2(self, ...Array.isArray(that) ? that : [that]));
var provideWith = (self, that, f) => fromBuild((memoMap, scope3) => flatMap(Array.isArray(that) ? mergeAllEffect(that, memoMap, scope3) : that.build(memoMap, scope3), (context) => self.build(memoMap, scope3).pipe(provideServices(context), map((merged) => f(merged, context)))));
var provide2 = /* @__PURE__ */ dual(2, (self, that) => provideWith(self, that, identity));
var provideMerge = /* @__PURE__ */ dual(2, (self, that) => provideWith(self, that, (self2, that2) => merge(that2, self2)));
var flatMap2 = /* @__PURE__ */ dual(2, (self, f) => fromBuild((memoMap, scope3) => flatMap(self.build(memoMap, scope3), (context) => f(context).build(memoMap, scope3))));
var orDie2 = (self) => fromBuildUnsafe((memoMap, scope3) => orDie(self.build(memoMap, scope3)));
var catch_2 = /* @__PURE__ */ dual(2, (self, onError3) => fromBuildUnsafe((memoMap, scope3) => catch_(self.build(memoMap, scope3), (e) => onError3(e).build(memoMap, scope3))));
var catchTag2 = /* @__PURE__ */ dual(3, (self, k, f) => fromBuildUnsafe((memoMap, scope3) => catchTag(self.build(memoMap, scope3), k, (error) => f(error).build(memoMap, scope3))));
var catchCause2 = /* @__PURE__ */ dual(2, (self, onError3) => fromBuildUnsafe((memoMap, scope3) => catchCause(self.build(memoMap, scope3), (cause) => onError3(cause).build(memoMap, scope3))));
var updateService2 = /* @__PURE__ */ dual(3, (layer, service3, f) => provide2(layer, effect(service3)(map(service3.asEffect(), f))));
var fresh = (self) => fromBuildUnsafe((_, scope3) => self.build(makeMemoMapUnsafe(), scope3));
var launch = (self) => scoped(andThen(build(self), never));
var mock = (service3) => (implementation) => succeed4(service3)(new Proxy({
  ...implementation
}, {
  get(target, prop, _receiver) {
    if (prop in target) {
      return target[prop];
    }
    const prevLimit = Error.stackTraceLimit;
    Error.stackTraceLimit = 2;
    const error = new Error(`${service3.key}: Unimplemented method "${prop.toString()}"`);
    Error.stackTraceLimit = prevLimit;
    error.name = "UnimplementedError";
    return makeUnimplemented(error);
  },
  has: constTrue
}));
var makeUnimplemented = (error) => {
  const dead = die(error);
  function unimplemented() {
    return dead;
  }
  Object.assign(unimplemented, dead);
  Object.setPrototypeOf(unimplemented, Object.getPrototypeOf(dead));
  return unimplemented;
};
var satisfiesSuccessType = () => (layer) => layer;
var satisfiesErrorType = () => (layer) => layer;
var satisfiesServicesType = () => (layer) => layer;
var span = (name, options) => {
  options = addSpanStackTrace(options);
  return effect(ParentSpan, options?.onEnd ? tap(makeSpanScoped(name, options), (span2) => addFinalizer((exit3) => options.onEnd(span2, exit3))) : makeSpanScoped(name, options));
};
var parentSpan = (span2) => succeedServices(ParentSpan.serviceMap(span2));
var withSpan2 = function() {
  const dataFirst = typeof arguments[0] !== "string";
  const name = dataFirst ? arguments[1] : arguments[0];
  const options = addSpanStackTrace(dataFirst ? arguments[2] : arguments[1]);
  if (dataFirst) {
    const self = arguments[0];
    return unwrap(map(options?.onEnd !== void 0 ? tap(makeSpanScoped(name, options), (span2) => addFinalizer((exit3) => options.onEnd(span2, exit3))) : makeSpanScoped(name, options), (span2) => withParentSpan2(self, span2)));
  }
  return (self) => unwrap(map(options?.onEnd !== void 0 ? tap(makeSpanScoped(name, options), (span2) => addFinalizer((exit3) => options.onEnd(span2, exit3))) : makeSpanScoped(name, options), (span2) => withParentSpan2(self, span2)));
};
var withParentSpan2 = function() {
  const dataFirst = isLayer(arguments[0]);
  const span2 = dataFirst ? arguments[1] : arguments[0];
  let options = dataFirst ? arguments[2] : arguments[1];
  let provideStackFrame = identity;
  if (span2._tag === "Span") {
    options = addSpanStackTrace(options);
    provideStackFrame = provideSpanStackFrame2(span2.name, options?.captureStackTrace);
  }
  const parentSpanLayer = parentSpan(span2);
  if (dataFirst) {
    return provide2(provideStackFrame(arguments[0]), parentSpanLayer);
  }
  return (self) => provide2(provideStackFrame(self), parentSpanLayer);
};
var provideSpanStackFrame2 = (name, stack) => {
  stack = typeof stack === "function" ? stack : constUndefined;
  return updateService2(CurrentStackFrame, (parent) => ({
    name,
    stack,
    parent
  }));
};

// ../../node_modules/effect/dist/ExecutionPlan.js
var TypeId7 = "~effect/ExecutionPlan";
var Proto2 = {
  [TypeId7]: TypeId7,
  get withRequirements() {
    const self = this;
    return servicesWith((services3) => succeed3(makeProto(self.steps.map((step) => ({
      ...step,
      provide: isLayer(step.provide) ? provide2(step.provide, succeedServices(services3)) : step.provide
    })))));
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var makeProto = (steps) => {
  const self = Object.create(Proto2);
  self.steps = steps;
  return self;
};
var CurrentMetadata = /* @__PURE__ */ Reference("effect/ExecutionPlan/CurrentMetadata", {
  defaultValue: /* @__PURE__ */ constant({
    attempt: 0,
    stepIndex: 0
  })
});

// ../../node_modules/effect/dist/Data.js
var Data_exports = {};
__export(Data_exports, {
  Class: () => Class3,
  Error: () => Error3,
  TaggedClass: () => TaggedClass,
  TaggedError: () => TaggedError2,
  taggedEnum: () => taggedEnum
});
var Class3 = class extends Class {
  constructor(props) {
    super();
    if (props) {
      Object.assign(this, props);
    }
  }
};
var TaggedClass = (tag) => class extends Class3 {
  _tag = tag;
};
var taggedEnum = () => new Proxy({}, {
  get(_target, tag, _receiver) {
    if (tag === "$is") {
      return isTagged;
    } else if (tag === "$match") {
      return taggedMatch;
    }
    return (props) => ({
      _tag: tag,
      ...props
    });
  }
});
function taggedMatch() {
  if (arguments.length === 1) {
    const cases2 = arguments[0];
    return function(value2) {
      return cases2[value2._tag](value2);
    };
  }
  const value = arguments[0];
  const cases = arguments[1];
  return cases[value._tag](value);
}
var Error3 = Error2;
var TaggedError2 = TaggedError;

// ../../node_modules/effect/dist/Pull.js
var catchDone = /* @__PURE__ */ dual(2, (effect2, f) => catchCauseIf(effect2, filterDoneLeftover, (l) => f(l)));
var filterDone = /* @__PURE__ */ composePassthrough(findError2, (e) => isDone2(e) ? succeed2(e) : fail2(e));
var filterDoneLeftover = /* @__PURE__ */ composePassthrough(findError2, (e) => isDone2(e) ? succeed2(e.value) : fail2(e));
var matchEffect2 = /* @__PURE__ */ dual(2, (self, options) => matchCauseEffect(self, {
  onSuccess: options.onSuccess,
  onFailure: (cause) => {
    const halt = filterDone(cause);
    return !isFailure2(halt) ? options.onDone(halt.success.value) : options.onFailure(halt.failure);
  }
}));

// ../../node_modules/effect/dist/Schedule.js
var TypeId8 = "~effect/Schedule";
var CurrentMetadata2 = /* @__PURE__ */ Reference("effect/Schedule/CurrentMetadata", {
  defaultValue: /* @__PURE__ */ constant({
    input: void 0,
    output: void 0,
    duration: zero,
    attempt: 0,
    start: 0,
    now: 0,
    elapsed: 0,
    elapsedSincePrevious: 0
  })
});
var ScheduleProto = {
  [TypeId8]: {
    _Out: identity,
    _In: identity,
    _Env: identity
  },
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var isSchedule = (u) => hasProperty(u, TypeId8);
var fromStep = (step) => {
  const self = Object.create(ScheduleProto);
  self.step = step;
  return self;
};
var metadataFn = () => {
  let n = 0;
  let previous;
  let start;
  return (now, input) => {
    if (start === void 0) start = now;
    const elapsed = now - start;
    const elapsedSincePrevious = previous === void 0 ? 0 : now - previous;
    previous = now;
    return {
      input,
      attempt: ++n,
      start,
      now,
      elapsed,
      elapsedSincePrevious
    };
  };
};
var fromStepWithMetadata = (step) => fromStep(map(step, (f) => {
  const meta = metadataFn();
  return (now, input) => f(meta(now, input));
}));
var toStep = (schedule2) => catchCause(schedule2.step, (cause) => succeed3(() => failCause(cause)));
var toStepWithMetadata = (schedule2) => clockWith((clock) => map(toStep(schedule2), (step) => {
  const metaFn = metadataFn();
  return (input) => suspend(() => {
    const now = clock.currentTimeMillisUnsafe();
    return flatMap(step(now, input), ([output, duration]) => {
      const meta = metaFn(now, input);
      meta.output = output;
      meta.duration = duration;
      return as(sleep(duration), meta);
    });
  });
}));
var passthrough = (self) => fromStep(map(toStep(self), (step) => (now, input) => matchEffect2(step(now, input), {
  onSuccess: (result3) => succeed3([input, result3[1]]),
  onFailure: failCause,
  onDone: () => done2(input)
})));
var recurs = (times) => while_(forever2, ({
  attempt
}) => succeed3(attempt <= times));
var spaced = (duration) => {
  const decoded = fromInputUnsafe(duration);
  return fromStepWithMetadata(succeed3((meta) => succeed3([meta.attempt - 1, decoded])));
};
var while_ = /* @__PURE__ */ dual(2, (self, predicate) => fromStep(map(toStep(self), (step) => {
  const meta = metadataFn();
  return (now, input) => flatMap(step(now, input), (result3) => {
    const [output, duration] = result3;
    return flatMap(predicate({
      ...meta(now, input),
      output,
      duration
    }), (check) => check ? succeed3(result3) : done2(output));
  });
})));
var forever2 = /* @__PURE__ */ spaced(zero);

// ../../node_modules/effect/dist/internal/layer.js
var provideLayer = (self, layer, options) => scopedWith((scope3) => flatMap(options?.local ? buildWithMemoMap(layer, makeMemoMapUnsafe(), scope3) : buildWithScope(layer, scope3), (context) => provideServices(self, context)));
var provide3 = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (self, source, options) => isServiceMap(source) ? provideServices(self, source) : provideLayer(self, Array.isArray(source) ? mergeAll2(...source) : source, options));

// ../../node_modules/effect/dist/internal/schedule.js
var repeatOrElse = /* @__PURE__ */ dual(3, (self, schedule2, orElse) => flatMap(toStepWithMetadata(schedule2), (step) => {
  let meta = CurrentMetadata2.defaultValue();
  return catch_(forever(tap(flatMap(suspend(() => provideService(self, CurrentMetadata2, meta)), step), (meta_) => sync(() => {
    meta = meta_;
  })), {
    disableYield: true
  }), (error) => isDone(error) ? succeed3(error.value) : orElse(error, meta.attempt === 0 ? none2() : some2(meta)));
}));
var retryOrElse = /* @__PURE__ */ dual(3, (self, policy, orElse) => flatMap(toStepWithMetadata(policy), (step) => {
  let meta = CurrentMetadata2.defaultValue();
  let lastError;
  const loop = catch_(suspend(() => provideService(self, CurrentMetadata2, meta)), (error) => {
    lastError = error;
    return flatMap(step(error), (meta_) => {
      meta = meta_;
      return loop;
    });
  });
  return catchDone(loop, (out) => internalCall(() => orElse(lastError, out)));
}));
var repeat = /* @__PURE__ */ dual(2, (self, options) => {
  const schedule2 = typeof options === "function" ? options(identity) : isSchedule(options) ? options : buildFromOptions(options);
  return repeatOrElse(self, schedule2, fail3);
});
var retry = /* @__PURE__ */ dual(2, (self, options) => {
  const schedule2 = typeof options === "function" ? options(identity) : isSchedule(options) ? options : buildFromOptions(options);
  return retryOrElse(self, schedule2, fail3);
});
var scheduleFrom = /* @__PURE__ */ dual(3, (self, initial, schedule2) => flatMap(toStepWithMetadata(schedule2), (step) => {
  let meta = CurrentMetadata2.defaultValue();
  const selfWithMeta = suspend(() => provideService(self, CurrentMetadata2, meta));
  return catch_(flatMap(step(initial), (meta_) => {
    meta = meta_;
    const body = constant(flatMap(selfWithMeta, step));
    return whileLoop({
      while: constTrue,
      body,
      step(meta_2) {
        meta = meta_2;
      }
    });
  }), (error) => isDone(error) ? succeed3(error.value) : fail3(error));
}));
var passthroughForever = /* @__PURE__ */ passthrough(forever2);
var buildFromOptions = (options) => {
  let schedule2 = options.schedule ?? passthroughForever;
  if (options.while) {
    schedule2 = while_(schedule2, ({
      input
    }) => {
      const applied = options.while(input);
      return isEffect(applied) ? applied : succeed3(applied);
    });
  }
  if (options.until) {
    schedule2 = while_(schedule2, ({
      input
    }) => {
      const applied = options.until(input);
      return isEffect(applied) ? map(applied, (b) => !b) : succeed3(!applied);
    });
  }
  if (options.times !== void 0) {
    schedule2 = while_(schedule2, ({
      attempt
    }) => succeed3(attempt <= options.times));
  }
  return schedule2;
};

// ../../node_modules/effect/dist/internal/executionPlan.js
var withExecutionPlan = /* @__PURE__ */ dual(2, (self, plan) => suspend(() => {
  let i = 0;
  let meta = {
    attempt: 0,
    stepIndex: 0
  };
  const provideMeta = provideServiceEffect(CurrentMetadata, sync(() => {
    meta = {
      attempt: meta.attempt + 1,
      stepIndex: i
    };
    return meta;
  }));
  let result3;
  return flatMap(whileLoop({
    while: () => i < plan.steps.length && (result3 === void 0 || isFailure2(result3)),
    body() {
      const step = plan.steps[i];
      let nextEffect = provideMeta(provide3(self, step.provide));
      if (result3) {
        let attempted = false;
        const wrapped = nextEffect;
        nextEffect = suspend(() => {
          if (attempted) return wrapped;
          attempted = true;
          return result3.asEffect();
        });
        nextEffect = retry(nextEffect, scheduleFromStep(step, false));
      } else {
        const schedule2 = scheduleFromStep(step, true);
        nextEffect = schedule2 ? retry(nextEffect, schedule2) : nextEffect;
      }
      return result(nextEffect);
    },
    step(result_) {
      result3 = result_;
      i++;
    }
  }), () => result3.asEffect());
}));
var scheduleFromStep = (step, first) => {
  if (!first) {
    return buildFromOptions({
      schedule: step.schedule ? step.schedule : step.attempts ? void 0 : scheduleOnce,
      times: step.attempts,
      while: step.while
    });
  } else if (step.attempts === 1 || !(step.schedule || step.attempts)) {
    return void 0;
  }
  return buildFromOptions({
    schedule: step.schedule,
    while: step.while,
    times: step.attempts ? step.attempts - 1 : void 0
  });
};
var scheduleOnce = /* @__PURE__ */ recurs(1);

// ../../node_modules/effect/dist/Request.js
var TypeId9 = "~effect/Request";
var requestVariance = /* @__PURE__ */ byReferenceUnsafe({
  /* c8 ignore next */
  _E: (_) => _,
  /* c8 ignore next */
  _A: (_) => _,
  /* c8 ignore next */
  _R: (_) => _
});
var RequestPrototype = {
  ...StructuralProto,
  [TypeId9]: requestVariance
};
var makeEntry = (options) => options;

// ../../node_modules/effect/dist/internal/request.js
var request = /* @__PURE__ */ dual(2, (self, resolver) => {
  const withResolver = (resolver2) => callback((resume) => {
    const entry = addEntry(resolver2, self, resume, getCurrentFiber());
    return maybeRemoveEntry(resolver2, entry);
  });
  return isEffect(resolver) ? flatMap(resolver, withResolver) : withResolver(resolver);
});
var requestUnsafe = (self, options) => {
  const entry = addEntry(options.resolver, self, options.onExit, {
    services: options.services,
    currentScheduler: get(options.services, Scheduler)
  });
  return () => removeEntryUnsafe(options.resolver, entry);
};
var batchPool = [];
var pendingBatches = /* @__PURE__ */ new Map();
var addEntry = (resolver, request3, resume, fiber3) => {
  let batchMap = pendingBatches.get(resolver);
  if (!batchMap) {
    batchMap = /* @__PURE__ */ new Map();
    pendingBatches.set(resolver, batchMap);
  }
  let batch;
  let completed = false;
  const entry = makeEntry({
    request: request3,
    services: fiber3.services,
    uninterruptible: false,
    completeUnsafe(effect2) {
      if (completed) return;
      completed = true;
      resume(effect2);
      batch?.entrySet.delete(entry);
    }
  });
  if (resolver.preCheck !== void 0 && !resolver.preCheck(entry)) {
    return entry;
  }
  const key = resolver.batchKey(entry);
  batch = batchMap.get(key);
  if (!batch) {
    if (batchPool.length > 0) {
      batch = batchPool.pop();
      batch.key = key;
      batch.resolver = resolver;
      batch.map = batchMap;
    } else {
      const newBatch = {
        key,
        resolver,
        map: batchMap,
        entrySet: /* @__PURE__ */ new Set(),
        entries: /* @__PURE__ */ new Set(),
        delayEffect: flatMap(suspend(() => newBatch.resolver.delay), (_) => runBatch(newBatch)),
        run: onExit(suspend(() => newBatch.resolver.runAll(Array.from(newBatch.entries), newBatch.key)), (exit3) => {
          for (const entry2 of newBatch.entrySet) {
            entry2.completeUnsafe(exit3._tag === "Success" ? exitDie(new Error("Effect.request: RequestResolver did not complete request", {
              cause: entry2.request
            })) : exit3);
          }
          newBatch.entries.clear();
          if (batchPool.length < 128) {
            newBatch.entrySet.clear();
            newBatch.key = void 0;
            newBatch.fiber = void 0;
            batchPool.push(newBatch);
          }
          return void_;
        })
      };
      batch = newBatch;
    }
    batchMap.set(key, batch);
    batch.fiber = runFork(batch.delayEffect, {
      scheduler: fiber3.currentScheduler
    });
  }
  batch.entrySet.add(entry);
  batch.entries.add(entry);
  if (batch.resolver.collectWhile(batch.entries)) return entry;
  batch.fiber.interruptUnsafe(fiber3.id);
  batch.fiber = runFork(runBatch(batch), {
    scheduler: fiber3.currentScheduler
  });
  return entry;
};
var removeEntryUnsafe = (resolver, entry) => {
  if (entry.uninterruptible) return;
  const batchMap = pendingBatches.get(resolver);
  if (!batchMap) return;
  const key = resolver.batchKey(entry.request);
  const batch = batchMap.get(key);
  if (!batch) return;
  batch.entries.delete(entry);
  batch.entrySet.delete(entry);
  if (batch.entries.size === 0) {
    batchMap.delete(key);
    batch.fiber?.interruptUnsafe();
  }
};
var maybeRemoveEntry = (resolver, entry) => sync(() => removeEntryUnsafe(resolver, entry));
function runBatch(batch) {
  if (!batch.map.has(batch.key)) return void_;
  batch.map.delete(batch.key);
  return batch.run;
}

// ../../node_modules/effect/dist/Metric.js
var CurrentMetricAttributesKey = "effect/Metric/CurrentMetricAttributes";
var CurrentMetricAttributes = /* @__PURE__ */ Reference(CurrentMetricAttributesKey, {
  defaultValue: () => ({})
});
var MetricRegistryKey = "~effect/observability/Metric/MetricRegistryKey";
var MetricRegistry = /* @__PURE__ */ Reference(MetricRegistryKey, {
  defaultValue: () => /* @__PURE__ */ new Map()
});
var TypeId10 = "~effect/observability/Metric";
var Metric$ = class {
  [TypeId10] = TypeId10;
  #metadataCache = /* @__PURE__ */ new WeakMap();
  #metadata;
  id;
  description;
  attributes;
  constructor(id, description, attributes) {
    this.id = id;
    this.description = description;
    this.attributes = attributes;
  }
  valueUnsafe(context) {
    return this.hook(context).get(context);
  }
  modifyUnsafe(input, context) {
    return this.hook(context).modify(input, context);
  }
  updateUnsafe(input, context) {
    return this.hook(context).update(input, context);
  }
  hook(context) {
    const extraAttributes = get(context, CurrentMetricAttributes);
    if (Object.keys(extraAttributes).length === 0) {
      if (isNotUndefined(this.#metadata)) {
        return this.#metadata.hooks;
      }
      this.#metadata = this.getOrCreate(context, this.attributes);
      return this.#metadata.hooks;
    }
    const mergedAttributes = mergeAttributes(this.attributes, extraAttributes);
    let metadata = this.#metadataCache.get(mergedAttributes);
    if (isNotUndefined(metadata)) {
      return metadata.hooks;
    }
    metadata = this.getOrCreate(context, mergedAttributes);
    this.#metadataCache.set(mergedAttributes, metadata);
    return metadata.hooks;
  }
  getOrCreate(context, attributes) {
    const key = makeKey(this, attributes);
    const registry = get(context, MetricRegistry);
    if (registry.has(key)) {
      return registry.get(key);
    }
    const hooks = this.createHooks();
    const meta = {
      id: this.id,
      type: this.type,
      description: this.description,
      attributes: attributesToRecord(attributes),
      hooks
    };
    registry.set(key, meta);
    return meta;
  }
  pipe() {
    return pipeArguments(this, arguments);
  }
};
var update = /* @__PURE__ */ dual(2, (self, input) => servicesWith((services3) => sync(() => self.updateUnsafe(input, services3))));
function makeKey(metric, attributes) {
  let key = `${metric.type}:${metric.id}`;
  if (isNotUndefined(metric.description)) {
    key += `:${metric.description}`;
  }
  if (isNotUndefined(attributes)) {
    key += `:${serializeAttributes(attributes)}`;
  }
  return key;
}
function serializeAttributes(attributes) {
  return serializeEntries(Array.isArray(attributes) ? attributes : Object.entries(attributes));
}
function serializeEntries(entries) {
  return entries.map(([key, value]) => `${key}=${value}`).join(",");
}
function mergeAttributes(self, other) {
  return {
    ...attributesToRecord(self),
    ...attributesToRecord(other)
  };
}
function attributesToRecord(attributes) {
  if (isNotUndefined(attributes) && Array.isArray(attributes)) {
    return attributes.reduce((acc, [key, value]) => {
      acc[key] = value;
      return acc;
    }, {});
  }
  return attributes;
}

// ../../node_modules/effect/dist/Effect.js
var TypeId11 = EffectTypeId;
var YieldableClass = class {
  [Symbol.iterator]() {
    return new SingleShotGen(this);
  }
};
var isEffect2 = (u) => typeof u === "object" && u !== null && TypeId11 in u;
var all2 = all;
var partition2 = partition;
var forEach2 = forEach;
var whileLoop2 = whileLoop;
var promise2 = promise;
var tryPromise2 = tryPromise;
var succeed5 = succeed3;
var succeedNone2 = succeedNone;
var succeedSome2 = succeedSome;
var suspend2 = suspend;
var sync3 = sync;
var void_2 = void_;
var undefined_2 = undefined_;
var callback2 = callback;
var never2 = never;
var gen2 = gen;
var fail4 = fail3;
var failSync2 = failSync;
var failCause2 = failCause;
var failCauseSync2 = failCauseSync;
var die2 = die;
var try_2 = try_;
var yieldNow2 = yieldNow;
var yieldNowWith2 = yieldNowWith;
var withFiber2 = withFiber;
var fromResult2 = fromResult;
var fromOption3 = fromOption2;
var fromNullishOr2 = fromNullishOr;
var fromYieldable2 = fromYieldable;
var flatMap3 = flatMap;
var flatten2 = flatten;
var andThen2 = andThen;
var tap2 = tap;
var result2 = result;
var option2 = option;
var exit2 = exit;
var map2 = map;
var as2 = as;
var asSome2 = asSome;
var asVoid2 = asVoid;
var flip2 = flip;
var zip2 = zip;
var zipWith2 = zipWith;
var catch_3 = catch_;
var catchTag3 = catchTag;
var catchTags2 = catchTags;
var catchReason2 = catchReason;
var catchReasons2 = catchReasons;
var unwrapReason2 = unwrapReason;
var catchCause3 = catchCause;
var catchDefect2 = catchDefect;
var catchIf2 = catchIf;
var catchNoSuchElement2 = catchNoSuchElement;
var catchCauseIf2 = catchCauseIf;
var mapError3 = mapError2;
var mapBoth2 = mapBoth;
var orDie3 = orDie;
var tapError2 = tapError;
var tapErrorTag2 = tapErrorTag;
var tapCause2 = tapCause;
var tapCauseIf2 = tapCauseIf;
var tapDefect2 = tapDefect;
var eventually2 = eventually;
var retry2 = retry;
var retryOrElse2 = retryOrElse;
var sandbox2 = sandbox;
var ignore2 = ignore;
var ignoreCause2 = ignoreCause;
var withExecutionPlan2 = withExecutionPlan;
var orElseSucceed2 = orElseSucceed;
var timeout2 = timeout;
var timeoutOption2 = timeoutOption;
var timeoutOrElse2 = timeoutOrElse;
var delay2 = delay;
var sleep2 = sleep;
var timed2 = timed;
var raceAll2 = raceAll;
var raceAllFirst2 = raceAllFirst;
var race2 = race;
var raceFirst2 = raceFirst;
var filter3 = filter2;
var filterOrElse2 = filterOrElse;
var filterOrFail2 = filterOrFail;
var when2 = when;
var match3 = match2;
var matchEager2 = matchEager;
var matchCause2 = matchCause;
var matchCauseEager2 = matchCauseEager;
var matchCauseEffectEager2 = matchCauseEffectEager;
var matchCauseEffect2 = matchCauseEffect;
var matchEffect3 = matchEffect;
var isFailure4 = isFailure3;
var isSuccess5 = isSuccess3;
var services2 = services;
var servicesWith2 = servicesWith;
var provide4 = provide3;
var provideServices2 = provideServices;
var service2 = service;
var serviceOption2 = serviceOption;
var updateServices2 = updateServices;
var updateService3 = updateService;
var provideService2 = provideService;
var provideServiceEffect2 = provideServiceEffect;
var withConcurrency2 = withConcurrency;
var scope2 = scope;
var scoped2 = scoped;
var scopedWith2 = scopedWith;
var acquireRelease2 = acquireRelease;
var acquireUseRelease2 = acquireUseRelease;
var addFinalizer2 = addFinalizer;
var ensuring2 = ensuring;
var onError2 = onError;
var onErrorIf2 = onErrorIf;
var onExitPrimitive2 = onExitPrimitive;
var onExit2 = onExit;
var onExitIf2 = onExitIf;
var cached2 = cached;
var cachedWithTTL2 = cachedWithTTL;
var cachedInvalidateWithTTL2 = cachedInvalidateWithTTL;
var interrupt2 = interrupt;
var interruptible2 = interruptible;
var onInterrupt2 = onInterrupt;
var uninterruptible2 = uninterruptible;
var uninterruptibleMask2 = uninterruptibleMask;
var interruptibleMask2 = interruptibleMask;
var forever3 = forever;
var repeat2 = repeat;
var repeatOrElse2 = repeatOrElse;
var replicate2 = replicate;
var replicateEffect2 = replicateEffect;
var schedule = /* @__PURE__ */ dual(2, (self, schedule2) => scheduleFrom2(self, void 0, schedule2));
var scheduleFrom2 = scheduleFrom;
var tracer2 = tracer;
var withTracer2 = withTracer;
var withTracerEnabled2 = withTracerEnabled;
var withTracerTiming2 = withTracerTiming;
var annotateSpans2 = annotateSpans;
var annotateCurrentSpan2 = annotateCurrentSpan;
var currentSpan2 = currentSpan;
var currentParentSpan2 = currentParentSpan;
var spanAnnotations2 = spanAnnotations;
var spanLinks2 = spanLinks;
var linkSpans2 = linkSpans;
var makeSpan2 = makeSpan;
var makeSpanScoped2 = makeSpanScoped;
var useSpan2 = useSpan;
var withSpan3 = withSpan;
var withSpanScoped2 = withSpanScoped;
var withParentSpan3 = withParentSpan;
var request2 = request;
var requestUnsafe2 = requestUnsafe;
var forkChild2 = forkChild;
var forkIn2 = forkIn;
var forkScoped2 = forkScoped;
var forkDetach2 = forkDetach;
var awaitAllChildren2 = awaitAllChildren;
var fiber2 = fiber;
var fiberId2 = fiberId;
var runFork2 = runFork;
var runForkWith2 = runForkWith;
var runCallbackWith2 = runCallbackWith;
var runCallback2 = runCallback;
var runPromise2 = runPromise;
var runPromiseWith2 = runPromiseWith;
var runPromiseExit2 = runPromiseExit;
var runPromiseExitWith2 = runPromiseExitWith;
var runSync2 = runSync;
var runSyncWith2 = runSyncWith;
var runSyncExit2 = runSyncExit;
var runSyncExitWith2 = runSyncExitWith;
var fnUntraced2 = fnUntraced;
var fn2 = fn;
var clockWith2 = clockWith;
var logWithLevel2 = logWithLevel;
var log = /* @__PURE__ */ logWithLevel();
var logFatal = /* @__PURE__ */ logWithLevel("Fatal");
var logWarning = /* @__PURE__ */ logWithLevel("Warn");
var logError = /* @__PURE__ */ logWithLevel("Error");
var logInfo = /* @__PURE__ */ logWithLevel("Info");
var logDebug = /* @__PURE__ */ logWithLevel("Debug");
var logTrace = /* @__PURE__ */ logWithLevel("Trace");
var withLogger = /* @__PURE__ */ dual(2, (effect2, logger) => updateService(effect2, CurrentLoggers, (loggers) => /* @__PURE__ */ new Set([...loggers, logger])));
var annotateLogs = /* @__PURE__ */ dual((args2) => isEffect(args2[0]), (effect2, ...args2) => updateService(effect2, CurrentLogAnnotations, (annotations) => {
  const newAnnotations = {
    ...annotations
  };
  if (args2.length === 1) {
    Object.assign(newAnnotations, args2[0]);
  } else {
    newAnnotations[args2[0]] = args2[1];
  }
  return newAnnotations;
}));
var withLogSpan = /* @__PURE__ */ dual(2, (effect2, label) => flatMap(currentTimeMillis, (now) => updateService(effect2, CurrentLogSpans, (spans) => {
  const span2 = [label, now];
  return [span2, ...spans];
})));
var track = /* @__PURE__ */ dual((args2) => isEffect2(args2[0]), (self, metric, f) => onExit2(self, (exit3) => {
  const input = f === void 0 ? exit3 : internalCall(() => f(exit3));
  return update(metric, input);
}));
var trackSuccesses = /* @__PURE__ */ dual((args2) => isEffect2(args2[0]), (self, metric, f) => tap2(self, (value) => {
  const input = f === void 0 ? value : f(value);
  return update(metric, input);
}));
var trackErrors = /* @__PURE__ */ dual((args2) => isEffect2(args2[0]), (self, metric, f) => tapError2(self, (error) => {
  const input = f === void 0 ? error : internalCall(() => f(error));
  return update(metric, input);
}));
var trackDefects = /* @__PURE__ */ dual((args2) => isEffect2(args2[0]), (self, metric, f) => tapDefect2(self, (defect) => {
  const input = f === void 0 ? defect : internalCall(() => f(defect));
  return update(metric, input);
}));
var trackDuration = /* @__PURE__ */ dual((args2) => isEffect2(args2[0]), (self, metric, f) => clockWith2((clock) => {
  const startTime = clock.currentTimeNanosUnsafe();
  return onExit2(self, () => {
    const endTime = clock.currentTimeNanosUnsafe();
    const duration = subtract(fromInputUnsafe(endTime), fromInputUnsafe(startTime));
    const input = f === void 0 ? duration : internalCall(() => f(duration));
    return update(metric, input);
  });
}));
var Transaction = class extends (/* @__PURE__ */ Service()("effect/Effect/Transaction")) {
};
var atomic = (effect2) => atomicWith(() => effect2);
var atomicWith = (f) => withFiber2((fiber3) => {
  if (fiber3.services.mapUnsafe.has(Transaction.key)) {
    return internalCall(() => f(getUnsafe2(fiber3.services, Transaction)));
  }
  return transactionWith(f);
});
var transaction = (effect2) => transactionWith(() => effect2);
var transactionWith = (f) => withFiber2((fiber3) => {
  const state = {
    journal: /* @__PURE__ */ new Map(),
    retry: false
  };
  const scheduler = fiber3.currentScheduler;
  let result3;
  return uninterruptibleMask2((restore) => flatMap3(whileLoop2({
    while: () => !result3,
    body: constant(restore(suspend2(() => f(state))).pipe(provideService2(Transaction, state), tapCause2(() => {
      if (!state.retry) return void_2;
      return restore(awaitPendingTransaction(state));
    }), exit2)),
    step(exit3) {
      if (state.retry || !isTransactionConsistent(state)) {
        return clearTransaction(state);
      }
      if (isSuccess4(exit3)) {
        commitTransaction(scheduler, state);
      } else {
        clearTransaction(state);
      }
      result3 = exit3;
    }
  }), () => result3));
});
var isTransactionConsistent = (state) => {
  for (const [ref, {
    version: version2
  }] of state.journal) {
    if (ref.version !== version2) {
      return false;
    }
  }
  return true;
};
var awaitPendingTransaction = (state) => suspend2(() => {
  const key = {};
  const refs = Array.from(state.journal.keys());
  const clearPending = () => {
    for (const clear of refs) {
      clear.pending.delete(key);
    }
  };
  return callback2((resume) => {
    const onCall = () => {
      clearPending();
      resume(void_2);
    };
    for (const ref of refs) {
      ref.pending.set(key, onCall);
    }
    return sync3(clearPending);
  });
});
function commitTransaction(scheduler, state) {
  for (const [ref, {
    value
  }] of state.journal) {
    if (value !== ref.value) {
      ref.version = ref.version + 1;
      ref.value = value;
    }
    for (const pending of ref.pending.values()) {
      scheduler.scheduleTask(pending, 0);
    }
    ref.pending.clear();
  }
}
function clearTransaction(state) {
  state.retry = false;
  state.journal.clear();
}
var retryTransaction = /* @__PURE__ */ flatMap3(/* @__PURE__ */ Transaction.asEffect(), (state) => {
  state.retry = true;
  return interrupt2;
});
var effectify = (fn3, onError3, onSyncError) => (...args2) => callback2((resume) => {
  try {
    fn3(...args2, (err, result3) => {
      if (err) {
        resume(fail4(onError3 ? onError3(err, args2) : err));
      } else {
        resume(succeed5(result3));
      }
    });
  } catch (err) {
    resume(onSyncError ? fail4(onSyncError(err, args2)) : die2(err));
  }
});
var satisfiesSuccessType2 = () => (effect2) => effect2;
var satisfiesErrorType2 = () => (effect2) => effect2;
var satisfiesServicesType2 = () => (effect2) => effect2;
var mapEager2 = mapEager;
var mapErrorEager2 = mapErrorEager;
var mapBothEager2 = mapBothEager;
var flatMapEager2 = flatMapEager;
var catchEager2 = catchEager;
var fnUntracedEager2 = fnUntracedEager;

// src/protocol/messages.ts
var VALID_TERMINAL_MESSAGE_TYPES = /* @__PURE__ */ new Set([
  "hello",
  "action.permission",
  "action.question",
  "action.plan-review",
  "action.turn-auto-approve",
  "request.session",
  "request.overview",
  "request.resync",
  "hint.session-dead",
  "poll",
  // Pi driver control messages (handled in handleTerminalMessage's switch).
  "pi.start",
  "pi.prompt",
  "pi.steer",
  "pi.abort",
  "pi.set-thinking",
  "pi.set-model",
  "pi.resume",
  "pi.compact",
  "pi.new-session",
  "pi.stop"
]);
function isHookMessage(obj) {
  return typeof obj.event === "string" && typeof obj.session_id === "string";
}
function parseTerminalMessage(line) {
  try {
    const msg = JSON.parse(line);
    if (typeof msg !== "object" || msg === null) return null;
    if (isHookMessage(msg)) return null;
    if (!VALID_TERMINAL_MESSAGE_TYPES.has(msg.type)) return null;
    return msg;
  } catch {
    return null;
  }
}

// src/handlers/event-handler.ts
import { homedir } from "os";
import { join } from "path";

// src/services/session-store.ts
var SessionStore = ServiceMap_exports.Service("SessionStore");
function extractLatestMessage(s) {
  if (s.streamingText) return s.streamingText;
  for (let i = s.turns.length - 1; i >= 0; i--) {
    const turn = s.turns[i];
    if (turn.stopText) return turn.stopText;
    for (let j = turn.steps.length - 1; j >= 0; j--) {
      const step = turn.steps[j];
      for (let k = step.tools.length - 1; k >= 0; k--) {
        if (step.tools[k].postText) return step.tools[k].postText;
      }
      if (step.text) return step.text;
    }
  }
  return null;
}
function extractLatestUserPrompt(s) {
  for (let i = s.turns.length - 1; i >= 0; i--) {
    if (s.turns[i].prompt?.text) return s.turns[i].prompt.text;
  }
  return null;
}
function makeSessionStore() {
  const sessions = /* @__PURE__ */ new Map();
  const purgeTimers = /* @__PURE__ */ new Map();
  const patchHistories = /* @__PURE__ */ new Map();
  let globalSeq = 0;
  const cancelPurge = (sessionId) => {
    const timer = purgeTimers.get(sessionId);
    if (timer) {
      clearTimeout(timer);
      purgeTimers.delete(sessionId);
    }
  };
  return {
    get: (sessionId) => sessions.get(sessionId),
    set: (sessionId, session) => {
      sessions.set(sessionId, session);
    },
    delete: (sessionId) => {
      cancelPurge(sessionId);
      return sessions.delete(sessionId);
    },
    has: (sessionId) => sessions.has(sessionId),
    getProjectSummaries: () => {
      const byProject = /* @__PURE__ */ new Map();
      for (const session of sessions.values()) {
        if (session.status === "ended") continue;
        const list = byProject.get(session.project) ?? [];
        list.push(session);
        byProject.set(session.project, list);
      }
      return Array.from(byProject.entries()).map(([project, ss]) => ({
        project,
        sessions: ss.map((s) => ({
          sessionId: s.sessionId,
          slug: s.slug,
          displayName: s.displayName,
          status: s.status,
          claudeStatus: s.claudeStatus,
          toolCount: s.totalToolCount,
          lastEventTime: s.lastEventTime,
          latestMessage: extractLatestMessage(s),
          latestUserPrompt: extractLatestUserPrompt(s)
        }))
      }));
    },
    schedulePurge: (sessionId, delayMs, onPurge) => {
      cancelPurge(sessionId);
      const timer = setTimeout(() => {
        purgeTimers.delete(sessionId);
        onPurge();
      }, delayMs);
      purgeTimers.set(sessionId, timer);
    },
    cancelPurge,
    clearAllPurgeTimers: () => {
      for (const timer of purgeTimers.values()) {
        clearTimeout(timer);
      }
      purgeTimers.clear();
    },
    all: () => Array.from(sessions.values()),
    appendPatches: (sessionId, patches) => {
      const history = patchHistories.get(sessionId) ?? [];
      const now = Date.now();
      const stored = patches.map((patch) => ({
        seq: ++globalSeq,
        patch,
        timestamp: now
      }));
      history.push(...stored);
      patchHistories.set(sessionId, history);
      return stored;
    },
    getPatchesSince: (sessionId, since) => {
      const history = patchHistories.get(sessionId) ?? [];
      let lo = 0;
      let hi = history.length;
      while (lo < hi) {
        const mid = lo + hi >>> 1;
        if (history[mid].seq <= since) {
          lo = mid + 1;
        } else {
          hi = mid;
        }
      }
      return history.slice(lo);
    },
    getSessionSeq: (sessionId) => {
      const history = patchHistories.get(sessionId) ?? [];
      return history.length > 0 ? history[history.length - 1].seq : 0;
    },
    clearPatches: (sessionId) => {
      patchHistories.delete(sessionId);
    }
  };
}
var SessionStoreLive = Layer_exports.succeed(SessionStore, makeSessionStore());

// src/services/inbox.ts
var Inbox = ServiceMap_exports.Service("Inbox");
function makeInbox() {
  let counter = 0;
  const items = [];
  const pending = /* @__PURE__ */ new Map();
  const closePendingSocket = (itemId) => {
    const p = pending.get(itemId);
    if (!p) return;
    try {
      p.hookSocket.write(JSON.stringify({}) + "\n");
      p.hookSocket.end();
    } catch {
    }
    pending.delete(itemId);
  };
  const removeItem = (id) => {
    const idx = items.findIndex((i) => i.id === id);
    if (idx === -1) return void 0;
    const [removed] = items.splice(idx, 1);
    pending.delete(id);
    return removed;
  };
  return {
    add: (type, sessionId, project, label, summary, data, hookSocket) => {
      counter++;
      const item = {
        id: counter,
        type,
        sessionId,
        project,
        label,
        timestamp: Date.now(),
        summary,
        data
      };
      items.unshift(item);
      if (hookSocket) {
        pending.set(item.id, { inboxItem: item, hookSocket });
      }
      return item;
    },
    remove: removeItem,
    removeForSession: (sessionId, type) => {
      const removed = [];
      for (let i = items.length - 1; i >= 0; i--) {
        const item = items[i];
        if (item.sessionId === sessionId && (!type || item.type === type)) {
          items.splice(i, 1);
          pending.delete(item.id);
          removed.push(item);
        }
      }
      return removed;
    },
    removeStaleForSession: (sessionId, type) => {
      const removed = [];
      for (let i = items.length - 1; i >= 0; i--) {
        const item = items[i];
        if (item.sessionId === sessionId && (!type || item.type === type)) {
          if (pending.has(item.id)) continue;
          items.splice(i, 1);
          removed.push(item);
        }
      }
      return removed;
    },
    forceCloseStaleForSession: (sessionId) => {
      const removed = [];
      for (let i = items.length - 1; i >= 0; i--) {
        const item = items[i];
        if (item.sessionId !== sessionId) continue;
        closePendingSocket(item.id);
        items.splice(i, 1);
        removed.push(item);
      }
      return removed;
    },
    find: (id) => items.find((i) => i.id === id),
    getPending: (id) => pending.get(id),
    respond: (id, response) => Effect_exports.sync(() => {
      const p = pending.get(id);
      if (!p) return false;
      try {
        p.hookSocket.write(JSON.stringify(response) + "\n");
        p.hookSocket.end();
      } catch {
      }
      pending.delete(id);
      removeItem(id);
      return true;
    }),
    removeBySocket: (socket) => {
      const removed = [];
      for (const [id, p] of pending) {
        if (p.hookSocket === socket) {
          const item = removeItem(id);
          if (item) removed.push(item);
        }
      }
      return removed;
    },
    all: () => [...items]
  };
}
var InboxLive = Layer_exports.succeed(Inbox, makeInbox());

// ../shared/src/safe-bash.ts
import { basename } from "path";

// ../shared/src/services/errors.ts
var FileReadError = class extends Data_exports.TaggedError("FileReadError") {
};
var FileWriteError = class extends Data_exports.TaggedError("FileWriteError") {
};

// ../shared/src/services/fs.ts
import {
  readFileSync,
  writeFileSync,
  appendFileSync,
  existsSync,
  statSync,
  openSync,
  readSync,
  closeSync,
  mkdirSync,
  unlinkSync,
  renameSync
} from "fs";
var Fs = ServiceMap_exports.Service("Fs");
var FsLive = Layer_exports.succeed(Fs, {
  readFile: (path) => Effect_exports.try({
    try: () => readFileSync(path, "utf-8"),
    catch: (cause) => new FileReadError({ path, cause })
  }),
  writeFile: (path, data) => Effect_exports.try({
    try: () => {
      writeFileSync(path, data, "utf-8");
    },
    catch: (cause) => new FileWriteError({ path, cause })
  }),
  appendFile: (path, data) => Effect_exports.try({
    try: () => {
      appendFileSync(path, data, "utf-8");
    },
    catch: (cause) => new FileWriteError({ path, cause })
  }),
  exists: (path) => Effect_exports.sync(() => existsSync(path)),
  stat: (path) => Effect_exports.try({
    try: () => {
      const s = statSync(path);
      return { size: s.size };
    },
    catch: (cause) => new FileReadError({ path, cause })
  }),
  readBytes: (path, offset, length) => Effect_exports.try({
    try: () => {
      const fd = openSync(path, "r");
      const buf = Buffer.alloc(length);
      readSync(fd, buf, 0, length, offset);
      closeSync(fd);
      return buf;
    },
    catch: (cause) => new FileReadError({ path, cause })
  }),
  mkdirp: (path) => Effect_exports.try({
    try: () => {
      mkdirSync(path, { recursive: true });
    },
    catch: (cause) => new FileWriteError({ path, cause })
  }),
  unlinkIfExists: (path) => Effect_exports.sync(() => {
    try {
      unlinkSync(path);
    } catch {
    }
  }),
  rename: (from, to) => Effect_exports.try({
    try: () => {
      renameSync(from, to);
    },
    catch: (cause) => new FileWriteError({ path: from, cause })
  })
});

// src/state/session.ts
var BLOATED_RESULT_FIELDS = ["structured_patch"];
function stripBloatedFields(result3) {
  if (result3 == null || typeof result3 !== "object" || Array.isArray(result3)) return result3;
  const rec = result3;
  const hasBloat = BLOATED_RESULT_FIELDS.some((f) => f in rec);
  if (!hasBloat) return result3;
  const cleaned = {};
  for (const [k, v] of Object.entries(rec)) {
    if (!BLOATED_RESULT_FIELDS.includes(k)) cleaned[k] = v;
  }
  return cleaned;
}
function createSession(sessionId, cwd, source) {
  const project = cwd.split("/").pop() || cwd;
  return {
    sessionId,
    cwd,
    project,
    status: "active",
    claudeStatus: "idle",
    slug: null,
    displayName: null,
    branch: null,
    pid: null,
    modelName: null,
    tmuxSession: null,
    source: source ?? null,
    startTime: Date.now(),
    lastEventTime: Date.now(),
    tokenUsage: null,
    cost: null,
    contextUsage: null,
    piSessionFile: null,
    plan: null,
    streamingText: null,
    permissionMode: null,
    turns: [createTurnNode(0)],
    // turn 0 = pre-prompt activity
    currentTurn: 0,
    toolIndex: {},
    agentIndex: {},
    tasks: {},
    files: {},
    totalToolCount: 0
  };
}
function createTurnNode(turnNumber) {
  return {
    turnNumber,
    prompt: null,
    steps: [],
    agents: [],
    tasks: [],
    toolCount: 0,
    agentCount: 0,
    frozen: false,
    stopText: null,
    stopThinking: null,
    tokenIn: null,
    tokenOut: null
  };
}
function createStepNode(thinking, text) {
  return {
    thinking: thinking ?? null,
    text: text ?? null,
    tools: []
  };
}
function sessionEnd(s) {
  s.status = "ended";
  s.claudeStatus = "idle";
  return [{ op: "set_status", status: "ended" }, { op: "set_claude_status", claudeStatus: "idle" }];
}
function resetSession(s) {
  s.status = "active";
  s.claudeStatus = "idle";
  s.turns = [createTurnNode(0)];
  s.currentTurn = 0;
  s.toolIndex = {};
  s.agentIndex = {};
  s.tasks = {};
  s.files = {};
  s.totalToolCount = 0;
  s.plan = null;
  s.streamingText = null;
  s.tokenUsage = null;
  s.cost = null;
  s.contextUsage = null;
  s.lastEventTime = Date.now();
  return [
    { op: "set_status", status: "active" },
    { op: "set_claude_status", claudeStatus: "idle" },
    { op: "set_plan", plan: null },
    { op: "set_streaming_text", text: null },
    { op: "set_token_usage", usage: { input_tokens: 0, output_tokens: 0, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 } },
    { op: "set_cost", cost: null },
    { op: "set_context_usage", contextUsage: null }
  ];
}
function setClaudeStatus(s, status) {
  if (s.claudeStatus === status) return [];
  s.claudeStatus = status;
  return [{ op: "set_claude_status", claudeStatus: status }];
}
function setPermissionMode(s, mode) {
  s.permissionMode = mode;
  return [{ op: "set_permission_mode", mode }];
}
function setTokenUsage(s, usage) {
  s.tokenUsage = usage;
  return [{ op: "set_token_usage", usage }];
}
function setCost(s, cost) {
  s.cost = cost;
  return [{ op: "set_cost", cost }];
}
function setContextUsage(s, contextUsage) {
  s.contextUsage = contextUsage;
  return [{ op: "set_context_usage", contextUsage }];
}
function setPlan(s, plan) {
  s.plan = plan;
  return [{ op: "set_plan", plan }];
}
function updateMeta(s, opts) {
  s.lastEventTime = Date.now();
  if (opts.pid && opts.pid > 0) s.pid = opts.pid;
  if (opts.slug && !s.slug) s.slug = opts.slug;
  if (opts.displayName) s.displayName = opts.displayName;
  if (opts.branch) s.branch = opts.branch;
  if (opts.modelName) s.modelName = opts.modelName;
  if (opts.tmuxSession && !s.tmuxSession) s.tmuxSession = opts.tmuxSession;
  if (opts.piSessionFile) s.piSessionFile = opts.piSessionFile;
  return [{ op: "set_meta", ...opts }];
}
function currentTurnNode(s) {
  return s.turns[s.turns.length - 1];
}
function getTurnNode(s, turnNumber) {
  return s.turns.find((t) => t.turnNumber === turnNumber);
}
function addPrompt(s, entry) {
  const patches = [];
  const prev = currentTurnNode(s);
  if (prev && !prev.frozen) {
    prev.frozen = true;
    patches.push({ op: "freeze_turn", turnNumber: prev.turnNumber });
  }
  s.currentTurn++;
  const turn = createTurnNode(s.currentTurn);
  turn.prompt = entry;
  s.turns.push(turn);
  patches.push({ op: "add_turn", turn });
  patches.push({ op: "add_prompt", turnNumber: s.currentTurn, prompt: entry });
  return patches;
}
function finalizeLastPrompt(s, stopText, stopThinking) {
  const turn = currentTurnNode(s);
  if (!turn) return [];
  const patches = [];
  if (turn.prompt && !turn.prompt.elapsed && turn.prompt.submitted) {
    turn.prompt.elapsed = (Date.now() - turn.prompt.submitted) / 1e3;
  }
  if (stopText) turn.stopText = stopText;
  if (stopThinking) turn.stopThinking = stopThinking;
  if (stopText || stopThinking) {
    patches.push({
      op: "set_turn_stop",
      turnNumber: turn.turnNumber,
      stopText,
      stopThinking
    });
  }
  return patches;
}
function setTurnTokens(s, tokenIn, tokenOut) {
  const turn = currentTurnNode(s);
  if (!turn) return [];
  turn.tokenIn = tokenIn;
  turn.tokenOut = tokenOut;
  return [{ op: "set_turn_tokens", turnNumber: turn.turnNumber, tokenIn, tokenOut }];
}
function updatePromptAnswer(s, toolUseId, answer) {
  for (const turn of s.turns) {
    if (turn.prompt && turn.prompt.type === "question" && turn.prompt.toolUseId === toolUseId) {
      turn.prompt.answer = answer;
      if (turn.prompt.submitted) {
        turn.prompt.elapsed = (Date.now() - turn.prompt.submitted) / 1e3;
      }
      return [{ op: "set_prompt_answer", turnNumber: turn.turnNumber, toolUseId, answer }];
    }
  }
  return [];
}
function ensureStep(steps, thinking, text) {
  const current = steps.length > 0 ? steps[steps.length - 1] : null;
  if (current) {
    const needsNew = text && text.length > 0 && current.text && current.text.length > 0 && !text.startsWith(current.text) || thinking && thinking.length > 0 && current.thinking && current.thinking.length > 0 && thinking !== current.thinking;
    if (!needsNew) {
      if (text && text.length > 0 && (!current.text || current.text.length === 0)) {
        current.text = text;
      }
      if (thinking && thinking.length > 0 && (!current.thinking || current.thinking.length === 0)) {
        current.thinking = thinking;
      }
      return { step: current, isNew: false };
    }
  }
  const newStep = createStepNode(thinking, text);
  steps.push(newStep);
  return { step: newStep, isNew: true };
}
function addTool(s, tool, agentId, candidateAgentIds) {
  if (tool.toolUseId && s.toolIndex[tool.toolUseId]) return [];
  const patches = [];
  if (agentId === "ambiguous") {
    tool.ambiguous = true;
    tool.candidateAgentIds = candidateAgentIds ?? null;
  }
  const turnNode = getTurnNode(s, tool.turn) || currentTurnNode(s);
  if (!turnNode) return [];
  if (agentId && agentId !== "ambiguous") {
    const agent = s.agentIndex[agentId];
    if (agent) {
      const agentObj = findAgent(s, agentId);
      if (agentObj) {
        const { step } = ensureStep(agentObj.steps, tool.assistantThinking, tool.assistantText);
        step.tools.push(tool);
        agentObj.toolCount++;
        tool.parentAgentId = agentId;
        const stepIdx = agentObj.steps.indexOf(step);
        if (tool.toolUseId) {
          s.toolIndex[tool.toolUseId] = {
            turnNumber: turnNode.turnNumber,
            stepIndex: stepIdx,
            toolIndex: step.tools.length - 1,
            agentId
          };
        }
        patches.push({
          op: "add_tool",
          turnNumber: turnNode.turnNumber,
          stepIndex: stepIdx,
          agentId,
          tool
        });
      }
    }
  } else {
    const { step, isNew } = ensureStep(turnNode.steps, tool.assistantThinking, tool.assistantText);
    if (!isNew && tool.assistantText === step.text) {
      tool.assistantText = null;
    }
    if (!isNew && tool.assistantThinking === step.thinking) {
      tool.assistantThinking = null;
    }
    step.tools.push(tool);
    turnNode.toolCount++;
    const stepIdx = turnNode.steps.indexOf(step);
    if (tool.toolUseId) {
      s.toolIndex[tool.toolUseId] = {
        turnNumber: turnNode.turnNumber,
        stepIndex: stepIdx,
        toolIndex: step.tools.length - 1,
        agentId: null
      };
    }
    patches.push({
      op: "add_tool",
      turnNumber: turnNode.turnNumber,
      stepIndex: stepIdx,
      tool
    });
  }
  s.totalToolCount++;
  return patches;
}
function completeTool(s, toolUseId, result3, status = "done", postText, postThinking) {
  const loc = s.toolIndex[toolUseId];
  if (!loc) return [];
  const tool = findToolByLocation(s, loc);
  if (!tool) return [];
  const cleanResult = stripBloatedFields(result3);
  tool.status = status;
  tool.result = cleanResult;
  if (tool.timestamp) {
    tool.duration = (Date.now() - tool.timestamp) / 1e3;
  }
  if (postText) tool.postText = postText;
  if (postThinking) tool.postThinking = postThinking;
  return [{
    op: "complete_tool",
    toolUseId,
    result: cleanResult,
    status,
    duration: tool.duration ?? void 0,
    postText,
    postThinking
  }];
}
function findToolByLocation(s, loc) {
  if (loc.agentId) {
    const agent = findAgent(s, loc.agentId);
    if (!agent) return void 0;
    const step2 = agent.steps[loc.stepIndex];
    if (!step2) return void 0;
    return step2.tools[loc.toolIndex];
  }
  const turn = getTurnNode(s, loc.turnNumber);
  if (!turn) return void 0;
  const step = turn.steps[loc.stepIndex];
  if (!step) return void 0;
  return step.tools[loc.toolIndex];
}
function addAgent(s, agent) {
  if (s.agentIndex[agent.agentId]) return [];
  const turn = currentTurnNode(s);
  if (!turn) return [];
  s.agentIndex[agent.agentId] = {
    turnNumber: turn.turnNumber,
    agentIndex: turn.agents.length
  };
  turn.agents.push(agent);
  turn.agentCount++;
  linkAgentToTaskTool(s, turn, agent);
  return [{ op: "add_agent", agent }];
}
function completeAgent(s, agentId, opts) {
  const agent = findAgent(s, agentId);
  if (!agent) return [];
  agent.status = "done";
  if (agent.timestamp) {
    agent.duration = (Date.now() - agent.timestamp) / 1e3;
  }
  if (opts?.stopText) agent.stopText = opts.stopText;
  if (opts?.stopThinking) agent.stopThinking = opts.stopThinking;
  if (opts?.transcriptPath) agent.transcriptPath = opts.transcriptPath;
  return [{
    op: "complete_agent",
    agentId,
    stopText: opts?.stopText,
    stopThinking: opts?.stopThinking,
    duration: agent.duration ?? void 0,
    transcriptPath: opts?.transcriptPath
  }];
}
function findAgent(s, agentId) {
  const loc = s.agentIndex[agentId];
  if (!loc) return void 0;
  const turn = getTurnNode(s, loc.turnNumber);
  if (!turn) return void 0;
  return turn.agents[loc.agentIndex];
}
function linkAgentToTaskTool(s, turn, agent) {
  for (let si = turn.steps.length - 1; si >= 0; si--) {
    const step = turn.steps[si];
    for (let ti = step.tools.length - 1; ti >= 0; ti--) {
      const tool = step.tools[ti];
      if (tool.name === "Task" && tool.input?.subagent_type === agent.type && !tool.agentId) {
        tool.agentId = agent.agentId;
        agent.taskToolUseId = tool.toolUseId;
        return;
      }
    }
  }
}
function trackFile(s, toolName, toolInput) {
  if (!toolInput) return [];
  let path;
  let op;
  switch (toolName) {
    case "Read":
      path = toolInput.file_path;
      op = "read";
      break;
    case "Edit":
      path = toolInput.file_path;
      op = "edit";
      break;
    case "Write":
      path = toolInput.file_path;
      op = "write";
      break;
    default:
      return [];
  }
  if (!path || !op) return [];
  const entry = s.files[path];
  if (entry) {
    if (!entry.ops.includes(op)) entry.ops.push(op);
    entry.lastTouched = Date.now();
  } else {
    s.files[path] = { ops: [op], lastTouched: Date.now() };
  }
  return [{ op: "track_file", path, fileOp: op }];
}
function trackTask(s, eventType, toolName, toolInput, toolUseId, toolResponse) {
  if (eventType === "PreToolUse" && toolName === "TaskCreate") {
    const task = {
      taskId: `_pending_${toolUseId}`,
      subject: toolInput?.subject ?? null,
      description: toolInput?.description ?? null,
      activeForm: toolInput?.activeForm ?? null,
      status: "pending",
      turn: s.currentTurn
    };
    s.tasks[task.taskId] = task;
    const turn = currentTurnNode(s);
    if (turn) turn.tasks.push(task);
    return [{ op: "update_task", taskId: task.taskId, task }];
  }
  if (eventType === "PreToolUse" && toolName === "TaskUpdate") {
    const taskId = toolInput?.taskId;
    if (!taskId) return [];
    const task = s.tasks[taskId];
    if (!task) return [];
    if (toolInput?.status) task.status = toolInput.status;
    if (toolInput?.subject) task.subject = toolInput.subject;
    if (toolInput?.description) task.description = toolInput.description;
    if (toolInput?.activeForm) task.activeForm = toolInput.activeForm;
    return [{ op: "update_task", taskId, task }];
  }
  if (eventType === "PostToolUse" && toolName === "TaskCreate") {
    const tempKey = `_pending_${toolUseId}`;
    const task = s.tasks[tempKey];
    const resp = toolResponse;
    const taskData = resp?.task;
    const realId = resp?.taskId || taskData?.id;
    if (task && realId) {
      task.taskId = realId;
      s.tasks[realId] = task;
      delete s.tasks[tempKey];
      return [{ op: "update_task", taskId: realId, task }];
    }
  }
  if (eventType === "PostToolUse" && toolName === "TaskList") {
    const resp = toolResponse;
    const taskList = resp?.tasks;
    if (!taskList || !Array.isArray(taskList)) return [];
    const patches = [];
    for (const t of taskList) {
      const id = t.id;
      if (!id) continue;
      const existing = s.tasks[id];
      const task = {
        taskId: id,
        subject: t.subject ?? null,
        description: t.description ?? null,
        activeForm: t.activeForm ?? existing?.activeForm ?? null,
        status: t.status ?? "pending",
        turn: existing?.turn ?? 0
      };
      s.tasks[id] = task;
      patches.push({ op: "update_task", taskId: id, task });
    }
    return patches;
  }
  return [];
}
var prevTokenUsage = /* @__PURE__ */ new WeakMap();
function updateTurnTokens(s, usage) {
  const prev = prevTokenUsage.get(s) ?? { totalIn: 0, totalOut: 0 };
  const newIn = (usage.input_tokens ?? 0) + (usage.cache_read_input_tokens ?? 0) + (usage.cache_creation_input_tokens ?? 0);
  const newOut = usage.output_tokens ?? 0;
  const deltaIn = Math.max(0, newIn - prev.totalIn);
  const deltaOut = Math.max(0, newOut - prev.totalOut);
  return setTurnTokens(s, deltaIn, deltaOut);
}
function finalizeTurnTokens(s, usage) {
  const patches = updateTurnTokens(s, usage);
  const newIn = (usage.input_tokens ?? 0) + (usage.cache_read_input_tokens ?? 0) + (usage.cache_creation_input_tokens ?? 0);
  const newOut = usage.output_tokens ?? 0;
  prevTokenUsage.set(s, { totalIn: newIn, totalOut: newOut });
  return patches;
}

// src/handlers/event-handler.ts
function shortModelName(modelId) {
  if (modelId.includes("opus")) return "opus";
  if (modelId.includes("sonnet")) return "sonnet";
  if (modelId.includes("haiku")) return "haiku";
  return modelId;
}
var RE_SYSTEM_REMINDER = /<system-reminder>[\s\S]*?<\/system-reminder>/g;
var RE_COMMAND_NAME_BLOCK = /<command-name>[\s\S]*?<\/command-name>/g;
var RE_COMMAND_ARGS_BLOCK = /<command-args>[\s\S]*?<\/command-args>/g;
var RE_COMMAND_NAME = /<command-name>([^<]+)<\/command-name>/;
var RE_COMMAND_ARGS = /<command-args>([^<]*)<\/command-args>/;
var RE_TASK_NOTIFICATION = /<task-notification>[\s\S]*?<\/task-notification>/g;
var RE_LOCAL_COMMAND_CAVEAT = /<local-command-caveat>[\s\S]*?<\/local-command-caveat>/g;
var RE_COMMAND_MESSAGE = /<command-message>[\s\S]*?<\/command-message>/g;
var RE_LOCAL_COMMAND_STDOUT = /<local-command-stdout>[\s\S]*?<\/local-command-stdout>/g;
function stripSystemTags(raw) {
  if (!raw) return null;
  const text = raw.replace(RE_SYSTEM_REMINDER, "").replace(RE_COMMAND_NAME_BLOCK, "").replace(RE_COMMAND_ARGS_BLOCK, "").replace(RE_TASK_NOTIFICATION, "").replace(RE_LOCAL_COMMAND_CAVEAT, "").replace(RE_COMMAND_MESSAGE, "").replace(RE_LOCAL_COMMAND_STDOUT, "").trim();
  return text.length > 0 ? text : null;
}
function stripSystemXml(raw) {
  return stripSystemTags(raw);
}
function extractSlashCommand(raw) {
  if (!raw) return null;
  const nameMatch = raw.match(RE_COMMAND_NAME);
  if (!nameMatch) return null;
  const cmdName = nameMatch[1];
  const argsMatch = raw.match(RE_COMMAND_ARGS);
  const cmdArgs = argsMatch?.[1];
  if (cmdArgs && cmdArgs.trim().length > 0) {
    return `/${cmdName} ${cmdArgs}`;
  }
  return `/${cmdName}`;
}
function extractAskAnswer(toolResponse) {
  if (!toolResponse || typeof toolResponse !== "object") return "";
  const resp = toolResponse;
  if (typeof resp.result === "string") return resp.result;
  if (typeof resp.answer === "string") return resp.answer;
  if (typeof resp.content === "string") return resp.content;
  return JSON.stringify(resp);
}
var lookupDisplayName = (cwd, sessionId) => Effect_exports.gen(function* () {
  const fs = yield* Effect_exports.service(Fs);
  const encoded = cwd.replace(/\//g, "-");
  const indexPath = join(homedir(), ".claude", "projects", encoded, "sessions-index.json");
  const exists = yield* fs.exists(indexPath);
  if (!exists) return null;
  return yield* pipe(
    fs.readFile(indexPath),
    Effect_exports.map((raw) => {
      const data = JSON.parse(raw);
      if (!data?.entries || !Array.isArray(data.entries)) return null;
      const entry = data.entries.find((e) => e.sessionId === sessionId);
      return entry?.summary || null;
    }),
    Effect_exports.catch(() => Effect_exports.succeed(null))
  );
});
var ensureSession = (store, sessionId, cwd, tmuxSession, source) => {
  let session = store.get(sessionId);
  if (!session) {
    session = createSession(sessionId, cwd);
    if (tmuxSession) session.tmuxSession = tmuxSession;
    if (source) session.source = source;
    store.set(sessionId, session);
  }
  return session;
};
var handleSessionStart = (ctx) => Effect_exports.gen(function* () {
  const store = yield* Effect_exports.service(SessionStore);
  const patches = [];
  const existing = store.get(ctx.sessionId);
  const isPi = ctx.data.source === "pi";
  if (existing && !isPi) {
    patches.push(...resetSession(existing));
  }
  const s = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session, ctx.data.source);
  const displayName = s.displayName ? void 0 : (yield* lookupDisplayName(ctx.cwd, ctx.sessionId)) ?? void 0;
  patches.push(...updateMeta(s, {
    pid: ctx.pid ?? void 0,
    slug: ctx.data.slug ?? void 0,
    displayName,
    branch: ctx.data.branch ?? void 0,
    tmuxSession: ctx.data.tmux_session ?? void 0
  }));
  const modelId = ctx.data.model;
  if (modelId && typeof modelId === "string" && modelId.length > 0) {
    s.modelName = shortModelName(modelId);
    patches.push({ op: "set_meta", modelName: s.modelName });
  }
  return patches;
});
var handleSessionEnd = (ctx) => Effect_exports.gen(function* () {
  const store = yield* Effect_exports.service(SessionStore);
  const session = store.get(ctx.sessionId);
  if (!session) return [];
  return sessionEnd(session);
});
var handleUserPromptSubmit = (ctx) => Effect_exports.gen(function* () {
  const store = yield* Effect_exports.service(SessionStore);
  const patches = [];
  const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
  const rawPrompt = ctx.data.prompt;
  const promptText = stripSystemXml(rawPrompt);
  const displayText = promptText || extractSlashCommand(rawPrompt);
  if (displayText) {
    const lastTurn = session.turns[session.turns.length - 1];
    const isDuplicate = lastTurn?.prompt?.text === displayText && lastTurn.prompt.submitted != null && Date.now() - lastTurn.prompt.submitted < 500;
    if (!isDuplicate) {
      patches.push(
        ...addPrompt(session, {
          type: "user",
          text: displayText,
          submitted: Date.now(),
          elapsed: null,
          toolUseId: null,
          answer: null
        })
      );
      if (!session.displayName) {
        const name = displayText.length <= 60 ? displayText : displayText.slice(0, 57).replace(/\s+\S*$/, "") + "...";
        patches.push(...updateMeta(session, { displayName: name }));
      }
    }
  }
  patches.push(...setClaudeStatus(session, "responding"));
  return patches;
});
var handleStop = (ctx) => Effect_exports.gen(function* () {
  const store = yield* Effect_exports.service(SessionStore);
  const inbox = yield* Effect_exports.service(Inbox);
  const session = store.get(ctx.sessionId);
  if (!session) return [];
  const patches = [];
  patches.push(...setClaudeStatus(session, "idle"));
  patches.push(
    ...finalizeLastPrompt(
      session,
      stripSystemTags(ctx.data.stop_text) ?? void 0,
      stripSystemTags(ctx.data.stop_thinking) ?? void 0
    )
  );
  const tokenUsage = ctx.data.token_usage;
  if (tokenUsage) {
    patches.push(...setTokenUsage(session, tokenUsage));
    patches.push(...finalizeTurnTokens(session, tokenUsage));
  }
  const turn = session.currentTurn;
  const stopText = ctx.data.stop_text;
  const snippet = stopText ? stopText.replace(/[\n\r\t]+/g, " ").substring(0, 80) : "idle";
  inbox.add("idle", ctx.sessionId, session.project, session.slug || ctx.sessionId.substring(0, 8), snippet, { turn, snippet });
  return patches;
});
var handleSubagentStart = (ctx) => Effect_exports.gen(function* () {
  const store = yield* Effect_exports.service(SessionStore);
  const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
  return addAgent(session, {
    agentId: ctx.data.agent_id || "unknown",
    type: ctx.data.agent_type || "unknown",
    status: "running",
    steps: [],
    toolCount: 0,
    stopText: null,
    stopThinking: null,
    duration: null,
    timestamp: Date.now(),
    transcriptPath: ctx.data.agent_transcript_path ?? null,
    taskToolUseId: null
  });
});
var handleSubagentStop = (ctx) => Effect_exports.gen(function* () {
  const store = yield* Effect_exports.service(SessionStore);
  const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
  const agentId = ctx.data.agent_id;
  if (!agentId) return [];
  return completeAgent(session, agentId, {
    stopText: stripSystemTags(ctx.data.agent_stop_text) ?? void 0,
    stopThinking: stripSystemTags(ctx.data.agent_stop_thinking) ?? void 0,
    transcriptPath: ctx.data.agent_transcript_path
  });
});
var handlePreToolUse = (ctx) => Effect_exports.gen(function* () {
  const store = yield* Effect_exports.service(SessionStore);
  const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
  const parentAgentId = ctx.data.parent_agent_id ?? null;
  const toolName = ctx.data.tool_name || "unknown";
  const toolUseId = ctx.data.tool_use_id || `unknown_${Date.now()}`;
  const tool = {
    toolUseId,
    name: toolName,
    input: ctx.data.tool_input || {},
    status: "running",
    result: null,
    timestamp: Date.now(),
    duration: null,
    turn: session.currentTurn,
    assistantText: stripSystemTags(ctx.data.assistant_text) ?? null,
    assistantThinking: stripSystemTags(ctx.data.assistant_thinking) ?? null,
    postText: null,
    postThinking: null,
    parentAgentId,
    ambiguous: false,
    candidateAgentIds: null,
    agentId: null
  };
  const patches = [];
  patches.push(
    ...addTool(session, tool, parentAgentId, ctx.data.candidate_agent_ids ?? null)
  );
  patches.push(
    ...setPermissionMode(session, ctx.data.permission_mode ?? null)
  );
  patches.push(...setClaudeStatus(session, "responding"));
  if (!parentAgentId) {
    const modelId = ctx.data.model;
    if (modelId && typeof modelId === "string" && modelId.length > 0) {
      session.modelName = shortModelName(modelId);
    }
  }
  patches.push(...trackFile(session, toolName, ctx.data.tool_input));
  patches.push(
    ...trackTask(session, "PreToolUse", toolName, ctx.data.tool_input, toolUseId)
  );
  if (toolName === "AskUserQuestion") {
    const input = ctx.data.tool_input;
    const questions = input?.questions;
    const firstQ = questions?.[0];
    const qText = firstQ?.question;
    if (qText) {
      patches.push(
        ...addPrompt(session, {
          type: "question",
          text: qText,
          submitted: Date.now(),
          elapsed: null,
          toolUseId,
          answer: null
        })
      );
    }
  }
  return patches;
});
var handlePostToolUse = (ctx) => Effect_exports.gen(function* () {
  const store = yield* Effect_exports.service(SessionStore);
  const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
  const toolUseId = ctx.data.tool_use_id;
  const postText = stripSystemTags(ctx.data.post_tool_text) ?? void 0;
  const postThink = stripSystemTags(ctx.data.post_tool_thinking) ?? void 0;
  const toolResponse = ctx.data.tool_response;
  const patches = [];
  if (toolUseId) {
    patches.push(
      ...completeTool(session, toolUseId, toolResponse, "done", postText, postThink)
    );
  }
  const toolName = ctx.data.tool_name || "";
  patches.push(...trackFile(session, toolName, ctx.data.tool_input));
  patches.push(
    ...trackTask(session, "PostToolUse", toolName, ctx.data.tool_input, toolUseId || "", toolResponse)
  );
  if (toolName === "AskUserQuestion" && toolUseId) {
    const answer = extractAskAnswer(toolResponse);
    patches.push(...updatePromptAnswer(session, toolUseId, answer));
  }
  if (toolName === "ExitPlanMode") {
    const input = ctx.data.tool_input;
    const resp = toolResponse;
    const planContent = input?.plan;
    const filePath = resp?.filePath;
    const allowedPrompts = input?.allowedPrompts;
    if (planContent) {
      patches.push(
        ...setPlan(session, {
          content: planContent,
          filePath: filePath ?? null,
          allowedPrompts: allowedPrompts ?? []
        })
      );
    }
    patches.push(
      ...addPrompt(session, {
        type: "phase-boundary",
        text: "[Plan approved]",
        submitted: Date.now(),
        elapsed: null,
        toolUseId: null,
        answer: null
      })
    );
  }
  const tokenUsage = ctx.data.token_usage;
  if (tokenUsage) {
    patches.push(...updateTurnTokens(session, tokenUsage));
  }
  return patches;
});
var handlePostToolUseFailure = (ctx) => Effect_exports.gen(function* () {
  const store = yield* Effect_exports.service(SessionStore);
  const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
  const toolUseId = ctx.data.tool_use_id;
  const errorMsg = ctx.data.error || "Unknown error";
  const postText = stripSystemTags(ctx.data.post_tool_text) ?? void 0;
  const postThink = stripSystemTags(ctx.data.post_tool_thinking) ?? void 0;
  const patches = [];
  if (toolUseId) {
    patches.push(
      ...completeTool(session, toolUseId, `[ERROR] ${errorMsg}`, "error", postText, postThink)
    );
  }
  const toolName = ctx.data.tool_name || "";
  patches.push(...trackFile(session, toolName, ctx.data.tool_input));
  patches.push(
    ...trackTask(session, "PostToolUseFailure", toolName, ctx.data.tool_input, toolUseId || "", errorMsg)
  );
  return patches;
});
var handlePermissionRequest = (ctx) => Effect_exports.gen(function* () {
  const store = yield* Effect_exports.service(SessionStore);
  const inbox = yield* Effect_exports.service(Inbox);
  const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
  const toolName = ctx.data.tool_name || "unknown";
  const summary = toolName;
  const inboxType = toolName === "ExitPlanMode" ? "plan-review" : "permission";
  const patches = [];
  patches.push(...setClaudeStatus(session, "idle"));
  if (ctx.hookSocket) {
    inbox.add(
      inboxType,
      ctx.sessionId,
      session.project,
      session.slug || ctx.sessionId.substring(0, 8),
      summary,
      ctx.data,
      ctx.hookSocket
    );
  }
  return patches;
});
var handleAskUserQuestionIntercept = (ctx) => Effect_exports.gen(function* () {
  const store = yield* Effect_exports.service(SessionStore);
  const inbox = yield* Effect_exports.service(Inbox);
  const session = ensureSession(store, ctx.sessionId, ctx.cwd, ctx.data.tmux_session);
  const toolName = ctx.data.tool_name || "AskUserQuestion";
  const input = ctx.data.tool_input;
  const questions = input?.questions;
  const firstQ = questions?.[0];
  const questionText = firstQ?.question || toolName;
  const summary = questionText.substring(0, 80);
  const patches = [];
  patches.push(...setClaudeStatus(session, "idle"));
  if (ctx.hookSocket) {
    inbox.add(
      "question",
      ctx.sessionId,
      session.project,
      session.slug || ctx.sessionId.substring(0, 8),
      summary,
      ctx.data,
      ctx.hookSocket
    );
  }
  return patches;
});
var dispatch = {
  SessionStart: handleSessionStart,
  SessionEnd: handleSessionEnd,
  UserPromptSubmit: handleUserPromptSubmit,
  Stop: handleStop,
  SubagentStart: handleSubagentStart,
  SubagentStop: handleSubagentStop,
  PreToolUse: handlePreToolUse,
  PostToolUse: handlePostToolUse,
  PostToolUseFailure: handlePostToolUseFailure,
  Notification: () => Effect_exports.succeed([]),
  PermissionRequest: handlePermissionRequest,
  AskUserQuestionIntercept: handleAskUserQuestionIntercept
};
var postStopEvents = /* @__PURE__ */ new Set([
  "PreToolUse",
  "PostToolUse",
  "PostToolUseFailure",
  "SubagentStart",
  "SubagentStop"
]);
function handleEvent(eventName, sessionId, cwd, data, pid, hookSocket) {
  return Effect_exports.gen(function* () {
    const store = yield* Effect_exports.service(SessionStore);
    if (postStopEvents.has(eventName)) {
      const session = store.get(sessionId);
      if (session && session.claudeStatus === "idle") {
      }
    }
    const preamblePatches = [];
    const existing = store.get(sessionId);
    if (existing) {
      if (existing.status === "ended" && eventName !== "SessionEnd" && eventName !== "Notification") {
        existing.status = "active";
        preamblePatches.push({ op: "set_status", status: "active" });
      }
      const freshDisplayName = yield* lookupDisplayName(cwd, sessionId);
      const displayName = freshDisplayName && freshDisplayName !== existing.displayName ? freshDisplayName : void 0;
      preamblePatches.push(...updateMeta(existing, {
        pid: pid ?? void 0,
        slug: data.slug ?? void 0,
        displayName,
        branch: data.branch ?? void 0,
        tmuxSession: data.tmux_session ?? void 0
      }));
    }
    const handler = dispatch[eventName];
    const eventPatches = handler ? yield* handler({ sessionId, cwd, data, pid, hookSocket }) : [];
    return [...preamblePatches, ...eventPatches];
  });
}

// src/services/config.ts
import { homedir as homedir2 } from "os";
import { join as join2 } from "path";
var ServerConfig = ServiceMap_exports.Service("ServerConfig");
var ServerConfigLive = Layer_exports.effect(
  ServerConfig,
  Effect_exports.sync(() => {
    const home = process.env.HOME || homedir2();
    const stateDir = join2(home, ".local", "state");
    const args2 = process.argv.slice(2);
    const piIndex = args2.indexOf("--pi");
    const piEnabled = piIndex >= 0;
    let piCwd;
    let piThinkingLevel;
    for (let i = 0; i < args2.length; i++) {
      if (args2[i] === "--pi-cwd" && i + 1 < args2.length) {
        piCwd = args2[i + 1];
        i++;
      } else if (args2[i] === "--pi-thinking" && i + 1 < args2.length) {
        piThinkingLevel = args2[i + 1];
        i++;
      }
    }
    return {
      hookSocketPath: process.env.GRAVITY_HOOK_SOCK ?? join2(stateDir, "gravity-hooks.sock"),
      terminalSocketPath: process.env.GRAVITY_TERMINAL_SOCK ?? join2(stateDir, "gravity-terminal.sock"),
      pidFilePath: process.env.GRAVITY_PID_FILE ?? join2(stateDir, "gravity-server.pid"),
      logPath: process.env.GRAVITY_LOG_PATH || "/tmp/gravity-server.log",
      logMaxSize: parseInt(process.env.GRAVITY_LOG_MAX_SIZE || "2097152", 10),
      piEnabled,
      piCwd,
      piThinkingLevel
    };
  })
);

// src/services/terminal.ts
var MAX_QUEUED_MESSAGES = 200;
var Terminal = ServiceMap_exports.Service("Terminal");
function makeTerminal(logFn) {
  let connections = [];
  const doLog = logFn ?? (() => {
  });
  const writeToConnection = (conn, json) => {
    if (conn.socket.destroyed || !conn.socket.writable) return;
    if (conn.draining) {
      conn.writeQueue.push(json);
      enforceQueueLimit(conn);
      return;
    }
    try {
      const flushed = conn.socket.write(json);
      if (!flushed) {
        conn.draining = true;
      }
    } catch (err) {
      doLog(`Terminal write error: ${err.message}`, "error");
    }
  };
  const flushQueue = (conn) => {
    while (conn.writeQueue.length > 0) {
      if (conn.socket.destroyed || !conn.socket.writable) {
        conn.writeQueue.length = 0;
        return;
      }
      const json = conn.writeQueue.shift();
      try {
        const flushed = conn.socket.write(json);
        if (!flushed) {
          conn.draining = true;
          return;
        }
      } catch (err) {
        doLog(`Terminal flush error: ${err.message}`, "error");
        conn.writeQueue.length = 0;
        return;
      }
    }
  };
  const enforceQueueLimit = (conn) => {
    if (conn.writeQueue.length > MAX_QUEUED_MESSAGES) {
      doLog(`Terminal write queue exceeded ${MAX_QUEUED_MESSAGES} \u2014 disconnecting stuck client`, "warn");
      conn.writeQueue.length = 0;
      conn.socket.destroy();
    }
  };
  return {
    addConnection: (socket) => {
      const conn = {
        socket,
        subscribedSessions: /* @__PURE__ */ new Set(),
        capabilities: /* @__PURE__ */ new Set(),
        writeQueue: [],
        draining: false
      };
      connections.push(conn);
      socket.on("close", () => {
        connections = connections.filter((c) => c !== conn);
      });
      socket.on("error", (err) => {
        doLog(`Terminal connection error: ${err.message}`, "error");
        socket.destroy();
      });
      socket.on("drain", () => {
        conn.draining = false;
        flushQueue(conn);
      });
      return conn;
    },
    broadcast: (message) => {
      const json = JSON.stringify(message) + "\n";
      for (const conn of [...connections]) {
        writeToConnection(conn, json);
      }
    },
    sendTo: (conn, message) => {
      writeToConnection(conn, JSON.stringify(message) + "\n");
    },
    sendToSubscribers: (sessionId, message) => {
      const json = JSON.stringify(message) + "\n";
      for (const conn of [...connections]) {
        if (conn.subscribedSessions.has(sessionId)) {
          writeToConnection(conn, json);
        }
      }
    },
    unsubscribeAll: (sessionId) => {
      for (const conn of connections) {
        conn.subscribedSessions.delete(sessionId);
      }
    },
    hasCapableTerminal: (capability) => connections.some((c) => c.capabilities.has(capability)),
    connectionCount: () => connections.length,
    // Pull mode: send lightweight signal (no payload)
    signalChanged: (what, sessionId, seq) => {
      const json = JSON.stringify({
        type: "state-changed",
        what,
        ...sessionId ? { sessionId } : {},
        seq: seq ?? 0
      }) + "\n";
      for (const conn of [...connections]) {
        writeToConnection(conn, json);
      }
    },
    signalChangedTo: (conn, what, sessionId, seq) => {
      const json = JSON.stringify({
        type: "state-changed",
        what,
        ...sessionId ? { sessionId } : {},
        seq: seq ?? 0
      }) + "\n";
      writeToConnection(conn, json);
    }
  };
}
var TerminalLive = Layer_exports.succeed(Terminal, makeTerminal());

// src/pi-driver/spawn.ts
import { spawn } from "child_process";
import { appendFileSync as appendFileSync2, mkdirSync as mkdirSync2 } from "fs";
import { homedir as homedir3 } from "os";
import { join as join3 } from "path";

// src/pi-driver/protocol.ts
function parseJsonLine(line) {
  try {
    const parsed = JSON.parse(line);
    if (parsed && typeof parsed === "object" && "type" in parsed) {
      if (parsed.type === "response" && typeof parsed.command === "string") {
        return { kind: "response", response: parsed };
      }
      return { kind: "event", event: parsed };
    }
    return { kind: "event", event: { type: "unknown", ...parsed } };
  } catch {
    return { kind: "event", event: { type: "unknown", raw: line } };
  }
}
var requestCounter = 0;
function nextRequestId() {
  return `gr-${process.pid}-${++requestCounter}`;
}
var PiProtocol = class _PiProtocol {
  buffer = "";
  onEvent;
  onStderr;
  commandWriter = null;
  /** In-flight RPC requests awaiting a matching response (keyed by request id). */
  pendingRequests = /* @__PURE__ */ new Map();
  constructor(options) {
    this.onEvent = options.onEvent;
    this.onStderr = options.onStderr ?? ((line) => {
      process.stderr.write(`[pi] ${line}
`);
    });
  }
  /**
   * Feed a text chunk from pi's stdout into the parser.
   * Dispatches events to onEvent and responses to outstanding request
   * promises (matched by id when present).
   */
  feed(data) {
    this.buffer += data;
    let newlineIdx;
    while ((newlineIdx = this.buffer.indexOf("\n")) !== -1) {
      const line = this.buffer.substring(0, newlineIdx).trim();
      this.buffer = this.buffer.substring(newlineIdx + 1);
      if (line.length === 0) continue;
      const parsed = parseJsonLine(line);
      if (parsed.kind === "response") {
        this.dispatchResponse(parsed.response);
      } else {
        this.onEvent({ event: parsed.event, raw: line });
      }
    }
  }
  /** Route a response to its waiter, or drop it if unmatched. */
  dispatchResponse(response) {
    const id = response.id;
    if (!id) {
      return;
    }
    const pending = this.pendingRequests.get(id);
    if (!pending) return;
    this.pendingRequests.delete(id);
    clearTimeout(pending.timer);
    pending.resolve(response);
  }
  /**
   * Send an RPC command with request/response correlation. Returns a promise
   * resolving to pi's response. Times out after `timeoutMs` (default 10s).
   *
   * Use for commands where the caller cares about the response: get_state,
   * get_session_stats, set_model, etc. For fire-and-forget commands use
   * `sendCommand`.
   */
  request(command, timeoutMs = 1e4) {
    if (!this.commandWriter) {
      return Promise.reject(new Error("pi command writer not connected"));
    }
    const id = nextRequestId();
    const line = _PiProtocol.formatCommand(command, id);
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.pendingRequests.delete(id);
        reject(new Error(`pi RPC ${command.type} timed out after ${timeoutMs}ms`));
      }, timeoutMs);
      this.pendingRequests.set(id, { resolve, reject, timer });
      this.commandWriter(line);
    });
  }
  /**
   * Feed a text chunk from pi's stderr.
   * Passes through onStderr callback.
   */
  feedStderr(data) {
    const lines = data.split("\n");
    for (const line of lines) {
      if (line.trim().length > 0) {
        this.onStderr(line);
      }
    }
  }
  /**
   * Flush any remaining buffered content (should be called when stdin closes).
   * Returns the remaining buffer if any.
   */
  flush() {
    const remaining = this.buffer;
    this.buffer = "";
    return remaining;
  }
  /**
   * Set the command writer (called by spawn.ts to connect to subprocess stdin).
   */
  setCommandWriter(writer) {
    this.commandWriter = writer;
  }
  /**
   * Send a command to pi's stdin.
   */
  sendCommand(command) {
    if (this.commandWriter) {
      this.commandWriter(_PiProtocol.formatCommand(command));
    }
  }
  /**
   * Format any PiCommand to a JSONL string. If `id` is provided, it is
   * attached for request/response correlation (pi echoes it on the
   * matching `{type:"response", id}` line).
   */
  static formatCommand(cmd, id) {
    const withId = (obj) => id ? { ...obj, id } : obj;
    switch (cmd.type) {
      case "prompt": {
        const base = { type: "prompt", message: cmd.message };
        if (cmd.images && cmd.images.length > 0) base.images = cmd.images;
        return JSON.stringify(withId(base)) + "\n";
      }
      case "steer":
        return JSON.stringify(withId({ type: "steer", message: cmd.message })) + "\n";
      case "abort":
        return JSON.stringify(withId({ type: "abort" })) + "\n";
      case "set_thinking_level":
        return JSON.stringify(withId({ type: "set_thinking_level", level: cmd.level })) + "\n";
      case "set_model":
        return JSON.stringify(withId({ type: "set_model", provider: cmd.provider, modelId: cmd.modelId })) + "\n";
      case "get_session_stats":
        return JSON.stringify(withId({ type: "get_session_stats" })) + "\n";
      case "get_state":
        return JSON.stringify(withId({ type: "get_state" })) + "\n";
      case "switch_session":
        return JSON.stringify(withId({ type: "switch_session", sessionPath: cmd.sessionPath })) + "\n";
      case "compact": {
        const body = { type: "compact" };
        if (cmd.customInstructions) body.customInstructions = cmd.customInstructions;
        return JSON.stringify(withId(body)) + "\n";
      }
      case "new_session": {
        const body = { type: "new_session" };
        if (cmd.parentSession) body.parentSession = cmd.parentSession;
        return JSON.stringify(withId(body)) + "\n";
      }
      case "extension_ui_response": {
        const body = { type: "extension_ui_response", id: cmd.id };
        if (cmd.value !== void 0) body.value = cmd.value;
        if (cmd.confirmed !== void 0) body.confirmed = cmd.confirmed;
        if (cmd.cancelled !== void 0) body.cancelled = cmd.cancelled;
        return JSON.stringify(body) + "\n";
      }
    }
  }
  /**
   * Format a prompt command for pi's stdin. Pi expects { message, images? } —
   * field name is `message`, not `text` (verified against pi 0.74).
   */
  static formatPrompt(text, images) {
    const cmd = { type: "prompt", message: text };
    if (images && images.length > 0) {
      cmd.images = images;
    }
    return JSON.stringify(cmd) + "\n";
  }
  /**
   * Format a steer command for pi's stdin.
   */
  static formatSteer(text) {
    return JSON.stringify({ type: "steer", message: text }) + "\n";
  }
  /**
   * Format an abort command for pi's stdin.
   */
  static formatAbort() {
    return JSON.stringify({ type: "abort" }) + "\n";
  }
  /**
   * Format a set_thinking_level command for pi's stdin.
   */
  static formatThinkingLevel(level) {
    return JSON.stringify({ type: "set_thinking_level", level }) + "\n";
  }
  /**
   * Format a set_model command for pi's stdin.
   * Pi expects { type: "set_model", provider, modelId } (verified against
   * pi 0.74 RPC docs).
   */
  static formatSetModel(provider, modelId) {
    return JSON.stringify({ type: "set_model", provider, modelId }) + "\n";
  }
};

// src/pi-driver/spawn.ts
var RAW_LOG = process.env.GRAVITY_PI_RAW_LOG;
var PI_BINARY = process.env.PI_BINARY_PATH ?? "pi";
var DEFAULT_THINKING_LEVEL = "medium";
var DEFAULT_PI_SESSION_DIR = join3(homedir3(), ".local", "state", "gravity-pi-sessions");
function spawnPiSync(options = {}) {
  const cwd = options.cwd ?? process.cwd();
  const thinkingLevel = options.thinkingLevel ?? DEFAULT_THINKING_LEVEL;
  const sessionDir = options.sessionDir ?? DEFAULT_PI_SESSION_DIR;
  try {
    mkdirSync2(sessionDir, { recursive: true });
  } catch (err) {
    process.stderr.write(`[pi-adapter] could not create session dir ${sessionDir}: ${err.message}
`);
  }
  const args2 = [
    "--mode",
    "rpc",
    "--session-dir",
    sessionDir,
    "--thinking",
    thinkingLevel
  ];
  if (options.resumeSession) {
    args2.push("--session", options.resumeSession);
  }
  const env = { ...process.env };
  if (options.model) env["PI_MODEL"] = options.model;
  if (options.provider) env["PI_PROVIDER"] = options.provider;
  const child = spawn(PI_BINARY, args2, {
    cwd,
    env,
    stdio: ["pipe", "pipe", "pipe"]
  });
  let stopped = false;
  let onPiEvent = null;
  const proto = new PiProtocol({
    onEvent: (evt) => {
      if (onPiEvent) {
        onPiEvent(evt);
      }
    },
    onStderr: (line) => {
      process.stderr.write(`[pi] ${line}
`);
    }
  });
  child.stdout?.on("data", (chunk) => {
    const s = chunk.toString();
    if (RAW_LOG) {
      try {
        appendFileSync2(RAW_LOG, s);
      } catch {
      }
    }
    proto.feed(s);
  });
  child.stderr?.on("data", (chunk) => {
    proto.feedStderr(chunk.toString());
  });
  child.on("exit", (code, signal) => {
    if (!stopped) {
      stopped = true;
      const msg = code !== null ? `pi subprocess exited with code ${code}` : signal ? `pi subprocess killed by signal ${signal}` : "pi subprocess exited";
      process.stderr.write(`[pi] ${msg}
`);
    }
  });
  child.on("error", (err) => {
    if (!stopped) {
      stopped = true;
      process.stderr.write(`[pi] subprocess error: ${err.message}
`);
    }
  });
  proto.setCommandWriter((line) => {
    if (child.stdin && !child.stdin.destroyed) {
      child.stdin.write(line);
    }
  });
  const driver = {
    prompt: (text, images) => {
      return new Promise((resolve, reject) => {
        if (stopped) {
          reject(new Error("pi subprocess already stopped"));
          return;
        }
        if (child.stdin && !child.stdin.destroyed) {
          const line = PiProtocol.formatPrompt(text, images);
          child.stdin.write(line, (err) => {
            if (err) reject(err);
            else resolve();
          });
        } else {
          reject(new Error("pi stdin unavailable"));
        }
      });
    },
    steer: (text) => {
      if (stopped || !child.stdin || child.stdin.destroyed) return;
      child.stdin.write(PiProtocol.formatSteer(text));
    },
    abort: () => {
      if (stopped || !child.stdin || child.stdin.destroyed) return;
      child.stdin.write(PiProtocol.formatAbort());
    },
    setThinkingLevel: (level) => {
      if (stopped || !child.stdin || child.stdin.destroyed) return;
      child.stdin.write(PiProtocol.formatThinkingLevel(level));
    },
    setModel: (provider, modelId) => {
      if (stopped || !child.stdin || child.stdin.destroyed) return;
      child.stdin.write(PiProtocol.formatSetModel(provider, modelId));
    },
    getSessionStats: async () => {
      if (stopped) throw new Error("pi subprocess already stopped");
      const response = await proto.request({ type: "get_session_stats" });
      if (!response.success) {
        throw new Error(`pi get_session_stats failed: ${response.error ?? "unknown error"}`);
      }
      return response.data ?? {};
    },
    getState: async () => {
      if (stopped) throw new Error("pi subprocess already stopped");
      const response = await proto.request({ type: "get_state" });
      if (!response.success) {
        throw new Error(`pi get_state failed: ${response.error ?? "unknown error"}`);
      }
      return response.data ?? {};
    },
    switchSession: async (sessionPath) => {
      if (stopped) throw new Error("pi subprocess already stopped");
      const response = await proto.request({ type: "switch_session", sessionPath });
      if (!response.success) {
        throw new Error(`pi switch_session failed: ${response.error ?? "unknown error"}`);
      }
      const data = response.data ?? {};
      return data.cancelled !== true;
    },
    sendExtensionUIResponse: (payload) => {
      if (stopped || !child.stdin || child.stdin.destroyed) return;
      proto.sendCommand({ type: "extension_ui_response", ...payload });
    },
    compact: async (customInstructions) => {
      if (stopped) throw new Error("pi subprocess already stopped");
      const response = await proto.request(
        customInstructions ? { type: "compact", customInstructions } : { type: "compact" },
        6e4
        // compaction can take a while (extra LLM call)
      );
      if (!response.success) {
        throw new Error(`pi compact failed: ${response.error ?? "unknown error"}`);
      }
      const data = response.data ?? {};
      return data;
    },
    newSession: async (parentSession) => {
      if (stopped) throw new Error("pi subprocess already stopped");
      const response = await proto.request(
        parentSession ? { type: "new_session", parentSession } : { type: "new_session" }
      );
      if (!response.success) {
        throw new Error(`pi new_session failed: ${response.error ?? "unknown error"}`);
      }
      const data = response.data ?? {};
      return data.cancelled !== true;
    },
    stop: async () => {
      if (stopped) return;
      stopped = true;
      if (!child.killed) {
        child.kill("SIGTERM");
      }
      if (child.stdin && !child.stdin.destroyed) {
        child.stdin.end();
      }
    },
    setEventHandler: (h) => {
      onPiEvent = h;
    }
  };
  return { driver, process: child };
}

// src/pi-driver/turn-accumulator.ts
function createAccState(sessionId, cwd, effortLevel = "medium") {
  return {
    sessionId,
    cwd,
    modelName: null,
    effortLevel,
    pendingAssistantText: "",
    pendingAssistantThinking: "",
    pendingPostText: "",
    pendingPostThinking: "",
    currentToolUseId: null,
    currentToolName: null,
    currentToolInput: null,
    currentToolStartTime: null,
    turns: [],
    currentTurn: -1,
    inTurn: false
  };
}
function flushPendingAssistantContext(state) {
  const assistantText = state.pendingAssistantText.trim() || void 0;
  const assistantThinking = state.pendingAssistantThinking.trim() || void 0;
  state.pendingAssistantText = "";
  state.pendingAssistantThinking = "";
  return { assistantText, assistantThinking };
}
function flushPendingPostContext(state) {
  const postText = state.pendingPostText.trim() || void 0;
  const postThinking = state.pendingPostThinking.trim() || void 0;
  state.pendingPostText = "";
  state.pendingPostThinking = "";
  return { postText, postThinking };
}
function accTurnStart(state, turnId) {
  state.inTurn = true;
  state.currentTurn++;
  const turn = {
    turnNumber: state.currentTurn + 1,
    startedAt: Date.now(),
    endedAt: null,
    tools: [],
    stepIndex: 0
  };
  state.turns.push(turn);
  flushPendingAssistantContext(state);
  flushPendingPostContext(state);
  return state;
}
function accTurnEnd(state, turnId) {
  if (!state.inTurn) return state;
  state.inTurn = false;
  const turn = state.turns[state.turns.length - 1];
  if (turn) {
    turn.endedAt = Date.now();
  }
  return state;
}
function accToolStart(state, toolCallId, toolName, toolInput) {
  state.currentToolUseId = toolCallId;
  state.currentToolName = toolName;
  state.currentToolInput = toolInput;
  state.currentToolStartTime = Date.now();
  flushPendingAssistantContext(state);
  return state;
}
function accToolEnd(state, toolCallId, toolName, toolResult, error) {
  const { postText, postThinking } = flushPendingPostContext(state);
  if (state.currentToolUseId !== toolCallId) {
    return [];
  }
  const toolUseId = state.currentToolUseId;
  const toolInput = state.currentToolInput ?? {};
  const { assistantText, assistantThinking } = flushPendingAssistantContext(state);
  const hookData = {
    tool_name: toolName,
    tool_use_id: toolUseId,
    tool_input: toolInput,
    assistant_text: assistantText,
    assistant_thinking: assistantThinking,
    post_tool_text: postText,
    post_tool_thinking: postThinking,
    cwd: state.cwd,
    ...error ? { error } : {}
  };
  const results = [
    { hookEvent: "PreToolUse", hookData, sessionId: state.sessionId },
    { hookEvent: error ? "PostToolUseFailure" : "PostToolUse", hookData, sessionId: state.sessionId }
  ];
  const turn = state.turns[state.turns.length - 1];
  if (turn) {
    const tool = {
      toolUseId,
      toolName,
      toolInput,
      assistantText: assistantText ?? void 0,
      assistantThinking: assistantThinking ?? void 0,
      startTime: state.currentToolStartTime ?? Date.now(),
      endTime: Date.now(),
      result: toolResult,
      error: error ?? null,
      postText: postText ?? void 0,
      postThinking: postThinking ?? void 0
    };
    turn.tools.push(tool);
  }
  state.currentToolUseId = null;
  state.currentToolName = null;
  state.currentToolInput = null;
  state.currentToolStartTime = null;
  return results;
}
function accTextDelta(state, delta) {
  state.pendingAssistantText += delta;
  return state;
}
function accThinkingDelta(state, delta) {
  state.pendingAssistantThinking += delta;
  return state;
}
function accModelSelect(state, model, provider) {
  state.modelName = model;
  return state;
}
function accAgentStart(state) {
  return [{
    hookEvent: "SessionStart",
    hookData: {
      session_id: state.sessionId,
      cwd: state.cwd,
      source: "pi",
      model: state.modelName ?? void 0,
      effort_level: state.effortLevel
    },
    sessionId: state.sessionId
  }];
}
function accUserPromptMessage(state, promptText) {
  if (!promptText) return [];
  return [{
    hookEvent: "UserPromptSubmit",
    hookData: {
      prompt: promptText,
      cwd: state.cwd
    },
    sessionId: state.sessionId
  }];
}
function accAgentEnd(state, resultType, usage, error) {
  const { assistantText, assistantThinking } = flushPendingAssistantContext(state);
  const hookData = {
    stop_text: assistantText,
    stop_thinking: assistantThinking,
    cwd: state.cwd,
    ...usage ? { token_usage: usage } : {}
  };
  return {
    hookEvent: "Stop",
    hookData,
    sessionId: state.sessionId
  };
}

// src/pi-driver/hook-translator.ts
var stamp = (state, r) => ({ ...r, sessionId: state.sessionId });
function translatePiEvent(event, state) {
  switch (event.type) {
    case "agent_start": {
      const events = accAgentStart(state);
      return { kind: "emit", results: events.map((r) => stamp(state, r)) };
    }
    case "agent_end": {
      const e = event;
      const usage = e.result?.usage ? {
        input_tokens: e.result.usage.input_tokens,
        output_tokens: e.result.usage.output_tokens,
        cache_read_input_tokens: e.result.usage.cache_read_input_tokens ?? 0,
        cache_creation_input_tokens: e.result.usage.cache_creation_input_tokens ?? 0
      } : void 0;
      const stop = accAgentEnd(
        state,
        e.result?.type ?? "success",
        usage,
        e.result?.error
      );
      return { kind: "emit", results: [stamp(state, stop)] };
    }
    case "turn_start": {
      const e = event;
      accTurnStart(state, e.turn_id);
      return { kind: "noop" };
    }
    case "turn_end": {
      const e = event;
      accTurnEnd(state, e.turn_id);
      return { kind: "noop" };
    }
    case "tool_execution_start": {
      const e = event;
      const id = e.toolCallId ?? e.tool_call_id ?? "";
      const name = e.toolName ?? e.tool_name ?? "";
      const input = e.args ?? e.tool_input ?? {};
      accToolStart(state, id, name, input);
      return { kind: "noop" };
    }
    case "tool_execution_end": {
      const e = event;
      const id = e.toolCallId ?? e.tool_call_id ?? "";
      const name = e.toolName ?? e.tool_name ?? "";
      const toolResult = e.result ?? e.tool_result;
      const errorMsg = e.isError === true ? e.error ?? "tool execution failed" : e.error;
      const results = accToolEnd(state, id, name, toolResult, errorMsg);
      if (results.length === 0) return { kind: "noop" };
      return { kind: "emit", results };
    }
    // Pi emits streaming partial-result updates between start and end.
    // No event for us to emit; just ignore.
    case "tool_execution_update":
      return { kind: "noop" };
    // Pi 0.74 emits message_start / message_end as full snapshot events.
    // For user-role messages, this is where the prompt text lives — extract
    // it once (on message_start) and emit UserPromptSubmit. Assistant-role
    // messages are streamed via message_update and surfaced elsewhere.
    case "message_start": {
      const e = event;
      const msg = e.message;
      if (msg?.role === "user" && Array.isArray(msg.content)) {
        const text = msg.content.filter((c) => c?.type === "text" && typeof c.text === "string").map((c) => c.text).join("");
        const results = accUserPromptMessage(state, text);
        if (results.length > 0) {
          return { kind: "emit", results: results.map((r) => stamp(state, r)) };
        }
      }
      return { kind: "noop" };
    }
    case "message_end":
      return { kind: "noop" };
    case "message_update": {
      const e = event;
      const update2 = e.assistantMessageEvent ?? e.message_update;
      if (!update2) return { kind: "noop" };
      if (update2.type === "text_delta" && update2.delta) {
        accTextDelta(state, update2.delta);
      } else if (update2.type === "thinking_delta" && update2.delta) {
        accThinkingDelta(state, update2.delta);
      }
      return { kind: "noop" };
    }
    // Pi emits flat text_delta / thinking_delta events too (in some modes),
    // not always wrapped in message_update. Handle both.
    case "text_delta": {
      const e = event;
      if (e.delta) accTextDelta(state, e.delta);
      return { kind: "noop" };
    }
    case "thinking_delta": {
      const e = event;
      if (e.delta) accThinkingDelta(state, e.delta);
      return { kind: "noop" };
    }
    case "model_select": {
      const e = event;
      accModelSelect(state, e.model, e.provider);
      const hookData = {
        model: e.model,
        cwd: state.cwd
      };
      return {
        kind: "emit",
        results: [{ hookEvent: "SessionStart", hookData, sessionId: state.sessionId }]
      };
    }
    case "error": {
      const e = event;
      process.stderr.write(`[pi-adapter] error event: ${e.error}
`);
      return { kind: "noop" };
    }
    default:
      return { kind: "noop" };
  }
}
function createSessionEnd(state) {
  return {
    hookEvent: "SessionEnd",
    hookData: {
      session_id: state.sessionId,
      cwd: state.cwd
    },
    sessionId: state.sessionId
  };
}

// src/pi-driver/types.ts
var EFFORT_FROM_THINKING = {
  off: "low",
  minimal: "low",
  low: "medium",
  medium: "medium",
  high: "high",
  xhigh: "high"
  // cap at "high"
};

// src/pi-driver/session.ts
function thinkingToEffort(level) {
  return EFFORT_FROM_THINKING[level] ?? "medium";
}
function normalizeThinkingLevel(level) {
  const valid = ["off", "minimal", "low", "medium", "high", "xhigh"];
  if (valid.includes(level)) {
    return level;
  }
  return "medium";
}
function createSessionMetadata(sessionId, cwd, thinkingLevel = "medium") {
  return {
    sessionId,
    cwd,
    modelName: null,
    effortLevel: thinkingToEffort(thinkingLevel),
    thinkingLevel,
    startedAt: Date.now()
  };
}
function updateModel(metadata, model, provider) {
  return {
    ...metadata,
    modelName: model
  };
}
function updateThinkingLevel(metadata, level) {
  return {
    ...metadata,
    thinkingLevel: level,
    effortLevel: thinkingToEffort(level)
  };
}

// src/pi-driver/mod.ts
function generateSessionId() {
  const now = Date.now().toString(36);
  const random2 = Math.random().toString(36).substring(2, 10);
  return `pi-${now}-${random2}`;
}
function startPiDriver(options) {
  const sessionId = options.sessionId ?? generateSessionId();
  const cwd = options.cwd ?? process.cwd();
  const thinkingLevel = options.thinkingLevel ?? "medium";
  let state = createAccState(sessionId, cwd, thinkingToEffort(thinkingLevel));
  let metadata = createSessionMetadata(sessionId, cwd, thinkingLevel);
  const onLifecycle = options.onLifecycle ?? (() => {
  });
  const onExtensionUIRequest = options.onExtensionUIRequest ?? (() => {
  });
  const { driver, process: childProcess } = spawnPiSync({
    cwd,
    thinkingLevel,
    model: options.model,
    provider: options.provider,
    piBinaryPath: options.piBinaryPath,
    sessionDir: options.sessionDir,
    resumeSession: options.resumeSession
  });
  let lifecycleStarted = false;
  driver.setEventHandler((evt) => {
    if (evt.event.type === "extension_ui_request") {
      onExtensionUIRequest(evt.event);
      return;
    }
    const result3 = translatePiEvent(evt.event, state);
    if (result3.kind === "emit") {
      for (const r of result3.results) {
        options.onTranslation(r);
      }
    }
    switch (evt.event.type) {
      case "model_select":
        metadata = updateModel(metadata, evt.event.model, evt.event.provider);
        state.modelName = evt.event.model;
        break;
      case "agent_start":
        if (!lifecycleStarted) {
          lifecycleStarted = true;
          onLifecycle("start", metadata);
        }
        break;
    }
  });
  childProcess.on("error", (err) => {
    onLifecycle("error", metadata);
    process.stderr.write(`[pi-adapter] process error: ${err.message}
`);
  });
  childProcess.on("exit", (code, signal) => {
    const sessionEnd2 = createSessionEnd(state);
    options.onTranslation(sessionEnd2);
    onLifecycle("stop", metadata);
  });
  return {
    prompt: (text, images) => {
      return driver.prompt(text, images);
    },
    steer: (text) => {
      driver.steer(text);
    },
    abort: () => {
      driver.abort();
    },
    setThinkingLevel: (level) => {
      driver.setThinkingLevel(level);
      metadata = updateThinkingLevel(metadata, level);
      state.effortLevel = thinkingToEffort(level);
    },
    setEffortLevel: (level) => {
      const normalized = normalizeThinkingLevel(level);
      driver.setThinkingLevel(normalized);
      metadata = updateThinkingLevel(metadata, normalized);
      state.effortLevel = thinkingToEffort(normalized);
    },
    setModel: (provider, modelId) => {
      driver.setModel(provider, modelId);
      metadata = updateModel(metadata, modelId, provider);
      state.modelName = modelId;
    },
    getSessionStats: () => driver.getSessionStats(),
    getState: () => driver.getState(),
    switchSession: (sessionPath) => driver.switchSession(sessionPath),
    sendExtensionUIResponse: (payload) => driver.sendExtensionUIResponse(payload),
    compact: (customInstructions) => driver.compact(customInstructions),
    newSession: (parentSession) => driver.newSession(parentSession),
    stop: () => {
      return driver.stop();
    },
    getMetadata: () => {
      return metadata;
    }
  };
}

// src/gravity-server.ts
var CAPABILITY_WAIT_MS = 1e4;
var CAPABILITY_POLL_MS = 500;
var PURGE_DELAY_MS = 2 * 60 * 1e3;
var HEALTH_CHECK_INTERVAL_MS = 3e4;
var STALENESS_THRESHOLD_MS = 5 * 60 * 1e3;
var HINT_RECENCY_GUARD_MS = 3e4;
var HOOKS_SILENCE_WARN_MS = 9e4;
var HOOKS_SILENCE_REARM_MS = 6e5;
var BIDIRECTIONAL_EVENTS = /* @__PURE__ */ new Set(["PermissionRequest", "AskUserQuestionIntercept"]);
var OVERVIEW_EVENTS = /* @__PURE__ */ new Set(["SessionStart", "SessionEnd", "UserPromptSubmit", "Stop", "PermissionRequest", "AskUserQuestionIntercept"]);
var PULL_MODE = process.env.GRAVITY_PUSH_MODE !== "true";
function logMsg(message, level = "info") {
  const ts = (/* @__PURE__ */ new Date()).toISOString();
  try {
    process.stderr.write(`[${ts}] [${level}] ${message}
`);
  } catch {
  }
}
var program = Effect_exports.gen(function* () {
  const config = yield* Effect_exports.service(ServerConfig);
  const fs = yield* Effect_exports.service(Fs);
  const store = yield* Effect_exports.service(SessionStore);
  const inbox = yield* Effect_exports.service(Inbox);
  const terminals = yield* Effect_exports.service(Terminal);
  const eventLayer = Layer_exports.mergeAll(
    Layer_exports.succeed(SessionStore, store),
    Layer_exports.succeed(Inbox, inbox),
    FsLive
  );
  const runEvent = (eventName, sessionId, cwd, data, pid, hookSocket) => Effect_exports.runSync(Effect_exports.provide(
    handleEvent(eventName, sessionId, cwd, data, pid, hookSocket),
    eventLayer
  ));
  const handlePiTranslation = (outerSessionId, result3) => {
    const sessionId = outerSessionId;
    const cwd = result3.hookData.cwd || config.piCwd || process.cwd();
    logMsg(`Pi driver event: ${result3.hookEvent} session=${sessionId}`);
    const patches = runEvent(result3.hookEvent, sessionId, cwd, result3.hookData, null);
    if (patches.length > 0) {
      if (PULL_MODE) {
        const stored = store.appendPatches(sessionId, patches);
        const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
        terminals.signalChanged("session", sessionId, seq);
      } else {
        terminals.broadcast({ type: "session.update", sessionId, patches });
      }
    }
    const hasStatusPatch = patches.some(
      (p) => p.op === "set_claude_status" || p.op === "set_status"
    );
    if (OVERVIEW_EVENTS.has(result3.hookEvent) || hasStatusPatch) {
      if (PULL_MODE) {
        terminals.signalChanged("overview");
      } else {
        terminals.broadcast({
          type: "overview.snapshot",
          projects: store.getProjectSummaries()
        });
      }
    }
    const session = store.get(sessionId);
    if (session && session.status === "ended") {
      schedulePurge(sessionId);
    } else if (session && session.status === "active") {
      store.cancelPurge(sessionId);
    }
  };
  const handlePiExtensionUIRequest = (sessionId, request3) => {
    const session = store.get(sessionId);
    if (!session) {
      logMsg(`pi extension_ui_request for unknown session ${sessionId}`, "warn");
      return;
    }
    const method = request3.method;
    const sendResponse = (payload) => {
      if (!activePiDriver) {
        logMsg(`pi extension_ui_response: no active driver to send to (request id=${request3.id})`, "warn");
        return;
      }
      activePiDriver.sendExtensionUIResponse({ id: request3.id, ...payload });
    };
    switch (method) {
      case "confirm": {
        const summary = request3.title ?? request3.message ?? "Pi confirm";
        const item = inbox.add(
          "permission",
          sessionId,
          session.project,
          session.slug || sessionId.substring(0, 8),
          summary,
          {
            // Shape the action.permission handler expects: tool_name +
            // tool_input. We use a synthetic tool name so the UI label
            // makes sense.
            tool_name: "pi:confirm",
            tool_input: { title: request3.title, message: request3.message },
            // Pi-specific fields for renderers that want them:
            pi_ui: { method, id: request3.id, options: ["Allow", "Block"] }
          }
        );
        pendingPiUIResponses.set(item.id, { piRequestId: request3.id, method });
        terminals.broadcast({ type: "inbox.added", item });
        break;
      }
      case "select": {
        const options = request3.options ?? [];
        const summary = request3.title ?? "Pi select";
        const item = inbox.add(
          "question",
          sessionId,
          session.project,
          session.slug || sessionId.substring(0, 8),
          summary,
          {
            tool_name: "pi:select",
            tool_input: {
              // action.question's handler reads tool_input.questions[] —
              // synthesize a single question entry with the option list.
              questions: [{ question: summary, options }]
            },
            pi_ui: { method, id: request3.id, options }
          }
        );
        pendingPiUIResponses.set(item.id, { piRequestId: request3.id, method });
        terminals.broadcast({ type: "inbox.added", item });
        break;
      }
      case "input":
      case "editor": {
        logMsg(`pi extension_ui_request ${method} not yet supported \u2014 cancelling`, "warn");
        sendResponse({ cancelled: true });
        break;
      }
      // Fire-and-forget: pi doesn't expect a response. Log for visibility;
      // wiring these into the UI (status bar, transient notifications,
      // window title) is a follow-up.
      case "notify":
      case "setStatus":
      case "setWidget":
      case "setTitle":
      case "set_editor_text":
        logMsg(`pi UI notice ${method}: ${JSON.stringify({
          text: request3.text,
          message: request3.message,
          statusText: request3.statusText,
          title: request3.title
        })}`);
        break;
      default:
        logMsg(`pi extension_ui_request unknown method ${method} \u2014 cancelling`, "warn");
        sendResponse({ cancelled: true });
    }
  };
  const applyPiSessionStats = (sessionId, stats) => {
    const session = store.get(sessionId);
    if (!session) return;
    const patches = [];
    if (typeof stats.cost === "number") {
      patches.push(...setCost(session, stats.cost));
    }
    if (stats.contextUsage) {
      patches.push(...setContextUsage(session, {
        tokens: stats.contextUsage.tokens,
        contextWindow: stats.contextUsage.contextWindow,
        percent: stats.contextUsage.percent
      }));
    }
    if (patches.length === 0) return;
    if (PULL_MODE) {
      const stored = store.appendPatches(sessionId, patches);
      const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
      terminals.signalChanged("session", sessionId, seq);
    } else {
      terminals.broadcast({ type: "session.update", sessionId, patches });
    }
  };
  const waitForCapableTerminal = (capability, timeoutMs) => new Promise((resolve) => {
    if (terminals.hasCapableTerminal(capability)) {
      resolve(true);
      return;
    }
    const start = Date.now();
    const interval = setInterval(() => {
      if (terminals.hasCapableTerminal(capability)) {
        clearInterval(interval);
        resolve(true);
      } else if (Date.now() - start >= timeoutMs) {
        clearInterval(interval);
        resolve(false);
      }
    }, CAPABILITY_POLL_MS);
  });
  const schedulePurge = (sessionId) => {
    store.schedulePurge(sessionId, PURGE_DELAY_MS, () => {
      store.delete(sessionId);
      inbox.removeForSession(sessionId);
      terminals.broadcast({ type: "session.removed", sessionId });
      terminals.unsubscribeAll(sessionId);
      terminals.broadcast({
        type: "overview.snapshot",
        projects: store.getProjectSummaries()
      });
      logMsg(`Purged ended session ${sessionId}`);
    });
  };
  let activePiDriver = null;
  const pendingPiUIResponses = /* @__PURE__ */ new Map();
  const startPiSession = (options) => {
    if (activePiDriver) {
      logMsg(`Pi session already running \u2014 use pi.abort first`, "warn");
      return null;
    }
    const sessionId = generateSessionId2();
    const cwd = options.cwd ?? process.cwd();
    const thinking = options.thinkingLevel ?? "medium";
    logMsg(`Starting pi session ${sessionId} (cwd=${cwd}, thinking=${thinking})`);
    terminals.broadcast({
      type: "pi.session",
      sessionId,
      event: "started",
      cwd
    });
    handlePiTranslation(sessionId, {
      hookEvent: "SessionStart",
      hookData: {
        session_id: sessionId,
        cwd,
        source: "pi",
        effort_level: thinking
      }
    });
    const driver = startPiDriver({
      cwd: options.cwd ?? process.cwd(),
      thinkingLevel: options.thinkingLevel ?? "medium",
      sessionId,
      resumeSession: options.resumeSession,
      onExtensionUIRequest: (request3) => {
        handlePiExtensionUIRequest(sessionId, request3);
      },
      onTranslation: (result3) => {
        handlePiTranslation(sessionId, result3);
        if (result3.hookEvent === "Stop") {
          setImmediate(() => {
            if (!activePiDriver) return;
            activePiDriver.getSessionStats().then(
              (stats) => applyPiSessionStats(sessionId, stats),
              (err) => logMsg(`pi get_session_stats failed: ${err.message}`, "warn")
            );
          });
        }
      },
      onLifecycle: (event, _metadata) => {
        if (event === "start") {
          logMsg(`Pi session started: ${sessionId}`);
        } else if (event === "stop" || event === "error") {
          if (event === "error") logMsg(`Pi session error`, "error");
          else logMsg(`Pi session ended: ${sessionId}`);
          for (const [itemId] of pendingPiUIResponses) {
            inbox.remove(itemId);
            terminals.broadcast({ type: "inbox.removed", itemId });
          }
          pendingPiUIResponses.clear();
          terminals.broadcast({
            type: "pi.session",
            sessionId,
            event: "stopped"
          });
          activePiDriver = null;
        }
      }
    });
    activePiDriver = driver;
    const captureSessionFile = (attempt = 0) => {
      if (!activePiDriver || activePiDriver !== driver) return;
      driver.getState().then((state) => {
        const f = state.sessionFile;
        if (typeof f === "string" && f.length > 0) {
          const session = store.get(sessionId);
          if (session) {
            const patches = updateMeta(session, { piSessionFile: f });
            if (PULL_MODE) {
              const stored = store.appendPatches(sessionId, patches);
              const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
              terminals.signalChanged("session", sessionId, seq);
            } else {
              terminals.broadcast({ type: "session.update", sessionId, patches });
            }
          }
        } else if (attempt < 3) {
          setTimeout(() => captureSessionFile(attempt + 1), 500 * (attempt + 1));
        }
      }, (err) => {
        if (attempt < 3) {
          setTimeout(() => captureSessionFile(attempt + 1), 500 * (attempt + 1));
        } else {
          logMsg(`pi get_state failed after retries: ${err.message}`, "warn");
        }
      });
    };
    setImmediate(() => captureSessionFile());
    return sessionId;
  };
  const piSessionPrompt = (text, images) => {
    if (!activePiDriver) {
      logMsg(`No active pi session`, "warn");
      return;
    }
    activePiDriver.prompt(text, images).catch((err) => {
      logMsg(`pi.prompt error: ${err.message}`, "error");
    });
  };
  const piSessionSteer = (text) => {
    if (!activePiDriver) {
      logMsg(`No active pi session`, "warn");
      return;
    }
    activePiDriver.steer(text);
  };
  const piSessionAbort = () => {
    if (!activePiDriver) {
      logMsg(`No active pi session to abort`, "warn");
      return;
    }
    activePiDriver.abort();
  };
  const piSessionSetThinking = (level) => {
    if (!activePiDriver) {
      logMsg(`No active pi session`, "warn");
      return;
    }
    activePiDriver.setEffortLevel(level);
  };
  const piSessionSetModel = (provider, modelId) => {
    if (!activePiDriver) {
      logMsg(`No active pi session`, "warn");
      return;
    }
    activePiDriver.setModel(provider, modelId);
  };
  const piSessionCompact = (customInstructions) => {
    if (!activePiDriver) {
      logMsg(`No active pi session to compact`, "warn");
      return;
    }
    activePiDriver.compact(customInstructions).then(
      (data) => {
        const summary = data.summary ? ` \u2014 ${data.summary.substring(0, 80)}\u2026` : "";
        logMsg(`pi compact done (tokensBefore=${data.tokensBefore ?? "?"})${summary}`);
      },
      (err) => logMsg(`pi compact failed: ${err.message}`, "error")
    );
  };
  const piSessionNewSession = () => {
    if (!activePiDriver) {
      logMsg(`No active pi session for new_session`, "warn");
      return;
    }
    activePiDriver.newSession().then(
      (ok) => {
        if (!ok) logMsg(`pi new_session cancelled by extension`, "warn");
      },
      (err) => logMsg(`pi new_session failed: ${err.message}`, "error")
    );
  };
  const piSessionResume = (sessionPath) => {
    if (!sessionPath) {
      logMsg(`pi.resume called without sessionPath`, "warn");
      return;
    }
    if (activePiDriver) {
      activePiDriver.switchSession(sessionPath).then(
        (ok) => {
          if (!ok) logMsg(`pi switch_session cancelled by extension`, "warn");
        },
        (err) => logMsg(`pi switch_session failed: ${err.message}`, "error")
      );
    } else {
      const sessionId = startPiSession({ resumeSession: sessionPath });
      if (sessionId) {
        logMsg(`Pi session ${sessionId} spawned resuming ${sessionPath}`);
      }
    }
  };
  const stopPiSession = async () => {
    if (!activePiDriver) {
      return;
    }
    await activePiDriver.stop();
    activePiDriver = null;
  };
  const generateSessionId2 = () => {
    const now = Date.now().toString(36);
    const random2 = Math.random().toString(36).substring(2, 10);
    return `pi-${now}-${random2}`;
  };
  if (config.piEnabled) {
    logMsg(`Pi driver mode enabled (cwd=${config.piCwd ?? process.cwd()}, thinking=${config.piThinkingLevel ?? "medium"})`);
    const sessionId = startPiSession({
      cwd: config.piCwd,
      thinkingLevel: config.piThinkingLevel
    });
    if (sessionId) {
      logMsg(`Auto-started pi session: ${sessionId}`);
    }
  }
  const handleHookMessage = async (msg, socket) => {
    const eventName = msg.event;
    const sessionId = msg.session_id || "unknown";
    const cwd = msg.cwd || "";
    const pid = msg.pid || null;
    const data = msg.data || {};
    const needsResponse = msg.needs_response === true;
    logMsg(`Hook event: ${eventName} session=${sessionId}`);
    hookEventReceived = true;
    if (needsResponse && BIDIRECTIONAL_EVENTS.has(eventName)) {
      if (!terminals.hasCapableTerminal("action.permission")) {
        logMsg(`No capable terminal connected \u2014 waiting up to ${CAPABILITY_WAIT_MS}ms for reconnect`, "warn");
        const arrived = await waitForCapableTerminal("action.permission", CAPABILITY_WAIT_MS);
        if (!arrived) {
          logMsg(`No capable terminal after ${CAPABILITY_WAIT_MS}ms \u2014 rejecting ${eventName}`, "warn");
          try {
            socket.write(JSON.stringify({ reason: "no_capable_terminal" }) + "\n");
            socket.end();
          } catch {
          }
          return;
        }
        logMsg(`Capable terminal connected during wait \u2014 proceeding with ${eventName}`);
      }
    }
    if (!BIDIRECTIONAL_EVENTS.has(eventName)) {
      const staleRemoved = inbox.removeStaleForSession(sessionId);
      for (const item of staleRemoved) {
        logMsg(`Inbox item ${item.id} (${item.type}) auto-removed: superseded by ${eventName}`);
        terminals.broadcast({ type: "inbox.removed", itemId: item.id });
      }
      if (eventName !== "Notification") {
        const forceClosed = inbox.forceCloseStaleForSession(sessionId);
        for (const item of forceClosed) {
          logMsg(`Inbox item ${item.id} (${item.type}) force-closed: superseded by ${eventName}`);
          terminals.broadcast({ type: "inbox.removed", itemId: item.id });
        }
      }
    }
    const patches = runEvent(eventName, sessionId, cwd, data, pid, needsResponse ? socket : void 0);
    if (patches.length > 0) {
      if (PULL_MODE) {
        const stored = store.appendPatches(sessionId, patches);
        const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
        terminals.signalChanged("session", sessionId, seq);
      } else {
        terminals.broadcast({ type: "session.update", sessionId, patches });
      }
    }
    const session = store.get(sessionId);
    if (session && session.status === "ended") {
      schedulePurge(sessionId);
    } else if (session && session.status === "active") {
      store.cancelPurge(sessionId);
    }
    const hasStatusPatch = patches.some(
      (p) => p.op === "set_claude_status" || p.op === "set_status"
    );
    if (OVERVIEW_EVENTS.has(eventName) || hasStatusPatch) {
      if (PULL_MODE) {
        terminals.signalChanged("overview");
      } else {
        terminals.broadcast({
          type: "overview.snapshot",
          projects: store.getProjectSummaries()
        });
      }
    }
    if (eventName === "SessionStart") {
      const session2 = store.get(sessionId);
      if (session2) {
        if (PULL_MODE) {
          terminals.signalChanged("session", sessionId, store.getSessionSeq(sessionId));
        } else {
          terminals.broadcast({ type: "session.snapshot", sessionId, session: session2 });
        }
      }
    }
    if (eventName === "PermissionRequest" || eventName === "AskUserQuestionIntercept") {
      const items = inbox.all();
      if (items.length > 0) {
        const item = items[0];
        logMsg(`Inbox broadcast: type=${item.type} tool_name=${item.data?.tool_name} id=${item.id}`);
        if (PULL_MODE) {
          terminals.signalChanged("inbox");
        } else {
          terminals.broadcast({ type: "inbox.added", item });
        }
      }
    }
  };
  const sendOverview = (conn) => {
    terminals.sendTo(conn, {
      type: "overview.snapshot",
      projects: store.getProjectSummaries()
    });
  };
  const handleTerminalMessage = (conn, msg) => {
    if (!msg) return;
    switch (msg.type) {
      case "hello": {
        const caps = msg.capabilities;
        if (Array.isArray(caps)) {
          conn.capabilities = new Set(caps.filter((c) => typeof c === "string"));
        }
        logMsg(`Terminal hello: capabilities=[${[...conn.capabilities].join(",")}]`);
        break;
      }
      case "request.overview": {
        terminals.sendTo(conn, {
          type: "overview.snapshot",
          projects: store.getProjectSummaries()
        });
        break;
      }
      case "poll": {
        sendOverview(conn);
        const items = inbox.all();
        if (items.length > 0) {
          terminals.sendTo(conn, { type: "inbox-items", items });
        }
        for (const sessionId of conn.subscribedSessions) {
          const session = store.get(sessionId);
          if (session) {
            const patches = store.getPatchesSince(sessionId, 0);
            const seq = store.getSessionSeq(sessionId);
            if (patches.length > 0) {
              terminals.sendTo(conn, {
                type: "session-patches",
                sessionId,
                seq,
                patches: patches.map((p) => p.patch)
              });
            }
          }
        }
        logMsg(`Terminal poll: overview sent, ${inbox.all().length} inbox items, ${conn.subscribedSessions.size} subscribed sessions`);
        break;
      }
      case "request.session": {
        const session = store.get(msg.sessionId);
        conn.subscribedSessions.add(msg.sessionId);
        if (session) {
          terminals.sendTo(conn, { type: "session.snapshot", sessionId: msg.sessionId, session });
        }
        break;
      }
      case "request.resync": {
        terminals.sendTo(conn, {
          type: "overview.snapshot",
          projects: store.getProjectSummaries()
        });
        for (const sessionId of conn.subscribedSessions) {
          const session = store.get(sessionId);
          if (session) {
            terminals.sendTo(conn, { type: "session.snapshot", sessionId, session });
          }
        }
        terminals.sendTo(conn, { type: "inbox.snapshot", items: inbox.all() });
        logMsg(`Terminal resync: ${conn.subscribedSessions.size} sessions`);
        break;
      }
      case "action.permission": {
        const { itemId, decision, message, updatedPermissions } = msg;
        const piUi = pendingPiUIResponses.get(itemId);
        if (piUi) {
          if (!activePiDriver) {
            logMsg(`action.permission for pi UI ${piUi.piRequestId}: no active driver`, "warn");
          } else {
            activePiDriver.sendExtensionUIResponse({
              id: piUi.piRequestId,
              confirmed: decision === "allow"
            });
          }
          pendingPiUIResponses.delete(itemId);
          inbox.remove(itemId);
          terminals.broadcast({ type: "inbox.removed", itemId });
          break;
        }
        Effect_exports.runSync(inbox.respond(itemId, {
          hookSpecificOutput: {
            hookEventName: "PermissionRequest",
            decision: { behavior: decision, message, updatedPermissions }
          }
        }));
        terminals.broadcast({ type: "inbox.removed", itemId });
        break;
      }
      case "action.question": {
        const { itemId, answers } = msg;
        const piUi = pendingPiUIResponses.get(itemId);
        if (piUi) {
          if (!activePiDriver) {
            logMsg(`action.question for pi UI ${piUi.piRequestId}: no active driver`, "warn");
          } else {
            const value = answers[0];
            activePiDriver.sendExtensionUIResponse({
              id: piUi.piRequestId,
              ...value !== void 0 ? { value } : { cancelled: true }
            });
          }
          pendingPiUIResponses.delete(itemId);
          inbox.remove(itemId);
          terminals.broadcast({ type: "inbox.removed", itemId });
          break;
        }
        const pending = inbox.getPending(itemId);
        const toolInput = pending?.inboxItem.data?.tool_input || {};
        const questions = toolInput.questions || [];
        const answersMap = {};
        questions.forEach((q, i) => {
          const qText = q.question || `question_${i}`;
          answersMap[qText] = answers[i] || answers[0] || "";
        });
        Effect_exports.runSync(inbox.respond(itemId, {
          hookSpecificOutput: {
            hookEventName: "PreToolUse",
            permissionDecision: "allow",
            updatedInput: { ...toolInput, answers: answersMap }
          }
        }));
        terminals.broadcast({ type: "inbox.removed", itemId });
        break;
      }
      case "action.plan-review": {
        const { itemId, decision, feedback } = msg;
        let message;
        let normalizedFeedback;
        if (feedback) {
          if (typeof feedback === "string") {
            normalizedFeedback = {
              inlineComments: [],
              claudeMarkers: [],
              diff: null,
              generalComment: feedback
            };
          } else {
            normalizedFeedback = feedback;
          }
        }
        if (normalizedFeedback) {
          const parts = ["# Plan Feedback\n"];
          if (normalizedFeedback.inlineComments?.length > 0) {
            parts.push("## Inline comments");
            normalizedFeedback.inlineComments.forEach((c) => {
              parts.push(`- Line ${c.line} (near "${c.nearText}"): ${c.comment}`);
            });
            parts.push("");
          }
          if (normalizedFeedback.claudeMarkers?.length > 0) {
            parts.push("## @claude markers");
            normalizedFeedback.claudeMarkers.forEach((m) => {
              parts.push(`- Line ${m.line} (near "${m.nearText}"): ${m.text}`);
            });
            parts.push("");
          }
          if (normalizedFeedback.diff) {
            parts.push("## Changes requested");
            parts.push(normalizedFeedback.diff);
            parts.push("");
          }
          if (normalizedFeedback.generalComment) {
            parts.push("## General comment");
            parts.push(normalizedFeedback.generalComment);
          }
          message = parts.join("\n");
        }
        Effect_exports.runSync(inbox.respond(itemId, {
          hookSpecificOutput: {
            hookEventName: "PermissionRequest",
            decision: { behavior: decision, message }
          }
        }));
        terminals.broadcast({ type: "inbox.removed", itemId });
        break;
      }
      case "action.turn-auto-approve": {
        break;
      }
      case "hint.session-dead": {
        const { sessionId } = msg;
        const session = store.get(sessionId);
        if (session && session.status === "active") {
          const age = Date.now() - session.lastEventTime;
          if (age < HINT_RECENCY_GUARD_MS) {
            logMsg(`Terminal hint: session ${sessionId} ignored \u2014 last event ${Math.round(age / 1e3)}s ago (< ${HINT_RECENCY_GUARD_MS / 1e3}s)`, "warn");
            break;
          }
          logMsg(`Terminal hint: session ${sessionId} is dead \u2014 marking ended`);
          const patches = sessionEnd(session);
          if (patches.length > 0) {
            terminals.broadcast({ type: "session.update", sessionId, patches });
          }
          schedulePurge(sessionId);
          terminals.broadcast({
            type: "overview.snapshot",
            projects: store.getProjectSummaries()
          });
        }
        break;
      }
      // ── Pi driver terminal messages ────────────────────────────────
      case "pi.start": {
        const m = msg;
        const sessionId = startPiSession({
          cwd: m.cwd,
          thinkingLevel: m.thinkingLevel
        });
        if (sessionId) {
          logMsg(`Pi session started via terminal: ${sessionId}`);
        } else {
          logMsg(`Pi session start failed \u2014 already running?`, "warn");
          terminals.sendTo(conn, {
            type: "pi.session",
            sessionId: "",
            event: "rejected",
            reason: "Pi session already running. Abort it first (p a)."
          });
        }
        break;
      }
      case "pi.prompt": {
        const m = msg;
        piSessionPrompt(m.text, m.images);
        break;
      }
      case "pi.steer": {
        const m = msg;
        piSessionSteer(m.text);
        break;
      }
      case "pi.abort": {
        piSessionAbort();
        break;
      }
      case "pi.set-thinking": {
        const m = msg;
        piSessionSetThinking(m.level);
        break;
      }
      case "pi.set-model": {
        const m = msg;
        piSessionSetModel(m.provider, m.modelId);
        break;
      }
      case "pi.resume": {
        const m = msg;
        piSessionResume(m.sessionPath);
        break;
      }
      case "pi.compact": {
        const m = msg;
        piSessionCompact(m.customInstructions);
        break;
      }
      case "pi.new-session": {
        piSessionNewSession();
        break;
      }
      case "pi.stop": {
        stopPiSession().catch((err) => logMsg(`pi.stop failed: ${err.message}`, "error"));
        break;
      }
    }
  };
  yield* fs.unlinkIfExists(config.hookSocketPath);
  yield* fs.mkdirp(dirname(config.hookSocketPath));
  const hookServer = createServer((socket) => {
    let buffer = "";
    socket.on("data", (chunk) => {
      buffer += chunk.toString();
      let newlineIdx;
      while ((newlineIdx = buffer.indexOf("\n")) !== -1) {
        const line = buffer.substring(0, newlineIdx).trim();
        buffer = buffer.substring(newlineIdx + 1);
        if (line.length === 0) continue;
        try {
          const msg = JSON.parse(line);
          handleHookMessage(msg, socket).catch(
            (e) => logMsg(`Hook message handler error: ${e}`, "error")
          );
        } catch (e) {
          logMsg(`Hook socket parse error: ${e}`, "error");
        }
      }
    });
    socket.on("error", (err) => {
      logMsg(`Hook socket connection error: ${err.message}`, "error");
    });
    socket.on("close", () => {
      const removed = inbox.removeBySocket(socket);
      for (const item of removed) {
        logMsg(`Inbox item ${item.id} (${item.type}) auto-removed: hook socket closed`);
        terminals.broadcast({ type: "inbox.removed", itemId: item.id });
      }
    });
  });
  hookServer.listen(config.hookSocketPath, () => {
    logMsg(`Hook socket listening on ${config.hookSocketPath}`);
  });
  yield* fs.unlinkIfExists(config.terminalSocketPath);
  yield* fs.mkdirp(dirname(config.terminalSocketPath));
  const terminalServer = createServer((socket) => {
    const conn = terminals.addConnection(socket);
    logMsg(`Terminal connected (total: ${terminals.connectionCount()})`);
    terminals.sendTo(conn, {
      type: "overview.snapshot",
      projects: store.getProjectSummaries()
    });
    let buffer = "";
    socket.on("data", (chunk) => {
      buffer += chunk.toString();
      let newlineIdx;
      while ((newlineIdx = buffer.indexOf("\n")) !== -1) {
        const line = buffer.substring(0, newlineIdx).trim();
        buffer = buffer.substring(newlineIdx + 1);
        if (line.length === 0) continue;
        try {
          const parsed = JSON.parse(line);
          if (typeof parsed === "object" && parsed !== null && isHookMessage(parsed)) {
            logMsg(`Hook event received on terminal socket \u2014 bridge may have wrong socket path (event=${parsed.event}, session=${parsed.session_id})`, "error");
            socket.destroy();
            return;
          }
        } catch {
        }
        const msg = parseTerminalMessage(line);
        if (!msg) {
          logMsg(`Terminal: invalid message: ${line.substring(0, 100)}`, "warn");
          continue;
        }
        handleTerminalMessage(conn, msg);
      }
    });
    socket.on("close", () => {
      logMsg(`Terminal disconnected (total: ${terminals.connectionCount()})`);
    });
    socket.on("error", (err) => {
      logMsg(`Terminal socket error: ${err.message}`, "error");
    });
  });
  terminalServer.listen(config.terminalSocketPath, () => {
    logMsg(`Terminal socket listening on ${config.terminalSocketPath}`);
  });
  const serverStartedAt = Date.now();
  let lastHooksSilenceWarn = 0;
  let hookEventReceived = false;
  const healthCheckInterval = setInterval(() => {
    const now = Date.now();
    for (const session of store.all()) {
      if (session.status !== "active") continue;
      const sessionId = session.sessionId;
      let isDead = false;
      if (session.pid && session.pid > 0) {
        try {
          process.kill(session.pid, 0);
        } catch {
          isDead = true;
          logMsg(`Health check: session ${sessionId} PID ${session.pid} is dead`);
        }
      } else if (now - session.lastEventTime > STALENESS_THRESHOLD_MS) {
        isDead = true;
        logMsg(`Health check: session ${sessionId} stale (no events for ${Math.round((now - session.lastEventTime) / 1e3)}s)`);
      }
      if (isDead) {
        const patches = sessionEnd(session);
        if (patches.length > 0) {
          if (PULL_MODE) {
            const stored = store.appendPatches(sessionId, patches);
            const seq = stored.length > 0 ? stored[stored.length - 1].seq : store.getSessionSeq(sessionId);
            terminals.signalChanged("session", sessionId, seq);
          } else {
            terminals.broadcast({ type: "session.update", sessionId, patches });
          }
        }
        schedulePurge(sessionId);
        if (PULL_MODE) {
          terminals.signalChanged("overview");
        } else {
          terminals.broadcast({
            type: "overview.snapshot",
            projects: store.getProjectSummaries()
          });
        }
      }
    }
    if (!hookEventReceived && store.all().length === 0 && terminals.connectionCount() > 0 && now - serverStartedAt > HOOKS_SILENCE_WARN_MS && now - lastHooksSilenceWarn > HOOKS_SILENCE_REARM_MS) {
      lastHooksSilenceWarn = now;
      const elapsed = Math.round((now - serverStartedAt) / 1e3);
      const text = `No hook events received in ${elapsed}s \u2014 is the emacs-bridge plugin enabled? Check project .claude/settings.json for enabledPlugins overrides.`;
      logMsg(text, "warn");
      terminals.broadcast({ type: "notice", level: "warn", text });
    }
  }, HEALTH_CHECK_INTERVAL_MS);
  yield* fs.mkdirp(dirname(config.pidFilePath));
  yield* fs.writeFile(config.pidFilePath, process.pid.toString());
  logMsg(`gravity-server ready (pid=${process.pid}, pidfile=${config.pidFilePath})`);
  const shutdown = () => {
    logMsg("gravity-server shutting down...");
    clearInterval(healthCheckInterval);
    store.clearAllPurgeTimers();
    hookServer.close();
    terminalServer.close();
    try {
      unlinkSync2(config.hookSocketPath);
    } catch {
    }
    try {
      unlinkSync2(config.terminalSocketPath);
    } catch {
    }
    try {
      unlinkSync2(config.pidFilePath);
    } catch {
    }
  };
  process.on("SIGINT", () => {
    shutdown();
    process.exit(0);
  });
  process.on("SIGTERM", () => {
    shutdown();
    process.exit(0);
  });
});
var pidGuard = Effect_exports.gen(function* () {
  const config = yield* Effect_exports.service(ServerConfig);
  const fs = yield* Effect_exports.service(Fs);
  logMsg("gravity-server starting...");
  const pidExists = yield* fs.exists(config.pidFilePath);
  if (pidExists) {
    const content = yield* fs.readFile(config.pidFilePath).pipe(
      Effect_exports.catch(() => Effect_exports.succeed(""))
    );
    const existingPid = parseInt(content.trim(), 10);
    if (existingPid > 0 && existingPid !== process.pid) {
      try {
        process.kill(existingPid, 0);
        logMsg(`Another gravity-server is running (pid=${existingPid}). Exiting.`, "warn");
        process.exit(0);
      } catch {
        logMsg(`Stale PID file (pid=${existingPid} dead). Taking over.`);
      }
    }
  }
});
var MainLive = Layer_exports.mergeAll(
  ServerConfigLive,
  FsLive,
  SessionStoreLive,
  InboxLive,
  TerminalLive
);
var main = Effect_exports.gen(function* () {
  yield* pidGuard;
  yield* program;
});
Effect_exports.runPromise(Effect_exports.provide(main, MainLive)).catch((e) => {
  logMsg(`Fatal error: ${e}`, "error");
  process.exit(1);
});
