import { last } from "./shared.js";

const splice = Array.prototype.splice;

// changeListenersSymbol
// -----------------------------------------------------------------------
//
// The symbol under which the change listeners callbacks are attached to
// an object. The value of this property is a Set. This means the
// callbacks must be unique. If you attach the same callback twice to an
// object (ie by using 'attachChangeListener') then it will be called only
// once.

const changeListenersSymbol = Symbol("aversChangeListeners");

// childListenersSymbol
// -----------------------------------------------------------------------
//
// If an object has listeners set up on c of its children, it'll keep
// a map from child to callback in a Map stored under this symbol.

const childListenersSymbol = Symbol("aversChildListeners");

function emitChanges(self: any, changes: Change<any>[]): void {
  const listeners = self[changeListenersSymbol];
  if (listeners) {
    listeners.forEach((fn: any) => {
      fn(changes);
    });
  }
}

function listenTo(self: any, obj: any, callback: ChangeCallback): void {
  let listeners = self[childListenersSymbol];
  if (!listeners) {
    listeners = self[childListenersSymbol] = new Map();
  }

  listeners.set(obj, callback);
  attachChangeListener(obj, callback);
}

function stopListening(self: any, obj: any): void {
  const listeners = self[childListenersSymbol];
  if (listeners) {
    const fn = listeners.get(obj);
    if (fn) {
      detachChangeListener(obj, fn);
      listeners.delete(obj);
    }
  }
}

// Symbol used as the key for the avers property descriptor object. It is
// kept private so only the Avers module has access to the descriptors.

const aversPropertiesSymbol = Symbol("aversProperties");

type PropertyDescriptor<T> =
  | PrimitivePropertyDescriptor<T>
  | ObjectPropertyDescriptor<T>
  | CollectionPropertyDescriptor<T>
  | VariantPropertyDescriptor<T>;

interface PrimitivePropertyDescriptor<T> {
  type: "PrimitivePropertyDescriptor";
  defaultValue: undefined | T;
}

interface ObjectPropertyDescriptor<T> {
  type: "ObjectPropertyDescriptor";
  parser: (json: any, parent: any) => T;
  defaultValue: undefined | T;
}

interface CollectionPropertyDescriptor<T> {
  type: "CollectionPropertyDescriptor";
  parser: (json: any, parent: any) => T;
}

interface VariantPropertyDescriptor<T> {
  type: "VariantPropertyDescriptor";
  parser: (json: any, parent: any) => T;
  typeField: any;
  typeMap: any;
  defaultValue: undefined | T;
}

interface AversProperties {
  [name: string]: PropertyDescriptor<any>;
}

// Return the property descriptors for the given object. Returns undefined
// if the object has no properties defined on it.
function aversProperties(obj: any): AversProperties {
  return Object.getPrototypeOf(obj)[aversPropertiesSymbol];
}

function withId<T>(json: any, obj: T): T {
  if (json.id !== undefined) {
    (<any>obj).id = json.id;
  }

  return obj;
}

function descendInto(obj: any, key: string): any {
  if (Array.isArray(obj)) {
    return (<any>obj).idMap[key];
  } else if (obj === Object(obj) && aversProperties(obj) && aversProperties(obj)[key]) {
    return obj[key];
  }
}

export function resolvePath<T>(obj: any, path: string): T {
  if (path === "") {
    return obj;
  } else {
    return path.split(".").reduce(descendInto, obj);
  }
}

export function clone(x: any): any {
  if (Array.isArray(x)) {
    return mkCollection(x.map(clone));
  } else if (x === Object(x) && aversProperties(x)) {
    return parseJSON(x.constructor, toJSON(x));
  } else {
    return x;
  }
}

function setValueAtPath(root: any, path: string, value: any): any {
  const pathKeys = path.split("."),
    lastKey = pathKeys.pop(),
    obj = resolvePath<any>(root, pathKeys.join("."));

  if (lastKey !== undefined) {
    obj[lastKey] = clone(value);
  }

  return root;
}

function parentPath(path: string): string {
  const pathKeys = path.split(".");
  return pathKeys.slice(0, pathKeys.length - 1).join(".");
}

// Splice operations can currently not be applied to the root. This is
// a restriction which may be lifted in the future.
function applySpliceOperation(root: any, path: string, op: Splice): any {
  const obj = resolvePath<any>(root, path),
    parent = resolvePath<any>(root, parentPath(path)),
    prop = aversProperties(parent)[last(path.split("."))];

  if (prop.type !== "CollectionPropertyDescriptor") {
    return;
  }

  const parser = prop.parser,
    insert = op.insert.map(json => parser(json, parent)),
    args: [number, number, ...any[]] = [op.index, op.remove, ...insert];

  splice.apply(obj, args);

  return root;
}

// applyOperation
// -----------------------------------------------------------------------
//
// Apply an operation to a root object. The operation can come from
// a local change (be sure to convert the change to an 'Operation' first)
// or loaded from the server.

export function applyOperation<T>(root: T, path: string, op: Operation): T {
  switch (op.type) {
    case "set":
      return setValueAtPath(clone(root), path, op.value);
    case "splice":
      return applySpliceOperation(clone(root), path, op);
  }
}

function defineProperty<T>(x: any, name: string, desc: PropertyDescriptor<T>): void {
  const proto = x.prototype,
    aversProps = proto[aversPropertiesSymbol] || Object.create(null);

  aversProps[name] = desc;
  proto[aversPropertiesSymbol] = aversProps;
}

export function declareConstant(x: any): void {
  const proto = x.prototype,
    aversProps = proto[aversPropertiesSymbol] || Object.create(null);

  proto[aversPropertiesSymbol] = aversProps;
}

export function definePrimitive<T>(x: any, name: string, defaultValue: undefined | T) {
  const desc: PrimitivePropertyDescriptor<T> = {
    type: "PrimitivePropertyDescriptor",
    defaultValue
  };

  defineProperty(x, name, desc);
}

export function defineObject<T extends object>(x: any, name: string, klass: any, def?: T) {
  const desc: ObjectPropertyDescriptor<T> = {
    type: "ObjectPropertyDescriptor",
    parser: createObjectParser<T>(klass),
    defaultValue: undefined
  };

  if (def) {
    desc.defaultValue = <any>mk(klass, def);
  }

  defineProperty(x, name, desc);
}

export function defineVariant<T extends object>(
  x: any,
  name: string,
  typeField: string,
  typeMap: { [name: string]: any },
  def?: T
): void {
  // Check that all constructors are valid Avers objects. This is an
  // invariant which we can't express in the type system, but want to
  // ensure it nonetheless.
  //
  // This is something which can be removed from the production builds.

  for (const k in typeMap) {
    const aversProps = typeMap[k].prototype[aversPropertiesSymbol];
    if (aversProps === undefined) {
      throw new Error(`Variant constructor of "${k}" is not an Avers object`);
    }
  }

  const desc: VariantPropertyDescriptor<T> = {
    type: "VariantPropertyDescriptor",
    parser: createVariantParser<T>(name, typeField, typeMap),
    typeField: typeField,
    typeMap: typeMap,
    defaultValue: def === undefined ? undefined : clone(def)
  };

  defineProperty(x, name, desc);
}

export function defineCollection(x: any, name: string, klass: any) {
  const desc: CollectionPropertyDescriptor<any> = {
    type: "CollectionPropertyDescriptor",
    parser: createObjectParser(klass)
  };

  defineProperty(x, name, desc);
}

function createObjectParser<T extends object>(klass: any): (json: any) => T {
  return json => parseJSON<T>(klass, json);
}

function createVariantParser<T extends object>(
  name: string,
  typeField: string,
  typeMap: { [typeField: string]: any }
): (json: any, parent: any) => T {
  return function(json: any, parent: any): T {
    const type = parent[typeField] || parent[name][typeField];
    return parseJSON<T>(typeMap[type], json);
  };
}

function parseValue(desc: PropertyDescriptor<any>, old: any, json: any, parent: any): any {
  switch (desc.type) {
    case "CollectionPropertyDescriptor":
      if (json) {
        if (!old) {
          old = mkCollection([]);
        } else {
          resetCollection(old);
        }

        json.forEach((x: any) => {
          old.push(withId(json, desc.parser(x, parent)));
        });

        return old;
      }
      break;

    case "ObjectPropertyDescriptor":
    case "VariantPropertyDescriptor":
      if (json) {
        if (old) {
          return withId(json, updateObject(old, json));
        } else {
          return withId(json, desc.parser(json, parent));
        }
      }
      break;

    case "PrimitivePropertyDescriptor":
      return json;
  }
}

export function updateObject<T>(x: T, json: any): T {
  const aversProps = aversProperties(x);

  for (const name in aversProps) {
    const desc = aversProps[name];

    if (json[name] != null) {
      (x as any)[name] = parseValue(desc, (x as any)[name], json[name], json);
    }
  }

  return x;
}

export function migrateObject<T>(x: T): T {
  const aversProps = aversProperties(x);

  for (const name in aversProps) {
    const desc = aversProps[name],
      prop = (x as any)[name];

    if (prop == null) {
      if (desc.type === "CollectionPropertyDescriptor") {
        (x as any)[name] = mkCollection([]);
      } else {
        const value = desc.defaultValue;
        if (value != null && value !== prop) {
          migrateObject(value);
          (x as any)[name] = value;
        }
      }
    } else if (desc.type === "ObjectPropertyDescriptor" || desc.type === "VariantPropertyDescriptor") {
      migrateObject(prop);
    } else if (desc.type === "CollectionPropertyDescriptor") {
      prop.forEach(migrateObject);
    }
  }

  return x;
}

const objectProxyHandler = {
  set: (target: any, property: string, value: any): boolean => {
    const oldValue = target[property],
      propertyDescriptor = aversProperties(target)[property];

    target[property] = value;

    if (propertyDescriptor) {
      if (isObservableProperty(propertyDescriptor)) {
        if (oldValue) {
          stopListening(target, oldValue);
        }

        if (value) {
          // FIXME: Is this 'stopListening' needed?
          stopListening(target, value);
          forwardChanges(target, value, property);
        }
      }

      emitChanges(target, [new Change(property, new Operation.Set(target, value, oldValue))]);
    }

    return true;
  },

  deleteProperty: (target: any, property: string): boolean => {
    const oldValue = target[property],
      propertyDescriptor = aversProperties(target)[property];

    if (propertyDescriptor && isObservableProperty(propertyDescriptor) && oldValue) {
      stopListening(target, oldValue);
    }

    emitChanges(target, [new Change(property, new Operation.Set(target, undefined, oldValue))]);

    return true;
  }
};

function createObject<T extends object>(x: new () => T): T {
  return new Proxy(new x(), objectProxyHandler);
}

export function parseJSON<T extends object>(x: new () => T, json: any): T {
  if (<any>x === String || <any>x === Number) {
    return new (<any>x)(json).valueOf();
  } else {
    return withId(json, updateObject(createObject(x), json));
  }
}

export function mk<T extends object>(x: new () => T, json: any): T {
  return migrateObject(parseJSON(x, json));
}

function concatPath(self: string, child: string): string {
  return child === "" ? self : `${self}.${child}`;
}

// Return true if the property can generate change events and thus the
// parent should listen to events.
function isObservableProperty(propertyDescriptor: PropertyDescriptor<any>): boolean {
  const type = propertyDescriptor.type;
  return (
    type === "ObjectPropertyDescriptor" ||
    type === "VariantPropertyDescriptor" ||
    type === "CollectionPropertyDescriptor"
  );
}

export function typeName(typeMap: { [klass: string]: any }, klass: any): string {
  for (const type in typeMap) {
    if (typeMap[type] === klass) {
      return type;
    }
  }

  return `typeName: Unknown klass ${klass}`;
}

function objectJSON(x: any): any {
  const json = Object.create(null),
    aversProps = aversProperties(x);

  for (const name in aversProps) {
    const desc = aversProps[name];

    switch (desc.type) {
      case "PrimitivePropertyDescriptor":
        json[name] = x[name];
        break;

      case "ObjectPropertyDescriptor":
        json[name] = x[name] ? toJSON(x[name]) : null;
        break;

      case "VariantPropertyDescriptor":
        const value = x[name];

        if (value) {
          json[name] = toJSON(value);
          json[desc.typeField] = typeName(desc.typeMap, value.constructor);
        }
        break;

      case "CollectionPropertyDescriptor":
        json[name] = toJSON(x[name]);
        break;
    }
  }

  return json;
}

export function toJSON(x: any): any {
  if (x === Object(x) && aversProperties(x)) {
    return objectJSON(x);
  } else if (Array.isArray(x)) {
    return x.map((item: any) => withId(item, toJSON(item)));
  } else {
    return x;
  }
}

export interface Item {
  id: string;
}

export function itemId<T extends Item>(_collection: Collection<T>, item: T): string {
  // ASSERT: collection.idMap[item.id] === item
  return item.id;
}

export interface Collection<T extends Item> extends Array<T> {
  idMap: { [id: string]: T };
}

function resetCollection<T extends Item>(x: Collection<T>): void {
  x.splice(0, x.length);
  x.idMap = Object.create(null);
}

function mkCollection<T extends Item>(items: T[]): Collection<T> {
  const collection: Collection<T> = <any>[];
  resetCollection(collection);

  if (items.length > 0) {
    const args = (<any>[0, 0]).concat(items);
    splice.apply(collection, args);
  }

  function _splice(start: number, deleteCount: number, ...items: T[]): T[] {
    const deletedItems = collection.slice(start, start + deleteCount);

    splice.call(collection, start, deleteCount, ...items);

    deletedItems.forEach(item => {
      stopListening(collection, item);
      delete collection.idMap[item.id];
    });

    items.forEach(item => {
      if (Object(item) === item) {
        collection.idMap[item.id] = item;

        listenTo(collection, item, changes => {
          const id = itemId(collection, item);
          emitChanges(collection, changes.map(change => embedChange(change, id)));
        });
      }
    });

    emitChanges(collection, [new Change("", new Operation.Splice(collection, start, deletedItems, items))]);

    return deletedItems;
  }

  collection.push = (...items) => {
    _splice(collection.length, 0, ...items);
    return collection.length;
  };

  collection.pop = () => {
    return _splice(collection.length - 1, 1)[0];
  };

  collection.splice = <any>((start: number, deleteCount: number, ...items: T[]): T[] => {
    return _splice(start, deleteCount, ...items);
  });

  collection.shift = () => {
    return _splice(0, 1)[0];
  };

  collection.unshift = (...items) => {
    _splice(0, 0, ...items);
    return collection.length;
  };

  return collection;
}

// lookupItem
// -----------------------------------------------------------------------
//
// Return the item in the collection which has the given id. May return
// undefined if no such item exists.

export function lookupItem<T extends Item>(collection: Collection<T>, id: string): T {
  return collection.idMap[id];
}

// Operation
// -----------------------------------------------------------------------
//
// Definition of a pure JavaScript object which describes a change at
// a particular path. It can be converted directly to JSON.

export type Operation = Set | Splice;

export interface Set {
  type: "set";
  path: string;
  value: any;
}

export interface Splice {
  type: "splice";
  path: string;
  index: number;
  remove: number;
  insert: any[];
}

// Change
// -----------------------------------------------------------------------
//
// A 'Change' is an description of a 'Set' or 'Splice' change which has
// happened at a particular path.

export class Change<T> {
  constructor(public path: string, public record: T) {}
}

export namespace Operation {
  export class Set {
    constructor(public object: any, public value: any, public oldValue: any) {}
  }

  export class Splice {
    constructor(public object: any, public index: number, public remove: any[], public insert: any[]) {}
  }
}

function embedChange<T>(change: Change<T>, key: string): Change<T> {
  return new Change(concatPath(key, change.path), change.record);
}

function forwardChanges(obj: any, prop: string, key: string): void {
  listenTo(obj, prop, changes => {
    emitChanges(obj, changes.map(change => embedChange(change, key)));
  });
}

// changeOperation
// -----------------------------------------------------------------------
//
// Convert a 'Change' to an 'Operation' which is a pure JS object and can
// be directly converted to JSON and sent over network.

export function changeOperation(change: Change<any>): Operation {
  const record = change.record;

  if (record instanceof Operation.Set) {
    return {
      path: change.path,
      type: "set",
      value: toJSON(record.value)
    };
  } else if (record instanceof Operation.Splice) {
    return {
      path: change.path,
      type: "splice",
      index: record.index,
      remove: record.remove.length,
      insert: toJSON(record.insert)
    };
  } else {
    throw new Error(`Unknown change record: ${record}`);
  }
}

export interface ChangeCallback {
  (changes: Change<any>[]): void;
}

// attachChangeListener
// -----------------------------------------------------------------------
//
// Attach a change callback to the object. It will be called each time the
// object or any of its properties change.

export function attachChangeListener(obj: any, fn: ChangeCallback): void {
  const listeners = obj[changeListenersSymbol] || new Set();
  obj[changeListenersSymbol] = listeners;

  listeners.add(fn);
}

// detachChangeListener
// -----------------------------------------------------------------------
//
// Detach a given change listener callback from an object.

export function detachChangeListener(obj: any, fn: ChangeCallback): void {
  const listeners = obj[changeListenersSymbol];
  if (listeners) {
    listeners.delete(fn);
  }
}
