import { ObjId, Handle, Editable, Static, StaticE, Ephemeral, EphemeralE, EntityId } from "./types.js";

export const aversNamespace = Symbol("aversNamespace");

export function endpointUrl(h: Handle, path: string): string {
  return h.config.apiHost + path;
}

export function entityLabel(entity: EntityId): string {
  if (typeof entity === "string") {
    return `Editable(${entity})`;
  } else if (entity instanceof Static) {
    return `Static(${entity.ns.toString()},${entity.key})`;
  } else if (entity instanceof Ephemeral) {
    return `Ephemeral(${entity.ns.toString()},${entity.key})`;
  } else {
    return ""; // XXX: EXHAUSTIVE
  }
}

export function withEditable<T>(h: Handle, objId: ObjId, f: (obj: Editable<T>) => void): void {
  const e = h.editableCache.get(objId);
  if (e) {
    applyEditableChanges(h, e, f);
  }
}

// ----------------------------------------------------------------------------
// applyEditableChanges
//
// Create a copy of the given 'Editable', apply the function on it, and insert
// the new copy into the cache, overwriting the previous object.

export function applyEditableChanges<T>(h: Handle, obj: Editable<T>, f: (obj: Editable<T>) => void): void {
  const copy = { ...obj };
  f(copy);
  h.editableCache.set(obj.objectId, Object.freeze(copy));
}

// ----------------------------------------------------------------------------
// Static

export function lookupStaticE<T>(h: Handle, ns: Symbol, key: string): undefined | StaticE<T> {
  const n = h.staticCache.get(ns);
  if (n) {
    return n.get(key);
  }
}

export function insertStaticE<T>(h: Handle, ns: Symbol, key: string, e: StaticE<T>): void {
  let n = h.staticCache.get(ns);
  if (!n) {
    n = new Map<string, StaticE<T>>();
    h.staticCache.set(ns, n);
  }

  n.set(key, Object.freeze(e));
}

function applyStaticChanges<T>(h: Handle, ns: Symbol, key: string, s: StaticE<T>, f: (s: StaticE<T>) => void): void {
  const copy = { ...s };
  f(copy);
  insertStaticE(h, ns, key, Object.freeze(copy));
}

export function withStaticE<T>(h: Handle, ns: Symbol, key: string, f: (s: StaticE<T>) => void): void {
  const e = lookupStaticE<T>(h, ns, key);
  if (e) {
    applyStaticChanges(h, ns, key, e, f);
  }
}

// ----------------------------------------------------------------------------
// Ephemeral

export function lookupEphemeralE<T>(h: Handle, ns: Symbol, key: string): undefined | EphemeralE<T> {
  const n = h.ephemeralCache.get(ns);
  if (n) {
    return n.get(key);
  }
}

export function insertEphemeralE<T>(h: Handle, ns: Symbol, key: string, e: EphemeralE<T>): void {
  let n = h.ephemeralCache.get(ns);
  if (!n) {
    n = new Map<string, EphemeralE<T>>();
    h.ephemeralCache.set(ns, n);
  }

  n.set(key, Object.freeze(e));
}

function applyEphemeralChanges<T>(
  h: Handle,
  ns: Symbol,
  key: string,
  s: EphemeralE<T>,
  f: (s: EphemeralE<T>) => void
): void {
  const copy = { ...s };
  f(copy);
  insertEphemeralE(h, ns, key, Object.freeze(copy));
}

export function withEphemeralE<T>(h: Handle, ns: Symbol, key: string, f: (s: EphemeralE<T>) => void): void {
  const e = lookupEphemeralE<T>(h, ns, key);
  if (e) {
    applyEphemeralChanges(h, ns, key, e, f);
  }
}
