import { immutableClone } from "../shared";
import { Operation, applyOperation, attachChangeListener, detachChangeListener } from "../core";
import { ObjId, Handle, Editable, Static, StaticE, Ephemeral, EphemeralE, Action, Patch, EntityId } from "./types";

export const aversNamespace = Symbol("aversNamespace");

export function endpointUrl(h: Handle, path: string): string {
  return h.config.apiHost + path;
}

export function modifyHandle<T>(h: Handle, { applyF, payload }: Action<T>): void {
  applyF(h, payload);
  startNextGeneration(h);
}

export function startNextGeneration(h: Handle): void {
  h.generationNumber++;

  for (const f of h.generationChangeCallbacks) {
    f();
  }
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
  const obj = h.editableCache.get(objId);
  if (obj) {
    applyEditableChanges(h, obj, f);
  }
}

// ----------------------------------------------------------------------------
// applyEditableChanges
//
// Create a copy of the given 'Editable', apply the function on it, and insert
// the new copy into the cache, overwriting the previous object.

export function applyEditableChanges<T>(h: Handle, obj: Editable<T>, f: (obj: Editable<T>) => void): void {
  h.editableCache.set(obj.objectId, immutableClone<Editable<T>>(Editable, obj, f));
}

// applyPatches
// -----------------------------------------------------------------------------
//
// The given patches MUST have consecutive revisionIds!

export function applyPatches(obj: Editable<unknown>, patches: Patch[]): void {
  if (obj.shadowContent === undefined) {
    // We simply ignore any attempt to apply patches to an Editable which
    // is not resolved.
    //
    // We could store the patches somewhere and attempt to apply them once
    // the Editable is resolved. But that would require careful changes to
    // multiple parts of the code. Given how low the chance is for that to
    // happen and cause problems, we decided to not handle that situation.

    return;
  }

  const applicablePatches = patches.filter(p => p.revisionId > obj.revisionId);
  if (applicablePatches.length === 0 || applicablePatches[0].revisionId !== obj.revisionId + 1) {
    // The first patch is not one that can be applied directly on the
    // Editable. This means there is a gap between the first patche we have
    // and the state of the Editable.
    //
    // We could consider storing the patches somewhere, reload the ones
    // which are missing and then apply them all. But, again, the chance
    // for that to happen is so low that we don't bother.

    return;
  }

  obj.revisionId += applicablePatches.length;
  obj.shadowContent = applicablePatches.reduce((c, patch) => {
    const op = patch.operation;
    return applyOperation(c, op.path, op);
  }, obj.shadowContent);
}

export function initContent(obj: Editable<unknown>): void {
  if (obj.shadowContent === undefined) {
    return;
  }

  if (obj.content) {
    detachChangeListener(obj.content, obj.changeListener);
  }

  obj.content = obj.submittedChanges.concat(obj.localChanges).reduce((c, o) => {
    return applyOperation(c, o.path, o);
  }, obj.shadowContent);

  attachChangeListener(obj.content, obj.changeListener);
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
  insertStaticE(h, ns, key, immutableClone<StaticE<T>>(StaticE, s, f));
}

export function withStaticE<T>(h: Handle, ns: Symbol, key: string, f: (s: StaticE<T>) => void): void {
  const e = lookupStaticE<T>(h, ns, key) || new StaticE<T>();
  applyStaticChanges(h, ns, key, e, f);
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
  insertEphemeralE(h, ns, key, immutableClone<EphemeralE<T>>(EphemeralE, s, f));
}

export function withEphemeralE<T>(h: Handle, ns: Symbol, key: string, f: (s: EphemeralE<T>) => void): void {
  const e = lookupEphemeralE<T>(h, ns, key) || new EphemeralE<T>();
  applyEphemeralChanges(h, ns, key, e, f);
}
