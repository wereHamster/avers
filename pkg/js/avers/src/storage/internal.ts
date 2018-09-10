import { immutableClone } from "../shared";
import { ObjId, Handle, Editable, Static, StaticE, Ephemeral, EphemeralE, Action, mkAction, NetworkRequest } from "./types";

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

export function entityLabel(entity: string | Static<any> | Ephemeral<any>): string {
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

// ----------------------------------------------------------------------------
// Static

function lookupStaticE<T>(h: Handle, ns: Symbol, key: string): undefined | StaticE<T> {
  const n = h.staticCache.get(ns);
  if (n) {
    return n.get(key);
  }
}

function insertStaticE<T>(h: Handle, ns: Symbol, key: string, e: StaticE<T>): void {
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
  applyStaticChanges(h, ns, key, mkStaticE<T>(h, ns, key), f);
}

// mkStatic
// -----------------------------------------------------------------------
//
// Even though this function has access to the 'Handle' and indeed modifies
// it, the changes have has no externally observable effect.

export function mkStaticE<T>(h: Handle, ns: Symbol, key: string): StaticE<T> {
  let s = lookupStaticE<T>(h, ns, key);
  if (!s) {
    s = new StaticE<T>();
    insertStaticE(h, ns, key, s);
  }

  return s;
}

// ----------------------------------------------------------------------------
// Ephemeral

function lookupEphemeralE<T>(h: Handle, ns: Symbol, key: string): undefined | EphemeralE<T> {
  const n = h.ephemeralCache.get(ns);
  if (n) {
    return n.get(key);
  }
}

function insertEphemeralE<T>(h: Handle, ns: Symbol, key: string, e: EphemeralE<T>): void {
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
  applyEphemeralChanges(h, ns, key, mkEphemeralE<T>(h, ns, key), f);
}

// mkEphemeralE
// -----------------------------------------------------------------------

export function mkEphemeralE<T>(h: Handle, ns: Symbol, key: string): EphemeralE<T> {
  let e = lookupEphemeralE<T>(h, ns, key);
  if (!e) {
    e = new EphemeralE<T>();
    insertEphemeralE(h, ns, key, e);
  }

  return e;
}


// runNetworkRequest
// -----------------------------------------------------------------------
//
// Run a network request attached to the given 'Entity'. This overwrites
// (invalidates) any currently running request. The promise is resolved only
// when the request is still valid. That is when you can handle the response
// and apply changes to the Handle.

export function attachNetworkRequestF(
  h: Handle,
  { entity, nr }: { entity: string | Static<any> | Ephemeral<any>; nr: NetworkRequest }
) {
  function f(e: { networkRequest: undefined | NetworkRequest }) {
    e.networkRequest = nr;
  }

  if (typeof entity === "string") {
    withEditable(h, entity, f);
  } else if (entity instanceof Static) {
    withStaticE(h, entity.ns, entity.key, f);
  } else if (entity instanceof Ephemeral) {
    withEphemeralE(h, entity.ns, entity.key, f);
  }
}

const attachNetworkRequestA = (entity: string | Static<any> | Ephemeral<any>, label: string, nr: NetworkRequest) =>
  mkAction(`attachNetworkRequest(${entityLabel(entity)},${label})`, { entity, nr }, attachNetworkRequestF);

function reportNetworkFailureF(
  h: Handle,
  {
    entity,
    nr,
    err
  }: {
    entity: string | Static<any> | Ephemeral<any>;
    nr: NetworkRequest;
    err: Error;
  }
) {
  function f(e: { networkRequest: undefined | NetworkRequest; lastError: any }): void {
    if (e.networkRequest === nr) {
      e.networkRequest = undefined;
      e.lastError = err;
    }
  }

  if (typeof entity === "string") {
    withEditable(h, entity, f);
  } else if (entity instanceof Static) {
    withStaticE(h, entity.ns, entity.key, f);
  } else if (entity instanceof Ephemeral) {
    withEphemeralE(h, entity.ns, entity.key, f);
  }
}

const reportNetworkFailureA = (entity: string | Static<any> | Ephemeral<any>, nr: NetworkRequest, err: Error) =>
  mkAction(`reportNetworkFailure(${entityLabel(entity)},${err})`, { entity, nr, err }, reportNetworkFailureF);

export async function runNetworkRequest<R>(
  h: Handle,
  entity: string | Static<any> | Ephemeral<any>,
  label: string,
  req: Promise<R>
): Promise<{ networkRequest: NetworkRequest; res: R }> {
  const networkRequest = new NetworkRequest(h.config.now(), req);
  modifyHandle(h, attachNetworkRequestA(entity, label, networkRequest));

  try {
    const res = await req;
    return { networkRequest, res };
  } catch (err) {
    modifyHandle(h, reportNetworkFailureA(entity, networkRequest, err));
    throw err;
  }
}