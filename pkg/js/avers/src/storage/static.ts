import Computation from "computation";

import { immutableClone } from "../shared";
import { Handle, NetworkRequest, runNetworkRequest, modifyHandle, mkAction } from "../storage";

// Static<T>
// -----------------------------------------------------------------------
//
// A static value which is read-only. Is loaded from the server when
// required, then cached indefinitely (or until pruned from the cache).
// The objects are managed by the Avers Handle, they trigger a generation
// change when they are modified.

export class Static<T> {
  [Symbol.species]: "Static";

  constructor(public ns: Symbol, public key: string, public fetch: () => Promise<T>) {}
}

export class StaticE<T> {
  networkRequest: undefined | NetworkRequest = undefined;
  lastError: undefined | Error = undefined;
  value: T = Computation.Pending;
}

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

function mkStaticE<T>(h: Handle, ns: Symbol, key: string): StaticE<T> {
  let s = lookupStaticE<T>(h, ns, key);
  if (!s) {
    s = new StaticE<T>();
    insertStaticE(h, ns, key, s);
  }

  return s;
}

// staticValue
// -----------------------------------------------------------------------
//
// Extract the value from the Static as a Computation. If the value is not
// loaded yet, then a request will be sent to the server to fetch it.

export function staticValue<T>(h: Handle, s: Static<T>): Computation<T> {
  return new Computation(() => {
    const ent = mkStaticE<T>(h, s.ns, s.key);
    refreshStatic(h, s, ent);
    return ent.value;
  });
}

// refreshStatic
// -----------------------------------------------------------------------
//
// Internal function which is used to initiate the fetch if required.
//
// FIXME: Retry the request if the promise failed.

async function refreshStatic<T>(h: Handle, s: Static<T>, ent: StaticE<T>): Promise<void> {
  if (ent.value === undefined && ent.networkRequest === undefined) {
    try {
      const res = await runNetworkRequest(h, s, "fetchStatic", s.fetch());
      resolveStatic(h, s, res.res);
    } catch (e) {
       // Ignore errors. runNetworkRequest already sets 'lastError'.
    }
  }
}

function resolveStaticF<T>(h: Handle, { s, value }: { s: Static<T>; value: T }): void {
  withStaticE(h, s.ns, s.key, e => {
    e.networkRequest = undefined;
    e.lastError = undefined;
    e.value = value;
  });
}

const resolveStaticA = <T>(s: Static<T>, value: T) =>
  mkAction(`resolveStatic(${s.ns.toString()}, ${s.key})`, { s, value }, resolveStaticF);

export function resolveStatic<T>(h: Handle, s: Static<T>, value: T): void {
  modifyHandle(h, resolveStaticA(s, value));
}
