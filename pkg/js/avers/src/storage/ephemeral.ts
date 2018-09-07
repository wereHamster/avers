import Computation from "computation";

import { immutableClone } from "../shared";
import { Handle, NetworkRequest, runNetworkRequest, modifyHandle, mkAction } from "../storage";

// Ephemeral<T>
// -----------------------------------------------------------------------
//
// Ephemeral<T> objects are similar to Static<T> in that they can't be
// modified, but they can expire and become stale. Once stale they are
// re-fetched.

export class Ephemeral<T> {
  [Symbol.species]: "Ephemeral";

  constructor(public ns: Symbol, public key: string, public fetch: () => Promise<{ value: T; expiresAt: number }>) {}
}

// EphemeralE<T>
// ------------------------------------------------------------------------
//
// The internal object for an Ephemeral<T> which stores the actual value and
// keeps track of the network interaction.
//
// This is an internal class. It is not exposed through any public API, except
// through the 'ephemeralCache' in the Handle.

export class EphemeralE<T> {
  networkRequest: undefined | NetworkRequest = undefined;
  lastError: undefined | Error = undefined;
  value: undefined | T = undefined;
  expiresAt: number = 0;
}

function lookupEphemeralE<T>(h: Handle, ns: Symbol, key: string): undefined | EphemeralE<T> {
  let n = h.ephemeralCache.get(ns);
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

function mkEphemeralE<T>(h: Handle, ns: Symbol, key: string): EphemeralE<T> {
  let e = lookupEphemeralE<T>(h, ns, key);
  if (!e) {
    e = new EphemeralE<T>();
    insertEphemeralE(h, ns, key, e);
  }

  return e;
}

// ephemeralValue
// -----------------------------------------------------------------------
//
// Extract the value from the Static as a Computation. If the value is not
// loaded yet, then a request will be sent to the server to fetch it.

export function ephemeralValue<T>(h: Handle, e: Ephemeral<T>): Computation<T> {
  return new Computation(() => {
    let ent = mkEphemeralE<T>(h, e.ns, e.key);

    refreshEphemeral<T>(h, e, ent);

    if (ent.value === undefined) {
      return Computation.Pending;
    } else {
      return ent.value;
    }
  });
}

// refreshEphemeral
// -----------------------------------------------------------------------
//
// Internal function which is used to initiate the fetch if required.
//
// FIXME: Retry the request if the promise failed.

function refreshEphemeral<T>(h: Handle, e: Ephemeral<T>, ent: EphemeralE<T>): void {
  let now = h.now();
  if ((ent.value === undefined || now > ent.expiresAt) && ent.networkRequest === undefined) {
    runNetworkRequest(h, e, "fetchEphemeral", e.fetch()).then(res => {
      resolveEphemeral(h, e, res.res.value, res.res.expiresAt);
    });
  }
}

// resolveEphemeral<T>
// -----------------------------------------------------------------------
//
// This function is used when we receive the response from the Ephemeral<T>
// fetch function. This is exported to allow users to simulate these responses
// without actually hitting the network.

function resolveEphemeralF<T>(
  h: Handle,
  { e, value, expiresAt }: { e: Ephemeral<T>; value: T; expiresAt: number }
): void {
  withEphemeralE(h, e.ns, e.key, e => {
    e.networkRequest = undefined;
    e.lastError = undefined;
    e.value = value;
    e.expiresAt = expiresAt;
  });
}

export function resolveEphemeral<T>(h: Handle, e: Ephemeral<T>, value: T, expiresAt: number): void {
  modifyHandle(
    h,
    mkAction(`resolveEphemeral(${e.ns.toString()}, ${e.key})`, { e, value, expiresAt }, resolveEphemeralF)
  );
}
