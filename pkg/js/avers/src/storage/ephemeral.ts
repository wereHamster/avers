import Computation from "computation";

import { Handle, mkAction, Ephemeral, EphemeralE } from "./types";
import { modifyHandle, mkEphemeralE, withEphemeralE, runNetworkRequest } from "./internal";

// ephemeralValue
// -----------------------------------------------------------------------
//
// Extract the value from the Static as a Computation. If the value is not
// loaded yet, then a request will be sent to the server to fetch it.

export function ephemeralValue<T>(h: Handle, e: Ephemeral<T>): Computation<T> {
  return new Computation(() => {
    const ent = mkEphemeralE<T>(h, e.ns, e.key);
    refreshEphemeral<T>(h, e, ent);
    return ent.value;
  });
}

// refreshEphemeral
// -----------------------------------------------------------------------
//
// Internal function which is used to initiate the fetch if required.
//
// FIXME: Retry the request if the promise failed.

async function refreshEphemeral<T>(h: Handle, e: Ephemeral<T>, ent: EphemeralE<T>): Promise<void> {
  const now = h.config.now();
  if ((ent.value === Computation.Pending || now > ent.expiresAt) && ent.networkRequest === undefined) {
    try {
      const res = await runNetworkRequest(h, e, "fetchEphemeral", e.fetch());
      resolveEphemeral(h, e, res.res.value, res.res.expiresAt);
    } catch (err) {
      // Ignore errors. runNetworkRequest already sets 'lastError'.
    }
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

const resolveEphemeralA = <T>(e: Ephemeral<T>, value: T, expiresAt: number) =>
  mkAction(`resolveEphemeral(${e.ns.toString()}, ${e.key})`, { e, value, expiresAt }, resolveEphemeralF);

export function resolveEphemeral<T>(h: Handle, e: Ephemeral<T>, value: T, expiresAt: number): void {
  modifyHandle(h, resolveEphemeralA(e, value, expiresAt));
}
