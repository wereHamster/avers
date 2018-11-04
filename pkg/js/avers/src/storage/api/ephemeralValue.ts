import Computation from "computation";

import { Handle, Ephemeral, EphemeralE } from "../types";
import { runNetworkRequest } from "../internal/runNetworkRequest";
import { mkEphemeralE } from "../internal/mkEphemeralE";
import { resolveEphemeral } from "./resolveEphemeral";

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
      resolveEphemeral(h, e, res.value, res.expiresAt);
    } catch (err) {
      // Ignore errors. runNetworkRequest already sets 'lastError'.
    }
  }
}
