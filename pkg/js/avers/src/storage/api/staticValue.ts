import Computation from "computation";

import { Handle, Static, StaticE } from "../types.js";
import { runNetworkRequest } from "../internal/runNetworkRequest.js";
import { mkStaticE } from "../internal/mkStaticE.js";
import { resolveStatic } from "./resolveStatic.js";

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

async function refreshStatic<T>(h: Handle, s: Static<T>, ent: StaticE<T>): Promise<void> {
  if (ent.value === Computation.Pending && ent.networkRequest === undefined && ent.lastError === undefined) {
    try {
      const res = await runNetworkRequest(h, s, "fetchStatic", s.fetch());
      resolveStatic(h, s, res);
    } catch (e) {
      // Ignore errors. runNetworkRequest already sets 'lastError'.
    }
  }
}
