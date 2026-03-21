import { insertEphemeralE, lookupEphemeralE } from "../internal.js";
import { type EphemeralE, emptyEphemeralE, type Handle } from "../types.js";

export function mkEphemeralE<T>(h: Handle, ns: symbol, key: string): EphemeralE<T> {
  let e = lookupEphemeralE<T>(h, ns, key);
  if (!e) {
    e = emptyEphemeralE;
    insertEphemeralE(h, ns, key, e);
  }

  return e;
}
