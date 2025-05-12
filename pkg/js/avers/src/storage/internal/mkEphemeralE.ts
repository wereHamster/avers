import { Handle, EphemeralE, emptyEphemeralE } from "../types.js";
import { lookupEphemeralE, insertEphemeralE } from "../internal.js";

export function mkEphemeralE<T>(h: Handle, ns: symbol, key: string): EphemeralE<T> {
  let e = lookupEphemeralE<T>(h, ns, key);
  if (!e) {
    e = emptyEphemeralE;
    insertEphemeralE(h, ns, key, e);
  }

  return e;
}
