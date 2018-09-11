import { Handle, EphemeralE, emptyEphemeralE } from "../types";
import { lookupEphemeralE, insertEphemeralE } from "../internal";

export function mkEphemeralE<T>(h: Handle, ns: Symbol, key: string): EphemeralE<T> {
  let e = lookupEphemeralE<T>(h, ns, key);
  if (!e) {
    e = emptyEphemeralE;
    insertEphemeralE(h, ns, key, e);
  }

  return e;
}
