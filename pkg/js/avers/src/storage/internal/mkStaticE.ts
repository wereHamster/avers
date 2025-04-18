import { Handle, StaticE, emptyStaticE } from "../types.js";
import { lookupStaticE, insertStaticE } from "../internal.js";

export function mkStaticE<T>(h: Handle, ns: Symbol, key: string): StaticE<T> {
  let s = lookupStaticE<T>(h, ns, key);
  if (!s) {
    s = emptyStaticE;
    insertStaticE(h, ns, key, s);
  }

  return s;
}
