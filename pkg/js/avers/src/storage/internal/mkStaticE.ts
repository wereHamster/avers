import { insertStaticE, lookupStaticE } from "../internal.js";
import { emptyStaticE, type Handle, type StaticE } from "../types.js";

export function mkStaticE<T>(h: Handle, ns: symbol, key: string): StaticE<T> {
  let s = lookupStaticE<T>(h, ns, key);
  if (!s) {
    s = emptyStaticE;
    insertStaticE(h, ns, key, s);
  }

  return s;
}
