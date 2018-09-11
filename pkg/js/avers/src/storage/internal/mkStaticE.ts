import { Handle, StaticE } from "../types";
import { lookupStaticE, insertStaticE } from "../internal";

export function mkStaticE<T>(h: Handle, ns: Symbol, key: string): StaticE<T> {
  let s = lookupStaticE<T>(h, ns, key);
  if (!s) {
    s = new StaticE<T>();
    insertStaticE(h, ns, key, s);
  }

  return s;
}
