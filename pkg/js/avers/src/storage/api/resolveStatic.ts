import { Handle, Static, mkAction } from "../types";
import { insertStaticE } from "../internal";
import { modifyHandle } from "../internal/modifyHandle";

function resolveStaticF<T>(h: Handle, { s, value }: { s: Static<T>; value: T }): void {
  insertStaticE(h, s.ns, s.key, {
    networkRequest: undefined,
    lastError: undefined,
    value
  });
}

const resolveStaticA = <T>(s: Static<T>, value: T) =>
  mkAction(`resolveStatic(${s.ns.toString()}, ${s.key})`, { s, value }, resolveStaticF);

export function resolveStatic<T>(h: Handle, s: Static<T>, value: T): void {
  modifyHandle(h, resolveStaticA(s, value));
}
