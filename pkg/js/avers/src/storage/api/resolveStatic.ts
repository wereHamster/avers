import { Handle, Static, mkAction } from "../types";
import { modifyHandle, withStaticE } from "../internal";

function resolveStaticF<T>(h: Handle, { s, value }: { s: Static<T>; value: T }): void {
  withStaticE(h, s.ns, s.key, e => {
    e.networkRequest = undefined;
    e.lastError = undefined;
    e.value = value;
  });
}

const resolveStaticA = <T>(s: Static<T>, value: T) =>
  mkAction(`resolveStatic(${s.ns.toString()}, ${s.key})`, { s, value }, resolveStaticF);

export function resolveStatic<T>(h: Handle, s: Static<T>, value: T): void {
  modifyHandle(h, resolveStaticA(s, value));
}
