import { Handle, Ephemeral, mkAction } from "../types";
import { insertEphemeralE } from "../internal";
import { modifyHandle } from "../internal/modifyHandle";

// resolveEphemeral<T>
// -----------------------------------------------------------------------
//
// This function is used when we receive the response from the Ephemeral<T>
// fetch function. This is exported to allow users to simulate these responses
// without actually hitting the network.

interface Payload<T> {
  e: Ephemeral<T>;
  value: T;
  expiresAt: number;
}

function resolveEphemeralF<T>(h: Handle, { e, value, expiresAt }: Payload<T>): void {
  insertEphemeralE(h, e.ns, e.key, {
    networkRequest: undefined,
    lastError: undefined,
    value,
    expiresAt
  });
}

const resolveEphemeralA = <T>(e: Ephemeral<T>, value: T, expiresAt: number) =>
  mkAction(`resolveEphemeral(${e.ns.toString()}, ${e.key})`, { e, value, expiresAt }, resolveEphemeralF);

export function resolveEphemeral<T>(h: Handle, e: Ephemeral<T>, value: T, expiresAt: number): void {
  modifyHandle(h, resolveEphemeralA(e, value, expiresAt));
}
