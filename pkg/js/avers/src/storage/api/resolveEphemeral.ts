import { Handle, Ephemeral, mkAction } from "../types";
import { modifyHandle, withEphemeralE } from "../internal";

// resolveEphemeral<T>
// -----------------------------------------------------------------------
//
// This function is used when we receive the response from the Ephemeral<T>
// fetch function. This is exported to allow users to simulate these responses
// without actually hitting the network.

function resolveEphemeralF<T>(
  h: Handle,
  { e, value, expiresAt }: { e: Ephemeral<T>; value: T; expiresAt: number }
): void {
  withEphemeralE(h, e.ns, e.key, e => {
    e.networkRequest = undefined;
    e.lastError = undefined;
    e.value = value;
    e.expiresAt = expiresAt;
  });
}

const resolveEphemeralA = <T>(e: Ephemeral<T>, value: T, expiresAt: number) =>
  mkAction(`resolveEphemeral(${e.ns.toString()}, ${e.key})`, { e, value, expiresAt }, resolveEphemeralF);

export function resolveEphemeral<T>(h: Handle, e: Ephemeral<T>, value: T, expiresAt: number): void {
  modifyHandle(h, resolveEphemeralA(e, value, expiresAt));
}
