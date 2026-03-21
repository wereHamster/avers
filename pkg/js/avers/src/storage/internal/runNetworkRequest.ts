import { type EntityId, type Handle, NetworkRequest } from "../types.js";

import { attachNetworkRequest } from "./attachNetworkRequest.js";
import { reportNetworkFailure } from "./reportNetworkFailure.js";

/**
 * Run a network request attached to the given 'Entity'.
 * This overwrites (invalidates) any currently running request.
 */
export async function runNetworkRequest<R>(h: Handle, entity: EntityId, label: string, req: Promise<R>): Promise<R> {
  const networkRequest = new NetworkRequest(h.config.now(), req);
  attachNetworkRequest(h, entity, label, networkRequest);

  try {
    return await req;
  } catch (err: unknown) {
    reportNetworkFailure(h, entity, networkRequest, err as Error);
    throw err;
  }
}
