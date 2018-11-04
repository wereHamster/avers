import { Handle, NetworkRequest, EntityId } from "../types";

import { attachNetworkRequest } from "./attachNetworkRequest";
import { reportNetworkFailure } from "./reportNetworkFailure";

// runNetworkRequest
// -----------------------------------------------------------------------
//
// Run a network request attached to the given 'Entity'. This overwrites
// (invalidates) any currently running request.

export async function runNetworkRequest<R>(
  h: Handle,
  entity: EntityId,
  label: string,
  req: Promise<R>
): Promise<R> {
  const networkRequest = new NetworkRequest(h.config.now(), req);
  attachNetworkRequest(h, entity, label, networkRequest);

  try {
    return await req;
  } catch (err) {
    reportNetworkFailure(h, entity, networkRequest, err);
    throw err;
  }
}
