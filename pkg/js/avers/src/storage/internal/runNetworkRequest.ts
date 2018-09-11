import { Handle, NetworkRequest, EntityId } from "../types";

import { attachNetworkRequest } from "./attachNetworkRequest";
import { reportNetworkFailure } from "./reportNetworkFailure";

// runNetworkRequest
// -----------------------------------------------------------------------
//
// Run a network request attached to the given 'Entity'. This overwrites
// (invalidates) any currently running request. The promise is resolved only
// when the request is still valid. That is when you can handle the response
// and apply changes to the Handle.

export async function runNetworkRequest<R>(
  h: Handle,
  entity: EntityId,
  label: string,
  req: Promise<R>
): Promise<{ networkRequest: NetworkRequest; res: R }> {
  const networkRequest = new NetworkRequest(h.config.now(), req);
  attachNetworkRequest(h, entity, label, networkRequest);

  try {
    const res = await req;
    return { networkRequest, res };
  } catch (err) {
    reportNetworkFailure(h, entity, networkRequest, err);
    throw err;
  }
}
