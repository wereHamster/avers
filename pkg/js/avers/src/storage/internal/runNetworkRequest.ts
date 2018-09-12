import { Handle, NetworkRequest, EntityId, Static, Ephemeral } from "../types";

import { attachNetworkRequest } from "./attachNetworkRequest";
import { reportNetworkFailure } from "./reportNetworkFailure";
import { lookupStaticE, lookupEphemeralE } from "../internal";

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
): Promise<undefined | { networkRequest: NetworkRequest; res: R }> {
  const networkRequest = new NetworkRequest(h.config.now(), req);
  attachNetworkRequest(h, entity, label, networkRequest);

  try {
    // Wait for the response. This may take a while.
    const res = await req;

    // Return the response only if the 'NetworkRequest' is still current.
    if (attachedNetworkRequest(h, entity) === networkRequest) {
      return { networkRequest, res };
    }
  } catch (err) {
    reportNetworkFailure(h, entity, networkRequest, err);
    throw err;
  }
}

function attachedNetworkRequest(h: Handle, entity: EntityId): undefined | NetworkRequest {
  const obj = (() => {
    if (typeof entity === "string") {
      return h.editableCache.get(entity);
    } else if (entity instanceof Static) {
      return lookupStaticE(h, entity.ns, entity.key);
    } else if (entity instanceof Ephemeral) {
      return lookupEphemeralE(h, entity.ns, entity.key);
    }
  })();

  if (obj) {
    return obj.networkRequest;
  }
}
