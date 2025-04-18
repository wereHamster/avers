import { Handle, Static, Ephemeral, Action, mkAction, NetworkRequest, EntityId } from "../types.js";
import { entityLabel, withEditable, withStaticE, withEphemeralE } from "../internal.js";
import { modifyHandle } from "./modifyHandle.js";

interface Payload {
  entity: EntityId;
  nr: NetworkRequest;
  err: Error;
}

function reportNetworkFailureF(h: Handle, { entity, nr, err }: Payload) {
  function f(e: { networkRequest: undefined | NetworkRequest; lastError: any }): void {
    if (e.networkRequest === nr) {
      e.networkRequest = undefined;
      e.lastError = err;
    }
  }

  if (typeof entity === "string") {
    withEditable(h, entity, f);
  } else if (entity instanceof Static) {
    withStaticE(h, entity.ns, entity.key, f);
  } else if (entity instanceof Ephemeral) {
    withEphemeralE(h, entity.ns, entity.key, f);
  }
}

const reportNetworkFailureA = (entity: EntityId, nr: NetworkRequest, err: Error): Action<Payload> =>
  mkAction(`reportNetworkFailure(${entityLabel(entity)},${err})`, { entity, nr, err }, reportNetworkFailureF);

export const reportNetworkFailure = (h: Handle, entity: EntityId, nr: NetworkRequest, err: Error): void => {
  modifyHandle(h, reportNetworkFailureA(entity, nr, err));
};
