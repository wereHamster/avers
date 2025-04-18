import { Handle, Static, Ephemeral, mkAction, NetworkRequest, EntityId, Action } from "../types.js";
import { entityLabel, withEditable, withStaticE, withEphemeralE } from "../internal.js";
import { modifyHandle } from "./modifyHandle.js";

interface Payload {
  entity: EntityId;
  nr: NetworkRequest;
}

function attachNetworkRequestF(h: Handle, { entity, nr }: Payload) {
  function f(e: { networkRequest: undefined | NetworkRequest }) {
    e.networkRequest = nr;
  }

  if (typeof entity === "string") {
    withEditable(h, entity, f);
  } else if (entity instanceof Static) {
    withStaticE(h, entity.ns, entity.key, f);
  } else if (entity instanceof Ephemeral) {
    withEphemeralE(h, entity.ns, entity.key, f);
  }
}

const attachNetworkRequestA = (entity: EntityId, label: string, nr: NetworkRequest): Action<Payload> =>
  mkAction(`attachNetworkRequest(${entityLabel(entity)},${label})`, { entity, nr }, attachNetworkRequestF);

export const attachNetworkRequest = (h: Handle, entity: EntityId, label: string, nr: NetworkRequest): void => {
  modifyHandle(h, attachNetworkRequestA(entity, label, nr));
};
