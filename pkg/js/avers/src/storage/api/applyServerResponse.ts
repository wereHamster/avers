import { ObjId, Handle, mkAction, NetworkRequest } from "../types";
import { modifyHandle, withEditable, initContent, applyPatches } from "../internal";

function applyServerResponseF(h: Handle, { objId, res, body }: { objId: string; res: any; body: any }): void {
  withEditable(h, objId, obj => {
    if (obj.networkRequest === res.networkRequest) {
      obj.networkRequest = undefined;
    }

    // Clear out any traces that we've submitted changes to the
    // server.
    obj.submittedChanges = [];

    applyPatches(obj, [].concat(body.previousPatches, body.resultingPatches));
    initContent(obj);
  });
}

const applyServerResponseA = (objId: ObjId, res: { networkRequest: NetworkRequest; res: any }, body: any) =>
  mkAction(`applyServerResponse(${objId})`, { objId, res, body }, applyServerResponseF);

export const applyServerResponse = (
  h: Handle,
  objId: ObjId,
  res: { networkRequest: NetworkRequest; res: any },
  body: any
): void => {
  modifyHandle(h, applyServerResponseA(objId, res, body));
};
