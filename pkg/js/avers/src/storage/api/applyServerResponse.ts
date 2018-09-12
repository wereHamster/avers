import { ObjId, Handle, mkAction, NetworkRequest } from "../types";
import { modifyHandle } from "../internal/modifyHandle";
import { changeEditable } from "../internal/changeEditable";
import { initContent } from "../internal/initContent";
import { applyPatches } from "../internal/applyPatches";

function applyServerResponseF(h: Handle, { objId, res, body }: { objId: string; res: any; body: any }): void {
  changeEditable(h, objId, obj => {
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
