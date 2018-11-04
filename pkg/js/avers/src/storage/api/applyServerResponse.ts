import { ObjId, Handle, mkAction } from "../types";
import { modifyHandle } from "../internal/modifyHandle";
import { changeEditable } from "../internal/changeEditable";
import { initContent } from "../internal/initContent";
import { applyPatches } from "../internal/applyPatches";

function applyServerResponseF(h: Handle, { objId, body }: { objId: string; body: any }): void {
  changeEditable(h, objId, obj => {
    // Clear out any traces that we've submitted changes to the
    // server.
    obj.submittedChanges = [];

    applyPatches(obj, [].concat(body.previousPatches, body.resultingPatches));
    initContent(obj);
  });
}

const applyServerResponseA = (objId: ObjId, body: any) =>
  mkAction(`applyServerResponse(${objId})`, { objId, body }, applyServerResponseF);

export const applyServerResponse = (
  h: Handle,
  objId: ObjId,
  body: any
): void => {
  modifyHandle(h, applyServerResponseA(objId, body));
};
