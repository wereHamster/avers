import { ObjId, Handle, mkAction } from "../types.js";
import { modifyHandle } from "../internal/modifyHandle.js";
import { changeEditable } from "../internal/changeEditable.js";
import { initContent } from "../internal/initContent.js";
import { applyPatches } from "../internal/applyPatches.js";

function applyServerResponseF(h: Handle, { objId, body }: { objId: string; body: any }): void {
  changeEditable(h, objId, obj => {
    obj.networkRequest = undefined;

    // Clear out any traces that we've submitted changes to the
    // server.
    obj.submittedChanges = [];

    applyPatches(obj, [].concat(body.previousPatches, body.resultingPatches));
    initContent(obj);
  });
}

const applyServerResponseA = (objId: ObjId, body: any) =>
  mkAction(`applyServerResponse(${objId})`, { objId, body }, applyServerResponseF);

export const applyServerResponse = (h: Handle, objId: ObjId, body: any): void => {
  modifyHandle(h, applyServerResponseA(objId, body));
};
