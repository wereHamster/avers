import { ObjId, Handle, mkAction } from "../types.js";
import { modifyHandle } from "../internal/modifyHandle.js";
import { changeEditable } from "../internal/changeEditable.js";

function prepareLocalChangesF(h: Handle, objId: ObjId) {
  changeEditable(h, objId, obj => {
    obj.submittedChanges = obj.localChanges;
    obj.localChanges = [];
  });
}

const prepareLocalChangesA = (objId: ObjId) => mkAction(`prepareLocalChanges(${objId})`, objId, prepareLocalChangesF);

export const prepareLocalChanges = (h: Handle, objId: ObjId): void => {
  modifyHandle(h, prepareLocalChangesA(objId));
};
