import { ObjId, Handle, mkAction } from "../types";
import { modifyHandle, withEditable } from "../internal";

function restoreLocalChangesF(h: Handle, objId: ObjId) {
  withEditable(h, objId, obj => {
    obj.localChanges = obj.submittedChanges.concat(obj.localChanges);
    obj.submittedChanges = [];
  });
}

const restoreLocalChangesA = (objId: ObjId) => mkAction(`restoreLocalChanges(${objId})`, objId, restoreLocalChangesF);

export const restoreLocalChanges = (h: Handle, objId: ObjId): void => {
  modifyHandle(h, restoreLocalChangesA(objId));
};
