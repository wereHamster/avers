import { Operation } from "../../core.js";
import { ObjId, Handle, mkAction } from "../types.js";
import { modifyHandle } from "../internal/modifyHandle.js";
import { changeEditable } from "../internal/changeEditable.js";
import { initContent } from "../internal/initContent.js";

function captureChangesF(h: Handle, { objId, ops }: { objId: string; ops: any }): void {
  changeEditable(h, objId, obj => {
    obj.localChanges = obj.localChanges.concat(ops);
    initContent(obj);
  });
}

const captureChangesA = (objId: ObjId, ops: Operation[]) =>
  mkAction(`captureChanges(${objId},${ops.length})`, { objId, ops }, captureChangesF);

export const captureChanges = (h: Handle, objId: ObjId, ops: Operation[]): void => {
  modifyHandle(h, captureChangesA(objId, ops));
};
