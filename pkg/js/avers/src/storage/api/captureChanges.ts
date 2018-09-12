import { Operation } from "../../core";
import { ObjId, Handle, mkAction } from "../types";
import { modifyHandle } from "../internal/modifyHandle";
import { changeEditable } from "../internal/changeEditable";
import { initContent } from "../internal/initContent";

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
