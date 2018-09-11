import { Operation } from "../../core";
import { ObjId, Handle, mkAction } from "../types";
import { modifyHandle, withEditable, initContent } from "../internal";

function captureChangesF(h: Handle, { objId, ops }: { objId: string; ops: any }): void {
  withEditable(h, objId, obj => {
    obj.localChanges = obj.localChanges.concat(ops);
    initContent(obj);
  });
}

const captureChangesA = (objId: ObjId, ops: Operation[]) =>
  mkAction(`captureChanges(${objId},${ops.length})`, { objId, ops }, captureChangesF);

export const captureChanges = (h: Handle, objId: ObjId, ops: Operation[]): void => {
  modifyHandle(h, captureChangesA(objId, ops));
};
