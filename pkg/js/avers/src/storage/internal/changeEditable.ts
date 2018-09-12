import { ObjId, Handle, Editable } from "../types";
import { applyEditableChanges } from "../internal";

export function changeEditable<T>(h: Handle, objId: ObjId, f: (obj: Editable<T>) => void): void {
  const obj = h.editableCache.get(objId);
  if (obj) {
    applyEditableChanges(h, obj, f);
  }
}
