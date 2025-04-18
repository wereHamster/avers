import { ObjId, Handle, Editable } from "../types.js";
import { applyEditableChanges } from "../internal.js";

export function changeEditable<T>(h: Handle, objId: ObjId, f: (obj: Editable<T>) => void): void {
  const obj = h.editableCache.get(objId);
  if (obj) {
    applyEditableChanges(h, obj, f);
  }
}
