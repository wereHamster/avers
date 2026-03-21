import { type Change, type ChangeCallback, changeOperation, type Operation } from "../../core.js";
import { type Editable, type Handle, newEditable, type ObjId } from "../types.js";

import { captureChanges } from "./captureChanges.js";
import { changeFeedSubscription } from "./changeFeedSubscription.js";
import { saveEditable } from "./saveEditable.js";

/**
 * Create a new Editable and load an object from the server into it.
 * The object is cached in the handle, so it is safe to call this function repeatedly with the same id.
 */
export function mkEditable<T>(h: Handle, id: ObjId): Editable<T> {
  let obj = h.editableCache.get(id);
  if (!obj) {
    obj = newEditable(id, mkChangeListener(h, id));
    h.editableCache.set(id, Object.freeze(obj));

    changeFeedSubscription(h, ["+", id]);
  }

  return obj;
}

function mkChangeListener(h: Handle, objId: ObjId): ChangeCallback {
  const save = debounce(saveEditable, 1500);

  return function onChange(changes: Change<Operation.Set | Operation.Splice>[]): void {
    captureChanges(h, objId, changes.map(changeOperation));
    save(h, objId);
  };
}

function debounce<T extends any[]>(func: (...args: T) => void, wait: any): (...args: T) => void {
  let timeout: undefined | ReturnType<typeof setTimeout>, args: any, timestamp: number;

  const later = () => {
    const last = Date.now() - timestamp;

    if (last < wait && last >= 0) {
      timeout = setTimeout(later, wait - last);
    } else {
      timeout = undefined;
      func.apply(null, args);
      args = undefined;
    }
  };

  return <any>function () {
    args = arguments;
    timestamp = Date.now();

    if (!timeout) {
      timeout = setTimeout(later, wait);
    }
  };
}
