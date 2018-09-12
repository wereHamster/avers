import { detachChangeListener, applyOperation, attachChangeListener } from "../../core";
import { Editable } from "../types";

export function initContent(obj: Editable<unknown>): void {
  if (obj.shadowContent === undefined) {
    return;
  }

  if (obj.content) {
    detachChangeListener(obj.content, obj.changeListener);
  }

  obj.content = obj.submittedChanges.concat(obj.localChanges).reduce((c, o) => {
    return applyOperation(c, o.path, o);
  }, obj.shadowContent);

  attachChangeListener(obj.content, obj.changeListener);
}
