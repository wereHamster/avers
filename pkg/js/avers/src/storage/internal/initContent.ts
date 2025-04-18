import { detachChangeListener, applyOperation, attachChangeListener, clone } from "../../core.js";
import { Editable } from "../types.js";

export function initContent(obj: Editable<unknown>): void {
  // XXX: When does this happen?
  if (obj.shadowContent === undefined) {
    return;
  }

  if (obj.content) {
    detachChangeListener(obj.content, obj.changeListener);
  }

  // All changes we want to apply on top of the 'shadowContent'.
  const changes = obj.submittedChanges.concat(obj.localChanges);

  obj.content = changes.reduce((c, o) => {
    return applyOperation(c, o.path, o);
  }, clone(obj.shadowContent));

  attachChangeListener(obj.content, obj.changeListener);
}
