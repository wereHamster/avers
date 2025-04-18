import { Operation } from "../../core.js";
import { guardStatus } from "../../shared.js";
import { ObjId, Handle } from "../types.js";
import { endpointUrl } from "../internal.js";
import { runNetworkRequest } from "../internal/runNetworkRequest.js";

import { prepareLocalChanges } from "./prepareLocalChanges.js";
import { applyServerResponse } from "./applyServerResponse.js";
import { restoreLocalChanges } from "./restoreLocalChanges.js";

export async function saveEditable(h: Handle, objId: ObjId): Promise<void> {
  const obj = h.editableCache.get(objId);
  if (!obj) {
    return;
  }

  // Guard on not having a request in flight. If this editable has any
  // local changes, they will be submitted when the request finishes.
  if (obj.submittedChanges.length > 0) {
    return;
  }

  // Guard on having some local changes which we can save.
  if (obj.localChanges.length === 0) {
    return;
  }

  const data = JSON.stringify({
    objectId: obj.objectId,
    revisionId: obj.revisionId,
    operations: filterOps(obj.localChanges)
  });

  // We immeadiately mark the Editable as being saved. This ensures that
  // any future attempts to save the editable are skipped.
  prepareLocalChanges(h, objId);

  try {
    const url = endpointUrl(h, "/objects/" + objId);
    const requestInit: RequestInit = {
      credentials: "include",
      method: "PATCH",
      body: data,
      headers: { accept: "application/json", "content-type": "application/json" }
    };

    const req = h.config
      .fetch(url, requestInit)
      .then(guardStatus("saveEditable", 200))
      .then(res => res.json());

    const res = await runNetworkRequest(h, objId, "saveEditable", req);
    applyServerResponse(h, objId, res);

    // See if we have any more local changes which we need to save.
    await saveEditable(h, objId);
  } catch (err) {
    restoreLocalChanges(h, objId);
  }
}

// Filter out subsequent operations which touch the same path.
function filterOps(ops: Operation[]): Operation[] {
  return ops.reduce((a: Operation[], op: Operation): Operation[] => {
    const lastOp = a[a.length - 1];

    if (lastOp && lastOp.path === op.path && lastOp.type === "set") {
      a[a.length - 1] = op;
    } else {
      a.push(op);
    }

    return a;
  }, []);
}
