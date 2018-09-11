import { Operation } from "../../core";
import { guardStatus } from "../../shared";
import { ObjId, Handle, Editable } from "../types";
import { endpointUrl } from "../internal";
import { runNetworkRequest } from "../internal/runNetworkRequest";

import { prepareLocalChanges } from "./prepareLocalChanges";
import { applyServerResponse } from "./applyServerResponse";
import { restoreLocalChanges } from "./restoreLocalChanges";
import { applyChange } from "./applyChange";

export async function saveEditable(h: Handle, objId: ObjId): Promise<void> {
  const obj = h.editableCache.get(objId);
  if (!obj) {
    return;
  }

  // ASSERT obj.status === Status.Loaded

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

    // We ignore whether the response is from the current NetworkRequest
    // or not. It's irrelevant, upon receeiving a successful response
    // from the server the changes have been stored in the database,
    // and there is no way back. We have no choice than to accept the
    // changes and apply to the local state.

    const body = res.res;

    console.log(
      [
        "Saved ",
        body.resultingPatches.length,
        " operations on ",
        objId,
        " (",
        body.previousPatches.length,
        " previous patches)"
      ].join("")
    );

    // Apply all server patches to the shadow content, to bring it up
    // to date WRT the server version. Also bump the revisionId to
    // reflect what the server has.

    applyServerResponse(h, objId, res, body);

    // See if we have any more local changes which we need to save.
    await saveEditable(h, objId);
  } catch (err) {
    // The server would presumably respond with changes which
    // were submitted before us, and we'd have to rebase our
    // changes on top of that.

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

// changeFeedSubscription
// -----------------------------------------------------------------------------
//
// Change the feed subscription. Opens the websocket if not already open.

function changeFeedSubscription(h: Handle, json: any): void {
  if (h.feedSocket === undefined) {
    h.feedSocket = h.config.createWebSocket("/feed");

    h.feedSocket.addEventListener("message", msg => {
      try {
        applyChange(h, JSON.parse(msg.data));
      } catch (e) {
        console.error("changeFeedSubscription: error when parsing message", e);
      }
    });
  }

  if (h.feedSocket.readyState === h.feedSocket.OPEN) {
    h.feedSocket.send(JSON.stringify(json));
  } else {
    h.feedSocket.addEventListener("open", function onOpen() {
      if (h.feedSocket !== undefined) {
        h.feedSocket.send(JSON.stringify(json));
        h.feedSocket.removeEventListener("open", onOpen);
      }
    });
  }
}
