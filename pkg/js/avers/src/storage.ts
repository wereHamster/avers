// Avers Storage Extension
// ---------------------------------------------------------------------------
//
// This is an extension for the Avers module which adds functionality to
// manage 'Editable' objects and synchronize changes to a server through
// a HTTP API.
//
// This file depends on the Computation library [1], ES6 Promises [2] and
// Symbol [3].
//
// [1]: https://github.com/wereHamster/computation
// [2]: https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/Promise
// [3]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol

import Computation from "computation";

import { guardStatus } from "./shared";
import {
  applyOperation,
  Operation,
  Change,
  ChangeCallback,
  changeOperation,
  parseJSON,
  migrateObject,
  attachChangeListener,
  detachChangeListener
} from "./core";

export * from "./storage/collection";
export * from "./storage/ephemeral";
export * from "./storage/internal";
export * from "./storage/keyed-collection";
export * from "./storage/patch";
export * from "./storage/static";
export * from "./storage/types";

import { ObjId, Handle, Editable, Static, Ephemeral, mkAction, NetworkRequest } from "./storage/types";
import {
  endpointUrl,
  modifyHandle,
  startNextGeneration,
  withEditable,
  applyEditableChanges,
  runNetworkRequest
} from "./storage/internal";
import { Patch, parsePatch } from "./storage/patch";

// attachGenerationListener
// -----------------------------------------------------------------------
//
// Attach a listener to the handle which will be invoked everytime data
// managed by the handle changes.
//
// If you need to detache the listener later, hang on to the return value
// and pass that to 'detachGenerationListener'.

export function attachGenerationListener(h: Handle, f: () => void): Function {
  const generationChangeCallback = f.bind(null);
  h.generationChangeCallbacks.add(generationChangeCallback);
  return generationChangeCallback;
}

// detachGenerationListener
// -----------------------------------------------------------------------
//
// Detach a generation listener from the handle. The listener is the value
// you get from 'attachGenerationListener'.

export function detachGenerationListener(h: Handle, listener: Function): void {
  h.generationChangeCallbacks.delete(listener);
}

// networkRequests
// -----------------------------------------------------------------------
//
// Array of all network requests which are currently active on the handle.

export function networkRequests(h: Handle): NetworkRequest[] {
  const ret: NetworkRequest[] = [];

  for (const obj of h.editableCache.values()) {
    const nr = obj.networkRequest;
    if (nr !== undefined) {
      ret.push(nr);
    }
  }

  return ret;
}

// localChanges
// -----------------------------------------------------------------------
//
// Array of all objects which have local changes which were not yet
// submitted to the server.

export function localChanges(h: Handle): { obj: Editable<any>; changes: Operation[] }[] {
  const ret: { obj: Editable<any>; changes: Operation[] }[] = [];

  for (const obj of h.editableCache.values()) {
    if (obj.localChanges.length > 0) {
      ret.push({ obj: obj, changes: obj.localChanges });
    }
  }

  return ret;
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

function applyChangeF(h: Handle, change: { type: string; content: any }): void {
  const { type, content } = change;

  if (type === "patch") {
    const patch = parsePatch(content);
    updateEditable(h, patch.objectId, obj => {
      applyPatches(obj, [patch]);
      initContent(obj);
    });
  } else {
    console.info("applyChangeF: Unhandled type: " + type);
  }
}

const applyChangeA = (change: { type: string; content: any }) =>
  mkAction(`applyChange(${change.type})`, change, applyChangeF);

function applyChange(h: Handle, change: { type: string; content: any }): void {
  modifyHandle(h, applyChangeA(change));
}

// applyPatches
// -----------------------------------------------------------------------------
//
// The given patches MUST have consecutive revisionIds!

function applyPatches(obj: Editable<any>, patches: Patch[]): void {
  if (obj.shadowContent === undefined) {
    // We simply ignore any attempt to apply patches to an Editable which
    // is not resolved.
    //
    // We could store the patches somewhere and attempt to apply them once
    // the Editable is resolved. But that would require careful changes to
    // multiple parts of the code. Given how low the chance is for that to
    // happen and cause problems, we decided to not handle that situation.

    return;
  }

  const applicablePatches = patches.filter(p => p.revisionId > obj.revisionId);
  if (applicablePatches.length === 0 || applicablePatches[0].revisionId !== obj.revisionId + 1) {
    // The first patch is not one that can be applied directly on the
    // Editable. This means there is a gap between the first patche we have
    // and the state of the Editable.
    //
    // We could consider storing the patches somewhere, reload the ones
    // which are missing and then apply them all. But, again, the chance
    // for that to happen is so low that we don't bother.

    return;
  }

  obj.revisionId += applicablePatches.length;
  obj.shadowContent = applicablePatches.reduce((c, patch) => {
    const op = patch.operation;
    return applyOperation(c, op.path, op);
  }, obj.shadowContent);
}

// mkEditable
// -----------------------------------------------------------------------
//
// Create a new Editable and load an object from the server into it. The
// object is cached in the handle, so it is safe to call this function
// repeatedly with the same id.

export function mkEditable<T>(h: Handle, id: string): Editable<T> {
  let obj = h.editableCache.get(id);
  if (!obj) {
    obj = new Editable<T>(id);
    obj.changeListener = mkChangeListener(h, id);

    h.editableCache.set(id, Object.freeze(obj) as any);

    changeFeedSubscription(h, ["+", id]);
  }

  return obj;
}

// lookupEditable
// -----------------------------------------------------------------------
//
// Get an object by its id. This computation is pending until the object
// has been fully loaded.

export function lookupEditable<T>(h: Handle, id: string): Computation<Editable<T>> {
  return new Computation(() => {
    if (id) {
      const obj = mkEditable<T>(h, id);
      if (!obj.content) {
        if (obj.networkRequest === undefined) {
          loadEditable(h, obj);
        }

        return Computation.Pending;
      } else {
        return obj;
      }
    } else {
      throw new Error("Avers.lookupEditable: invalid id <" + id + ">");
    }
  });
}

// lookupContent
// -----------------------------------------------------------------------
//
// Often you don't need the whole Editable wrapper, but the content inside
// it. This is a convenience function to get just that.

export function lookupContent<T>(h: Handle, id: string): Computation<T> {
  return lookupEditable<T>(h, id).fmap(x => x.content);
}

// fetchEditable
// -----------------------------------------------------------------------
//
// Wait until an Editable is Loaded or Failed (the Promise is resolved or
// rejected accordingly). This is useful in asynchronous code where you
// can't use 'Computation' (lookupEditable).

export function fetchEditable<T>(h: Handle, id: string): Promise<Editable<T>> {
  return new Promise((resolve, reject) => {
    (function check(obj?: any) {
      obj = mkEditable(h, id);

      if (obj.content !== undefined) {
        resolve(obj);
      } else if (obj.lastError !== undefined) {
        reject();
      } else {
        const nr = obj.networkRequest,
          req = nr ? nr.promise : loadEditable(h, obj);

        req.then(check).catch(check);
      }
    })();
  });
}

function debounce<T extends any[]>(func: (...args: T) => void, wait: any): (...args: T) => void {
  let timeout: void | number, args: any, timestamp: number;

  const later = function() {
    const last = Date.now() - timestamp;

    if (last < wait && last >= 0) {
      timeout = setTimeout(later, wait - last);
    } else {
      timeout = undefined;
      func.apply(context, args);
      args = undefined;
    }
  };

  return <any>function() {
    args = arguments;
    timestamp = Date.now();

    if (!timeout) {
      timeout = setTimeout(later, wait);
    }
  };
}

// updateEditable
// -----------------------------------------------------------------------
//
// A non-destructive update of an 'Editable'. The callback is given a copy
// of the original and can set any properties on it. The copy is then
// inserted into the cache.
//
// If the 'Editable' doesn't exist in the cache then it is created.

function updateEditable<T>(h: Handle, objId: ObjId, f: (obj: Editable<T>) => void): void {
  const obj = mkEditable<T>(h, objId);
  applyEditableChanges<T>(h, obj, f);
}

// loadEditable
// -----------------------------------------------------------------------
//
// Fetch an object from the server and initialize the Editable with the
// response.

export async function loadEditable<T>(h: Handle, obj: Editable<T>): Promise<void> {
  const objId = obj.objectId;

  const res = await runNetworkRequest(h, objId, "fetchEditable", fetchObject(h, objId));
  const e = h.editableCache.get(objId);

  if (e && e.networkRequest === res.networkRequest) {
    // FIXME: Clearing the networkRequest from the entity maybe should
    // be a separate action, eg. 'finishNetworkRequest'. Currently it's
    // part of resolveEditable. But that function may be called from
    // somebody else, outside of the context of a network request.

    resolveEditable<T>(h, objId, res.res);
  }
}

// fetchObject
// -----------------------------------------------------------------------
//
// Fetch the raw JSON of an object from the server.

export async function fetchObject(h: Handle, id: string): Promise<any> {
  const url = endpointUrl(h, "/objects/" + id);
  const requestInit: RequestInit = {
    credentials: "include",
    headers: { accept: "application/json" }
  };

  const res = await h.config.fetch(url, requestInit);
  await guardStatus("fetchObject", 200)(res);
  return res.json();
}

export async function createObject(h: Handle, type: string, content: any): Promise<string> {
  const url = endpointUrl(h, "/objects");
  const requestInit: RequestInit = {
    credentials: "include",
    method: "POST",
    body: JSON.stringify({ type: type, content: content }),
    headers: { accept: "application/json", "content-type": "application/json" }
  };

  const res = await h.config.fetch(url, requestInit);
  await guardStatus("createObject", 200)(res);
  const json = await res.json();
  startNextGeneration(h);
  return json.id;
}

export async function createObjectId(h: Handle, objId: ObjId, type: string, content: any): Promise<{}> {
  const url = endpointUrl(h, "/objects/" + objId);
  const requestInit: RequestInit = {
    credentials: "include",
    method: "POST",
    body: JSON.stringify({ type: type, content: content }),
    headers: { accept: "application/json", "content-type": "application/json" }
  };

  const res = await h.config.fetch(url, requestInit);
  await res.json();
  startNextGeneration(h);
  return {};
}

export async function deleteObject(h: Handle, id: string): Promise<void> {
  const url = endpointUrl(h, "/objects/" + id);
  const res = await h.config.fetch(url, { credentials: "include", method: "DELETE" });
  startNextGeneration(h);
}

function initContent(obj: Editable<any>): void {
  if (obj.shadowContent === undefined) {
    return;
  }

  if (obj.content) {
    detachChangeListener(obj.content, obj.changeListener);
  }

  obj.content = (<Operation[]>[]).concat(obj.submittedChanges, obj.localChanges).reduce((c, o) => {
    return applyOperation(c, o.path, o);
  }, obj.shadowContent);

  attachChangeListener(obj.content, obj.changeListener);
}

// resolveEditable
// -----------------------------------------------------------------------
//
// Given a response from the server, initialize an 'Editable' with the data.
//
// Note that this will invalidate any currently running network requests and
// drop any local changes.

function resolveEditableF<T>(h: Handle, { objId, json }: { objId: string; json: any }) {
  // ASSERT objId === json.id

  updateEditable(h, objId, obj => {
    obj.networkRequest = undefined;
    obj.lastError = undefined;

    obj.type = json.type;
    obj.objectId = json.id;
    obj.createdAt = new Date(Date.parse(json.createdAt));
    obj.createdBy = json.createdBy;
    obj.revisionId = json.revisionId || 0;

    const ctor = h.config.infoTable.get(obj.type);
    if (ctor === undefined) {
      throw new Error(`resolveEditable: unknown type ${obj.type}`);
    }
    obj.shadowContent = parseJSON<T>(ctor, json.content);

    obj.submittedChanges = [];
    obj.localChanges = [];

    initContent(obj);
  });
}

const resolveEditableA = (objId: ObjId, json: any) =>
  mkAction(`resolveEditable(${objId})`, { objId, json }, resolveEditableF);

export function resolveEditable<T>(h: Handle, objId: ObjId, json: any): void {
  modifyHandle(h, resolveEditableA(objId, json));

  // The migration must be done outside of the 'modifyHandle' block, because
  // migration may dispatch actions which modify the handle. Even though
  // recursive calls to 'modifyHandle' work fine, we should avoid them.
  //
  // The lookup in the cache can not fail, the object is guaranteed
  // to exist. But we can't encode that in the type system so we should do
  // the check.

  const obj = h.editableCache.get(objId);
  if (obj !== undefined) {
    migrateObject(obj.content);
  }
}

function captureChangesF(h: Handle, { objId, ops }: { objId: string; ops: any }): void {
  withEditable(h, objId, obj => {
    obj.localChanges = obj.localChanges.concat(ops);
    initContent(obj);
  });
}

const captureChangesA = (objId: ObjId, ops: Operation[]) =>
  mkAction(`captureChanges(${objId},${ops.length})`, { objId, ops }, captureChangesF);

function mkChangeListener<T>(h: Handle, objId: ObjId): ChangeCallback {
  const save = debounce(saveEditable, 1500);

  return function onChange(changes: Change<any>[]): void {
    modifyHandle(h, captureChangesA(objId, changes.map(changeOperation)));
    save(h, objId);
  };
}

// --------------------------------------------------------------------------
// prepareLocalChanges

function prepareLocalChangesF(h: Handle, objId: ObjId) {
  withEditable(h, objId, obj => {
    obj.submittedChanges = obj.localChanges;
    obj.localChanges = [];
  });
}

const prepareLocalChangesA = (objId: ObjId) => mkAction(`prepareLocalChanges(${objId})`, objId, prepareLocalChangesF);

// --------------------------------------------------------------------------
// applyServerResponse

function applyServerResponseF(h: Handle, { objId, res, body }: { objId: string; res: any; body: any }): void {
  withEditable(h, objId, obj => {
    if (obj.networkRequest === res.networkRequest) {
      obj.networkRequest = undefined;
    }

    // Clear out any traces that we've submitted changes to the
    // server.
    obj.submittedChanges = [];

    applyPatches(obj, [].concat(body.previousPatches, body.resultingPatches));
    initContent(obj);
  });
}

const applyServerResponseA = (objId: ObjId, res: { networkRequest: NetworkRequest; res: any }, body: any) =>
  mkAction(`applyServerResponse(${objId})`, { objId, res, body }, applyServerResponseF);

// --------------------------------------------------------------------------
// restoreLocalChanges

function restoreLocalChangesF(h: Handle, objId: ObjId) {
  withEditable(h, objId, obj => {
    obj.localChanges = obj.submittedChanges.concat(obj.localChanges);
    obj.submittedChanges = [];
  });
}

const restoreLocalChangesA = (objId: ObjId) => mkAction(`restoreLocalChanges(${objId})`, objId, restoreLocalChangesF);

async function saveEditable(h: Handle, objId: ObjId): Promise<void> {
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
  modifyHandle(h, prepareLocalChangesA(objId));

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

    modifyHandle(h, applyServerResponseA(objId, res, body));

    // See if we have any more local changes which we need to save.
    await saveEditable(h, objId);
  } catch (err) {
    // The server would presumably respond with changes which
    // were submitted before us, and we'd have to rebase our
    // changes on top of that.

    modifyHandle(h, restoreLocalChangesA(objId));
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
