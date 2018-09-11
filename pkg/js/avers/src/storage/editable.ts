import Computation from "computation";

import { guardStatus } from "../shared";
import { ObjId, Handle, Editable } from "./types";
import { endpointUrl, startNextGeneration } from "./internal";
import { runNetworkRequest } from "./internal/runNetworkRequest";
import { resolveEditable, mkEditable } from "./api";

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
          req = nr ? nr.promise : loadEditable(h, id);

        req.then(check).catch(check);
      }
    })();
  });
}

// loadEditable
// -----------------------------------------------------------------------
//
// Fetch an object from the server and initialize the Editable with the
// response.

export async function loadEditable(h: Handle, id: string): Promise<void> {
  const res = await runNetworkRequest(h, id, "fetchEditable", fetchObject(h, id));
  const e = h.editableCache.get(id);

  if (e && e.networkRequest === res.networkRequest) {
    // FIXME: Clearing the networkRequest from the entity maybe should
    // be a separate action, eg. 'finishNetworkRequest'. Currently it's
    // part of resolveEditable. But that function may be called from
    // somebody else, outside of the context of a network request.

    resolveEditable(h, id, res.res);
  }
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
          loadEditable(h, id);
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
