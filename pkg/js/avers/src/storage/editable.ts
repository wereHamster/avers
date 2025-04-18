import Computation from "computation";

import { guardStatus } from "../shared.js";
import { ObjId, Handle, Editable } from "./types.js";
import { startNextGeneration } from "./api/startNextGeneration.js";
import { endpointUrl } from "./internal.js";
import { runNetworkRequest } from "./internal/runNetworkRequest.js";
import { resolveEditable, mkEditable } from "./api.js";

// fetchObject
// -----------------------------------------------------------------------
//
// Fetch the raw JSON of an object from the server.

export async function fetchObject(h: Handle, id: string): Promise<unknown> {
  const url = endpointUrl(h, "/objects/" + id);
  const requestInit: RequestInit = {
    credentials: "include",
    headers: { accept: "application/json" },
  };

  const res = await h.config.fetch(url, requestInit);
  await guardStatus("fetchObject", 200)(res);
  return res.json();
}

export async function createObject(h: Handle, type: string, content: unknown): Promise<string> {
  const url = endpointUrl(h, "/objects");
  const requestInit: RequestInit = {
    credentials: "include",
    method: "POST",
    body: JSON.stringify({ type, content }),
    headers: { accept: "application/json", "content-type": "application/json" },
  };

  const res = await h.config.fetch(url, requestInit);
  await guardStatus("createObject", 200)(res);
  const json = await res.json();
  startNextGeneration(h);
  return json.id;
}

export async function createObjectId(h: Handle, objId: ObjId, type: string, content: unknown): Promise<{}> {
  const url = endpointUrl(h, "/objects/" + objId);
  const requestInit: RequestInit = {
    credentials: "include",
    method: "POST",
    body: JSON.stringify({ type, content }),
    headers: { accept: "application/json", "content-type": "application/json" },
  };

  const res = await h.config.fetch(url, requestInit);
  await res.json();
  startNextGeneration(h);
  return {};
}

export async function deleteObject(h: Handle, id: string): Promise<void> {
  const url = endpointUrl(h, "/objects/" + id);
  await h.config.fetch(url, { credentials: "include", method: "DELETE" });
  startNextGeneration(h);
}

// fetchEditable
// -----------------------------------------------------------------------
//
// Wait until an Editable is Loaded or Failed (the Promise is resolved or
// rejected accordingly). This is useful in asynchronous code where you
// can't use 'Computation' (lookupEditable).

export async function fetchEditable<T>(h: Handle, id: string): Promise<Editable<T>> {
  const obj = mkEditable<T>(h, id);

  if (obj.content !== undefined) {
    return obj;
  } else if (obj.lastError !== undefined) {
    throw obj.lastError;
  } else {
    try {
      await (obj.networkRequest?.promise ?? loadEditable(h, id));
      return fetchEditable(h, id);
    } catch (err) {
      return fetchEditable(h, id);
    }
  }
}

// loadEditable
// -----------------------------------------------------------------------
//
// Fetch an object from the server and initialize the Editable with the
// response.

export async function loadEditable(h: Handle, id: string): Promise<void> {
  const e = mkEditable<unknown>(h, id);
  if (e.shadowContent === undefined && e.networkRequest === undefined) {
    try {
      const res = await runNetworkRequest(h, id, "fetchEditable", fetchObject(h, id));
      resolveEditable(h, id, res);
    } catch (err) {
      // Ignore errors. runNetworkRequest already sets 'lastError'.
    }
  }
}

// lookupEditable
// -----------------------------------------------------------------------
//
// Get an object by its id. This computation is pending until the object
// has been fully loaded.

export function lookupEditable<T>(h: Handle, id: string): Computation<Editable<T>> {
  return new Computation(() => {
    const obj = mkEditable<T>(h, id);
    if (obj.content) {
      return obj;
    } else if (obj.lastError) {
      throw obj.lastError;
    } else {
      loadEditable(h, id);
      return Computation.Pending;
    }
  });
}

// lookupContent
// -----------------------------------------------------------------------
//
// Often you don't need the whole Editable wrapper, but the content inside
// it. This is a convenience function to get just that.

export function lookupContent<T>(h: Handle, id: string): Computation<T> {
  return lookupEditable<T>(h, id).fmap((x) => x.content);
}
