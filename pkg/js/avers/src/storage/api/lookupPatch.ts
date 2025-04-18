import Computation from "computation";

import { guardStatus } from "../../shared.js";
import { ObjId, RevId, Handle, Static, Patch, parsePatch } from "../types.js";
import { aversNamespace, endpointUrl } from "../internal.js";
import { staticValue } from "./staticValue.js";

// lookupPatch
// -----------------------------------------------------------------------
//
// Get an patch by its identifier (objectId + revId). This computation is
// pending until the patch has been fetched from the server.

export function lookupPatch(h: Handle, objectId: ObjId, revId: RevId): Computation<Patch> {
  return staticValue(h, mkPatch(h, objectId, revId));
}

async function fetchPatch(h: Handle, objectId: ObjId, revId: RevId): Promise<Patch> {
  const url = endpointUrl(h, `/objects/${objectId}/patches/${revId}`);
  const requestInit: RequestInit = {
    credentials: "include",
    headers: { accept: "application/json" }
  };

  const res = await h.config.fetch(url, requestInit);
  await guardStatus("fetchPatch", 200)(res);

  const json = await res.json();
  return parsePatch(json);
}

function mkPatch(h: Handle, objectId: ObjId, revId: RevId): Static<Patch> {
  const key = `${objectId}@${revId}`;
  return new Static<Patch>(aversNamespace, key, () => fetchPatch(h, objectId, revId));
}
