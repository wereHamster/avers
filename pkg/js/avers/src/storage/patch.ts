import Computation from "computation";

import { guardStatus } from "../shared";
import { Operation } from "../core";
import { Handle, ObjId, RevId, endpointUrl } from "../storage";
import { aversNamespace } from "./internal";
import { Static, staticValue } from "./static";

// Patch
// -----------------------------------------------------------------------
//
// Patches are read-only on the client.

export class Patch {
  [Symbol.species]: "Patch";

  constructor(
    public objectId: ObjId,
    public revisionId: RevId,
    public authorId: ObjId,
    public createdAt: string,
    public operation: Operation
  ) {}
}

export function parsePatch(json: any): Patch {
  return new Patch(json.objectId, json.revisionId, json.authorId, json.createdAt, json.operation);
}

async function fetchPatch(h: Handle, objectId: ObjId, revId: RevId): Promise<Patch> {
  const url = endpointUrl(h, "/objects/" + objectId + "/patches/" + revId);
  const requestInit: RequestInit = {
    credentials: "include",
    headers: { accept: "application/json" }
  };

  const res = await h.config.fetch(url, requestInit);
  await guardStatus("fetchPatch", 200)(res);

  return parsePatch(await res.json());
}

function mkPatch(h: Handle, objectId: ObjId, revId: RevId): Static<Patch> {
  const key = objectId + "@" + revId;
  return new Static<Patch>(aversNamespace, key, () => fetchPatch(h, objectId, revId));
}

// lookupPatch
// -----------------------------------------------------------------------
//
// Get an patch by its identifier (objectId + revId). This computation is
// pending until the patch has been fetched from the server.

export function lookupPatch(h: Handle, objectId: ObjId, revId: RevId): Computation<Patch> {
  return staticValue(h, mkPatch(h, objectId, revId));
}
