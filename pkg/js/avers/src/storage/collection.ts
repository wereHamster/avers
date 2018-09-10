import Computation from "computation";

import { Handle, mkAction } from "./types";
import { endpointUrl, modifyHandle } from "./internal";

const MAXAGE = 10 * 1000;

// --------------------------------------------------------------------------
// Action: updateObjectCollection

const updateObjectCollectionF = (h: Handle, { c, ids }: { c: ObjectCollection; ids: string[] }) => {
  c.objectIds = ids;
};

const updateObjectCollectionA = (c: ObjectCollection, ids: string[]) =>
  mkAction(`updateObjectCollection(${c.collectionName})`, { c, ids }, updateObjectCollectionF);

// --------------------------------------------------------------------------
// Action: resetObjectCollection

const resetObjectCollectionF = (h: Handle, c: ObjectCollection) => {
  c.fetchedAt = 0;
};

const resetObjectCollectionA = (c: ObjectCollection) =>
  mkAction(`resetObjectCollection(${c.collectionName})`, c, resetObjectCollectionF);

export class ObjectCollection {
  fetchedAt: number;
  url: string;
  objectIds: string[];

  ids: Computation<string[]> = new Computation(() => {
    fetchObjectCollection(this);
    return this.objectIds;
  });

  constructor(public h: Handle, public collectionName: string) {
    this.fetchedAt = 0;
    this.url = endpointUrl(h, "/collection/" + collectionName);
    this.objectIds = Computation.Pending;
  }
}

export function resetObjectCollection(c: ObjectCollection): void {
  modifyHandle(c.h, resetObjectCollectionA(c));
}

function mergeIds(c: ObjectCollection, ids: string[]): void {
  const isChanged =
    c.objectIds === Computation.Pending ||
    ids.length !== c.objectIds.length ||
    ids.reduce((a, id, index) => a || (c.objectIds !== Computation.Pending && id !== c.objectIds[index]), false);

  if (isChanged) {
    modifyHandle(c.h, updateObjectCollectionA(c, ids));
  }
}

async function fetchObjectCollection(c: ObjectCollection): Promise<void> {
  const now = Date.now();
  if (now - c.fetchedAt > MAXAGE) {
    c.fetchedAt = now;

    try {
      const res = await c.h.config.fetch(c.url, {
        credentials: "include",
        headers: { accept: "application/json" }
      });
      mergeIds(c, await res.json());
    } catch (err) {
      console.error("Avers.Collection fetch", err);
    }
  }
}
