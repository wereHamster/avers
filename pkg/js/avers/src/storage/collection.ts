import Computation from "computation";

import { Handle } from "./types";
import { endpointUrl } from "./internal";
import { updateObjectCollection } from "./api";

const MAXAGE = 10 * 1000;

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

function mergeIds(c: ObjectCollection, ids: string[]): void {
  const isChanged =
    c.objectIds === Computation.Pending ||
    (ids.length !== c.objectIds.length || ids.reduce<boolean>((a, id, index) => a || id !== c.objectIds[index], false));

  if (isChanged) {
    updateObjectCollection(c.h, c, ids);
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
