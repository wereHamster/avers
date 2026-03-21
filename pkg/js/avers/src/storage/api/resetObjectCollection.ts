import type { ObjectCollection } from "../collection.js";
import { modifyHandle } from "../internal/modifyHandle.js";
import { type Action, type Handle, mkAction } from "../types.js";

const resetObjectCollectionF = (_h: Handle, c: ObjectCollection) => {
  c.fetchedAt = 0;
};

const resetObjectCollectionA = (c: ObjectCollection): Action<ObjectCollection> =>
  mkAction(`resetObjectCollection(${c.collectionName})`, c, resetObjectCollectionF);

export function resetObjectCollection(c: ObjectCollection): void {
  modifyHandle(c.h, resetObjectCollectionA(c));
}
