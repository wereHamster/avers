import { Handle, mkAction, Action } from "../types";
import { modifyHandle } from "../internal/modifyHandle";
import { ObjectCollection } from "../collection";

const resetObjectCollectionF = (_h: Handle, c: ObjectCollection) => {
  c.fetchedAt = 0;
};

const resetObjectCollectionA = (c: ObjectCollection): Action<ObjectCollection> =>
  mkAction(`resetObjectCollection(${c.collectionName})`, c, resetObjectCollectionF);

export function resetObjectCollection(c: ObjectCollection): void {
  modifyHandle(c.h, resetObjectCollectionA(c));
}
