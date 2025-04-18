import { Handle, mkAction, Action } from "../types.js";
import { modifyHandle } from "../internal/modifyHandle.js";
import { ObjectCollection } from "../collection.js";

interface Payload {
  c: ObjectCollection;
  ids: string[];
}

const updateObjectCollectionF = (_h: Handle, { c, ids }: Payload) => {
  c.objectIds = ids;
};

const updateObjectCollectionA = (c: ObjectCollection, ids: string[]): Action<Payload> =>
  mkAction(`updateObjectCollection(${c.collectionName})`, { c, ids }, updateObjectCollectionF);

export const updateObjectCollection = (h: Handle, c: ObjectCollection, ids: string[]): void => {
  modifyHandle(h, updateObjectCollectionA(c, ids));
};
