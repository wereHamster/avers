import { Handle, mkAction, Action } from "../types";
import { modifyHandle } from "../internal/modifyHandle";
import { ObjectCollection } from "../collection";

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
