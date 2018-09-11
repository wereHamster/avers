import { parseJSON, migrateObject } from "../../core";
import { ObjId, Handle, Editable, mkAction } from "../types";
import { modifyHandle, applyEditableChanges, initContent } from "../internal";
import { mkEditable } from './mkEditable';

// updateEditable
// -----------------------------------------------------------------------
//
// A non-destructive update of an 'Editable'. The callback is given a copy
// of the original and can set any properties on it. The copy is then
// inserted into the cache.
//
// If the 'Editable' doesn't exist in the cache then it is created.

export function updateEditable<T>(h: Handle, objId: ObjId, f: (obj: Editable<T>) => void): void {
  const obj = mkEditable<T>(h, objId);
  applyEditableChanges<T>(h, obj, f);
}

// resolveEditable
// -----------------------------------------------------------------------
//
// Given a response from the server, initialize an 'Editable' with the data.
//
// Note that this will invalidate any currently running network requests and
// drop any local changes.

function resolveEditableF<T>(h: Handle, { objId, json }: { objId: string; json: any }) {
  // ASSERT objId === json.id

  updateEditable(h, objId, obj => {
    obj.networkRequest = undefined;
    obj.lastError = undefined;

    obj.type = json.type;
    obj.objectId = json.id;
    obj.createdAt = new Date(Date.parse(json.createdAt));
    obj.createdBy = json.createdBy;
    obj.revisionId = json.revisionId || 0;

    const ctor = h.config.infoTable.get(obj.type);
    if (ctor === undefined) {
      throw new Error(`resolveEditable: unknown type ${obj.type}`);
    }
    obj.shadowContent = parseJSON<T>(ctor, json.content);

    obj.submittedChanges = [];
    obj.localChanges = [];

    initContent(obj);
  });
}

const resolveEditableA = (objId: ObjId, json: any) =>
  mkAction(`resolveEditable(${objId})`, { objId, json }, resolveEditableF);

export function resolveEditable(h: Handle, objId: ObjId, json: any): void {
  modifyHandle(h, resolveEditableA(objId, json));

  // The migration must be done outside of the 'modifyHandle' block, because
  // migration may dispatch actions which modify the handle. Even though
  // recursive calls to 'modifyHandle' work fine, we should avoid them.
  //
  // The lookup in the cache can not fail, the object is guaranteed
  // to exist. But we can't encode that in the type system so we should do
  // the check.

  const obj = h.editableCache.get(objId);
  if (obj !== undefined) {
    migrateObject(obj.content);
  }
}
