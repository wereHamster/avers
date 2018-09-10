import { immutableClone } from "../shared";
import { ObjId, Handle, Editable, Static, Ephemeral, Action } from "./types";

export const aversNamespace = Symbol("aversNamespace");

export function endpointUrl(h: Handle, path: string): string {
  return h.config.apiHost + path;
}

export function modifyHandle<T>(h: Handle, { applyF, payload }: Action<T>): void {
  applyF(h, payload);
  startNextGeneration(h);
}

export function startNextGeneration(h: Handle): void {
  h.generationNumber++;

  for (const f of h.generationChangeCallbacks) {
    f();
  }
}

export function entityLabel(entity: string | Static<any> | Ephemeral<any>): string {
  if (typeof entity === "string") {
    return `Editable(${entity})`;
  } else if (entity instanceof Static) {
    return `Static(${entity.ns.toString()},${entity.key})`;
  } else if (entity instanceof Ephemeral) {
    return `Ephemeral(${entity.ns.toString()},${entity.key})`;
  } else {
    return ""; // XXX: EXHAUSTIVE
  }
}

export function withEditable<T>(h: Handle, objId: ObjId, f: (obj: Editable<T>) => void): void {
  const obj = h.editableCache.get(objId);
  if (obj) {
    applyEditableChanges(h, obj, f);
  }
}

// applyEditableChanges
// -----------------------------------------------------------------------
//
// Create a copy of the given 'Editable', apply the function on it, and insert
// the new copy into the cache, overwriting the previous object.

export function applyEditableChanges<T>(h: Handle, obj: Editable<T>, f: (obj: Editable<T>) => void): void {
  h.editableCache.set(obj.objectId, immutableClone<Editable<T>>(Editable, obj, f));
}
