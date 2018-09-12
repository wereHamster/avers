import { applyOperation } from "../../core";
import { Editable, Patch } from "../types";

// applyPatches
// -----------------------------------------------------------------------------
//
// The given patches MUST have consecutive revisionIds!

export function applyPatches(obj: Editable<unknown>, patches: Patch[]): void {
  if (obj.shadowContent === undefined) {
    // We simply ignore any attempt to apply patches to an Editable which
    // is not resolved.
    //
    // We could store the patches somewhere and attempt to apply them once
    // the Editable is resolved. But that would require careful changes to
    // multiple parts of the code. Given how low the chance is for that to
    // happen and cause problems, we decided to not handle that situation.

    return;
  }

  const applicablePatches = patches.filter(p => p.revisionId > obj.revisionId);
  if (applicablePatches.length === 0 || applicablePatches[0].revisionId !== obj.revisionId + 1) {
    // The first patch is not one that can be applied directly on the
    // Editable. This means there is a gap between the first patche we have
    // and the state of the Editable.
    //
    // We could consider storing the patches somewhere, reload the ones
    // which are missing and then apply them all. But, again, the chance
    // for that to happen is so low that we don't bother.

    return;
  }

  obj.revisionId += applicablePatches.length;
  obj.shadowContent = applicablePatches.reduce((c, patch) => {
    const op = patch.operation;
    return applyOperation(c, op.path, op);
  }, obj.shadowContent);
}

