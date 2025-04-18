// Avers Storage Extension
// ---------------------------------------------------------------------------
//
// This is an extension for the Avers module which adds functionality to
// manage 'Editable' objects and synchronize changes to a server through
// a HTTP API.
//
// This file depends on the Computation library [1], ES6 Promises [2] and
// Symbol [3].
//
// [1]: https://github.com/wereHamster/computation
// [2]: https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/Promise
// [3]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol

import { attachChangeListener, Change, changeOperation, clone, Operation } from "./core.js";
import { mkEditable } from "./storage/api.js";
import { changeEditable } from "./storage/internal/changeEditable.js";
import { initContent } from "./storage/internal/initContent.js";

export * from "./storage/api.js";
export * from "./storage/collection.js";
export * from "./storage/editable.js";
export * from "./storage/internal.js";
export * from "./storage/keyed-collection.js";
export * from "./storage/api/lookupPatch.js";
export * from "./storage/types.js";

import { Handle, Editable, NetworkRequest } from "./storage/types.js";

// attachGenerationListener
// -----------------------------------------------------------------------
//
// Attach a listener to the handle which will be invoked everytime data
// managed by the handle changes.
//
// If you need to detach the listener later, hang on to the return value
// and pass that to 'detachGenerationListener'.

export function attachGenerationListener(h: Handle, f: () => void): Function {
  const generationChangeCallback = f.bind(null);
  h.generationChangeCallbacks.add(generationChangeCallback);
  return generationChangeCallback;
}

// detachGenerationListener
// -----------------------------------------------------------------------
//
// Detach a generation listener from the handle. The listener is the value
// you get from 'attachGenerationListener'.

export function detachGenerationListener(h: Handle, listener: Function): void {
  h.generationChangeCallbacks.delete(listener);
}

// networkRequests
// -----------------------------------------------------------------------
//
// Array of all network requests which are currently active on the handle.

export function networkRequests(h: Handle): NetworkRequest[] {
  const ret: NetworkRequest[] = [];
  const addNetworkRequest = (nr: undefined | NetworkRequest): void => {
    if (nr !== undefined) {
      ret.push(nr);
    }
  };

  for (const obj of h.editableCache.values()) {
    addNetworkRequest(obj.networkRequest);
  }

  for (const ns of h.staticCache.values()) {
    for (const obj of ns.values()) {
      addNetworkRequest(obj.networkRequest);
    }
  }

  for (const ns of h.ephemeralCache.values()) {
    for (const obj of ns.values()) {
      addNetworkRequest(obj.networkRequest);
    }
  }

  return ret;
}

// localChanges
// -----------------------------------------------------------------------
//
// Array of all objects which have local changes which were not yet
// submitted to the server.

export function localChanges(h: Handle): { obj: Editable<unknown>; changes: Operation[] }[] {
  const ret: { obj: Editable<unknown>; changes: Operation[] }[] = [];

  for (const obj of h.editableCache.values()) {
    if (obj.localChanges.length > 0) {
      ret.push({ obj: obj, changes: obj.localChanges });
    }
  }

  return ret;
}

export function updateEditable<T>(h: Handle, id: string, f: (content: T) => void) {
  const obj = mkEditable<T>(h, id);
  const content = clone(obj.content);

  let localChanges: Change<unknown>[] = [];
  attachChangeListener(content, function onChange(changes: Change<unknown>[]): void {
    localChanges = [...localChanges, ...changes];
  });

  f(content);

  changeEditable(h, id, (obj) => {
    obj.localChanges = obj.localChanges.concat(localChanges.map(changeOperation));
    initContent(obj);
  });
}
