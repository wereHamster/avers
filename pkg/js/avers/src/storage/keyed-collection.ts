import { ObjectCollection } from "./collection.js";
import { resetObjectCollection } from "./api.js";
import { Handle } from "./types.js";

export class KeyedObjectCollection<T> {
  cache = new Map<string, ObjectCollection>();

  constructor(public h: Handle, public keyFn: (key: T) => string) {}

  get(keyInput: T): ObjectCollection {
    const key = this.keyFn(keyInput);
    let collection = this.cache.get(key);

    if (!collection) {
      collection = new ObjectCollection(this.h, key);
      this.cache.set(key, collection);
    }

    return collection;
  }
}

export function resetKeyedObjectCollection<T>(kc: KeyedObjectCollection<T>): void {
  kc.cache.forEach(resetObjectCollection);
}
