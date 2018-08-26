export function last<T>(xs: T[]): T {
  return xs[xs.length - 1];
}

export function immutableClone<T>(ctor: new (...args: any[]) => T, x: T, f: (x: T) => void): T {
  let copy = assign(new ctor(), x);
  f(copy);
  return Object.freeze(copy);
}

export function guardStatus(functionName: string, ...statuses: number[]): (res: Response) => Response {
  return res => {
    if (statuses.indexOf(res.status) >= 0) {
      return res;
    } else {
      throw new Error(`${functionName}: response status not one of ${statuses.join(" ")}`);
    }
  };
}

// ---
// Object.assign polyfill

let propIsEnumerable = Object.prototype.propertyIsEnumerable;

function ToObject<T extends Object>(val: any): T {
  if (val == null) {
    throw new TypeError("Object.assign cannot be called with null or undefined");
  } else {
    return Object(val);
  }
}

function ownEnumerableKeys<T>(obj: T): (string | symbol)[] {
  let keys: (string | symbol)[] = Object.getOwnPropertyNames(obj);

  if (Object.getOwnPropertySymbols) {
    keys = keys.concat(Object.getOwnPropertySymbols(obj));
  }

  return keys.filter(key => propIsEnumerable.call(obj, key));
}

export function assign<T>(target: T, ...source: Object[]): T {
  let to = ToObject<T>(target);

  for (let s = 1; s < arguments.length; s++) {
    let from = arguments[s],
      keys = ownEnumerableKeys(Object(from));

    for (let i = 0; i < keys.length; i++) {
      to[keys[i]] = from[keys[i]];
    }
  }

  return to;
}

// ---