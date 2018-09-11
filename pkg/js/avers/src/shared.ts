export function last<T>(xs: T[]): T {
  return xs[xs.length - 1];
}

export function immutableClone<T>(ctor: new (...args: any[]) => T, x: T, f: (x: T) => void): T {
  const copy = Object.assign(new ctor(), x);
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
