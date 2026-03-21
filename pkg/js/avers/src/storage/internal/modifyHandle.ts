import { startNextGeneration } from "../api/startNextGeneration.js";
import type { Action, Handle } from "../types.js";

export function modifyHandle<T>(h: Handle, { applyF, payload }: Action<T>): void {
  applyF(h, payload);
  startNextGeneration(h);
}
