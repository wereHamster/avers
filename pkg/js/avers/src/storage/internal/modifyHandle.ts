import { Handle, Action } from "../types.js";
import { startNextGeneration } from "../api/startNextGeneration.js";

export function modifyHandle<T>(h: Handle, { applyF, payload }: Action<T>): void {
  applyF(h, payload);
  startNextGeneration(h);
}
