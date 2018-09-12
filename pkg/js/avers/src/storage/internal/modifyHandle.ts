import { Handle, Action } from "../types";
import { startNextGeneration } from "../api/startNextGeneration";

export function modifyHandle<T>(h: Handle, { applyF, payload }: Action<T>): void {
  applyF(h, payload);
  startNextGeneration(h);
}
