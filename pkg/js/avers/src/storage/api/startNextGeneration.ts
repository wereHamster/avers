import { Handle } from "../types.js";

export function startNextGeneration(h: Handle): void {
  h.generationNumber++;

  for (const f of h.generationChangeCallbacks) {
    f();
  }
}
