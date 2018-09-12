import { Handle } from "../types";

export function startNextGeneration(h: Handle): void {
  h.generationNumber++;

  for (const f of h.generationChangeCallbacks) {
    f();
  }
}
