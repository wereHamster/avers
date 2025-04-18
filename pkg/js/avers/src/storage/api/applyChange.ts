import { Handle, mkAction, parsePatch } from "../types.js";
import { modifyHandle } from "../internal/modifyHandle.js";
import { changeEditable } from "../internal/changeEditable.js";
import { initContent } from "../internal/initContent.js";
import { applyPatches } from "../internal/applyPatches.js";

function applyChangeF(h: Handle, change: { type: string; content: any }): void {
  const { type, content } = change;

  if (type === "patch") {
    const patch = parsePatch(content);
    changeEditable(h, patch.objectId, obj => {
      applyPatches(obj, [patch]);
      initContent(obj);
    });
  } else {
    console.info("applyChangeF: Unhandled type: " + type);
  }
}

const applyChangeA = (change: { type: string; content: any }) =>
  mkAction(`applyChange(${change.type})`, change, applyChangeF);

export const applyChange = (h: Handle, change: { type: string; content: any }): void => {
  modifyHandle(h, applyChangeA(change));
};
