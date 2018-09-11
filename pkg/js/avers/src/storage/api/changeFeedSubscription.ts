import { Handle } from "../types";
import { applyChange } from "./applyChange";

// changeFeedSubscription
// -----------------------------------------------------------------------------
//
// Change the feed subscription. Opens the websocket if not already open.

export function changeFeedSubscription(h: Handle, json: any): void {
  if (h.feedSocket === undefined) {
    h.feedSocket = h.config.createWebSocket("/feed");

    h.feedSocket.addEventListener("message", msg => {
      try {
        applyChange(h, JSON.parse(msg.data));
      } catch (e) {
        console.error("changeFeedSubscription: error when parsing message", e);
      }
    });
  }

  if (h.feedSocket.readyState === h.feedSocket.OPEN) {
    h.feedSocket.send(JSON.stringify(json));
  } else {
    h.feedSocket.addEventListener("open", function onOpen() {
      if (h.feedSocket !== undefined) {
        h.feedSocket.send(JSON.stringify(json));
        h.feedSocket.removeEventListener("open", onOpen);
      }
    });
  }
}
