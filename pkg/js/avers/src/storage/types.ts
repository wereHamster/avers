import Computation from "computation";
import type { ChangeCallback, Operation } from "../core.js";

// ----------------------------------------------------------------------------
// Helpful type synonyms

export type ObjId = string;
export type RevId = number;

export type InfoTable = Map<string, { new (): any }>;

export interface Config {
  /**
   * The hostname where we can reach the Avers API server.
   * Leave out the trailing slash.
   *
   * Example: "//localhost:8000"
   */
  apiHost: string;

  /**
   * API to send network requests.
   * If you use this extension in a web browser, you can pass in the 'fetch' function directly.
   */
  fetch: typeof window.fetch;

  /**
   * Create a WebSocket connection to the given path.
   */
  createWebSocket: (path: string) => WebSocket;

  /**
   * Function which returns the current time.
   * You can use 'Date.now' or 'window.performance.now', depending on how accurate time resolution you need.
   */
  now: () => number;

  /**
   * All object types which the client can parse.
   */
  infoTable: InfoTable;
}

export class Handle {
  /**
   * Incremented everytime something managed by this handle changes.
   */
  generationNumber = 0;

  /**
   * List of callbacks which are invoked when the generation changes.
   */
  generationChangeCallbacks: Set<Function> = new Set();

  editableCache = new Map<string, Editable<any>>();
  staticCache = new Map<symbol, Map<string, StaticE<any>>>();
  ephemeralCache = new Map<symbol, Map<string, EphemeralE<any>>>();

  /**
   * A WebSocket connected to the feed through which the client receives change notifications (eg. new patches).
   */
  feedSocket: undefined | WebSocket = undefined;

  constructor(public config: Config) {}
}

export function newHandle(config: Config): Handle {
  return new Handle(config);
}

/**
 * An 'Editable' is one of the entity types that is managed by the Avers 'Handle'.
 * It's called Editable because its content can be changed (ie. it can be edited) and those changes are automatically synchronized to the server.
 */
export interface Editable<T> {
  objectId: ObjId;

  /**
   * If we have a active network request at the moment (either to fetch the object or saving changes etc) then this describes it.
   * We store the time when the request was started along with the promise.
   * This helps identify long running requests, so the UI can update accordingly.
   *
   * To cancel the request (or rather, its effects on the local state), simply set the field to 'undefined' or start another request.
   * Before a promise applies its effects, it checks whether it is still current, and if not it will simply abort.
   */
  networkRequest: undefined | NetworkRequest;

  lastError: undefined | Error;

  type: string;

  createdAt: Date;

  /**
   * The primary author who created this object.
   */
  createdBy: string;

  /**
   * The RevId as we think is the latest on the server.
   * Local changes are submitted against this RevId.
   */
  revisionId: RevId;

  /**
   * The content of the object at 'revisionId'.
   */
  shadowContent: T;

  content: T;
  changeListener: ChangeCallback;

  submittedChanges: Operation[];
  localChanges: Operation[];
}

export const newEditable = (objectId: ObjId, changeListener: ChangeCallback): Editable<any> =>
  Object.freeze({
    objectId,

    networkRequest: undefined,
    lastError: undefined,

    type: "",

    createdAt: new Date(),

    createdBy: "",
    revisionId: 0,
    shadowContent: undefined,
    content: undefined,
    changeListener,

    submittedChanges: [],
    localChanges: [],
  });

/**
 * A static value which is read-only.
 * Is loaded from the server when required, then cached indefinitely (or until pruned from the cache).
 * The objects are managed by the Avers Handle, they trigger a generation change when they are modified.
 */
export class Static<T> {
  [Symbol.species] = "Static" as const;

  constructor(
    public ns: symbol,
    public key: string,
    public fetch: () => Promise<T>,
  ) {}
}

export interface StaticE<T> {
  networkRequest: undefined | NetworkRequest;
  lastError: undefined | Error;
  value: T;
}

export const emptyStaticE: StaticE<any> = Object.freeze({
  networkRequest: undefined,
  lastError: undefined,
  value: Computation.Pending,
});

/**
 * Ephemeral<T> objects are similar to Static<T> in that they can't be modified, but they can expire and become stale.
 * Once stale they are re-fetched.
 */
export class Ephemeral<T> {
  [Symbol.species] = "Ephemeral" as const;

  constructor(
    public ns: symbol,
    public key: string,
    public fetch: () => Promise<{ value: T; expiresAt: number }>,
  ) {}
}

/**
 * The internal object for an Ephemeral<T> which stores the actual value and keeps track of the network interaction.
 *
 * This is an internal class.
 * It is not exposed through any public API, except through the 'ephemeralCache' in the Handle.
 */
export interface EphemeralE<T> {
  networkRequest: undefined | NetworkRequest;
  lastError: undefined | Error;
  value: T;
  expiresAt: number;
}

export const emptyEphemeralE: EphemeralE<any> = Object.freeze({
  networkRequest: undefined,
  lastError: undefined,
  value: Computation.Pending,
  expiresAt: 0,
});

/**
 * All modifications to data managed by a 'Handle' must be done through an Action.
 * This ensures all modifications can be easily tracked throughout the whole codebase.
 * Simply search for 'mkAction' and you'll find all those places.
 *
 * @see 'modifyHandle'
 */
export type ActionF<T> = (h: Handle, payload: T) => void;

export interface Action<T> {
  label: string;
  payload: T;
  applyF: ActionF<T>;
}

export const mkAction = <T>(label: string, payload: T, applyF: ActionF<T>): Action<T> => ({
  label,
  payload,
  applyF,
});

/**
 * In-flight network request are tracked with this object, which is attached to various entities (Editable, Static, Ephemeral etc.).
 *
 * @see 'runNetworkRequest'
 */
export class NetworkRequest {
  constructor(
    public createdAt: number,
    public promise: Promise<{}>,
  ) {}
}

export class Patch {
  [Symbol.species] = "Patch" as const;

  constructor(
    public objectId: ObjId,
    public revisionId: RevId,
    public authorId: ObjId,
    public createdAt: string,
    public operation: Operation,
  ) {}
}

export function parsePatch(json: any): Patch {
  return new Patch(json.objectId, json.revisionId, json.authorId, json.createdAt, json.operation);
}

/**
 * EntityId
 */
export type EntityId = string | Static<unknown> | Ephemeral<unknown>;
