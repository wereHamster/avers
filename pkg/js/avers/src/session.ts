import { Handle, startNextGeneration, endpointUrl } from "./storage";
import { guardStatus } from "./shared";

export enum Transition {
  Restore,
  Signin,
  Signup,
  Signout
}

export class Session {
  constructor(public h: Handle) {}

  objId: undefined | string = undefined;
  transition: undefined | Transition = undefined;
  lastError: undefined | Error = undefined;
}

function runReq(session: Session, path: string, opts: RequestInit): Promise<Response> {
  const url = endpointUrl(session.h, path);
  return session.h.fetch(url, { credentials: "include", ...opts });
}

async function jsonOk<T>(res: Response): Promise<T> {
  if (res.status >= 200 && res.status < 300) {
    return res.json();
  } else {
    throw new Error("Status " + res.status);
  }
}

function beginTransition(session: Session, t: Transition) {
  session.transition = t;
  session.lastError = undefined;

  startNextGeneration(session.h);
}

function finishTransition(session: Session, objId: undefined | string, err: undefined | Error) {
  session.objId = objId;
  session.transition = undefined;
  session.lastError = err;

  startNextGeneration(session.h);
}

// restoreSession
// -----------------------------------------------------------------------
//
// Attempt to determine the session status by contacting the server. The
// server returns either 404 if no session exists, or 200 and includes
// the session ObjId in the response.

export async function restoreSession(session: Session): Promise<void> {
  beginTransition(session, Transition.Restore);

  try {
    const res = await runReq(session, "/session", {});

    // We allow both 200 and 404 to be valid responses and don't set the
    // last error in these cases.

    if (res.status === 200) {
      finishTransition(session, (await res.json()).objId, undefined);
    } else if (res.status === 404) {
      finishTransition(session, undefined, undefined);
    } else {
      throw new Error("Status " + res.status);
    }
  } catch (err) {
    finishTransition(session, undefined, err);
    throw err;
  }
}

// signup
// -----------------------------------------------------------------------
//
// Create a new object on the server against which one can sign in. This
// will usually be an account, if the server has such a concept.
//
// Note that this doesn't automatically authenticate the client. If you want
// to continue with the just created account, you need to sign in with it.
//
// Also, creating a new account will not invalidate an existing session.

export async function signup(session: Session, login: string): Promise<string> {
  const requestInit: RequestInit = {
    method: "POST",
    body: JSON.stringify({ login }),
    headers: { accept: "application/json", "content-type": "application/json" }
  };

  beginTransition(session, Transition.Signup);

  try {
    const res = await runReq(session, "/signup", requestInit);
    const json = await jsonOk<any>(res);

    // Signup doesn't authenticate the client. Therefore we preserve
    // the existing objId in the session.
    finishTransition(session, session.objId, undefined);

    return json.objId;
  } catch (err) {
    finishTransition(session, session.objId, err);
    throw err;
  }
}

// signin
// -----------------------------------------------------------------------
//
// Sign in with an identifier of an object against which one can
// authenticate.

export async function signin(session: Session, login: string, secret: string): Promise<void> {
  const requestInit: RequestInit = {
    method: "POST",
    body: JSON.stringify({ login: login, secret: secret }),
    headers: { accept: "application/json", "content-type": "application/json" }
  };

  beginTransition(session, Transition.Signin);

  try {
    const res = await runReq(session, "/session", requestInit);
    const json = await jsonOk<any>(res);
    finishTransition(session, json.objId, undefined);
  } catch (err) {
    finishTransition(session, undefined, err);
    throw err;
  }
}

// signout
// -----------------------------------------------------------------------
//
// Delete the session and revert the session to the Anynomous state.

export async function signout(session: Session): Promise<void> {
  beginTransition(session, Transition.Signout);

  try {
    const res = await runReq(session, "/session", { method: "DELETE" });
    await guardStatus("signout", 200, 204)(res);
    finishTransition(session, undefined, undefined);
  } catch (err) {
    finishTransition(session, session.objId, err);
    throw err;
  }
}

// changeSecret
// -----------------------------------------------------------------------

export async function changeSecret(h: Handle, newSecret: string): Promise<void> {
  const url = endpointUrl(h, "/secret");
  const res = await h.fetch(url, { credentials: "include", method: "POST" });
  await guardStatus("changeSecret", 200)(res);
}
