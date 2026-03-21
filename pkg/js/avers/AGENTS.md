Avers is a TypeScript library for managing mutable, observable application state with a client-server sync protocol.

The library exposes:

- A DSL for defining typed domain object schemas (`definePrimitive`, `defineObject`, `defineCollection`, `defineVariant`)
- A reactive storage layer (`Handle`, `Editable`, `Static`, `Ephemeral`, `ObjectCollection`) backed by a REST API
- A session management module

**Entry point:** `src/index.ts` → re-exports `core.ts`, `storage.ts`, `session.ts`  
**Output:** `dist/` (compiled JavaScript + `.d.ts` declarations)

---

## Build & Tooling Commands

The project has **no npm scripts**. Run tools directly.

### Build

```sh
pnpm exec tsc
```

Compiles `src/**/*.ts` → `dist/`. Always build before running tests (tests run against compiled output).

### Test (all tests)

```sh
pnpm exec ava
```

### Test (single file)

```sh
pnpm exec ava dist/index.test.js
```

### Test (single test by title pattern)

```sh
pnpm exec ava --match "Avers.parseJSON: should create a new object from json"
```

Titles follow the pattern `"<group>: <name>"` as defined by the `describe`/`it` wrappers in `src/index.test.ts`.

### Test (single test by file + line)

```sh
pnpm exec ava "dist/index.test.js:176"
```

### Lint

```sh
biome lint .
```

### Format (check only)

```sh
biome format .
```

### Format (apply changes)

```sh
biome format --write .
```

### Lint + format combined

```sh
biome check .
```

> **Note:** `biome` is installed via Nix (`flake.nix`), not in `node_modules`. Use `direnv` / `nix develop` to get it in `PATH`. The project uses `direnv` with `use flake` in `.envrc`.

---

## Project Structure

```
src/
  index.ts              # Public entry point; re-exports core, storage, session
  index.test.ts         # All tests (single file for the whole library)
  core.ts               # DSL: defineObject, definePrimitive, defineCollection, defineVariant
  storage.ts            # Storage entry point (re-exports storage submodules)
  session.ts            # Session management: signin, signup, signout, restoreSession
  shared.ts             # Small shared utilities: last(), guardStatus()

  storage/
    types.ts            # All types/classes: Handle, Editable, Static, Ephemeral, NetworkRequest, etc.
    api.ts              # Re-exports all public API functions
    internal.ts         # Re-exports internal helpers
    editable.ts         # loadEditable, lookupEditable, fetchEditable, CRUD ops
    collection.ts       # ObjectCollection + fetch logic
    keyed-collection.ts # KeyedObjectCollection (map of ObjectCollections by key)

    api/                # One file per public API function
    internal/           # One file per internal helper

dist/                   # Compiled output (mirrors src/ structure)
```

Each public API function lives in its own file under `storage/api/`. Each internal helper lives in its own file under `storage/internal/`. Barrel files (`api.ts`, `internal.ts`) re-export everything from their subdirectories.

---

## TypeScript Configuration

Key settings from `tsconfig.json`:

| Option                 | Value    | Implication                                                |
| ---------------------- | -------- | ---------------------------------------------------------- |
| `module`               | `node16` | Local imports **must** use `.js` extension in `.ts` source |
| `moduleResolution`     | `node16` | Same as above                                              |
| `moduleDetection`      | `force`  | Every file is a module, even without `import`/`export`     |
| `verbatimModuleSyntax` | `false`  | `import type` is **not** required for type-only imports    |
| `strict`               | `true`   | All strict checks enabled                                  |
| `noUnusedLocals`       | `true`   | Unused variables are compile errors                        |
| `noUnusedParameters`   | `true`   | Unused parameters are compile errors                       |
| `target`               | `es2022` | Modern JS features available                               |

---

## Code Style

### Imports

- **ESM only** — `"type": "module"` in `package.json`
- All local imports use the **`.js` extension** even though source files are `.ts`:
  ```typescript
  import { last } from "./shared.js";
  import { mkEditable } from "./storage/api.js";
  ```
- Third-party packages use bare specifiers:
  ```typescript
  import Computation from "computation";
  ```
- Re-exports use `export * from`:
  ```typescript
  export * from "./core.js";
  ```
- Import organization is **not** auto-sorted (`organizeImports: "off"` in Biome)

### Formatting

- **Indentation:** 2 spaces (never tabs)
- **Line width:** 120 characters
- **Semicolons:** Always
- **Quotes:** Double quotes (`"`) everywhere
- **Trailing commas:** Present in multi-line structures

### Naming Conventions

| Pattern                | Usage                                                                                   |
| ---------------------- | --------------------------------------------------------------------------------------- |
| `camelCase`            | Variables, function parameters, regular functions                                       |
| `PascalCase`           | Classes, interfaces, type aliases                                                       |
| `SCREAMING_SNAKE_CASE` | Module-level constants (e.g. `MAXAGE`)                                                  |
| `mk` prefix            | Factory/constructor functions: `mkEditable`, `mkHandle`, `mkAction`                     |
| `_` prefix             | Intentionally unused parameters: `_group`, `_newSecret`                                 |
| `lookup*` prefix       | Query functions returning `Computation<T>`: `lookupEditable`, `lookupContent`           |
| `with*` prefix         | Read-then-mutate helpers: `withEditable`, `withStaticE`                                 |
| `*E` suffix            | Internal cache entity types: `StaticE`, `EphemeralE`                                    |
| `*F` suffix            | Raw `ActionF<T>` implementations: `resolveEditableF`, `applyChangeF`                    |
| `*A` suffix            | `Action` factory functions: `resolveEditableA`, `applyChangeA`                          |
| `*Symbol` suffix       | Module-private Symbol keys: `changeListenersSymbol`                                     |
| Verb prefixes          | Side-effecting functions: `attach*`, `detach*`, `resolve*`, `apply*`, `load*`, `fetch*` |

### Types and TypeScript Patterns

- Use `undefined | T` (not `T | undefined`) — `undefined` goes on the left in unions
- Prefix unused parameters with `_` to satisfy `noUnusedParameters`
- Use `!` (definite assignment assertion) for class properties initialized at runtime by Avers:
  ```typescript
  class Author {
    firstName!: string;
    lastName!: string;
  }
  ```
- Group related classes/types in namespaces:
  ```typescript
  export namespace Operation {
    export class Set { ... }
    export class Splice { ... }
  }
  ```
- `any` is permitted where necessary (`noExplicitAny` is disabled in Biome)
- `import type` is optional (both `import` and `import type` are acceptable for type-only imports)

### Error Handling

- Use `catch (err: unknown)` with an explicit cast: `(err as Error).message`
- Network errors are stored on the entity (`obj.lastError`) rather than propagated as rejections; callers check `obj.lastError`
- Guard functions for HTTP status: `guardStatus("functionName", 200)` returns a function that throws on mismatch
- Use `console.error` for unexpected network failures in fire-and-forget async paths
- Use `console.info` for unhandled-but-non-fatal cases in switch statements

---

## Testing Patterns

AVA 7 is the test framework. All tests live in `src/index.test.ts` (compiled to `dist/index.test.js`).

The test file defines thin `describe`/`it` wrappers that compose titles:

```typescript
describe("Avers.parseJSON", () => {
  it("should create a new object from json", t => { ... });
});
// → test title: "Avers.parseJSON: should create a new object from json"
```

Common AVA assertions used:

- `t.is(actual, expected)` — strict equality
- `t.not(a, b)` — strict inequality
- `t.true(expr)` — boolean assertion
- `t.deepEqual(a, b)` — deep equality
- `t.notThrows(fn)` — no exception thrown
- `t.plan(n)` — expect exactly n assertions

New tests should follow the same `describe`/`it` pattern and be added to `src/index.test.ts`.

---

## Biome Lint Rules Disabled

These rules are explicitly disabled; do not add them back without discussion:

- `style/useImportType` — `import type` is optional
- `style/noParameterAssign` — reassigning parameters is allowed
- `style/noUselessElse` — `else` after `return` is permitted
- `suspicious/noExplicitAny` — `any` is allowed
- `suspicious/noConfusingVoidType` — `void` in union types is allowed
- `correctness/noUnusedVariables` — handled by TypeScript compiler instead
- `complexity/useArrowFunction` — regular `function` expressions are allowed
- `complexity/noForEach` — `.forEach()` is allowed alongside `for...of`
- `complexity/noBannedTypes` — `Function`, `Object`, `{}` are allowed

---

## Package Manager

This project uses **pnpm**. Do not use `npm` or `yarn`.

```sh
pnpm install          # install dependencies
pnpm install --frozen-lockfile  # CI-safe install
```
