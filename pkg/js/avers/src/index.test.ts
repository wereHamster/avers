import assert from "node:assert/strict";
import { describe, it } from "node:test";
import * as Avers from "./index.js";

class Sentinel {}
const sentinel: any = new Sentinel();

const testNamespace = Symbol("testNamespace");

async function flushChanges() {
  return new Promise((resolve) => setTimeout(resolve, 1));
}

class Author {
  firstName!: string;
  lastName!: string;
}

const jsonAuthor = {
  firstName: "Tomas",
  lastName: "Carnecky",
};

Avers.definePrimitive(Author, "firstName", "John");
Avers.definePrimitive(Author, "lastName", "Doe");

const unknownAuthor = Avers.mk(Author, {
  firstName: "John",
  lastName: "Doe",
});

class Book {
  title!: string;
  author!: Author;
  tags!: string[];
}

const jsonBook = {
  title: "Game of Thrones",
  author: jsonAuthor,
  tags: ["violent", "fantasy"],
};

const jsonBookWithId = {
  id: "some-random-id",
  title: "Game of Thrones",
  author: jsonAuthor,
  tags: ["violent", "fantasy"],
};

Avers.definePrimitive(Book, "title", undefined);
Avers.defineObject(Book, "author", Author, unknownAuthor);
Avers.defineCollection(Book, "tags", String);

class Magazine {
  title!: string;
  publisher!: string;
}

/*
var jsonMagazine = {
    title: 'Vouge',
    publisher: 'Condé Nast'
};
*/

Avers.definePrimitive(Magazine, "title", undefined);
Avers.definePrimitive(Magazine, "publisher", undefined);

class Diary {}
Avers.declareConstant(Diary);

class Item {
  id!: string;
  content!: Book | Magazine | Diary;
}

const jsonItem = {
  type: "book",
  content: jsonBook,
};

const jsonBookItemWithId = {
  id: "some-random-id",
  type: "book",
  content: jsonBook,
};

const def = Avers.mk(Book, jsonBook);
Avers.defineVariant(Item, "content", "type", { book: Book, magazine: Magazine, diary: Diary }, def);

class NullableTest {
  obj!: Diary;
  variant!: Book | Magazine;
}

Avers.defineObject(NullableTest, "obj", Diary);
Avers.defineVariant(NullableTest, "variant", "type", { book: Book, magazine: Magazine });

class Library {
  items!: Avers.Collection<Item>;
}

Avers.defineCollection(Library, "items", Item);

function now(): number {
  return Date.now();
}

function mkHandle(json: any): Avers.Handle {
  const fetch = (): Promise<any> => {
    return Promise.resolve({
      status: 200,
      json: () => {
        return Promise.resolve(json);
      },
    });
  };

  function createWebSocket(): any {
    return {
      addEventListener() {
        // EMPTY
      },
      send() {
        // EMPTY
      },
    };
  }

  const infoTable = new Map<string, { new (): any }>();
  infoTable.set("library", Library);
  infoTable.set("book", Book);

  return new Avers.Handle({ apiHost: "/api", fetch, createWebSocket, now, infoTable });
}

function mkObjectCollection() {
  const h = mkHandle(["one", "two"]);
  return new Avers.ObjectCollection(h, "/test");
}

const libraryObjectResponse = {
  type: "library",
  id: "id",
  createdAt: new Date().toISOString(),
  createdBy: "me",
  content: {},
};

// const bookObjectResponse = {
//   type: "book",
//   id: "id",
//   createdAt: new Date().toISOString(),
//   createdBy: "me",
//   content: jsonBook,
// };

function unresolvedPromiseF(): Promise<any> {
  return new Promise(() => {
    /* empty */
  });
}

describe("Avers.parseJSON", () => {
  it("should create a new object from json", () => {
    const book = Avers.parseJSON(Book, jsonBook);
    assert.strictEqual("Game of Thrones", book.title);
    assert.strictEqual("Tomas", book.author.firstName);
    assert.strictEqual("Carnecky", book.author.lastName);
  });

  it("should accept an empty JSON if the fields have a default", () => {
    const author = Avers.parseJSON(Author, {});
    assert.strictEqual(author.firstName, undefined as any);
    assert.strictEqual(author.lastName, undefined as any);
  });

  it("should instantiate plain classes in variant properties", () => {
    const item = Avers.parseJSON(Item, { type: "diary", content: {} });
    assert.ok(item.content instanceof Diary);
  });
});

describe("Avers.updateObject", () => {
  it("Avers.updateObject should update an existing object", () => {
    const book = new Book();
    Avers.updateObject(book, jsonBook);
    assert.strictEqual("Game of Thrones", book.title);
    assert.strictEqual("Tomas", book.author.firstName);
    assert.strictEqual("Carnecky", book.author.lastName);
  });
});

describe("Avers.toJSON", () => {
  function runTest(x: any, json: any): void {
    assert.deepEqual(JSON.parse(JSON.stringify(Avers.toJSON(x))), JSON.parse(JSON.stringify(json)));
    assert.doesNotThrow(() => {
      JSON.stringify(Avers.toJSON(x));
    });
  }

  it("should handle primitive types", () => {
    [null, 42, "string"].forEach((x) => {
      runTest(x, x);
    });
  });
  it("should handle objects", () => {
    runTest(Avers.parseJSON(Book, jsonBook), jsonBook);
  });
  it("should handle variants", () => {
    const json = { type: "book", content: jsonBook };
    runTest(Avers.parseJSON(Item, json), json);
  });
  it("should handle variant properties with plain constructors", () => {
    const json = { type: "diary", content: {} };
    runTest(Avers.parseJSON(Item, json), json);
  });
  it("should handle collections", () => {
    const library = Avers.mk(Library, {});
    library.items.push(Avers.parseJSON(Item, jsonBookItemWithId));
    runTest(library.items, [jsonBookItemWithId]);
  });
});

describe("Change event propagation", () => {
  // This timeout is very conservative;
  // this.timeout(500);

  function waitForChange<T>(obj: T, expectedPath: string): Promise<void> {
    return new Promise((resolve) => {
      Avers.attachChangeListener(obj, function changeCallback(changes) {
        for (const change of changes) {
          if (change.path === expectedPath) {
            Avers.detachChangeListener(obj, changeCallback);
            resolve();
          }
        }
      });
    });
  }

  it("should deliver changes of primitive values on the root object", async () => {
    const book = Avers.parseJSON(Book, jsonBook);

    const p = waitForChange(book, "title");
    book.title = "GAME OF THRONES";
    await p;
  });

  it("should deliver changes of embedded objects", async () => {
    const book = Avers.parseJSON(Book, jsonBook);

    const p = waitForChange(book, "author.firstName");
    book.author.firstName = "TOMAS";
    await p;
  });

  it("should deliver changes inside variant properties", async () => {
    const item = Avers.mk(Item, jsonItem);

    const p = waitForChange(item, "content.author.firstName");
    (item.content as Book).author.firstName = "TOMAS";
    await p;
  });

  it("should deliver changes when adding elments to a collection", async () => {
    const library = Avers.mk(Library, {});

    const p = waitForChange(library, "items");
    library.items.push(Avers.parseJSON(Item, jsonItem));
    await p;
  });

  it("should deliver multiple changes done during the same microtask", async () => {
    const book = Avers.parseJSON(Book, jsonBook);

    const p1 = waitForChange(book, "title");
    const p2 = waitForChange(book, "author.firstName");

    book.title = "GAME OF THRONES";
    book.author.firstName = "TOMAS";

    await Promise.all([p1, p2]);
  });
});

describe("Avers.resolvePath", () => {
  it("should resolve in a simple object", () => {
    const book = Avers.parseJSON(Book, jsonBook);
    assert.strictEqual("Game of Thrones", Avers.resolvePath(book, "title") as any);
  });
  it("should resolve an empty string to the object itself", () => {
    const book = Avers.parseJSON(Book, jsonBook);
    assert.strictEqual("Game of Thrones", Avers.resolvePath(book.title, "") as any);
  });
  it("should resolve nested objects", () => {
    const book = Avers.parseJSON(Book, jsonBook);
    assert.strictEqual("Tomas", Avers.resolvePath(book, "author.firstName") as any);
  });
  it("should resolve across arrays", () => {
    const item = Avers.parseJSON(Item, jsonBookItemWithId),
      library = Avers.mk(Library, {});

    library.items.push(item);

    const id = Avers.itemId(library.items, item);
    const path = `items.${id}.content.author.firstName`;

    assert.strictEqual("Tomas", Avers.resolvePath(library, path) as any);
  });
  it("should return undefined if the path can not be resolved", () => {
    assert.strictEqual(Avers.resolvePath({}, "array.0.deep.key"), undefined);
  });
  it("should ignore properties that are not registered", () => {
    const book = Avers.parseJSON(Book, jsonBook);
    (book.author as any).something = "42";
    assert.strictEqual(Avers.resolvePath(book, "author.something"), undefined);
  });
  it("should ignore array indices out of bounds", () => {
    const library = new Library();
    assert.strictEqual(Avers.resolvePath(library.items, "1"), undefined);
  });
  it("should ignore properties on arrays", () => {
    const library = Avers.mk(Library, {});

    (library.items as any).something = "42";
    assert.strictEqual(Avers.resolvePath(library.items, "something"), undefined);
  });
});

describe("Avers.applyOperation", () => {
  function run(op: Avers.Operation, f: (a: Book, b: Book) => void): void {
    const orig = Avers.mk(Book, jsonBook),
      copy = Avers.applyOperation(orig, op.path, op);

    f(orig, copy);
  }

  describe("set", () => {
    function mkOp(path: string, value: any): Avers.Operation {
      return { type: "set", path, value };
    }

    it("should return a copy of the object if some property was changed", () => {
      run(mkOp("title", "A Song of Ice and Fire"), (orig, copy) => {
        assert.notStrictEqual(orig, copy);
      });
    });
    // it("should return a the same object if no changes were needed", function () {
    //   run(mkOp("title", jsonBook.title), (orig, copy) => {
    //     assert.strictEqual(orig, copy);
    //   });
    // });
  });
  describe("splice", () => {
    function mkOp(path: string, index: number, remove: number, insert: any[]): Avers.Operation {
      return { type: "splice", path, index, remove, insert };
    }

    it("should return a copy of the object if some property was changed", () => {
      const lib = Avers.mk(Library, {});
      const copy = Avers.applyOperation(lib, "items", mkOp("items", 0, 0, [jsonBookItemWithId]));

      assert.notStrictEqual(lib, copy);
    });
    // it("should return a the same object if no changes were needed", function () {
    //   const lib = Avers.mk(Library, {});
    //   const copy = Avers.applyOperation(lib, "items", mkOp("items", 0, 0, []));

    //   assert.strictEqual(lib, copy);
    // });
  });
});

describe("Avers.itemId", () => {
  it("should return undefined until changes have been delivered", () => {
    const item = Avers.parseJSON(Item, jsonItem),
      library = Avers.mk(Library, {});

    library.items.push(item);
    assert.strictEqual(Avers.itemId(library.items, item), undefined as any);
  });
  it("should return the item id when the item has one set", () => {
    const item = Avers.parseJSON(Item, jsonBookItemWithId),
      library = Avers.mk(Library, {});

    library.items.push(item);
    assert.strictEqual(Avers.itemId(library.items, item), jsonBookItemWithId.id);
  });
});

describe("Avers.clone", () => {
  it("should clone primitive values", () => {
    assert.strictEqual("str", Avers.clone("str"));
  });
  it("should clone Avers objects", () => {
    const book = Avers.parseJSON(Book, jsonBook);
    const clone = Avers.clone(book);

    assert.notStrictEqual(book, clone);
    assert.deepEqual(JSON.parse(JSON.stringify(Avers.toJSON(book))), JSON.parse(JSON.stringify(Avers.toJSON(clone))));
  });
  it("should clone collections", () => {
    const item = Avers.parseJSON(Item, jsonItem),
      library = Avers.mk(Library, {});

    library.items.push(item);
    const clone = Avers.clone(library.items);
    assert.deepEqual(
      JSON.parse(JSON.stringify(Avers.toJSON(library.items))),
      JSON.parse(JSON.stringify(Avers.toJSON(clone))),
    );
  });
});

describe("Avers.migrateObject", () => {
  it("should set primitive properties to their default value", () => {
    const author = Avers.parseJSON(Author, {});
    Avers.migrateObject(author);
    assert.strictEqual("John", author.firstName);
  });
  it("should initialize objects with their default value", () => {
    const book = Avers.parseJSON(Book, {});
    Avers.migrateObject(book);
    assert.ok(book.author instanceof Author);
  });
  it("should not initialize object properties without a default value", () => {
    const nt = Avers.parseJSON(NullableTest, {});
    Avers.migrateObject(nt);
    assert.ok(!nt.obj);
  });
  it("should not initialize variant properties without a default value", () => {
    const nt = Avers.parseJSON(NullableTest, {});
    Avers.migrateObject(nt);
    assert.ok(!nt.variant);
  });
  it("should initialize variant properties with a default value", () => {
    const item = Avers.parseJSON(Item, {});
    Avers.migrateObject(item);
    assert.ok(item.content instanceof Book);
  });
  it("should initialize collections to an empty array", () => {
    const library = Avers.parseJSON(Library, {});
    Avers.migrateObject(library);
    assert.ok(Array.isArray(library.items));
  });
});

describe("Avers.mk", () => {
  it("should create and migrate the object", () => {
    const author = Avers.mk(Author, {});
    assert.strictEqual("John", author.firstName);
  });

  it("should flush all changes", () => {
    let author = Avers.mk(Author, jsonAuthor),
      allChanges: Avers.Change<any>[] = [];

    Avers.attachChangeListener(author, (changes) => {
      allChanges = allChanges.concat(changes);
    });

    author.firstName = "Jane";
    assert.strictEqual(allChanges.length, 1);
  });
});

describe("Avers.lookupItem", () => {
  it("should find the item in the collection", () => {
    const library = Avers.mk(Library, {});
    library.items.push(Avers.mk(Item, jsonBookItemWithId));
    assert.ok(!!Avers.lookupItem(library.items, jsonBookWithId.id));
  });
  it("should find non-existing in the collection", () => {
    const library = Avers.mk(Library, {});
    library.items.push(Avers.mk(Item, jsonBookItemWithId));
    assert.strictEqual(Avers.lookupItem(library.items, "non-existing-id"), undefined as any);
  });
});

describe("Avers.attachGenerationListener", () => {
  it("should invoke the listener when the data changes", () => {
    const h = mkHandle({});
    Avers.attachGenerationListener(h, () => {});
    Avers.startNextGeneration(h);
  });
});

describe("Avers.lookupEditable", () => {
  it("should return a Computation in Pending status", () => {
    assert.strictEqual(sentinel, Avers.lookupEditable(mkHandle(libraryObjectResponse), "id").get(sentinel));
  });
  it("should resolve to the object after it is loaded", async () => {
    const h = mkHandle(libraryObjectResponse);

    Avers.lookupEditable(h, "id").get(sentinel);
    await flushChanges();

    const obj = Avers.lookupEditable(h, "id").get(sentinel);
    assert.ok(obj.content instanceof Library);
  });
  it("should return a copy when its content changes", () => {
    const h = mkHandle({});

    const obj = Avers.mkEditable(h, "id");
    assert.strictEqual(obj.content, undefined);

    Avers.resolveEditable(h, "id", libraryObjectResponse);
    const copy = Avers.mkEditable(h, "id");

    assert.ok(copy.content instanceof Library);

    assert.notStrictEqual(obj, copy);
  });
});

describe("registering changes on an Editable", () => {
  it("should make a copy of the content", () => {
    const h = mkHandle({});

    Avers.resolveEditable(h, "id", libraryObjectResponse);
    const obj = Avers.mkEditable<Library>(h, "id");

    assert.ok(obj.content instanceof Library);
    obj.content.items.push(Avers.mk(Item, jsonBookItemWithId));

    const copy = Avers.mkEditable<Library>(h, "id");
    assert.ok(copy.content instanceof Library);
    assert.notStrictEqual(obj.content, copy.content);
  });
  // it("should preserve objects not in the change path", function () {
  //   const h = mkHandle({});
  //   Avers.resolveEditable(h, "id", bookObjectResponse);

  //   const obj = Avers.mkEditable<Book>(h, "id");
  //   obj.content.title = "A Song of Ice and Fire";

  //   const copy = Avers.mkEditable<Book>(h, "id");
  //   assert.notEqual(obj.content, copy.content, "content");
  //   assert.strictEqual(obj.content.author, copy.content.author, "content.author");
  // });
});

describe("Avers.ObjectCollection", () => {
  describe("ids", () => {
    it("should return a pending Computation when not fetched yet", () => {
      const col = mkObjectCollection();
      assert.strictEqual(sentinel, col.ids.get(sentinel));
    });
    it("should resolve to the object after it is loaded", async () => {
      const col = mkObjectCollection();
      col.ids.get(sentinel);
      await flushChanges();

      const ids = col.ids.get(sentinel);
      assert.ok(Array.isArray(ids));
      assert.strictEqual(ids.length, 2);
    });
  });
});

describe("Avers.ephemeralValue", () => {
  const e = new Avers.Ephemeral(testNamespace, "test", unresolvedPromiseF);

  it("should return pending when the object is empty", () => {
    const h = mkHandle({});
    assert.strictEqual(sentinel, Avers.ephemeralValue(h, e).get(sentinel));
  });
  it("should return the value when the object is resolved", () => {
    const h = mkHandle({});
    Avers.resolveEphemeral(h, e, 42, h.config.now() + 99);
    assert.strictEqual(42, Avers.ephemeralValue(h, e).get(sentinel) as any);
  });
  it("should return the value even if it is stale", () => {
    const h = mkHandle({});
    Avers.resolveEphemeral(h, e, 42, h.config.now() - 99);
    assert.strictEqual(42, Avers.ephemeralValue(h, e).get(sentinel) as any);
  });
  it("should invoke the fetch function when the value is stale", () => {
    const h = mkHandle({}),
      ne = new Avers.Ephemeral(testNamespace, "test", async () => {
        return { value: {}, expiresAt: 0 };
      });

    Avers.resolveEphemeral(h, ne, 42, h.config.now() - 99);
    Avers.ephemeralValue(h, ne).get(sentinel);
  });
  it("should not invoke the fetch function when the value is fresh", () => {
    const h = mkHandle({}),
      ne = new Avers.Ephemeral(testNamespace, "test", () => {
        assert.fail("fetch of a fresh Ephemeral was invoked");
        throw new Error("fetch of a fresh Ephemeral was invoked");
      });

    Avers.resolveEphemeral(h, ne, 42, h.config.now() + 99);
    Avers.ephemeralValue(h, ne).get(sentinel);
  });
  it("should abort computation when request fails", async () => {
    const h = mkHandle({}),
      ne = new Avers.Ephemeral(testNamespace, "test", async () => {
        throw new Error("…");
      });

    Avers.ephemeralValue(h, ne).get(sentinel);
    await flushChanges();

    assert.strictEqual(Avers.ephemeralValue(h, ne).get(sentinel), sentinel);
  });
});

describe("Avers.staticValue", () => {
  const s = new Avers.Static(testNamespace, "test", unresolvedPromiseF);

  it("should return pending when the object is empty", () => {
    const h = mkHandle({});
    assert.strictEqual(Avers.staticValue(h, s).get(sentinel), sentinel);
  });
  it("should return the value when the object is resolved", () => {
    const h = mkHandle({});
    Avers.resolveStatic(h, s, 42);
    assert.strictEqual(Avers.staticValue(h, s).get(sentinel), 42);
  });
});

describe("Avers.networkRequests", () => {
  it("should return network requests attached to all entities", () => {
    const h = mkHandle({});

    const ep = new Avers.Ephemeral(testNamespace, "test", unresolvedPromiseF);
    const st = new Avers.Static(testNamespace, "test", unresolvedPromiseF);

    Avers.mkEditable(h, "test");
    Avers.loadEditable(h, "test");
    Avers.ephemeralValue(h, ep).get(sentinel);
    Avers.staticValue(h, st).get(sentinel);

    assert.strictEqual(Avers.networkRequests(h).length, 3);
  });
});

describe("Editable", () => {
  it("allows multiple mutations via updateEditable()", () => {
    const h = mkHandle({});
    const objId = "id";

    Avers.resolveEditable(h, objId, {
      type: "book",
      id: objId,
      createdAt: new Date().toISOString(),
      createdBy: "me",
      content: jsonBook,
    });

    Avers.updateEditable<Book>(h, objId, (content) => {
      content.title = "GAME OF THRONES";
      content.author.firstName = "TOMAS";
    });

    const obj = Avers.mkEditable(h, objId);
    assert.strictEqual(obj.localChanges.length, 2);
  });
});
