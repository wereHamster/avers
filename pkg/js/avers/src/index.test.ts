import test, { ExecutionContext, Implementation } from "ava";
import * as Avers from "./index";

let group = "";
function it(name: string, f: Implementation<unknown>) {
  test(`${group}: ${name}`, f);
}

function describe(_group: string, f: () => unknown) {
  group = _group;
  f();
  group = "";
}

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

var jsonAuthor = {
  firstName: "Tomas",
  lastName: "Carnecky",
};

Avers.definePrimitive(Author, "firstName", "John");
Avers.definePrimitive(Author, "lastName", "Doe");

var unknownAuthor = Avers.mk(Author, {
  firstName: "John",
  lastName: "Doe",
});

class Book {
  title!: string;
  author!: Author;
  tags!: string[];
}

var jsonBook = {
  title: "Game of Thrones",
  author: jsonAuthor,
  tags: ["violent", "fantasy"],
};

var jsonBookWithId = {
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

var jsonItem = {
  type: "book",
  content: jsonBook,
};

var jsonBookItemWithId = {
  id: "some-random-id",
  type: "book",
  content: jsonBook,
};

var def = Avers.mk(Book, jsonBook);
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
  var h = mkHandle(["one", "two"]);
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
  return new Promise(function () {
    /* empty */
  });
}

describe("Avers.parseJSON", function () {
  it("should create a new object from json", function (t) {
    var book = Avers.parseJSON(Book, jsonBook);
    t.is("Game of Thrones", book.title);
    t.is("Tomas", book.author.firstName);
    t.is("Carnecky", book.author.lastName);
  });

  it("should accept an empty JSON if the fields have a default", function (t) {
    var author = Avers.parseJSON(Author, {});
    t.is(author.firstName, undefined);
    t.is(author.lastName, undefined);
  });

  it("should instantiate plain classes in variant properties", function (t) {
    var item = Avers.parseJSON(Item, { type: "diary", content: {} });
    t.true(item.content instanceof Diary);
  });
});

describe("Avers.updateObject", function () {
  it("Avers.updateObject should update an existing object", function (t) {
    var book = new Book();
    Avers.updateObject(book, jsonBook);
    t.is("Game of Thrones", book.title);
    t.is("Tomas", book.author.firstName);
    t.is("Carnecky", book.author.lastName);
  });
});

describe("Avers.toJSON", function () {
  function runTest(t: ExecutionContext<unknown>, x: any, json: any): void {
    t.deepEqual(Avers.toJSON(x), json);
    t.notThrows(function () {
      JSON.stringify(Avers.toJSON(x));
    });
  }

  it("should handle primitive types", function (t) {
    [null, 42, "string"].forEach((x) => {
      runTest(t, x, x);
    });
  });
  it("should handle objects", function (t) {
    runTest(t, Avers.parseJSON(Book, jsonBook), jsonBook);
  });
  it("should handle variants", function (t) {
    var json = { type: "book", content: jsonBook };
    runTest(t, Avers.parseJSON(Item, json), json);
  });
  it("should handle variant properties with plain constructors", function (t) {
    var json = { type: "diary", content: {} };
    runTest(t, Avers.parseJSON(Item, json), json);
  });
  it("should handle collections", function (t) {
    var library = Avers.mk(Library, {});
    library.items.push(Avers.parseJSON(Item, jsonBookItemWithId));
    runTest(t, library.items, [jsonBookItemWithId]);
  });
});

describe("Change event propagation", function () {
  // This timeout is very conservative;
  // this.timeout(500);

  function expectChangeAtPath<T>(obj: T, expectedPath: string, done: () => void) {
    Avers.attachChangeListener(obj, function changeCallback(changes) {
      changes.forEach(function (change) {
        if (change.path === expectedPath) {
          Avers.detachChangeListener(obj, changeCallback);
          done();
          done = function () {
            // Intentionally left blank to avoid calling the done
            // callback more than once.
          };
        }
      });
    });
  }

  it("should deliver changes of primitive values on the root object", function (t) {
    var book = Avers.parseJSON(Book, jsonBook);

    expectChangeAtPath(book, "title", t.pass);
    book.title = "GAME OF THRONES";
  });

  it("should deliver changes of embedded objects", function (t) {
    var book = Avers.parseJSON(Book, jsonBook);

    expectChangeAtPath(book, "author.firstName", t.pass);
    book.author.firstName = "TOMAS";
  });

  it("should deliver changes inside variant properties", function (t) {
    var item = Avers.mk(Item, jsonItem);

    expectChangeAtPath(item, "content.author.firstName", t.pass);
    (<Book>item.content).author.firstName = "TOMAS";
  });

  it("should deliver changes when adding elments to a collection", function (t) {
    var library = Avers.mk(Library, {});

    expectChangeAtPath(library, "items", t.pass);
    library.items.push(Avers.parseJSON(Item, jsonItem));
  });
});

describe("Avers.resolvePath", function () {
  it("should resolve in a simple object", function (t) {
    var book = Avers.parseJSON(Book, jsonBook);
    t.is("Game of Thrones", Avers.resolvePath(book, "title"));
  });
  it("should resolve an empty string to the object itself", function (t) {
    var book = Avers.parseJSON(Book, jsonBook);
    t.is("Game of Thrones", Avers.resolvePath(book.title, ""));
  });
  it("should resolve nested objects", function (t) {
    var book = Avers.parseJSON(Book, jsonBook);
    t.is("Tomas", Avers.resolvePath(book, "author.firstName"));
  });
  it("should resolve across arrays", function (t) {
    var item = Avers.parseJSON(Item, jsonBookItemWithId),
      library = Avers.mk(Library, {});

    library.items.push(item);

    var id = Avers.itemId(library.items, item);
    var path = "items." + id + ".content.author.firstName";

    t.is("Tomas", Avers.resolvePath(library, path));
  });
  it("should return undefined if the path can not be resolved", function (t) {
    t.is(Avers.resolvePath({}, "array.0.deep.key"), undefined);
  });
  it("should ignore properties that are not registered", function (t) {
    var book = Avers.parseJSON(Book, jsonBook);
    (<any>book.author).something = "42";
    t.is(Avers.resolvePath(book, "author.something"), undefined);
  });
  it("should ignore array indices out of bounds", function (t) {
    var library = new Library();
    t.is(Avers.resolvePath(library.items, "1"), undefined);
  });
  it("should ignore properties on arrays", function (t) {
    var library = Avers.mk(Library, {});

    (<any>library.items).something = "42";
    t.is(Avers.resolvePath(library.items, "something"), undefined);
  });
});

describe("Avers.applyOperation", function () {
  function run(op: Avers.Operation, f: (a: Book, b: Book) => void): void {
    const orig = Avers.mk(Book, jsonBook),
      copy = Avers.applyOperation(orig, op.path, op);

    f(orig, copy);
  }

  describe("set", function () {
    function mkOp(path: string, value: any): Avers.Operation {
      return { type: "set", path, value };
    }

    it("should return a copy of the object if some property was changed", function (t) {
      run(mkOp("title", "A Song of Ice and Fire"), (orig, copy) => {
        t.not(orig, copy);
      });
    });
    // it("should return a the same object if no changes were needed", function () {
    //   run(mkOp("title", jsonBook.title), (orig, copy) => {
    //     t.is(orig, copy);
    //   });
    // });
  });
  describe("splice", function () {
    function mkOp(path: string, index: number, remove: number, insert: any[]): Avers.Operation {
      return { type: "splice", path, index, remove, insert };
    }

    it("should return a copy of the object if some property was changed", function (t) {
      const lib = Avers.mk(Library, {});
      const copy = Avers.applyOperation(lib, "items", mkOp("items", 0, 0, [jsonBookItemWithId]));

      t.not(lib, copy);
    });
    // it("should return a the same object if no changes were needed", function () {
    //   const lib = Avers.mk(Library, {});
    //   const copy = Avers.applyOperation(lib, "items", mkOp("items", 0, 0, []));

    //   t.is(lib, copy);
    // });
  });
});

describe("Avers.itemId", function () {
  it("should return undefined until changes have been delivered", function (t) {
    var item = Avers.parseJSON(Item, jsonItem),
      library = Avers.mk(Library, {});

    library.items.push(item);
    t.is(Avers.itemId(library.items, item), undefined);
  });
  it("should return the item id when the item has one set", function (t) {
    var item = Avers.parseJSON(Item, jsonBookItemWithId),
      library = Avers.mk(Library, {});

    library.items.push(item);
    t.is(Avers.itemId(library.items, item), jsonBookItemWithId.id);
  });
});

describe("Avers.clone", function () {
  it("should clone primitive values", function (t) {
    t.is("str", Avers.clone("str"));
  });
  it("should clone Avers objects", function (t) {
    var book = Avers.parseJSON(Book, jsonBook);
    var clone = Avers.clone(book);

    t.not(book, clone);
    t.deepEqual(Avers.toJSON(book), Avers.toJSON(clone));
  });
  it("should clone collections", function (t) {
    var item = Avers.parseJSON(Item, jsonItem),
      library = Avers.mk(Library, {});

    library.items.push(item);
    var clone = Avers.clone(library.items);
    t.deepEqual(Avers.toJSON(library.items), Avers.toJSON(clone));
  });
});

describe("Avers.migrateObject", function () {
  it("should set primitive properties to their default value", function (t) {
    var author = Avers.parseJSON(Author, {});
    Avers.migrateObject(author);
    t.is("John", author.firstName);
  });
  it("should initialize objects with their default value", function (t) {
    var book = Avers.parseJSON(Book, {});
    Avers.migrateObject(book);
    t.true(book.author instanceof Author);
  });
  it("should not initialize object properties without a default value", function (t) {
    var nt = Avers.parseJSON(NullableTest, {});
    Avers.migrateObject(nt);
    t.true(!nt.obj);
  });
  it("should not initialize variant properties without a default value", function (t) {
    var nt = Avers.parseJSON(NullableTest, {});
    Avers.migrateObject(nt);
    t.true(!nt.variant);
  });
  it("should initialize variant properties with a default value", function (t) {
    var item = Avers.parseJSON(Item, {});
    Avers.migrateObject(item);
    t.true(item.content instanceof Book);
  });
  it("should initialize collections to an empty array", function (t) {
    var library = Avers.parseJSON(Library, {});
    Avers.migrateObject(library);
    t.true(Array.isArray(library.items));
  });
});

describe("Avers.mk", function () {
  it("should create and migrate the object", function (t) {
    var author = Avers.mk(Author, {});
    t.is("John", author.firstName);
  });

  it("should flush all changes", function (t) {
    var author = Avers.mk(Author, jsonAuthor),
      allChanges: Avers.Change<any>[] = [];

    Avers.attachChangeListener(author, (changes) => {
      allChanges = allChanges.concat(changes);
    });

    author.firstName = "Jane";
    t.is(allChanges.length, 1);
  });
});

describe("Avers.lookupItem", function () {
  it("should find the item in the collection", function (t) {
    var library = Avers.mk(Library, {});
    library.items.push(Avers.mk(Item, jsonBookItemWithId));
    t.true(!!Avers.lookupItem(library.items, jsonBookWithId.id));
  });
  it("should find non-existing in the collection", function (t) {
    var library = Avers.mk(Library, {});
    library.items.push(Avers.mk(Item, jsonBookItemWithId));
    t.is(Avers.lookupItem(library.items, "non-existing-id"), undefined);
  });
});

describe("Avers.attachGenerationListener", function () {
  it("should invoke the listener when the data changes", function (t) {
    const h = mkHandle({});
    Avers.attachGenerationListener(h, () => {
      t.pass();
    });
    Avers.startNextGeneration(h);
  });
});

describe("Avers.lookupEditable", function () {
  it("should return a Computation in Pending status", function (t) {
    t.is(sentinel, Avers.lookupEditable(mkHandle(libraryObjectResponse), "id").get(sentinel));
  });
  it("should resolve to the object after it is loaded", async function (t) {
    const h = mkHandle(libraryObjectResponse);

    Avers.lookupEditable(h, "id").get(sentinel);
    await flushChanges();

    const obj = Avers.lookupEditable(h, "id").get(sentinel);
    t.true(obj.content instanceof Library);
  });
  it("should return a copy when its content changes", function (t) {
    const h = mkHandle({});

    const obj = Avers.mkEditable(h, "id");
    t.is(obj.content, undefined);

    Avers.resolveEditable(h, "id", libraryObjectResponse);
    const copy = Avers.mkEditable(h, "id");

    t.true(copy.content instanceof Library);

    t.not(obj, copy);
  });
});

describe("registering changes on an Editable", function () {
  it("should make a copy of the content", function (t) {
    const h = mkHandle({});

    Avers.resolveEditable(h, "id", libraryObjectResponse);
    const obj = Avers.mkEditable<Library>(h, "id");

    t.true(obj.content instanceof Library);
    obj.content.items.push(Avers.mk(Item, jsonBookItemWithId));

    const copy = Avers.mkEditable<Library>(h, "id");
    t.true(copy.content instanceof Library);
    t.not(obj.content, copy.content);
  });
  // it("should preserve objects not in the change path", function () {
  //   const h = mkHandle({});
  //   Avers.resolveEditable(h, "id", bookObjectResponse);

  //   const obj = Avers.mkEditable<Book>(h, "id");
  //   obj.content.title = "A Song of Ice and Fire";

  //   const copy = Avers.mkEditable<Book>(h, "id");
  //   assert.notEqual(obj.content, copy.content, "content");
  //   t.is(obj.content.author, copy.content.author, "content.author");
  // });
});

describe("Avers.ObjectCollection", function () {
  describe("ids", function () {
    it("should return a pending Computation when not fetched yet", function (t) {
      var col = mkObjectCollection();
      t.is(sentinel, col.ids.get(sentinel));
    });
    it("should resolve to the object after it is loaded", async function (t) {
      var col = mkObjectCollection();
      col.ids.get(sentinel);
      await flushChanges();

      const ids = col.ids.get(sentinel);
      t.true(Array.isArray(ids));
      t.is(ids.length, 2);
    });
  });
});

describe("Avers.ephemeralValue", function () {
  const e = new Avers.Ephemeral(testNamespace, "test", unresolvedPromiseF);

  it("should return pending when the object is empty", function (t) {
    const h = mkHandle({});
    t.is(sentinel, Avers.ephemeralValue(h, e).get(sentinel));
  });
  it("should return the value when the object is resolved", function (t) {
    const h = mkHandle({});
    Avers.resolveEphemeral(h, e, 42, h.config.now() + 99);
    t.is(42, Avers.ephemeralValue(h, e).get(sentinel));
  });
  it("should return the value even if it is stale", function (t) {
    const h = mkHandle({});
    Avers.resolveEphemeral(h, e, 42, h.config.now() - 99);
    t.is(42, Avers.ephemeralValue(h, e).get(sentinel));
  });
  it("should invoke the fetch function when the value is stale", function (t) {
    const h = mkHandle({}),
      ne = new Avers.Ephemeral(testNamespace, "test", async () => {
        t.pass();
        return { value: {}, expiresAt: 0 };
      });

    Avers.resolveEphemeral(h, ne, 42, h.config.now() - 99);
    Avers.ephemeralValue(h, ne).get(sentinel);
  });
  it("should not invoke the fetch function when the value is fresh", function (t) {
    const h = mkHandle({}),
      ne = new Avers.Ephemeral(testNamespace, "test", () => {
        t.fail("fetch of a fresh Ephemeral was invoked");
        throw new Error("fetch of a fresh Ephemeral was invoked");
      });

    Avers.resolveEphemeral(h, ne, 42, h.config.now() + 99);
    Avers.ephemeralValue(h, ne).get(sentinel);

    t.pass();
  });
  it("should abort computation when request fails", async function (t) {
    const h = mkHandle({}),
      ne = new Avers.Ephemeral(testNamespace, "test", async () => {
        throw new Error("…");
      });

    Avers.ephemeralValue(h, ne).get(sentinel);
    await flushChanges();

    Avers.ephemeralValue(h, ne)
      .then(
        () => t.pass(),
        () => t.fail()
      )
      .get(sentinel);
  });
});

describe("Avers.staticValue", function () {
  const s = new Avers.Static(testNamespace, "test", unresolvedPromiseF);

  it("should return pending when the object is empty", function (t) {
    const h = mkHandle({});
    t.is(Avers.staticValue(h, s).get(sentinel), sentinel);
  });
  it("should return the value when the object is resolved", function (t) {
    const h = mkHandle({});
    Avers.resolveStatic(h, s, 42);
    t.is(Avers.staticValue(h, s).get(sentinel), 42);
  });
});

describe("Avers.networkRequests", function () {
  it("should return network requests attached to all entities", function (t) {
    const h = mkHandle({});

    const ep = new Avers.Ephemeral(testNamespace, "test", unresolvedPromiseF);
    const st = new Avers.Static(testNamespace, "test", unresolvedPromiseF);

    Avers.mkEditable(h, "test");
    Avers.loadEditable(h, "test");
    Avers.ephemeralValue(h, ep).get(sentinel);
    Avers.staticValue(h, st).get(sentinel);

    t.is(Avers.networkRequests(h).length, 3);
  });
});
