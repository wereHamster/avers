Avers.js
--------

Avers is a JavaScript library which provides basic functionality for web
applications which need to manage and synchronize objects with a server.


### Motivation

My main goals were:

 - To have a library for the **Model** layer of an application which can be
   fully checked by static type checkers.
 - Reduce boilerplate when parsing JSON into proper JavaScript class instances.
 - Abstract all synchronization between client and server behind an efficient
   API which supports concurrent editing.

Avers is written in **TypeScript**. It is composed of three parts: **Core**,
**Storage** and **Session**.
The **Core** provides a DSL to define object types and their properties. With
   that information Avers can automatically parse JSON into proper class
   instances and track changes you make to these objects.
The **Storage** part provides an API to manage these objects and synchronize
   them with the server.
The **Session** part implements a simple concept of a client session.


### Immutable data structures

While I do like immutable datastructures, the current implementations have some
serious shortcomings. All libraries which use string arrays as cursors (eg.
Immutable.js) are impenetrable for static type checkers. Consider the following
example which has a typo in it. Can you spot it? Well, you can't and neither can
**TypeScript** or **Flow**.

```javascript
let a = new Immutable.Map();
let b = a.setIn(['users',userId,'age'], 42);
```

Avers does partially embrace immutable datastructures, but hides them behind
an imperative API. Because you have to define all object types and their
properties, Avers can
automatically construct cursors which are guaranteed to be correct.

Modifying a property on an Avers object is a destructive operation. But those
changes can be captured and it's up to you what you do with it. The **Storage**
part for example applies the changes to a fresh copy, so to the outside
it looks like the data are immutable.

```javascript
let userE = Avers.lookupEditable(aversH, '956-userid-37').get(undefined);
if (userE) { // Editable may not be loaded yet on the client.
    userE.content.age = 42;
    Avers.deliverChangeRecords(userE.content);

    let x = Avers.lookupEditable(aversH, '956-userid-37').get(undefined);
    assert(userE != x);
}
```


### Fully typed objects all the way down

The library works particularly well in a TypeScript project. When you add full
type annotations to your objects then the compiler can warn you when you try
to access or modify non-existing properties. This works even for arbitrary
deep structures.

```javascript
class Author {
    firstName : string;
    lastName  : string;
}
Avers.definePrimitive(Author, 'firstName', '');
Avers.definePrimitive(Author, 'lastName', '');

class Book {
    title  : string;
    author : Author;
}
Avers.definePrimitive(Book, 'title', '');
Avers.defineObject(Book, 'Author', {});

let book = Avers.mk<Book>(Book, {});
book.title = 'A Song of Ice and Fire';
book.author.firstName  = 'George';
book.author.middleName = 'R.R.';
book.author.lastName   = 'Martin';
// TypeScript error: non-existing property 'middleName' on 'Author'.
```

Object properties can be of four types:

 - Primitive values (string, number, boolean).
 - Variant properties (also sometimes called sum types).
 - Child objects (Avers objects).
 - Collections (arrays of Avers objects).


### Requirements

The implementation makes heavy use of modern web technologies, such as
[Object.observe][object-observe], [Symbol][symbol], [Map][map] and [Set][set].
At this date (5015-07-18) only Chrome supports Object.observe. On other
platforms you'll have to use polyfills.



### Storage

The **Storage** part implements functionality to
manage and synchronize objects with a compliant server (one implementation is
available as a [Haskell library][avers-haskell] library).

All data is managed in a `Handle`. All IO operations are abstracted away and
you have to supply their implementation. The `Handle` expects API compatible
with the whatwg fetch spec for the network interaction. Access to a clock
is also abstracted, so you can supply implementation based on `Date.now`
or `prformance.now`, depending on accuracy requirements.


```javascript
let infoTable = new Map();
infoTable.set('book', Book);

let h = new Avers.Handle
    ( '//api.domain.tld'
    , window.fetch.bind(window)
    , window.performance.now.bind(window.performance)
    , infoTable
    );
```

The handle has a `generationNumber` property. It is a number, incremented
every time any data managed by the handle changes. You can attach a watcher to
be notified whenever it changes, so you know when to re-render the UI.

The module defines the type `Editable<T>` which wraps a top-level
object with metadata needed for synchronization (such as whether it has been
loaded or not, any local changes you have done to the object, or the network
request in flight).


### Integration with React

Use query functions (such as `lookupEditable`, `lookupContent`, `lookupPatch`)
to extract data from the Handle. Rendering in React must be synchronous, so
those functions return a `Computation`. This way you are forced to handle to
case when the object is not loaded yet or has failed loading.

The data is fetched automatically in the background when you first try to
access it. There is no need to trigger anything from the component lifecycle
callbacks (eg. `componentDidMount`).

Never compare the Avers Handle for equality to decide if you need to re-render
a component. Though you can use strict equality comparison on an `Editable`
or its content.


This is a simple React component which shows the summary of a book. It
illustrates how to extract data from the Avers Handle.

```javascript
interface BookSummaryProps {
    aversH : Avers.Handle;
    bookId : string;
}
class BookSummary extends React.Component<BookSummaryProps, {}> {
    render() {
        let { aversH, bookId } = this.props;

        return Avers.lookupEditable<Book>(aversH, bookId).fmap(bookE => {
            var book   = bookE.content
              , author = book.author;

            return (
                <span>
                    {book.title} by {author.firstName} {author.lastName}
                </span>
            );

        ).get(<span>Loading book {bookId}...</span>);
    }
}
```

At the top level of your React application you need to wire everything up. There
is very little boilerplate needed. Note that this minimal example doesn't
include routing or how to store and manage data outside of the Avers Handle.

```javascript
// Create the handle early during the startup.
let aversH = new Avers.Handle(...);

// The callback you want to call whenever the UI needs to be re-rendered. Give
// it the Avers Handle so it can access the data.
function render() {
    React.render(<App aversH={aversH} />, document.body);
}

// Do the initial render on application startup and install a listener on
// the Handle to re-render the UI whenever the data changes.
render();
Avers.attachGenerationListener(aversH, () => {
    render();
});
```





[typescript]: http://www.typescriptlang.org/
[object-observe]: http://www.html5rocks.com/en/tutorials/es7/observe/
[symbol]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol
[map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
[set]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set
[avers-haskell]: https://github.com/wereHamster/avers-haskell/
