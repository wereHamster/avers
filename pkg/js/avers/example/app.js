import Avers from "../avers.js";

class Author {}
Avers.definePrimitive(Author, "firstName");
Avers.definePrimitive(Author, "lastName");

class Book {}
Avers.definePrimitive(Book, "title");
Avers.defineObject(Book, "author", Author);

class Library {}
Avers.definePrimitive(Library, "location");
Avers.defineCollection(Library, "books", Book);

const library = Avers.mk(Library, {});
window.library = library;

const changesElement = document.querySelector("#changes");
Avers.attachChangeListener(library, changes => {
  changes.forEach(({ path, record }) => {
    var li = document.createElement("li");
    li.innerText = path + " changed " + JSON.stringify(record);
    changesElement.appendChild(li);
  });
});

Avers.updateObject(library, {
  location: "Europe",
  books: []
});

const book = Avers.parseJSON(Book, {
  title: "The Little Prince",
  author: { firstName: "Antoine", lastName: "Saint-Exupéry" }
});
window.book = book;

library.books.push(book);
