Avers
-----

This is the server-side implementation of the Avers storage model. It is
a Haskell library, intended to be used by your application.

The library provides you with everything you need to write your own Avers
server. You can create and patch objects and collections thereof. It has
builtin support for sessions, authentication and managing blobs.

The database is not configurable. Avers is currently hardcoded to use
RethinkDB.

The library has recently been extracted from a larger application. It is rough
at the edges and has many features which are only implemented in simplified
form.



### Usage

Most of the actions run in the `Avers` monad, which wraps `State`, `Except`
and `IO`. To run these actions you first have to create a handle, and then
use `evalAvers` to run them.

To create a handle you need to supply a config object. There you tell Avers
where to find the database, where to report metrics to and what object types
you have.


### Example

This is an incomplete example showing basic usage of the library.

You have to define the objects which you want to manage by Avers. These
objects must be records and have {To,From}{JSON,Datum} instances. It's easiest
to use TemplateHaskell to derive those. Avers comes with TH which derives both
at the same time.

```haskell
data Flight = Red | Blue | Green | Bronze | Black
data Dragon = Dragon
    { dragonFlight :: Flight
    }

$(deriveEncoding defaultOptions ''Flight)
$(deriveEncoding defaultOptions ''Dragon)
```

For each such object you define an `ObjectType`. It tells Avers how to
identify those objects, how to generate Ids and some other metadata.

```haskell
dragonObjectType :: ObjectType Dragon
dragonObjectType = ObjectType
    { otType  = "dragon"
    , otId    = ObjId <$> liftIO (newId 10)
    , otViews = []
    }
```


You can create as many object types as you need. If your application has
user accounts, you'll probably have an 'accountObjectType' as well. Once you
have all object types, you can build the whole config object and allocate
a new handle.

```haskell
let objectTypes =
        [ SomeObjectType dragonObjectType
        -- , SomeObjectType accountObjectType
        ]

let config = Config
        "localhost" RethinkDB.defaultPort
        saveBlob -- IO action to save a blob
        objectTypes
        (\_ _ -> return ()) -- emitMeasurement

h <- newState config
```

Now that we have a handle, we can create objects and save them in the
database.

```haskell
Right objId <- evalAvers h $ do
    createObject
        dragonObjectType
        rootObjId -- createdBy
        (Dragon Bronze)
```

To update an existing object you apply patches to it. Please refer to
Operational Transform (lots of resources available online) to see how exactly
it works. Note that Avers does not implement the full OT, just enough to get
by for the most general cases.

```haskell
void $ evalAvers h $ do
    applyObjectUpdates
        (BaseObjectId objId)
        (RevId 0)
        rootObjId
        [Set "flight" (toJSON Red)]
        False
```

In a web server you'll probably want to lookup the object and send it to the
client encoded as JSON. You can get the latest version, an earlier version, or
individual patches. Here we'll just get the latest version.

```haskell
Right dragon <- evalAvers h $ do
    objectContent (BaseObjectId objId) :: Avers Dragon

replyWithJSON $ toJSON dragon
```
