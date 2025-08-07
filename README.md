# Hustle

A Haskell library for the [KDL](https://github.com/kdl-org/kdl/blob/main/SPEC.md) document language,
featuring a Parser and a Formatter.

## Usage

### Library

This is the source code for the provided `hustle-fmt` program,
it reformats a KDL document received through `stdin`.

```hs
main :: IO ()
main = do
  input <- T.pack <$> getContents
  case parse document "" input of
    Left  e -> putStrLn (errorBundlePretty e)
    Right d -> print d
```

The `KDL` module hence exports `document :: Parser Document`, which you can run
using [Megaparsec](https://hackage.haskell.org/package/megaparsec)'s `parse`.

Moreover, `Document`'s `show` class instance is backed by
[Prettyprinter](https://hackage.haskell.org/package/prettyprinter)'s `Pretty` type class,
this allows for control over the document's layout options and using the various
available rendering backends.

A `Document` is simply a list of `Nodes`s:

```hs
data Node = Node
  { nodeAnn      :: Maybe Identifier
  , nodeName     :: Identifier
  , nodeArgs     :: [Value]
  , nodeProps    :: Map Identifier Value
  , nodeChildren :: [Node]
  }
```

The following is an example of a KDL document and its resulting
Haskell representation:

```kdl
author "Alex Monad" email="alex@example.com" active=true
```

```hs
Document
  { docNodes =
    [ Node
      { nodeAnn      = Nothing
      , nodeName     = Identifier "author"
      , nodeArgs     = [ Value { valueAnn = Nothing
                               , valueExp = StringValue "Alex Monad" } ]
      , nodeProps    = fromList
        [ ( Identifier "active"
          , Value { valueAnn = Nothing
                  , valueExp = BooleanValue True } )
        , ( Identifier "email"
          , Value { valueAnn = Nothing
                  , valueExp = StringValue "alex@example.com" } ) ]
      , nodeChildren = [] } ] }
```

### Building

This is a Cabal project, you can build it using `cabal build`, the test suite is described
in the `test/Spec.hs` file and can be run with `cabal test`.

#### Dependencies

Aside from `Megaparsec` and `Prettyprinter`, this library makes use of:

* [text](https://hackage.haskell.org/package/text): An efficient packed Unicode text type.
* [scientific](https://hackage.haskell.org/package/scientific): Convenience representation of
numbers using scientific notation.
* [containers](https://hackage.haskell.org/package/containers): Provides the `Map` data type used
for representing a node's set of properties.

## Roadmap

As it stands, this library is not anywhere near a battle-tested trusty tool; this is further
aggravated by the ongoing discussions on the [specification](https://github.com/kdl-org/kdl/issues).

* More extensive Unit/Property-based tests.
* Full support for the reserved [Type Annotations](https://github.com/kdl-org/kdl/blob/main/SPEC.md#type-annotation) in the specification.
* Support for the [Query](https://github.com/kdl-org/kdl/blob/main/SPEC.md) and [Schema](https://github.com/kdl-org/kdl/blob/main/SCHEMA-SPEC.md) specifications.

## Contributing

Feel free to open issues and/or pull requests on [fuzzypixelz/hustle](https://github.com/fuzzypixelz/hustle); the kdl-org [Code of Conduct](https://github.com/kdl-org/kdl/blob/main/CODE_OF_CONDUCT.md) applies.

## Acknowledgements

As an uninitiated Haskell programmer, I can happily bear witness to the friendliness of the Haskell community.
I may have asked a bit too many questions on the Libera IRC `#haskell` channel, but the people over there
never failed to deliver crystal clear explanations; thank you.

## License

Hustle is open-source software under the terms of the permissive [MIT License](https://opensource.org/licenses/MIT).
