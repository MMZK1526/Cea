# Cea

TODO: Introduction

## Example
TODO

## Basic Usage

To use `Cea`, you will need to import the following modules and enable the following extensions:

```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

import Cea.Pointer
import Cea.Pointer.Accessor
```

### Pointable

Use `make` to create a pointer, use `load` and `store` to read and write to the pointer.

```Haskell
main :: IO ()
main = do
  ptr <- make (0 :: Int)
  val <- load ptr
  print val -- 0
  store ptr 1
  val <- load ptr
  print val -- 1
```

Note that currently we do not deal with freeing the pointers, and the examples here will contain
memory leaks.

`make` can be used on any type that is an instance of `Pointable`, which is a partial generalisation of `Storable`. It has been implemented on many common data types, for example, tuples:

```Haskell
main :: IO ()
main = do
  ptr <- make (0 :: Int, 0 :: Int)
  val <- load ptr
  print val -- (0, 0)
  store ptr (1, 2)
  val <- load ptr
  print val -- (1, 2)
```

To make custom data types an instance of `Pointable`, we can derive via the wrapper `WithPointable`:

```Haskell
data Foo = Foo Int Int
  deriving (Generic, Show)
  deriving (Pointable) via WithPointable Foo

data Bar = Bar Foo Foo
  deriving (Generic, Show)
  deriving (Pointable) via WithPointable Bar

main :: IO ()
main = do
  ptr <- make (Bar (Foo 0 0) (Foo 0 0))
  val <- load ptr
  print val -- Bar (Foo 0 0) (Foo 0 0)
  store ptr (Bar (Foo 1 2) (Foo 3 4))
  val <- load ptr
```

In the example above, the fields of `Bar`, namely the two `Foo`s, are stored as
pointers, and the fields of `Foo`, namely the two `Int`s, are stored as values.

This is because custom types are considered as "non-primitive" types, and their
fields are stored as pointers.

Built-in tuples, on the other hand, are treated as primitive types, therefore `((1, 2), (3, 4))` is stored in one contiguous block of memory:

```Haskell
main :: IO ()
main = do
  ptr  <- make ((0, 0), (0, 0))
  val  <- load ptr
  print val -- ((0, 0), (0, 0))
  store ptr ((1, 2), (3, 4))
  val  <- load ptr
  print val -- ((1, 2), (3, 4))
  let ptr' = castPtr ptr :: Ptr (Int, Int, Int, Int)
  val' <- load ptr'
  print val' -- (1, 2, 3, 4)
```

Note that we does not support deriving `Pointable` for sum types yet.

### Accessor

Sometimes, we may only want to modify a certain field of a data structure. For example, we may want to increment the first field of a tuple, or the first field of a custom data type:

```Haskell
main :: IO ()
main = do
  ptr          <- make ((0, 0, 0, 0) :: (Int, Int, Int, Int))
  (a, b, c, d) <- load ptr
  store ptr (a + 1, b, c, d)
```

This is not only tedious to write, but also introduces extra overhead since we need to read the whole tuple, modify it, and write it back.

Accessor functions provide ways to modify a certain field of a data structure without reading the whole data structure. For example, we can use `access` to acquire a pointer that points to the
first field of a tuple:

```Haskell
main :: IO ()
main = do
  ptr  <- make ((0, 0, 0, 0) :: (Int, Int, Int, Int))
  ptr0 <- access @0 ptr
  store ptr0 1
  val  <- load ptr
  print val -- (1, 0, 0, 0)
```

The type application `@n` specifies the `n`-th field. Note that the index starts from 0.

`access` is often used directly with a `load` or a `store`. In this case, we can use the shorthand functions `loadAt` and `storeAt`, which reads and writes to the pointer returned by `access`:

```Haskell
main :: IO ()
main = do
  ptr  <- make ((0, 0, 0, 0) :: (Int, Int, Int, Int))
  storeAt @0 ptr 1
  val  <- load ptr
  print val -- (1, 0, 0, 0)
  val0 <- loadAt @0 ptr
  print val0 -- 1
```

If the fields has nested fields, we can use `accesses` to acquire a pointer that points to the nested field:

```Haskell
main :: IO ()
main = do
  ptr  <- make (((0, 1), (2, 3)) :: (Int, Int, Int, Int))
  ptr0 <- accesses @[0, 0] ptr
  store ptr0 114
  val  <- load ptr
  print val -- ((114, 1), (2, 3))
```

The type application `@[n1, n2, ...]` specifies the path to the nested field, namely the n1-th field's n2-th field's ... n-th field. Again the indices start from 0. In particular `accesses @[] ptr` is the same as the orginal poinrter `ptr`.

Similarly, we have the shorthand functions `loadsAt` and `storesAt`.
