# Cea

One of the core features of Haskell is immutability. It has greatly simplified the reasoning of Haskell programs, and together with the type system it can capture many bugs before they happen. However, immutability introduces overhead, and while the overhead is sometimes neglibible thanks to the various means of optimisations, there are situations where we want to avoid it.

In Haskell, a common way of achieving mutability is to use `IORef`s. However, `IORef`s are notoriously slow. In fact, they are often slower than pure `StateT`s.

A second way relies on Haskell's Foreign Function Interface (FFI), where we can have access to C pointers. Many common Haskell libraries use these pointers under the hood. However, working with raw, untyped pointers are error-prone, and it is easy to introduce memory leaks and segfaults. While Haskell has a `Storable` type class that offers basic types of pointers, it is only implemented for the basic types, and one have to write much boilerplate if they want to use custom data types.

The library `cea` is an attempt to provide a safe, high-level interface to C pointers. It can derive `Pointable` instances for most custom data types, and provides a variety of accessor functions that allows one to modify a certain field of a data structure without reading the whole data structure. All the type guarantees are checked at compile time, introducing minimum overhead over raw pointers.

Currently, it is still a prototype that can only handle non-recursive product types, but in the future I will extend it to support sum types and arrays to make it more useful.

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

Use `make` to create a pointer, use `load` and `store` to read and write to the pointer. Use `delete` to free the pointer.

```Haskell
main :: IO ()
main = do
  ptr <- make (0 :: Int)
  val <- load ptr
  print val -- 0
  store ptr 1
  val <- load ptr
  print val -- 1
  delete ptr -- free the pointer
```

After calling `delete`, the pointer is no longer valid and should not be used.

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
  delete ptr
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
  print val -- Bar (Foo 1 2) (Foo 3 4)
  delete ptr
```

In the example above, the fields of `Bar`, namely the two `Foo`s, are stored as
pointers, and the fields of `Foo`, namely the two `Int`s, are stored as values.

This is because custom types are considered as "indirect" types, and their
fields are stored as pointers.

Finally, the `delete` function at the end of the `do`-block frees the structure
recursively, so that we do not (and shouldn't) access the pointer of each fields
and free them manually.

Built-in tuples, on the other hand, are treated as direct types, therefore `((1, 2), (3, 4))` is stored in one contiguous block of memory:

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
  delete ptr
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
  delete ptr
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
  delete ptr
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
  delete ptr
```

If the fields has nested fields, we can use `accesses` to acquire a pointer that points to the nested field:

```Haskell
main :: IO ()
main = do
  ptr  <- make (((0, 1), (2, 3)) :: ((Int, Int), (Int, Int)))
  ptr0 <- accesses @[0, 0] ptr
  store ptr0 114
  val  <- load ptr
  print val -- ((114, 1), (2, 3))
  delete ptr
```

The type application `@[n1, n2, ...]` specifies the path to the nested field, namely the n1-th field's n2-th field's ... n-th field. Again the indices start from 0. In particular `accesses @[] ptr` is the same as the orginal poinrter `ptr`.

Similarly, we have the shorthand functions `loadsAt` and `storesAt`.

If we are using a custom data type with selector names, we can also use the selector names themselves to access the fields:

```Haskell
data Foo = Foo { foo1 :: Int, foo2 :: Int }
  deriving (Generic, Show)
  deriving (Pointable) via WithPointable Foo

main :: IO ()
main = do
  ptr  <- make (Foo 0 0)
  storeAt @"foo1" ptr 1
  val  <- load ptr
  print val -- Foo {foo1 = 1, foo2 = 0}
  delete ptr
```
