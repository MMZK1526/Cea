# Cea

One of the core features of Haskell is immutability. It has greatly simplified the reasoning of Haskell programs, and together with the type system it can capture many bugs before they happen. However, immutability introduces overhead, and while the overhead is sometimes neglibible thanks to the various means of optimisations, there are situations where we want to avoid it.

In Haskell, a common way of achieving mutability is to use `IORef`s. However, `IORef`s are notoriously slow. In fact, they are often slower than pure `StateT`s.

A second way relies on Haskell's Foreign Function Interface (FFI), where we can have access to C pointers. Many common Haskell libraries use these pointers under the hood. However, working with raw, untyped pointers are error-prone, and it is easy to introduce memory leaks and segfaults. While Haskell has a `Storable` type class that offers basic types of pointers, it is only implemented for the basic types, and one have to write much boilerplate if they want to use custom data types.

The library `cea` is an attempt to provide a safe, high-level interface to C pointers. It can derive `Pointable` instances for most custom data types, and provides a variety of accessor functions that allows one to modify a certain field of a data structure without reading the whole data structure. All the type guarantees are checked at compile time, introducing minimum overhead over raw pointers.

Currently, it is still a prototype that can only handle non-recursive product types and arrays, but in the future I will extend it to support sum types to make it more useful.

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

To make custom data types an instance of `Pointable`, we need to derive `Generic` and `Cea` before we can derive `Pointable`:

```Haskell
data Foo = Foo Int Int
  deriving stock Generic
  deriving anyclass (Cea, Pointable)

data Bar = Bar Foo Foo
  deriving stock Generic
  deriving anyclass (Cea, Pointable)
```

Here `Cea` is a dummy type class that enables the generic derivation of `Pointable`. In most of the cases, we just need to derive `Cea` and `Pointable` for our custom data types, and the compiler will do the rest for us. In the rare occasion where customised `Pointable` instances are needed, we **must not** derive `Cea` as it will automatically derive the generic implementation of `Accessible` for us, which may lead to errors when working with custom `Pointable`.  More details can be found in [this section](#accessor).

```Haskell
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

The `access` function comes from the `Accessible` type class, which is automatically provided for `Cea` and `Pointable` instances. Therefore, if the `Pointable` instance is not derived but an explicit customised implementation, we **must not** derive `Cea` and **must** implement `Accessible` manually. In the vast majority of the cases, of course, the derived instances are sufficient and we don't need to worry about implementing `Accessible`.

```Haskell

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
  deriving stock (Generic, Show)
  deriving anyclass (Cea, Pointable)

main :: IO ()
main = do
  ptr <- make (Foo 0 0)
  storeAt @"foo1" ptr 1
  val <- load ptr
  print val -- Foo {foo1 = 1, foo2 = 0}
  delete ptr
```

## Array

There are two types of `Pointable` arrays, one has compile-time known size, and the other has runtime known size. To use arrays, import `Cea.Array`:

```Haskell
-- Previous imports...
import Cea.Array
```

### Creation, Read & Write

To create an array with compile-time known size, we can use `makeArr`:

```Haskell
main :: IO ()
main = do
  arr <- makeArr @4 (0 :: Int)
  e0  <- readArr' @0 arr
  print e0 -- 0
  writeArr' @0 arr 1
  e0  <- readArr' @0 arr
  print e0 -- 1
  len <- arrLen arr
  print len -- 4
  deleteArr arr
```

`makeArr` takes a type application that specifies the size of the array, and a value that specifies the initial value of the array (the same initial value is used for all elements, similar to `MArray`'s `newArray`).

To read and write to an array, we can use `readArr'` and `writeArr'`, which takes a type application that specifies the index of the element to read/write. If we feed an index that is out of bound, the program will not compile.

The size of the array can be acquired by `arrLen`. In this case it is already known at compile time, but this function works with any array in general.

Finally, we use `deleteArr` to free the array. Note that the array is assumed to take ownership of all its elements, so we do not need to free the elements manually, and we **should not** free the elements manually.

To create an array with runtime known size, we can use `makeArrFromList`:

```Haskell
main :: IO ()
main = do
  arr <- makeArrFromList @Int [0, 1, 2, 3]
  e0  <- readArr 0 arr
  print e0 -- 0
  writeArr 1 arr 1
  e0  <- readArr 1 arr
  print e0 -- 1
  len <- arrLen arr
  print len -- 4
  deleteArr arr
```

Here we create an array directly from a list (or any `Foldable`), and the size of the array is determined by the length of the list. In this case we know that the length is 4, but in general the length cannot be determined at compile time.

To read/write the array, we can use `readArr` and `writeArr`, which takes an `Int` (as opposed to the type application in `readArr'` and `writeArr'`). If we feed an index that is out of bound, the program will throw an error. Note that these functions also work with arrays with compile-time known size; in fact, all functions that work with arrays with runtime known size also work with arrays with compile-time known size, but apparently not *vice versa*.

### Take Snapshots

We can take a "snapshot" of the array an turn it into an immutable `Array` or list using `loadArr` or `loadArrToList`, similar to the `freeze` function for `MArray`s:

```Haskell
-- Assuming the relevant imports are already there
main :: IO ()
main = do
  arr  <- makeArrFromList @Int [0, 1, 2, 3]
  arr' <- loadArr arr
  print $ toList arr' -- [0, 1, 2, 3]
  list <- loadArrToList arr
  print list -- [0, 1, 2, 3]
  deleteArr arr
```

### Access Element Pointers

So far, when we talk about writing to an array, we are overwriting the entire element, but sometimes our elements could represent a more complicated data type, and we only want to modify a sub-field of them. In this case, we can use the `accessArr` function to acquire a pointer to the element, and then use `load` and `store` to read/write to the element (as discussed in [the previous section](#pointable)).

Assuming we have the following data type `Student`:

```Haskell
data Student = Student { name       :: String
                       , age        :: Int
                       , isDeanList :: Bool }
  deriving stock (Generic, Show)
  deriving anyclass (Cea, Pointable)
```

Here's an example of creating an array of two students, then adding them to the Dean's List:

```Haskell
main :: IO ()
main = do
  let bob = Student { name = "Bob", age = 20, isDeanList = False }
      tom = Student { name = "Tom", age = 21, isDeanList = False }
  arr    <- makeArrFromList [bob, tom]
  bobPtr <- accessArr 0 arr
  tomPtr <- accessArr 1 arr
  storeAt @"isDeanList" bobPtr True
  storeAt @"isDeanList" tomPtr True
  list   <- loadArrToList arr
  print $ all isDeanList list -- True
  deleteArr arr
```

For arrays with compile-time known size, we can also use `accessArr'` instead, which takes a type application that specifies the index of the element to access, similar to `readArr'` and `writeArr'`.
