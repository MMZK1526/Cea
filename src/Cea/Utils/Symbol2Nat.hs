-- | This helper module is inspired by the work of Csongor Kiss in his "symbols"
-- package. The whole design can be viewed in his blog post:
-- https://blog.csongor.co.uk/symbol-parsing-haskell/
module Cea.Utils.Symbol2Nat where

import           Data.Proxy
import           Data.Type.Ord
import           GHC.TypeLits

type DigitLits = '["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

type family Digit2Nat (d :: Symbol) :: Nat where
  Digit2Nat "0" = 0
  Digit2Nat "1" = 1
  Digit2Nat "2" = 2
  Digit2Nat "3" = 3
  Digit2Nat "4" = 4
  Digit2Nat "5" = 5
  Digit2Nat "6" = 6
  Digit2Nat "7" = 7
  Digit2Nat "8" = 8
  Digit2Nat "9" = 9

type family PrependDigits (d :: Symbol) (ds :: [Symbol]) :: [Symbol] where
  PrependDigits d '[]        = '[]
  PrependDigits d (d' ': ds) = AppendSymbol d d' ': PrependDigits d ds

type family Lookup (sym :: Symbol)
                   (ps :: [Symbol])
                   (ds :: [Symbol])
                   (cur :: Symbol)
                   (n :: Nat) :: Nat where
  Lookup sym (p ': p' ': ps) ds cur n
    = Lookup2 (Compare p sym) (Compare p' sym) sym p (p' ': ps) ds cur n
  Lookup sym '[d] _ cur n
    = Lookup2 (Compare d sym)
              (Compare (AppendSymbol cur ":") sym)
              sym
              d
              '[]
              '[]
              cur
              n

type family Lookup2 (ol :: Ordering)
                    (or :: Ordering)
                    (sym :: Symbol)
                    (d :: Symbol)
                    (ps :: [Symbol])
                    (ds :: [Symbol])
                    (cur :: Symbol)
                    (n :: Nat) :: Nat where
  Lookup2 'EQ _ _ _ _ (d' ': _) _ n       = 10 * n + Digit2Nat d'
  Lookup2 'LT 'GT sym d _ (d' ': _) cur n
    = Lookup sym (PrependDigits d DigitLits) DigitLits d (10 * n + Digit2Nat d')
  Lookup2 'LT _ sym _ ps (_ ': ds) cur n  = Lookup sym ps ds cur n

type H sym = Lookup sym DigitLits DigitLits "" 0

foo :: Proxy (H "12345")
foo = Proxy
