Zdefiniuj typ list niepustych, na przykład

``` haskell
type NEL a = (a, [a])
```

oraz odpowiedniki dla niego standardowych funkcji na listach, w tym co najmniej

```
head, tail, init, last, (++), (!!), elem, nub, enumFromTo, enumFromThenTo, 
length, replicate, take, drop, zip, sort, inits, tails
```

Dla każdej funkcji stwórz testy jednostkowe i własności testowane przez QuickCheck.
Zastanów się gdzie można zrobić błąd (np. off-by-one) i jakie testy pomogą go wykryć.

Dla testów mogą się przydać funkcje `fromList` i `toList`, ale staraj się unikać ich w implementacjach.

**Uwagi:**

- w bibliotece istnieje typ `NonEmpty a`, ale nie podglądaj!
- `enumFromTo 1 9 :: [Int] = [1..9]`
- `enumFromThenTo 1 3 9 :: [Int]`

Ładniejsze, acz bardziej pracochłonne rozwiązanie:

``` haskell
data NEL a = a :> [a] deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NEL a) where -- ...
```
(wskazówka: `Gen` należy do klas Functor, Applicative, Monad)

Przykład uruchomienia wszystkich testów QuickCheck w module:

``` haskell
-- Program obowiązkowy
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

-- nasze testy
prop_AddCom3 :: Int -> Int -> Bool
prop_AddCom3 x y = x + y == y + x

prop_Mul1 :: Int -> Property
prop_Mul1 x = (x>0) ==> (2*x > 0)

-- tu dzieje się magia :)
return []  
runTests = $quickCheckAll

main = runTests
```

Efekt:
```
$ cabal run checkAll
=== prop_AddCom3 from checkAll.hs:5 ===
+++ OK, passed 100 tests.

=== prop_Mul1 from checkAll.hs:8 ===
+++ OK, passed 100 tests; 120 discarded.
```

