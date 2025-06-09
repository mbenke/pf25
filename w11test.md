---
title: Programowanie Funkcyjne
subtitle: Testowanie
author:  Marcin Benke
date: Wykład 11, 26 maja 2025
---


# Testowanie programów w Haskellu
* doctest [github: sol/doctest](https://github.com/sol/doctest)
* HUnit
* Quickcheck
* QuickCheck + doctest

## doctest
Przykłady w dokumentacji mogą być użyte jako testy regresji

``` {.haskell }
module DoctestExamples where
-- | Expect success
-- >>> 2 + 2
-- 4

-- | Expect failure
-- >>> 2 + 2
-- 5

```
(NB dla wykonania w VS Code wystarczy samo `-- >>>`, <br/> natomiast `doctest` wymaga także `-- |` powyżej, <br/>
które jest elementem składni narzędzia dokumentacji [Haddock](https://haskell-haddock.readthedocs.iolatest/))

```
$ cabal install doctest
$ doctest DoctestExamples.hs
### Failure in DoctestExamples.hs:7: expression `2 + 2'
expected: 5
 but got: 4
Examples: 2  Tried: 2  Errors: 0  Failures: 1
```


### Dygresja - Haddock

Haddock (<http://haskell.org/haddock>) jest powszechnie uzywanym narzedziem do tworzenia dokumentacji.

Ciąg `{-|`  or `-- |` (spacja jest istotna!)
rozpoczyna komentarz przekazywany do dokumentcji

``` haskell
-- | The 'square' function squares an integer.
-- It takes one argument, of type 'Int'.
square :: Int -> Int
square x = x * x
```

```
$ haddock --html Square.hs
```


### Przykład
``` {.haskell}
module Fib where

-- | Compute Fibonacci numbers
--
-- Examples:
--
-- >>> fib 10
-- 55
--
-- >>> fib 5
-- 5

fib :: Int -> Integer
fib n = fibs !! n where fibs = 1 : 1 : zipWith (+) fibs (drop 1 fibs)
```

```
-- fib.cabal
cabal-version: 1.12
name:           fib
version:        0.0.0
build-type: Simple

library
  build-depends: base == 4.*
  hs-source-dirs: src
  exposed-modules: Fib
```


```
$ cabal repl --with-compiler=doctest

src/Fib.hs:8: failure in expression `fib 10'
expected: 55
 but got: 89
          ^

Examples: 2  Tried: 1  Errors: 0  Failures: 1
```

## HUnit

Podobnie jak w innych językach, w Haskellu możemy stosowac testy jednostkowe, np.

``` haskell
import Fib
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain $ testGroup "Unit tests" [test1,test2]

test1 = testCase "fib 3 = fib 1 + fib 2" $ fib 3 @?= fib 1 + fib 2
test2 = testCase "fib 10 = 55"           $ fib 10 @?= 55
```

```
$ cabal test
Test suite fib-tests: RUNNING...
Unit tests
  fib 3 = fib 1 + fib 2: OK
  fib 10 = 55:           FAIL
    test/Test.hs:10:
    expected: 55
     but got: 89
    Use -p '/fib 10 = 55/' to rerun this test only.

1 out of 2 tests failed (0.00s)
```


### `cabal test`

``` cabal
-- fib.cabal
cabal-version: 1.12
name:           fib
version:        0.0.0
build-type: Simple

library
  build-depends: base == 4.*
  hs-source-dirs: src
  exposed-modules: Fib
  default-language: Haskell2010

test-suite fib-tests

  type: exitcode-stdio-1.0
  hs-source-dirs: test
  default-language: Haskell2010

  main-is: Test.hs

  build-depends: base == 4.*
               , fib
               , tasty
               , tasty-hunit
```

# Posortujmy listę

~~~~ {.haskell}
mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort pred = go
  where
    go []  = []
    go [x] = [x]
    go xs  = merge (go xs1) (go xs2)
      where (xs1,xs2) = split xs

    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | pred x y  = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

    split []       = ([],[])
    split [x]      = ([x],[])
    split (x:y:zs) = (x:xs,y:ys)
      where (xs,ys) = split zs
~~~~


# Sortowanie: testy jednostkowe


~~~~
sort = mergeSort ((<=) :: Int -> Int -> Bool)

sort [1,2,3,4] == [1,2,3,4]
sort [4,3,2,1] == [1,2,3,4]
sort [1,4,2,3] == [1,2,3,4]
...
~~~~

To powoli staje się nudne...

...ale dzięki typom można to zrobić lepiej; zaraz zobaczymy jak.

# Własności

Oczywista wlasność sortowania:

~~~~ {.haskell}
prop_idempotent = sort . sort == sort
~~~~

nie jest definiowalna — nie umiemy porównywać funkcji.

Możemy porównywać funkcje dla wybranych argumentów:

~~~~ {.haskell}
prop_idempotent xs =
    sort (sort xs) == sort xs
~~~~

Spróbujmy w REPL:

~~~~
*Main> prop_idempotent [3,2,1]
True
~~~~

# Próba automatyzacji

Mozemy to próbowac zautomatyzować:

``` haskell
prop_permute :: ([a] -> Bool) -> [a] -> Bool
prop_permute prop = all prop . permutations

*Main> prop_permute prop_idempotent [1,2,3]
True
*Main> prop_permute prop_idempotent [1..4]
True
*Main> prop_permute prop_idempotent [1..5]
True
*Main> prop_permute prop_idempotent [1..10]
  C-c C-cInterrupted.
```

`prop_permute`  mozemy też zapisać inaczej:

``` haskell
prop_permute' prop xs = forAll (permutations xs) prop
  where forAll = flip all
```

Możemy teraz zaimplementować sprytniejsze `forAll`.

# QuickCheck

* Generowanie wielu testów jednostkowych jest nudne
* Sprawdzenie wszystkkich możliwości nie jest realistyczne (chyba, że dla bardzo małych danych - patrz SmallCheck)
* Pomysł: wygenerujmy odpowiednią próbkę danych

~~~~
*Main> import Test.QuickCheck
*Main Test.QuickCheck> quickCheck prop_idempotent
+++ OK, passed 100 tests.
~~~~


QuickCheck wygenerował 100 losowych list i sprawdził, ze dla nich własność `prop_idempotent` zachodzi

Oczywiście 100 nie jest magiczne:

~~~~
*Main Test.QuickCheck> quickCheckWith stdArgs {maxSuccess = 1000}  prop_idempotent
+++ OK, passed 1000 tests.
~~~~

NB: ponieważ generowanie losowych "wartości polimorficznych" jest trudne, musimy nadać własnościom typ monomorficzny, np.

``` haskell
prop_idempotent :: [Int] -> Bool
```

**Ćwiczenie:** napisz i sprawdź kilka własności sortowania (i innych funkcji)

## Przykład


``` haskell
import Test.QuickCheck

prop_iadd_comm :: Int -> Int -> Bool
prop_iadd_comm a b = a + b == b + a

prop_iadd_assoc :: Int -> Int -> Int -> Bool
prop_iadd_assoc a b c = (a + b) + c  == a + (b + c)

ghci> quickCheck prop_iadd_comm
+++ OK, passed 100 tests.

ghci> quickCheckWith stdArgs {maxSuccess = 1000} prop_iadd_comm
+++ OK, passed 1000 tests.

ghci> quickCheck prop_iadd_assoc
+++ OK, passed 100 tests.
```


* definiujemy własności, które mają być przetestowane - w przybliżeniu: funkcje o typie wyniku `Bool`,
dokładniej  - typu, który należy do klasy `Testable`;
* QuickCheck losuje pewną próbę danych
i sprawdza, czy dla wszystkich własność jest spełniona;
* Istnieją standardowe generatory dla typów wbudowanych, dla własnych typów trzeba je zdefiniować.

## Niespodzianka

``` haskell
import Test.QuickCheck


prop_fadd_comm :: Float -> Float -> Bool
prop_fadd_comm a b = a + b == b + a

prop_fadd_assoc :: Float -> Float -> Float -> Bool
prop_fadd_assoc a b c = (a + b) + c  == a + (b + c)


ghci> quickCheck prop_fadd_com
+++ OK, passed 100 tests.

ghci> quickCheck prop_fadd_assoc
*** Failed! Falsified (after 6 tests and 6 shrinks):
1.0
-1.95
-2.06
```

## forAll, Gen, arbitrary, shrink


``` haskell
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]

forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property
```

- Gen jest monadą - obliczenia generujące ``losowe'' wartości
- shrink mówi jak próbować zmniejszyć kontrprzykład

# Implikacja (testy warunkowe)

Implikacja: sprawdź własność q, pod warunkiem, że dane spełniają p

~~~~ {.haskell}
(==>) :: Testable a => Bool -> a -> Property
True  ==> a = property a
False ==> a = property () -- bad test data

propMul1 :: Int -> Property
propMul1 x = (x>0) ==> (2*x > 0)

propMul2 :: Int -> Int -> Property
propMul2 x y = (x>0) ==> (x*y > 0)
~~~~

~~~~

> check = quickCheck

> check propMul1
OK, passed 100 tests

> check propMul2
Falsifiable, after 0 tests:
2
-2
~~~~



# Problem z implikacją

~~~~
prop_insert1 x xs = ordered (insert x xs)

*Main Test.QuickCheck> quickCheck prop_insert1
*** Failed! Falsifiable (after 6 tests and 7 shrinks):
0
[0,-1]
~~~~

...oczywiście...

~~~~
prop_insert2 x xs = ordered xs ==> ordered (insert x xs)

>>> quickCheck prop_insert2
*** Gave up! Passed only 75 tests; 1000 discarded tests.
~~~~

Prawdopodobieństwo, że losowa lista jest uporządkowana jest nikłe...

# Rozkład przypadków testowych

...a te, które są uporzadkowane, nie są zwykle zbyt przydatne:

~~~~
-- collect :: (Show a, Testable prop) => a -> prop -> Property
-- Attaches a label to a test case. This is used for reporting test case distribution.

prop_insert3 x xs = collect (length xs) $  ordered xs ==> ordered (insert x xs)

>>> quickCheck prop_insert3
*** Gave up! Passed only 37 tests:
51% 0
32% 1
16% 2
~~~~

## Uruchomienie wszystkich testów w module:

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

## Czasem trzeba napisać własny generator

* Zdefiniujmy nowy typ - list uporządkowanych

~~~~
newtype OrderedInts = OrderedInts [Int]

instance Arbitrary OrderedInts where
  arbitrary = OrderedInts . L.sort <$> (arbitrary :: Gen [Int])

prop_insert4 :: Int -> OrderedInts -> Bool
prop_insert4  x (OrderedInts xs) = ordered (insert x xs)

>>> sample (arbitrary:: Gen OrderedInts)
OrderedInts []
OrderedInts [0,0]
OrderedInts [-2,-1,2]
OrderedInts [-4,-2,0,0,2,4]
OrderedInts [-7,-6,-6,-5,-2,-1,5]
OrderedInts [-13,-12,-11,-10,-10,-7,1,1,1,10]
OrderedInts [-13,-10,-7,-5,-2,3,10,10,13]
OrderedInts [-19,-4,26]
OrderedInts [-63,-15,37]
OrderedInts [-122,-53,-47,-43,-21,-19,29,53]
~~~~

<!--
# Function properties

~~~~ {.haskell}
infix 4 ===
(===)  f g x = f x == g x

instance Show(a->b) where
  show f = "<function>"

propCompAssoc f g h = (f . g) . h === f . (g . h)
  where types = [f,g,h::Int->Int]
~~~~
-->


## Czasem trzeba napisać własny generator

``` haskell
data Nat = Zero | S Nat deriving(Eq, Ord, Show)

instance Num Nat where ...

instance Arbitrary Nat where
    arbitrary = do
      (n :: Integer) <- arbitrary
      pure (fromInteger (abs n))

    shrink Zero = []
    shrink (S n) = n : shrink n
```


## Listy nieskończone

Funkcja `cycle` powtarza swój argument "w kółko". Czy mozemy ją przetestować

``` haskell
prop_DoubleCycleBad :: [Int] -> Property
prop_DoubleCycleBad xs =
  not (null xs) ==>
    cycle xs == cycle (xs ++ xs)
```

Tak oczywiście nie można, ale możemy testować skończone prefiksy:

``` haskell
prop_DoubleCycle :: [Int] -> Int -> Property
prop_DoubleCycle xs n =
  not (null xs) && (n>0) ==>
    take n (cycle xs) == take n (cycle (xs ++ xs))

ghci> quickCheck prop_DoubleCycle
+++ OK, passed 100 tests; 137 discarded.
```


## Jak to działa?

Dla uproszczenia spójrzmy na wersję QuickCheck v1

Główne składniki:

~~~~ {.haskell}
quickCheck  :: Testable a => a -> IO ()

class Testable a where
  property :: a -> Property
  -- the Property type will be explained later

instance Testable Bool where...

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f

-- forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property

class Arbitrary a where
  arbitrary   :: Gen a
  shrink      :: a -> [a]

instance Monad Gen where ...
~~~~

`Gen` jest monadą: `Gen t` jest obliczeniem dającym `t` (być może korzystającym z generatorów pseudolosowych)


### Przypomnienie - liczby pseudolosowe

<!-- Haskell Programming s.875; NB deprecated next -->

``` haskell
{- cabal:
    build-depends: base, mtl, random
-}
import System.Random

roll :: StdGen -> (Int, StdGen)
roll = uniformR (1, 6::Int)

pureGen = mkStdGen 1

rollD6 :: Int
rollD6 = fst $ roll pureGen

-- >>> pureGen
-- StdGen {unStdGen = SMGen 12994781566227106604 10451216379200822465}
-- roll pureGen
-- (6,StdGen {unStdGen = SMGen 4999253871718377453 10451216379200822465})
-- rollD6
-- 6
```

na tej kostce zawsze wypada 6


(oczywiście można użyć IO, ale wtedy się już od niego nie uwolnimy)


### Rzuć 3d6

Jak rzucić trzema kostkami?

``` haskell
roll3D6 :: (Int, Int, Int)
roll3D6 = do
  let g0 = pureGen
  let (r1, g1) = roll g0
  let (r2, g2) = roll g1
  let (r3, g3) = roll g2
  (r1, r2, r3)
```

Zamiast jawnie przekazywać generator, możęmy użyć `State`:

``` haskell
rollM :: State StdGen Int
rollM = state roll        -- state :: (StdGen -> (a, StdGen)) -> State StdGen a;  roll :: StdGen -> (Int, StdGen)

roll3D6M' :: State StdGen (Int, Int, Int)
roll3D6M' = do
  r1 <- rollM
  r2 <- rollM
  r3 <- rollM
  return (r1, r2, r3)
```

# Generowanie losowych danych

`Gen` jest monadą: `Gen t` jest obliczeniem dającym `t` (być może korzystającym z generatorów pseudolosowych)

``` haskell
choose :: (Int,Int) -> Gen Int
oneof :: [Gen a] -> Gen a

instance Arbitrary Int where
    arbitrary = choose (-100, 100)

data Colour = Red | Green | Blue
instance Arbitrary Colour where
    arbitrary = oneof [return Red, return Green, return Blue]

instance Arbitrary a => Arbitrary [a] where
    arbitrary = oneof [return [], (:) <$> arbitrary <*> arbitrary]
    -- NB to nie jest najlepszy generator dla list - jaka jest oczekiwana długość listy

generate :: Gen a -> IO a
sample :: Show a => Gen a -> IO ()
```

$$ \sum_{n=0}^\infty {n\over 2^{n+1}} = 1 $$


# Dopasowywanie rozkładu

``` haskell
frequency :: [(Int, Gen a)] -> Gen a  -- weighted distribution

instance Arbitrary a => Arbitrary [a] where
  arbitrary = frequency
    [ (1, return [])
    , (4, liftM2 (:) arbitrary arbitrary)
    ]

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = frequency
        [(1, liftM Leaf arbitrary)
        ,(2, liftM2 Branch arbitrary arbitrary)
        ]

threetrees :: Gen [Tree Int]
threetrees = sequence [arbitrary, arbitrary, arbitrary]
```

jakie jest prawdopodobieństwo, że generowanie drzewa się zatrzyma?

$$ p = {1\over 3} + {2\over 3} p^2 $$

$$ p = 1/2 $$

# Sterowanie rozmiarem danych

``` haskell
-- Gen a bierze parametr rozmiaru oraz StdGen i daje a
newtype Gen a = Gen (Int -> StdGen -> a)

-- | `sized` tworzy generator z rodziny generatorów indeksowanej rozmiarem
sized :: (Int -> Gen a) -> Gen a
sized fgen = Gen (\n r -> let Gen m = fgen n in m n r)

listOf :: Gen a -> Gen [a]
listOf gen = sized $ \n ->
  do k <- choose (0,n)
     vectorOf k gen
```

```
sample (arbitrary :: Gen [Int])
[]
[-1]
[-4,1,-2]
[]
[1,-1,-8,-6,7,4]
[0]
[12,-8,8,-4,-3,2,-8,-12,-5,-6,4]
[-1,14,1,1,0,11,-12,8,-8]
[16,14,-3,-15,13,15,-9,-8,1,-6,14,-10,-13,16,-8]
[14,15,-9,8,7,17,-8,-3,-10,-18,-6,15,-2,12,15,-15,5]
[12,-13,-3,12,-5,-17,4,-6,20,-3,-6,14,10,18,1]
```
# Lepsze `Arbitrary` dla `Tree`

~~~~ {.haskell}
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbTree

-- arbTree n generates a tree of size < n
arbTree 0 = Leaf <$> arbitrary
arbTree n = frequency
        [(1, Leaf <$> arbitrary)
        ,(4, Branch <$> arbTree (div n 2) <*> arbTree (div n 2)
        --   Branch <$> g <*> g where g = arbTree (div n 2)
        ]
~~~~

NB krótszy, zapis w komentarzu jest równoważny;
`g` nie jest drzewem, ale obliczeniem produkującym drzewa


# Monada generatorów

``` haskell
-- Podobna do monady stanu, ale styan jest rozdzielany na dwa
instance Monad Gen where
  return a = Gen $ \n r -> a
  Gen m >>= k = Gen $ \n r0 ->
    let (r1,r2) = split r0
        Gen m'  = k (m n r1)
     in m' n r2

instance Functor Gen where
  fmap f m = m >>= return . f

rand :: Gen StdGen  -- like `get` in the state monad
rand = Gen (\n r -> r)

chooseInt :: (Int,Int) -> Gen Int
chooseInt bounds = (fst . randomR bounds) <$> rand

choose ::  Random a => (a, a) -> Gen a
choose bounds = (fst . randomR bounds) <$> rand
```

# Arbitrary

``` haskell
class Arbitrary a where
  arbitrary :: Gen a

-- randomly choose one of the elements
elements :: [a] -> Gen a
elements xs = (xs !!) <$> choose (0, length xs - 1)

vector :: Arbitrary a => Int -> Gen [a]
vector n = sequence [ arbitrary | i <- [1..n] ]
-- sequence :: Monad m => [m a] -> m [a]

instance Arbitrary () where
  arbitrary = return ()

instance Arbitrary Bool where
  arbitrary = elements [True, False]

instance Arbitrary a => Arbitrary [a] where
  arbitrary = sized (\n -> choose (0,n) >>= vector)

instance Arbitrary Int where
  arbitrary = sized $ \n -> choose (-n,n)
```

# Wynik testu

Test może mieć jeden z trzech wyników:

* `Just True` - sukces
* `Just False` - porażka (plus kontrprzykład)
* `Nothing` - nierozstrzygnięty, dane nie spełniają warynków

``` haskell
data Result = Result { ok :: Maybe Bool, arguments :: [String] }

nothing :: Result
nothing = Result{ ok = Nothing,  arguments = [] }

newtype Property
  = Prop (Gen Result)
```

`Property` jest obliczeniem w monadzie `Gen`, produkującym `Result`.

# Testable

Aby wykonać test, potrzebujemy generatora wyników (`Result`). Takim generatorem jest `Property`.

~~~~ {.haskell}
class Testable a where
  property :: a -> Property

result :: Result -> Property
result res = Prop (return res)

instance Testable () where
  property () = result nothing

instance Testable Bool where
  property b = result (nothing { ok = Just b })

instance Testable Property where
  property prop = prop
~~~~

~~~~
*SimpleCheck1> check True
OK, passed 100 tests
*SimpleCheck1> check False
Falsifiable, after 0 tests:
~~~~
(`False` ma trywialny kontrprzykład)

# Uruchamianie testów

~~~~ {.haskell}
generate :: Int -> StdGen -> Gen a -> a

tests :: Config -> Gen Result -> StdGen -> Int -> Int -> IO ()
tests c gen rnd0 ntest nfail
  | ntest == configMaxTest c = done "OK, passed" ntest
  | nfail == configMaxFail c = done "Arguments exhausted after" ntest
  | otherwise               =
         case ok result of
           Nothing    ->
             tests gen rnd1 ntest (nfail+1)
           Just True  ->
             tests gen rnd1 (ntest+1) nfail
           Just False ->
             putStr ( "Falsifiable, after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    )
     where
      result      = generate (configSize c ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0
~~~~

`configSize n` wyznacza rozmiar dla testu numer `n` (domyślnie: `n/2+3`)

# forAll

~~~~ {.haskell}
-- | `evaluate` extracts a generator from the `Testable` instance
-- Property = Prop (Gen Result)
evaluate :: Testable a => a -> Gen Result
evaluate a = gen where Prop gen = property a

forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property
forAll gen body = Prop $
  do a   <- gen               -- gen :: Gen a
     res <- evaluate (body a) -- body a :: b
     return (argument a res)
 where
  argument a res = res{ arguments = show a : arguments res }


propAddCom1, propAddCom2 :: Property
propAddCom1 =  forAll (chooseInt (-100,100)) (\x -> x + 1 == 1 + x)
propAddCom2 =  forAll int (\x -> forAll int (\y -> x + y == y + x)) where
  int = chooseInt (-100,100)
~~~~

~~~~
>>> check $ forAll (chooseInt (-100,100)) (\x -> x + 0 == x)
OK, passed 100 tests
>>> check $ forAll (arbitrary::Gen Int) (\x -> x + 1 == x)
Falsifiable, after 0 tests:
1
~~~~

# Funkcje

Mając `forAll`, funkcje są zaskakująco łatwe:

~~~~ {.haskell}
instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f

propAddCom3 :: Int -> Int -> Bool
propAddCom3 x y = x + y == y + x

-- instance Testable Bool
-- instance Testable (Int -> Bool)
-- instance Testable (Int -> (Int -> Bool))
~~~~

# Pytania?

# Bonus

## Przykład

``` haskell
module Collatz where
import Test.QuickCheck

f :: Integer -> Integer
f n | even n = n `div` 2
    | odd n  = 3*n + 1

collatz :: Integer -> Bool
collatz(1) = True
collatz(n) = collatz(f(n))

checkCollatz = quickCheck (\n -> n > 0 ==> collatz(n))

verboseCollatz = quickCheckWith stdArgs {maxSuccess = 3} $ verbose(\n -> n > 0 ==> collatz(n))
```

``` haskell
ghci> checkCollatz
+++ OK, passed 100 tests; 99 discarded.

ghci> verboseCollatz
Skipped (precondition false):
0
...
Passed:
1
Passed:
28
Skipped (precondition false):
-65
Passed:
39
+++ OK, passed 3 tests; 11 discarded.
```
