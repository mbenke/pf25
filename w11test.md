---
title: Programowanie Funkcyjne
subtitle: Testowanie
author:  Marcin Benke
date: Wykład 10, 2025
---

<meta name="duration" content="80" />

# Testowanie programów w Haskellu
* doctest [github: sol/doctest](https://github.com/sol/doctest)
* HUnit
* Quickcheck
* QuickCheck + doctest
<!-- * Hedgehog [github: hedgehogqa/haskell-hedgehog](https://github.com/hedgehogqa/haskell-hedgehog)
-->

# doctest
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
które jest elementem składni narzędzia dokumentacji [Haddock](https://haskell-haddock.readthedocs.io/en/latest/))

```
$ cabal install doctest
$ doctest DoctestExamples.hs
### Failure in DoctestExamples.hs:7: expression `2 + 2'
expected: 5
 but got: 4
Examples: 2  Tried: 2  Errors: 0  Failures: 1
```


# Dygresja  - Haddock

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


# Przykład z BNFC
``` {.haskell}
-- | Generate a name in the given case style taking into account the reserved
-- word of the language.
-- >>> mkName [] SnakeCase "FooBAR"
-- "foo_bar"
-- >>> mkName [] CamelCase "FooBAR"
-- "FooBAR"
-- >>> mkName [] CamelCase "Foo_bar"
-- "FooBar"
-- >>> mkName ["foobar"] LowerCase "FooBAR"
-- "foobar_"
mkName :: [String] -> NameStyle -> String -> String
mkName reserved style s = ...
```

# HUnit

Podobnie jak w innych językach, w Haskellu możemy stosowac testy jednostkowe, np.

~~~~ {.haskell}
import Test.HUnit
import MyArray

main = runTestTT tests

tests = TestList [test1,test2]

listArray1 es = listArray (1,length es) es
test1 = TestCase$assertEqual "a!2 = 2" (listArray1 [1..3] ! 2) 2
test2 = TestCase$assertEqual "elems . array = id"
                             (elems $ listArray1 [1..3]) [1..3]
~~~~

~~~~
> main
Cases: 2  Tried: 2  Errors: 0  Failures: 0
Counts {cases = 2, tried = 2, errors = 0, failures = 0}
~~~~

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

ni jest definiowalna - nie umiemy porównywać funkcji.

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

# QuickCheck

* Generowanie wielu testów jednostkowych jest nudne
* Sprawdzenie wszystkkich możliwości nie jest realistyczne (chyba, że dla bardzo małych danych - patrz SmallCheck)
* Pomysł: wygenerujmy odpowiednią próbkę danych

~~~~
*Main> import Test.QuickCheck
*Main Test.QuickCheck> quickCheck prop_idempotent
+++ OK, passed 100 tests.
~~~~


QuickCheck wygenerował 100 losowych list i sprawdził, ze dal nich własność `prop_idempotent` zachodzi  

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

prop_fadd_comm :: Float -> Float -> Bool
prop_fadd_comm a b = a + b == b + a

prop_fadd_assoc :: Float -> Float -> Float -> Bool
prop_fadd_assoc a b c = (a + b) + c  == a + (b + c)
```


```
λ> quickCheck prop_fadd_com
+++ OK, passed 100 tests.
λ> quickCheckWith stdArgs {maxSuccess = 1000} prop_fadd_comm
+++ OK, passed 1000 tests.
λ> quickCheck prop_fadd_assoc
*** Failed! Falsified (after 6 tests and 6 shrinks):
1.0
-1.95
-2.06
```
 

* definiujemy własności, które mają być przetestowane - w przybliżeniu: funkcje o typie wyniku `Bool`, 
dokładniej  - typu, który należy do klasy `Testable`;
* QuickCheck losuje pewną próbę danych
i sprawdza, czy dla wszystkich własność jest spełniona;
* Istnieją standardowe generatory dla typów wbudowanych, dla własnych typów trzeba je zdefiniować.

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

instance Monad Gen where ...
~~~~

`Gen` jest monadą: `Gen t` jest obliczeniem dającym `t` (być może korzystającym z generatorów pseudolosowych)

## Generacja liczb losowych

``` haskell
import System.Random
  ( StdGen       -- :: *
  , newStdGen    -- :: IO StdGen
  , randomR      -- :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
  , split        -- :: RandomGen g => g -> (g, g)
                 -- splits its argument into independent generators
  -- class RandomGen where
  --   next     :: g -> (Int, g)
  --   split    :: g -> (g, g)
  -- instance RandomGen StdGen
  -- instance Random Int
  )

roll :: StdGen -> Int
roll rnd = fst $ randomR (1,6) rnd
main = do
  rnd <- newStdGen
  let (r1,r2) = split rnd
  print (roll r1)
  print (roll r2)
  print (roll r1)
  print (roll r2)
```

## Generacja liczb losowych

``` haskell
main = do
  rnd <- newStdGen
  let (r1,r2) = split rnd
  print (roll r1)
  print (roll r2)
  print (roll r1)
  print (roll r2)
```

```
*Main System.Random> main
4
5
4
5
```

Samo `StdGen` jest czyste i daje za każdym razem ten sam wynik, dlatego zwykle opakowywane jest w odpowiednią monadę.

Nie będziemy w tym momencie wchodzić w szczegóły, ale w przypadku QuickCheck używamy `Gen`.

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

chooseInt1 :: (Int,Int) -> Gen Int
chooseInt1 bounds = Gen $ \n r  -> fst (randomR bounds r)
-- randomR :: (Random a) => (a, a) -> StdGen -> (a, StdGen)

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

NB krótszy, zapis w komentarzu jest rónoważny;
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

chooseInt :: (Int,Int) -> Gen Int
chooseInt bounds = (fst . randomR bounds) <$> rand

rand :: Gen StdGen  -- like `get` in the state monad
rand = Gen (\n r -> r)

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

To test something, we need a `Result` generator (i.e. `Property`)

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
(`False` has a trivial counterexample)

# Running tests

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

`configSize n` determines data size for test `n` (default: `n/2+3`)

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

# Functions

Given `forAll`, functions are surprisingly easy:

~~~~ {.haskell}
instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f

propAddCom3 :: Int -> Int -> Bool
propAddCom3 x y = x + y == y + x

-- instance Testable Bool
-- instance Testable (Int -> Bool)
-- instance Testable (Int -> (Int -> Bool))
~~~~

# Implication (conditional tests)

Implication: test q, providing data satisfies p

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
> check propMul1
OK, passed 100 tests

> check propMul2
Falsifiable, after 0 tests:
2
-2
~~~~

<!--
# Generating functions

We can test functions, but to test higher-order functons we need to generate random functions.


Note that

~~~~ {.haskell}
Gen a ~ (Int -> StdGen -> a)
Gen(a -> b) ≃ (Int -> StdGen -> a -> b) ≃ (a -> Int -> StdGen -> b) ≃ (a -> Gen b)
~~~~

so we can write

~~~~ {.haskell}
promote :: (a -> Gen b) -> Gen (a -> b)
promote f = Gen (\n r -> \a -> let Gen m = f a in m n r)
~~~~

We can use `promote` to construct a function generator if we can create a generator family for results depending somehow on arguments

# Coarbitrary

We can describe this with a class:

~~~~ {.haskell}
class CoArbitrary a where
  coarbitrary :: a -> Gen b -> Gen b
~~~~

`coarbitrary` produces a generator transformer from its argument

Now we can use `Coarbitrary` to define `Arbitrary` instance for functions:

~~~~ {.haskell}
instance (CoArbitrary a, Arbitrary b) => Arbitrary(a->b) where
  arbitrary = promote $ \a -> coarbitrary a arbitrary
~~~~

NB in QuickCheck v1.1 `coarbitrary` is a method of `Arbitrary`;
v2 uses a different approach to function generation

**Exercise:** write a few instances of `Arbitrary` for your types.

# CoArbitrary instances

To define CoArbitrary instances

~~~~ {.haskell}
class CoArbitrary where
  coarbitrary :: a -> Gen b -> Gen b
~~~~

we need a way to construct generator transformers. Let us define the function

~~~~ {.haskell}
variant :: Int -> Gen a -> Gen a
variant v (Gen m) = Gen (\n r -> m n (rands r !! (v+1)))
 where
  rands r0 = r1 : rands r2 where (r1, r2) = split r0
~~~~

which splits the input generator into many variants and chooses one of them
depending on the argument

~~~~ {.haskell}
instance CoArbitrary Bool where
  coarbitrary False = variant 0
  coarbitrary True  = variant 1
~~~~

# Example: coarbitrary for trees

~~~~ {.haskell}
instance Arbitrary Tree where
  arbitrary = sized tree'
    where tree' 0 = liftM Leaf arbitrary
	  tree' n | n>0 =
		oneof [liftM Leaf arbitrary,
	          liftM2 Branch subtree subtree]
  	    where subtree = tree' (n `div` 2)

  coarbitrary (Leaf n) =
	variant 0 . coarbitrary n  --
	-- coarbitrary n :: Gen T -> Gen T
	-- variant 0 :: Gen T -> Gen T

  coarbitrary (Branch t1 t2) =
	variant 1 . coarbitrary t1 . coarbitrary t2
~~~~


~~~
variant :: Int -> Gen a -> Gen a
coarbitrary :: a -> Gen b -> Gen b
~~~


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

# A problem with implication

~~~~
prop_insert1 x xs = ordered (insert x xs)

*Main Test.QuickCheck> quickCheck prop_insert1
*** Failed! Falsifiable (after 6 tests and 7 shrinks):
0
[0,-1]
~~~~

...obviously...

~~~~
prop_insert2 x xs = ordered xs ==> ordered (insert x xs)

>>> quickCheck prop_insert2
*** Gave up! Passed only 75 tests; 1000 discarded tests.
~~~~

Probability that a random list is ordered is small...

# Test case distribution

...and those which are, are usually not very useful

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


# Sometimes you need to write your own generator

* Define a new type

~~~~
newtype OrderedInts = OrderedInts [Int]

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
# doctest + QuickCheck

~~~~ {.haskell}
module Fib where

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> newtype Small = Small Int deriving Show
-- >>> instance Arbitrary Small where arbitrary = Small . (`mod` 10) <$> arbitrary

-- | Compute Fibonacci numbers
--
-- The following property holds:
--
-- prop> \(Small n) -> fib n == fib (n + 2) - fib (n + 1)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
~~~~

```
stack install QuickCheck
stack exec doctest Fib.hs
Run from outside a project, using implicit global project config
Using resolver: lts-9.21 from implicit global project's config file: /Users/ben/.stack/global/stack.yaml
Examples: 5  Tried: 5  Errors: 0  Failures: 0
```
-->

# Running all tests in a module

`quickCheckAll` tests all properties with names starting with `prop_` (and proper type).
It uses TemplateHaskell.

The next lecture will discuss how such functions work.

Usage example

``` haskell
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

prop_AddCom3 :: Int -> Int -> Bool
prop_AddCom3 x y = x + y == y + x

prop_Mul1 :: Int -> Property
prop_Mul1 x = (x>0) ==> (2*x > 0)

return []  -- tells TH to typecheck definitions above and insert an empty decl list
runTests = $quickCheckAll

main = runTests
```
