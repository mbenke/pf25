# Liczby naturalne

## Arytmetyka Peano

``` haskell
data Nat = Zero | S Nat deriving Show

mul :: Nat -> Nat -> Nat
mul m Zero = Zero
mul m (S n) = add (mul m n) n
```

1. Zapisz i wypróbuj przykłady z wykładu, tudzież

a. Zdefiniuj potęgowanie

b. Popatrzmy na funkcję

``` haskell
eqNat :: Nat -> (Nat -> Bool)
eqNat Zero = \n -> case n of
                     Zero -> True
                     _ -> False
eqNat (S m) = \n -> case n of
                     Zero -> False
                     (S n') -> eqNat m n'
```
zapisz `eqNat` nie używając lambdy ani `case`

2. Zdefiniuj instancję `Show` dla `Nat` (nawiasy!)

3. Zdefiniuj instancję klasy `Enum` dla `Nat` (metody `toEnum`, `fromEnum`)

4. Wypróbuj podane na wykładzie definicje silni i Fibonacciego

```
λ> fact tre
S (S (S (S (S (S Zero)))))
λ> fromEnum it
6
λ> fact tre
S (S (S (S (S (S Zero)))))
λ> fib it
S (S (S (S (S (S (S (S (S (S (S (S (S Zero))))))))))))
λ> fromEnum it
13
```

4. Spróbuj zdefiniować przy pomocy `foldn` funkcję poprzednika `npred`:

```
> npred (S (S (S Zero)))
S (S Zero)
```

# Listy

## Podstawowe operacje


Wypróbuj funkcje

- head, tail
- last, init - podobnie
- take, drop
- replicate
- length, null
- reverse
- uncons, unsnoc (wymaga `import Data.List`)
- intercalate, intersperse (można na napisach)

Uwaga, pytając o ich typy, lepiej użyć opcji `+d`, inaczej odpowiedzi mogą być niezrozumiałe np

```
ghci> :t and
and :: Foldable t => t Bool -> Bool
ghci> :type +d and
and :: [Bool] -> Bool
```

Spróbuj stworzyć ich własne implementacje. Możesz dać im własne nazwy lub ukryć standardowe wersje:

``` haskell
import Prelude hiding(head,tail,last,init,take,drop,replicate,reverse,length,null)
```

## Łączenie i sortowanie

Napisz funkcję `fair`, która połączy dwie listy wybierając na przemian po jednym elemencie z każdej, np.

``` haskell
-- >>> fair [1,2,3,4] [55,77]
-- [1,55,2,77,3,4]
```

Napisz funkcję `merge :: Ord a => [a] -> [a] -> [a]` która połączy dwie listy uporządkowane rosnąco w jedną tak uporządkowaną, np

``` haskell
-- >>> merge [2,5,6] [1,3]
-- [1,2,3,5,6]
```

Używając powyższej funkcji, napisz mergesort.


## zip - ćwiczenia


Przy pomocy `zip` napisz funkcję `positions` taką, że `positions x xs` daje listę pozycji wystąpień `x` w liście `xs`

``` haskell
ghci> positions 'a' "alamakota"
[0,2,4,8]
```

Teraz napisz funkcję `position` która daje pierwszą pozycję lub (-1) gdy nie ma wystąpień.


## foldr & friends

a. Napisz funkcje
```
incAll :: [[Int]] -> [[Int]]
```
która zwiększy o 1 każdy element każdego elementu swojego argumentu, np
```
> incAll $ inits [1..3]
[[],[2],[2,3],[2,3,4]]
```
b. Napisz przy pomocy foldl/foldr

* silnię
* `concat :: [[a]] -> [a]`
* `maximum :: [Int] -> Int`
* `minimum :: [Int] -> Int`
* uogólnij dwie powyższe przy uzyciu `winner :: (a -> a -> a) -> [a] -> a`

c. Napisz `nub` (funkcję eliminującą duplikaty z listy) przy pomocy `filter`

d. Napisz funkcję obliczającą iloczyn skalarny dwóch list liczb; użyj `zipWith`
