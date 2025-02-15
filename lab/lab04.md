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

# Listy

## Podstawowe operacje


Wypróbuj funkcje

- head, tail 
- last, init - podobnie
- take, drop
- replicate
- length, null
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
import Prelude hiding(head,tail,last,init,take,drop,replicate,length,null)
```


## zip - ćwiczenia

``` haskell
nondec xs = and(map leq pairs) where
  leq (x,y) = x <= y
  pairs = zip xs (tail xs)
```

Jaki wynik da `nondec` dla listy pustej? Jak to poprawić?

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

*   silnię
*   `concat :: [[a]] -> [a]`
* `maximum :: [Int] -> Int`
* `minimum :: [Int] -> Int`
* uogólnij dwie powyższe przy uzyciu `winner :: (a -> a -> a) -> [a] -> a`

c. Napisz `nub` (funkcję eliminującą duplikaty z listy) przy pomocy `filter`

d. Napisz funkcję obliczającą iloczyn skalarny dwóch list liczb; użyj `zipWith`

