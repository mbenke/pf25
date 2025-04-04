## Foldable

Stwórz instancję `Foldable` dla drzew

``` haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
```

czy `Data.Foldable.toList $ fullTree 3` daje takli sam wynik jak `toList $ fullTree 3`? Jeśli nie, to jak to poprawić?

Czy potrafisz zapisać `joinT` przy pomocy `foldMap`?

## Traversable

Stwórz instancje `Traversable` dla drzew i wypróbuj

``` haskell
let f x = if x < 9 then Just(x+1) else Nothing in traverse f $ fullTreeFrom 1 3
let f x = if x < 9 then Just(x+1) else Nothing in traverse f $ fullTreeFrom 8 3
traverse print $ fullTreeFrom 1 2
sequenceA (Node getLine Empty Empty)
```

## Zipper

1. Zaimplementuj i wypróbuj  omówiony na wykładzie typ suwaka dla drzew

2. Równoważną (w pewnym sensie dualną) reprezentacją
jest opis ścieżki od korzenia zawierający kierunki ruchu<br/>oraz mijane po drodze poddrzewa:

``` haskell
data Dir = DL | DR deriving (Show, Eq)
type Path a = [(Dir, Tree a)]
type Foc a = (Tree a, Path a)
```
Podstawowa różnica polega na tym, że suwak przechowuje ścieżkę w odwrotnej kolejności, so usprawnia nawigację.

Napisz funkcje przekształcające pomiedzy tymi reprezentacjami

``` haskell
toFoc   :: Loc a -> Foc a
fromFoc :: Foc a -> Loc a
```

3. Napisz funkcję `showLoc` wypisującą drzewa z lokalizacją jak widzieliśmy wcześniej

``` haskell
(Bin (Bin (Tip 1) (Tip 2)) {Tip 7})
```

(instancja `Show` będzie wymagała dodania pragmy `{-# OVERLAPPING #-}` )

4. Stwórz zipper dla typu wyrażeń

``` haskell
data Expr = Var Name
          | Con Name
          | Expr :$ Expr
          | IntLit Integer
```

można też użyć typu parametryzowanego:

``` haskell
type Expr = Exp a
data Exp a = a
           | Con Name
           | Expr :$ Expr
           | IntLit Integer
```


## Inne

1. Przy pomocy poznanych mechanizmów (Applicative/Foldable/Traversable) napisz funkcję allCombinations:

``` haskell
>>> allCombinations ["xyz","ab", "12"]
["xa1","xa2","xb1","xb2","ya1","ya2","yb1","yb2","za1","za2","zb1","zb2"]
```

2. (Nietrywialne) Rozważmy klasę


``` haskell
class Liftable f where
  unit :: f ()
  pair :: f a -> f b -> f (a, b)
```

pokaż, że jest ona wzajemnie definiowalna z `Applicative` (przynajmniej w sensie typów).