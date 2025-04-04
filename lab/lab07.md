Rozważmy typ drzew z wartościami w wierzchołkach zewnętrznych

``` haskell
data ETree a = Tip a | Bin (ETree a) (ETree a) -- deriving Show
```

### Rozgrzewka
- napisz funkcję `fullTree :: Int -> ETree Int` tworzącą pełne drzewo podanej głębokości
- `toList :: ETree a -> [a]` (z akumulatorem/ogonową)
    ``` haskell
    -- >>> toList (fullTree 4)
    -- [1,2,3,4,5,6,7,8]
    ```

- `instance Show a => Show (ETree a)` tak aby

``` haskell
-- >>> fullTree 4
-- (((1 2) (3 4)) ((5 6) (7 8)))
```

### Functor

Napisz `instance Functor ETree`

Porównaj, dla różnych drzew `t`

``` haskell
toList (allOnes t)
allOnes (toList t)
```

### Applicative

Napisz `instance Applicative ETree`

Metodę `<*>` można napisać przez indukcję po pierwszym bądź drugiom argumencie, ewentualnie obu naraz.<br/>
Porównaj te implementacje. Ile różnych funkcji potrafisz napisać?

Niech

``` haskell
funTree :: ETree (Int -> Int)
funTree = Bin (Tip (+10)) (Tip (*2))

argTree :: ETree Int
argTree = fullTree 2
```

porównaj wyniki `funTree <*> argTree` dla różnych implementacji (mogą się nazywać `apply1, apply2, ...)


Przekonaj się na tym przykładzie o słuszności praw:

``` haskell
pure (g x)     =  pure g <*> pure x
x <*> pure y   =  pure (\g -> g y) <*> x
```

Pomyśl jakie będą wartości następujących wyrażeń

``` haskell
pure (+) <*> pure 2 <*> pure 3 :: Maybe Int
pure (+) <*> pure 2 <*> pure 3 :: [Int]
[(+),(*)] <*> [1,2] <*> [5,8]
```
a potem sprawdź w GHCI

NB `Applicative` dla drzew z wartościami w wierzchołkach wewnętrznych (`Tree`) na tym etapie może się wydawać niewykonalne. Ale jeszcze zobaczymy.

### Solo

``` haskell
data Solo a = MkSolo a

instance Functor Solo where
    fmap f (Solo a) = Solo (f a)

instance Applicative Solo where
    pure = Solo
    Solo f <*> Solo x = Solo (f x)
```

Sprawdź, że prawa `Applicative` zachodzą dla `Solo`.

## Semigroup, Monoid

1. (Trywialne) Stwórz instancję `Semigroup` dla `ETree`

2. (Nietrywialne) Stwórz instancję `Semigroup` dla `Tree`

``` haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
```

tak aby

``` haskell
-- >>> toList (fullTreeFrom 1 3 <> fullTreeFrom 11 3)
[1,2,3,4,5,6,7,11,12,13,14,15,16,17]
```

3. (Trywialne) Stwórz instancję `Monoid` dla `Tree`

4. (Łatwe) Czy potrafisz napisać funkcję

``` haskell
joinT :: Tree(Tree a) -> Tree a
```

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