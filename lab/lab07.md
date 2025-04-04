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

