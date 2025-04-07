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

1. Napisz `instance Functor ETree`

Porównaj, dla różnych drzew `t`

``` haskell
toList (allOnes t)
allOnes (toList t)
```

2. Wypróbuj 'flip trick' z wykładu (w starszych wersjach GHC wymaga pragm LANGUAGE):

``` haskell
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

newtype Flip f a b = Flip { unFlip :: (f b a) }
  deriving (Eq, Show)

instance Functor (Flip Either a) where
  fmap f (Flip (Right e)) = Flip (Right e)
  fmap f (Flip (Left a)) = Flip (Left (f a))


-- >>> fmap succ (Flip (Left 'x'))
-- Flip {unFlip = Left 'y'}

pamf f = unFlip . fmap f . Flip

-- >>> pamf succ (Left 'x')
-- Left 'y'
```

- spróbuj napisać typ funkcji `pamf`
- stwórz instancje Functor dla `(,)` po czym przy pomocy fmap/pamf napisz funkcje

``` haskell
first :: (a -> c) -> (a,b) -> (c,b)
second :: (b -> d) -> (a,b) -> (a,d)
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



