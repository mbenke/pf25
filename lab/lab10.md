## Arytmetyka Peano

``` haskell
data Nat = Zero | S Nat deriving Show

add :: Nat -> Nat -> Nat
add m Zero = Zero
add m (S n) = S(add m n)

mul :: Nat -> Nat -> Nat
mul m Zero = Zero
mul m (S n) = add (mul m n) n
```

## Wnioskowanie

Udowodnij:
- `(S Zero)` jest elementem neutralnym mnożenia
- łączność dodawania/mnożenia
- rozdzielność mnożenia względem dodawania: `k*(m+n) = k*m + k*n`


## Konstrukcja programów

Bazując na przykładzie z wykładu dla odejmowania napisz specyfikację dla dzielenia i wyprowadź program spełniający tę specyfikację.

# Listy

## Wnioskowanie
a. udowodnij łączność konkatenacji zdefiniowanej jako

``` haskell
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x:(xs++ys)
```

b. niech
``` haskell
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]
```
udowodnij
```
rev(xs ++ ys) = rev ys ++ rev xs
rev(rev xs) = xs
```
c. Niech
``` haskell
take, drop :: Int -> [a] -> [a]
take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs

drop n xs     | n <= 0 =  xs
drop _ []              =  []
drop n (_:xs)          =  drop (n-1) xs

splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs           =  (take n xs, drop n xs)
```
Udowodnij `take n xs ++ drop n xs = xs`

d. Niech

``` haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs
```

``` haskell
filter p . filter q = filter (p && q)
filter p . concat = concat . map(filter p)
```

Udowodnij, że `filter p (xs ++ ys) = filter p xs ++ filter p ys`.
Nie używaj indukcji.

e. Napisz swoje wersje funkcji

```haskell
takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]
```
Czy dla wszystkich predykatów p i list xs zachodzi

``` haskell
takeWhile p xs ++ dropWhile p xs = xs
```

## Wyprowadzanie implementacji

Na wykładzie było

``` haskell
revA [] ys     = ys
revA (x:xs) ys = revA xs (x:ys)

reverse xs = revA xs []
```

- dla powyższej definicji reverse wykaż

``` haskell
reverse (reverse xs) = xs
reverse (xs ++ ys)   = reverse ys ++ reverse xs
```

- spróbuj podobnie ulepszyć funkcję
```
flatten :: Tree a -> [a]
flatten (Leaf x)   = [x]
flatten (Node l r) = flatten l ++ flattten r
```

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

``` haskell
pure id <*> x  =  x                              -- identity
pure (g x)     =  pure g <*> pure x              -- homomorphism
x <*> pure y   =  pure (\g -> g y) <*> x         -- interchange
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z   -- composition
```

## Prawa dla Monad - ćwiczenie

``` haskell
return x >>= f    =  f x
mx >>= return     =  mx
(mx >>= f) >>= g  =  mx >>= (\y -> f y >>= g)
```

**Ćwiczenie:** wykaż, że powyzsze prawa są spełnione dla

``` haskell
instance Monad Maybe where
  return         = Just
  Nothing  >>= f = Nothing
  (Just x) >>= f = Just (f x)
```
