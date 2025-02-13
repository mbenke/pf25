# Lab 2 - typy

## Maybe, Either

Napisz funkcje

``` haskell
elimMaybe :: c -> (a -> c) -> Maybe a -> c
fromMaybe :: a -> Maybe a -> a
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
maybeHead :: [a] -> Maybe a
elimEither :: (a  -> c) -> (b -> c) -> Either a b -> c
mapEither :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
mapRight ::  (b1 -> b2) -> Either a b1 -> Either a b2
fromEither :: Either a a -> a
```

Dla których typy dopuszczają więcej niż jedną implementację?

## Krotki

``` haskell
both :: (a -> b) -> (a, a) -> (b, b)
all3 :: (a -> b) -> (a, a, a) -> (b, b, b)
first :: (a -> c) -> (a, b) -> (c, b)
second :: (b -> d) -> (a, b) -> (a, d)
```

(są bardzo proste, ale mogą się w przyszłości przydać)

## Wycinanki

Napisz funkcje
- obliczające listę dzielników (właściwych) danej liczby całkowitej
- `isPrime` - czy argument jest liczbą pierwszą
- trójki pitagorejskie w danym zakresie

## Drzewa

``` haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)
```

Napisz funkcje

``` haskell
-- Pełne drzewo binarne ponumerowane infiksowo od lewej do prawej
fullTree :: Int -> Tree Int
-- >>> fullTree 2
-- Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)

toList :: Tree a -> [a]
-- >>> toList $ fullTree 3
-- [1,2,3,4,5,6,7]
```

Rozwiąż

https://leetcode.com/problems/construct-string-from-binary-tree/ - w Haskellu i innym wybranym języku programowania.

Napisz funkcje

``` haskell
fullTreeFrom :: Int -> Int -> Tree Int
leftistFromTo
rightistFromTo

-- >>> fullTreeFrom 11 3
-- (14 (12 11 13) (16 15 17))
-- >>> rightistFromTo 3 7
-- (3 () (4 () (5 () (6 () 7))))
-- >>> leftistFromTo 3 7
-- (7 (6 (5 (4 3 ()) ()) ()) ())
```

Czy potrafisz je napisać tak aby działały nie tylko dla liczb, ale na przykład też

``` haskell
-- >>> fullTreeFrom 'a' 3
-- ('d' ('b' 'a' 'c') ('f' 'e' 'g'))
-- >>> rightistFromTo 'h' 'k'
-- ('h' () ('i' () ('j' () 'k')))
```

## Kombinatory

Rozważamy wyrażenia złozone z predefiniowanych kombinatorów, zmiennych  i aplikacji.

``` haskell
data Expr = S | K | I | B
          | Expr :$ Expr
          | X | Z | V Int
          deriving (Show, Read)
```
konstruktor `:$` reprezentuje aplikację; wiąże w lewo.
Na przykład `(S K) K` możemy zapisać jako

```
S :$ K :$ K
```

Uniwwersalną formą zmiennej jest `v_n` reprezentowane w naszym typie przez `(V n)`. Dla ułatwienia may też zmienne X i Z (Y pominięte dla uniknięcia nieporozumień).

### Przykłady wyrażeń

```
test1 = S :$ K :$ K :$ X
twoB = S :$B :$ I
threeB = S :$ B :$ (S :$B :$ I)
test3 = threeB :$ X :$ Z
omega = ((S :$ I) :$ I) :$ ((S :$ I) :$ I)
kio = K :$ I :$ omega
add = (B :$ S) :$ (B :$ B)
```

### Drukowanie

Wyrażenia postaci `((S :$ I) :$ I) :$ ((S :$ I) :$ I)` są mało czytelne.

Napisz funkcję `prettyExpr` która przedstawi swój argument bez zbędnych nawiasów,ze zmiennymi w czytelnej formie np.

```
ghci> prettyExpr omega
"S I I (S I I)"

ghci> prettyExpr (K :$ X :$ (V 7))
"K x v7"
```