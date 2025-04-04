## Maybe

a. Zaimplementuj i uruchom przykład z wykładu

``` haskell
data Exp = Val Int | Div Exp Exp
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (div x y)

eval :: Exp -> Maybe Int
eval (Val n)   = return n
eval (Div x y) = do
    n <- eval x
    m <- eval y
    safediv n m
```

uzupełnij ten przykład o inne operacje arytmetyczne.

b. Napisz funkcje obliczające wartość listy wyrażeń:

```
evalList' :: [Exp] -> [Maybe Int]
evalList :: [Exp] -> Maybe [Int]
```

c. Zmodyfikuj kod z (a) i (b) używając `Either` zamiast `Maybe`:

``` haskell
safediv :: Int -> Int -> Either String Int
safediv _ 0 = Left "Division by zero"
safediv x y = Right (div x y)

eval :: Exp -> Either String Int
```
d. Rozszerz wyrażenia o

``` haskell
data Exp = ... Var String
```

Napisz funkcję

``` haskell
eval :: Env -> Exp -> Either String Int
```

W przypadku użycia w wyrazeniu nieznanej zmiennej zgłoś odpowiedni błąd.

Można przyjąć `Env = [(String, Int)]` lub `Data.Map.Map String Int`.

## MonadReader
a.
``` haskell
 data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)
```

Napisz funkcję

``` haskell
renumber :: Tree a -> Tree Int
```

która dla danego drzewa da drzewo, gdzie w każdym węźle przechowywana będzie głębokość tego węzła (odległość od korzenia).

Porównaj rozwiązania z użyciem monady Reader i bez.

b. Dany typ wyrażeń arytmetycznych
```
data Exp = EInt Int | EVar String | EAdd Exp Exp | ESub Exp Exp | EMul Exp Exp | ELet String Exp Exp deriving(Eq, Show)
```

Napisz funkcję

``` haskell
    evalExp :: Exp -> Int
```

ktora obliczy wartość takiego wyrażenia, np

``` haskell
-- let x = (let y = 15 in y-1) in x * 3
test = ELet "x" ((ELet "y" 15) (y - 1)) (x * 3) where
    (x, y) = (EVar "x", EVar "y")
```
(żeby w ten sposób zapisać `test` potrzebna jest odpowiednia instancja `Num` dla `Exp`;
można też uzyć typu wyrażeń z jednego z poprzednich labów)

Użyj monady czytelnika środowiska (Reader). Środowisko może być
np. jednego z typów

```
Map Var Int
[(Var, Int)]
Var -> Maybe Int
```

c. Zdefiniuj
``` haskell
newtype Arr a b = Arr { apply ::  a -> b }

instance Functor (Arr r) where
instance Applicative (Arr r) where
instance Monad (Arr r) where
instance MonadReader r (Arr r) where
```

Spróbuj wyrazić `(>>=)` przy użyciu `(<*>)`.

Aby korzystać z pakietów takich jak random i mtl, na początku pliku umieść inkantację

``` haskell
{- cabal:
    build-depends: base, mtl, random
-}
```
a potem zamiast GHCi uruchom plik przy użyciu `cabal repl <nazwapliku>

## MonadState

a. Wypróbuj przykłady rzucania kostkami z wykładu.

b. Napisz funkcję

``` haskell
renumberTree :: Tree a -> Tree Int
```
która ponumeruje wezly drzewa tak, ze kazdy z nich bedzie mial inny numer.
Porownaj rozwiazania z uzyciem monady State i bez.

możliwe dodatkowe wymaganie: ponumeruj wezly drzewa w kolejnosci infiksowej.

``` haskell
(toList $ renumber $ fromList "Learn Haskell") == [0..12]
```
