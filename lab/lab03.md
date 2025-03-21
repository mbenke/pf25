# Lab 03 - klasy

## Drzewa

``` haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving
```
a. stwórz własne instancje `Eq`, `Show`

 ``` haskell
instance Show a => Show (Tree a) where
   show t = ...

instance Eq a => Eq (Tree a) where
   t1 == t2 = ...
```

b. zaimplementuj drzewa BST z funkcjami

``` haskell
insert :: (Ord a) => a -> Tree a -> Tree a
contains :: (Ord a) => a -> Tree a -> Bool
fromList :: (Ord a) => [a] -> Tree a
```
Dla znudzonych: `delete`, drzewa czerwono-czarne:

``` haskell
data Color = R | B deriving Show
data Tree a = E | T Color (Tree a) a (Tree a) deriving Show
```
c. Stwórz instancję **Read** dla drzew, która odczyta drzewo z napisu postaci 
`"(1 (2 () ()) (3 () ()))"` (było na wykładzie)

## Wyrażenia

Rozważmy typ dla wyrażeń arytmetycznych z let:

``` haskell

data Exp
    = EInt Int             -- stała całkowita
    | EAdd Exp Exp         -- e1 + e2
    | EMul Exp Exp         -- e1 * e2
    | EVar String          -- zmienna
    | ELet String Exp Exp  -- let var = e1 in e2
```

a. Napisz instancje Eq oraz Show dla Exp (nietrywialne: nawiasy)

b. Napisz instancje Num dla Exp tak, żeby można było napisać

    testExp2 :: Exp
    testExp2 = (2 + 2) * 3

(metody abs i signum mogą mieć wartość undefined)

c. Napisz funkcję `simpl`, przekształcającą wyrażenie na równoważne prostsze wyrażenie, np.

0*x + 1*y -> y

(nie ma tu precyzyjnej specyfikacji, należy użyć zdrowego rozsądku; uwaga na zapętlenie).

d. Można dopisać liczenie pochodnej względem danej zmiennej:

~~~~
deriv :: String -> Exp -> Exp
~~~~

Dla znudzonych:

e. Podstawienie

```
type Subst = [(Name,Exp)]
apply :: Subst -> Exp -> Exp
```


f. wyrażenia parametryzowane:

``` haskell
    data Exp a
      = EInt Int             -- stała całkowita
      | EAdd Exp Exp         -- e1 + e2
      | ESub Exp Exp         -- e1 - e2
      | EMul Exp Exp         -- e1 * e2
      | EVar a          -- zmienna
      | ELet a Exp Exp  -- let var = e1 in e2

deriv :: Eq a => a -> Exp a -> Exp a
```

## Kombinatory

Mały podzbiór składni Haskella:

```
type Name = String
data Def = Def Name [Pat] Expr
data Expr = Var Name | Expr :$ Expr |  IntLit Integer
type Pat = Name
```

napisz instancje `Show` dla `Def` i `Expr` (nawiasy!)
