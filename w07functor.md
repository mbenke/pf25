---
title: Programowanie Funkcyjne
subtitle: Wyższe klasy
author:  Marcin Benke
date: Wykład 7/2025
---

# Functor

1. Motywacja - `map`
2. Klasa `Functor`
3. Klasy konstruktorowe

## Przypomnienie - map
Przypomnijmy funkcję `map` na listach:

``` haskell
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
```

Podobne funkcje możmy pisać dla innych struktur np. drzew

``` haskell
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = go where
  go Empty = Empty
  go (Node x l r) = Node (f x) (go l) (go r)
```

Ogólna zasada: zachowujemy strukturę, a do elementów stosujemy podaną funkcję.

## Functor

``` haskell
map :: (a -> b) -> [a] -> [b]
mapTree :: (a -> b) -> Tree a -> Tree b
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
```

Możemy wyrazić ten schemat przy pomocy klasy

``` haskell
class Functor t where
    fmap :: (a -> b) -> t a -> t b

instance Functor [] where
    fmap = map

instance Functor Tree where
    fmap = mapTree

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)
```

NB ta klasa mogła by sie nazywać `Fun` albo `Mappable` i w niektórych językach tak się nazywa.

Haskell lubi straszyć noobów nazwami typu *Semigroup, Monoid, Functor, Monad, ...*
nie nalezy się tego bać.

### Konstruktory wartości i typów

```haskell
data Tree a = Leaf a | Branch (Tree a) (Tree a)

mapTree :: (a->b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Branch l r) = Branch (m l) (m r) where
m = mapTree f
```
**Leaf** jest 1-argumentowym konstruktorem,<br/>
**Branch** — 2-argumentowym.

Per analogiam mówimy, że **Tree** jest jednoargumentowym *konstruktorem typu*:

-   jeśli **x** jest wartością, to **Leaf x** jest wartością;
-   jesli **a** jest typem, to **Tree a** jest typem.

(ale nie na krzyż - **Leaf Int** ani **Tree 1** nie mają sensu)

### Klasy konstruktorowe

Typy polimorficzne jak **\[a\]** czy **Tree a** mogą być instancjami klas (przeważnie pod warunkiem, ze **a** jest też instancją odpowiedniej klasy)…

```haskell
    data Tree a = Leaf a | Branch (Tree a) (Tree a)
      deriving Show

    instance Eq a => Eq (Tree a) where
      Leaf x == Leaf y = x == y
      Branch l r == Branch l' r' = (l==l')&&(r==r')
      _ == _ = False
```

…ale są też klasy, których instancjami są nie typy, a *konstruktory typów*,
takie jak **Tree** czy **Maybe**.

### Functor jest klasą konstruktorową

Spójrzmy jeszcze raz na klasę `Functor` i jej instancje

``` haskell
class Functor t where
    fmap :: (a -> b) -> t a -> t b

instance Functor [] where

instance Functor Tree where

instance Functor Maybe where
```

`Tree` i `Maybe` nie są typami, ale konstruktorami typów.

Podobnie, w tym kontekście `[]` oznacza konstruktor typou list.

Klasa `Functor` jest klasą konstruktorową - jej instancjami są konstruktory typów.

Klasy typów opisują własności typów, klasy konstruktorowe - własności konstruktorów typów.<br />

Konstruktory typów i ich własności są pojęciami *wyższego rodzaju* (ang. *higher-kinded*)

### Prawa dla fmap

``` haskell
class Functor t where
    fmap :: (a -> b) -> t a -> t b
```

Prawa dla `fmap` są analogiczne jak dla `map`:

``` haskell
fmap id = id
fmap f . fmap g = fmap (f . g)
```

Pierwsze prawo sugeruje, że `fmap` zachowuje strukturę.<br/>
Zauważmy, że `fmap` jest polimorficzne, zatem z uwagi na parametryczność musi działać dla wszystkich funkcji tak samo.

**Pytanie:** czy `fmap f x` ma zawsze tyle samo elementów co `x`?

Tym niemniej `fmap` nie zawsze musi być tak trywialne jak dla list czy drzew.<br/>
Pomyślmy na przykład o zbiorach czy drzewach BST i funkcjach, które nie są różnowartościowe czy monotoniczne.

Dla typów, które są instancją `Monoid`, można pokusić się o inną charakteryzacje:

``` haskell
fmap f mempty = mempty
fmap f (x <> y) = fmap f x <> fmap f y
```

## Skróty

Dane drzewo, jak uzyskać drzewo tego samego kształtu, ale złożone z samych jedynek?

I podobnie dla list ...

``` haskell
allOnes :: Functor t => t a -> t Char
allOnes t = fmap (const '1') t
```

ale można krócej:

``` haskell
allOnes t = '1' <$ t

-- (<$) :: Functor f => a -> f b -> f a
-- (<$) = fmap . const
-- '1' <$ t = (<$) '1' t = (fmap . const) '1' t = fmap (const '1') t

-- >>> '1' <$ "abc"
-- "111"

-- >>> allOnes $ Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)
-- Node '1' (Node '1' Empty Empty) (Node '1' Empty Empty)
```

To może trochę słaby przykład, ale w przyszłości zobaczymy  lepsze, np.

``` haskell
pBool :: Parser Bool
pBool =  EBool True  <$ pKeyword "true"
     <|> EBool False <$ pKeyword "false"
```

## Applicative

Motywacja: chcemy napisać funkcję `addMaybe :: Maybe Int -> Maybe Int -> Maybe Int`

Po co? `Maybe` jest typowym rozwiązaniem dla obliczeń zawodnych.

Łatwo to zrobić przez dopasowanie, ale czy możemy skorzystać z `fmap`? (NB `<$>` to infiksowa wersja `fmap`)

``` haskell
ghci> fmap (+1) (Just 5::Maybe Int)
Just 6

ghci> (+) <$> (Just 2) <$> (Just 3::Maybe Int)

error: [GHC-83865]
    • Couldn't match expected type: Int -> b
                  with actual type: Maybe (a0 -> a0)
```

Tak się nie da :(

(NB wskazanie typu `::Maybe Int`) tylko po to, żeby klasa `Num` nie zaciemniała obrazu)

### Problem z typami

Co poszło nie tak?

``` haskell
ghci> :t (+1) <$> (Just 5::Maybe Int)
(+1) <$> (Just 5::Maybe Int) :: Maybe Int

ghci> :t (+) <$> (Just 5::Maybe Int)
(+) <$> (Just 5::Maybe Int) :: Maybe (Int -> Int)
```

Potrzebujemy albo funkcji

``` haskell
fmap2 :: (a -> b ->c) -> Maybe a -> Maybe b -> Maybe c
```
(tudzież `fmap3,fmap4`, itd.) ...albo funkcji:

``` haskell
apply :: Maybe (a -> b) -> Maybe a -> Maybe b
```
wtedy
``` haskell
ghci> apply((+) <$> (Just 2)) (Just 3::Maybe Int)
Just 5
```

Początkowo stosowano perwsze podejście: funkcje `liftM .. liftM5`, ale to mało elastyczne.

### Lift me to the stars

drugie podejście jest lepsze, `<$>` i `<*>` (apply) pozwalają wyrazić `fmap_n` dla n>0:

``` haskell
(<*>) ::  Maybe (a -> b) -> Maybe a -> Maybe b

fmap2 :: (a -> b ->c) -> Maybe a -> Maybe b -> Maybe c
fmap2 f a b = f <$> a <*> b
fmap3 f a b c = f <$> a <*> b <*> c
...
```

Czyli  pozostaje n=0;
dla `Maybe` można użyć `Just`, ale chcemy stworzyć ogólny mechanizm:

``` haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

Także `<$>` (czyli `fmap`) da się wyrazić:

``` haskell
f <$> a = pure f <*> a
```

W rezultacie

``` haskell
ghci> pure (+) <*> Just 2 <*> Just 3
Just 5
```

### Bro, do U even liftA2 ?

Funkcje dwuargumentowe są na tyle powszechne, że `Applicative` ma odpowiednik `fmap2`:

``` haskell
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x = (<*>) (fmap f x)

(<*>) :: f (a -> b) -> f a -> f b
(<*>) = liftA2 id
```

![](https://i.kym-cdn.com/entries/icons/mobile/000/009/740/DoULift.jpg
)

### Implementacja Applicative dla Maybe

``` haskell
instance Applicative Maybe where
    -- pure :: a -> Maybe a
    pure = Just
    -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Just g <*> Just x = Just (g x)
    _      <*> _      = Nothing

```


### Programowanie z efektami

Zaczęliśmy od uogólnienia `map` na inne kolekcje i funkcje wieloargumentowe.

Ważniejszą motywacją jest jednak programowanie z efektami; używaliśmy przykładu **Maybe**<br />
gdzie "efektem" jest porażka,
ale podobny charakter mają inne rodzaje efektów, np.

``` haskell
ghci> :t getLine
getLine :: IO String
ghci> (++) <$> getLine <*> getLine    -- albo: liftA2 (++) getLine getLine
Ala
bama
"Alabama"
```
Biblioteka standardowa zawiera funkcję sekwencjonującą obliczenia

``` haskell
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs
```

Na kolejnych wykładach poznamy bardziej ogólne mechanizmy.

### Na skróty
Czasami przy łączeniu efektów `<*>` interesuje nas tylko jeden z wyników (ale oba efekty)

``` haskell
ghci> const <$> putStr "Ala" <*> putStrLn "bama"
Alabama

ghci> flip const <$> putStr "Enter something: " <*> getLine
Enter something: 42
"42"
```

Możemy to zapisać prościej przy uzyciu skrótów

``` haskell
putStr "Ala" <* putStrLn "bama"
putStr "Enter something: " *> getLine
```
zdefiniowanych w klasie **Applicative** jako

```
(*>) :: Applicative f => f a -> f b -> f b
a1 *> a2 = (id <$ a1) <*> a2
-- essentially the same as liftA2 (flip const), but if the Functor instance has an optimized (<$)
-- it may be better to use that instead.

(<*) :: f a -> f b -> f a
(<*) = liftA2 const
```

### Either

**Maybe** daje tylko informacje czy obliczenie się powiodło czy nie.<br/>
Możemy dodać komunikaty o błędach przy pomocy **Either**:

``` haskell
data Either error result = Left error | Right result
```

**Either** jest dwuargumentowym konstruktorem typu,<br/>
ale po ustaleniu pierwszego argumentu `(Either e)` jest jednoargumentowym

``` haskell
instance Functor (Either e) where
  fmap _ (Left e) = Left e
  -- Uwaga: "Left e" ma różne typy po lewej i po prawej, nie można `fmap f x@(Left e) = x`
  fmap f (Right r) = Right (f r)

instance Applicative (Either e) where
  pure = Right
  -- pure a = Right a
  (Right f) <*> (Right x) = Right (f x)
  (Left e)  <*>  _        = Left e      -- uwaga jak przy Functor
  _         <*> (Left e)  = Left e

```

### Either odwrotnie? (Rehtie?)

``` haskell
instance Functor (Either e) where
  fmap _ (Left e) = Left e
  fmap f (Right r) = Right (f r)
```

A co jeśli chcielibysmy odwrócić znaczenie Left i Right?

``` haskell
-- Uwaga: to nie jest poprawny Haskell

instance Functor (Either ? e) where
  fmap _ (Right e) = Right e
  fmap f (Left r) = Left (f r)
```

tak niestety nie można...

### Ey, what about the ring trick?

![](https://ficquotes.com/images/quotes/leon-leon-the-professional-14634.jpg)


### The ~~ring~~ Flip trick

Przez analogię do funkcji

``` haskell
flip f x y = flip f y x
```

możemy zdefiniować "funktor wyższego rzędu":

``` haskell
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

**Ćwiczenie:** spróbuj napisać typ funkcji `pamf`

### That's the ring trick

![](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEgkEts7w_B9dxmokq5M8yFOS0KgGZPrLRoDlCeuescCPs3hg5veAlnet6CldcHrwXgxnhA2BKo4B5ODHNo6lBqapSvzyZAX98Ur1LAvq1FlFEBtbm6dYtZuBt9cwNm_ziqthBCrcs9mcWY/s1600/pierscionek.png)


### Pary

W przypadku Either nie miało to może sensu, ale co z parami? łatwo zastosowac funkcję do drugiego elementu:

``` haskell
data Pair a b = Pair a b

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

-- >>> fmap succ (1,'x')
-- (1,'y')
```

Dla pierwszego elementu mozemy posłuzyć się sztuczką z `Flip`:

```
instance Functor (Flip Pair a) where
  fmap f (Flip (Pair a b)) = Flip (Pair (f a) b)

instance Functor (Flip (,) a) where
  fmap f (Flip (a, b)) = Flip ((f a), b)

-- >>> pamf succ ('x','a')
-- (y','a')
```

NB `pamf` jest uniwersalne

### Wiele wyników

Czysta funkcja daje dokładnie jeden wynik dla danego argumentu.<br/>
Obliczenia zawodne czasem nie dają wyniku, dlatego używamy `Maybe`.

A co z obliczeniami, które moga dać wiele wyników ("niedeterminizm")?<br/>
Możemy użyć list (chociaż są lepsze rozwiązania):

``` haskell
instance Applicative [] where
    -- pure :: a -> [a]
    pure x = [x]
    -- (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs, x <- xs]
```

Intuicja: obliczenie dla funkcji daje listę możliwości, podobnie obliczenie dla argumentu;<br/> obliczenie dla aplikacji da wszystkie możliwe kombinacje:

``` haskell
ghci> [(+1), (*2)] <*> [5, 7]
[6,8,10,14]
ghci> pure (+1) <*> [1,2,3]
[2,3,4]
ghci> pure (+1) <*> pure 6 :: [Int]
[7]
ghci> [(+1), (*2)] <*> pure 6
[7,12]
```

### Uogólnienie: klasa **Alternative**

``` haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]         -- v+
  some v = (:) <$> v <*> many v
  many :: f a -> f [a]         -- v*
  many v = some v <|> pure []

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> r = r
  l       <|> _ = l
```

przykłady użycia **Alternative** zobaczymy przy omawianiu zagadnień parsingu.



# Bonus

Pytania?

## Tour de force - palindromy
``` haskell
ghci> palindrome = (==) <*> reverse
ghci> palindrome "ala"
True
ghci> palindrome "ela"
False
ghci> palindrome "kajak"
True
```
## Tour de force - palindromy

<img src="https://imgur.com/FCgfKjB.png" width=1600 height=900>

### Funkcje jako Applicative

Konstruktor typu funkcji `(->)` jest dwuargumentowy<br/>
Ustalając dziedzinę możemy uzyskac jednoargumentowy: `((->) r)`

``` haskell
instance Applicative ((->) r) where
  pure x y = x
  (<*>) f g z = f z(g z)
```

Pamiętacie kombinatory?

```
K x y = x
S f g z = f z(g z)
```

funkcje są strukturą aplikatywną

## Prawa dla Applicative

``` haskell
pure id <*> x  =  x                              -- identity
pure (g x)     =  pure g <*> pure x              -- homomorphism
x <*> pure y   =  pure (\g -> g y) <*> x         -- interchange
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z   -- composition
```

Te prawa nie są tak groźne jak wyglądają:

1. Pierwsze znamy już dla `fmap` (`pure id <*> x = fmap id x = x`)
2. Drugie mówi że `pure` zachowuje aplikację
3. Trzecie mówi, że obliczenia czyste można wykonać przed albo po obliczeniu z efektami
4. Czwarte zaraz wyjaśnimy

Prawa te formalizują intuicję, że `pure` reprezentuje obliczenie czyste (bez efektów).

Zapewniają także, że każde wyrażenie aplikatywne da się zapisać w postaci

``` haskell
pure g <*> x1 <*> ... <*> xn

```
czyli
``` haskell
g <$> x1 <*> ... <*> xn
```

### Prawa dla Applicative - identity

``` haskell
pure id <*> x  =  x                              -- identity
```

To prawo jest odpowiednikiem pierwszego prawa dla `fmap`:

``` haskell
fmap id x = x
```

i podobnie jak ono mówi o zachowywaniu struktury (`pure f <*> x = fmap f x`)

### Prawa dla Applicative - composition

``` haskell
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z   -- composition
```

Pamiętając, że `<*>` uogólnia aplikację, to prawo uogólnia własność złożenia funkcji

``` haskell
x (y  z) = (.) x y z
```

Na przykład

``` haskell
ghci> (.) (+1) (*2) 3
7
ghci> (+1) ((*2) 3)
7

ghci> (pure (.) <*> [(+1)] <*> [(*2)]) <*> [1,2,3]
[3,5,7]
ghci> [(+1)] <*> ([(*2)] <*> [1,2,3])
[3,5,7]
```

### Prawa dla Applicative - homomorphism

Homomorfizm to mapowanie algebr zachowujące ich strukturę, na przykład dla słów z konkatenacją</br>
(albo dowolnego monoidu)


$$ f(\epsilon) = \epsilon $$
$$ f(u\cdot v) = f(u)\cdot f(v) $$

<!--
f(ε) = ε

f(uv) = f(u)f(v)
-->

``` haskell
pure id <*> x  =  x                              -- identity
pure (g x)     =  pure g <*> pure x              -- homomorphism
```

Przy czym tutaj zmienia się nośnik i operacja. Odpowiednikiem dla słów byłby homomorfizm

$$h : \langle \Sigma^*, \epsilon, \cdot\rangle \to \langle N, 0, +\rangle $$
$$ h(\epsilon) = 0 $$
$$ h(u\cdot v) = h(u) + h(v) $$


### Prawa dla Applicative - interchange

``` haskell
x <*> pure y   =  pure (\g -> g y) <*> x         -- interchange
```

To prawo mówi, że obliczenia czyste można wykonać przed albo po obliczeniu z efektami

### Solo: A Star Wars Story

Haskell nie ma krotek jednoelementowych, ale możemy zdefiniować (patrz `Data.Tuple`)

```haskell
data Solo a = MkSolo a

instance Functor Solo where
    fmap f (Solo a) = Solo (f a)

instance Applicative Solo where
    pure = Solo
    Solo f <*> Solo x = Solo (f x)
```

**Ćwiczenie:** sprawdź, że prawa `Applicative` zachodzą dla `Solo`.



### Tour de force - soczewki

Chcemy zdefiniować typ pozwalający skupić się na pewnym fragmencie struktury danych.

Nazwijmy go "soczewką":

``` haskell
type Lens a b
view :: Lens a b -> a -> b                -- odczytaj pole
set  :: Lens a b -> b -> a -> a           -- ustaw pole
over :: Lens a b -> (b -> b) -> (a -> a)  -- zmodyfikuj pole
```


Znów przydadzą się funktory wyższego rzędu:

``` haskell
newtype I x = I { unI :: x }

instance Functor I where
  fmap f  = I . f . unI

newtype K b x = K { unK :: b }

instance Functor (K b) where
  fmap f (K b) = K b


type Lens a b = forall t . Functor t => (b -> t b) -> (a -> t a)

view :: Lens a b -> a -> b
view l a = unK (l K a)

over :: Lens a b -> (b -> b) -> (a -> a)
over l f = unI . l (I . f)
-- over l f a = unI $ l f' a where f' b = I (f b)

set :: Lens a b -> b -> a -> a
set l x = over l (const x)
```

### Tour de force: `fmap fmap fmap`

``` haskell
ghci>  fmap fmap fmap negate (+) 2 3
-5
```

WTF?

### Tour de force: `fmap fmap fmap` explained

``` haskell
instance Functor (->r) where
  fmap = (.)

fmap fmap fmap = fmap . fmap


(f . g) x = f(g x)

fmap fmap fmap negate (+) 2 3
= (fmap . fmap) negate (+) 2 3
= ((fmap . fmap) negate) (+) 2 3
= fmap (fmap negate) (+) 2 3
= (fmap (fmap negate) (\x -> (\y -> x + y))) 2 3
= (fmap negate . ((\x -> (\y -> x + y))) 2 3
= (fmap negate((+) 2 )) 3
= fmap negate (+2) 3
= negate . (+2) 3
= negate ((+2) 3)
= negate 5
= -5
```

``` haskell
ghci>  fmap fmap fmap negate (+) 2 3
-5
```
