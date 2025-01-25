---
title: Programowanie Funkcyjne
subtitle: Wyższe klasy
author:  Marcin Benke
date: Wykład 6/2025
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

Dane drzewo, jak uzyskac drzewo tego samego kształtu, ale złożone z samych jedynek?

I podobnie dla list ...

``` haskell
allOnes :: Functor t => t a -> t Char
allOnes t = fmap (const '1') ts
```

ale można krócej:

```
allOnes t = '1' <$ t
-- (<$) = fmap . const

ghci> '1' <$ "abc" 
"111"
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

Później zauważono, że drugie podejście jest lepsze:

``` haskell
(<*>) ::  Maybe (a -> b) -> Maybe a -> Maybe b

fmap2 :: (a -> b ->c) -> Maybe a -> Maybe b -> Maybe c
fmap2 f a b = f <$> a <*> b
fmap3 f a b c = f <$> a <*> b <*> c
...
```

Czyli `<$>` i `<*>` pozwalają wyrazić `fmap_n` dla n>0; pozostaje n=0.<br />
Dla `Maybe` można użyć `Just`, ale chcemy stworzyć ogólny mechanizm:

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
ghci> (++) <$> getLine <*> getLine
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

instance Applicative (Either e) where
  pure = Right
  (Right f) <*> (Right x) = Right (f x)
  (Left e)  <*>  _        = Left e
  _         <*> (Left e)  = Left e
```

**Either** jest dwuargumentowym konstruktorem typu,<br/>
ale po ustaleniu pierwszego argumentu `(Either e)` jest jednoargumentowym

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

## Tour de force - palindromy

<img src="https://imgur.com/FCgfKjB.png" width=1600 height=900>

### Funkcje

Konstruktor typu funkcji `(->)` jest dwuargumentowy<br/>
Ustalając dziedzinę możemy uzyskac jednoargumentowy: `((->) r)`

``` haskell
instance Applicative ((->) r) where
  pure x y = x
  (<*>) f g z = f z(g z)
```

Pamiętacie kombinatory?

```
K x y =x
S f g z = f z(g z)
```

funkcje są strukturą aplikatywną

### Prawa dla Applicative

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
4. Czwarte to łączność dla `<*>` (z poprawką na asymetrię)

Prawa te formalizują intuicję, że `pure` reprezentuje obliczenie czyste (bez efektów).

Zapewniają także, że każde wyrażenie aplikatywne da się zapisać w postaci

``` haskell
pure g <*> x1 <*> ... <*> xn

```
czyli
``` haskell
g <$> x1 <*> ... <*> xn
```

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


# Foldable, Traversable

Zajmiemy się teraz typowymi schematami przetwarzania struktur danych.

Tak jak poprzednio uogólniliśmy `map`, teraz uogólnimy `fold`
...a potem jeszcze raz `map`.

Wzorce projektowe są często opisywane przy uzyciu klas.
Omówimy teraz klasy

- Monoid (łączenie wyników)
- Foldable (transformacje czyste)
- Traversable (transformacje z efektami)

## ~~Star~~ Monoid Wars

Z klasą `Monoid` juz się zetknęliśmy, teraz zobaczymy ją w akcji


``` haskell
class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a

  mappend :: a -> a -> a
  mappend = (<>)
```

 - `mappend` jest w klasie `Monoid` ze względu na wsteczną kompatybilność (kiedyś nie było odrębnej klasy `Semigroup`)


``` haskell
instance Semigroup [a] where
  (<>) = ( ++ )

newtype Sum a = Sum {getSum :: a}   
instance Num n => Semigroup (Sum n) where
  Sum x <> Sum y = Sum (x + y)

newtype All = All {getAll :: Bool}
instance Semigroup All where (<>) = (&&)

newtype Product a = Product {getProduct :: a}
newtype Any = Any {getAny :: Bool}
```

### ~~Empire~~ Endo strikes back

Ciekawym przypadkiem monoidu są endomorfizmy, to jest funkcje w obrębie danego typu

```haskell
newtype Endo a = Endo {appEndo :: a -> a}

Endo f <> Endo g = Endo (f . g)
mempty = Endo id
```

```haskell
>>> let computation = Endo ("Hello, " ++) <> Endo (++ "!")
>>> appEndo computation "Haskell"
"Hello, Haskell!"

>>> let computation = Endo (*3) <> Endo (+1)
>>> appEndo computation 1
6
```

za chwilę zobaczymy jak można je wykorzystać.

### Return of the ~~Jedi~~ Monoid
 Monoid pozwala na utworzenie jednej wartości na podstawie zawartości kolekcji
 (czyli to o jest esencją funkcji typu `fold`);<br/>
 na przykład obliczenie sumy czy stworzenie listy elementów kolekcji.

Na przykład dla list moglibyśmy napisać


``` haskell
fold :: Monoid a => [a] -> a
fold [] = mempty
fold (x:xs) = x <> fold xs
-- fold = foldr mempty (<>)

ghci> fold ["Ala","bama"]
"Alabama"
```
niestety `fold [1..5]` nie zadziała, bo nie wiadomo jaką operację mamy na myśli.

Dlatego przyda się funkcja

``` haskell
foldMap :: Monoid b => (a -> b) -> [a] -> b

ghci> foldMap Product [1..5]
Product {getProduct = 120}
ghci> foldMap Sum [1,2,3]
Sum {getSum = 6}
```

## Foldable

Podobnie jak `fmap` uogólnia `map` na inne kolekcje, tak `Foldable` uogólnia `fold*`:

``` haskell
class Foldable t where
  fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl, foldl' :: (b -> a -> b) -> b -> t a -> b
  {-# MINIMAL foldMap | foldr #-}
```
(jest więcej metod, tu najwazniejsze).

Na przykład dla drzew

```haskell
data ETree a = Leaf a | Bin (ETree a) (ETree a) deriving (Show)

instance Foldable ETree where
    fold (Leaf x) = x
    fold (Bin l r) = fold l <> fold r
    
    foldMap f (Leaf x) = f x
    foldMap f (Bin l r) = foldMap f l <> foldMap f r
```

### Składane drzewo

```haskell
instance Foldable ETree where
    fold :: Monoid m => ETree m -> m
    fold (Leaf x) = x
    fold (Bin l r) = fold l <> fold r

    foldMap :: Monoid m => (a -> m) -> ETree a -> m
    foldMap f (Leaf x) = f x
    foldMap f (Bin l r) = foldMap f l <> foldMap f r

    foldr :: (a -> b -> b) -> b -> ETree a -> b
    foldr f v (Leaf x) = f x v
    foldr f v (Bin l r) = foldr f (foldr f v r) l

    foldl :: (b -> a -> b) -> b -> ETree a -> b
    foldl f v (Leaf x) = f v x
    foldl f v (Bin l r) = foldl f (foldl f v l) r
```

Dlaczego `foldl/foldr` wygladają w ten sposób?

A pamiętacie funkcję `flatten`?

### ~~The Force~~ Deforestation Awakens
pamiętacie funkcję `flatten`?

``` haskell
revA xs ys = rev xs ++ ys
reverse xs = revA xs []

flatA :: flatA :: ETree a -> [a] ->[a]
flatA (Leaf x) xs = x:xs
-- foldr f v (Bin l r) = foldr f (foldr f v r) l
flatA (Bin l r) xs = flatA l (flatA r xs)

flatten :: ETree a -> [a]
flatten t = flatA t []

-- flatten = foldr (:) []
```

`Foldable` pozwala nam działać na kolekcjach jak na listach, bez przekształcania ich w listy (deforestacja) ...<br/>
...chociaż w każdej chwili możemy to łatwo zrobić.

```haskell
toList :: Foldable t => t a -> [a]
toList t = build (\ c n -> foldr c n t)  -- = foldr (:) [] t
```

Funkcja `toList` używa `build`, aby ułatwić ewentualną  zastosowanie reguły `foldr/build` (deforestacja).

### Revenge of the ~~Sith~~ List

`Foldable` pozwala nam działać na kolekcjach jak na listach:

``` haskell
etree4 = Bin (Bin (Leaf 1) (Leaf 2)) (Bin (Leaf 3) (Leaf 4))
4
ghci> elem 3 etree4
True
ghci> maximum etree4
4
ghci> product etree4
24
```

...stąd na pierwszy rzut oka dziwne odpowiedzi `ghci` na pytanie o typy prostych funkcji:

``` haskell
ghci> :t length
length :: Foldable t => t a -> Int
```

### ` {-# MINIMAL foldMap | foldr #-}`

klasa `Foldable` ma wiele funkcji, ale wystarczy zaimplementowac jedną, inne dają się wyrazić za jej pomocą

```haskell
fold = foldMap id
toList = foldr (:) []          -- dla list to identyczność
length = foldl' (\c _ -> c+1) 0
```

### ` {-# MINIMAL foldMap | foldr #-}`

Trochę trudniejsza jest wzajemna wyrażalność `foldMap` i `foldr`

```haskell
foldMap :: Monoid m => (a -> b) -> t a -> b
foldMap f = foldr (mappend . f) mempty
-- f :: a -> b; mappend :: b -> (b -> b); mappend . f  :: a -> b -> b; mempty :: b

foldr :: (a -> b -> b) -> b -> t a -> b
foldr f z t = appEndo (foldMap (Endo . f) t) z 
```

Implementacja `foldr` wykorzystuje monoid `Endo`:

- każdy element jest mapowany na jego działanie; 
- `foldMap` sklada je w jedną funkcję
- ta funkcja jest stosowana do wartości `z`

## Traversable

Poznaliśmy już `fmap`

``` haskell
class Functor t where
    fmap :: (a -> b) -> t a -> t b
```

jako uogólnienie `map`:

```haskell
map :: (a -> b) -> [a] -> [b]
map g [] = []
map g (x:xs) = (:) (g x) (map g xs)
```

Załóżmy teraz, że użycie `g` może się nie powieść (wynikiem jest Maybe). 
Możemy sobie poradzić korzystając z `Applicative`:

```haskell
traverse :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse g [] = pure []
traverse g (x:xs) = (:) <$> g x <*> traverse g xs
```
na przykład

```haskell
dec n = if n>0 then Just(n-1) else Nothing
>>> traverse dec [1,2,3]
Just [0,1,2]
>>> traverse dec [2,3,0]
Nothing
```
### Klasa Traversable

Klasa `Traversable` uogólnia ten schemat na inne kolekcje i przypadki `Applicative`:

```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f b) -> f (t b)
  sequenceA = traverse id  -- a ~ f b
```

`traverse` dla każdego elementu `a` kolekcji wybiera akcję typu `f b`,
wykonuje te akcje w kolejności i zbiera wyniki

Skoro `f` jest konstruktorem obliczenia (efektu), a `t` konstruktorem kolekcji, to `f (t b)`
jest obliczeniem dającym kolekcję wyników.

```haskell
>>> sequenceA [Just 1, Just 2, Just 3]
Just [1,2,3]

>>> traverse Just [1,2,3]
Just [1,2,3]
```

### Trawers drzewa

```haskell
instance Traversable ETree where
    traverse :: Applicative f => (a -> f b) -> ETree a -> f (ETree b)
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (Bin l r) = Bin <$> traverse f l <*> traverse f r
 
    sequenceA :: Applicative f => ETree (f a) -> f (ETree a)
    sequenceA (Leaf x) = Leaf <$> x
    sequenceA (Bin l r) = Bin <$> sequenceA l <*> sequenceA r

-- >>> traverse dec (Bin (Leaf 1) (Leaf 2))
-- Just (Bin (Leaf 0) (Leaf 1))

-- >>> traverse dec (Bin (Leaf 1) (Leaf 0))
-- Nothing

-- >>> sequenceA (Bin (Leaf (Just 1)) (Leaf (Just 2)))
-- Just (Bin (Leaf 1) (Leaf 2))

-- >>> sequenceA (Bin (Leaf (Just 1)) (Leaf Nothing))
-- Nothing
```

## Ćwiczenia

- Powtórz omawiane konstrukcje (Functor, Foldable, Traversable) dla drzew z wierzchołkami wewnetrznymi:

``` haskell
data ITree a = Empty | Node a (ITree a) (ITree a) deriving (Show)
```

- Wiemy jak zdefiniować `fmap2` przez `<*>`. A czy da się na odwrót?

``` haskell
f <*> g = fmap2 ...
```

- przy pomocy poznanych dziś mechanizmów (Applicative/Foldable/Traversable)
napisz funkcję `allCombinations`:

```
>>> allCombinations ["xyz","ab", "12"]
["xa1","xa2","xb1","xb2","ya1","ya2","yb1","yb2","za1","za2","zb1","zb2"]
```

- (*) Rozważmy klasę

```haskell
class Liftable f where
  unit :: f ()
  pair :: f a -> f b -> f (a, b)
```

pokaż, że jest ona wzajemnie definiowalna z  `Applicative` (przynajmniej w sensie typów)

# The End(o)


``` haskell

```

# Bonus

### Tour de force: fmap fmap fmap

``` haskell
instance Functor (->r) where
  fmap = (.)

fmap fmap fmap = fmap . fmap

ghci>  fmap fmap fmap negate (+) 2 3
-5
```

