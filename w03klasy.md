---
title: Programowanie Funkcyjne
subtitle: Klasy typów
author:  Marcin Benke
date: Wykład 3, 10 marca 2025
---

## Klasy typów

Rozważmy funkcję sprawdzającą, czy element należy do listy:

``` haskell
el x [] = False
el x (y:ys) = (x==y) || el x ys
```

Można by oczekiwać, że będzie ona miała typ 
`a -> [a] -> Bool`.

Ale pamiętajmy, że nie da się zdefiniować w pełni polimorficznej równości;
nie dla wszystkich typów zreszta da się sensownie zdefiniować jakąkolwiek równość.

Dlatego funkcja `el` będzie miała typ ograniczony (*constrained type*):

``` haskell
el :: Eq a => a -> a -> Bool
```

oznacza to, że `el` działa dla typów, które mają zdefiniowaną równość 

(dokładniej: przynależą do klasy typów `Eq`)


### Klasy typów

Pojęcie klasy nie jest tym samym co w językach obiektowych.

(tam do klas należą obiekty, tu: typy)

Określa raczej protokół, który musi wypełnić typ, by należeć dodanej klasy <br/>
(podobnie jak trait w Rust bądź interface w Javie).

Innym skojarzeniem jest mechanizm przeciążania operatorów, ale klasy pozwalają na  wiele więcej

Funkcja typu `C a => t` jest przeciązona ze względu na `a`:

- działa dla wszystkich typów `a`, które należą do klasy `C`
- może działać w różny sposób dla różnych typów<br/>
    (w odróżnieniu od funkcji polimorficznych parametrycznie jak `a -> a`);

W ogólności klasa typów jest relacją na typach (zwykle jednoargumentową).

Nazwy klas muszą się zaczynać z wielkiej litery

### Definicja klasy 
Zdefiniujmy własną klasę typów Porównywalnych:

``` haskell
data TakNie  = Tak | Nie
class Por a where
  (===) :: a -> a -> TakNie
  (=/=) :: a -> a -> TakNie
```

teraz możemy powiedzieć, że elementy typu `TakNie` są Porównywalne (pamiętając o wcięciach!):

``` haskell
instance Por TakNie where
  Tak === Tak = Tak
  Nie === Nie = Tak
  _   ===  _  = Nie

  a =/= b = nie(a === b)
```

Zauważmy, że to ostatnie równanie będzie się pojawiało w każdej prawie definicji instancji,<br/>
 możemy więc je przenieść do definicji klasy jako domyślną definicję     `(=/=)`


### Klasa Eq

tak właśnie jest zdefiniowana klasa `Eq`:


``` haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

  x /= y = not(x == y)
  x == y = not(x /= y)
  {-# MINIMAL (==) | (/=) #-}
```

Definiując instancję klasy `Eq` potrzebujemy zaimplementować jedną z tych metod.

#### Własności
Równość powinna być relacją równoważności,<br/>
ponadto dla każdego typu `b` z klasy `Eq`<br/>
i publicznej funkcji `f :: a -> b` powinno zachodzić
<br />

jeśli `x == y = True` to `f x == f y = True`

("publicznej" dlatego, że mogą być wartości różniące się reprezentacją, ale "moralnie" równe).

Ta własność jest trochę nieprecyzyjna, ale równość jest trudna.

### Porządki: klasa Ord

``` haskell
data Ordering = LT | EQ | GT

class  (Eq a) => Ord a  where
 compare              :: a -> a -> Ordering
 (<), (<=), (>=), (>) :: a -> a -> Bool
 max, min             :: a -> a -> a
 {-# MINIMAL compare | (<=) #-}

```

Notacja `(Eq a) => Ord a` oznacza "typ **a** jest instancją **Ord** jeśli jest instancją **Eq**, a ponadto są zdefiniowane funkcje..."

#### Własności
Oczekujemy, że `(<=)` jest częściowym porządkiem, ponadto metody spełniają pewne oczywiste zależności, np.


```
  x >= y = y <= x
  x < y = compare x y == LT
  min x y == if x <= y then x else y = True
```
**Uwaga notacyjna**

- `==` oznacza równość z klasy `Eq` (rozstrygalną); 
- `=` oznacza, że dwa wyrażenia mają tę samą wartość (pojęcie z metajęzyka, niekoniecznie rozstrzygalne)

### Klasa Show
Klasa **Show** zawiera funkcje dające tekstową reprezentację wartości. W skrócie

``` haskell
class Show a where
  show :: a -> String
  ...
```

`ghci` potrafi wydrukować tylko elementy typów, które są instancjami klasy **Show**

``` haskell
ghci> Tak
No instance for (Show TakNie)
 arising from a use of `print' at ...
Possible fix: add an instance declaration for
  (Show TakNie)
```

powinniśmy więc dodać definicję, która powie w jaki sposób typ `TakNie` może spełnić protokół **Show**, ale jest prostszy sposób

### Automatyczne instancje
Instancje dla klas typu **Eq** czy  **Show** są przeważnie nudne
i przewidywalne, kompilator mógłby je sam wygenerować...

... i potrafi, możemy go poinstruować w tym kierunku przy pomocy dyrektywy `deriving`, np.

```  haskell
data TakNie = Tak | Nie
  deriving (Eq, Show)
```
Ten mechanizm ma zastosowanie tylko do klas standardowych<br />
(istnieją bogatsze mechanizmy, ale nie będziemy się tu nimi zajmować).


### Ograniczony polimorfizm
  Zmienne typowe wystepujące w typach i instancjach mogą podlegać ograniczeniom, np.

``` haskell
  el :: Eq a => a -> [a] -> Bool

  instance Show a => Show (Tree a) where
```

  Czasami ograniczenie może mieć kilka elementów, np.

``` haskell
  fromDP :: (Eq a, Num a) => DP a -> SP a

  instance (Show a, Eq a, Num a) => Show (SP a) where
```

oznacza to, że `a` musi należec do wszystkich wymienionych klas.


### Przykłady nietrywialnych instancji

Możemy zdefiniować prostą uniwersalną funkcję `assertEqual`
``` haskell
assertEqual :: (Eq a, Show a) => a -> a -> IO ()         
assertEqual e a = when (e /= a) (prints ["Fail, expected:", show e, "actual:", show a])
```

Za to bardziej zaawansowane biblioteki do testów (QuickCheck) definiują na przykład
``` haskell
instance Testable Bool where  ...

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where ...

quickCheck :: Testable a => a -> IO ()
```

Taka konstrukcja pozwala testować funkcje o dowolnej liczbie argumentów<br/>
(o ile ich typy należą do klas `Show` i `Arbitrary`).

Do zagadnień związanych z testowaniem jeszcze wrócimy.

## Klasy numeryczne

Haskell ma bogatą hierarchię klas numerycznych,
klasyfikującą typy według dostepnych operacji:

```
Num
Real
Fractional
Integral
Floating
RealFrac
RealFloat
```

wygląda to groźnie, ale w praktyce wystarczy znać klasy `Num` oraz `Integral`
i uważać na dzielenie:

```
ghci> :t (/)
(/) :: Fractional a => a -> a -> a
ghci> :t div
div :: Integral a => a -> a -> a
```

Czyli `(/)` używamy dla typów zmiennoprzecinkowych i ułamków,<br/>
natomiast `div` dla całkowitych (`Int`, `Integer`).

Możemy oczywiście definiować własne typy (liczby zespolone, wektory, $Z_p$) i operacje arytmetyczne dla nich.

### Klasa Num

Wszystkie typy numeryczne należą do klasy `Num`:
``` haskell 
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```

Metoda `fromInteger` jest używana głównie dla przeciążania literałów całkowitych <br>
(`0` to tak naprawdę `fromInteger 0`)

Przydaje się też do konwersji `Integer -> Double`


### Klasa Integral

Typy `Int` oraz `Integer` należą do klasy `Integral`

``` haskell
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
```

używamy jej gdy chcemy zdefiniować funkcje działające zarówno na `Int` jak i `Integer`

Funkcji `toInteger` można użyć do konwersji `Int -> Integer`

Funkcji `fromIntegral` można użyć do konwersji `Int` na inne typy

``` haskell
ghci> xs = [1,2,3]
ghci> sum xs / length xs

error: No instance for ‘Fractional Int’ arising from a use of ‘/’
-- length :: [a] -> Int

ghci> sum xs / (fromIntegral (length xs))
2.0
```

**Real** to, wbrew nazwie, liczby wymierne, z metodą `toRational`. O Enum za chwilę.

## Inne klasy 

- Read
- Enum
- Bounded
- Półgrupy:
  - Semigroup
  - Monoid

### Klasa Read

Klasa `Read` jest poniekąd dualna do klasy `Show`:
pozwala na odczytanie wartości z jej reprezentacji napisowej.

To zwykle trudniejsze niż `show`, na razie wystarczy nam znać funkcję

``` haskell
read :: (Read a) => String -> a
```

```
ghci> read "2" + read "3"
5
```

Klasa Read ma instancje dla standardowych typów, np.

``` haskell
instance Read Bool
instance Read Char
instance Read Double
instance Read Float
instance Read Int
instance Read Integer
instance Read a => Read [a]
instance Read a => Read (Maybe a)
instance (Read a, Read b) => Read (Either a b)
instance (Read a, Read b) => Read (a, b)
-- itd.
```


### Wskazywanie typu

W przypadku funkcji przeciążonych zwn. wynik (jak `read`) nie zawsze widać
o jaki typ nam chodzi, zwłaszcza w REPL np.

```
ghci> :type 42
42 :: Num p => p

ghci> :type read
read :: Read a => String -> a

ghci> read "42"
*** Exception: Prelude.read: no parse
```

Możemy wtedy użyć konstrukcji wskazywania typu

```
wyrażenie :: typ
```

Na przykład

```
ghci> show (42::Float)
"42.0"

ghci> read "42"::Float
42.0
```

### Wskazywanie typu

Jeżeli użyjemy wyniku jako argumentu, problem się rozwiąże 


``` haskell
ghci> take (read "3") "Haskell"
"Has"
```

chyba, że ta funkcja też jest przeciążona

``` haskell
print :: Show a => a -> IO ()
ghci> print(read "42")
*** Exception: Prelude.read: no parse
```

GHCi próbuje zgadywać, ale nie zawsze trafnie. W braku przesłanek ucieka do typu `()`:


``` haskell
ghci> read "()"
()
```

### Deriving Read

Napisanie funkcji `read` dla naszego typu może być trudne.

Na szczęście możemy skorzystać z `deriving`:

``` haskell
data TakNie = Nie | Tak deriving(Enum, Show, Read)
-- >>> read "Tak" :: TakNie
-- Tak
```

Notacja `-- >>>` to doctest (oryginalnie bodaj z Pythona, przyjął się w Haskellu, wspierany przez VSCode)

Do kwestii konstrukcji wartości z napisów (analizy składniowej) jeszcze wrócimy.

### Klasa Enum

Klasa typów wyliczeniowych (niekoniecznie skończonych):
``` haskell
class  Enum a  where
 succ, pred     :: a -> a
 toEnum         :: Int -> a
 fromEnum       :: a -> Int
 enumFrom       :: a -> [a]           -- [n..]
 enumFromThen   :: a -> a -> [a]      -- [n,n'..]
 enumFromTo     :: a -> a -> [a]      -- [n..m]
 enumFromThenTo :: a -> a -> a -> [a] -- [n,n'..m]
 {-# MINIMAL toEnum, fromEnum #-}
 ```

``` haskell
ghci> [1,3..9]
[1,3,5,7,9]
ghci> enumFromThenTo 1 3 9
[1,3,5,7,9]
```


### Przykład Enum

``` haskell 
data TakNie = Nie | Tak deriving(Enum, Show, Read)

-- >>> fromEnum Tak
-- 1


-- | Uwaga: w [Nie ..] obowiązkowa spacja!
-- >>> [Nie .. Tak]
-- [Nie,Tak]
-- >>> [Nie ..]
-- [Nie,Tak]
-- >>> toEnum 0 :: TakNie
-- Nie
```

``` haskell
ghci> succ Nie
Tak
ghci> pred Tak
Nie
ghci> succ Tak
*** Exception: succ{TakNie}: tried to take `succ' of last tag in enumeration
```


### Półgrupy

Dla nietórych typów istnieje naturalna operacja łączenia elementów (konkatenacja, dodawanie)

Tę własność opisuje klasa Semigroup:

``` haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

Jeżeli operacja ma element neutralny, to mamy *monoid* (półgrupę z jedynką):

``` haskell 
class Semigroup a => Monoid a where
  mempty :: a
  mconcat :: [a] -> a
  {-# MINIMAL mempty | mconcat #-}

  mappend :: a -> a -> a  -- ze względu na wsteczną kompatybilność; mappend = (<>)
```


Oczekiwane własności

``` haskell
x <> mempty = x
mempty <> x = x

(x <> y) <> z = x <> (y <> z)
```

### Przykłady

``` haskell
instance Semigroup [a] where
  (<>) = ( ++ )

instance Monoid [a] where
  mempty = []

newtype Sum n = Sum n

instance Num n => Semigroup (Sum n) where
  Sum x <> Sum y = Sum (x + y)

instance Num n => Monoid (Sum n) where
  mempty = Sum 0

newtype Product n = Product n
-- ...
```

## Jeszcze o Show
 - metoda shows, typ ShowS
 - metoda showsPrec
 - przykłady

### Pełniejsza definicja klasy Show

``` haskell
type ShowS = String -> String

class Show a where
  showsPrec :: Int    -- ^ the operator precedence of the enclosing
                        -- context (a number from @0@ to @11@).
                        -- Function application has precedence @10@.
              -> a      -- ^ the value to be converted to a 'String'
              -> ShowS
  showsPrec _ x s = show x ++ s

  show :: a -> String
  show = shows ""
  showList :: [a] -> ShowS

shows = showsPrec 0
 
-- showList ma domyślną definicję, która daje "[x,y,...]"
-- podstawowe zastosowanie showList to wypisywanie napisów w instancjach Show Char, Show [Char]
-- type String = [Char]
```

Na przykładzie wyrażeń arytmetycznych postaramy się wyjaśnić o co tu chodzi.

(a jeśli nie wiadomo o co chodzi, to chodzi o nawiasy)


### shows, ShowS

Przy naiwnym tworzeniu instancji `Show` wielokrotnie używamy konkatenacji,
która jest nieefektywna.

W języku obiektowym pewnie użylibyśmy czegoś w rodzaju wzorca **Builder**.

W Haskellu używamy reprezentacji ~~kontynuacyjnej~~ funkcyjnej:

```
type ShowS = String -> String
```

napis (ogólniej: lista) jest reprezentowany jako funkcja, która dokleja swój napis przed argumentem

``` haskell
showEmpty :: ShowS
showEmpty s = s

showChar :: Char -> ShowS
-- showChar c s = (c:s)
showChar = (:)
```

Konkatenacja to złożenie funkcji; przejście do zwykłych napisów: zastosuj funkcję do pustego napisu


``` haskell
showABC = showChar 'a' . showChar 'b' . showChar 'c'
show x = shows x ""
```

### Wyrażenia arytmetyczne - pierwsza próba

``` haskell
infixl 6 :+
infixl 7 :*
data Exp = N Int | Exp :+ Exp | Exp :* Exp

instance Show Exp where
  shows (e1 :+ e2) = 
    shows e1 . showString " + " . show e2
  shows (e1 :* e2) =
    shows e1 . showString " * " . show e2
  shows (N n) = shows n

exp1 = (N 2 :+ N 3) :* N 4

-- >>> exp1
-- 2 + 3 * 4
```

Oops.

### Wyrażenia arytmetyczne - druga próba

``` haskell
parens :: ShowS -> ShowS
parens s = showChar '(' . s . showChar ')'

instance Show Exp where
  shows (e1 :+ e2) =
    parens(shows e1 . showString " + " . shows e2)
  shows (e1 :* e2) =
    shows e1 . showString " * " . shows e2
  shows (N  n) = shows n
 
-- >>> (N 2 :+ N 3) :* N 4
-- (2 + 3) * 4

-- >>> N 2 :+ N 3 :+ N 4
-- ((2 + 3) + 4)
```

Teraz jest z kolei za dużo nawiasów

### Priorytety

``` haskell
infixl 6 :+
infixl 7 :*
-- showParen :: Bool -> ShowS -> ShowS

instance Show Exp where
  showsPrec p (x :+ y) = showParen(p>add_prec)
    (showsPrec add_prec x . (" + "++) . showsPrec (add_prec+1) y) where add_prec = 6
  showsPrec p (x :* y) = showParen(p>7)
    (showsPrec mul_prec x . (" * "++) . showsPrec (mul_prec+1) y) where mul_prec = 7
  showsPrec p (N n) = showsPrec p n     -- w razie potrzeby nawiasy dla liczb ujemnych

-- >>> (N 2 :+ N 3) :* N 4
-- (2 + 3) * 4

-- >>> N 2 :+ N 3 :+ N 4
-- 2 + 3 + 4

-- >>> N 2 :+ N (-1)
-- 2 + (-1)
```

Nawiasy wstawiamy tylko gdy kontekst ma wyższy priorytet niż bieżąca konstrukcja

## Jeszcze o Read
Podobnie jak `Show`, klasa `Read` ma inne metody niż `read :: Read a => String -> a`, najważniejsza jest

```haskell
readsPrec :: Read a => Int -> ReadS a
type ReadS a = String -> [(a, String)]
```

Poznamy lepsze sposoby analizy napisów, na razie zauważmy, że `ReadS` pozwala na modularyzację analizy.<br/>
Analizator (parser) wartości typu `a` bierze napis i listę możliwych rozbiorów - par `(a,resztaNapisu)`.

Na przykład dla drzew reprezentowanych jako S-wyrażenia (wyrażenia nawiasowe):
```haskell
-- >>> showTree $ Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
-- "(1 (2 () ()) (3 () ()))"

readTreeS :: [Char] -> [(ITree Int, [Char])]
readTreeS (' ':xs) = readTreeS xs
readTreeS ('(':')':xs) = [(Empty, xs)]
readTreeS ('(':xs) = 
    [(Node x l r, rest4) | 
            (x, rest1) <- readsInt xs, 
            (l, rest2) <- readTreeS rest1, 
            (r, rest3) <- readTreeS rest2, 
            (")", rest4) <- lex rest3]     -- lex :: ReadS String
readTreeS _ = []
```
To, że wynik jest listą pozwala na kreatywne wykorzystanie wycinanki

## Klasa IsString

``` haskell
-- | Class for string-like datastructures; used by the overloaded string
--   extension (-XOverloadedStrings in GHC).
class IsString a where
    fromString :: String -> a
```

Przykład użycia (warp - serwer HTTP): 

``` haskell
{-# LANGUAGE OverloadedStrings #-}
defaultSettings :: Settings
setServerName :: ByteString -> Settings -> Settings

mySettings = setServerName "kermit" defaultSettings
-- mySettings = setServerName (fromString "kermit") defaultSettings
```

Podobnie jak literały liczbowe, literaly napisowe mogą być zamieniane na odpowiedni typ.


## Zaawansowane klasy

- Klasy wieloparametrowe
- zależności funkcyjne
- klasy konstruktorowe

### Klasy wieloparametrowe

Powiedzmy, ze chcemy zdefiniować klasę kolekcji
``` haskell
class Collection c where
  insert :: e -> c -> c
  member :: e -> c -> Bool

instance Eq a => Collection [a] where
     insert x xs = x:xs
     member = elem
```
to się niestety nie skompiluje (co to jest `e`?)

### Klasy wieloparametrowe

``` haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
class Collection c e where
  insert :: e -> c -> c
  member :: e -> c -> Bool

instance Eq a => Collection [a] a where
     insert = (:)
     member = elem
```

NB musimy uzyć rozszerzeń wykraczających poza standard
Haskell 2010, stąd pragmy.

Typ kolekcji determinuje typ elementu

Można to wyrazić przez *zależności funkcyjne* (functional dependencies)

``` haskell
{-# LANGUAGE FunctionalDependencies #-}
class Collection c e | c -> e where
```

Skojarzenie z bazami danych jest słuszne.

### Inny przykład
Powiedzmy, że piszemy bibliotekę dla algebry liniowej (w duzym uproszczeniu):

``` haskell
data Vector = Vector Int Int deriving (Eq, Show)
data Matrix = Matrix Vector Vector
  deriving (Eq, Show)
```
i między innymi chcemy zdefiniować mnożenie: macierzy i macierzy przez wektor i skalar

``` haskell
(|*) :: Matrix -> Matrix -> Matrix
(|*) :: Matrix -> Vector -> Vector
(|*) :: Matrix -> Int -> Matrix
(|*) :: Int -> Matrix -> Matrix
```

``` haskell
class Mult a b c | a b -> c where
  (|*) :: a -> b -> c

instance Mult Matrix Vector Matrix where
instance Mult Int Matrix Matrix
```

Zależności funkcyjne są potrzebne aby typ wyniku był jednoznacznie określony.

### Ostrzeżenie

> Functional dependenciess are very, very tricky. 
- Simon Peyton Jones

Zależności funkcyjne są skomplikowanym mechanizmem.

Na szczęście raczej nie będzimy musieli ich pisać, ale dobrze jest umieć je czytać.

### Typy skojarzone (associated types)

Nowszym mechanizmem alternatywnym dla zalezności funkcyjnych są tzw. typy skojarzone:

``` haskell
class Collection c where
  type Elem c
  insert :: Elem c -> c -> c
  member :: Elem c -> c -> Bool

instance Eq a => Collection [a] where
  type Elem = a
-- ...  
```

Pomysł: klasa może mieć skojarzony typ (tu: typ elementów kolekcji).

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

## Klasy konstruktorowe

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

### Przykład klasy konstruktorowej: singleton

Chcemy uogólnić tworzenie kolekcji jednoelementowej:

``` haskell
class Pointed f where
   just :: a -> f a

instance Pointed Maybe where
   just = Just

instance Pointed [] where  -- [] oznacza tu konstruktor typu list
   just x = [x]

instance Pointed Tree where
   just = Leaf
```

NB takiej klasy nie ma w Prelude


### Klasy konstruktorowe

-   Typy takie jak **Tree** czy listy są pojemnikami przechowującymi obiekty
-   Instancja **Eq(Tree a)** mówi o własnościach pojemnika z zawartością
-   Instancja **Pointed Tree** mówi o własnościach samego pojemnika, *niezależnie od zawartości*

**Uwaga:** słowo *pojemnik* jest tu użyte tylko dla pokazania intuicji,<br/>
`T a` nie musi przechowywać `a`,  może to być np. obliczenie które produkuje
pewną ilość elementów typu `a`.

# Podsumowanie

- Klasa typów grupuje typy spełniające pewien wspólny interfejs
- Instancja klasy opisuje w jaki sposób dany typ spełnia taki interferjs
- Pozwalają na tworzenie funkcji generycznych (ograniczony polimorfizm)
- Haskell ma bogaty ekosystem klas
- Najważniejsze klasy: **Eq, Ord, Show, Num**
- Mechanizm automatycznego generowania instancji `deriving`
- Zaawansowane mechanizmy: klasy wieloparametrowe i konstruktorowe

To ostatnie zagadnienie rozwiniemy później.
