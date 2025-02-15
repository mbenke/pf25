---
title: Programowanie Funkcyjne
subtitle: Studium przypadku - liczby naturalne
author:  Marcin Benke
date: Wykład 4, 17 marca 2025
---

## Liczby naturalne

Chociaż mamy typy liczbowe takie jak **Integer**, liczby naturalne i operacje na nich możemy zdefiniować samodzielnie.

Mimo, iż ma to niewielkie znaczenie praktyczne, posłuży nam do lepszego zrozumienia trzech waznych pojęć:

- rekurencyjne typy danych
- definiowanie funcji przez rekurencję
- rozumowanie równościowe i indukcyjne

### Typ Nat

Dla naszych celów wygodna będzie definicja liczb naturalnych wprowadzona przez Peano:

- zero jest liczbą naturalną
- następnik liczby naturalnej jest liczbą naturalną

``` haskell
data Nat = Zero | S Nat
```

Definicja `data Nat` wprowadza typ danych `Nat` i jego konstruktory:

``` haskell
Zero :: Nat
S :: Nat -> Nat
```

Elementami typu `Nat` są wartości

``` haskell
Zero
S Zero
S (S Zero)
...
```

(można by użyć lepszej nazwy niż `S`, ale zaoszczędzi nam to pisania).

### Dodawanie

Dodawanie możemy zdefiniować przez rekurencję zwn jeden z argumentów:

``` haskell
add :: Nat -> Nat -> Nat
add m Zero  = m           -- add.1
add m (S n) = S(add m n)  -- add.2
```

Na przykład dla `add Zero (S (S Zero))` mamy

``` haskell
   add Zero (S (S Zero))
 = {- add.2 -}
   S(add Zero (S Zero))
 = {- add.2 -}
   S(S(add Zero Zero)) 
 = {- add.1 -}
   S(S Zero)
```

Powyższe stanowi (bardzo prosty) przykład *rozumowania równościowego*, z którym będziemy się często spotykać.

Zauwazmy, że obliczenie `m + n` wymaga O(n) kroków.

### Show

Próba przetestowania funkcji `add` może się skończyć błędem

```
    • No instance for ‘Show Nat’ arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it
```

wynika on z faktu, że nie zdefiniowaliśmy sposobu wypisywania elementów typu Nat.

Na początek możemy temu zaradzić dodając `deriving Show` w definicji `Nat`:

``` haskell
data Nat = Zero | S Nat deriving Show
```

Później spróbujemy sami zdefiniować odpowiednią instancję `Show`.

### Mnożenie

Mnożenie mozemy zdefiniować bardzo podobnie do dodawania:

``` haskell
mul :: Nat -> Nat -> Nat
mul m Zero = Zero
mul m (S n) = add (mul m n) n
```

analogicznie możemy zdefniować potęgowanie (ćwiczenie).

Wszystkie te definicje wpisują się w ogólny schemat rekurencji (zwn jeden argument), który za chwilę zobaczymy.

### Równość

Równość wymaga rekurencji po obu argumentach. Dopasowanie wzorca pozwala nam prowadzić rekurencję po obu jednocześnie:

``` haskell
instance Eq Nat where
    Zero == Zero = True
    S m  == S n  = m == n
    _    == _    = False
```

Oczywiście moglibyśmy równość prowadząc rekurencję po jednym argumencie, np.

``` haskell
eqNat :: Nat -> (Nat -> Bool)
eqNat Zero = \n -> case n of
                     Zero -> True
                     _ -> False
eqNat (S m) = \n -> case n of
                     Zero -> False
                     (S n') -> eqNat m n'
```
jak widać nie jest to jednak tak zwięzłe i czytelne jak pierwsza definicja.

**Ćwiczenie:** zapisz `eqNat` nie używając lambdy ani `case`

### Porządek

Gdy mamy już równość, dla zdefiniowania `Ord` wystarczy `<=` lub `compare`.<br/>
(wystarczy, bo np. `a < b = a <= b && a /= b;  a <= b = compare a b /= GT)`)


Dla urozmaicenia zobaczmy oba te sposoby:


``` haskell
instance Ord Nat where
    Zero <= _    = True
    S _  <= Zero = False
    S m  <= S n  = m <= n

    compare Zero  Zero  = EQ
    compare Zero  (S _) = LT
    compare (S _) Zero  = GT
    compare (S m) (S n) = compare m n
```
### Num 

Zamiast `add (mul m n) n` wygodniej pisać `m*n + n`. W tym celu zdefiniujmy instancję klasy `Num`:

``` haskell
instance Num Nat where
  fromInteger 0 = Zero
  fromInteger n = S(fromInteger(n-1))
  (+) = add
  (*) = mul
```

(dostaniemy komunikat o brakujących metodach; na razie możemy go zignorować)

### Show must go on

Spróbujmy teraz zdefiniować wypisywanie `Nat` bez pomocy `deriving`:


``` haskell
showNat :: Nat -> String
showNat Zero = "Zero"
showNat (S Zero) = "S Zero"
showNat (S n) = "S (" ++ showNat n ++ ")"
```

Drugi przypadek pozwala uniknąć wypisywania zbędnych nawiasów.

Inną metodą jest użycie `showsPrec`:

``` haskell
instance Show Nat where
    showsPrec _ Zero = showString "Zero" -- ("Zero"++)
    showsPrec p (S n) = showParen (p > 10) (showString "S " . showsPrec 11 n)
```

Dlaczego 10? Operatory infiksowe mają priorytety 0..9; aplikacja ma priorytet 10.<br/>
Priorytet 11 oznacza, że argument będzie zawsze ujety w nawiasy, chyba, że jest atomem.

``` haskell
ghci> showsPrec 10 (S Zero) ""
"S Zero"
ghci> showsPrec 11 (S Zero) ""
"(S Zero)"
ghci> showsPrec 11 Zero ""
"Zero"
```

### Odejmowanie
Spróbujmy teraz zdefiniować odejmowanie:

``` haskell
  m - Zero  = m
  S m - S n = m - n           
```

Zauważmy, że znów użwamy rekursji po obu argumentach,
ale nie wszytkie kombinacje wzorców są użyte.

Na liczbach naturalnych, odejmowanie jest funkcją częściową:

```
  S Zero - S(S Zero)
= { (-).2 }
  Zero - S Zero
= { wyczerpanie przypadków }
  ⊥
```

Zobaczmy w ghci:
```
ghci> S Zero - S(S Zero)
*** Exception: Nat.hs:(57,3)-(58,19): Non-exhaustive patterns in function -
```

# Wartości częściowe

Na pierwszy rzut oka wydaje się, że do typu Nat należą wartości odpowiadające liczbom naturalnym:

```
Zero, S Zero, S(S Zero), ...
```

W rzeczywistości należy do niego także ⊥, ale na tym nie koniec

```
⊥, S ⊥, S(S ⊥), ...
```

Nazwiemy je wartościami częściowymi;<br/>
w innych językach byłyby nieodróżnialne, ale w Haskellu możemy je odróznić.


### Nieskończoność

Wreszcie `Nat` zawiera jeszcze jedną wartość, która możemy zdefiniować jako

``` haskell
infinity :: Nat
infinity = S infinity
```

Podsumowując, `Nat` (podobnie jak inne typy rekurencyjne) zawiera trzy rodzaje wartości:

- skończone (właściwe): `Zero, S Zero, S(S Zero), ...`
- częściowe: `⊥, S ⊥, S(S ⊥), ...`
- nieskończone (w tym wypadku jedną): `S(S(...))`

Teraz skupimy się na wartościach właściwych (skończonych).

Do pozostałych wrócimy jeszcze przy omawianiu leniwej ewaluacji.

### Indukcja

Jedną z przyczyn, dla których zajmujemy się tu typem `Nat`, jest właśnie fakt,<br/>
że zasada indukcji jest najbardziej znana dla liczb naturalnych

Aby udowodnić, że własność `P` zachodzi dla wszystkich liczb
<br/>naturalnych, wystarczy udowodnić:

- `P(Zero)`
- jeśli `P(n)` to `P(S n)`

jak zobaczymy, ta sama zasada znajduje zastosowanie dla innych typów.

### Lemat 1

``` haskell
add :: Nat -> Nat -> Nat
add m Zero  = m           -- add.1
add m (S n) = S(add m n)  -- add.2
```
Spróbujemy teraz udowodnić, ze nasze dodawanie jest przemienne. Zaczniemy od dwóch prostych lematów:

**Lemat 1** 

$P(n) \equiv  Zero + n = n$ (to nie jest trywialne, bo  w `add` rekurencja po drugim argumencie). 

- $P(Zero) \equiv Zero + Zero = Zero$ - z pierwszego równania (`add.1`)

- Załóżmy, P(n), czyli `Zero + n = n` (IH); pokażemy `Zero + S n = S n`:

```
  add Zero (S n)
= { add.2 }
  S(add Zero n)
= { IH }
  S n
```
$\Box$

### Lemat 2

$S\ m + n = S(m + n)$

Dowód przez indukcję po $n$:

- krok podstawowy 
```
   S m + Zero 
=  { add.1 } 
   S m
=  { add.1 wspak }
   S (m + Zero)
```
- krok indukcyjny: załóżmy $P(n) \equiv  S\ m + n = S(m + n)$ (IH),
pokażemy P(S n): `S m + S n = S(m + S n)`

```
  S m + S n
= { add.2 }
  S(S m + n)
= { IH }
  S(S(m + n))
= { add.2 wspak }
  S(m + S n)
```
$\Box$

###  Przemienność dodawania
**Twierdzenie**

$$ \forall m\, n.m + n = n + m $$
Dowód przez indukcję po $n$:

- krok podstawowy:
```
  m + Zero ={ add }= m ={ Lemat 1 }= Zero + m
```
- krok indukcyjny
```
  m + S n
= { add }
  S(m + n)
= { IH }
  S(n + m)
= { Lemat 2}
  S n + m
```
$\Box$

### Pobudka

To było tak rutynowe i nudne, że pewnie już wszyscy zasypiają.

Zatem może postawmy mniej oczywiste pytania:

- czy zawsze $\bot + n = n + \bot$ ?
- czy zawsze $\bot \geq n = n \leq \bot$ ?
- czy zawsze `infinity + n = n + infinity` ?

Wrócimy do nich jeszcze przy omawianiu leniwej ewaluacji.

Na razie jednak skupiamy się na wartościach właściwych.

## Synteza programów

Zobaczmy teraz, że poznane metody można wykorzystać także do syntezy programów na podstawie specyfikacji.

Odejmowanie możemy wyspecyfikować jako odwrotność dodawania:

```
(m + n) - n = m
```

Przy pomocy indukcji skonstruujemy funkcję `(−)` dla tej specyfikacji:

### Synteza odejmowania
Specyfikacja: `(m + n) - n = m`

- Przypadek podstawowy: `Zero`

``` haskell
  (m + Zero) - Zero = m
  {- add.1 -}
  m - Zero = m
```

to ostatnie równanie możemy przyjąć jako część definicji funkcji

- Przypadek inukcyjny: `S n`
``` haskell
(m + S n) - S n = m
{- add.2 -}
S(m + n) - S n = n
{- IH: (m + n) - n = m -}
S (m + n) - S n = (m + n) - n
```
Oznaczając w ostatnim równaniu `m+n` przez `x` otrzymujemy

``` haskell
x - Zero = x
S x - S n = x - n
```

czyli definicję odejmowania, którą widzieliśmy wcześniej.

## Iterator (funkcja fold)

Większość definicji, które dotąd widzieliśmy ma wspólny schemat:

``` haskell
f :: Nat -> A
f Zero  = c
f (S n) = h(f n)
```

Możemy ten schemat zamknąć w jednej funkcji `foldn`:

``` haskell
foldn :: (a -> a) -> a -> Nat -> a
foldn h c Zero  = c
foldn h c (S n) = h(foldn h c n)
```

Teraz mamy

``` haskell
m + n = foldn S m n             -- krócej: (+) = foldn S
m * n = foldn (+m) Zero n       
m ^ n = foldn (*m) (S Zero) n
```

Zauwazmy też, że `fold S Zero = id`

### Ciekawsze użycia fold

``` haskell
foldn :: (a -> a) -> a -> Nat -> a
```

W poprzednich przykładach mieliśmy `a = Nat`, ale ciekawsze przykłady używają innych typów, np.

``` haskell
fact = snd . foldn f (Zero, S Zero) where
  f :: (Nat, Nat) -> (Nat, Nat)
  f (m, n) = (S m, S m * n)

fib = snd . foldn g (Zero, S Zero) where
  g (m, n) = (n, m + n)
```

Możemy udowodnić przez indukcję, że `fact` i `fib` obliczają silnię i Fibonacciego, dokładniej:

```
foldn f (Zero, S Zero) = (n, n!)
foldn g (Zero, S Zero) = (F(n), F(n+1))
```

### Zalety fold

- zwięzłość zapisu
- możliwości optymalizacji

Zamiast żmudnie dowodzić przez indukcję w,łasności funkcji rekurencyjnych,<br />
wystarczy raz udowodnić własności `fold` a potem z nich skorzystać.

Dla liczb naturalnych nie wygląda to może imponująco, ale analogiczne funkcje można zdefiniowac dla innych typów rekurencyjnych <br />
(np. list) i tam są one bardzo użyteczne.

# Listy

Listy są bardzo popularną strukturą danych w programowaniu funkcyjnym,
od jego zarania (Lisp)

Najprostszym sposobem stworzenia listy jest wyliczenie jej elementów

``` haskell
[1,2,3]             :: [Int]
[ [1,2], [3], [] ]  :: [[Int]]
[ (+), (-),  (*) ]  :: [Int->Int->Int]
```

## Listy jako typ danych

Kanonicznym sposobem tworzenia listy jest użycie jednego z konstruktorów:

- `[]` - lista pusta
- `(x:xs)` - lista o głowie `x` i ogonie `xs` (nawiasy czasem można pominąć)

Dwukropek (czytany "cons") jest konstruktorem listy, zwykle używanym infiksowo.

Zapis oparty na wyliczeniu elementów jest lukrem syntaktycznym:

``` haskell
[1,2,3] = 1:(2:(3:[])) = 1:2:3:[]
```

Dekonstrukcja listy odbywa się zwykle przez dopasowanie wzorca, np.

``` haskell
len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs
```

(NB funkcja `length` w bibliotece standardowej ma inną implementację, podobnie z innymi funkcjami)

### Przykładowe operacje na listach

- `head`, `tail` - częściowe, błąd dla listy pustej (w nowszych wersjach GHC ostrzeżenie)
- `last`, `init` - podobnie
- `take n xs` - weź pierwsze `n` elementów listy `xs` (albo cała `xs` jeśli krótsza niż `n`)
- `drop` - pomiń pierwsze `n` elementów (lista pusta jeśli za mało)
- `replicate n x` - lista `n` kopii `x`
- `uncons :: [a] -> Maybe (a, [a])` - `head/tail` na sterydach
- `length :: [a] -> Int`
- `null :: [a] -> Bool` - czy lista pusta?

NB takie funkcje są w marę możności pobłażliwe, np

``` haskell
ghci> length [error "crash"]
1
```

### Konkatenacja

Infiksowy operator `(++) :: [a] -> [a] -> [a]`

```
ghci> [1,2,3] ++ [4,5]
[1,2,3,4,5]
ghci> take 3 ([1,2,3] ++ [4,5])
[1,2,3]
ghci> undefined ++ []
*** Exception: Prelude.undefined
ghci> [1,2,3]++undefined
[1,2,3*** Exception: Prelude.undefined
ghci> take 3 ([1,2,3] ++ undefined)
[1,2,3]
```

Możemy używać też `(<>)` z klasy Semigroup, które jest bardziej ogólne.

Na przykład gdy będziemy chcieli przejść z list na zbiory:

``` haskell
ghci> [1,2,1]<>[2,3]
[1,2,1,2,3]

ghci> s = Set.fromList [1,2,1]
ghci> t = Set.fromList [2,3]
ghci> s <> t 
fromList [1,2,3]
```

### concat

O ile `(++)` łączy dwie listy, to concat łączy listę list:

``` haskell
concat :: [[a]] -> [a]
concat [ [1,2], [3], [] ] = [1,2,3]
```
mozemy też użyć `mconcat` z klasy `Monoid`:

``` haskell
ghci> :t mconcat
mconcat :: Monoid a => [a] -> a

ghci> mconcat [ [1,2], [3], [] ] 
[1,2,3]
```

### intersperse, intercalate

``` haskell
ghci> amk = ["ala", "ma", "kota"]

ghci> concat amk
"alamakota"

ghci> intercalate " - " amk
"ala - ma - kota"

ghci> intersperse " - " amk
["ala"," - ","ma"," - ","kota"]

ghci> concat it
"ala - ma - kota"
```

NB `intersperse` i `intercalate` działają na dowolnych listach, nie tylko na napisach:

``` haskell
intercalate :: [a] -> [[a]] -> [a]
intersperse :: a -> [a] -> [a]
```


na marginesie: `concat = intercalate []`:

```
ghci> intercalate [] [ [1,2], [3], [] ]
[1,2,3]
```

## Indukcja dla list

Aby udowodnić, że własność `P` zachodzi dla wszystkich list, wystarczy pokazać:

1. `P(⊥)`
2. `P([])`
3. Dla dowolnych `x` oraz `xs`, jeżeli `P(xs)` to `P(x:xs)`

## Łączność konkatenacji

Rozważmy następującą definicję konkatenacji

``` haskell
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x:(xs++ys)
```

Pokażemy teraz, że konkatenacja jest łączna oraz `[]` jest jej elementem neutralnym

### Neutralność
``` haskell
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x:(xs++ys)
```

`[] ++ ys = ys` z definicji

Pokażemy `xs ++ [] = xs` przez indukcję po `xs`:

1. gdy `xs = ⊥` istotnie `⊥ ++ [] = ⊥` 
2. `xs = []` - trywialnie z definicji `[] ++ [] = []`
3. Krok indukcyjny, załóżmy  IH: `xs ++ [] = xs`, 
wykażemy `(x:xs) ++ [] = x:xs`

    ```
    (x:xs) ++ []
    = { ++.2 }
    x:(xs++[])
    = { IH }
    x : xs
    ```

QED.

### Łączność

```
(xs ++ ys) ++ zs = xs ++ (ys ++ zs)
```

Szkic kroku indukcyjnego: 

```
  ((x:xs) ++ ys) ++ zs
=  { ++.2 }
   (x:(xs ++ ys)) ++ zs
=  { ++.2}
    x:((xs ++ ys) ++ zs)
=  { IH }
    x:(xs ++ (ys ++ zs))
=  { ++.2, symetria }
    (x:xs) ++ (ys++ zs)
```

Szczegóły do uzupełnienia na ćwiczeniach.

## reverse

Kolejną ważną funkcją jest `reverse`, która odwraca kolejność elementów.

Jej naiwna implementacja

``` haskell 
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
```

ma złożoność $O(n^2)$. Później wyprowadzimy lepszą wersję.

Na razie udowodnijmy

```
reverse(xs ++ ys) = reverse ys ++ reverse xs
reverse(reverse xs) = xs
```

(na ćwiczeniach?)

## head, tail, init, last

``` haskell
head (x:xs) = x
tail (x:xs) = xs

init [x] = []
init (x:xs) = x:init xs

last [x] = x
last (x:xs) = last xs
```

Wszystkie te funkcje są częściowe, można ich używać tylko gdy mamy pewność, że argument jest niepusty.

A zamiast head/tail i tak lepiej użyć dopasowania - GHCi daje ostrzeżenie

```
ghci> head [1]

<interactive>:1:1: warning: [GHC-63394] [-Wx-partial]
    In the use of ‘head’
    This is a partial function, it throws an error on empty lists. Use pattern matching or Data.List.uncons instead.
```

### uncons/unsnoc

Istnieją bezpieczne alternatywy dla head/tail/init/last, np.

``` haskell
-- import Data.List
uncons :: [a] -> Maybe (a, [a])  -- Just (głowa, ogon) albo Nothing
unsnoc :: [a] -> Maybe (a, [a])  -- analogicznie dla last/init

ghci> import Data.List
ghci> uncons[1,2,3]
Just (1,[2,3])
ghci> unsnoc[1,2,3]
Just ([1,2],3)
```

## take, drop

Widzieliśmy je już w akcji, teraz przykładowe implementacje

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

Udowodnić `take n xs ++ drop n xs = xs`

Przypomnienie: to bardziej specyfikacje, prawdziwe implementacje są zoptymalizowane.

Te funkcje działają dla wszystkich list (takze pustej i nieskończonych).

```
ghci> take 10 (init [1..])
[1,2,3,4,5,6,7,8,9,10]

ghci> drop 999999 (take (10^6) [1..])
[1000000]
```

## map, filter

Funkcje map i filter ilustrują dwie ważne idee programowania funkcyjnego:

- programowanie całościowe
- użycie funkcji

```
ghci> sum(map (^2) [1..100])
338350

ghci> filter even [1..20]
[2,4,6,8,10,12,14,16,18,20]

ghci> filter isPrime [3,5..100]       -- isPrime nie jest standardowa
[3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
```

### map

Stosuje funkcję do każdego elementu listy

``` haskell
map :: (a->b) ->  [a] -> b
map :: (a->b) -> ([a] -> b)
map f [] = []
map f (x:xs) = f x : map f xs
```

Własności:

```
map id = id
map (f . g) = map f . map g
map f (xs ++ ys) = map f xs ++ map f ys
```

```
map f . tail = tail . map f
map f . reverse = reverse . map f
map f . concat = concat . map (map f)
```

Ponadto dla rygorystycznych f

```
f . head = head . map f
```

(dla pobłażliwych f może się zdarzyć, że lewa strona da wynik, a prawa nie)

### filter

Wybiera elementy spełniające pewien warunek (dla których funkcja daje `True`)

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

``` haskell
  filter p (xs ++ ys)
= filter p (concat [xs, ys])
= concat(map filter p [xs, ys])
= concat[filter p xs, filter p ys]
= filter p xs ++ filter p ys
```

### takeWhile, dropWhile

Ciekawym wariantem `filter` jest funkcja

``` haskell
takeWhile :: (a -> Bool) -> [a] -> [a]
```

Daje ona najdłuższy prefiks listy spełniający podany warunek.

przykład - "świadkowie złożoności" 

``` haskell
compWitness n = [d | d <- cands, n `mod` d == 0] where
    cands = 2:takeWhile (\d -> d*d <= n) [3,5..]
```
Liczba `n>2` jest pierwsza wtw gdy lista takich świadków jest pusta.

Co robi `dropWhile` - łatwo się domyśleć


**Ćwiczenie:** zastanów się, czy jest prawdą, że
``` haskell
takeWhile p xs ++ dropWhile p xs = xs
```

## Wycinanki listowe (list comprehensions)

Wycinanki stanowią wygodny sposób zapisu kombinacji `concat/map/filter`

Następujące reguły objaśniają sposób ich tłumaczenia
(`q` oznacza pojedynczy generator/filtr, a `Q` ich ciąg):

``` haskell
[e | x <- xs, Q] = concatMap f xs where f x = [e | Q]
[e | p, Q] = if p then [e | Q] else []

[e | x <- xs] = concatMap f xs where f x = [e] = map f xs where f x = e
[e | p] = if p then [e] else []

concatMap f = concat . map f
```

Na przykład

```
  [x * x | x <- [1..5], odd x]
= concatMap f [1..5] where f x = [x*x | odd x]
= concatMap f [1,2,3,4,5] where f x = if odd x then [x*x] else []
= concat [[1],[],[9],[],[25]]
= [1,9,25]
```
### Przykłady wycinanek

``` haskell
[(i, j) | i <-[1..4], even i, j <- [i+1..4], odd j]
[(2,3))]

[(x,y,z) | x<-[1..n], y <-[x..n], z <- [y..n]]
```

### Własności wycinanek

``` haskell
[f x | x <- xs]      = map f xs
[x | x <- xs, p x]   = filter p xs
[f x | x <- xs, p x] = map f(filter p xs)

[e | Q, P]           = concat [[e|P]|Q]
[e | Q, x <- [d|P]]  = [e[x:=d]|Q,P]
```

Na przykład

```
  [x * x | x <- [1..5], odd x] = [x * x | x <- filter odd [1..5]] = [x*x|x<-1,3,5]
```

## zip

Funkcja `zip` łączy elementy dwóch list w pary

``` haskell
ghci> zip [1..] "halo"
[(1,'h'),(2,'a'),(3,'l'),(4,'o')]
ghci> unzip it
([1,2,3,4],"halo")
```

``` haskell
zip :: [a] -> [b] -> [(a,b)]
zip []     _bs    = []
zip _as    []     = []
zip (a:as) (b:bs) = (a,b) : zip as bs
```
Prostym przykładem użycia `zip` jest iloczyn skalarny:

``` haskell
sp :: Num a => [a] -> [a] -> a
sp xs ys = sum(map times(zip xs ys)) 
         where times (x,y) = x * y
            -- times = uncurry (*)
```

### Sekwencje niemalejące

Ciekawszym przykładem użycia zip jest funkcja, która sprawdza czy dana lista jest niemalejąca:

``` haskell
nondec xs = and(map leq pairs) where
  leq (x,y) = x <= y
  pairs = zip xs (tail xs)
```

Konstrukcja `zip xs (tail xs)` tworzy listę par kolejnych elementów:

``` haskell
ghci> let xs = [1,2,3,4] in zip xs (tail xs)
[(1,2),(2,3),(3,4)]
```

Z kolei `map leq` dla każdej pary stwierdza, czy jest niemalejąca; `and` daje koniunkcję listy.

``` haskell
ghci> map leq [(1,2),(2,3),(3,4)]
[True,True,True]
ghci> and it
True

ghci> map leq [(1,2),(2,3),(3,0)]
[True,True,False]
ghci> and it
False
```

### zipWith

W naszych przykładach powtarza się schemat `map f (zip xs ys) where f (x,y) = ...`

Sugeruje to uogólnienie funkcji `zip` do

``` haskell
zipWith :: (a->b->c) -> [a] -> [b] ->[c]
```

Teraz nasze funkcje stają się "jednolinijkowcami":

``` haskell
sp xs ys = sum(zipWith (*)) xs ys
nondec xs = and(zipWith (<=) xs (tail xs))
```

Inny ciekawy jednolinijkowiec, który wyjaśnimy na kolejnym wykładzie:

``` haskell
ghci> fibs = 0:1:zipWith (+) fibs (tail fibs)

ghci> take 20 fibs
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
```

### zip - ćwiczenia

Jaki wynik da `nondec` dla listy pustej? Jak to poprawić?

Przy pomocy `zip` napisz funkcję `positions` taką, że `positions x xs` daje listę pozycji wystąpień `x` w liście `xs`

``` haskell
ghci> positions 'a' "alamakota"
[0,2,4,8]
```

Teraz napisz funkcję `position` która daje pierwszą pozycję lub (-1) gdy nie ma wystąpień.

## Funkcje fold

Dla `Nat` wiele funkcji rekurencyjnych dało się wyrazić przy pomocy funkcji `foldn`.<br/>
Uosabia ona schemat *pierwotnej rekursji* dla `Nat`

Podobnie dla list wiele funkcji powtarza schemat

``` haskell
h [] = e
h (x:xs) = x ⊕ h xs
```
zamieniający listę `x1:(x2:(x3:...xn:[]))` na wartość `x1⊕(x2⊕(x3⊕...xn ⊕ e))`.

Możemy ten schemat wyrazić przy pomocy funkcji `foldr`:

``` haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f e []     = e
foldr f e (x:xs) = f x (foldr f e xs)

-- alternatywnie:
foldr f e xs = go xs where
  go []     = e
  go (x:xs) = x `f` go xs
```

### Przykłady uzycia foldr

``` haskell
concat  = foldr (++) []
sum     = foldr (+)  0
product = foldr (*)  1
and     = foldr (&&) True
or      = foldr (||) False

xs ++ ys = foldr (:) ys xs
map f = foldr (cons . f) [] where cons x xs = x : xs
map f = foldr ((:) . f) []
reverse = foldr snoc [] where snoc x xs = xs ++ [x]
```

Zauważmy, że niektóre operacje maja typ postaci `T->T->T` i są łączne,
inne zaś nie są łączne i często mają typ innej postaci.

W pierwszym przypadku

`x1⊕(x2⊕(x3⊕...xn ⊕ e)) = ((x1⊕x2)⊕x3)⊕...xn) ⊕ e`

i nie ma znaczenia czy zwijamy od lewej, czy od prawej.

W drugim przypadku jednak jest to istotne (po przestawieniu nawiasów często typy się nie zgodzą).

### Zwijanie w lewo - motywacja

Powiedzmy, że chcemy napisać funkcję `decimal`, która dla listy cyfr da wartość takiej listy w systemie dziesiętnym, np.

```
decimal [3, 5, 7] = 357
```

możemy ją zrealizować przy pomocy mnożenia przez 10 i sumowania

```
decimal [x1, x2, x3] = (((x1*10)+x2)*10)+x3
```

widać, że nawiasy tutaj ciążą w lewo a ich przegrupowanie zmieni wynik.
Zatem foldr nie do końca się tu nadaje:

``` haskell
ghci> let step a b = 10*b + a in foldr step 0 [3,5,7]
753
```

Niby możemy odwrócić listę, ale ...

### Zwijanie w lewo - foldl

bardziej naturalne byłoby wyrażenie `decimal`  w terminach innej funkcji

``` haskell
ghci> let step b a = 10*b + a in foldl step 0 [3,5,7]
357
```

``` haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f e []     = e
foldl f e (x:xs) = foldl f (f e x) xs
```

zamieniającej listę `x1:(x2:(x3:...xn:[]))` 
na wartość `(((e ⊕ x1)⊕x2)⊕x3)⊕...xn))`.


``` haskell
reverse = foldl (flip(:)) []
flip f x y = f y x
```

Wadą `foldl` jest to, że kolejne wywołania rekurencyjne akumulują
coraz większe wyrażenia.

Zaletą z kolei jest to, że `foldl` jest ogonowe;
sprytny kompilator może zamienić taką rekurencję na iterację.

Ale GHC jest jeszcze sprytniejszy i często potrafi zrobić to także dla `foldr`.

W praktyce w Haskellu nie używamy `foldl` - powiemy o tym więcej  na kolejnych wykładach.

### Zwijanie list niepustych

Jeden z argumentów `foldr/foldl` określa wartość dla listy pustej.

Jeśli zwijamy listy niepuste mozna użyć prostszych funkcji

``` haskell
foldr1, foldl1          :: (a -> a -> a) -> [a] -> a
foldr1 f [x]            =  x
foldr1 f (x:xs)         =  f x (go xs)
```

Np `maxlist = foldr1 max`

```
ghci> foldr1 max [2,1,4,3]
4
```

## Znaczenie praw

Prawa w postaci równości typu

``` haskell
map f . map g = map (f . g)
```

mają co najmniej dwa zastosowania

1. Wnioskowanie o programach (dowodzenie własności, wyprowadzanie implementacji na podstawie specyfikacji)

2. Automatyczne przekształcenia programów ("optymalizacja"):

biblioteki zawierają reguły przekształcenia postaci

``` haskell
{-# RULES
      "map/map"    forall f g xs.  map f (map g xs) = map (f.g) xs
  #-}
```

jeżeli kompilator wykryje wyrażenie postaci takiej jak lewa strona reguły, zastapi ją prawą.

Jest to możliwe dzieki zasadzie przejrzystości:

> zastąpienie części wyrażenia innym wyrażeniem o tej samej wartości daje równoważne wyrażenie.

Dzięki przejrzystości kompilator może wykonać także inną optymalizację - tzw. inlining czyli zastapienie lewej strony definicji prawą.

## Deforestacja

Pod tym terminem kryje się operacja polegająca na eliminacji pośrednich list (w ogólności: drzew).

Na przykład

``` haskell
fact :: Int -> Int
fact n = foldr (*) 0 [1..n]
```

wydawałoby się, że ta funkcja jest bardzo nieefektywna - buduje listę, a potem ją konsumuje.

Tymczasem jest inaczej:

- po pierwsze to nie działa w ten sposób - argumenty są obliczane na tyle, na ile są potrzebne, czyli produkcja listy jest sterowana konsumpcją (pamiętacie `zip [1..] "halo"`?)
- po drugie kompilator potrafi wykryć takie sytuacje i całkowicie wyeliminować listę.
- w efekcie zostanie wydajna funkcja działająca tylko na liczbach

### Deforestacja - przykład

(z programu obliczającego liczbę związków organicznych pewnego rodzaju)

``` haskell
three_partitions :: Int -> [(Int,Int,Int)]
three_partitions m
  = [ (i,j,k) | i <- [0..(m `div` 3)],
      j <- [i..(m-i `div` 2)],
      let k = m - (i+j)
    ]

main = print (length (three_partitions 4000))
```
Program tworzy ca 4 miliony krotek.

Bez deforestacji alokuje ca 800M pamięci. 

Dzięki zastosowaniu reguły `foldr/build`, tylko 50k (i działa 40-krotnie szybciej).

## Przykład wyprowadzania implementacji - lepsze reverse

Rozważmy funkcję odwracającą listę

``` haskell
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]
```

Zauważmy, że ma ona złożoność kwadratową, gdyż `(++)` ma złożoność liniową zwn długość pierwszego argumentu.

Skoro problemem jest tu konkatencja, możemy spróbować zaradzić temu pisząc funkcję ogólniejszą, która łączy odwracanie i konkatenację:

``` haskell
revA xs ys = rev xs ++ ys
```
oczywiście gdybyśmy potraktowali to jako definicję, nic by to nie pomogło,<br />
ale możemy potraktować powyższą równość jako specyfikację i systematycznie skonstruować lepszą definicję `revA`

### Od specyfikacji do implementacji
Specyfikacja

``` haskell
revA xs ys = rev xs ++ ys
```

Wyprowadzenie dla listy pustej:

``` haskell
revA [] ys   = 
rev [] ++ ys =
[] ++ ys = ys
```
Wyprowadzenie dla listy niepustej:
``` haskell
revA (x:xs) ys   =
rev (x:xs) ++ ys =
(rev xs ++ [x]) ++ ys = 
rev xs ++ ([x] ++ ys) =
rev xs ++ (x:ys) =
revA xs (x:ys)
```

### Implementacja

w ten sposób otrzymujemy następującą definicję `revA`:

``` haskell
revA [] ys     = ys
revA (x:xs) ys = revA xs (x:ys)
```

która ma złożoność liniową zwn długość pierwszego argumentu.
W związku z tym 

``` haskell
reverse xs = revA xs []
```

też ma złożoność liniową.

**Ćwiczenie:** spróbuj podobnie ulepszyć funkcję

``` haskell
flatten :: Tree a -> [a]
flatten (Leaf x)   = [x]
flatten (Node l r) = flatten l ++ flattten r
```

**Ćwiczenie:** dla powyższej definicji `reverse` wykaż

``` haskell
reverse (reverse xs) = xs
reverse (xs ++ ys)   = reverse ys ++ reverse xs
```

# Administrivia

Zadanie 2 - termin 13.4

Rozszerzamy wizualizację procesu redukcji wyrażeń kombinatorowych.
Tym razem zestaw kombinatorów nie jest ustalony, ale dany przez użytkownika,
w składni będącej podzbiorem Haskella, np.


``` haskell
s x y z = x z (y z)
k x y = x
main = s k k x
------------------------------------------------------------
s k k x
k x (k x)
x
```
