---
title: Programowanie Funkcyjne
subtitle: Wnioskowanie o programach
author:  Marcin Benke
date: Wykład 10, 12 maja 2025
---

# Wnioskowanie o programach

Dzięki zasadzie przejrzystości i typom, rozumowanie o programach w
Haskellu często jest łatwiejsze niż w innych językach.

Własności przeważnie mają postać równości, np

``` haskell
reverse (xs++ys) = reverse ys ++ reverse xs

fmap id x = x
fmap (f . g) x = (fmap f . fmap g) x
```

## Zasady wnioskowania równościowego

Symetria: `(x = y) → (y = x)`

Przechodniość: `(x = y) ∧ (y = z) → (x = z)`

Zasada Leibniza: `P(x) ∧ (x = y) → P(y)`

Kongruencja (szczególny przypadek): `(x = y) → (f x = f y)`

Z definicji: jeśli w programie mamy definicję `f x = e` to dla dowolnego
`y` (odpowiedniego typu) `f y = e[x := y]`

ale uwaga na definicje, w której przypadki nie są ściśle rozłączne, np

    isZero 0 = True
    isZero n = False

tu nie możemy wnioskować, że `isZero y = False` dla dowolnego `y`.
Dlatego tutaj posługujemy się tylko definicjami o ściśle rozłącznych
przypadkach.

## Rozgrzewka: liczby naturalne

``` haskell
-- import Numeric.Natural

data Nat = Zero | S Nat deriving(Eq,Show)

instance Num Nat where ...
instance Integral Nat where ...
```

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

### Odejmowanie
Spróbujmy teraz zdefiniować odejmowanie:

``` haskell
  m - Zero  = m
  S m - S n = m - n
```

Zauważmy, że znów użwamy rekursji po obu argumentach,
ale nie wszytkie kombinacje wzorców są użyte.

Na liczbach naturalnych, odejmowanie jest funkcją częściową:

``` haskell
  S Zero - S(S Zero)
= {- (-).2 -}
  Zero - S Zero
= {- wyczerpanie przypadków -}
  ⊥
```

Zobaczmy w ghci:
```
ghci> S Zero - S(S Zero)
*** Exception: Nat.hs:(57,3)-(58,19): Non-exhaustive patterns in function -
```

# Wartości częściowe

Na pierwszy rzut oka wydaje się, że do typu Nat należą wartości odpowiadające liczbom naturalnym:

``` haskell
Zero, S Zero, S(S Zero), ...
```

W rzeczywistości należy do niego także ⊥, ale na tym nie koniec

``` haskell
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


### Indukcja na liczbach naturalnych

Jedną z przyczyn, dla których zajmujemy się tu typem `Nat`, jest właśnie fakt,<br/>
że zasada indukcji jest najbardziej znana dla liczb naturalnych

     P(Zero)      ∀n.P(n) → P(S n)
    ————————————————————————————————
               ∀n.P(n)

Aby udowodnić, że własność `P` zachodzi dla wszystkich liczb
<br/>naturalnych, wystarczy udowodnić:

- `P(Zero)`
- jeśli `P(n)` to `P(S n)`

jak zobaczymy, ta sama zasada znajduje zastosowanie dla innych typów.


### Pełna indukcja

Klasyczna zasada indukcji obowiązuje dla właściwych liczb naturalnych.
Jeśli chcemy wnioskować o wartościach częściowych, trzeba wykazać,
że rozważana własność zachodzi też dla  `⊥`


     P(⊥)   P(Zero)   ∀n.P(n) → P(S n)
    ————————————————————————————————————
               ∀n.P(n)

(aby wykazać `P(∞)` trzeba użyć indukcji pozaskończonej; potrzebna przesłanka `∀n<∞ P(n)`  jest konkluzją klasycznej indukcji).

Tym niemniej w zasadzie ograniczamy się do wartości skończonych.

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

``` haskell
  add Zero (S n)
= {- add.2 -}
  S(add Zero n)
= {- IH -}
  S n
```
$\Box$

### Lemat 2

``` haskell
add :: Nat -> Nat -> Nat
add m Zero  = m           -- add.1
add m (S n) = S(add m n)  -- add.2
```


$S\ m + n = S(m + n)$

Dowód przez indukcję po $n$:

- krok podstawowy
``` haskell
   S m + Zero
=  {- add.1 -}
   S m
=  {- add.1 wspak -}
   S (m + Zero)
```
- krok indukcyjny: załóżmy $P(n) \equiv  S\ m + n = S(m + n)$ (IH),
pokażemy P(S n): `S m + S n = S(m + S n)`

``` haskell
  S m + S n
= {- add.2 -}
  S(S m + n)
= {- IH, kongruencja -}
  S(S(m + n))
= {- add.2 wspak -}
  S(m + S n)
```
$\Box$

###  Przemienność dodawania
**Twierdzenie**

$$ \forall m\, n.m + n = n + m $$
Dowód przez indukcję po $n$:

- krok podstawowy:
``` haskell
  m + Zero
= {- add.1 -}
  m
= {- Lemat 1 -}
  Zero + m
```
- krok indukcyjny
``` haskell
  m + S n
= {- add.2 -}
  S(m + n)
= {- IH -}
  S(n + m)
= {- Lemat 2 -}
  S n + m
```
$\Box$

### Pobudka

To było tak rutynowe i nudne, że pewnie już wszyscy zasypiają.

Zatem może postawmy mniej oczywiste pytania:

- czy zawsze $\bot + n = n + \bot$ ?
- czy zawsze $\bot \geq n = n \leq \bot$ ?
- czy zawsze `infinity + n = n + infinity` ?

Na razie jednak skupiamy się na wartościach właściwych.

## Synteza programów

Zobaczmy teraz, że poznane metody można wykorzystać także do syntezy programów na podstawie specyfikacji.

Odejmowanie możemy wyspecyfikować jako odwrotność dodawania:

```
(m + y) - y = m
```

Przy pomocy indukcji skonstruujemy funkcję `(−)` dla tej specyfikacji

### Synteza odejmowania
Specyfikacja: `(m + y) - y = m`

- Przypadek podstawowy:

``` haskell
  (m + Zero) - Zero = m {- specyfikacja dla y = Zero -}
  {- add.1 -}
  m - Zero = m
```

to ostatnie równanie możemy przyjąć jako część definicji funkcji

- Przypadek indukcyjny:
``` haskell
(m + S n) - S n = m  {- specyfikacja dla y = S n -}
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

Zauważmy też, że `fold S Zero = id`

### Zalety fold

- zwięzłość zapisu
- możliwości optymalizacji

Zamiast żmudnie dowodzić przez indukcję własności funkcji rekurencyjnych,<br />
wystarczy raz udowodnić własności `fold` a potem z nich skorzystać.

Dla liczb naturalnych nie wygląda to może imponująco, ale analogiczne funkcje można zdefiniowac dla innych typów rekurencyjnych <br />
(np. list) i tam są one bardzo użyteczne.


## Fuzja

Dla przykładu udowodnimy teraz następujące prawo fuzji:


jeśli `f a = b` oraz `f . g = h . f`, to
``` haskell
f . foldn g a = foldn h b
```

Na przykład aby pokazać nasz Lemat 1: `Zero + n = n`  czyli (pamiętając, że `foldn S Zero n = n`)

``` haskell
(Zero +) . foldn S Zero = foldn S Zero
```
przy pomocy prawa fuzji, wystarczy pokazać (`f = (Zero+); g = h = S`; a = b = Zero)

``` haskell
f a = (Zero+) Zero = Zero + Zero = Zero = b

(f . g) n =     {- f, g, (.) -}
(Zero+)(S n) =  {- (x+) -}
Zero + S n =    {- (+)  -}
S(Zero + n)     {- (x+) -}
= S((Zero+) n)) {- h, f, (.) -}
= (h . f) n
```

Analogicznie możemy pokazać, że `(S Zero)` jest elementem neutralnym mnożenia.

Uwaga: dla własności częsciowych prawo fuzji wymaga dodatkowo aby funkcja `f` była pedantyczna.

### Dowód prawa fuzji
``` haskell
foldn h c Zero  = c
foldn h c (S n) = h(foldn h c n)
```
jeśli `f a = b` oraz `f . g = h . f`, to
``` haskell
f . foldn g a = foldn h b
```
Przypadek (⊥):

``` haskell
LHS = f(foldn g a ⊥) {- case -} f ⊥
RHS = foldn h b ⊥ = ⊥
```
teza zachodzi gdy `f` jest pedantyczna.

Przypadek (`Zero`):

``` haskell
LHS = f(foldn g a Zero) {- foldn.1 -}  f a
RHS = foldn h b Zero = b
```
teza zachodzi gdy `f a = b`

### Dowód prawa fuzji cd
``` haskell
foldn h c Zero  = c
foldn h c (S n) = h(foldn h c n)
```
jeśli `f a = b` oraz `f . g = h . f`, to

``` haskell
f . foldn g a = foldn h b
```

Przypadek indukcyjny:
``` haskell
IH:  f(foldn g a n) = foldn h b n

LHS:
  f(foldn g a (S n))
= {- foldn.2 -}
  f(g(foldn g a n))
= {- (.) -}
  (f . g) (foldn g a n)

RHS:
  foldn h b (S n)
= {- foldn.2 -}
  h(foldn h b n)
= {- IH wspak -}
  h(f(foldn g a n))
= {- (.) -}
  (h . f) (foldn g a n)
```
Teza zachodzi gdy `f . g = h . f`

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
3. Krok indukcyjny, załóżmy  IH: `xs ++ [] = xs`,<br/>
wykażemy `(x:xs) ++ [] = x:xs`

    ``` haskell
    (x:xs) ++ []
    = {- ++.2 -}
    x:(xs++[])
    = {- IH -}
    x : xs
    ```

QED.

### Łączność

```
(xs ++ ys) ++ zs = xs ++ (ys ++ zs)
```

Szkic kroku indukcyjnego:

``` haskell
  ((x:xs) ++ ys) ++ zs
=  {- ++.2 -}
   (x:(xs ++ ys)) ++ zs
=  {- ++.2 -}
    x:((xs ++ ys) ++ zs)
=  {- IH -}
    x:(xs ++ (ys ++ zs))
=  {- ++.2 wspak  -}
    (x:xs) ++ (ys ++ zs)
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

Na razie udowodnimy

``` haskell
reverse(xs ++ ys) = reverse ys ++ reverse xs
reverse(reverse xs) = xs
```

(na ćwiczeniach)

### map

Stosuje funkcję do każdego elementu listy

``` haskell
map :: (a->b) ->  [a] -> b
map :: (a->b) -> ([a] -> b)
map f [] = []
map f (x:xs) = f x : map f xs
```

Własności:

``` haskell
map id = id
map (f . g) = map f . map g
map f (xs ++ ys) = map f xs ++ map f ys
```

``` haskell
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


Szkic, do uzupełnienia na ćwiczeniach:
``` haskell
  filter p (xs ++ ys)
= filter p (concat [xs, ys])
= concat(map filter p [xs, ys])
= filter p xs ++ filter p ys
```

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

### Zwijanie w lewo - foldl

Funkcja `foldl` jest podobna do `foldr` z tym, że grupuje elementy listy w lewą stronę:

``` haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f e []     = e
foldl f e (x:xs) = foldl f (f e x) xs
```

zamienia listę `x1:(x2:(x3:...xn:[]))`
na wartość `(((e ⊕ x1)⊕x2)⊕x3)⊕...xn))`.


``` haskell
reverse = foldl (flip(:)) []
flip f x y = f y x
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

## foldr/build
Efektem wykonania `foldr k z xs` jest zastąpienie każdego `(:)` przez `k` i koncowego `[]`  przez `z`:

``` haskell
foldr k z [x1...xn] = x1 `k` ... xn `k` z
```

Dualnie, możemy uogólnić funkcje tworzące listy:

``` haskell
build g = g (:) []
```

Wtedy zachodzi następująca równość (foldr/build)

``` haskell
foldr k z (build g) = g k z
```

Na przykład zdefiniujmy funkcję (podobną do [a..b]):

``` haskell
from a b = if a>b then [] else a : (from (a+1) b)
```

Abstrahując (:) i [] otrzymamy
``` haskell
from' a b = λc n -> if a>b then n else c a (from' (a+1) b)
from a b = build (from' a b)
```

Teraz konsumpcja listy zbudowanej przez `from` może ulec deforestacji
``` haskell
product (from a b)              = {- product, from -}
foldr (*) 1 (build (from' a b)) = {- foldr/build -}
from' a b (*) 1
```

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

## Prawa dla funkcji `fold*` - twierdzenia o dualności


1. Jeśli `⊕` jest operacją łączną z elementem neutralnym `e` to

``` haskell
foldr (⊕) e xs = foldl (⊕) e xs
```
Na przykład `foldr (+) 0 xs = foldl (+) 0 xs`

2. Jeśli `⊕::a->b->b`, `⊗::b->a->b` i `e::b` spełniają warunki
```
x ⊕ (y ⊗ z) = (x ⊕ y) ⊗ z
x ⊕ e = e ⊗ x
```
to `foldr (⊕) e xs = foldl (⊗) e xs`

Na przykład
``` haskell
reverse = foldr snoc [] where snoc x xs = xs ++ [x]
reverse = foldl cons [] where cons xs x = [x] ++ xs
```
3. Dla wszystkich skończonych list `xs`
``` haskell
foldr f e xs = foldl (flip f) e (reverse xs)
foldl g e xs = foldr (flip g) e (reverse xs) -- konsekwencja
```
Na przykład
``` haskell
id = foldr (:) [] xs = foldl (flip(:)) [] (reverse xs) = reverse(reverse xs)
```

# Klasy, metody i własności

Klasy pozwalają nam na tworzenie funkcji generycznych, np.

``` haskell
elem :: Eq a => [a] -> [a]
```

czy

``` haskell
foo :: Functor f => (a->b) -> f a -> f b
foo f x = fmap f . fmap id
```

dla podobnej funkcji na listach

```
bar :: (a->b) -> [a] -> [b]
bar f = map f . map id
```

dzięki wykazanym wcześniej własnościom `map`, wiemy, że `bar = map`.
Ale co jeśli chcemy wykazać podobną własność `foo`?

Dlatego zwykle definiując klasę postulujemy spełnienie pewnych własności, np dla klasy `Functor`

``` haskell
fmap id = id                           -- fmapId
fmap (f . g) = (fmap f . fmap g)       -- fmapComp
```

dzięki nim możemy wykazać `foo = fmap`.


### Ćwiczenie

wykaż spełnienie własności

``` haskell
fmap id = id                           -- fmapId
fmap (f . g) = (fmap f . fmap g)       -- fmapComp
```

przez instancje

``` haskell
instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)
```

``` haskell
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)
```

## Prawa dla Monad

Każda instancja `Monad` musi spełniać własności gwarantujace, że sekwencjonowanie jest (w pewnym sensie) łączne<br/>
zaś `return` jest jego elementem neutralnym:

``` haskell
return x >>= f    =  f x
mx >>= return     =  mx
(mx >>= f) >>= g  =  mx >>= (\y -> f y >>= g)
```

Asymetria tych praw wynika z asymetrii operatora `(>>=)`:

``` haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```
można temu zaradzić, wyrażając je w terminach operatora złożenia Kleisli `(>=>)`:

``` haskell
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g

return >=> f      =  f
f >=> return      =  f
(f >=> g) >=> h   =  f >=> (g >=> h)
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

## Prawa dla Applicative

Prawa dla Applicative są (przynajmniej na pierwszy rzut oka) trochę bardziej złozone:

``` haskell
pure id <*> x  =  x                              -- identity
pure (g x)     =  pure g <*> pure x              -- homomorphism
x <*> pure y   =  pure (\g -> g y) <*> x         -- interchange
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z   -- composition
```

Ponadto mamy `fmap f x = pure f <*> x`.

Jeśli mamy też instancję Monad, to dodatkowo powinno zachodzić

``` haskell
pure = return
m1 <*> m2 = m1 >>= (x1 -> m2 >>= (x2 -> return (x1 x2)))
```

## Prawa dla Applicative - ćwiczenie

``` haskell
pure id <*> x  =  x                              -- identity
pure (g x)     =  pure g <*> pure x              -- homomorphism
x <*> pure y   =  pure (\g -> g y) <*> x         -- interchange
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z   -- composition

fmap f x = pure f <*> x
m1 <*> m2 = m1 >>= (x1 -> m2 >>= (x2 -> return (x1 x2)))
```

**Ćwiczenie:** wykaż spełnienie praw Applicative dla

``` haskell
instance Applicative Maybe where
  pure                  = Just
  Nothing  <*> _        = Nothing
  _        <*> Nothing  = Nothing
  (Just f) <*> (Just x) = Just (f x)
```

# Pytania?

# Bonus

### Fuzja

1. **foldr:** niech `f` pedantyczna, `f a = b` oraz `f(g x y) = h x (f y)`; wtedy
``` haskell
f . foldr g a = foldr h b
```
2. **foldl:** niech `f` pedantyczna, `f a = b` oraz `f(g x y) = h (f x) y)`; wtedy
``` haskell
f . foldl g a = foldl h b
```
3. **foldr/map:** (konsekwencja fuzji dla `foldr` oraz równości `map g = foldr (:).g`)
``` haskell
foldr f a . map g = foldr (f . g) a
map f . map g = map (f .g)
```
4. **foldr/concat:**
``` haskell
foldr f a . concat = foldr (flip (foldr f)) a
```

5. niech `f` łączne z elementem neutralnym `e`; wtedy

``` haskell
foldr f e . concat = foldr f e . map (foldr f e)
```
na przykład

``` haskell
sum . concat = sum . map sum
concat . concat = concat . map concat
```

## Uniwersalność foldr

Dla funkcji na listach, równość `g = foldr f v` zachodzi wtedy i tylko wtedy gdy

``` haskell
g [] = v
g (x:xs) = f x (g xs)
```

Na przykład pierwsze prawo fuzji:

``` haskell
f . foldr g a = foldr h b
```

jest równoważne warunkom

```
(f . foldr g a) [] = b    -- f a = b
(f . foldr g a) (x:xs) = h x ((f . foldr g a) xs
-- wynika z f(g x y) = h x (f y) dla y = foldr g a xs
```

### suml

Zanim zajmiemy się foldl spójrzmy na konkretny przykład: sumowanie listy od lewej:


``` haskell
suml :: [Int] → Int
suml xs = suml' xs 0 where
  suml' :: [Int] -> (Int -> Int)
  suml'   [ ] n = n
  suml'  (x : xs) n = suml' xs (n + x)
```

Aby wyrazić `suml'` przy pomocy `foldr` potrzeba znaleźć f, v takie aby

``` haskell
suml' [] = v                   -- v :: Int -> Int
suml' (x:xs) = f x (suml' xs)
```

Z pierwszego równania widać `v = id`; z drugiego obliczymy f:

``` haskell
    suml' (x:xs) = f x (suml' xs)
<=> {- aplikacja do n -}
    suml' (x:xs) n = f x (suml' xs) n
<=> {- definicja suml' -}
    suml' xs (n+x) = f x (suml' xs) n
<=  {- uogólnienie (suml' xs) do g -}
    g (n+x) = f x g n
<=> {- abstrakcja -}
    f = \x g -> (\n -> g (n+x))
```

Zatem

``` haskell
suml' = foldr (\x g -> (\n -> g (n+x))) id
```


### foldl via foldr

Zdefiniowaliśmy

``` haskell
suml :: [Int] → Int
suml xs = foldr (\x g -> (\n -> g (n+x))) id xs 0
```

W podobny sposób mozemy wyliczyć jak wyrazić

``` haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f v []     = v
foldl f v (x:xs) = foldl f (f v x) xs
```

przy pomocy foldr:

``` haskell
foldl f v xs = foldr (\x g -> (\a -> g  (f a x))) id xs v
```

W drugą stronę, zdefiniowanie foldr  przy pomocy foldr nie jest możliwe,<br/>
chociażby dlatego, że foldl jest pedantyczna zwn ogon listy, a foldr nie.