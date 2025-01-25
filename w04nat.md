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

# Bonus

### (zimna) fuzja FIXME

Dla przykładu udowodnimy teraz następujące prawo fuzji:


jeśli `f a = b` oraz `f . g = h . f`, to
```
f . foldn g a = foldn h b
```

Na przykład aby pokazać nasz Lemat 1: `Zero + n = n`  czyli (pamiętając, że `foldn S Zero n = n`)

```
(Zero +) . foldn S Zero = foldn S Zero
```
przy pomocy prawa fuzji, wystarczy (`f = (Zero+); g = h = S`)

```
Zero + Zero = Zero         -- f a = b
Zero + S n = S(Zero + n)   -- f . g = h . f
```
co wynika trywialnie z definicji dodawania.

Analogicznie możemy pokazać, że `(S Zero)` jest elementem neutralnym mnożenia.

### Dowód prawa fuzji
``` haskell
foldn h c Zero  = c
foldn h c (S n) = h(foldn h c n)
```
jeśli `f a = b` oraz `f . g = h . f`, to
```
f . foldn g a = foldn h b
```
Przypadek (⊥):

```
LHS = f(foldn g a ⊥) ={ brak przypadku }= f ⊥
RHS = foldn h b ⊥ = ⊥
```
teza zachodzi gdy `f` jest pedantyczna.

Przypadek (`Zero`):

```
LHS = f(foldn g a Zero) ={ foldn.1 }=  f a
RHS = foldn h b Zero = b
```
teza zachodzi gdy `f a = b`

### Dowód prawa fuzji cd
``` haskell
foldn h c Zero  = c
foldn h c (S n) = h(foldn h c n)
```
jeśli `f a = b` oraz `f . g = h . f`, to
```
f . foldn g a = foldn h b
```
Przypadek indukcyjny:
```
IH:  f(foldn g a n) = foldn h b n

LHS: f(foldn g a (S n)) ={ foldn.2 }= f(g(foldn g a n))
RHS: foldn h b (S n) ={ foldn.2 }= h(foldn h b n) ={ IH wspak }= h(f(foldn g a n))
```
Teza zachodzi gdy `f . g = h . f`

## Liczebniki Churcha (TODO)
[Bird s86-88]


### Wartości logiczne


Zacznijmy od wartości logicznych.

Istotą wartości logicznych jest dokonywanie wyborów.

``` haskell
true, false :: a -> a -> a
true x y = x
false x y = x

when b e1 e2 = b e1 e2

not x = x false true
```

### Liczebniki Churcha

``` haskell
zero, one,two, three :: Cnum a
zero =  \f x ->  x
one f x = f x
suc n = \f x -> f(n f x)

runNat n = n (\x -> x + 1) (0::Int)
two = suc one
plus, mul :: Cnum a -> Cnum a -> Cnum a
plus m n = \f x -> m f(n f x)
-- shorter, but less general type           
plus' :: Cnum(Cnum a) -> Cnum a -> Cnum a
plus' cn = cn suc
           
mul m n = \f x -> m (n f) x

-- iter :: (c -> c) -> c -> N -> c

iter f z n = n f z

fac :: N -> N 
fac n = snd (fac' n) where
  fac' = iter step (zero, one)
  step (k, fk) = (suc k, mul (suc k) fk)
```