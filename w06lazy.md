---
title: Programowanie Funkcyjne
subtitle: Leniwa ewaluacja
author:  Marcin Benke
date: Wykład 6, 31.3.2025
---

# Lenistwo

> Progress doesn't come form early risers<br/>
> - progress is made by lazy men looking for easier ways to do things.

Robert A. Heinlein

> Nadgorliwość jest gorsza od faszyzmu.

(przysłowie ludowe)

&nbsp;

Haskell jest językiem *leniwym* (lazy, non-strict). Co to oznacza?

Przed omówieniem leniwej ewaluacji warto poświęcić chwilę na omówienie jej przeciwieństwa,<br />
jakim jest ewaluacja *gorliwa* (eager, strict).

## Gorliwa ewaluacja
> **Pan Jourdain** — *Jak to? Więc kiedy mówię: Michasiu, podaj mi pantofle i przynieś krymkę — to proza?*

> **Nauczyciel filozofii** — *Tak, panie.*

Większość języków programowania (np. C, Java, ML, etc) używa gorliwej strategii ewaluacji wyrażeń.<br />
Dla większości programistów jest ona niezauważalna, jak dla molierowskiego pana Jourdain — proza.

Otóż gdy w języku gorliwym funkcja

``` haskell
k x y = x
```

użyta zostanie w wyrażeniu

``` haskell
k 42 (fib 1234567)
```

przed wywołaniem funkcji `k`, obliczone zostaną jej argumenty: `42` (już obliczone) i `fib 1234567` (dużo pracy).<br />
Praca ta zostanie wykonana na darmo, albowiem `k` nie uzywa swojego drugiego argumentu.

## Leniwa ewaluacja

Alternatywą dla gorliwej ewaluacji jest *leniwa ewaluacja* - wartości argumentów są obliczane kiedy (i o ile w ogóle są potrzebne).
W Haskellu obowiązuje taki własnie paradygmat. Dlaczego jednak wszystkie języki go nie używają? Przyczyny są dwojakiego rodzaju:

- Implementacja leniwej ewaluacji jest trudniejsza - do funkcji nie przekazujemy wartości,<br/> ale domknięcie, które pozwoli ją obliczyć.

- W przypadku gorliwej ewaluacji łatwiej przewidzieć sposób i kolejność realizacji efektów ubocznych; rozważmy np.

``` haskell
main = f (print 1) (print 2)
```

W przypadku gorliwej ewaluacji możemy się spodziewać że przed wywołaniem `f` wypisane zostanie 1 i 2<br />
 (chociaż nie każdy język zagwarantuje nam kolejność).

W przypadku leniwej ewaluacji nie wiemy kiedy i czy w ogóle cokolwiek zostanie wypisane.

## Wartość nieokreślona

Obliczenia mogą nie prowadzić do wyniku (błąd, zapętlenie).

Aby jednak zachować zasadę, że każde poprawne wyrażenie opisuje jakąś wartość, <br />
czasami wprowadza się "wartość nieokreśloną": $\bot$ (tzw. pinezka, ang. *bottom*).

Dokładniej, wartością wyrażenia jest $\bot$,<br />
 jeśli jego obliczenie w porządku normalnym prowadzi do błędu lub zapętlenia.

W Haskellu taką wartość mają np

``` haskell
bottom1 = undefined
bottom2 = error "some message"
bottom3 = bottom3
```

## Funkcje rygorystyczne i pobłażliwe

Jeśli $f(\bot) = \bot$, mówimy że funkcja $f$ jest *rygorystyczna*  albo *pedantyczna* (ang. *strict*).

W przeciwnym wypadku mówimy, że jest *pobłażliwa*  (ang. *non-strict*).

W wypadku funkcji wieloargumentowej możemy mówić, ze funkcja jest rygorystyczna ze względu na któryś argument.

Rozważmy na przykład funkcje

``` haskell
id x = x
const x y = x
```

Funkcja `id` jest rygorystyczna.

Funkcja `const` jest rygorystyczna dla pierwszego argumentu, ale pobłażliwa dla drugiego:

``` haskell
id undefined = undefined
const undefined y = undefined
const x undefined = x
```

<!--
Nie jest jednak w pełni rygorystyczna:
$\quad const(\bot) = \lambda y.\bot \neq \bot$
-->

Gorliwa (eager) ewaluacja (najpierw argumenty) daje funkcje pedantyczne.<br/>
Leniwa (lazy) ewaluacja (dopiero kiedy trzeba) pozwala na funkcje pobłażliwe.

### Czystość, pobłażliwość, lenistwo

Haskell jest językiem czystym, przez co rozumiemy przede wszystkim zasadę przejrzystości.

Spójrzmy jeszcze raz na funkcję

``` haskell
k x y = x
```

zgodnie z zasadą przejrzystości **dla wszystkich** x, y mamy `k x y = x`.<br />
Jest ona zatem pobłażliwa dla drugiego argumentu.

W związku z tym nie może ona być (zawsze) ewaluowana gorliwie, bo gorliwa ewaluacja nie pozwala na pobłażliwość<br />
- jeżeli ewaluacja `y` skończy się błędem/zapętleniem, to tak skonczy się ewaluacja `k x y`.

Z kolei widać, że argument `x` jest zawsze używany, zatem obliczenie go przed wywołaniem (albo równolegle) nie zmieni znaczenia programu.

Niestety w ogólności problem czy funkcja jest pedantyczna jest nierozstrzygalny (tak jak problem stopu).

Dlatego domyślna ewaluacja jest leniwa, jednak kiedy kompilator potrafi wykazać, że funkcja jest pedantyczna,<br/> może użyć (wydajniejszej) gorliwej ewaluacji.

### Leniwa ewaluacja struktur danych

Leniwa ewaluacja oznacza że wartości wyrażeń obliczane są wtedy kiedy są potrzebne,<br />
ale też tylko w takim stopniu, w jakim są potrzebne

``` haskell
ghci> take 2 [1..10^80]
[1,2]
```

Funkcja `take` jest (prawie) rygorystyczna, tym niemniej nie ma sensu tworzenia wielkiej listy przed wywołaniem<br />
kiedy potrzebne są tylko dwa elementy;
dlatego tylko te dwa elementy zostaną obliczone


``` haskell
ghci> xs = [1..10^80]::[Integer]
ghci> :print xs
xs = (_t1::[Integer])
ghci> take 2 xs
[1,2]
ghci> :print xs
xs = 1 : 2 : (_t2::[Integer])
```

### Listy "nieskończone"

W poprzednim przykładzie użylismy listy "absurdalnej" długości: $10^{80}$.<br />
Taka lista nigdy się w całosci nie zmaterializuje; w praktyce oznacza "tyle ile trzeba".

Równie dobrze moglibyśmy nie stawiać ograniczenia górnego:

``` haskell
ghci> take 3 [1..]
[1,2,3]
```

W tym sensie Haskell wspiera nieskończone struktury danych.

``` haskell
ghci> zip [0..] "halo"
[(0,'h'),(1,'a'),(2,'l'),(3,'o')]
```

Funkcja `zip` wybiera z listy `[0..]`  tyle elementów ile ma druga lista

### Operacje na listach nieskończonych

Na listach nieskończonych możemy operować nie sprowadzając ich wcześniej do skończonych:

``` haskell
ghci> nats = [0..]
ghci> evens = map (2*) nats
ghci> odds = map (+1) evens
ghci> take 10 odds
[1,3,5,7,9,11,13,15,17,19]

ghci> ys = foldr (:) evens odds
ghci> zs = zip evens ys
ghci> take 10 zs
[(0,1),(2,3),(4,5),(6,7),(8,9),(10,11),(12,13),(14,15),(16,17),(18,19)]
```

Niektóre operacje (takie jak `foldl`, `reverse`)     nie nadają sie dla list nieskończonych (albo bardzo długich),<br />
albowiem wymagają przejścia całej listy zanim zaczną produkować wynik
(nie są produktywne).

``` haskell
foldl f e (x:xs) = foldl f (f e x) xs
```
Z kolei operacje takie jak `foldr`, `map`, `filter`, `zip` są produktywne<br/>
 - na wyprodukowanie każdego kolejnego elementu potrzeba skończonego czasu.

### Lista, która zjada swój ogon

Widzieliśmy przykład obliczania liczb Fibonacciego:

``` haskell
ghci> fibs = 0:1:zipWith (+) fibs (tail fibs)

ghci> take 20 fibs
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
```

Jak to działa?

- pierwsze dwa elementy listy są podane jawnie
- trzeci jest sumą pierwszych elementów `fibs` (0) i `tail fibs` (1)
- czwarty jest sumą drugich elementów `fibs` (1) i `tail fibs` (1)
- i tak dalej: w każdym momencie jest obliczone tyle listy ile potrzeba

![](https://upload.wikimedia.org/wikipedia/commons/7/71/Serpiente_alquimica.jpg)

### Liczby pierwsze

W podobny sposób możemy stworzyć listę wsztstkich liczb pierwszych:

``` haskell
primes :: [Integer]
primes = sieve [2..] where
  sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]
```

albo lepiej:

``` haskell
-- Melissa O'Neill http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
primes3 :: [Int]
primes3 = 2:[x | x <- [3,5..], isPrime x] where
  isPrime x = all (x -/) (factorsToTry x)
  factorsToTry x = takeWhile (\p -> p*p <= x) primes3
  x -/ p = x `mod` p > 0
```

## iterate
Funkcja `iterate` tworzy listę powstałą przez iterowanie zastosowania funkcji:

``` haskell
ghci> :t iterate
iterate :: (a -> a) -> a -> [a]

ghci> take 20 (iterate (+1) 0 )
[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]

ghci> take 17 (iterate (*2) 1 )
[1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536]

ghci> let next a = (a + 2/a) / 2 in takeWhile (\x -> abs(x^2 - 2) > 1e-12) (iterate next 1)
[1.0,1.5,1.4166666666666665,1.4142156862745097,1.4142135623746899]

ghci> let next a = (a + 2/a) / 2 in take 1 (dropWhile (\x -> abs(x^2 - 2) > 1e-12) (iterate next 1))
[1.414213562373095]

ghci> sqrt 2
 1.4142135623730951
```


## Gorliwa aplikacja

Domyślna aplikacja jest ewaluowana leniwie:

``` haskell
square x = x * x

  square (1+2)
= (1+2) * (1+2)
= 3*3
```

Mozemy wymusić (ang. *force*) ewaluację argumentu używając gorliwej aplikacji `$!`:

``` haskell
  square $! (1+2)
= square 3
= 3*3
```

Dla funkcji wieloargumentowych, obliczeniem kazdego argumentu sterujemy oddzielnie:

``` haskell
(f $! x) y   -- wymusza x
f x $! y     -- wymusza y
f $! x $! y  -- wymusza oba
```

Wymuszanie argumentu nie zmienia znaczenia programu jeśli funkcja jest pedantyczna dla danego argumentu<br />
 - zmienia się wtedy tylko moment obliczenia (i zachowanie pamięciowe programu).

### WHNF

Nawet przy użyciu `$!`, ewaluacja odbywa się do tzw. postaci WHNF pierwszego konstruktora (lub lambdy):

``` haskell
ghci> p = (undefined, undefined)
ghci> const 4 p
4
ghci> :print p
p = (_t4::(a, b))
ghci> const 4 $! p
4
ghci> :print p
p = (,) (_t5::a1) (_t6::b1)
```

W tym aspekcie użycie `$!` jest analogiczne do użycia `case`:

``` haskell
ghci> p = (undefined, undefined)
ghci> :print p
p = (_t7::(a, b))
ghci> case p of (_,_) -> 4
4
ghci> :print p
p = (,) (_t8::a2) (_t9::b2)
```
### WHNF Int

Użycie `$!` jest prostsze niż `case`, zwłaszcza dla typów takich jak `Int` gdzie jest to bardziej kłopotliwe:

```
ghci> :set -XMagicHash
ghci> :info Int
type Int :: *
data Int = GHC.Types.I# GHC.Prim.Int#
 	-- Defined in ‘GHC.Types
ghci> import GHC.Types

ghci> x = 40+2::Int
ghci> :print x
x = (_t14::Int)
ghci> case x of (I# _) -> 17
17
ghci> :print x
x = 42
```

Z użyciem `$!`
```
ghci> x = 40+2::Int
ghci> :print x
x = (_t15::Int)
ghci> const 17 $! x
17
ghci> :print x
x = 42
```

### `seq`

Operator `$!` jest zdefiniowany za pomocą pierwotnej funkcji `seq`

``` haskell
seq :: a -> b -> b
```

która wymusza pierwszy argument i daje w wyniku drugi

Zamiast

``` haskell
const 17 $! x
```
możemy napisać

``` haskell
const x 17
```
### Obliczenia z akumulatorem

Przykładem zastosowania `$!` są obliczenia z akumulatorem, np.

``` haskell
sumwith :: Int -> [Int] -> Int
sumwith a [] = a
sumwith a (x:xs) = sumwith (a+x) xs
```

Przy leniwej ewaluacji mamy

``` haskell
  sumwith 0 [1,2,3]
= sumwith (0+1)  [2,3]
= sumwith ((0+1)+2) [3]
= sumwith (((0+1)+2)+3) []
= (1 + 2) + 3 = 3 + 3 = 6
```
niepotrzebnie budujemy, przechowujemy i przekazujemy nieobliczoną sumę;
możemy tego uniknąc pisząc
``` haskell
sumwith v (x:xs) = (sumwith $! (v+x)) xs
```

``` haskell
  sumwith 0 [1,2,3]
= (sumwith $! (0+1)) [2,3] = sumwith 1  [2,3]
= (sumwith $! (1+2)) [3] = sumwith 3 [3]
= (sumwith $! (3+3)) [] sumwith 6 []
= 6
```

### foldl'

Możemy uogólnić ten schemat tworząc gorliwy wariant funkcji foldl

```
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x:xs) = (foldl f (f v x)) xss

foldl’ :: (a -> b -> a) -> a -> [b] -> a
foldl’ f v [] = v
foldl’ f v (x:xs) = ((foldl’ f) $! (f v x)) xs

sumwith' = foldl' (+)
```

Którego `fold` używać?

- w razie wątpliwości - `foldr`
- jeśli przetwarzamy dużą, ale skończoną listę przy użyciu pedantycznej, łącznej operacji (takiej jak `(+)`)<br />
i zależy nam na wydajności, można użyć `foldl'`
- w innych wypadkach -`foldr` (pamiętajmy o fuzji `foldr/build`)
- w praktyce nie ma powodu żeby używać `foldl`

## Strumieniowe I/O

Leniwa ewaluacja jest w konflikcie z efektami ubocznymi; poznaliśmy jeden ze sposobów radzenia sobie z tym: typ IO

Z drugiej strony, strumienie (leniwe listy znaków) pozwalają na inne (historycznie pierwsze) spojrzenie  na I/O:

- program produkuje strumień wyjściowy, (leniwie) konsumując strumień wejściowy


W aktualnych wersjach Haskella takie podejście jest rzadziej używane, ale wciąz dostępne za pośrednictwem funkcji `interact`:

```haskell
smain :: [Char] -> [Char]
main = interact smain
```

## Pytania ?

## Poniedziałek zaczyna się w sobotę

> "Poznanie nieskończoności wymaga nieskończonego czasu.
> toteż wszystko jedno czy się pracuje, czy nie"

- A. B. Strugaccy "Poniedziałek zaczyna się w sobotę"