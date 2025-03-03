---
title: Programowanie Funkcyjne
subtitle: Typy
author:  Marcin Benke
date: Wykład 2, 3.3.2025
---

## Typy

Każde (poprawne) wyrażenie ma typ. <br />
Typ wartości jest pewną klasą abstrakcji: <br /> wskazuje własności wspólne dla wartości tego typu.

Na najniższym poziomie, komputery operują na ciągach bitów.
<br />
System typów pozwala na tworzenie abstrakcji - nadaje nowe znaczenia ciągom bitów  <br />
(to jest adres, to jest liczba, to jest numer rezerwacji).

Haskell ma *silne typowanie* - typy nie zmieniają się w trakcie obliczeń (nie ma rzutowań).

Typy są wyprowadzalne - interpreter/kompilator potrafi odtworzyć typ dowolnego wyrażenia.
<br />
W zwiazku z tym deklarowanie typów funkcji nie jest obowiązkowe, ale jest użyteczną dokumentacją (sprawdzaną przez kompilator!)


## Typy

Wyrażenia opisują wartości. 

Typ to zbiór wartości, które mają wspólne cechy (operacje jakie można na nich wykonać, reprezentacja).

Z kolei typ funkcji mówi, na wartościach jakiego typu ona działa

Oczywiście funkcje też są wartościami, ale nie każda wartość jest funkcją

W ghci możemy zapytać o typ dowolnego wyrażenia

```
ghci> :type words "a b c"
words "a b c" :: [String]
ghci> :t words
words :: String -> [String]
ghci> :type (+)
(+) :: Num a => a -> a -> a
ghci> :type +d (+)
(+) :: Integer -> Integer -> Integer
```

### Podstawowe rodzaje typów

- typy wbudowane (np. `Int, Char`)
- typy funkcyjne (np. `Bool -> Bool`)
- synonimy (aliasy) (np. `String = [Char]`)
- typy danych, np. 
     - `data Color = Red | Green | Blue`
     - `data ExitCode = ExitSuccess | ExitFailure Int`
     - `data Nat = Zero | Succ Nat`
     - `data Maybe a = Nothing | Just a`


jak się przekonamy typy wbudowane są tak naprawdę typami danych,<br />
ale z uwagi na towarzyszącą im odrobinę "magii" można je traktować specjalnie.

Podstawowe typy i operacje na nich są zdefiniowane w module `Prelude` który jest importowany domyślnie.

### Liczby całkowite

* `Int` - maszynowe liczby całkowite (64 bity)
* `Integer` - liczby całkowite dowolnej precyzji


**Arytmetyka**

Zwykłe operacje `+ - *`

Dzielenie z resztą przy użyciu funkcji `div` oraz `mod`

```
ghci> div 5 2
2
ghci> mod 5 2
1
ghci> div (-5) 2
-3
```

**NB** literały ujemne wymagaja nawiasów

Potęgowanie

```
ghci> 2 ^ 100
1267650600228229401496703205376
```

## Liczby zmiennoprzecinkowe

Zwykłe operacje `+ - *`

Dzielenie

```
ghci> 5 / 2
2.5
```

Potęgowanie

```
ghci> 2.5 ^ 2
6.25
ghci> exp 1
2.718281828459045
ghci> pi ** it
22.45915771836104
```

Istnieją dwa standardowe typy zmiennoprzecinkowe: `Float` oraz `Double`.<br/>
Podobnie jak w C, zasadniczo używamy tylko `Double` (jeśli w ogóle).

### Priorytety

Operacje arytmetyczne mają zwyczajowe priorytety

```
ghci> 1+2*3^4
163
```

aplikacja funkcji ma priorytet wyższy niż wszystkie operatory

```
ghci> div 9 2 ^ 2
16
ghci> div 9 (2 ^ 2)
2
```

aplikacja wiąże w lewo czyli `f x y = (f x) y`

Operator `$` oznacza aplikację, ale ma najniższy priorytet i wiąże w prawo. 


``` haskell
f $ g $ a + b = f(g(a + b))
```

...ale nawiasy są często bardziej czytelne.

### Operatory i przecięcia

Operatory są tak naprawdę funkcjami.<br />
Tradycyjnie piszemy je infiksowo, ale można ich używać też prefiksowo jak innych funkcji:

```
ghci> (+) 2 2
4
```

Możemy też podać jeden z argumentów, uzyskamy wtedy funkcję jednoargumentową, np. `(+1) (*2) (1/)`.

Wyjątek: `(-1)` oznacza liczbę ujemną, a nie funkcję. W razie potrzeby `(+(-1))` albo `\x->x-1` (lub `pred`)

Identyfikator złożony z symboli (z wyjątkiem zastrzeżonych) domyślnie jest używany infiksowo.

Identyfikator alfanumeryczny jest domyslnie używany prefiksowo, ale można go też używać infiksowo

```
ghci> 5 `div` 2
2
```

Zbiór operatorów nie jest zamknięty; możemy definiować własne, np.

```
x +++ y = (x+y)*(x+y+1) `div` 2
(!=) = (/=)
```


## Wartości logiczne

`True, False :: Bool` 

(potem dlaczego z wielkiej litery)

Zwykłe operacje `&&`, `||`

Wyrażenie **if**:

`if e1 then e2 else e3`

(wartością `e1` musi byc `True` lub `False`)

```
ghci> 1 == 1
True
ghci> 1 > 2
False
```

Podobnie inne porównania: `  /=  <  <=  >=`

### i/lub

Operacje `&&`, `||` są definiowalne

``` haskell
False && _ = False
True  && x = x

False || x = x
True  || _ = True
```

Są one pobłażliwe dla drugim argumentu- nie ewaluują go gdy pierwszy przesądza wynik.

Podobnie można by zdefiniować funkcje `if_then_else`

``` haskell
if_then_else False t _ = t
if_then_else True  _ e = e
```

ale w języku mamy już konstrukcję `if then else`:

``` haskell
mn x y = if x < y then x else y
```

### Definicje warunkowe

Definicja może zawierać warunki (guards):

``` haskell
mn x y | x <  y = x
       | x >= y = y
```

Warunki muszą  być wyrażeniami typu Bool.

`otherwise` jest lukrem syntaktycznym dla `True`.

``` haskell
mn x y | x <  y    = x
       | otherwise = y
```

Warunki sprawdzane są od góry do dołu, pierwszy spełniony wygrywa;<br/>
w dobrym stylu jest jednak pisać warunki rozłączne.

## Krotki

Krotki podobnie jak w Pythonie; mogą mieć różne rozmiary (w tym 0, ale nie 1) i różne typy elementów:

``` haskell 
(4, False)
('a', "train", 7)
()
```

W wyrażeniu `f(x,y)` argumentem funkcji `f` jest para `(x,y)`. 

Oczywiście krotki mogą być argumentami funkcji, ale pamiętajmy, że `f(x,y)` to nie to samo co `f x y`

Krotki można tworzyć też używając prefiksowego konstruktora

``` haskell
(,) 4 False
(,,) 'a' "train" 7
```

## Pary

Najczęściej używaną wersją krotek są pary.

Dostęp do elementów pary jet możliwy przy użyciu funkcji

``` haskell
fst :: (a,b) -> a
snd :: (a,b) -> b
```

ale możemy też użyć krotek bezpośrednio w definicji funkcji

``` haskell
pair :: (a->b, a->c) -> a -> (b,c)
pair (f,g) x = (f x, g x)

cross :: (a->b, c->d) -> (a,c) -> (b,d)
cross (f,g) = pair (f . fst, g . snd)
```

Ćwiczenie: napisz funkcje

``` haskell
first :: (a->b) -> (a,c) -> (b,c)
second :: (a->b) -> (c,a) -> (c,b)
```

## Listy

-   `[]` — lista pusta

-   `(x:xs)` — lista o głowie x i ogonie xs


```
ghci> [1,2,3]
[1,2,3]
ghci> [4] ++ [5,6]
[4,5,6]
ghci> 1:[]
[1]
ghci> [0..9]
[0,1,2,3,4,5,6,7,8,9]
ghci> [1,3..10]
[1,3,5,7,9]
ghci> take 4 [0..9]
[0,1,2,3]
```

NB w razie potrzeby istnieją wyspecjalizowane implementacje np **Data.Sequence**, **Data.Vector**.

## Napisy

Napisy są listami znaków

``` haskell
    ghci> "napis"
    "napis"
    ghci> ['H','a','s','k','e','l','l']
    "Haskell"
    ghci> unwords ["hello","world"]
    "hello world"
```

NB w razie potrzeby, istnieją bardziej efektywne implementacje, np.
**Data.Text, Data.ByteString**.


### Ciągi arytmetyczne

Łatwo domyśleć się, jaką listę oznacza wyrażenie `[1..9]`.

``` 
ghci> [1..9]
[1,2,3,4,5,6,7,8,9]
```

Podobnie możemy zapisać inne ciągi arytmetyczne,
na przykład `[1,3..9]`. Trochę więcej myślenia wymaga `[0,2..9]`.

```
ghci> [0,2..9]
[0,2,4,6,8]
```

Notacja `..` jest dostepna dla niektórych innych typów, np

```
ghci> ['0'..'9']++['A'..'F']
"0123456789ABCDEF"
```

Dla liczb zmiennoprzecinkowych wyniki mogą być dziwne. Z ~~pszczołami~~ floatami nigdy nic nie wiadomo.

### Wycinanki listowe

W matematyce często tworzymy zbiory przy pomocy aksjomatu wycinania:

{ 3x | x∈{1,...,10\} x mod 2 = 1}

Podobnie możemy tworzyć listy w Haskellu:

    ghci> [3*x | x <- [1..10], mod x 2 == 1]
    [3,9,15,21,27]

`x<-[1..10]` jest *generatorem*

`mod x 2 == 1` jest *filtrem*. Możemy tworzyć wycinanki o większej
liczbie generatorów i filtrów, np.

    ghci> [(x,y) | x<-[1..5], y<-[1,2,3], x+y == 5, x*y == 6]
    [(2,3),(3,2)]

(Python: `[(x,y) for x in range(1,6) for y in range(1,4) if x+y==5 if x*y==6]`)

Większy przykład:
``` haskell
f m = [ (i,j,k) | i <- [0..(m `div` 3)], j <- [i..(m-i `div` 2)], let k = m - (i+j) ]
```
Do tematów związanych z listami będziemy wracać na kolejnych wykładach 

## Typy funkcyjne

Typ `T -> U` (gdzie T,U - typy) oznacza typ funkcji z T do U.

Konwencja składniowa: `T -> U -> V` oznacza `T -> (U ->V)`

(funkcja dwuargumentowa to funkcja, która dostawszy jeden argument daje funkcję jednoargumentową).

**Przykłady**

`Int -> Int -> (Int, Int)`<br />
oznacza funkcję, która bierze dwa argumenty typu `Int` i daje w wyniku parę takich wartości.


`(Int, Int) -> Int`<br />
bierze argument, który jest parą i daje jeden `Int`.


`(Int -> Int) -> Int -> Int`<br />
bierze funkcję typu `Int -> Int` oraz wartość typu `Int` i daje `Int`.

`Int -> Int -> (Int -> Int)`<br />
— równoważne `Int -> Int -> Int -> Int`; trzy argumenty `Int`, wynik `Int`

### Tworzenie funkcji

Podstawowym mechanizmem tworzenia funkcji jest lambda:

jeśli `e` jest wyrażeniem typu `U`, 

w którym może występować zmienna `x :: T`

To `\x -> e` jest funkcją typu `T -> U`

Typ x możemy podać: `\(x::T) -> e` ale zwykle nie jest to potrzebne.

Zamiast `f = \x -> e` zwykle piszemy `f x = e`, 
ale te dwie formy są (prawie) równoważne.

Możemy podac kilka argumentów np `\x y z -> (x, (y,z))`.

Konwencja składniowa: lambda sięga tak daleko jak może, czyli  zwykle do końca wyrażenia<br> albo pierwszego niezrównoważonego nawiasu itp.

### Eliminacja (użycie) funkcji

Podstawowym sposobem użycia funkcji jest jej zastosowanie do argumentu:

jeśli `e1` jest wyrażeniem (być może złożonym) typu `T -> U`<br>
zaś `e2` jest typu `T`, to `(e1)(e2)` jest typu `U`

Oczywiście możemy pominąć nawiasy wokół zmiennych/stałych, np.
`f 2` zamiast `(f)(2)`.

Konwencja sładniowa: `f x y` oznacza `(f x) y`

Przykłady:

``` haskell
(\x -> x + 1) (-1)                        -- nawiasy potrzebne
abs 1 = abs(1) = (abs)(1) = (abs)1        -- nawiasy zbędne
(\x y z -> (x, (y,z))) 1 2 3              -- nawiasy potrzebne
(\x -> \y -> \z -> (x, (y,z))) 1 2 3      -- nawiasy potrzebne
```

## Polimorfizm

Czasami funkcje moga działać na obiektach różnych typów, np.

```
id x = x
```

albo

```
fst (a,b) = a
```

Nazywamy je polimorficznymi (mają typy polimorficzne)

```
id :: a -> a
fst :: (a, b) -> a
```

"Dla dowolnych typów $a$ i $b$, funkcja dająca dla argumentu typu $(a,b)$ wynik typu $a$."

W takim typie a i b nazywamy *zmiennymi typowymi* - w ich miejsce można podstawić dowolne typy.


### Składanie funkcji
Użytecznym przykładem funkcji polimorficznej (a zarazem operującej na funkcjach) jest złożenie funkcji<br>
— standardowo zdefiniowane jako operator `(.)` —  ponieważ `f . g` ma optycznie przypominać $f\circ g$

``` haskell
(.) :: (b->c) -> (a->b) -> (a -> c)
(f . g) = \x -> f(g x)
```

Przykłady:

``` haskell
ghci> (unwords . reverse . words) "Ala ma kota"
"kota ma Ala"
```

``` haskell
fromHsString :: String -> Prog
fromHsString = Prog . fromParseResult . parseModule
```

Jesli ktos woli, może wybrać odwrotną kolejność składania:

``` haskell
ghci> (&) = flip (.)
ghci> revwords = words & reverse & unwords
ghci> revwords "Ala ma kota"
"kota ma Ala"
```
NB `flip` jest innym przykładem funkcjonału - zamienia kolejność argumentów
``` haskell
flip :: (a -> b -> c) -> b -> a -> c

```
### Parametryczność


Ważne aby pamiętać, że możliwość wyboru typu leży po stronie **wywołującego**. 

Oznacza to, że implementacja funkcji musi działać dla **wszystkich** typów.

Co więcej, musi działać **dla wszystkich typów tak samo**.

### Theorems for free!

Dlaczego parametryczność jest ważna?

1. Umożliwia *wycieranie typów*. Skoro funkcja działa dla każdego typu tak samo, <br /> 
nie potrzebuje informacji,
jakiego typu są jej faktyczne parametry. 

    W związku z tym typy nie są potrzebne w trakcie wykonania, a jedynie w trakcie kompilacji.

2. Ograniczenie sposobów działania funkcji polimorficznych daje nam **twierdzenia za darmo** <br>
(*theorems for free*, termin ukuty przez Phila Wadlera, a zarazem tytuł jego [słynnej pracy](https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf)).

Rozważmy na przykład funkcje o następujących sygnaturach

```haskell
zagadka1 :: a -> a
zagadka2 :: a -> b -> a
```

ile różnych implementacji potrafisz napisać?


### Darmowe twierdzenia
```haskell
zagadka1 :: a -> a
zagadka2 :: a -> b -> a
```

ile różnych implementacji potrafisz napisać?

Darmowe twierdzenie dla

```
i :: a -> a
```

głosi, że dla dowolnych `A, B` oraz funkcji `f :: A -> B` mamy

$$ f\circ i = i\circ f $$

czyli $i$ musi być identycznością (*)

Podobnie, dla `k :: a -> b -> a`

$$ f(k\ x\ y) = k (f\ x)\ (g\ y) $$

czyli `k x y = x`

.

Dowód (*): Przypuśćmy przeciwnie $i(a)=b \neq a$; jeśli $i(b)=b$ to niech $f(b)=a$, wpp niech $f(b)=b$ - sprzeczność.

### Równość (nie) dla wszystkich

Czy da się zdefiniować uniwersalną równość, czyli parametryczną funkcję typu `eq :: a -> a -> Bool`? 

Możemy podejrzewać, że nie, ale jak to udowodnić?

Otóż "darmowe twierdzenie" dla tego typu mówi że dla dowolnych typów A,B  i funkcji 

```haskell
f :: A -> B
```

mamy

```
(eq x y) = (eq (f x) (f y))
```

co pokazuje, że `eq` jest funkcją stałą, czyli nie jest zbyt użyteczna jako równość<br />
 (chyba, że w sensie "wszyscy są równi").

O tym jak poradzić sobie z tym problemem porozmawiamy na kolejnych zajęciach.

## (Algebraiczne) Typy danych *(ang. datatypes)*

Jednym z silnych mechanizmów abstrakcji jest możliwość definiowania nowych typów danych<br/>
poprzez wskazywanie jakie wartości do nich należą i jak są konstruowane.

``` haskell
data TakNie = Tak | Nie
```

Na przykład standardowy typ `Bool` jest zdefiniowany następująco

``` haskell
data Bool = False | True
```

definicja taka oznacza że do typu `Bool` należą wartośći `False` i `True` (i żadne inne)*.

Trochę ciekawsza może być definicja liczb naturalnych:

``` haskell
data Nat = Zero | S Nat
```

(*) fine print: tak, tak, jeszcze $\bot$

### Liczby naturalne

Poniższa definicja liczba naturalnych nie jest może efektywna, ale może służyć za ilustrację

``` haskell
data Nat = Zero | S Nat
```

oznacza:

- `Zero :: Nat`  (słownie: `Zero` jest typu `Nat`)
- jeśli `n::Nat`, to `S n :: Nat`

Mówimy, że typ `Nat` jest rekurencyjny, bo jego definicja odwołuje się do tegoż `Nat`.

(Typ `Bool` nie był rekurencyjny, był typem wyliczeniowym)

## A morze tak, a może nie [1]

W wielu językach brak wartości możemy wyrazić przy użyciu `null`:

``` java
        Integer i = null;
        i = i+1;
```
niestety zwykle kończy się to tak:

```
Exception in thread "main" java.lang.NullPointerException
```

**W Haskellu nie ma czegoś takiego jak `NullPointerException`.**

Wartości opcjonalne możemy natomiast wyrazić przy użyciu `Maybe`

``` haskell
data Maybe a = Nothing | Just a
```

`Maybe Int` to inny typ niż `Int` i system typów wymusi sprawdzenie,<br />
 czy coś nie jest `Nothing` zanim zaczniemy na tym operować.

.

.

[1] *A morze tak, a może nie* - album zespołu Banana Boat, BananaArt.Pl 2004.

## Maybe maybe maybe

Do eliminacji `Maybe` można użyć funkcji `maybe`:

```
ghci>  :t maybe
maybe :: b -> (a -> b) -> Maybe a -> b

ghci> maybe 42 (+1) Nothing
42

ghci> maybe 42 (+1) (Just 1)
2

ghci> maybe 0 id (Just 1)
1
```

Poznamy także inne sposoby.

### Either ... or

Typ `Either` reprezentuje sumę rozłączną dwóch typów

``` haskell
data Either a b = Left a | Right b
```

Często używany do obsługi błędów, np.

``` haskell
f :: X -> Either String Y
```

Mówi, że wynikiem funkcji jest albo błąd albo wartość typu Y.

Dlaczego `Either String Y` a nie `Either Y String`? Po to żeby poprawna wartość była `Right`.

Czy istnieje funkcja `either`? Oczywiście:

```
ghci> :t either
either :: (a -> c) -> (b -> c) -> Either a b -> c
```


### Drzewa

Drzewa są przykładem typu parametryzowanego (jak `Maybe`) i rekurencyjnego (jak `Nat`)

Z wartościami w liściach:

```haskell
data ETree a = Leaf a | Bin (ETree a) (ETree a)
```

z wartościami w wierzchołkach wewnetrznych:

```haskell
data ITree a = Empty | Node a (ITree a) (ITree a)
```

Drzewa o dowolnym stopniu rozgałęzienia

``` haskell
data RoseTree a = RoseTree a [RoseTree a]
```
### Konstruktory

Przykład: wyrażenia arytmetyczne ze zmiennymi

```haskell
data Exp a = EInt Int | EVar a | Exp a :+ Exp a | EMul (Exp a) (Exp a)
```
- `EInt` jest 1-argumentowym konstruktorem wartości (typu `Int -> Exp a`)
- `EMul :: Exp a -> Exp a -> Exp a`  jest konstruktorem 2-argumentowym
- `:+` jest też 2-argumentowym (ale infiksowym)

Nazwy konstruktorów muszą zaczynać się od wielkiej litery (lub dwukropka dla infiksowych)

`Exp` jest jednoargumentowym konstruktorem typu: jeśli `a` jest typem, to `Exp a` też.

### Dopasowanie wzorca

Jak widzieliśmy wcześniej, funkcje możemy definiować przez przypadki. 

W szczególności dotyczy to typów algebraicznych, np.

``` haskell
not :: Bool -> Bool
not False = True
not True = False

False && x = False
True && x = x

add m Zero = m
add m (S n) = S (add m n)

isEven Zero = True
isEven (S (S n)) = isEven n
isEven _ = False

subtrees :: ITree a -> [ITree a]
subtrees Empty = [Empty]
subtrees t@(Node _ left right) = t : subtrees left ++ subtrees right
```

Dopasowanie wzorca może dotyczyć wielu argumentów i zawierać złożone wzorce.

### Wyrażenie `case`

Dopasowania wzorca (ale tylko jednego naraz) możemy też użyć w wyrażeniach:

``` haskell
nie = \b -> case b of { Tak -> Nie; Nie -> Tak }
```

Nawiasy klamrowe i średniki można pominąć przy systematycznych wcięciach:


``` haskell
nie = \b -> case b of 
             Tak -> Nie
             Nie -> Tak
```

`case` jest wyrażeniem, dlatego dopuszczalne `f( case ...)` albo `1 + case ...`

Wewnętrznie równania z dopasowaniem wzorca są tłumaczone na `case`.


## Etykiety pól

Spójrzmy na definicje

```haskell
    data Point = Pt Float Float
    pointx                  :: Point -> Float
    pointx (Pt x _)         =  x
    pointy ...
```

Definicja **pointx** jest “oczywista”; możemy krócej:

```haskell
    data Point = Pt { pointx :: Float, pointy :: Float }
```

W jednej linii definiujemy typ **Point**, konstruktor **Pt**
oraz funkcje **pointx** i **pointy**.

### Etykiety pól - przykład

Na przyklad zamiast
``` haskell
-- typ świata  gracz kierunek  boxy    mapa    xDim  yDim  lvl mov
data State = S Coord Direction [Coord] MazeMap [Int] [Int] Int Int
```

można

``` haskell
data State = S {
  stPlayer       :: Coord,pozwalające 
  stDir          :: Direction,
  stBoxes        :: [Coord],
  stMap          :: MazeMap,
  stXdim, stYdim :: [Int],
  stLvl, stMov   :: Int
}
```

Oprócz wartościowej dokumentacji uzyskujemy też funkcje wyłuskujące poszczególne składowe, np. `stLvl :: State -> Int`<br/>
oraz możliwość łatwiejszego zapisywania modyfikacji składowych, np. zamiast

``` haskell
foo (S c _ b mm xd yd n mn) = S c D b mm xd yd n (mn+1)
```

wystarczy

``` haskell
foo s = s { stDir = D, stMove = stMove s + 1  }
```
### Synonimy 

Czasem przydatne jest wprowadzenie własnej nazwy (synonimu) dla jakiegoś typu.

Ma to znaczenie dla czytelności kodu, na przykład definiując synonimy

``` haskell 
type Name = String
type Address = String
```

możemy ich używać w sygnaturach funkcji zamiast mniej specyficznego `String`

``` haskell
foo :: Name -> Address -> String
```

oprócz dokumentacji uzyskamy też lepsze komunikaty o błędach:

```
ghci> foo True "a"

<interactive>:2:5: error: [GHC-83865]
    • Couldn't match type ‘Bool’ with ‘[Char]’
      Expected: Name
        Actual: Bool
```

Jednak typ `Name` jest nadal tożsamy ze `String` zatem i z `Address` 
— kompilator nie zaprotestuje jeśli pomylimy nazwisko z adresem.

### Opakowywanie typów: **newtype**

Jeśli chcemy opakować istniejący typ w nowy konstruktor typu, możemy uzyć konstrukcji **newtype**:

```haskell
newtype Kg = Kg { unKg :: Double } deriving (Eq, Ord, Show, Num)
newtype Km = Km { unKm :: Double } deriving (Eq, Ord, Show, Num)
```

Teraz możemy dodawać kilogramy do kilogramów ale nie do kilometrów:

``` haskell
ghci> Kg 5 + Kg 3
Kg {unKg = 8.0}
ghci> Kg 5 + Km 3

<interactive>:4:8: error: [GHC-83865]
    • Couldn't match expected type ‘Kg’ with actual type ‘Km’
    • In the second argument of ‘(+)’, namely ‘Km 3’
      In the expression: Kg 5 + Km 3
      In an equation for ‘it’: it = Kg 5 + Km 3
```

**newtype** działa niemal identycznie jak **data** z jednym konstruktorem<br/>
(ale efektywniej;
pakowanie/odpakowywanie odbywa się w czasie kompilacji a nie wykonania).

przypomnienie: `unKg` jest etykietą pola - automatycznie definiuje funkcję wyłuskująca to pole:

``` haskell
unKg :: Kg -> Double
```

# Podsumowanie
Dzisiaj poznaliśmy:

- Typy bazowe: liczby, znaki, logiczne
- Predefiniowane typy złożone: liczby, krotki, listy
- Typy funkcyjne
- Polimorfizm
- Maybe, Either
- Typy danych użytkownika
- Konstruktory wartści i konstruktory typów

# Administrivia

Zadanie 1 - termin 16 marca

Celem zadania jest stworzenie fukcji do wizualizacji procesu redukcji wyrażeń, na przykład

```
ghci> printPath test1
S K K x
K x (K x)
x

ghci> printPath test3
S B (S B I) x z
B x (S B I x) z
x (S B I x z)
x (B x x z)
x (x (x z))
```