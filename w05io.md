---
title: Programowanie Funkcyjne
subtitle: IO - wejście-wyjście
author:  Marcin Benke
date: Wykład 5, 24.3.2025
---

# IO - wejście-wyjście

* Efekty i obliczenia
* Łączenie obliczeń
* Ważniejsze funkcje IO
* Moduły, eksport/import
* Budowanie programów, CABAL

## Przejrzystość

Haskell jest językiem czystym, w którym obowiązuje zasada przejrzystości:

* każde obliczenie tego samego wyrażenia daje ten sam wynik
* zastąpienie wyrażenia innym wyrażeniem o tej samej wartości daje równoważny program

Na przykład

* `let x = 2 in x+x` jest równoważne `2+2`
* `let f x = x + x in f 2` jest równoważne `2+2`
* `let x = g 2 in x + x` jest równoważne `g 2 + g 2` dla dowolnej funkcji g (odpowiedniego typu).

Podobnie jest (w pewnym zakresie) w innych językach (np. ML, Lisp).

### Efekty uboczne

Sytuacja komplikuje się w obecności efektów ubocznych, np. I/O.

Powiedzmy, że mamy funkcję `readInt :: Handle -> Int` wczytującą liczbę ze strumienia (np. `stdin`). Czy

``` haskell
let x = readInt stdin in x+x
```
jest równoważne 
``` haskell
readInt stdin + readInt stdin
```

Efekty uboczne są w konflikcie z zasadą przejrzystości. Różne języki rozwiązują to na różne sposoby.

W ML niektóre funkcje nie są przejrzyste. W C prawie żadne funkcje nie są przejrzyste.

## Obliczenia

W Haskellu przejrzystość jest zasadą nadrzędną, dlatego<br />
 **nie może** być funkcji takiej jak `readInt :: Handle -> Int`.<br />

Funkcja spełniająca podobną rolę będzie miała typ `Handle -> IO Int`.

Różnica wydaje się kosmetyczna, ale jest w istocie fundamentalna:

- wyrażenie `readInt stdin` nie daje teraz
wartości typu Int, ale **obliczenie**,<br />
 którego wykonanie da wartość typu `Int` (przepis na uzyskanie wartości typu `Int`);
- dzięki temu zachowujemy przejrzystość - każde wywołanie da takie samo obliczenie (wczytaj liczbę z stdin).

### Funkcja `main`
Program w Haskellu generuje obliczenie - funkcja `main` jest typu `IO ()`.<br />
Wykonanie obliczenia, które jest wynikiem `main` przez system wykonawczy realizuje to obliczenie.

Obliczenie to może być dowolnie skomplikowane, ale zaczniemy od bardzo prostego - <br />skorzystamy z funkcji (bibliotecznej)
`putStrLn :: String -> IO ()`

```haskell
main :: IO ()
main = putStrLn "Hello"
```

### Uruchamianie

To jest kompletny program w Haskellu. Umieśćmy go w pliku, dajmy na to, `hello.hs` i uruchommy go (na dwa sposoby):

```
$ runhaskell hello.hs
Hello!

$ ghc hello.hs
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello ...

$ ./hello
Hello!
```

- `runhaskell` uruchamia program przy pomocy interpretera (`ghci`);
- `ghc` kompiluje program, podobnie jak `gcc` dla C.

### Skróty

Pisanie `putStrLn` jest trochę niewygodne, możemy dodać definicje

```haskell
write, writeln :: String -> IO ()
write = putStr
writeln = putStrLn
```

Jak później zobaczymy, może to dać więcej korzyści niż tylko ładnijsza nazwa.

## Łączenie obliczeń

Proste sekwencjonowanie:

```haskell
(>>) :: IO a -> IO b -> IO b

main = write "Hello," >> writeln "world!"
```

...nie wystarczy jeżeli chcemy użyć nie tylko efektu obliczenia, ale także jego wyniku, np. dla funkcji

```haskell
getLine :: IO String
```

mamy problem z jej wynikiem:


``` haskell
getLine >> writeln  -- ?? to nawet się nie typuje
```

Operator `>>` ignoruje wynik pierwszego argumentu i nie wiemy co wypisać.

### Ogólne sekwencjonowanie

Dlatego właściwy operator sekwencjonowania to

```haskell 
(>>=) :: IO a -> (a -> IO b) -> IO b
```

Bierze on 

- obliczenie o wyniku (typu) `a`
- oraz funkcję która na podstawie `a` tworzy obliczenie o wyniku `b`...
- ...i łaczy je w obliczenie o wyniku `b`, np.

```haskell
main = getLine   >>= writeln
--    (IO String)    (String -> IO ()) 
```

Czasami przydają się obliczenia, które dają wynik bez efektów ubocznych.
Mozna je tworzyć przy użyciu funkcji
```haskell
pure :: a -> IO a
```
Tak naprawdę typy sekwencjonowania i `pure` są ogólniejsze - wrócimy jeszcze do tej kwestii.

### Lukier syntaktyczny - notacja **do**

Przy budowaniu obliczeń często pojawia się kod typu
```haskell
foo = obliczenie1 >>= (\x -> 
      obliczenie2 >>= (\y -> 
      obliczenie3))
```

Dla usprawnienia zapisu oraz dla poprawienia czytelności możemy użyć notacji `do`:

```haskell
do { 
  x <- obliczenie1; 
  y <- obliczenie2;
  obliczenie3
}
```

a nawet

```haskell
do
  x <- obliczenie1
  y <- obliczenie2
  obliczenie3
```

**Uwaga:**
`do` jest tylko równoważną notacją, niczym więcej

### `do` i `let`

Konstrukcja
```haskell
do { fragment1; let x=e; fragment2 }
```
jest równoważna
```haskell
do { fragment1; let x = e in do { fragment2 }}
```

Przykład:
```haskell
main = do
   input <- getLine
   let output = map toUpper input
   putStrLn output
```
...oczywiście moglibyśmy to zapisać krócej:
```haskell
main = getLine >>= putStrLn . map toUpper
```
### do bez IO

Podkreślmy jeszcze raz, że do jest tylko lukrem syntaktycznym i można go równie dobrze używać bez I/O np

``` haskell
do
  let x = 2
  let y = 3
  x + y
```

Co jest równoważne 

``` haskell
let x = 2 in let y = 3 in  x + y
```

NB  `do expr` (z jednym wyrażeniem) jest równoważne po prostu `expr` 

### Ważniejsze funkcje IO

```haskell
print     :: Show a => a -> IO ()
putStrLn  :: String -> IO ()
putChar   :: Char -> IO ()
putStr    :: String -> IO ()
getChar   :: IO Char
getLine   :: IO String
getContents :: IO String
getArgs :: IO [String]    -- import System.Environment

exitSuccess, exitFailure :: IO ()
die :: String -> IO ()
```

Funkcja `getContents` daje całą zawartość wejścia jako leniwą listę (strumień) znaków.<br/>
Innymi słowy kolejne znaki bedą wczytywane dopiero kiedy będą potrzebne.


### Operacje na plikach

``` haskell
type FilePath = String
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()

data Handle = ...
stdin, stdout, stderr :: Handle
hClose :: Handle -> IO ()
hFlush :: Handle -> IO ()

hGetChar :: Handle -> IO Char
hGetContents :: Handle -> IO String
hPutStr :: Handle -> String -> IO ()
```

### Operacje na plikach (2)

``` haskell 
openFile :: FilePath -> IOMode -> IO Handle
hClose :: Handle -> IO ()
-- ale lepiej
withFile :: FilePath -> IOMode ->
            (Handle -> IO r) -> IO r

data IOMode = ReadMode
            | WriteMode
            | AppendMode
            | ReadWriteMode

hSetBuffering :: Handle -> BufferMode -> IO ()
data BufferMode = NoBuffering
                | LineBuffering
                | BlockBuffering (Maybe Int)

```

### Przykład


``` haskell
module Main where
import System.Environment
-- import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []  -> getContents >>= runString
    [f] -> readFile f >>= runString 
    _   -> usage

usage :: IO ()
usage = do
  putStrLn "Usage: program [--help] [file]"
  putStrLn "  --help  - display this message"
  putStrLn "  file    - input file 

runString :: String -> IO ()
```

## Moduły

Program w Haskellu jest kolekcją modułów

``` haskell
module Lub where
import TakNie

lub :: TakNie -> TakNie -> TakNie
Tak `lub` x = x
Nie `lub` x = Nie
```

w tym przykładzie widzimy moduł **Lub** importujący moduł **TakNie**

Moduł powinien być umieszczony w pliku o odpowiedniej nazwie (np. moduł **Lub** w pliku `Lub.hs`)<br/>
Wyjątkiem jest moduł **Main** - może być w dowolnym pliku.

Nazwy modułów muszą się zaczynać z wielkiej litery.

### Moduły hierarchiczne
Moduły mogą być (i często są) organizowane hierarchicznie
``` haskell
module Language.Core where
import Common.Pretty
import Language.Core.Types
import Language.Yul
...
```

Moduł **A.B.C** powinien być w pliku `A/B/C.hs`
 (względem korzenia drzewa naszego programu).

### Selektywny eksport i import
Domyślnie moduł eksportuje (a import importuje) wszystkie definicje modułu. Możemy to oczywiście zmienić:

``` haskell
module Utils.Char(decDigit,isDigit) where
import Data.Char(isDigit,ord)

decDigit :: Char -> Int
...
```

W tym przykładzie 

- z  **Data.Char** importujemy symbole `isDigit, ord`
- eksportujemy własną funkcje \textbf{decDigit};
- reeksportujemy zaimportowaną definicję `isDigit`.

### Import kwalifikowany i ukrywanie
Do nazw możemy się też odwoływać, kwalifikując je nazwą modułu

``` haskell
Prelude.map (Prelude.+ 1) [1..9] Prelude.++ [11::Prelude.Word]
```

Jeśli chcemy, żeby zaimportowane nazwy nie mieszały się z lokalnymi, możemy użyć  **import qualified**

``` haskell
import qualified Data.Vector as V
import qualified Data.List as L

L.map (*2) $ V.toList $ V.map (+1) $ V.fromList[0..9]
```

Z kolei jeśli chcemy zaimportować wszystkie nazwy oprócz kilku,
możemy użyć **hiding**:

``` haskell
import Prelude hiding(map, filter, (<>))
map = ...
```

## Kompilacja programu wielomodułowego

Wśród modułów programu jeden musi być główny:

``` haskell
module Main(main) where
import My.Other.Module
...
main :: IO ()
```

Najprościej zbudować program przez

```
ghc -o nazwa main.hs
```

Moduł **Main** stanowi wyjątek od reguły nazywania plików, może się nazywać jakkolwiek (zwykle tak jak cały program)

```
ghc nazwa.hs
```

stworzy plik wykonywalny `nazwa`.

# Cabal

CABAL — *Common Architecture for Building Applications and Libraries*

Narzędzie budowania pakietów i aplikacji z nich korzystających (jak `cargo` dla Rust, czy `maven` dla Javy).

Podobnie jak Java, kompilator `ghc` potrafi zbudować program złożony z wielu modułów — o ile mamy wszystkie importowane moduły.

Często jednak korzystamy z zewnętrznych bibliotek — modułów pogrupowanych w *pakiety*.

## Pakiety

Często korzystamy z zewnętrznych bibliotek modułów pogrupowanych w *pakiety*.

Najczęściej korzystamy z pakietów dostępnych w centralnym repozytorium: `https://hackage.haskell.org/`

Na przykład w pakiecie `containers` są dostępne struktury danych takie jak:

- słowniki — `Data.Map`, 
- zbiory  — `Data.Set`,
- sekwencje — `Data.Sequence`

Patrz `https://hackage.haskell.org/package/containers`

Pakiet `base` jest zawsze dostępny, przy instalacji `ghc` często instalowane są popularne pakiety takie jak `containers` czy `mtl`.

Inne pakiety musimy osobno zainstalować (jeśli budujemy bezpośrednio przez `ghc`) albo dołączyć do projektu (jeśli uzywany `cabal`).


## Tworzenie projektu (pakietu)

```
$ cabal update
$ mkdir foo && cd foo
$ cabal init -n
...
$ cabal build
...
$ cabal run

```

Polecenie `cabal init` stworzy projekt `foo.cabal` złożony z jednego programu; ważniejsze elementy:

``` cabal
-- Tej częsci można nie ruszać
cabal-version:      3.4
name:               demo
version:            0.1.0.0

-- Tu edytujemy:
executable foo
    main-is: Main.hs
    
    other-modules: My.Modules.One, My.Modules.Two
    -- Pakiety niezbędne do zbudowania
    build-depends:
      base ^>=4.16.4.0,
      containers >= 0.6,
    hs-source-dirs:   app
```
Wcięcia są istotne!

Moze być więcej programów, biblioteki, testy.

## Demo

```
$ mkdir foo && cd foo

$ cabal init -n           # tworzy plik .cabal; bez -n interaktywnie

$ cabal build             # buduje projekt

$ cabal run               # uruchamia, jeśli jedno executable

$ cabal run foo

$ cabal sdist  -o ..      # tworzy paczkę ze źródłami w podanym katalogu 
```
### Użyteczny przykład

```
cabal-version:      3.4
name:               zadanie2
version:            0.1.0.0

executable zadanie2
    main-is:          Main.hs
    other-modules: Syntax, Pretty, Fun, Reduce, FromHs

    build-depends:
        base >=4.15.1.0,
        haskell-src,
        pretty,
        containers,

    hs-source-dirs:   app
    default-language: GHC2021
```

### Większy przykład

```
common warnings
    ghc-options: -Wall

common deps
    build-depends: base ^>=4.19.1.0,

library
    import:           warnings, deps
    exposed-modules:  Syntax, Pretty, Fun, Reduce, FromHs
    build-depends:    haskell-src, pretty, containers,
    hs-source-dirs:   src

executable zadanie2
    import:           warnings, deps
    main-is:          Main.hs
    build-depends:    zadanie2
    hs-source-dirs:   app

test-suite zadanie2-test
    import:           warnings, deps
    type:             exitcode-stdio-1.0
    hs-source-dirs:   test
    main-is:          Main.hs
    build-depends: zadanie2
```

## Cabal bez pliku .cabal

``` haskell
#!/usr/bin/env cabal
{- cabal:
   build-depends: base >=4.11 && <5, containers
-}   

import qualified Data.Map as Map

main = do
  print "Hello, cabal"
  print (Map.fromList [("ala",1), ("ela",7)])
```

```
$ chmod +x ./helloCabal.hs
$ ./helloCabal.hs 
"Hello, cabal"
fromList [("ala",1),("ela",7)]
```
# Pytania ?

# Bonus

### Dialogi

```haskell
main = do
  putStrLn "Hej, co powiesz?"
  input <- getLine
  putStrLn $ "Powiedziałeś: " ++ input
  putStrLn "Do widzenia"
```

#### Pułapka - buforowanie

Jeśli w poprzednim przykładzie użyjemy `putStr` zamiast `putStrLn`,
przeważnie efekt będzie inny niż oczekiwany.<br />
Nie ma to związku z Haskellem, a tylko ze standardowym buforowaniem terminala. Możemy ten problem rozwiązać np. tak:

```haskell
import System.IO

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    hFlush stdout
    getLine

main = do
  input <- promptLine "Prompt>"
  putStrLn $ "Powiedziałeś: " ++ input
```

### Wyłączenie buforowania
Innym rozwiązaniem jest wyłączenie buforowania:

```haskell
import System.IO

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine

main = do
  hSetBuffering stdout NoBuffering
  input <- promptLine "Prompt> "
  putStrLn $ "Powiedziałeś: " ++ input
```

### Dialog w pętli
Pętlę możemy zastapić rekurencją:

```haskell
main = do
  input <- promptLine "> "
  if doesQuit input
     then return ()
     else processInput input >> main

doesQuit :: String -> Bool
doesQuit "q" = True
doesQuit "quit" = True
doesQuit _ = False

promptLine :: String -> IO String
promptLine prompt = putStr prompt >> getLine


processInput :: String -> IO ()
processInput input = 
  putStrLn $ "Powiedziałeś: " ++ input
```

NB rekurencyjne wywołanie funkcji `main` jest możliwe, ale w praktyce zwykle lepiej uzyć osobnej funkcji.

