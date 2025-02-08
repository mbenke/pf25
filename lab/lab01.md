0. Instalacja przy pomocy [ghcup](https://www.haskell.org/ghcup/), uruchomienie ghci

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup tui
```
Zainstalować rekomendowane wersje GHC, cabal, HLS.

Stack nie jest wymagany.

1. Obliczanie wyrażeń w ghci

``` haskell
2 + 2
2 ^ 100
negate 10
negate it
negate(-1)
```

co się stanie, jeśli w ostatnim wyrażeniu opuścimy nawiasy?

Komunikat o błędzie może wydawac się dziwny, nie należy się tym przerażać (ale trzeba pamiętać, że liczby ujemne zawsze w nawiasach)

2. Definicje w pliku


``` haskell
sayhello x = putStrLn ("Hello, " ++ x)
```

załaduj plik do ghci przez `:load` (albo jako argument dla ghci) i wypróbuj

``` haskell
sayhello "world"
sayhello world
```

3. Kompilacja

Dodaj do swojego pliku linię

```
main = sayhello "world"
```

i skompiluj

```
ghc hello.hs
./hello
```

Co się stanie, jesli zamiast `putStrLn` uzyć `print`


4. Kombinatory

Zdefiniuj kombinatory S, K, I, B z wykładu (nazwij je z małej litery np `comS`, albo po prostu `s k i b`, ale wtedy uwaga na konflikty nazw).

Oblicz w ghci

``` haskell
-- >>> s k k 42
-- >>> i 42
-- >>> b (+1) (*2) 3
-- >>> s (k s) (s (k k) i) (+1) (*2) 3
```

5. Arytmetyka

Zdefiniuj funkcje dla zera, następnika, dodawania i mnożenia: `zero suc add mul`

(Uwaga: nie `succ`, `+`, `*`; dlaczego okaże się później).

Użyj funkcji

``` haskell
runNat n = n (+1) 0

-- >>> runNat (suc (suc zero))
-- 2

funNat n = n ('|':) ""

-- >>> funNat (suc (suc zero))
-- "||"
```

do przetestowania swoich implementacji

``` haskell
-- >>> runNat two
-- 2
-- >>> runNat (add two three)
-- 5
-- >>> runNat (mul two three)
-- 6
```
## Operacje na napisach

Wypróbuj (wpisuj kolejno w ghci; it oznacza wartość ostatniego wyrażenia)

``` haskell
s = "Ala ma kota"
reverse s
reverse it
words s
unlines it
lines it
concat it
unwords(reverse(words s))
(unwords . reverse . words) it  -- nawiasy!
drop 4 s
take 2 it
takeWhile (<'n') s
isVowel = flip elem "aeiouy"
filter isVowel s
filter (not . isVowel) s
filter (/=' ') s
```