---
title: Zadanie 3
---

W tym zadaniu rozszerzamy język z Zadania 2 o konstruktory wartości i dopasowanie wzorca. Tym niemniej nadal nie wprowadzamy żadnej kontroli typów.

Na przykład

``` haskell
two = S (S Z)
add Z n = n
add (S m) n = S (add m n)
main = add (S (S Z)) two
------------------------------------------------------------
add (S (S Z)) two
S (add (S Z) two)
S (S (add Z two))
S (S two)
S (S (S (S Z)))
```

## Dopasowania

Stwierdzenie, czy argument pasuje do wzorca może wymagać wykonania jednego lub więcej kroków redukcji tego argumentu. W językach leniwych istotnie zwykle to dopasowanie wzorca wymusza redukcje. Tym  niemniej argument jest redukowany "tylko tyle ile potrzeba", czyli aż do momentu rozstrzygnięcia czy pasuje do wzorca.


``` haskell
add two two
add (S (S Z)) two
S (add (S Z) two)
S (S (add Z two))
S (S(two))
S (S (S (S Z)))
```

Przy prostej implementacji dopasowania wzorca może się zdarzyć że takie "wymuszone" redukcje pozostaną nieodnotowane, np.

``` haskell
two = S (S Z)
add Z n = n
add (S m) n = S (add m n)
main = add two two
------------------------------------------------------------
add two two
S (add (S Z) two)
S (S (add Z two))
S (S two)
S (S (S (S Z)))
```

### Poziom 1

Na tym poziomie można poprzestać na rozwiązaniu, które w takich przypadkach łączy niektóre kroki, oczywiście pod warunkiem że redukcja jest ogólnie poprawna.
Takie rozwiązania mogą liczyć (o ile nie mają innych braków) na ok. 60-70% punktów.


### Poziom 2

Jednym ze sposobów rozwiązania tego problemu jest precyzyjne odnotowywanie wszystkich kroków wraz z kontekstem w jaki się odbywają. Można wykorzystać do tego celu monadę stanu, która będzie przechowywać historię.
Stan może ponadto zawierać także listę definicji i ilość pozostałego "paliwa" (kroków, po których uznamy, że obliczenie jest zapętlone lub za długie do wyświetlenia), tudzież inne informacje które uznamy za potrzebne.
Do reprezentacji kontekstu i nawigacji wewnątrz wyrażen mozna uzyć techniki "zipper" (ewentualnie po prostu ścieżki zbędącej listą elementów "lewo-prawo", ale to słabsz rozwiązanie).

``` haskell
two = S (S Z)
add Z n = n
add (S m) n = S (add m n)
main = add two two
------------------------------------------------------------
{main}
{add two two}
add {S (S Z)} two
add {S (S Z)} two
{S (add (S Z) two)}
S {add (S Z) two}
S {S (add Z two)}
S (S {add Z two})
S (S {two})
S (S {S (S Z)})
S (S (S {S Z}))
S (S (S (S {Z})))
```

## Sekwencje

Do przechowywania historii przyda się typ sekwencji, w którym dodawanie elementu na końcu odbywa się w czasie stałym

Zdefiniuj typ

``` haskell
newtype SnocList a = SnocList {unSnocList :: [a]}
toList :: SnocList a -> [a]
fromList :: [a] -> SnocList a
snoc :: SnocList a -> a -> SnocList a
```

oraz instancje `Eq, Show, Semigroup, Monoid, Functor, Applicative, Alternative` (mozna używać `deriving`)

## Wymagania techniczne

Analogicznie jak w poprzednio, w tym zadaniu tworzymy pakiet cabal o nazwie `identyfikator-zadanie3`
(identyfikator ze students, np. mb128410)
który powinien budować się przy użyciu narzędzi ze students (GHC 9.0.2, cabal 3.4);
mile widziane, zeby budowal się też z nowszymi wersjami GHC (np 9.4.8, 9.8.2)

Pakiet powinien dostarczać co najmniej plik wykonywalny `zadanie3`, n.p.

```
$ cabal run -- zadanie3
Usage: zadanie3 [--help] [file]
  --help  - display this message
  file    - file with program to reduce
```

Oddajemy pojedynczy plik `.tar.gz` stworzony poprzez `cabal sdist`

```
$ cabal sdist
Wrote tarball sdist to
/home/ben/Zajecia/pf/code/zadanie3/dist-newstyle/sdist/zadanie3-0.1.0.0.tar.gz
```

Proszę sprawdzić, że pakiet otrzymany z rozpakowania tego pliku się buduje.

Zadanie MUSI być rozwiązane samodzielnie.
Wszelkie zapożyczenia muszą być wyraźnie zaznaczone z podaniem źródła.
Dotyczy to także kodu wygenerowanego/zasugerowanego przez narządzia AI i pokrewne
(VS Code, Copilot, ChatGPT, Claude itp.)

Ponadto student musi umieć objaśnić sposób działania każdego fragmentu oddanego kodu
(wyjaśnienia typu "Znalazłem na Stackoverflow/Copilot mi podpowiedział i działa ale nie wiem jak" itp => 0p).
