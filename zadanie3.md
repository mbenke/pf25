---
title: Zadanie 3
---

W tym zadaniu rozszerzamy język z Zadania 2 o konstruktory wartości i dopasowanie wzorca. Tym niemniej nadal nie wprowadzamy żadnej kontroli typów - każda wartość może być zastosowana do dowolnych argumentów.

Na przykład

``` haskell
one = S Z
two = S one
add Z n = n
add (S m) n = S (add m n)
main = add two two
------------------------------------------------------------
add two two
S (add one two)
S (S (add Z two))
S (S two)
S (S (S one))
S (S (S (S Z)))

```

Tak jak w Haskellu, nazwy z wielkiej litery oznaczają konstruktory.

## Składnia
Korzystamy tylko z małego podzbioru skladni Haskella, nie ma typów - każda wartość może być zastosowana do dowolnych argumentów.

Dzieki temu nie musimy pisać własnego parsera, ale możemy skorzystać z biblioteki `haskell-src`.
Wynik `parseModule`  musimy  przekształcić do naszej uproszczonej składni

``` haskell
type Name = String
data Def = Def { defMatches :: [Match] }
data Match = Match
    { matchName :: Name
    , matchPats :: [Pat]
    , matchRhs  ::Expr
    }

infixl 9 :$
data Expr
    = Var Name
    | Con Name
    | Expr :$ Expr
data Pat = PVar Name | PApp Name [Pat]
```


## Dopasowania

Stwierdzenie, czy argument pasuje do wzorca może wymagać wykonania jednego lub więcej kroków redukcji tego argumentu.<br/>
W językach leniwych istotnie zwykle to dopasowanie wzorca wymusza redukcje.<br/>
Tym niemniej argument jest redukowany "tylko tyle ile potrzeba", czyli aż do momentu rozstrzygnięcia czy pasuje do wzorca.


``` haskell
one = S Z
two = S one
add Z n = n
add (S m) n = S (add m n)
main = add two two
------------------------------------------------------------
add two two
S (add one two)
S (S (add Z two))
S (S two)
S (S (S one))
S (S (S (S Z)))
```

### Poziom 1

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

Na tym poziomie można poprzestać na rozwiązaniu, które w takich przypadkach łączy niektóre kroki, oczywiście pod warunkiem że redukcja jest ogólnie poprawna.
Takie rozwiązania mogą liczyć (o ile nie mają innych braków) na ok. 50-60% punktów.


### Poziom 2

Jednym ze sposobów rozwiązania tego problemu jest precyzyjne odnotowywanie wszystkich kroków wraz z kontekstem w jaki się odbywają. Można wykorzystać do tego celu monadę stanu, która będzie przechowywać historię redukcji (wraz z ich kontekstami).
Stan może ponadto zawierać także listę definicji i ilość pozostałego "paliwa" (kroków, po których uznamy, że obliczenie jest zapętlone lub za długie do wyświetlenia), tudzież inne informacje które uznamy za potrzebne.
Do reprezentacji kontekstu i nawigacji wewnątrz wyrażen można uzyć techniki "zipper" (ewentualnie po prostu ścieżki będącej listą elementów "lewo-prawo", ale to słabsze rozwiązanie).

``` haskell
two = S (S Z)
add Z n = n
add (S m) n = S (add m n)
main = add two two
------------------------------------------------------------
{main}
{add two two}
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

Postać wyjścia nie musi być dokładnie taka jak powyżej, ważne aby w poprawny i czytelny sposób ilustrowała proces redukcji.

## Sekwencje

Do przechowywania historii przyda się typ sekwencji, w którym dodawanie elementu na końcu odbywa się w czasie stałym

Zdefiniuj typ

``` haskell
newtype SnocList a = SnocList {unSnocList :: [a]}
toList :: SnocList a -> [a]
fromList :: [a] -> SnocList a
snoc :: SnocList a -> a -> SnocList a
```

oraz instancje `Eq, Show, Semigroup, Monoid, Functor, Applicative, Alternative`.

## Możliwe rozszerzenia
- napisz własny parser (używając gotowej biblioteki kombinatorów parsujących, lub napisz własną)
- dodaj deklaracje typów danych (np. `data Nat = Z | S Nat`) i sprawdzanie czy wzorce wyczerpują wszystkie przypadki

## Wymagania techniczne

Analogicznie jak w poprzednio, w tym zadaniu tworzymy pakiet cabal o nazwie `identyfikator-zadanie3`
(identyfikator ze students, np. mb128410)
który powinien budować się przy użyciu narzędzi ze students (GHC 9.0.2, cabal 3.4);
mile widziane, zeby budowal się też z nowszymi wersjami GHC (np 9.4.8, 9.8.2);
w tym celu warto w pliku cabal zamiast frazy `base^>=4.15.1.0` użyć `base>=4.15.1.0`.

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
