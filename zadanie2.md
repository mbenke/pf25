---
title: Zadanie 2
---

Tematy: typy algebraiczne, klasy, moduły, cabal

Przedmiotem zadania jest znowu stworzenie funkcji wizualizującej proces redukcji wyrażeń kombinatorowych. W odróżnieniu od poprzedniego zadania, tym razem zestaw kombinatorów nie jest ustalony, ale dany przez użytkownika, w składni będącej podzbiorem Haskella, np.


``` haskell
s x y z = x z (y z)
k x y = x
main = s k k x
------------------------------------------------------------
s k k x
k x (k x)
x
```

## Składnia

Korzystamy tylko z małego podzbioru skladni Haskella, nie ma typów - każda wartość może być zastosowana do dowolnych argumentów.
Dzieki temu nie musimy pisać własnego parsera, ale możemy skorzystać z biblioteki `haskell-src`. Ta biblioteka buduje drzewo struktury dla pełnej składni Haskella, musimy je przekształcić do naszej uproszczonej składni

``` haskell
data Def = Def Name [Name] Expr
data Expr = Var Name | Expr :$ Expr
newtype Prog = Prog {progDefs :: [Def]}
```

Zapoznaj się z dokumentacją [haskell-src](https://hackage.haskell.org/package/haskell-src) i napisz funkcje

``` haskell
fromHsString :: String -> Prog
fromParseResult :: ParseResult HsModule -> [Def]
fromHsModule :: HsModule -> [Def]
```
(i inne potrzebne)

## Słownik definicji

W trakcie redukcji będzie nam potrzebne mapowanie nazw kombinatorów na ich definicje. Możemy zdefiniować

``` haskell
type DefMap = Data.Map.Map Name Def

buildDefMap :: Prog -> DefMap
```

## Redukcja

Podobnie jak w poprzednim zadaniu, definiujemy funkcje `rstep` i `rpath` obliczające pojedynczy krok i ścieżke redukcji.

Jak poprzednio redeksem jest kombinator zaaplikowany do właściwej liczby argumentów (być może 0, np. `main`). Redukujemy w kolejnosci normalnej (od zewnątrz i od lewej)

Przez kombinator będziemy rozumieć nazwę, która posiada definicję. Nazwy nie mające definicji będziemy traktować jako zmienne.



``` haskell
one f z = f z
two = suc one
tre = suc two
suc n f z = f (n f z)
o f g x = f (g x)
add m n f x = m f (n f x)
mul m n = o m n
fyr = add two two
six = mul two tre
main = six s z
------------------------------------------------------------
six s z
mul two tre s z
o two tre s z
two (tre s) z
suc one (tre s) z
tre s (one (tre s) z)
suc two s (one (tre s) z)
s (two s (one (tre s) z))
s (suc one s (one (tre s) z))
s (s (one s (one (tre s) z)))
s (s (s (one (tre s) z)))
s (s (s (tre s z)))
s (s (s (suc two s z)))
s (s (s (s (two s z))))
s (s (s (s (suc one s z))))
s (s (s (s (s (one s z)))))
s (s (s (s (s (s z)))))
```

albo


``` haskell
s x y z = x z (y z)
k x y = x
i = s k k
om x = x x
omega = om om
main = k i omega z
------------------------------------------------------------
k i omega z
i z
s k k z
k z (k z)
0
```

Dla zrealizowania kroku redukcji potrzebna będzie funkcja, która dokona podstawienia wyrażeń stanowiących parametry faktyczne w miejsce parametrów formalnych w ciele kombinatora, np.

``` haskell
subst :: (Name, Expr) -> Expr -> Expr
```
**Uwaga o zmiennych:** w trakcie redukcji trzeba uważać, żeby nie pomieszać zmiennych, inaczej moze nam się przydarzyć błędna redukcja:

```
s x y z = x z (y z)
k x y = x
main = s k k x
------------------------------------------------------------
s k k x
k k (k k) -- BAD!
k
```

Najprostszym sposobem uniknięcia tego problemu jest przemianowanie parametrów formalnych tak, aby każdy z nich miał unikalną nazwę.

## Wymagania techniczne

W ramach tego zadania tworzymy pakiet cabal o nazwie `identyfikator-zadanie2`
(identyfikator ze students, np. mb128410)
który powinien budować się przy użyciu narzędzi ze students (GHC 9.0.2, cabal 3.4);
mile widziane, zeby budowal się też z nowszymi wersjami GHC (np 9.4.8, 9.8.2)

Pakiet powinien dostarczać co najmniej plik wykonywalny `zadanie2`, n.p.

```
$ cabal run -- zadanie2
Usage: zadanie2 [--help] [file]
  --help  - display this message
  file    - file with program to reduce
```

Oddajemy pojedynczy plik `.tar.gz` stworzony poprzez `cabal sdist`

```
$ cabal sdist
Wrote tarball sdist to
/home/ben/Zajecia/pf/code/zadanie2/dist-newstyle/sdist/zadanie2-0.1.0.0.tar.gz
```

Proszę sprawdzić, że pakiet otrzymany z rozpakowania tego pliku się buduje.

Zadanie MUSI być rozwiązane samodzielnie.
Wszelkie zapożyczenia muszą być wyraźnie zaznaczone z podaniem źródła.
Dotyczy to także kodu wygenerowanego/zasugerowanego przez narządzia AI i pokrewne
(VS Code, Copilot, ChatGPT, Claude itp.)

Ponadto student musi umieć objaśnić sposób działania każdego fragmentu oddanego kodu
(wyjaśnienia typu "Znalazłem na Stackoverflow/Copilot mi podpowiedział i działa ale nie wiem jak" itp => 0p).
