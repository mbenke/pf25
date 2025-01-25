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

Korzystamy tylko z małego podzbioru skladni Haskella, nie ma typów.
Dzieki temu nie musimy pisać własnego parsera, ale możemy skorzystać z biblioteki `haskell-src`. Ta biblioteka buduje drzewo struktury dla pełnej składni Haskella, musimy je przekształcić do naszej uproszczonej składni

``` haskell
data Def = Def Name [Name] Expr
data Expr = Var Name | Expr :$ Expr | IntLit Integer
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
main = six s 0
------------------------------------------------------------
six s 0
mul two tre s 0
o two tre s 0
two (tre s) 0
suc one (tre s) 0
tre s (one (tre s) 0)
suc two s (one (tre s) 0)
s (two s (one (tre s) 0))
s (suc one s (one (tre s) 0))
s (s (one s (one (tre s) 0)))
s (s (s (one (tre s) 0)))
s (s (s (tre s 0)))
s (s (s (suc two s 0)))
s (s (s (s (two s 0))))
s (s (s (s (suc one s 0))))
s (s (s (s (s (one s 0)))))
s (s (s (s (s (s 0)))))
```

albo 


``` haskell
s x y z = x z (y z)
k x y = x
i = s k k
om x = x x
omega = om om
main = k i omega 0
------------------------------------------------------------
k i omega 0
i 0
s k k 0
k 0 (k 0)
0
```

Dla zrealizowania kroku redukcji potrzebna będzie funkcja, która dokona podstawienia wyrażeń stanowiących parametry faktyczne w miejsce parametrów formalnych w ciele kombinatora, np.

``` haskell
subst :: (Name, Expr) -> Expr -> Expr
```
**Uwaga o zmiennych:** w trakcie redukcji trzeba uważać, eby nie pomieszać zmiennych, inaczej moze nam się przydarzyć błędna redukcja:

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