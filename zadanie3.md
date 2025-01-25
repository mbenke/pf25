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
Do reprezentacji kontekstu i nawigacji wewnątrz wyrażen mozna uzyć techniki "zipper".

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