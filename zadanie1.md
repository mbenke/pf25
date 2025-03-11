# Zadanie 1

Tematy: kombinatory, typy algebraiczne, dopasowanie wzorca, rekurencja, proste I/O.

Przedmiotem zadania jest stworzenie funkcji wizualizującej proces redukcji wyrażeń kombinatorowych.

## Wyrażenia

Rozważamy wyrażenia złozone z predefiniowanych kombinatorów, zmiennych  i aplikacji.


``` haskell
data Expr = S | K | I | B 
          | Expr :$ Expr 
          | X | Z | V Int  
          deriving (Show, Read)
```

### Kombinatory
Podstawowe kombinatory to S i K, których zachowanie można opisać:

```
S x y z = x z (y z)
K x y = x
```

przez takie definicje rozumiemy "dla dowolnych wyrażeń x,y,z, wyrażenie `S x y z` redukuje się do `x z (y z)`. Takie wyrażenie (kombinator z odpowiednią liczbą argumentów) nazywamy *redeksem*. Kombinator z niewystarczjaącą liczbą argumentów (np. `S K S`) nie jest redeksem. Proces redukcji kończy się, gdy w wyrażeniu nie ma redeksów - mówimy, ze wyrazenie jest w *postaci normalnej*.

Na przykład

```
S K K x = K x (K x) = x
```
czyli wyrażenie `S K K x` redukuje się (w dwóch krokach) do x.

Podobnie w Haskellu używamy znaku równości, ale proces redukcji jest skierowany od lewej do prawej

Przy użyciu kombinatorów S i K zasadniczo da się zdefiniować wszystkie inne, ale dla ułatwienia używamy jeszcze dwóch pomocniczych:

```
I x = x
B x y z = x (y z)
```

### Aplikacja

``` haskell 
infixl 9 :$
```

konstruktor `:$` reprezentuje aplikację; wiąże w lewo.
Na przykład `(S K) K` możemy zapisać jako

```
S :$ K :$ K
```

### Zmienne

Uniwersalną formą zmiennej jest `v_n` reprezentowane w naszym typie przez `(V n)`. Dla ułatwienia mamy też zmienne X i Z (Y pominięte dla uniknięcia nieporozumień).

### Przykłady wyrażeń

```
test1 = S :$ K :$ K :$ X
twoB = S :$B :$ I
threeB = S :$ B :$ (S :$B :$ I)
test3 = threeB :$ X :$ Z
omega = ((S :$ I) :$ I) :$ ((S :$ I) :$ I)
kio = K :$ I :$ omega
add = (B :$ S) :$ (B :$ B)
```

## Drukowanie

Wyrażenia postaci `((S :$ I) :$ I) :$ ((S :$ I) :$ I)` są mało czytelne.

Napisz funkcję `prettyExpr` która przedstawi swój argument bez zbędnych nawiasów,ze zmiennymi w czytelnej formie np.

```
ghci> prettyExpr omega
"S I I (S I I)"

ghci> prettyExpr (K :$ X :$ (V 7))
"K x v7"
```

## Redukcja 

Celem zadania jest stworzenie funkcji do wizualizacji procesu redukcji wyrażeń, na przykład

```
ghci> printPath test1
S K K x
K x (K x)
x

ghci> printPath test3
S B (S B I) x z
B x (S B I x) z
x (S B I x z)
x (B x (I x) z)
x (x (I x z))
x (x (x z))

ghci> printPath (add :$ twoB :$ threeB :$ X :$ Z)
B S (B B) (S B I) (S B (S B I)) x z
S (B B (S B I)) (S B (S B I)) x z
B B (S B I) x (S B (S B I) x) z
B (S B I x) (S B (S B I) x) z
S B I x (S B (S B I) x z)
B x (I x) (S B (S B I) x z)
x (I x (S B (S B I) x z))
x (x (S B (S B I) x z))
x (x (B x (S B I x) z))
x (x (x (S B I x z)))
x (x (x (B x (I x) z)))
x (x (x (x (I x z))))
x (x (x (x (x z))))

ghci> printPath kio
K I (S I I (S I I))
I
```

Ostatni przykład zasługuje na szczególną uwagę: musimy pamiętać, żeby redukować zawsze najbardziej zewnętrzny redeks - próba redukcji `S I I (S I I)` zaprowadzi nas na nieskończoną ścieżke.

Z drugiej strony, jeśli dwie strategie redukcji prowadzą do celu (postaci normalnej),
to jest to ten sam cel (twierdzenie Churcha-Rossera).

### Wskazówki

Warto najpierw stworzyć funkcję `rstep` obliczającą jeden krok redukcji,
potem `rpath :: Expr -> [Expr]`, która da listę kroków redukcji danego wyrażenia, wreszcie `printPath`, która wypisze pierwsze 30 kroków tej ścieżki.

```
ghci> rpath test1
[((S :$ K) :$ K) :$ X, (K :$ X) :$ (K :$ X), X]

ghci> printPath omega
S I I (S I I)
I (S I I) (I (S I I))
S I I (I (S I I))
...
I (I (I (I (I (I (S I I)))))) (I (I (I (I (I (I (I (S I I))))))))
```

Ostatni przykład pokazuje, że strategia normalna, chociaż najlepsza w znajdowaniu rozwiązań nie jest optymalna wydajnościowo. Można ją wzbogacić o usuwanie redeksów "skracających", jak `I x = x` przed zastosowaniem "wydłużającego" S, wtedy otrzymamy

```
ghci> printPath' omega
S I I (S I I)
I (S I I) (I (S I I))
S I I (I (S I I))
...
I (S I I) (I (S I I))
```

## Wymagania techniczne

Należy oddać jeden plik: `<uid>.hs` gdzie uid to identyfikator ze students. Na początku pliku musi się znajdować komentarz z danymi autora.

W tym zadaniu korzystamy wyłącznie ze standardu Haskell2010, bez żadnych rozszerzeń,
w szczególności oddawany plik musi działać na `students`.

Nie można importować żadnych modułów (poza domyślnie importowanym Prelude). 

Zadanie powinno być rozwiązane samodzielnie. Wprawdzie niektóre LLM potrafią je rozwiązać, ale to zadanie jest wprawką do kolejnych, których LLM już mogą nie rozwiązać.

