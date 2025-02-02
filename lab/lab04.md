# Liczby naturalne

## Arytmetyka Peano

``` haskell
data Nat = Zero | S Nat deriving Show

mul :: Nat -> Nat -> Nat
mul m Zero = Zero
mul m (S n) = add (mul m n) n
```

a. Zdefiniuj potęgowanie

b. Popatrzmy na funkcję 

``` haskell
eqNat :: Nat -> (Nat -> Bool)
eqNat Zero = \n -> case n of
                     Zero -> True
                     _ -> False
eqNat (S m) = \n -> case n of
                     Zero -> False
                     (S n') -> eqNat m n'
```
zapisz eqNat nie używając lambdy ani case

## Wnioskowanie

Udowodnij:
- `(S Zero)` jest elementem neutralnym mnożenia
- łączność dodawania/mnożenia
- rozdzielność mnożenia względem dodawania: `k*(m+n) = k*m + k*n`

Udowodnij przemienność dodawania przy pomocy fuzji

## Konstrukcja programów

Bazując na przykładzie z wykładu dla odejmowania napisz specyfikację dla dzielenia i wyprowadź program spełniający tę specyfikację.