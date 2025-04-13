---
title: Programowanie Funkcyjne
subtitle: Monady
author:  Marcin Benke
date: Wykład 8/2025
---

# Monady

Na poprzednich wykładach widzieliśmy już obliczenia z efektami na przykład **Maybe** przy **Applicative** i **IO**.

poznalismy też metodę

``` haskell
pure :: Applicative f => a -> f a
```
i operator sekwencjonowania

```haskell
(>>=) :: IO a -> (a -> IO b) -> IO b
```

## Klasa Monad
Sekwencjonowanie odnosi się do wszelkich obliczeń z efektami, co jest ujęte w klasie **Monad**:

``` haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=) #-}
```

Tylko pierwsza metoda jest istotna, domyślnie mamy

``` haskell
return = pure
a >> b = a >>= const b
```

## Dlaczego nie wystarczy Applicative?

Proste sekwencjonowanie:

```haskell
(*>) :: IO a -> IO b -> IO b

main = write "Hello," *> writeln "world!"
```

...nie wystarczy jeżeli chcemy użyć nie tylko efektu obliczenia, ale także jego wyniku, np. dla funkcji

```haskell
getLine :: IO String
```

mamy problem z jej wynikiem:


``` haskell
getLine *> writeln ?
```

Operator `*>` ignoruje wynik pierwszego argumentu i nie wiemy co wypisać.

Użycie `<*>` też nie pomoże (sprawdź!) - mamy dostep do wyniku pierwszego obliczenia, ale nie może on wpłynąć na efekt drugiego obliczenia

### Proste przykłady

**Maybe** może zostać użyte dla obliczeń zawodnych

``` haskell
instance Monad Maybe where
  (Just a) >= k = k a
  Nothing >= _ = Nothing

  -- return = pure = Just
```

- `Just x` to sukces z wynikiem `x`
- `Nothing` to porażka

### Wyrażenia z dzieleniem

``` haskell
data Exp = Val Int | Div Exp Exp

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (div x y)

eval :: Exp -> Maybe Int
eval (Val n) = pure n
eval (Div x y) = case (eval x) of
  Nothing -> Nothing
  Just n -> case eval y of
    Nothing -> Nothing
    Just m -> safediv n m
```
mozna krócej:

``` haskell
eval (Div x y) = eval x >>= \n ->
		 eval y >>= \m ->
		 safediv n m
```

albo używając **do**:

``` haskell
eval (Div x y) = do
  n <- eval x
  m <- eval y
  safediv n m
```

... ale uwaga: nie wystarczy tu `<*>` z **Applicative** (sprawdź!).

### Komunikaty o błędach

Podobnie jak wcześniej, mozemy też użyc **Either**:

``` haskell
instance Monad (Either e) where
    Left e  >>= _ = Left e
    Right x >>= k = k x
```

Abstrakcyjny protokół obsługi błędów<br/>
(warunek `m -> e` oznacza, że typ `m` jednoznacznie wyznacza typ błędu `e`; na razie nieistotne)

``` haskell
class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance MonadError () Maybe
instance MonadError e (Either e)

ghci> throwError () :: Maybe a
Nothing

ghci> throwError "Boo!" :: Either String a
Left "Boo!"

ghci> throwError "Boo!" `catchError` const (pure 42) :: Either String Int
Right 42
```

### Przypomnienie - funkcje jako Applicative

Konstruktor typu funkcji `(->)` jest dwuargumentowy<br/>
Ustalając dziedzinę możemy uzyskac jednoargumentowy: `((->) r)`

``` haskell
instance Applicative ((->) r) where
  pure x y = x
  (<*>) f g z = f z(g z)
```

Na wykładzie o **Applicative** było to trochę *curiosum*:

``` haskell
palindrome = (==) <*> reverse
```

Tu zobaczymy bardziej praktyczne zastosowanie instancji **Monad** dla funkcji

## Środowisko

Mamy grupę funkcji, które korzystają ze wspólnego środowiska, np.

``` haskell
data Exp = EInt Int | EVar String | EAdd Exp Exp | ELet String Exp Exp deriving(Eq, Show)

type Env = Map.Map String Int

eval1 :: Exp -> Env -> Int
eval1 (EInt i) env = i
eval1 (EVar n) env = getVar n env
eval1 (EAdd e1 e2) env = do
  let v1 = eval1 e1 env
  let v2 = eval1 e2 env
  v1 + v2

eval1 (ELet n e1 e2) env = do
  let v1 = eval1 e1 env
  eval1 e2 (Map.insert n v1 env)

getVar :: String -> Env -> Int
getVar n env = case Map.lookup n env of
  Nothing -> error("unknown: "++n)
  Just v -> v
```

i chcemy uniknąć jawnego przekazywania środowiska

### Czytelnik

``` haskell
data Exp = EInt Int | EVar String | EAdd Exp Exp | ELet String Exp Exp deriving(Eq, Show)

type Env = Map.Map String Int

eval2 :: Exp -> Env -> Int
eval2 (EInt i) = pure i
eval2 (EVar n) = getVar n
eval2 (EAdd e1 e2) = do -- (+) <$> eval e1 <*> eval e2
  v1 <- eval2 e1
  v2 <- eval2 e2
  pure (v1 + v2)

eval2 (ELet n e1 e2) = do
  v1 <- eval2 e1
  local (Map.insert n v1) (eval2 e2)

getVar :: String -> Env -> Int
getVar n env = case Map.lookup n env of
  Nothing -> error("unknown: "++n)
  Just v -> v
```

### MonadReader

``` haskell
-- ((->) r) a = r -> a
instance Monad ((->) r) where
   f >>= k = \r -> k(f r)
class Monad m => MonadReader r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a

instance MonadReader r ((->) r)
```

* `ask` pyta o środowisko
* `local` wykonuje obliczenie w **lokalnie** zmienionym środowisku
* `reader` używamy, gdy interesuje nas pewien aspekt środowiska, np

``` haskell
getVar n = do
  mv <- reader (Map.lookup n)
  case mv of ...
```

albo krócej z rozszerzeniem `LambdaCase`:

 ``` haskell
getVar n = reader (Map.lookup n) >>= \case ...
```

### Ćwiczenia

**Ćwiczenie:** zdefiniuj

``` haskell
newtype Arr a b = Arr { apply ::  a -> b }

instance Functor (Arr r) where
instance Applicative (Arr r) where
instance Monad (Arr r) where
instance MonadReader r (Arr r) where
```

Spróbuj wyrazić `(>>=)` przy użyciu `(<*>)`.

# Stan

Najbardziej powszechnym (oprócz IO) rodzajem efektu jest globalny stan.

W Haskellu nie ma niejawnych modyfikacji stanu, ale możemy modelować stan przy pomocy funkcji typu

``` haskell
type Oblicz wynik = (Stan -> (wynik, Stan))

-- pure :: a -> Oblicz a
pure x s = (x,s)

-- (>>=) :: Oblicz a ->(a->Oblicz b)->Oblicz b
(o >>= k) s = let (x,s') = o s in (k x) s'
```
Typy pozwalają kontrolować gdzie i jaki stan może się zmieniać.

## newtype State

`Oblicz` nie może być instancją klas, ale możemy użyć `newtype`:

``` haskell
newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap g p = State $ \s -> let (a, s') = runState p s in (g a, s')

instance Applicative (State s) where
    pure x = State $ (\s -> (x,s))
    -- (<*>) :: State (a->b) -> State a -> State b
    mf <*> mx = State $ \s -> let
                    (f, s1) = runState mf s
                    (x, s2) = runState mx s1
                in (f x, s2)

instance Monad (State s) where
  p >>= k = State $ \s -> let (a, s') = runState p s in runState (k a) s'
```
(to samo co dla `Oblicz` plus pakowanie/odpakowywanie)

## Krótsze definicje

``` haskell
first :: (a->b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)
-- first = pamf

ap :: Monad m => m (t -> b) -> m t -> m b
ap mf mx = do { f <- mf; x <- mx; pure (f x) }

instance Functor (State s) where
    fmap g (State p) = State (first g . p)
    -- fmap g (State p) = State (pamf g . p)

instance Applicative (State s) where
    pure x = State (x,)  -- \s -> (x,s)
    (<*>) = ap
```

## Protokół **MonadState**

``` haskell
class Monad m => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a
    {-# MINIMAL state | get, put #-}

-- | Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.
--
-- >      Main> :t modify ((+1) :: Int -> Int)
-- >      modify (...) :: (MonadState Int a) => a ()
--
--    This says that modify (+1) acts over any
--    Monad that is a member of the MonadState class,
--    with an Int state.
modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))
```

###  MonadState --- przykład PSM
  pikojęzyk imperatywny:

  - jedna zmienna globalna
  - program jest listą wyrażeń  (z efektami ubocznymi!)
  - wykonanie wyrażenia ustawia (jedyną) zmienną na jego wartość


``` haskell
type Prog = [Stmt]
type Stmt = Exp
data Exp = EInt Int | EAdd Exp Exp | Var

instance Num Exp where ...

prog1 :: Prog
prog1 = [41, Var+1 ]

-- > run prog1
-- 42

prog2 :: Prog
prog2 = [1, 2, Var + Var]

-- > run prog2
4
```

###  MonadState --- przykład PSM

``` haskell
type PSM a = State Int a         -- PicoStateMonad

exec :: Stmt -> PSM ()
exec e = eval e >>= put

eval :: Exp -> PSM Int
eval (EInt n) = return n
eval Var = get
eval (EAdd e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1+v2)
-- eval (EAdd e1 e2) = (+) <$> eval e1 <*> eval e2

run :: Prog -> Int
run p = execState (mapM_ exec p) 0


-- mapM  :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

-- runState  :: State s a -> s -> (a, s)
-- execState :: State s a -> s -> s
-- evalState :: State s a -> s -> a
```

### MonadState - liczby pseudolosowe

<!-- Haskell Programming s.875; NB deprecated next -->

``` haskell
{- cabal:
    build-depends: base, mtl, random
-}
import System.Random

roll :: StdGen -> (Int, StdGen)
roll = uniformR (1, 6::Int)

pureGen = mkStdGen 1

rollD6 :: Int
rollD6 = fst $ roll pureGen

-- >>> pureGen
-- StdGen {unStdGen = SMGen 12994781566227106604 10451216379200822465}
-- roll pureGen
-- (6,StdGen {unStdGen = SMGen 4999253871718377453 10451216379200822465})
-- rollD6
-- 6
```

na tej kostce zawsze wypada 6


(oczywiście można uzyć IO, ale wtedy się już od niego nie uwolnimy)

```
ghci> randomRIO (1,6)
1
ghci> randomRIO (1,6)
4
ghci> randomRIO (1,6)
2
```

### Rzuć 3d6

Jak rzucić trzema kostkami?

``` haskell
roll3D6 :: (Int, Int, Int)
roll3D6 = do
  let g0 = pureGen
  let (r1, g1) = roll g0
  let (r2, g2) = roll g1
  let (r3, g3) = roll g2
  (r1, r2, r3)
```

Zamiast jawnie przekazywać generator, możęmy użyć `State`:

``` haskell
rollM :: State StdGen Int
rollM = state roll        -- state :: (StdGen -> (a, StdGen)) -> State StdGen a;  roll :: StdGen -> (Int, StdGen)

roll3D6M' :: State StdGen (Int, Int, Int)
roll3D6M' = do { r1 <- rollM; r2 <- rollM; r3 <- rollM; return (r1, r2, r3) }
```

albo krócej z użyciem **Applicative**:
``` haskell
roll3D6M = (,,) <$> rollM <*> rollM <*> rollM
-- >>> evalState roll3D6M pureGen
-- (6,4,3)
```

### N kostek

``` haskell
replicate :: Int -> a -> [a]
-- >>> replicate 10 7
-- [7,7,7,7,7,7,7,7,7,7]

import Control.Monad
replicateM :: Applicative m => Int -> m a -> m [a]
sequence :: Monad m => [m a] -> m [a]

rollND6 :: Int -> State StdGen [Int]
rollND6 n = replicateM n rollM
-- rollND6 = sequence (replicate n rollM)

-- >>> evalState (rollND6 10) pureGen
-- [6,4,3,5,4,4,5,3,3,4]
```



### Strumień liczb pseudolosowych

Inne podejście: dzięki leniwości możemy łatwo stworzyć strumień liczb pseudolosowych

``` haskell
import Data.List
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

rolls :: StdGen -> [Int]
rolls = unfoldr (Just . roll)

ghci> take 10 (rolls pureGen)
[6,4,3,5,4,4,5,3,3,4]
```

### Nieskończenie wiele kostek

``` haskell
repeat :: a -> [a]
ghci> take 10 (repeat 7)
[7,7,7,7,7,7,7,7,7,7]

sequence :: Monad m => [m a] -> m [a]
streamM :: State StdGen [Int]
streamM = sequence (repeat rollM) -- repeat rollM :: [State StdGen Int]

stream = evalState streamM pureGen

ghci> take 10 stream
[6,4,3,5,4,4,5,3,3,4]
```

## IO --- monady zmieniają świat
Monadę **IO** możemy traktować jako bardzo szczególny przypadek monady **State**:
``` haskell
newtype IO = IO (RealWorld# -> (RealWorld#, a))
```
(dokładniej nie tyle IO jest szczególna, co jej stan --- `RealWorld#` odpowiada stanowi świata)

Typ `RealWorld#` jest "magiczny" (na co wskazuje #). jest on traktowany bardzo szczególnie przez system wykonawczy.

### Globalny stan --- IORef

``` haskell
import Data.IORef
  ( newIORef    -- :: a -> IO (IORef a)
  , readIORef   -- :: IORef a -> IO a
  , writeIORef  -- :: IORef a -> a -> IO ()
  , modifyIORef -- :: IORef a -> (a -> a) -> IO ()
  )

main = do
  ref <- newIORef 0
  writeIORef ref 41
  modifyIORef ref (+1)
  val <- readIORef ref
  print val
```

Oczywiście należy używać z umiarem.

### Przypomnienie: klasa **Alternative**

Klasa **Alternative** opisuje obiczenia które moga dać wiele (w tym zero!) wyników:

``` haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]         -- v+
  some v = (:) <$> v <*> many v
  many :: f a -> f [a]         -- v*
  many v = some v <|> pure []

instance Alternative [] where
  empty = []
  (<|>) = (++)
```

## MonadPlus

Klasa **MonadPlus** daje dla monad operacje analogiczne do **Alternative**

``` haskell
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

  -- Laws:
  --  mzero >>= f  =  mzero
  --  v >> mzero  =  mzero

guard :: MonadPlus m => Bool -> m ()
guard True  = return ()
guard False = mzero


-- Bez MonadPlus:
when, unless :: Monad m => Bool -> m () -> m ()
when True  s = s
when False s = return ()
```

### Monada list

Listę możemy traktować jako monadę reprezentującą obliczenia niedeterministyczne (wiele możliwych wyników, relacje)

``` haskell
instance Monad [] where
  return a = [a]
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  m >>= f  = concatMap f m
```
NB `[]` w nagłówku definicji oznacza konstruktor typu: `[] a = [a]`

Uwaga: skoro `(>>)` ignoruje wynik pierwszego obliczenia, kuszace może być zdefiniowanie `xs >> ys = ys`, ale tak nie można:<br/>
 trzeba zachować **efekt** pierwszego obliczenia - w tym wypadku **liczbę** wyników:

``` haskell
ghci> "a" >> "b"
"b"
ghci> "xy" >> "b"
"bb"
ghci> "" >> "b"
""
```


### Instancja **MonadPlus**

Instancja **MonadPlus** dodaje operacje braku wyniku i ``niedeterministycznego'' wyboru:

``` haskell
instance MonadPlus [] where
  mzero = []
  mplus = (++)
```

Można rozważać inne implementacje `mplus` (ćwiczenie)

### Przykład --- odcinki początkowe

Funkcja dajaca listę wszystkich prefiksów danej listy:

``` haskell
inits :: [a] -> [[a]]
inits [] = pure []   -- tylko [] jest prefiksem []
inits (x:xs) = pure [] `mplus` do
   ys <- inits xs   -- gdy ys jest prefiksem xs
   return (x:ys)    -- (x:ys) jest prefiksem (x:xs)
```

ale zwykle prościej użyć równoważnej wycinanki:

``` haskell
inits [] = [[]]
inits (x:xs) = []:[(x:ys) | ys <- inits xs]
```
### Wycinanki vs do

``` haskell
triads :: Int -> [(Int, Int, Int)]
triads n = [(x,y,z) |
              (x,y,z) <- triples n,
               z^2 == x^2 + y^2
           ]

triads2 :: Int -> [(Int, Int, Int)]
triads2 n = do
             (x,y,z) <- triples n
             guard $ z^2 == x^2 + y^2
             return (x,y,z)

guard :: MonadPlus m => Bool -> m()
guard b = if b then return () else mzero
```

<!-- ### Guards! Guards! -->

# Zadanie 3
W tym zadaniu rozszerzamy język z Zadania 2 o konstruktory wartości i dopasowanie wzorca.
Tym niemniej nadal nie wprowadzamy żadnej kontroli typów - każda wartość może być zastosowana do dowolnych argumentów.

**Termin:** 1 czerwca, godz. 20

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

Tak jak w Haskellu, nazwy z wielkiej litery oznaczają konstruktory.

## Składnia

Korzystamy tylko z małego podzbioru skladni Haskella, nie ma typów - każda wartość może być zastosowana do dowolnych argumentów.

Dzieki temu nie musimy pisać własnego parsera, ale możemy skorzystać z biblioteki `haskell-src`.
Wynik `parseModule`  musimy  przekształcić do naszej uproszczonej składni

``` haskell
type Name = String
data Def = Def { defMatches :: [Match] }
data Match = Match { matchName :: Name
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

Stwierdzenie, czy argument pasuje do wzorca może wymagać wykonania jednego lub więcej kroków redukcji tego argumentu. W językach leniwych istotnie zwykle to dopasowanie wzorca wymusza redukcje. Tym  niemniej argument jest redukowany "tylko tyle ile potrzeba", czyli aż do momentu rozstrzygnięcia czy pasuje do wzorca.


``` haskell
one = S Z
two = S one
add Z     n = n
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
add Z     n = n
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

Jednym ze sposobów rozwiązania tego problemu jest precyzyjne odnotowywanie wszystkich kroków wraz z kontekstem w jaki się odbywają.

Można wykorzystać do tego celu monadę stanu, która będzie przechowywać historię redukcji (wraz z ich kontekstami).<br/>
Stan może ponadto zawierać także listę definicji i ilość pozostałego "paliwa" (kroków, po których uznamy, że obliczenie jest zapętlone lub za długie do wyświetlenia), oraz inne informacje które uznamy za potrzebne.

Do reprezentacji kontekstu i nawigacji wewnątrz wyrażen można uzyć techniki "zipper"<br/>
(ewentualnie po prostu ścieżki będącej listą elementów "lewo-prawo", ale to słabsze rozwiązanie).

``` haskell
two = S (S Z)
add Z     n = n
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


## Wymagania techniczne

Analogicznie jak w poprzednio, w tym zadaniu tworzymy pakiet cabal o nazwie `identyfikator-zadanie3`
(identyfikator ze students, np. mb128410)
który powinien budować się przy użyciu narzędzi ze students (GHC 9.0.2, cabal 3.4);
mile widziane, zeby budowal się też z nowszymi wersjami GHC (np 9.4.8, 9.8.2);
w tym celu warto w pliku cabal zamiast frazy
```
base^>=4.15.1.0
```

użyć
```
base>=4.15.1.0
```

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

# Pytania?
