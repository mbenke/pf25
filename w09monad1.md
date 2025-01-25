---
title: Programowanie Funkcyjne
subtitle: Monady
author:  Marcin Benke
date: Wykład 9/2025
---

# Monady

Na poprzednich wykładach widzieliśmy już obliczenia z efektami na przykład **Maybe** przy **Applicative** i **Traversable**, czy **IO**.

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

**Maybe** może zostac użyte dla obliczeń zawodnych

``` haskell
instance Monad Maybe where
  (Just a) >= k = k a
  Nothing >= _ = Nothing

instance Monad [] where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= k = concatMap k xs
```

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

### Komunikaty o błedach

Podobnie jak wcześniej, mozemy też użyc **Either**:

``` haskell
instance Monad (Either e) where
    Left e  >>= _ = Left e
    Right x >>= k = k x
```

Abstrakcyjny protokół obsługi błędów (warunek `m -> e` oznacza, że typ `m` jednoznacznie wyznacza typ błędu `e`; na razie nieistotne)

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
getVar n = reader (Map.lookup n) >>= \case ...`
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

ap :: Monad m => m (t -> b) -> m t -> m b
ap mf mx = do { f <- mf; x <- mx; pure (f x) }

instance Functor (State s) where
    fmap g (State p) = State (first g . p)

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
--    This says that @modify (+1)@ acts over any
--    Monad that is a member of the @MonadState@ class,
--    with an @Int@ state.
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

Haskell Programming s.875; NB deprecated next

``` haskell
{- cabal:
    build-depends: base, mtl, random
-}
import System.Random

roll :: StdGen -> (Int, StdGen)
roll = uniformR (1, 6::Int)rollD6 :: Int

pureGen = mkStdGen 1
rollD6 = fst $ roll pureGen

-- >>> pureGen
-- StdGen {unStdGen = SMGen 12994781566227106604 10451216379200822465}
-- roll pureGen
-- (6,StdGen {unStdGen = SMGen 4999253871718377453 10451216379200822465})
-- rollD6
-- 6
```

na tej kostce zawsze wypada 6

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

### Przyomnienie: klasa **Alternative**

Klasa **Alternative** opisuje obiczenia któr moga dać wiele (w tym zero!) wyników:

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
  m >>= f  = concatMap f m
  fail s = []
```
NB `[]` w nagłówku definicji oznacza konstruktor typu: `[] a = [a]`


### Instancja **MonadPlus**

Instancja **MonadPlus** dodaje operacje braku wyniku i ``niedeterministycznego'' wyboru:

``` haskell
instance MonadPlus [] where
  mzero = []
  mplus = (++)
```

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


## Parsery - analiza tekstów

Pierwsze przybliżenie: parser dla typu **a** dostaje listę znaków i daje rozpoznany w nim element **a** (tak jak klasa **Read**)

``` haskell
read :: Read a => String -> a
```
Co z błędami? Niejednoznacznośc rozkładu? Jak łączyć parsery?

Pomysł 1:
``` haskell
readMaybe :: Read a => String -> Maybe a  -- Text.Read
```

Pomysł 2:
``` haskell
readMany :: Read a => String -> [a]       -- fikcja
```

Pomysł 3:

``` haskell
type ReadS a = String -> [(a, String)]
```

Parser ``zużywa'' pewien prefiks wejścia aby odczytać **a**, oddaje niewykorzystaną resztę wejścia (którą może odczytać kolejny parser)

Wynikiem działania jest lista możliwych odczytań (być może pusta).

## Parsery: stan + lista


Zauważmy, że

``` haskell
String -> (a, String)
```

reprezentuje obliczenia ze stanem typu String