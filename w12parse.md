---
title: Programowanie Funkcyjne
subtitle: Kombinatory parsujące
author:  Marcin Benke
date: Wykład 12/2025
---


# Wykład 10 - parsery, łączenie monad

**Plan**

* analiza tekstu, parsery
* kombinatory parsujące
* typy parserów
* biblioteki
* łączenie monad,  **MonadTrans**, **MonadIO**


## Parsery - analiza tekstu

Parser analizuje tekst zgodnie z zadaną definicją języka (często gramatyką)

Jeśli się uda, buduje wartość odpowiadającą tekstowi wejściowemu (często drzewo, tzw AST - Abstract Syntax Tree);<br />
w przypadku porażki pożądany jest pomocny komunikat o błędzie.

W językach imperatywnych zwykle uzywa się automatu ze stosem.

W językach funkcyjnych to też możliwe, ale jest też inny sposób: tzw. *kombinatory parserów*:<br />
parsery są funkcjami, które mozemy ze sobą łączyć przy użyciu kombinatorów

**Przykład:**
``` haskell
digits :: Parser String
digits = some digit

nat, int :: Parser Int
nat = read <$> digits

int = nat <|> negative
negative = do char '-';
              n <- nat
              return (-n)

-- negative = char '-' *> negate <$> nat
```

### Typ parsera

Pierwsze przybliżenie: parser dla typu **a** dostaje napis i daje rozpoznany w nim element **a** (tak jak klasa **Read**)

``` haskell
read :: Read a => String -> a
```
Co z błędami? Niejednoznaczność rozkładu? Jak łączyć parsery?

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

Parser ``konsumuje'' pewien prefiks wejścia aby odczytać **a**, oddaje niewykorzystaną resztę wejścia (którą może odczytać kolejny parser)

Wynikiem działania jest lista możliwych odczytań (być może pusta).

Phil Wadler: *How to replace failure by a list of successes*

## Parsery: stan + lista


Zauważmy, że

``` haskell
String -> (a, String)
```

reprezentuje obliczenia ze stanem typu String

z kolei

``` haskell
newtype Parser a = P { runP :: String -> [(a, String)] }
```

łączy efekty stanu i wielu wyników. Mozemy łatwo stworzyć instancję **Monad**:

``` haskell
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P p) = P $ \s -> [(f a, s') | (a, s') <- p s]

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (P p) >>= f = P $ \s -> concat [runP (f v) out | (v, out) <- p s]

instance Applicative Parser where
  pure a = P $ \s -> [(a, s)]

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = do { f <- pf; x <- px; pure (f x) }  -- Control.Monad.ap
```

## Alternatywa

Często musimy rozponać "jedno z ..." np

```

Expr ::= int | Bool | id ( CommaSepExprs ) | id
Bool ::= "true" | "false"
CommaSepExprs  ::= Expr | Expr "," CommaSepExprs
```

Wtedy przyda się nam **Alternative** (lub **MonadPlus**):

``` haskell
instance Alternative Parser where
  empty = P $ const []
  (P p1) <|> (P p2) = P $ \s -> p1 s ++ p2 s

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

pBool = string "true" *> EBool True
    <|> string "false" *> EBool False
```

## Inne typy parserów

Zaletą parserów typu "stan + lista" jest wzięcie pod uwagę wszystkich możliwości rozbioru.

Wadą jest wynikająca z tego nieefektywność oraz brak komunikatów o błędach.

Dlatego częściej stosuje się parsery typu

``` haskell
String -> Maybe (a, String)
String -> Either ErrorMessage (a, String)
```

zaletą pierwszego typu jest prostota, drugiego - możliwość obsługi błędów.

Biblioteki używają często bardziej złożonych typów.<br />
W naszych przykładach wejście jest typu **String**, ale może być **Text**, **ByteString** lub inne.

### Lewicowa alternatywa
``` haskell
String -> Maybe (a, String)
String -> Either ErrorMessage (a, String)
```

Alternatywa jest "lewicowa": w `p1 <|> p2` jeśli `p1` odniesie sukces, `p2` nie jest brane pod uwagę<br/>
(spróbuj napisać `instance Alternative Maybe` a przekonasz się dlaczego)

czasem idzie to dalej - `p2` wchodzi do gry tylko gdy `p1` zawiedzie nie konsumując wejścia (nie wracamy do raz przeczytanych znaków, co dajew liniową złożoność).

W razie potrzeby powrotu, można użyć kombinatora `try`, zwykle w wersji `try p1 <|> p2`.

### Przykład

``` haskell
-- Expr ::= int | "true" | "false" | id ( CommaSepExprs ) | id
data Expr = EInt Integer | EBool Bool | ECall Name [Expr] | EVar Name

pExpr :: Parser Expr
pExpr = choice          -- choice [e1,...,en] = e1 <|> ... <|> en
    [ EWord <$> integer
    , EBool True  <$ pKeyword "true"    -- (<$) :: Functor f => a -> f b -> f a
    , EBool False <$ pKeyword "false"
    , try (ECall <$> identifier <*> parens (commaSep pExpr))
    , EVar <$> (identifier  <* notFollowedBy (symbol "("))
                        -- (<*) :: Applicative f => f a -> f b -> f a
    ]

-- "true" jest słowem kluczowym, ale "trueCrime" już nie
pKeyword :: String -> Parser String
pKeyword w = try $ lexeme (string w <* notFollowedBy identChar)

-- negacja: notFollowedByP odnosi sukces gdy p zawodzi (przybliżenie)
notFollowedBy :: Show a => Parser a -> Parser ()
notFollowedBy p = do { c <- try p; unexpected (show c) } <|> return ()
```

## Łączenie monad

Wspomniane wcześniej typy parserów

``` haskell
type ListParser a  = String -> [(a, String)]
type MaybeParser a = String -> Maybe (a, String)
type ErrorParser a = String -> Either ErrMsg (a, String)
```

łączą efekt stanu z inną monadą; możemy uogólnić ten schemat:


``` haskell
--      State  s   a ~ State  { runState  :: s ->   (a, s) }
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

type ListParser a  = (StateT String []) a
type MaybeParser a = (StateT String Maybe) a
type ErrorParser a = (StateT String (Either ErrMsg)) a
```

### Instancje dla StateT

Instancje odpowiednich klas wystarczy napisać raz dla **StateT**:

``` haskell
instance Functor m => Functor (StateT s m) where
  fmap :: Functor m => (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \s -> (\(a, s') -> (f a, s')) <$> runStateT m s

instance Monad m => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  (>>=) :: Monad m => StateT s m a -> (a -> StateT s m b) -> StateT s m b
  m >>= k = StateT $ \s -> do
    (a, s') <- runStateT m s
    runStateT (k a) s'

instance (Monad m, Alternative m) => Alternative (StateT s m) where
  empty = StateT (const empty)
  p1 <|> p2 = StateT $ \s -> runStateT p1 s <|> runStateT p2 s

instance MonadError e m => MonadError e (StateT s m) where
  throwError e = StateT $ \s -> throwError e
  catchError m h = StateT $ \s -> runStateT m s `catchError` \e -> runStateT (h e) s
```

## Protokół MonadTrans

W procesie łączenia monad często natkniemy się na schemat typu
``` haskell
throwError e = StateT $ \s -> throwError e
```

Możemy go ująć w klasę **MonadTrans**

``` haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (StateT s) where
    lift :: Monad m => m a -> StateT s m a
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)
```

i potem wystarczy

``` haskell
throwError = lift . throwError
ask = lift ask
reader = lift . reader
-- ...
```

Niestety funkcje o bardziej skomplikowanych typach (`catchError`, `local`) wymagają specjalnej troski.

### Biblioteki

Oczywiście nie musimy tego pisać sami, wszystko to dostepne jest w bibliotekach np. `transformers`, `mtl`.

Musimy tylko pamietać aby dodać je do `build-requires` w naszym `.cabal`

Na przykład opisane tu **StateT** jest w **Control.Monad.State**<br/>
a **MonadTrans** w **Control.Monad.Trans**

Przykład z rzeczywistego kodu:


``` haskell
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

type TCM a = ExceptT String (StateT TcState (Reader Env)) a

runTCM :: TCM a -> (Either String a, TcState)
runTCM = runEnvR . runTcS . runExceptT

runEnvR :: Reader Env a -> a
runEnvR r = runReader r emptyEnv

runTcS :: StateT TcState m a -> m (a, TcState)
runTcS t = runStateT t initState
```
### MonadIO

Szczególnego traktowania przy łączeniu monad wymaga też IO - musi być na samym dole stosu monad, na przykład:

``` haskell
type RedM a = StateT RedS IO a

runROM :: RedM a  -> IO (a, RedS)
runROM = flip runStateT initRS

writeln :: String -> RedM ()
writeln = liftIO . putStrLn

debugs :: [String] -> RedM ()
debugs = whenDebug . writeln . unwords -- whenDebug czyta stan

emitFunDef :: FunDef -> RedM [Core.Stmt]
emitFunDef (FunDef sig body) = do
  (name, args, typ) <- translateSig sig
  debugs ["emitFunDef ", name, " :: ", show typ]
  coreBody <- emitStmts body
  pure [Core.SFunction name args typ coreBody]
```

## Lewostronna rekursja

Popatrzmy na typową gramatykę dla wyrażeń:

```
Exp  ::= Exp "-" Term | Term
Term ::= Term "*" Factor | Factor
Factor ::= int | "(" Exp ")"
```

próba implementacji

``` haskell
pExp = mkSub <$> pExp <*> (symbol "-") <*> pTerm <|> pTerm
mkSub e _ t = ESub e t
```

zapętli się (`pExp` w pierwszej chhwili wywołuje `pExp`)

Natomiast próba odwrócenia produkcji

``` haskell
-- Exp ::= Term - Exp | Term
pExp = (pTerm >> '-' >> pExp) <|> pTerm
```

Spowoduje, że `x - y - z` zostanie błędnie rozpoznane jako `x - (y - z)`.<br/>
Ponadto prawdopodobnie powinniśmy użyć `try` (i to kosztownego).

Problematyczne są wszelkie produkcje postaci `A ::= A X` (lub równoważne)

### Rozwiązanie

```
Exp ::= Term { "-" Term}
```

`Exp` to ciąg `Term` rozdzielonych symbolami "-"

``` haskell
exp = chainl1 minus term where
   minus = symbol "-" >> pure ESub

chainl1 p op = do { x <- p; rest x } where
    rest x = do{ f <- op
               ; y <- p
               ; rest (f x y)
               }
              <|> return x
```

### Przykład

``` haskell
pStmts :: Parser [Stmt]
pStmts = many1 pStmt

pStmt :: Parser Stmt
pStmt = do
  v <- identifier
  foo <- symbol "="
  e <- pExp
  pure (v := e)

pExp, pTerm, pF :: Parser Exp
pExp = pTerm `chainl1` pAdd where pAdd = symbol "+" >> pure EAdd

pTerm = pF `chainl1` pMul where pMul = symbol "*" >> pure EMul

pF = EInt <$> integer <|> EVar <$> identifier <|> parens pExp
```

## Odstępy

``` haskell
ghci> parse ((,) <$> digit <*> digit) "12"
Right ('1','2')
ghci> parse ((,) <$> nat <*> nat) "12"
Left "expected digit"
ghci> parse ((,) <$> nat <*> nat) "1 2"
Left "expected digit"
```
Można sobie  poradzić tak:

``` haskell
space :: Parser ()
space = many (satisfy isSpace) >> pure ()

lexeme :: Parser a -> Parser a
lexeme p = space *> p <* space
  -- do { space; v <- p; space; pure v}

symbol :: String -> Parser String
symbol = lexeme . string

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

natural :: Parser Int
natural = lexeme nat

-- >>> parse ((,) <$> natural <*> natural) "1 2"
-- Right (1,2)
```

## Biblioteki  - kombinatory parsujące

parsec, megaparsec


## Rodzaje (kinds)

* Typy klasyfikują wartości i operacje na nich (np. `'x' :: Char`, `isLetter :: Char -> Bool` )

* Rodzaje klasyfikują typy i operacje na nich

* Typy są rodzaju `*` (np. `Char -> Bool :: *`)

* Jednoargumentowe konstruktory (np. `Tree`) są ropdzaju `* -> *`, dwuargumentowe `* -> * -> *` itp.

    ~~~~ {.haskell}
    {-#LANGUAGE KindSignatures, ExplicitForAll #-}

    class Functor f => Pointed (f :: * -> *) where
        pure :: forall (a :: *).a -> f a
    ~~~~

* Bywają bardiej złożone typy np. dla transformatorów monad

    ~~~~ {.haskell}
    class MonadTrans (t :: (* -> *) -> * -> *) where
        lift :: Monad (m :: * -> *) => forall (a :: *).m a -> t m a
    ~~~~

NB spacje są oboiwiązkowe - `::*->*` to jeden leksem

W nowsszych wersjach GHC można zamiast `*` używać `Type` (import z `Data.Kind`)

## Jeszcze o łączeniu monad

Czemu musimy mieć osobny transformator do każdego rodzaju efektu, zamiast ogólnego schematu?

Dla dowolnych funktorów, ich złożenie jest też funktorem
``` haskelldewl
{-# LANGUAGE ScopedTypeVariables #-}

gfmap :: forall f g a b. (Functor g, Functor f)
         => (a -> b) -> g(f(a)) -> g(f(b))
gfmap fun a = mapG (mapF fun) a where
  mapF :: (a -> b) -> f a -> f b
  mapG :: (f a -> f b) -> g (f a) -> g (f b)
  mapF = fmap
  mapG = fmap
```

NB rozszerzenie `ScopedTypeVariables` jest potrzebne tylko żeby dać sygnatury typów dla `mapF` i `mapG`

### Łączenie Applicative

Składanie Applicative jest trudniejsze, ale możliwe, w przybliżeniu

``` haskell
apF :: F (a -> b) -> F a -> F b
apG :: G (c -> d) -> G c -> G d

apGF            :: G(F(a -> b)) -> G(F a) -> G(F b)
apGF            = apG . fmapG(apF)

fmapG(apF) :: G(F(a -> b)) -> G(F a -> F b)
       apG :: G (F a -> F b)  ->  G(F a) ->  G(F b)

apGF            :: G(F(a -> b)) -> G(F a) -> G(F b)
apG . fmapG(apF) :: G(F(a -> b)) -> G(F a) -> G(F b)
```

### Łączenie monad

Nie istnieje ogólny przepis na składanie monad.

Łatwo to zobaczyć dla alternatywnej prezentacji monad:

``` haskell
class Applicative m => Monad' m where
  join :: m(m a) -> m a
--  m >>= f = join(fmap f m)
--  join mma = mma >>= id

joinMN ::(Monad' m, Monad' n)=> m(n(m(n a))) -> m(n a)
```

Potrzebowalibyśmy

```
swapNM :: n(m a) -> m(n a)
```

### Drzewa jako monady (1)

Równowazność `>>=` i `join` możemy wykorzystać dla zdefiniowania instancji `Monad` dla drzew

Dla drzew z wartościami w liściach jest łatwo:

``` haskell
joinT :: ETree(ETree a) -> ETree a
joinT (Tip t) = t
joinT (Bin l r) = Bin (joinT l) (joinT r)

instance Monad ETree where
    m >>= f = joinT (fmap f m)
```

### Drzewa jako monady (2)

Dla drzew z wartościami w wierzchołkach wewnętrznych jest trochę trudniej, ale da się:

``` haskell
instance Semigroup (Tree a) where
instance Foldable Tree where 
instance Monoid (Tree a) where
-- było jako ćwiczenie

joinT :: Tree(Tree a) -> Tree a
-- joinT Empty = Empty
-- joinT (Node t l r) = joinT l <> t <> joinT r
joinT = foldMap id

-- foldMap f Empty = Empty
-- foldMap f (Node x l r) = go l <> f x <> go r where go = foldMap f


instance Monad Tree where
  -- ta >>= k = joinT (fmap k ta)
  --          = foldMap id (fmap k ta)  -- k :: a -> Tree b
  --          = foldMap k ta
  (>>=) = flip foldMap
```

Dowód równości `foldMap id (fmap k ta) = foldMap k ta` pozostawiamy jako ćwiczenie.

# Administrivia

Egzamin ustny 26.6 (dla osób, które nie uzyskają oceny przed sesją)

# Dziekuję za uwagę