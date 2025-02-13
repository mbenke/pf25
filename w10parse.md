---
title: Programowanie Funkcyjne
subtitle: Kombinatory parsujące
author:  Marcin Benke
date: Wykład 10/2025
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

Pamiętajmy, że przy takich typach, alternatywa jest "lewicowa": parser `p1 <|> p2` bierze pod uwagę tylko lewy parser `p1` o ile się uda;<br/>
a czasem jeszcze bardziej - `p2` wchodzi do gry tylko gdy `p1` zawiedzie nie konsumując wejścia (nie wracamy do raz przeczytanych znaków).
W razie potrzeby powrotu można użyć kombinatora `try`, zwykle w wersji `try p1 <|> p2`.

Biblioteki używają często bardziej złożonych typów.<br />
W naszych przykładach wejście jest typu **String**, ale może być **Text**, **ByteString** lub inne.

## Łączenie monad

Wspomniane wcześniej typy parserów

``` haskell
type ListParser a  = String -> [(a, String)]
type MaybeParser a = String -> Maybe (a, String)
type ErrorParser a = String -> Either ErrMsg (a, String)
```

łączą efekt stanu z inną monadą; możemy uogólnić ten schemat:


``` haskell
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

Szczególnego traktowania przy łączeniu monad wymaga też IO - musi być na samym dole stosu monad, na przykład.

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

Spowoduje, że `x - y - z` zostanie błędnie rozpoznane jako `x - (y - z)`.

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