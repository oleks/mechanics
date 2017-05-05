module InterpExp

import Ast
import Data.Vect
import MonadExt
import Data.SortedMap

%access public export
%default total

data State
  = MkState (SortedMap String (m ** n ** FunBody m n))

initialState : State
initialState = MkState empty

data InterpExp a
  = MkInterpExp (State -> Maybe a)

bind' : InterpExp a -> (a -> InterpExp b) -> InterpExp b
bind' (MkInterpExp ma) f = MkInterpExp (\s => do
  a <- ma s
  let (MkInterpExp mb) = f a
  mb s)

pure' : a -> InterpExp a
pure' a = MkInterpExp (\s => Just a)

Functor InterpExp where
  map f m = bind' m (\a => pure' (f a))

Applicative InterpExp where
  pure = pure'
  pf <*> px = bind' pf (\f => bind' px (pure . f))

Monad InterpExp where
  (>>=) = bind'

reject : InterpExp a
reject = MkInterpExp (\s => Nothing)

get : InterpExp State
get = MkInterpExp (\s => Just s)

liftMaybe : Maybe a -> InterpExp a
liftMaybe v = MkInterpExp (\s => v)

local : (State -> State) -> InterpExp a -> InterpExp a
local f (MkInterpExp g) = MkInterpExp (\s => g (f s))

getFun : String -> InterpExp (m ** n ** FunBody m n)
getFun name = do
  (MkState st) <- get
  liftMaybe $ lookup name st

mkVar : Expr -> (m ** n ** (FunBody m n))
mkVar expr = (1 ** 0 ** [MkClause [] expr])

unpackVar : (m ** n ** (FunBody m n)) -> InterpExp Expr
unpackVar ((S Z) ** Z ** [MkClause [] expr]) = pure expr
unpackVar _ = reject

mutual
  add' : Int -> Expr -> Expr
  add' 0 e = e
  add' n e = ExpAdd (ExpVal n) e

  mul' : Int -> Expr -> Expr
  mul' 0 e = e
  mul' 1 e = e
  mul' n e = ExpMul (ExpVal n) e

  partial
  baseInterpExp : (Expr -> Expr -> Expr) -> (Int -> Int -> Int)
    -> (Int -> Expr -> Expr)
    -> Expr -> Expr -> InterpExp Expr
  baseInterpExp op fn fc e1 e2 = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    pure $ case (v1, v2) of
      (ExpVal x, ExpVal y) => ExpVal $ fn x y
      (ExpVal c, e2) => fc c e2
      (e1, ExpVal c) => fc c e1
      _ => op v1 v2

  partial
  interpExp : Expr -> InterpExp Expr
  interpExp (FnCall "fst" [(ExpTup e1 _)]) = pure e1
  interpExp (FnCall "snd" [(ExpTup _ e2)]) = pure e2
  interpExp (FnCall "dup" [e]) = pure $ ExpTup e e
  interpExp (FnCall "diff" [e1, e2]) = do
    d <- diff e1 e2
    interpExp d
  interpExp (ExpNam name) = do
    v <- getFun name
    unpackVar v
  interpExp (ExpAdd e1 e2) = baseInterpExp ExpAdd (+) add' e1 e2
  interpExp (ExpMul e1 e2) = baseInterpExp ExpMul (*) mul' e1 e2
  interpExp (ExpLet n e1 e2) = do
    v1 <- interpExp e1
    local (\(MkState s) => MkState $ insert n (mkVar v1) s) (interpExp e2)
  interpExp (ExpIf ec et ef) = do
    vc <- interpExp ec
    case vc of
      ExpVal 0 => interpExp ef
      _ => interpExp et
  interpExp v = pure $ v

  partial
  diff : Expr -> Expr -> InterpExp Expr
  diff (ExpNam n) (ExpNam m) =
    pure $ if n == m then ExpVal 1 else ExpVal 0
  diff (ExpNam _) (ExpVal v) =
    pure $ ExpVal 0
  diff ed (ExpNeg e) = do
    d <- diff ed e
    pure $ ExpNeg d
  diff ed (ExpAdd e1 e2) = do
    d1 <- diff ed e1
    d2 <- diff ed e2
    pure $ ExpAdd d1 d2
  diff ed (ExpMul e1 e2) = do
    d1 <- diff ed e1
    d2 <- diff ed e2
    pure $ ExpAdd
      (ExpMul e1 d2)
      (ExpMul d1 e2)
  diff ed @ (ExpNam m) (ExpLet n e1 e2) = do
    d1 <- diff ed e1
    d2 <- diff (if n == m then (ExpNam "") else ed) e2
    pure $ ExpLet n d1 d2
  diff ed @ (ExpNam m) (ExpIf ec et ef) = do
    dt <- diff ed et
    df <- diff ed ef
    pure $ ExpIf ec dt df

interp : InterpExp a -> State -> Maybe a
interp (MkInterpExp i) s = i s

partial
runInterpExp : State -> Expr -> Maybe Expr
runInterpExp s e = interp (interpExp e) s
