{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v (s, _) = case M.lookup v s of
                        Just x -> Right x
                        Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
-- update v n (s, str) = (M.insert v n s, str)
update v n (s, str) = addTrace ("Let " ++ v ++ " " ++ show n) (M.insert v n s, str)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace str (s, trace)= (s, trace ++ " | " ++ str)

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm (Let v e) s =  case evalExp e s of
                          Left err -> Left err
                          Right (n :!: s') -> Right (Skip :!: update v n s')

stepComm (Seq Skip c1) s = Right (c1 :!: s)

stepComm (Seq c0 c1) s = case stepComm c0 s of
                          Left err -> Left err
                          Right (c0' :!: s') -> Right (Seq c0' c1 :!: s')

stepComm (IfThenElse b c0 c1) s = case evalExp b s of
                                    Left err -> Left err
                                    Right (b' :!: s') -> if b' then Right (c0 :!: s') else Right (c1 :!: s')

stepComm (RepeatUntil c b) s = Right (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const nv) s = Right (nv :!: s)

evalExp (Var x) s = case lookfor x s of
                    Left e -> Left e
                    Right n -> Right (n :!: s)

evalExp (UMinus e) s = case evalExp e s of
                        Left err -> Left err
                        Right (n :!: s') -> Right ((-n) :!: s')

evalExp (Plus e0 e1) s = evalOp e0 e1 s (+)

-- evalExp (Plus e0 e1) s = case evalOp e0 e1 s (+) of
--                                       Left e -> Left e
--                                       Right (p :!: state) -> Right (p :!: addTrace ("Plus " ++ show e0 ++ " " ++ show e1) state)

evalExp (Minus e0 e1) s = evalOp e0 e1 s (-)
evalExp (Times e0 e1) s = evalOp e0 e1 s (*)

evalExp (Div e0 e1) s = case evalExp e0 s of
                         Left e -> Left e
                         Right (n :!: s') -> case evalExp e1 s' of
                                              Left e -> Left e
                                              Right (0 :!: _) -> Left DivByZero
                                              Right (n' :!: s'') -> Right (div n n' :!: s'')

evalExp (VarInc x) s = case lookfor x s of
                        Left e -> Left e
                        Right v -> Right (v + 1 :!: s)

evalExp (VarDec x) s = case lookfor x s of
                        Left e -> Left e
                        Right v -> Right (v - 1 :!: s)

evalExp BTrue s = Right (True :!: s)
evalExp BFalse s = Right (False :!: s)

evalExp (Not p) s = case evalExp p s of
                      Left e -> Left e
                      Right (b :!: s') -> Right (not b :!: s')

evalExp (Or p0 p1) s =  evalOp p0 p1 s (||)
evalExp (And p0 p1) s = evalOp p0 p1 s (&&)
evalExp (Eq e0 e1) s = evalOp e0 e1 s (==)
evalExp (Lt e0 e1) s =  evalOp e0 e1 s (<)
evalExp (Gt e0 e1) s = evalOp e0 e1 s (>)
evalExp (NEq e0 e1) s = evalOp e0 e1 s (/=)

evalOp :: Exp a -> Exp a -> State -> (a -> a -> b) -> Either Error (Pair b State)
evalOp e0 e1 s f = case evalExp e0 s of
                    Left e -> Left e
                    Right (n0 :!: s') -> case evalExp e1 s' of
                                          Left e -> Left e
                                          Right (n1 :!: s'') -> Right (f n0 n1 :!: s'')
