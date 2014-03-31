-- -*- haskell -*- --
module Main where
import Parser
import Library

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)

import Control.Monad.State

import Data.List

import Debug.Trace 



-------------------------------------------
-- the Lambda Calculus with State monads --
-------------------------------------------
-- lisp style if --
if' a b c
    | a == False = c
    | otherwise = b

type VMap = (Map Int LambdaTree)
type Alphabet = [LambdaTree]
type Env = State (Bool, VMap, Alphabet)

-- looks up vv in map, if Nothing is found, return vv
lookupVar :: LambdaTree -> Env LambdaTree
lookupVar vv@(Var i) =
    state $ \env@(_, map, _) ->
        (case M.lookup i map of
           Nothing -> vv
           Just vv' -> vv'
        , env)

bindVar :: LambdaTree -> LambdaTree -> Env LambdaTree
bindVar param@(Var i) vv = 
    state $ \env@(evalP, map, alph) ->
        (param, (evalP, (M.insert i vv map), alph))

scoped expr = state $ \env -> (evalState expr env, env)

 ------------------------------------------
 -- APPLY: do one thing, then back out.  --
 ------------------------------------------
apply :: LambdaTree -> Env LambdaTree
apply (Number i) = return $ nthChurch i

apply b@(Var _) = lookupVar b

apply (Lambda param body) =
    scoped $ bindVar param param >> apply body >>= (return . (Lambda param))

apply (Apply left@(Lambda param body) right) =
    state
    $ \env@(evalP,m,a) ->
        (evalState (if evalP
                    then bindVar param right >> apply body
                    else (scoped $ apply left) >>= (\lhs -> apply right >>=  return . (Apply lhs)))
         (False, m, a)
        , env)

apply (Apply left right) =
    (scoped $ apply left) >>= (\lhs -> apply right >>=  return . (Apply lhs))

---------------------------------------------------------------------------------
--                               Numbers                                       --
---------------------------------------------------------------------------------
nthChurch n = parseLambda $ "(\\ab." ++ (churchString n) ++ ")"
          where churchString :: Int -> String
                churchString i | i == 0 = "(b)"
                               | otherwise = "(a" ++ churchString  (i - 1) ++ ")"

--------------------------------------------------------------------------------
--                           REPL and IO                                      --
--------------------------------------------------------------------------------
alphabet = (Var (fromEnum 'a')) : map (\(Var n) -> (Var (n + 1))) alphabet
step expr = evalState (apply expr) (True, library, alphabet)

nApply :: LambdaTree -> Int -> LambdaTree
nApply expr n
       | n == 0 = expr
       | otherwise = nApply (step expr) $ n - 1


nInterpret str n = showLx $ nApply e n
    where e = parseLambda str

-- apply reductions until we reach a fixed point --
reApply :: LambdaTree -> LambdaTree
reApply e = loop e $ step e
    where loop e e'
              | e == e' = e'
              | otherwise = loop e' $ step e'

interpret :: [Char] -> String
interpret str = showLx $ reApply $ parseLambda str

main = do
  line <- getLine
  if line == "--repl"
  then let loop :: IO ()
           loop = do
             line <- getLine
             putStrLn $ interpret line
             loop
       in loop
  else
      putStrLn $ interpret line
           
