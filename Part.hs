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

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
--  the Lambda Calculus interpreter. for slightly nicer (but non compliant)        --
--  printing, see the commented out 'showLx (Lambda vv tt) ...' in Parser.hs       --
--    Note: gloably 'free' variables (bound in no surounding context)              --
--          are forbidden in general. A hack allowance is made to pass the         --
--          'renaming' examples in the test file, however the user is responsible  --
--          for enforcing this constraint in other cases (becaue the implementor   --
--          is lazy and doesn't want to crap up his code any more.)                --
-------------------------------------------------------------------------------------
-- lisp style if --
if' a b c
    | a == False = c
    | otherwise = b

type VMap = (Map Int LambdaTree)
type Alphabet = [LambdaTree]
type Free = Set Int
type Env = State (Free, VMap, Alphabet)

-- looks up vv in map, if Nothing is found, return vv
lookupVar :: LambdaTree -> Env LambdaTree
lookupVar vv@(Var i) =
    state $ \env@(_, map, _) ->
        (case M.lookup i map of
           Nothing -> vv
           Just vv' -> vv'
        , env)

bindVar :: LambdaTree -> Env LambdaTree
bindVar vv@(Var _)=
    state $ \env@(free, map, alph) -> let (alph', vv') = rebind free vv alph in
                                      (vv', (free, map, alph'))
    where rebind free p@(Var i) a@(f:fs) =
              if' (S.member i free) (rebind free f fs) (a, p)

