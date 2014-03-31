module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Data.Char

import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.Char

data LambdaTree = Lambda LambdaTree LambdaTree
                | Apply LambdaTree LambdaTree
                | Number Int
                | Var Int
                | Promise LambdaTree
  deriving (Show, Eq)

-- Merges adjacent lambdas as the paper does for nicer printouts; 
--   causes test failures.  This is a disjoint pattern to
--   Nat's showLx (Lambda vv tt), so don't comment that out
--   when uncommenting this.
showLx (Lambda aa ll@(Lambda bb cc))
       | aa == cc = "T"
       | bb == cc = "0" -- aka F
       | otherwise = "(\\" ++ showLx aa ++ next ll
                      where next (Lambda vv ll) = showLx vv ++ next ll
                            next tt = "." ++ showLx tt ++ ")"

-- showLx (Lambda aa ll@(Lambda bb cc)) = "(\\" ++ showLx aa ++ next ll
--         where next (Lambda vv ll) = showLx vv ++ next ll
--               next tt = "." ++ showLx tt ++ ")"

showLx (Lambda vv tt) = "(\\" ++ showLx vv ++ "." ++ showLx tt ++ ")"

showLx (Apply aa bb@(Apply _ _)) =
       showLx aa ++ "(" ++ showLx bb ++ ")"
showLx (Apply aa bb)  = showLx aa ++ showLx bb

showLx (Number nn)    = show nn
showLx (Var vv)
       | vv <= fromEnum 'z' && vv >= fromEnum 'a' = [(chr vv)]
       | otherwise = "<" ++ show vv ++ ">"

showLx (Promise e) = "{#" ++ showLx e ++ "}"

nameExpr = 
  do nn <- (letter <|> oneOf "+-*/∨∧=")
     spaces
     return (Var (fromEnum nn))

numbExpr :: GenParser Char st LambdaTree
numbExpr =
  do nn <- many1 digit
     spaces
     return (Number (read nn :: Int))

makeLambda :: [Int] -> LambdaTree -> LambdaTree
makeLambda [nn] bb = Lambda (Var nn) bb
makeLambda (nn:ns) bb = Lambda (Var nn) (makeLambda ns bb)

lambdaExpr :: GenParser Char st LambdaTree
lambdaExpr =
  do char '\\'
     spaces
     ns <- many1 lower
     spaces
     char '.'
     spaces
     ee <- expr
     spaces
     return (makeLambda (map fromEnum $ ns) ee)

boundExpr = 
  do char '<'
     nn <- many1 digit
     char '>'
     return (Var (read nn :: Int))

parenExpr =
  do char '('
     spaces
     ee <- expr
     spaces
     char ')'
     spaces
     return ee

simpleExpr = nameExpr <|> lambdaExpr <|> parenExpr <|> numbExpr <|> boundExpr

expr =
  do es <- many1 simpleExpr
     spaces
     return $ foldl1 Apply es

fullExpr :: GenParser Char st LambdaTree
fullExpr =
  do spaces
     ee <- expr
     spaces
     eof
     return ee

parseLambda ss =
  case parse fullExpr "error" ss of
    Left  ee -> error (show ee)
    Right tt -> tt 
