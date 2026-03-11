{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Task2 (Expr(..), Parse(..), Eval(..), evalExpr)
import Data.List (nub)
import Data.Maybe (fromMaybe)

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
--

data BoolOp = And | Or | Xor deriving Show

instance Parse BoolOp where
    parse "and" = Just And
    parse "or"  = Just Or
    parse "xor" = Just Xor
    parse _     = Nothing

instance Eval Bool BoolOp where
    evalBinOp And = (&&)
    evalBinOp Or  = (||)
    evalBinOp Xor = (/=)


solveSAT :: String -> Maybe Bool
solveSAT s = do
    expr <- parse s :: Maybe (Expr Bool BoolOp)
    let vars = nub (collectVars expr)
        assignments = allAssignments vars
    Just (any (`evalWithEnv` expr) assignments)
  where
    collectVars :: Expr a op -> [String]
    collectVars (Var v) = [v]
    collectVars (BinOp _ l r) = collectVars l ++ collectVars r
    collectVars (Lit _) = []

    evalWithEnv :: [(String, Bool)] -> Expr Bool BoolOp -> Bool
    evalWithEnv env e = fromMaybe False (evalExpr env e)

    allAssignments :: [String] -> [[(String, Bool)]]
    allAssignments [] = [[]]
    allAssignments (v:vs) =
        let rest = allAssignments vs
        in map ((v, False):) rest ++ map ((v, True):) rest
