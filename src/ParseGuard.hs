module ParseGuard (Guard(..), parseGuard) where

import BasePrelude
import Data.Set (Set)
import ParseSt (Value(..), StBinaryOp(..), LValue(..), parseValue)

data Guard = Guard
    { guardEvent :: Maybe String
    , guardCondition :: Maybe Value
    } deriving (Show,Eq)

parseGuard :: Set String -> String -> Maybe Guard
parseGuard events str = do
  v <- tokenizeGuard str
  justEvent events v <|>
    eventCondition events (rewriteValue events v) <|>
    justCondition v

justEvent :: Set String -> Value -> Maybe Guard
justEvent set v = case v of
  StLValue (SimpleLValue s) -> fmap f (find (==s) set)
  _ -> mzero
  where
    f s = Guard (pure s) mzero

justCondition :: Value -> Maybe Guard
justCondition = pure . Guard mzero . pure

eventCondition :: Set String -> Value -> Maybe Guard
eventCondition set v = case v of
  StBinaryOp StAnd (StLValue (SimpleLValue s)) e -> fmap (f e) (find (==s) set)
  _ -> mzero
  where
    f e s = Guard (pure s) (pure e)

-- As AND has higher precedence than OR, if the guard is something
-- like "event AND this OR that", then the OR will be the outermost
-- operator.  However in the guard condition the first AND is special,
-- so move that to the outside.  Note that XOR also has lower
-- precedence than AND, so that needs to be checked as well.
isRewriteRequired :: Set String -> Value -> Maybe String
isRewriteRequired set v = case v of
  StBinaryOp StAnd (StBinaryOp StAnd (StLValue (SimpleLValue s)) _) _ ->
    find (==s) set
  StBinaryOp StOr (StBinaryOp StAnd (StLValue (SimpleLValue s)) _) _ ->
    find (==s) set
  StBinaryOp StXor (StBinaryOp StAnd (StLValue (SimpleLValue s)) _) _ ->
    find (==s) set
  StBinaryOp StAnd l _ -> isRewriteRequired set l
  StBinaryOp StOr l _ -> isRewriteRequired set l
  StBinaryOp StXor l _ -> isRewriteRequired set l
  _ -> mzero

rewriteValue :: Set String -> Value -> Value
rewriteValue set v = maybe v (recurse v) (isRewriteRequired set v)
  where
    recurse value@(StBinaryOp StAnd (StLValue (SimpleLValue s)) _) target
      | s == target = value
      | otherwise = recurse (leftRotate value) target
    recurse value target = recurse (leftRotate value) target

leftRotate :: Value -> Value
leftRotate (StBinaryOp op1 (StBinaryOp op2 vll vlr) vr) =
  StBinaryOp op2 vll (StBinaryOp op1 vlr vr)
leftRotate x = x

tokenizeGuard :: String -> Maybe Value
tokenizeGuard str = either (const mzero) pure (parseValue str)
