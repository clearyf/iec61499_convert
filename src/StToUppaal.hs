{-# LANGUAGE FlexibleContexts #-}

module StToUppaal (stToUppaal, showValue, showVarType) where

import           BasePrelude
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Reader (MonadReader, ask, local, runReaderT)
import           Control.Monad.Writer (MonadWriter, execWriterT, tell)
import           Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NE
import           ParseIec61499 (ECAlgorithm(..))
import           ParseSt
       (LValue(..), Statement(..), Value(..), IECVariable(..), Width(..),
        StMonoOp(..), StBinaryOp(..), CaseSubExpression(..))

showVarType :: MonadError String m => IECVariable -> m (String,String)
showVarType v =
    case v of
        IECBool -> pure ("bool", mempty)
        IECUInt Eight -> pure $ uintWithRange 8
        IECUInt Sixteen -> pure $ uintWithRange 16
        IECUInt ThirtyTwo -> pure $ uintWithRange 32
        IECUInt SixtyFour -> pure $ uintWithRange 64
        IECInt Eight -> pure $ intWithRange 7
        IECInt Sixteen -> pure $ intWithRange 15
        IECInt ThirtyTwo -> pure $ intWithRange 31
        IECInt SixtyFour -> pure $ intWithRange 63
        IECArray idxs var -> do
            (x,_) <- showVarType var
            let y = "[" <> fold (NE.intersperse "," (fmap show idxs)) <> "]"
            pure (x, y)
        IECString size ->
            pure (uppaalIntWithRange 0 127, "[" <> show size <> "]")
        t -> throwError ("Uppaal doesn't support " <> show t <> " type!")

uppaalIntWithRange :: Integer -> Integer -> String
uppaalIntWithRange from to = "int[" <> show from <> "," <> show to <> "]"

intWithRange :: Integer -> (String, String)
intWithRange i = (uppaalIntWithRange ((-2) ^ i) (2 ^ i - 1), mempty)

uintWithRange :: Integer -> (String, String)
uintWithRange i = (uppaalIntWithRange 0 (2 ^ i), mempty)

stToUppaal :: MonadError String m => ECAlgorithm -> m String
stToUppaal al =
    fmap fold (execWriterT (runReaderT writeFunction 0))
  where
    writeFunction = do
        writeLine ("void " <> ecAlgorithmName al <> "()")
        writeBlock (ecAlgorithmStText al)
        -- Append blank line for formatting.
        writeLine
            mempty

writeLine
    :: (MonadReader Int m, MonadWriter (DList String) m)
    => String -> m ()
writeLine l = do
    n <- ask
    tell (DList.singleton (replicate n '\t' <> l <> "\n"))

increaseIndent :: MonadReader Int m => m a -> m a
increaseIndent = local (1 +)

writeBlock
    :: (MonadError String m, MonadWriter (DList String) m, MonadReader Int m)
    => [Statement] -> m ()
writeBlock statements = do
    writeLine "{"
    increaseIndent (traverse_ writeStatement statements)
    writeLine "}"

writeStatement
    :: (MonadError String m, MonadWriter (DList String) m, MonadReader Int m)
    => Statement -> m ()
writeStatement st =
    case st of
        Declaration name typeIn -> do
            (typeOut,suffix) <- showVarType typeIn
            writeLine (typeOut <> " " <> name <> suffix <> ";")
        Assignment lvalue rvalue -> do
            lv <- showLocation lvalue
            rv <- showValue rvalue
            writeLine (lv <> " = " <> rv <> ";")
        For name start end step body -> do
            writeLine
                ("for (int " <> name <> " = " <> show start <> "; " <> name <>
                 " != " <> show end <> "; " <> name <> " = " <>
                 name <> " + (" <> show (fromMaybe 1 step) <> "))")
            writeBlock body
        While cond body -> do
            value <- showValue cond
            writeLine ("while (" <> value <> ")")
            writeBlock body
        Repeat body cond -> do
            writeLine "do"
            writeBlock body
            value <- showValue cond
            writeLine ("while (" <> value <> ")")
        Case var branches defaultBranch -> do
            value <- showValue var
            writeLine ("case (" <> value <> ")")
            writeLine "{"
            traverse_ writeBranch branches
            writeDefaultBranch
            writeLine "}"
            where writeCase i = writeLine (i <> ":")
                  writeCases = traverse_ (writeCase . show)
                  writeCaseExp (CaseInt i) = writeCase (show i)
                  writeCaseExp (CaseRange from to)
                    | from <= to = writeCases (enumFromTo from to)
                    | otherwise =
                        writeCases (enumFromThenTo from (from - 1) to)
                  writeBranch (cases,body) = do
                      traverse_ writeCaseExp cases
                      increaseIndent $
                          do traverse_ writeStatement body
                             writeStatement Break
                  writeDefaultBranch = do
                      writeCase "default"
                      increaseIndent $
                          do traverse_ writeStatement defaultBranch
                             writeStatement Break
        If cond branch -> do
            value <- showValue cond
            writeLine ("if (" <> value <> ")")
            writeBlock branch
        IfElse cond branch1 branch2 -> do
            value <- showValue cond
            writeLine ("if (" <> value <> ")")
            writeBlock branch1
            writeLine "else"
            writeBlock branch2
        Break -> writeLine "break;"
        Return -> writeLine "return;"

showArgs :: MonadError String m => [Value] -> m String
showArgs = fmap (fold . intersperse ", ") . traverse showValue

showValue :: MonadError String m => Value -> m String
showValue value =
    case value of
        StSubValue values ->
            fmap
                (\x -> "(" <> x <> ")")
                (showValue values)
        StBool True -> pure "true"
        StBool False -> pure "false"
        StBinaryOp op a b -> showBinaryValue a op b
        StMonoOp op a -> showMonoValue op a
        StTime t -> pure $ show t
        StInt i -> pure $ show i
        StLValue v -> showLocation v
        -- This entry will not be required once the float -> int conversion
        -- is working correctly.
        StFloat i -> pure $ showFFloat Nothing i ""
        StFunc name args ->
            fmap
                (\x -> name <> "(" <> x <> ")")
                (showArgs args)
        StChar c -> pure $ show (ord c)

showBinaryValue
    :: MonadError String m
    => Value -> StBinaryOp -> Value -> m String
showBinaryValue a op b = fmap fold (sequence (createString opStr))
  where
    createString x = [showValue a, pure " ", x, pure " ", showValue b]
    opStr =
        case op of
            StAddition -> pure "+"
            StSubtract -> pure "-"
            StExp -> throwError "Uppaal doesn't support exponentiation!"
            StMultiply -> pure "*"
            StDivide -> pure "/"
            StEquals -> pure "="
            StNotEquals -> pure "!="
            StLessThanEquals -> pure "<="
            StLessThan -> pure "<"
            StGreaterThanEquals -> pure ">="
            StGreaterThan -> pure ">"
            StMod -> pure "%"
            StBitwiseAnd -> pure "&"
            StAnd -> pure "&&"
            StOr -> pure "||"
            StXor -> pure "^"

showMonoValue :: MonadError String m => StMonoOp -> Value -> m String
showMonoValue op v =
    case op of
        StNegate -> mappend <$> pure "-" <*> showValue v
        StNot -> mappend <$> pure "!" <*> showValue v

showLocation :: MonadError String m => LValue -> m String
showLocation value =
    case value of
        SimpleLValue name -> pure name
        ArrayLValue name idx -> mappend <$> pure name <*> showValue idx
