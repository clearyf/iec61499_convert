module StTypes
       (CaseSubExpression(..), IECVariable(..), LValue(..), StMonoOp(..),
        StBinaryOp(..), Statement(..), Value(..), Width(..))
       where

import BasePrelude
import Data.List.NonEmpty (NonEmpty(..))

data Statement
    = Assignment LValue
                 Value
    | Declaration String
                  IECVariable
    | If Value
         [Statement]
    | IfElse Value
             [Statement]
             [Statement]
    | For String
          Int
          Int
          (Maybe Int)
          [Statement] -- Start End Step
    | While Value
            [Statement]
    | Repeat [Statement]
             Value
    | Case Value
           [(NonEmpty CaseSubExpression, [Statement])]
           [Statement]
    | Break
    | Return
    deriving (Show,Eq)

data CaseSubExpression
    = CaseInt Int
    | CaseRange Int
                Int
    deriving (Show,Eq)

data Value
    = StBool Bool
    | StChar Char
    | StMonoOp StMonoOp
               Value
    | StBinaryOp StBinaryOp
                 Value
                 Value
    | StLValue LValue
    | StInt Integer
    | StFloat Double
    | StTime Integer
    | StFunc String
             [Value]
    | StSubValue Value
    deriving (Show,Eq)

data StMonoOp
    = StNegate
    | StNot
    deriving (Show,Eq)

data StBinaryOp
    = StAddition
    | StSubtract
    | StExp
    | StMultiply
    | StDivide
    | StEquals
    | StNotEquals
    | StLessThanEquals
    | StLessThan
    | StGreaterThanEquals
    | StGreaterThan
    | StMod
    | StBitwiseAnd
    | StAnd
    | StOr
    | StXor
    deriving (Show,Eq)

data LValue
    = SimpleLValue String
    | ArrayLValue String
                  Value
    deriving (Show,Eq)

data Width
    = Eight
    | Sixteen
    | ThirtyTwo
    | SixtyFour
    deriving (Show,Eq)

data IECVariable
    = IECReal
    | IECInt Width
    | IECUInt Width
    | IECBool
    | IECTime
    | IECArray (NonEmpty Int)
               IECVariable
    | IECString Int
    deriving (Show,Eq)
