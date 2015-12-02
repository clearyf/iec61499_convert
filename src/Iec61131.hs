module Iec61131 where

import BasePrelude

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
    deriving (Show,Eq)

vartypeFromString :: String -> IECVariable
vartypeFromString str =
    maybe (error "Unhandled IEC variable type!") id (lookup lowerCased alist)
  where
    lowerCased = fmap toLower str
    alist =
        [ ("bool", IECBool)
        ,
          -- Real types
          ("real", IECReal)  -- TODO REAL is 32bit
        , ("lreal", IECReal) -- TODO LREAL is 0..1 64 bit
        ,
          -- Unsigned integer types
          ("byte", IECUInt Eight)
        , ("usint", IECUInt Eight)
        , ("word", IECUInt Sixteen)
        , ("uint", IECUInt Sixteen)
        , ("udint", IECUInt ThirtyTwo)
        , ("dword", IECUInt ThirtyTwo)
        , ("ulint", IECUInt SixtyFour)
        ,
          -- Signed integer types
          ("sint", IECInt Eight)
        , ("int", IECInt Sixteen)
        , ("dint", IECInt ThirtyTwo)
        , ("lint", IECInt SixtyFour)
        ,
          -- Various other types
          ("time", IECTime)
        , ("date_and_time", (error "Can't handle DATE_AND_TIME type!"))
        , ("time_of_day", (error "Can't handle TIME_OF_DAY type!"))]
