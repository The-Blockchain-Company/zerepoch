-- | @unit@ and related functions.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module ZerepochCore.StdLib.Data.Unit
    ( unit
    , unitval
    , sequ
    ) where

import           ZerepochCore.Core
import           ZerepochCore.MkPlc
import           ZerepochCore.Name
import           ZerepochCore.Quote

import           Universe

-- | '()' as a PLC type.
unit :: uni `Includes` () => Type TyName uni ()
unit = mkTyBuiltin @_ @() ()

-- | '()' as a PLC term.
unitval :: (TermLike term TyName Name uni fun, uni `Includes` ()) => term ()
unitval = mkConstant () ()

-- | 'seq' specified to '()' as a PLC term.
sequ :: (TermLike term TyName Name uni fun, uni `Includes` ()) => term ()
sequ = runQuote $ do
    x <- freshName "x"
    y <- freshName "y"
    return
        . lamAbs () x unit
        . lamAbs () y unit
        $ unitval
