
{-|
Module      : $header$
Copyright   : (c) Laurent P René de Cotret, 2019
License     : GNU GPL, version 2 or above
Maintainer  : laurent.decotret@outlook.com
Stability   : internal
Portability : portable

This module re-exports internal pandoc-pyplot functionality.
-}

module Text.Pandoc.Filter.Pyplot.Internal (
      module Text.Pandoc.Filter.Pyplot.Configuration
    , module Text.Pandoc.Filter.Pyplot.FigureSpec
    , module Text.Pandoc.Filter.Pyplot.Scripting
    , module Text.Pandoc.Filter.Pyplot.Types
 ) where

import           Text.Pandoc.Filter.Pyplot.Configuration
import           Text.Pandoc.Filter.Pyplot.FigureSpec
import           Text.Pandoc.Filter.Pyplot.Scripting
import           Text.Pandoc.Filter.Pyplot.Types
