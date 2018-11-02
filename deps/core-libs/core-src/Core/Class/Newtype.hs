{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Core.Class.Newtype (
    Newtype(..)
  , un
) where


-- ~
import Core
import qualified Prelude    as Pre

import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Mon
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Proxy                   as Proxy
import qualified Data.Functor                 as Fun
import qualified Data.Functor.Identity        as Fun
import qualified Data.Vector                  as Vector
import qualified Data.Traversable             as Traverse
import qualified Data.String                  as String



-- + Local
-- ~



class Newtype new a | new -> a where
  wrap :: a -> new
  unwrap :: new -> a


-- |
-- Short alias for 'unwrap'.
un :: Newtype new a => new -> a
un = unwrap


