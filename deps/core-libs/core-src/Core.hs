{-# LANGUAGE NoImplicitPrelude #-}
module Core (
    System.IO.print
  , System.IO.putStrLn
  , Data.Functor.Functor(..)
  , Control.Applicative.Applicative(..)
  , Data.Semigroup.Semigroup(..)
  , Data.Bifunctor.Bifunctor(..)
  , Data.Foldable.Foldable(..)
  , Data.Foldable.any
  , Data.Foldable.all
  , Data.Foldable.find
  , Data.Foldable.notElem
  , Data.Foldable.concatMap
  , Data.List.map
  , (Data.List.++)
  , Data.Text.pack
  , module Data.Ord
  , module Data.Eq
  , module Data.Int
  , module Data.Word
  , module Data.Bool
  , module Data.Maybe
  , module Data.Either
  , module Data.Tuple
  , module Data.Char
  , Data.Text.Text
  , Data.ByteString.ByteString
  , Data.Text.Encoding.encodeUtf8
  , Data.Text.Encoding.decodeUtf8
  , Data.Text.Encoding.decodeUtf8'
  , module Data.Function
  , Control.Monad.Monad(..)
  , Control.Monad.mapM
  , Control.Monad.mapM_
  , Control.Monad.when
  , (Control.Monad.=<<)
  , (Control.Category.>>>)
  , (Control.Category.<<<)
  , (Control.Arrow.&&&)
  , (Control.Arrow.***)
  , Data.Monoid.Monoid
  , Control.Monad.IO.Class.liftIO
  , Prelude.Integer
  , Prelude.Double
  , Prelude.Float
  , Prelude.String
  , Prelude.IO
  , Prelude.Show
  , Prelude.Read
  , Prelude.read
  , Prelude.show
  , Prelude.error
  , Prelude.Num(..)
  , (Control.Applicative.<**>)
  , Data.Functor.void
  , (Data.Functor.$>)
  , (Data.Functor.<$>)
  , (Data.Functor.<&>)
  , Data.Typeable.Typeable
  , GHC.Generics.Generic
  , Data.Proxy.Proxy(..)
) where

import qualified System.IO
import qualified Prelude
import qualified Data.Functor
import Data.Function
import qualified Data.Semigroup
import qualified Data.Monoid
import qualified Control.Monad
import qualified Control.Monad.IO.Class
import qualified Control.Arrow
import qualified Control.Applicative
import qualified Control.Category
import qualified Data.List
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.ByteString
import qualified Data.Proxy

import Data.Ord
import Data.Eq
import Text.Show
import Text.Read
import Data.Int
import Data.Word
import Data.Bool
import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Typeable
import GHC.Generics



