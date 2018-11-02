{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Core.Record.Dev where


-- ~
import Core
import Data.Kind
import Data.Monoid          (Monoid)
import Control.Arrow        ((&&&), (***))
import Control.Category     ((>>>), (<<<))
import Data.Function        ((&), const, flip)
import Control.Applicative  ((<**>))
import Control.Monad        ((>>=), (=<<))
import Data.Proxy           (Proxy(..))
import GHC.Generics         (Generic)
import Data.ByteString      (ByteString)
import Data.Vector          (Vector)
import Data.Map             (Map)
import Data.Data            (Typeable)
import Prelude
  ( return
  , String
  , IO
  , show
  , error
  , (<$>)
  , (>>)
  , putStrLn
  , fromIntegral
  , fmap
  , pure
  )

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

-- + Generics
import qualified GHC.Generics as Gen

-- + Serialization
import Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.UUID.Types
  ( UUID
  )
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as A
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID


-- + Type Utils
import GHC.TypeLits
  ( Symbol
  , KnownNat
  , KnownSymbol
  , TypeError
  , ErrorMessage(..)
  )
import GHC.OverloadedLabels
  ( IsLabel(..)
  )
import Data.Type.Bool as Bool
import GHC.Exts as Ext
-- import qualified GHC.TypeLits as TL


-- Misc.
import GHC.TypeLits as TL
import Language.Haskell.TH as TH

-- + Local
import Core.Record as R
-- ~




{-# ANN module ("HLint: ignore" :: Pre.String) #-}




type User =
  ( "id" := Int
  , "name" := Text
  , "email" := Text
  )



