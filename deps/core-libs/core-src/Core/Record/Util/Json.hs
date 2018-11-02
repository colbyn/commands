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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
module Core.Record.Util.Json (
    module Core.Record.Util.Json.Joins
  , (-:-)
  , (-=-)
  , (-.-)
  , (<===)
  , decodeListVia
  , encodeField
  , encodeField'
  , encodeFieldWith
  , (|:|)
  , (|=|)
  , (|.|)
) where


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
import GHC.TypeLits as TL
import GHC.OverloadedLabels (IsLabel(..))
import Data.Type.Bool as Bool


-- import Language.Haskell.TH as TH

-- + Local
import Core.Record as R
import Core.Record.Internal (Cons(..))
import Core.Class.Newtype as New
import Core.Record.Util.Json.Joins
-- ~



{-# ANN module ("HLint: ignore" :: Pre.String) #-}




-------------------------------------------------------------------------------
-- | JSON Decoders
-------------------------------------------------------------------------------


(-:-)
  :: forall l v.
      ( KnownSymbol l
      , A.FromJSON v
      , FromJSON v
      )
  => Proxy l
  -> A.Object
  -> A.Parser (l := v)
(-:-) = decodeField


(-=-)
  :: forall l v new.
      ( KnownSymbol l
      , New.Newtype new v
      , A.FromJSON new
      )
  => Proxy l
  -> A.Object
  -> Proxy new
  -> A.Parser (l := v)
(-=-) = decodeField'

(-.-)
  :: forall l v.
    ( KnownSymbol l
    )
  => Proxy l
  -> A.Object
  -> (A.Value -> A.Parser v)
  -> A.Parser (l := v)
(-.-) = decodeFieldWith


-- | Simple helper functon.
-- Exists to turn:
-- @@
-- instance A.FromJSON AccountI where
--   parseJSON (A.Object o) = (return <<< SomeWrapper) =<< join2
--     ( #id <:> o
--     , #name <:> o
--     )
-- @@
-- Into:
-- @@
-- instance A.FromJSON AccountI where
--   parseJSON (A.Object o) = SomeWrapper <=== join2
--     ( #id +:+ o
--     , #name +:+ o
--     )
-- @@
(<===)
  :: Monad m
  => (a -> b)
  -> m a
  -> m b
(<===) f v = (return . f) =<< v


-- | Simple helper functon (based on 'parseJSONList') for parsing lists via the
--  'FromJSON' instance accociated with the given 'Newtype' type.
decodeListVia
  :: forall l v new.
      ( New.Newtype new v
      , A.FromJSON new
      )
  => Proxy new
  -> A.Value
  -> A.Parser [v]
decodeListVia _ input = result <&> (List.map New.unwrap)
  where
    result :: A.Parser [new]
    result = A.parseJSONList input




-------------------------------------------------------------------------------
-- | JSON Encoders
-------------------------------------------------------------------------------


encodeField
  :: forall label value record res. (Has label value record, KnownSymbol label, ToJSON value, A.KeyValue res)
  => Proxy label
  -> record
  -> res
encodeField key rec = key' .= val
  where
    val :: value
    val = rec \- key

    key' :: Text
    key' = Text.pack $ symbolVal (Proxy :: Proxy label)


encodeField'
  :: forall label value record res new.
    ( Has label value record
    , KnownSymbol label
    , New.Newtype new value
    , ToJSON new
    , A.KeyValue res
    )
  => Proxy label
  -> record
  -> Proxy new
  -> res
encodeField' key rec _ = key' .= (toJSON val')
  where
    val :: value
    val = rec \- key

    val' :: new
    val' = New.wrap val

    key' :: Text
    key' = Text.pack $ symbolVal (Proxy :: Proxy label)

encodeFieldWith
  :: forall label value record res json.
    ( Has label value record
    , KnownSymbol label
    , A.ToJSON json
    , A.KeyValue res
    , json ~ A.Value
    )
  => Proxy label
  -> record
  -> (value -> json)
  -> res
encodeFieldWith key rec f = key' .= (f val)
  where
    val :: value
    val = rec \- key

    key' :: Text
    key' = Text.pack $ symbolVal (Proxy :: Proxy label)



(|:|) :: (Has label value record, KnownSymbol label, ToJSON value, A.KeyValue res)
  => Proxy label
  -> record
  -> res
(|:|) = encodeField


(|=|) ::
    ( Has label value record
    , KnownSymbol label
    , New.Newtype new value
    , ToJSON new
    , A.KeyValue res
    )
  => Proxy label
  -> record
  -> Proxy new
  -> res
(|=|) = encodeField'


(|.|)
  :: forall label value record res json.
    ( Has label value record
    , KnownSymbol label
    , A.ToJSON json
    , A.KeyValue res
    , json ~ A.Value
    )
  => Proxy label
  -> record
  -> (value -> json)
  -> res
(|.|) = encodeFieldWith



