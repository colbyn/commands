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
module Core.Json.Utils where


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
import qualified Data.ByteString.Char8        as Char8
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
-- | JSON Decoder Utils
-------------------------------------------------------------------------------


decode :: (A.FromJSON value) => Text -> Either Text value
decode input = case A.eitherDecode serialized of
  Left err -> Left $ Text.pack err 
  Right value -> Right value
  where
    serialized :: LBS.ByteString
    serialized = LBS.fromStrict $ encodeUtf8 input


decode' :: (A.FromJSON value) => Text -> value
decode' input = case A.eitherDecode serialized of
  Left err -> error $ Text.unpack $ Text.unlines
    [ "decode' (unsafe): Failure to parse given input as json."
    , "Given:"
    , " " <> input
    , "Message:"
    , " " <> Text.pack err
    ]
  Right value -> value
  where
    serialized :: LBS.ByteString
    serialized = LBS.fromStrict $ encodeUtf8 input


decodeVia
  :: forall new value.
    ( A.FromJSON new
    , New.Newtype new value
    )
  => Proxy new
  -> Text
  -> Either Text value
decodeVia _ input = result & second New.unwrap
  where
    result :: Either Text new
    result = decode input


decodeVia'
  :: forall new value.
    ( A.FromJSON new
    , New.Newtype new value
    )
  => Proxy new
  -> Text
  -> value
decodeVia' _ input = result & New.unwrap
  where
    result :: new
    result = decode' input

-- | Simple helper functon (based on 'parseJSONList') for parsing lists via the
--  'FromJSON' instance accociated with the given 'Newtype' type.
decodeList
  :: forall value.
      ( A.FromJSON value
      )
  => A.Value
  -> A.Parser [value]
decodeList = A.parseJSONList


-- | Simple helper functon (based on 'parseJSONList') for parsing lists via the
--  'FromJSON' instance accociated with the given 'Newtype' type.
decodeListVia
  :: forall value new.
      ( New.Newtype new value
      , A.FromJSON new
      )
  => Proxy new
  -> A.Value
  -> A.Parser [value]
decodeListVia _ input = result <&> (List.map New.unwrap)
  where
    result :: A.Parser [new]
    result = A.parseJSONList input



-------------------------------------------------------------------------------
-- | JSON Encoder Utils
-------------------------------------------------------------------------------


encodeList :: A.ToJSON value => [value] -> A.Value
encodeList = A.listValue A.toJSON


encodeListVia
  :: forall new value.
    ( A.ToJSON new
    , New.Newtype new value
    )
  => Proxy new
  -> [value]
  -> A.Value
encodeListVia _ = encodeList . go
  where
    go :: [value] -> [new]
    go = map New.wrap

