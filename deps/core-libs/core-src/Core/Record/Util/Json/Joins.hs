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


-- | TODO: Automate somehow
-- 

module Core.Record.Util.Json.Joins where


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
import Core.Class.Newtype as New
-- ~


-- TODO: Automate somehow
--



{-# ANN module ("HLint: ignore" :: Pre.String) #-}



decodeField
  :: forall l v.
    ( KnownSymbol l
    , A.FromJSON v
    )
  => Proxy l
  -> A.Object
  -> A.Parser (l := v)
decodeField label o = do
  res <- o .: name
  return ( label := res )
  where
    name :: Text
    name = Text.pack $ symbolVal (Proxy :: Proxy l)


decodeField'
  :: forall l v new.
    ( KnownSymbol l
    , New.Newtype new v
    , A.FromJSON new
    )
  => Proxy l
  -> A.Object
  -> Proxy new
  -> A.Parser (l := v)
decodeField' label o _ = do
  val' <- New.unwrap <$> val
  return (label := val')

  where
    val' = New.unwrap <$> val

    val :: A.Parser new
    val = o .: name

    name :: Text
    name = Text.pack $ symbolVal (Proxy :: Proxy l)


decodeFieldWith
  :: forall l v.
    ( KnownSymbol l
    )
  => Proxy l
  -> A.Object
  -> (A.Value -> A.Parser v)
  -> A.Parser (l := v)
decodeFieldWith label o f = do
  val' <- A.explicitParseField f o name
  return (label := val')

  where
    name :: Text
    name = Text.pack $ symbolVal (Proxy :: Proxy l)


join1
  :: A.Parser (l1 := v1) -> A.Parser (l1 := v1)
join1 = id


join2
  :: (A.Parser (l1 := v1), A.Parser (l2 := v2))
  -> A.Parser (l1 := v1, l2 := v2)
join2 (a, b) = do
  a' <- a
  b' <- b
  return (a', b')


join3
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    )
join3 (a, b, c) = do
  a' <- a
  b' <- b
  c' <- c
  return
    (a', b', c')

join4
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    )
join4 (a, b, c, d) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  return
    (a', b', c', d')



join5
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    , A.Parser (l5 := v5)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    )
join5 (a, b, c, d, e) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  e' <- e
  return
    (a', b', c', d', e')


join6
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    , A.Parser (l5 := v5)
    , A.Parser (l6 := v6)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    )
join6 (a, b, c, d, e, f) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  e' <- e
  f' <- f
  return
    (a', b', c', d', e', f')

join7
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    , A.Parser (l5 := v5)
    , A.Parser (l6 := v6)
    , A.Parser (l7 := v7)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    )
join7 (a, b, c, d, e, f, g) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  e' <- e
  f' <- f
  g' <- g
  return
    (a', b', c', d', e', f', g')

join8
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    , A.Parser (l5 := v5)
    , A.Parser (l6 := v6)
    , A.Parser (l7 := v7)
    , A.Parser (l8 := v8)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    , l8 := v8
    )
join8 (a, b, c, d, e, f, g, h) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  e' <- e
  f' <- f
  g' <- g
  h' <- h
  return
    (a', b', c', d', e', f', g', h')



join9
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    , A.Parser (l5 := v5)
    , A.Parser (l6 := v6)
    , A.Parser (l7 := v7)
    , A.Parser (l8 := v8)
    , A.Parser (l9 := v9)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    , l8 := v8
    , l9 := v9
    )
join9 (a, b, c, d, e, f, g, h, i) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  e' <- e
  f' <- f
  g' <- g
  h' <- h
  i' <- i
  return
    (a', b', c', d', e', f', g', h', i')




join10
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    , A.Parser (l5 := v5)
    , A.Parser (l6 := v6)
    , A.Parser (l7 := v7)
    , A.Parser (l8 := v8)
    , A.Parser (l9 := v9)
    , A.Parser (l10 := v10)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    , l8 := v8
    , l9 := v9
    , l10 := v10
    )
join10 (a, b, c, d, e, f, g, h, i, j) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  e' <- e
  f' <- f
  g' <- g
  h' <- h
  i' <- i
  j' <- j
  return
    (a', b', c', d', e', f', g', h', i', j')







join11
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    , A.Parser (l5 := v5)
    , A.Parser (l6 := v6)
    , A.Parser (l7 := v7)
    , A.Parser (l8 := v8)
    , A.Parser (l9 := v9)
    , A.Parser (l10 := v10)
    , A.Parser (l11 := v11)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    , l8 := v8
    , l9 := v9
    , l10 := v10
    , l11 := v11
    )
join11 (a, b, c, d, e, f, g, h, i, j, k) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  e' <- e
  f' <- f
  g' <- g
  h' <- h
  i' <- i
  j' <- j
  k' <- k
  return
    (a', b', c', d', e', f', g', h', i', j', k')













join12
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    , A.Parser (l5 := v5)
    , A.Parser (l6 := v6)
    , A.Parser (l7 := v7)
    , A.Parser (l8 := v8)
    , A.Parser (l9 := v9)
    , A.Parser (l10 := v10)
    , A.Parser (l11 := v11)
    , A.Parser (l12 := v12)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    , l8 := v8
    , l9 := v9
    , l10 := v10
    , l11 := v11
    , l12 := v12
    )
join12 (a, b, c, d, e, f, g, i, u, j, k, l) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  e' <- e
  f' <- f
  g' <- g
  i' <- i
  u' <- u
  j' <- j
  k' <- k
  l' <- l
  return
    (a', b', c', d', e', f', g', i', u', j', k', l')









join13
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    , A.Parser (l5 := v5)
    , A.Parser (l6 := v6)
    , A.Parser (l7 := v7)
    , A.Parser (l8 := v8)
    , A.Parser (l9 := v9)
    , A.Parser (l10 := v10)
    , A.Parser (l11 := v11)
    , A.Parser (l12 := v12)
    , A.Parser (l13 := v13)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    , l8 := v8
    , l9 := v9
    , l10 := v10
    , l11 := v11
    , l12 := v12
    , l13 := v13
    )
join13 (a, b, c, d, e, f, g, i, u, j, k, l, m) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  e' <- e
  f' <- f
  g' <- g
  i' <- i
  u' <- u
  j' <- j
  k' <- k
  l' <- l
  m' <- m
  return
    (a', b', c', d', e', f', g', i', u', j', k', l', m')



join14
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    , A.Parser (l5 := v5)
    , A.Parser (l6 := v6)
    , A.Parser (l7 := v7)
    , A.Parser (l8 := v8)
    , A.Parser (l9 := v9)
    , A.Parser (l10 := v10)
    , A.Parser (l11 := v11)
    , A.Parser (l12 := v12)
    , A.Parser (l13 := v13)
    , A.Parser (l14 := v14)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    , l8 := v8
    , l9 := v9
    , l10 := v10
    , l11 := v11
    , l12 := v12
    , l13 := v13
    , l14 := v14
    )
join14 (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  e' <- e
  f' <- f
  g' <- g
  h' <- h
  i' <- i
  j' <- j
  k' <- k
  l' <- l
  m' <- m
  n' <- n
  return
    (a', b', c', d', e', f', g', h', i', j', k', l', m', n')



join15
  ::
    ( A.Parser (l1 := v1)
    , A.Parser (l2 := v2)
    , A.Parser (l3 := v3)
    , A.Parser (l4 := v4)
    , A.Parser (l5 := v5)
    , A.Parser (l6 := v6)
    , A.Parser (l7 := v7)
    , A.Parser (l8 := v8)
    , A.Parser (l9 := v9)
    , A.Parser (l10 := v10)
    , A.Parser (l11 := v11)
    , A.Parser (l12 := v12)
    , A.Parser (l13 := v13)
    , A.Parser (l14 := v14)
    , A.Parser (l15 := v15)
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    , l8 := v8
    , l9 := v9
    , l10 := v10
    , l11 := v11
    , l12 := v12
    , l13 := v13
    , l14 := v14
    , l15 := v15
    )
join15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = do
  a' <- a
  b' <- b
  c' <- c
  d' <- d
  e' <- e
  f' <- f
  g' <- g
  h' <- h
  i' <- i
  j' <- j
  k' <- k
  l' <- l
  m' <- m
  n' <- n
  o' <- o
  return
    (a', b', c', d', e', f', g', h', i', j', k', l', m', n', o')





