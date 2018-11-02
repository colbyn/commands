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
module Core.Record.Util.Json.OldJoins (
    join1
  , join2
  , join3
  , join4
  , join5
  , join6
  , join7
  , join8
  , join9
  , join10
  , join11
  , join12
  , join13
  , join14
  , join15
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
import Core.Class.Newtype as New
-- ~


-- TODO: Automate somehow
--



{-# ANN module ("HLint: ignore" :: Pre.String) #-}






-- instance (A.FromJSON v, KnownSymbol l, FromJSON v)  => FieldDecodeable v where
--   decodeField (label := o) = do
--     res <- o .: name
--     return ( label := res )
--     where
--       name :: Text
--       name = Text.pack $ symbolVal (Proxy :: Proxy l)
-- 
-- 
-- instance (KnownSymbol l, New.Newtype new v, A.FromJSON new)  => FieldDecodeable v where
  -- decodeField (key := o) = do
  --   val' <- New.unwrap <$> val
  --   return (key := val')
  -- 
  --   where
  --     val' = New.unwrap <$> val
  -- 
  --     val :: A.Parser new
  --     val = o .: name
  -- 
  --     name :: Text
  --     name = Text.pack $ symbolVal (Proxy :: Proxy l)



-- decodeFields
--   ::
--     ( Cons label value record
--     )
--   -> [(label := A.Object)]
--   -> record
-- decodeFields xs

-- decodeFields'
--   :: forall label value record inp.
--     ( Cons label value record
--     , FieldDecodeable value
--     , inp ~ (label := A.Object)
--     )
--   => [label := value]
--   -> record
--   -> A.Parser record
-- decodeFields' [] r = return r
-- decodeFields' (x:xs) r = do
--   x' <- decodeField x
--   return (x' cons r)


step :: forall l v.
    ( KnownSymbol l
    , A.FromJSON v
    )
  => (l := A.Object)
  -> A.Parser (l := v)
step (label := o) = do
  res <- o .: name
  return ( label := res )
  where
    name :: Text
    name = Text.pack $ symbolVal (Proxy :: Proxy l)



join1
  :: forall l1 v1.
    ( KnownSymbol l1
    , A.FromJSON v1
    )
  => ( l1 := A.Object )
  -> A.Parser (l1 := v1)
join1 = step

join2
  :: forall l1 v1 l2 v2.
    ( KnownSymbol l1
    , KnownSymbol l2
    , A.FromJSON v1
    , A.FromJSON v2
    )
  => ( l1 := A.Object, l2 := A.Object)
  -> A.Parser (l1 := v1, l2 := v2)
join2 (a, b) = do
  a' <- step a
  b' <- step b
  return (a', b')


join3
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    )
join3 (a, b, c) = do
  a' <- step a
  b' <- step b
  c' <- step c
  return (a', b', c')


join4
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    )
join4 (a, b, c, d) = do
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  return (a', b', c', d')

join5
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , KnownSymbol l5
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    , A.FromJSON v5
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    , l5 := A.Object
    )
  -> A.Parser
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    )
join5 (a, b, c, d, e) = do
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  e' <- step e
  return (a', b', c', d', e')

join6
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , KnownSymbol l5
    , KnownSymbol l6
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    , A.FromJSON v5
    , A.FromJSON v6
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    , l5 := A.Object
    , l6 := A.Object
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
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  e' <- step e
  f' <- step f
  return (a', b', c', d', e', f')


join7
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , KnownSymbol l5
    , KnownSymbol l6
    , KnownSymbol l7
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    , A.FromJSON v5
    , A.FromJSON v6
    , A.FromJSON v7
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    , l5 := A.Object
    , l6 := A.Object
    , l7 := A.Object
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
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  e' <- step e
  f' <- step f
  g' <- step g
  return (a', b', c', d', e', f', g')

join8
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , KnownSymbol l5
    , KnownSymbol l6
    , KnownSymbol l7
    , KnownSymbol l8
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    , A.FromJSON v5
    , A.FromJSON v6
    , A.FromJSON v7
    , A.FromJSON v8
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    , l5 := A.Object
    , l6 := A.Object
    , l7 := A.Object
    , l8 := A.Object
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
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  e' <- step e
  f' <- step f
  g' <- step g
  h' <- step h
  return (a', b', c', d', e', f', g', h')

join9
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , KnownSymbol l5
    , KnownSymbol l6
    , KnownSymbol l7
    , KnownSymbol l8
    , KnownSymbol l9
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    , A.FromJSON v5
    , A.FromJSON v6
    , A.FromJSON v7
    , A.FromJSON v8
    , A.FromJSON v9
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    , l5 := A.Object
    , l6 := A.Object
    , l7 := A.Object
    , l8 := A.Object
    , l9 := A.Object
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
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  e' <- step e
  f' <- step f
  g' <- step g
  h' <- step h
  i' <- step i
  return (a', b', c', d', e', f', g', h', i')

join10
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , KnownSymbol l5
    , KnownSymbol l6
    , KnownSymbol l7
    , KnownSymbol l8
    , KnownSymbol l9
    , KnownSymbol l10
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    , A.FromJSON v5
    , A.FromJSON v6
    , A.FromJSON v7
    , A.FromJSON v8
    , A.FromJSON v9
    , A.FromJSON v10
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    , l5 := A.Object
    , l6 := A.Object
    , l7 := A.Object
    , l8 := A.Object
    , l9 := A.Object
    , l10 := A.Object
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
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  e' <- step e
  f' <- step f
  g' <- step g
  h' <- step h
  i' <- step i
  j' <- step j
  return (a', b', c', d', e', f', g', h', i', j')



join11
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , KnownSymbol l5
    , KnownSymbol l6
    , KnownSymbol l7
    , KnownSymbol l8
    , KnownSymbol l9
    , KnownSymbol l10
    , KnownSymbol l11
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    , A.FromJSON v5
    , A.FromJSON v6
    , A.FromJSON v7
    , A.FromJSON v8
    , A.FromJSON v9
    , A.FromJSON v10
    , A.FromJSON v11
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    , l5 := A.Object
    , l6 := A.Object
    , l7 := A.Object
    , l8 := A.Object
    , l9 := A.Object
    , l10 := A.Object
    , l11 := A.Object
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
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  e' <- step e
  f' <- step f
  g' <- step g
  h' <- step h
  i' <- step i
  j' <- step j
  k' <- step k
  return (a', b', c', d', e', f', g', h', i', j', k')




join12
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , KnownSymbol l5
    , KnownSymbol l6
    , KnownSymbol l7
    , KnownSymbol l8
    , KnownSymbol l9
    , KnownSymbol l10
    , KnownSymbol l11
    , KnownSymbol l12
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    , A.FromJSON v5
    , A.FromJSON v6
    , A.FromJSON v7
    , A.FromJSON v8
    , A.FromJSON v9
    , A.FromJSON v10
    , A.FromJSON v11
    , A.FromJSON v12
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    , l5 := A.Object
    , l6 := A.Object
    , l7 := A.Object
    , l8 := A.Object
    , l9 := A.Object
    , l10 := A.Object
    , l11 := A.Object
    , l12 := A.Object
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
join12 (a, b, c, d, e, f, g, h, i, j, k, l) = do
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  e' <- step e
  f' <- step f
  g' <- step g
  h' <- step h
  i' <- step i
  j' <- step j
  k' <- step k
  l' <- step l
  return (a', b', c', d', e', f', g', h', i', j', k', l')





join13
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , KnownSymbol l5
    , KnownSymbol l6
    , KnownSymbol l7
    , KnownSymbol l8
    , KnownSymbol l9
    , KnownSymbol l10
    , KnownSymbol l11
    , KnownSymbol l12
    , KnownSymbol l13
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    , A.FromJSON v5
    , A.FromJSON v6
    , A.FromJSON v7
    , A.FromJSON v8
    , A.FromJSON v9
    , A.FromJSON v10
    , A.FromJSON v11
    , A.FromJSON v12
    , A.FromJSON v13
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    , l5 := A.Object
    , l6 := A.Object
    , l7 := A.Object
    , l8 := A.Object
    , l9 := A.Object
    , l10 := A.Object
    , l11 := A.Object
    , l12 := A.Object
    , l13 := A.Object
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
join13 (a, b, c, d, e, f, g, h, i, j, k, l, m) = do
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  e' <- step e
  f' <- step f
  g' <- step g
  h' <- step h
  i' <- step i
  j' <- step j
  k' <- step k
  l' <- step l
  m' <- step m
  return (a', b', c', d', e', f', g', h', i', j', k', l', m')





join14
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , KnownSymbol l5
    , KnownSymbol l6
    , KnownSymbol l7
    , KnownSymbol l8
    , KnownSymbol l9
    , KnownSymbol l10
    , KnownSymbol l11
    , KnownSymbol l12
    , KnownSymbol l13
    , KnownSymbol l14
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    , A.FromJSON v5
    , A.FromJSON v6
    , A.FromJSON v7
    , A.FromJSON v8
    , A.FromJSON v9
    , A.FromJSON v10
    , A.FromJSON v11
    , A.FromJSON v12
    , A.FromJSON v13
    , A.FromJSON v14
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    , l5 := A.Object
    , l6 := A.Object
    , l7 := A.Object
    , l8 := A.Object
    , l9 := A.Object
    , l10 := A.Object
    , l11 := A.Object
    , l12 := A.Object
    , l13 := A.Object
    , l14 := A.Object
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
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  e' <- step e
  f' <- step f
  g' <- step g
  h' <- step h
  i' <- step i
  j' <- step j
  k' <- step k
  l' <- step l
  m' <- step m
  n' <- step n
  return (a', b', c', d', e', f', g', h', i', j', k', l', m', n')






join15
  :: forall
    l1 v1
    l2 v2
    l3 v3
    l4 v4
    l5 v5
    l6 v6
    l7 v7
    l8 v8
    l9 v9
    l10 v10
    l11 v11
    l12 v12
    l13 v13
    l14 v14
    l15 v15
  .
    ( KnownSymbol l1
    , KnownSymbol l2
    , KnownSymbol l3
    , KnownSymbol l4
    , KnownSymbol l5
    , KnownSymbol l6
    , KnownSymbol l7
    , KnownSymbol l8
    , KnownSymbol l9
    , KnownSymbol l10
    , KnownSymbol l11
    , KnownSymbol l12
    , KnownSymbol l13
    , KnownSymbol l14
    , KnownSymbol l15
    , A.FromJSON v1
    , A.FromJSON v2
    , A.FromJSON v3
    , A.FromJSON v4
    , A.FromJSON v5
    , A.FromJSON v6
    , A.FromJSON v7
    , A.FromJSON v8
    , A.FromJSON v9
    , A.FromJSON v10
    , A.FromJSON v11
    , A.FromJSON v12
    , A.FromJSON v13
    , A.FromJSON v14
    , A.FromJSON v15
    )
  =>
    ( l1 := A.Object
    , l2 := A.Object
    , l3 := A.Object
    , l4 := A.Object
    , l5 := A.Object
    , l6 := A.Object
    , l7 := A.Object
    , l8 := A.Object
    , l9 := A.Object
    , l10 := A.Object
    , l11 := A.Object
    , l12 := A.Object
    , l13 := A.Object
    , l14 := A.Object
    , l15 := A.Object
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
  a' <- step a
  b' <- step b
  c' <- step c
  d' <- step d
  e' <- step e
  f' <- step f
  g' <- step g
  h' <- step h
  i' <- step i
  j' <- step j
  k' <- step k
  l' <- step l
  m' <- step m
  n' <- step n
  o' <- step o
  return (a', b', c', d', e', f', g', h', i', j', k', l', m', n', o')

