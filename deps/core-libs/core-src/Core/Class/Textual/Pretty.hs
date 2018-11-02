{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE PolyKinds #-}
module Core.Class.Textual.Pretty (
  -- Basic combinators
  -- Alignment
  -- Operators
    (<+>)
  , (</>)
  -- List combinators
  , pun
  , endWith
  , startWidth
  , sep
  , hcat
  , vcat
  -- Fillers
  -- Bracketing combinators
  , enclose
  , squotes
  , dquotes
  , parens
  , angles
  , braces
  , brackets
  -- Character documents
  -- Primitive type documents
  , int
  , condInt
  -- Debugging
  -- Builder
) where


-- ~
import Core hiding (print)
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
import Data.String
  ( IsString(..)
  )
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
  , (<>)
  , Applicative(..)
  , Monad(..)
  , Semigroup(..)
  , Functor(..)
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
  ( (.:)
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
import qualified Data.Text.Encoding         as Encoding


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
import qualified GHC.TypeLits as TL


-- + Local
-- ~



type Textual a = (IsString a, Semigroup a, Monoid a)




-------------------------------------------------------------------------------
-- | Basic combinators
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- | Alignment
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- | Operators
-------------------------------------------------------------------------------


(<+>) :: Textual a => a -> a -> a
(<+>) l r = l <> " " <> r


(</>) :: Textual a => a -> a -> a
(</>) l r = (l <> "\n") <> (r <> "\n")


infixr 0 <+>
infixr 0 </>


-------------------------------------------------------------------------------
-- | List combinators
-------------------------------------------------------------------------------

pun :: Textual a => a -> [a] -> [a]
pun p [] = []
pun p [x] = [x]
pun p (x:xs) = (x <> p) : pun p xs

endWith :: Textual a => a -> [a] -> [a]
endWith _ [] = []
endWith p (x:xs) = (x <> p) : endWith p xs


startWidth :: Textual a => a -> [a] -> [a]
startWidth p xs = go xs []
  where
    go [] xs = xs
    go ys xs = Fold.foldl (\ xs y -> xs ++ [p <> y]) xs ys


-- |
-- Concatenates horizontally with (<+>).
sep :: forall a. Textual a => [a] -> a
sep = pun (" " :: a) >>> Mon.mconcat


-- |
-- Concatenates horizontally with (<>).
hcat :: Textual a => [a] -> a
hcat = Mon.mconcat


-- |
-- Concatenates vertically with (<>) and a newline.
vcat :: forall a. Textual a => [a] -> a
vcat = endWith ("\n" :: a) >>> Mon.mconcat



-------------------------------------------------------------------------------
-- | Fillers
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Bracketing combinators
-------------------------------------------------------------------------------

type Left = ByteString
type Right = ByteString
type Center = ByteString


-- |
-- The document (enclose l r x) encloses document x between documents l and r using (<>).
enclose :: Textual a => a -> a -> a -> a
enclose l r c = l <> c <> r

-- |
-- Document (squotes x) encloses document x with single quotes """,
squotes :: forall a. Textual a => a -> a
squotes x = (squote :: a) <> x <> (squote :: a)


-- |
-- Document (squotes x) encloses document x with single quotes """.
dquotes :: forall a. Textual a => a -> a
dquotes x = (dquote :: a) <> x <> (dquote :: a)


-- |
-- Document (parens x) encloses document x in parenthesis, "(" and ")".
parens :: forall a. Textual a => a -> a
parens x = ("(" :: a) <> x <> (")" :: a)

-- |
-- Document (angles x) encloses document x in angles, "<" and ">".
angles :: forall a. Textual a => a -> a
angles x = "<" <> x <> ">"

-- |
-- Document (braces x) encloses document x in braces, "{" and "}".
braces :: forall a. Textual a => a -> a
braces x = (lbrace :: a) <> x <> (rbrace :: a)

-- |
-- Document (brackets x) encloses document x in square brackets, "[" and "]".
brackets :: forall a. Textual a => a -> a
brackets x = (lbracket :: a) <> x <> (rbracket :: a)





-------------------------------------------------------------------------------
-- | Character documents
-------------------------------------------------------------------------------


squote :: forall a. Textual a => a
squote = "'" :: a

dquote :: forall a. Textual a => a
dquote = "\"" :: a

lparen :: forall a. Textual a => a
lparen = "(" :: a

rparen :: forall a. Textual a => a
rparen = ")" :: a

langle :: forall a. Textual a => a
langle = "<" :: a

rangle :: forall a. Textual a => a
rangle = ">" :: a

lbrace :: forall a. Textual a => a
lbrace = "{" :: a

rbrace :: forall a. Textual a => a
rbrace = "}" :: a

lbracket :: forall a. Textual a => a
lbracket = "[" :: a

rbracket :: forall a. Textual a => a
rbracket = "]" :: a



-------------------------------------------------------------------------------
-- | Primitive type documents
-------------------------------------------------------------------------------

type Default = ByteString


int :: Textual a => Int -> a
int = show >>> fromString


condInt :: (Int -> Bool) -> Default -> Int -> ByteString
condInt p def x
  | p x = int x
  | otherwise = def




-------------------------------------------------------------------------------
-- | Debugging
-------------------------------------------------------------------------------

-- print :: ByteString -> IO ()
-- print = Text.putStrLn <<< Encoding.decodeUtf8




-------------------------------------------------------------------------------
-- | Dev
-------------------------------------------------------------------------------

