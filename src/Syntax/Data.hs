{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
-- {-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE TemplateHaskell #-}
module Syntax.Data where


import Core
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Foldable
import Data.Traversable
import Data.List ((!!))

import qualified Prelude                      as Pre
import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
import qualified Control.Monad.State          as M
import qualified Control.Monad.Except         as M
import qualified Control.Monad.RWS            as M
import qualified Control.Monad.Identity       as M
import qualified Control.Monad.Reader         as M
import qualified Control.Monad.Writer         as M
import qualified Control.Monad.Trans          as M
import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.String                  as String
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as Char8
import qualified Data.Map                     as Map
import qualified System.Random                as Random

-- + SYB
import Data.Functor.Foldable as R
import Data.Functor.Foldable.TH
import qualified Data.Generics.Uniplate.Data as Uni



-- |
-- TODO: rename to something like 'syntax AST'...
data Ast
  = Fn Text [Ast]
  | Do [Ast]
  | Atom Text
  | Op Ast Text Ast
  deriving (Show, Eq, Data, Typeable, Generic)


data AstF a
  = FnF Text [a]
  | DoF [a]
  | AtomF Text
  | OpF a Text a
  deriving (Show, Functor, Foldable, Traversable)


type instance Base Ast = AstF

instance R.Recursive Ast where
  project (Fn name xs) = FnF name xs
  project (Do xs) = DoF xs
  project (Atom name) = AtomF name
  project (Op l name r) = OpF l name r

instance R.Corecursive Ast where
  embed (FnF name xs) = Fn name xs
  embed (DoF xs) = Do xs
  embed (AtomF name) = Atom name
  embed (OpF l name r) = Op l name r



