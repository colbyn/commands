{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
-- {-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE TemplateHaskell #-}
module Commands.Data where


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

-- + Pretty Printing
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import qualified Text.PrettyPrint.Leijen.Text as P




data Ast
  = Fn Text [Ast]
  | Do [Ast]
  | Atom Text
  deriving (Show, Eq, Data, Typeable, Generic)


data AstF a
  = FnF Text [a]
  | DoF [a]
  | AtomF Text
  deriving (Show, Functor, Foldable, Traversable)


type instance Base Ast = AstF

instance R.Recursive Ast where
  project (Fn name xs) = FnF name xs
  project (Do xs) = DoF xs
  project (Atom name) = AtomF name

instance R.Corecursive Ast where
  embed (FnF name xs) = Fn name xs
  embed (DoF xs) = Do xs
  embed (AtomF name) = Atom name









-------------------------------------------------------------------------------
-- Bash
-------------------------------------------------------------------------------

data Bash
  = InitLocal Text (Maybe Bash)
  | Def Text [Bash]
  | Cmd Text [Bash]
  | Var Text
  | Op Bash Text Bash
  | String Text
  | Block [Bash]
  | Subshell [Bash]
  deriving (Show, Eq, Data, Typeable, Generic)


data BashF a
  = InitLocalF Text (Maybe a)
  | DefF Text [a]
  | CmdF Text [a]
  | VarF Text
  | OpF a Text a
  | ArgF Text
  | BlockF [a]
  | SubshellF [a]
  deriving (Show, Functor, Foldable, Traversable)


type instance Base Bash = BashF

instance R.Recursive Bash where
  project (InitLocal l x) = InitLocalF l x
  project (Def l x) = DefF l x
  project (Cmd l [x]) = CmdF l [x]
  project (Var x) = VarF x
  project (Op x name y) = OpF x name y
  project (String x) = ArgF x
  project (Block x) = BlockF x
  project (Subshell x) = SubshellF x

instance R.Corecursive Bash where
  embed (InitLocalF l x) = InitLocal l x
  embed (DefF l x) = Def l x
  embed (CmdF l [x]) = Cmd l [x]
  embed (VarF x) = Var x
  embed (OpF x name y) = Op x name y
  embed (ArgF x) = String x
  embed (BlockF x) = Block x
  embed (SubshellF x) = Subshell x




renderBash :: Bash -> P.Doc
renderBash (String l) = P.dquotes (P.textStrict (bashStringEscapes l))

renderBash (InitLocal label inital) = case inital of
  Nothing               -> "local" <+> P.textStrict label
  Just (String txt)     -> "local" <+> P.textStrict label <> "=" <> P.textStrict txt
  Just   (Var x)        -> "local" <+> P.textStrict label <> "=" <> varSubstitute (P.textStrict x)
  Just x@(Cmd _ _)      -> "local" <+> P.textStrict label <> "=" <> cmdSubstitute (renderBash x)
renderBash (Def name stmts) = P.textStrict name <+> "()" <+> renderBlock stmts'
  where
    stmts' = map renderBash stmts
renderBash (Cmd name args) = renderCall name args
renderBash (Op left name right) = left' <+> name' <+> right'
  where
    name' = P.textStrict name
    left' = renderBash left
    right' = renderBash right
renderBash (Subshell xs) = renderInSubshell $ map renderBash xs



cmdSubstitute :: Doc -> Doc
cmdSubstitute x = "$" <> parens x

varSubstitute :: Doc -> Doc
varSubstitute x = "$" <> x

renderCall :: Text -> [Bash] -> Doc
renderCall name args = name' <+> args'
  where
    to :: Bash -> P.Doc
    to x@(String _) = renderBash x
    to   (Var x) = varSubstitute $ P.textStrict x
    to x@(Cmd _ _) = cmdSubstitute $ renderBash x
    
    name' = P.textStrict name
    args' = map to args
      & P.punctuate P.space
      & P.hcat



renderBlock :: [Doc] -> Doc
renderBlock stmts = "{" <$$> P.indent 4 (P.vcat stmts) <$$> "}" <> P.line

renderInSubshell :: [Doc] -> Doc
renderInSubshell stmts = "(" <$$> P.indent 4 (P.vcat stmts) <$$> ")"



manifest :: [Bash] -> Text
manifest xs = display xs'
  where
    xs' = map renderBash xs
      & P.vcat








-------------------------------------------------------------------------------
-- Misc.
-------------------------------------------------------------------------------

type Mappings = Map.Map CommandPath TargetName

type CommandPath = Text
type TargetName = Text



(\+\) x y = x P.<$> y

renderMode :: Doc -> P.SimpleDoc
renderMode = P.renderPretty 0.8 80

display :: Doc -> Text
display = P.displayTStrict <<< renderMode <<< (<> P.line)


renderPathCaseBranch :: (Text, Text) -> Doc
renderPathCaseBranch (path, name) = k <+> v <+> ";;"
  where
    k = P.parens $ P.dquotes $ P.textStrict path
    v = P.textStrict name


renderPathCases :: Mappings -> Text
renderPathCases = renderPathCases' >>> display


renderPathCases' :: Mappings -> Doc
renderPathCases' xs = "case" <+> "$1" <+> "in" <$$> P.indent 4 body <$$> "esac"
  where
    body = map renderPathCaseBranch (Map.toList xs)
      & P.vcat


bashStringEscapes :: Text -> Text
bashStringEscapes txt = txt
  -- & Text.replace "\\" (Text.pack ['\\', '\\'])
  & Text.replace "\"" (Text.pack ['\\', '"'])
  & Text.replace "?" "\\?"




