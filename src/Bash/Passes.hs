module Bash.Passes (
    runCompiler
) where

import Core
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

-- + OS Apis
import qualified System.IO      as Sys
import qualified System.Exit    as Sys
import qualified System.Process as SP
import qualified System.IO.Temp as Temp

-- + Cli
-- import qualified System.Console.Haskeline as H

-- + Dev
import qualified Text.Show.Prettyprint as PP

-- + SYB
import Data.Functor.Foldable as R
import Data.Functor.Foldable.TH
import qualified Data.Generics.Uniplate.Data as Uni


-- + Local
import qualified Syntax.Data   as Syntax
import qualified Syntax.Passes as Syntax
import Bash.Data




-------------------------------------------------------------------------------
-- Compiler
-------------------------------------------------------------------------------

runCompiler :: IO (Either Text (Text, Mappings))
runCompiler = do
  result <- Syntax.compiler
  case result of
    Left err -> return $ Left err
    Right ast -> Right <$> compile ast


-- |
-- Helper for 'runCompiler'.
compile :: [Syntax.Ast] -> IO (Text, Mappings)
compile ast = do
  let (bash, env) = convert ast
  out <- rendered env <$> passes bash
  return (out, env)
  where
    rendered :: Mappings -> [Bash] -> Text
    rendered env body =
          Text.append header (manifest body)
        & (`Text.append` footer)
      where
        header = "set -e;\n\n"
        footer = Text.append "\n" (renderPathCases env)




-------------------------------------------------------------------------------
-- Dev
-------------------------------------------------------------------------------

dryRun :: IO ()
dryRun = do
  parsed <- Syntax.compiler
  case parsed of
    Left err -> Text.putStrLn err
    Right ast -> do
      let (bash, env) = convert ast
      out <- passes bash
      mapM_ PP.prettyPrint out




-------------------------------------------------------------------------------
-- Convert
-------------------------------------------------------------------------------


convert :: [Syntax.Ast] -> ([Bash], Mappings)
convert xs =
  let
    (ss, _, fns) = M.runRWS (mapM gen xs) () 0
  in
    fixes (fns <> ss)
  where
    fixes :: [Bash] -> ([Bash], Mappings)
    fixes inp = M.runWriter (Uni.transformBiM f inp)
      where
        f :: Bash -> M.Writer (Map.Map Text Text) Bash
        f (Cmd "command" (String path : etc)) = do
          let name = Text.replace "::" "___" path
          let site = Def name etc
          M.tell $ Map.singleton path name
          return site
        
        f x = return x


-- |
-- Helper for 'convert'
gen :: Syntax.Ast -> Env Bash
gen = cataM f
  where
    emit :: [Bash] -> Env Text
    emit stmts = do
      name <- freshIdent
      M.tell $ pure $ Def name stmts
      return name
    
    f :: Syntax.AstF Bash -> Env Bash
    f (Syntax.DoF xs) = do
      name <- emit xs
      return $ Cmd name []
    f (Syntax.FnF name args) = return $ Cmd name args
    f (Syntax.AtomF name) = return $ String name
    f (Syntax.OpF l sym r) = return $ Op l sym r




-------------------------------------------------------------------------------
-- Root Transformations
-------------------------------------------------------------------------------

passes :: [Bash] -> IO [Bash]
passes xs = (rewritesPure xs & rewritesIO) <&> finalize



rewritesIO :: [Bash] -> IO [Bash]
rewritesIO = Uni.transformBiM fromMacro

rewritesPure :: [Bash] -> [Bash]
rewritesPure xs = Uni.transformBi voidMacro xs
  & Uni.transformBi runMacro
  & Uni.transformBi bindMacro
  & Uni.transformBi opsMacro

-- | Run before rendering into text.
finalize :: [Bash] -> [Bash]
finalize = Uni.transformBi inlineBlocksMacro



-------------------------------------------------------------------------------
-- Bash Individual Rewrites
-------------------------------------------------------------------------------


fromMacro :: Bash -> IO Bash
fromMacro (Cmd "from" [String path, Cmd name []]) = do
  fnName <- randomFinallyName
  pwdVar <- randomName
  return $ Subshell
    [ InitLocal pwdVar $ Just $ Cmd "pwd" []
    , Def fnName [Cmd "cd" [Var pwdVar]]
    , Cmd "trap" [String fnName, String "EXIT"]
    , Cmd "cd" [String path]
    , Cmd name []
    ]
fromMacro x = return x


voidMacro :: Bash -> Bash
voidMacro (Cmd "void" [block]) = Op block "||" (String "true")
voidMacro x = x



inlineBlocksMacro :: Bash -> Bash
inlineBlocksMacro (Def name body) = Def name (inline [] body)
  where
    inline :: [Bash] -> [Bash] -> [Bash]
    inline xs [] = xs
    inline xs (y:ys) = case y of
      Block ss -> inline (xs <> ss) ys
      _ -> inline (xs <> [y]) ys
inlineBlocksMacro x = x



runMacro :: Bash -> Bash
runMacro (Cmd "run" (String path : args)) = 
  Cmd (Text.pack ['\"'] <> bashStringEscapes path <> Text.pack ['\"']) args
runMacro (Cmd "run_" (String path : args)) = 
  Op (Cmd (Text.pack ['\"'] <> bashStringEscapes path <> Text.pack ['\"']) args)
    "&>"
    (String "/dev/null")
runMacro x = x


bindMacro :: Bash -> Bash
bindMacro (Cmd name (String "<-" : String cmd : args)) =
  InitLocal name $ Just $ Cmd cmd args
bindMacro x = x


-- streamFnsMacro :: Bash -> Bash
-- streamFnsMacro = 

opsMacro :: Bash -> Bash
opsMacro (Op l "|" r) = Op l "|" r
opsMacro x = x




-------------------------------------------------------------------------------
-- Misc.
-------------------------------------------------------------------------------



cataM :: (Traversable (Base t), Monad m, Recursive t) => (Base t c -> m c) -> t -> m c
cataM = R.cata . (sequence M.>=>)

type Env a = M.RWS () [Bash] Int a


randomName :: IO Text
randomName = Text.append "ID_" . Text.pack . show <$> f
  where
    f :: IO Word32
    f = Random.randomIO

randomFinallyName :: IO Text
randomFinallyName = Text.append "finally_subroutine_" . Text.pack . show <$> f
  where
    f :: IO Word32
    f = Random.randomIO



freshIdent :: Env Text
freshIdent = do
  ix <- M.get
  M.put (ix + 1)
  return $ pack (letters !! ix)
  where
    prefix :: Text
    prefix = Text.pack "_subroutine_"

    label :: Text -> Text
    label x = prefix `Text.append` x

    pack :: Text -> Text
    pack x = prefix `Text.append` x

    letters :: [Text]
    letters = Text.pack <$> ([1..] >>= flip M.replicateM ['a'..'z'])




