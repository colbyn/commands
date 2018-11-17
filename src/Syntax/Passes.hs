module Syntax.Passes (
    compiler
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
import Syntax.Data
import Syntax.Parser (parseContents)



compiler :: IO (Either Text [Ast])
compiler = do
  result <- parseContents "./Commands"
  case result of
    Left err -> return $ Left err
    Right ast -> return $ Right $ postparser ast


-------------------------------------------------------------------------------
-- Dev
-------------------------------------------------------------------------------

dryRun :: IO ()
dryRun = do
  result <- compiler
  case result of
    Left err -> Text.putStrLn err
    Right xs ->
      mapM_ PP.prettyPrint xs





-------------------------------------------------------------------------------
-- Post Parser transformations
-------------------------------------------------------------------------------


postparser :: [Ast] -> [Ast]
postparser =
  -- We don't want to apply this to top-level items
  map (Uni.transform opsMacro)


opsMacro :: Ast -> Ast
opsMacro (Fn name args)
  | hasOps args = fixOpsInArgs name args
  | otherwise   = Fn name args
opsMacro x = x


fixOpsInArgs :: Text -> [Ast] -> Ast
fixOpsInArgs fnName xs
  | hasOps xs = case List.break f xs of
    (args, Atom sym : rest) | isOp sym -> Op (Fn fnName args) sym (toFn rest)
  where
    f (Atom x) = isOp x
    f _ = False
    
    toFn :: [Ast] -> Ast
    toFn [Atom name] = Fn name []
    toFn [x] = x
    toFn (Atom name : args)
      | hasOps args = fixOpsInArgs name args
      | otherwise   = Fn name args




hasOps :: [Ast] -> Bool
hasOps = List.any f
  where
    f (Atom x) = isOp x
    f _ = False

isOp :: Text -> Bool
isOp "|" = True
isOp _ = False









