{-# LANGUAGE QuasiQuotes #-}
module Commands.System where


import Core
import NeatInterpolation (text)

import qualified Prelude                      as Pre
import qualified Control.Monad                as M
import qualified Control.Monad.IO.Class       as M
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

-- + OS Apis
import qualified System.IO          as Sys
import qualified System.Exit        as Sys
import qualified System.Environment as SE
import qualified System.Process     as SP
import qualified System.IO.Temp     as Temp

-- + Cli
-- import qualified System.Console.Haskeline as H

-- + Dev
import qualified Text.Show.Prettyprint as PP

-- + SYB
import Data.Functor.Foldable as R


-- + Local
import Commands.Data
import Commands.Compile (runCompiler)




{-# ANN module ("HLint: ignore" :: Pre.String) #-}



main :: IO ()
main = do
  args <- SE.getArgs <&> map Text.pack
  runCli args


runCli :: [Text] -> IO ()
runCli args = case args of
  ["--bash-completion-script", "cmd"] -> Text.putStrLn autocompleteScript
  ("--bash-completion-index" : index : rest) -> do
    result <- runCompiler
    case result of
      Left err -> return ()
      Right (out, env) -> runPathAutocompletion env rest
  _ -> do
    result <- runCompiler
    case result of
      Left err -> Sys.hPrint Sys.stderr err
      Right (out, env) -> invoke (out, env) (Text.intercalate "::" args)



-------------------------------------------------------------------------------
-- Invoke Command
-------------------------------------------------------------------------------


invoke :: (Text, Mappings) -> Text -> IO ()
invoke (out, env) path = do
  Text.writeFile ".commands.out.sh" out
  run $ "bash .commands.out.sh " <> path
  where
    go :: Stdout -> Stderr -> Sys.ExitCode -> IO ()
    go stdout stderr Sys.ExitSuccess = return ()
    go stdout stderr (Sys.ExitFailure code) = return ()






-------------------------------------------------------------------------------
-- Autocomplete
-------------------------------------------------------------------------------


runPathAutocompletion :: Mappings -> [Text] -> IO ()
runPathAutocompletion env raw = do
  mapM_ Text.putStrLn options
  where
    ("cmd":inpaths) = List.filter (/= "--bash-completion-word") raw
    inpath = Text.intercalate "::" inpaths
    
    options = List.filter (Text.isPrefixOf inpath) (Map.keys env)


autocompleteScript :: Text
autocompleteScript = [text|
  _cmd()
  {
      local CMDLINE
      local IFS=$$'\n'
      CMDLINE=(--bash-completion-index $$COMP_CWORD)

      for arg in $${COMP_WORDS[@]}; do
          CMDLINE=($${CMDLINE[@]} --bash-completion-word $$arg)
      done

      COMPREPLY=( $$(cmd "$${CMDLINE[@]}") )
  }
  complete -o filenames -F _cmd cmd
|]



-------------------------------------------------------------------------------
-- Misc. Helpers
-------------------------------------------------------------------------------


type Command           = Text
type ExitInt           = Int
type Stderr            = Text
type Stdout            = Text
type ProcessOutput     = (Stdout, Stderr, Sys.ExitCode)


run :: Command -> IO ()
run cli = SP.withCreateProcess cmd block
  where
    block :: Maybe Sys.Handle -> Maybe Sys.Handle -> Maybe Sys.Handle -> SP.ProcessHandle -> IO ()
    block _ _ _ handle = do
      exit <- SP.waitForProcess handle
      Sys.exitWith exit
    
    cmd :: SP.CreateProcess
    cmd = (SP.shell $ Text.unpack cli)
      { SP.std_in = SP.Inherit
      , SP.std_err = SP.Inherit
      , SP.std_out = SP.Inherit
      }


