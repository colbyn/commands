module Syntax.Parser (
    parseContents
) where


import Core
import NeatInterpolation (text)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Void
import Control.Applicative (empty, Alternative)
import Control.Monad (join)

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
import qualified Data.Set                     as Set

-- + OS Apis
import qualified System.IO      as Sys
import qualified System.Exit    as Sys
import qualified System.Process as SP
import qualified System.IO.Temp as Temp

-- + Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error

-- + Ast
import Data.Generics.Uniplate.Data as Uni

-- + Dev
import qualified Text.Show.Prettyprint as PP

-- + Local
import Syntax.Data






parseContents :: Pre.FilePath -> IO (Either Text [Ast])
parseContents filename = go . (++ "\n") <$> Sys.readFile filename
  where
    go file = runParser parser filename file
      & first (Text.pack . parseErrorPretty)
      & second postProcess





-------------------------------------------------------------------------------
-- Basics
-------------------------------------------------------------------------------

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty


sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: String -> Parser String
symbol = L.symbol sc



-------------------------------------------------------------------------------
-- Syntax
-------------------------------------------------------------------------------

parser :: Parser [Ast]
parser = someTill (scn *> fnP <* scn) eof
  -- where
  --   item =
  --         try (eof >> return Nothing)
  --     <|> ((fnP <* scn) <&> Just)


atomP :: Parser Ast
atomP = do
  result <- go
  if Text.strip result == "do"
    then fancyFailure $ Set.fromList
      [ ErrorFail "do is a reserved word."
      ]
    else return $ Atom result
  where
    go = Text.pack <$> some (satisfy (not . Char.isSpace))




-- commentP :: Parser Ast
-- commentP = do
--   symbol "#"
--   xs <- Text.pack <$> some (satisfy ((/=) '\n'))
--   return $ Comment xs



literalP :: Parser Ast
literalP = try multilineStringLiteral <|> stringLiteral


stringLiteral :: Parser Ast
stringLiteral = singleLine
  where
    singleLine = (tk >> manyTill L.charLiteral tk) <&> (Atom . Text.pack)
    tk = char '"'

multilineStringLiteral :: Parser Ast
multilineStringLiteral = do
  try sc
  pos <- L.indentLevel
  symbol "\"\"\""
  body <- manyTill anyChar (atEnd pos)
  return $ Atom $ format pos body
  where
    format :: Pos -> String -> Text
    format pos x = Text.pack x
      & Text.lines
      & map (trim pos)
      & List.filter (not . isNull)
      & Text.unlines
      & Text.stripEnd
    
    isNull :: Text -> Bool
    isNull x = Text.null x || Text.all Char.isSpace x
    
    leading pos = Text.pack $ List.replicate (unPos pos - 1) ' '
    
    trim :: Pos -> Text -> Text
    trim pos x = Maybe.fromMaybe x (Text.stripPrefix (leading pos) x)
    
    atEnd pos = do
      current <- L.indentLevel
      if pos == current
        then symbol "\"\"\""
        else M.fail "not yet"




fnP :: Parser Ast
fnP = do
  (name, args) <- indent Flexible Nothing intro outro
  return $ Fn name args
  where
    intro = do
      Atom name <- atomP
      
      if Text.all Char.isAscii name
        then return name
        else fancyFailure $ Set.fromList
          [ ErrorFail "Function/command names (currently) must all be valid ASCI characters (TODO is this necessary?)."
          ]
    outro = (try literalP <|> try doP <|> atomP) <* try sc





doP :: Parser Ast
doP = do
  ref <- lookAhead nextNewline
  (_, args) <- indent Strict (fmap dec ref) (symbol "do" <* notFollowedBy (satisfy (not . Char.isSpace))) outro
  let body = join args
  if null body
    then fancyFailure $ Set.fromList
      [ ErrorFail "do block is empty"
      ]
    else return $ Do body
  where
    dec :: Pos -> Pos
    dec x = mkPos (unPos x - 1)
    
    outro = some fnP



-------------------------------------------------------------------------------
-- Misc. Helpers
-------------------------------------------------------------------------------


divBy :: (Alternative m, Monad m, M.MonadPlus m) => m a -> m sep -> m [a]
divBy p sep = do
  x <- p
  xs <- some (sep *> p)
  return $ x : xs


nextNewline :: Parser (Maybe Pos)
nextNewline = optional $ do
    void go
    sc'
    L.indentLevel
  where
    go = manyTill anyChar (satisfy pred)
    pred = (==) '\n'
    
    sc' :: Parser ()
    sc' = L.space (void $ takeWhile1P Nothing f) empty empty
      where
        f x = x == ' ' || x == '\t'


contentBeforeNextNewline :: Parser String
contentBeforeNextNewline = go
  where
    go = manyTill anyChar (satisfy pred)
    pred = (==) '\n'



-- |
-- Strict:
-- * All items are expected to be indented equally.
-- Flexible:
-- * All items are expected to be indented equally, or greater than the reference point.
-- 
-- The reference point is defined by the beginning position of the first parser.
--
data AlignmentMode
  = Strict
  | Flexible


indent
  :: AlignmentMode
  -> Maybe Pos
  -> Parser a
  -> Parser b
  -> Parser (a, [b])
indent mode pos parserA parserB = do
  sc
  ref <- L.indentLevel
  intro <- parserA
  outro <- indented (Maybe.fromMaybe ref pos)
  return (intro, M.join outro)
  where
    indented lvl = indentedHandler lvl mode parserB


indentedHandler
  :: Pos
  -> AlignmentMode
  -> Parser b
  -> Parser [[b]]
indentedHandler ref mode p =
  setup
  where
    check Nothing = L.indentGuard scn GT ref
    check (Just pos) = L.indentGuard scn EQ pos
    consume = manyTill p (lookAhead eol)
    
    go setting = do
        flag <- (optional . try) (check setting)
        case flag of
          Just _ -> (:) <$> consume <*> go setting
          _ -> return []
    
    setup = do
      flag <- (optional . try) (check Nothing)
      case (mode, flag) of
        (Flexible, Just _) -> go Nothing
        (Strict, override@(Just _)) -> go override
        (_, Nothing) -> return []




-------------------------------------------------------------------------------
-- Post Processing
-------------------------------------------------------------------------------

postProcess :: [Ast] -> [Ast]
postProcess = id





-------------------------------------------------------------------------------
-- Dev
-------------------------------------------------------------------------------


-- parseContentsTestVia filename = go <$> Sys.readFile filename
--   where
--     go file = runParser p filename file
--       & first (Text.pack . parseErrorPretty)
-- 
--     p = multilineStringLiteral
-- 

run :: IO ()
run = do
  result <- parseContents "./Commands"
  case result of
    Left err -> Text.putStrLn err
    Right x -> mapM_ PP.prettyPrint x


