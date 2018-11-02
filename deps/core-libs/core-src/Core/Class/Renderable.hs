-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
module Core.Class.Renderable (
    module Text.PrettyPrint.Leijen.Text
  , Renderable(..)
  -- Primitive Utilities (in addition to @Text.PrettyPrint.Leijen.Text@.)
  , lineSep
  , (\+\)
  , (\++\)
  , punLeft
  -- Output/Display Utils
  , display
  , logIO
  , logIO'
) where


import Core hiding ((<$>))
import Data.Time (UTCTime)
import GHC.TypeLits (symbolVal)

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
import qualified Data.Time                    as Time


-- + Serialization
import Data.UUID.Types (UUID)
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID

-- + Pretty Printing
import Text.PrettyPrint.Leijen.Text hiding ((<$>), Pretty)
import qualified Text.PrettyPrint.Leijen.Text as P hiding (Pretty)

-- + Local
import Core.Record as R



-- packDoc :: P.Doc -> Text
-- packDoc doc =
--     P.displayTStrict toSimpleDoc
--     where
--         toSimpleDoc = P.renderPretty 0.4 400 doc


class Renderable a where
  manifest :: a -> P.Doc


-- + --------------------------------------------------------------------------
-- + Primitive Utilities (in addition to @Text.PrettyPrint.Leijen.Text@.)
-- + --------------------------------------------------------------------------


-- | Prefix alternative to @(Text.PrettyPrint.Leijen.<$>)@.
lineSep :: Doc -> Doc -> Doc
lineSep = (P.<$>)

-- | Infix version of lineSep (alternative to @(Text.PrettyPrint.Leijen.<$>)@.).
(\+\) = lineSep

-- | Infix alternative to @(Text.PrettyPrint.Leijen.<$$>)@.
(\++\) = (P.<$$>)



infixr 5 `lineSep`
infixr 5 \+\
infixr 5 \++\


recordBlock :: [(Text, P.Doc)] -> P.Doc
recordBlock [] = "()"
recordBlock xs = map item xs & recordBlock'
  where
    item (x, y)
      | Text.length x >= 10 = P.nest 4 (P.textStrict x <+> P.text ":=" \+\ y)
      | otherwise           = P.nest 4 (P.textStrict x <+> P.text ":=" <> (P.softline <> y))


recordBlock' :: [P.Doc] -> P.Doc
recordBlock' [] = "()"
recordBlock' xs =
    map f xs
  & P.encloseSep P.lparen P.rparen P.comma
  where
    f x = P.space <> x


listBlock :: [P.Doc] -> P.Doc
listBlock [] = "[]"
listBlock xs =
    map f xs
  & P.encloseSep P.lbracket P.rbracket P.comma
  where
    f x = P.space <> x



punLeft :: P.Doc -> [P.Doc] -> [P.Doc]
punLeft mark [] = []
punLeft mark (x:xs) = (P.spacebreak <> x) : map (mark <+>) xs

renderViaShow :: Show a => a -> Doc
renderViaShow = P.stringStrict <<< Text.pack <<< show


-- + --------------------------------------------------------------------------
-- + Primitive Instances
-- + --------------------------------------------------------------------------

instance Renderable a => Renderable [a] where
  manifest = listBlock <<< map manifest

instance Renderable Int where
  manifest = renderViaShow
instance Renderable Int8 where
  manifest = renderViaShow
instance Renderable Int16 where
  manifest = renderViaShow
instance Renderable Int32 where
  manifest = renderViaShow
instance Renderable Int64 where
  manifest = renderViaShow
instance Renderable Word16 where
  manifest = renderViaShow
instance Renderable Word32 where
  manifest = renderViaShow
instance Renderable Word64 where
  manifest = renderViaShow



instance Renderable String where
  manifest = P.dquotes <<< P.stringStrict <<< Text.pack
instance Renderable Text where
  manifest = P.dquotes <<< P.stringStrict
instance Renderable UUID where
  manifest = P.stringStrict <<< UUID.toText
instance Renderable UTCTime where
  manifest = renderViaShow
instance Renderable Bool where
  manifest = renderViaShow

instance Renderable a => Renderable (Maybe a) where
  manifest Nothing = P.text "Nothing"
  manifest (Just x) = P.text "Just" <+> manifest x

instance (Renderable v) => Renderable (l := v) where
  manifest a = recordBlock [renderKeyVal a]
instance (Renderable v1, Renderable v2) => Renderable (l1 := v1, l2 := v2) where
  manifest (a, b) = recordBlock [renderKeyVal a, renderKeyVal b]
instance (Renderable v1, Renderable v2, Renderable v3) => Renderable (l1 := v1, l2 := v2, l3 := v3) where
  manifest (a, b, c) = recordBlock [renderKeyVal a, renderKeyVal b, renderKeyVal c]
instance
  ( Renderable v1, Renderable v2, Renderable v3, Renderable v4) => Renderable
    (l1 := v1, l2 := v2, l3 := v3, l4 := v4) where
      manifest (a, b, c, d) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        ]
instance
  ( Renderable v1
  , Renderable v2
  , Renderable v3
  , Renderable v4
  , Renderable v5
  ) => Renderable
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    ) where
      manifest (a, b, c, d, e) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        , renderKeyVal e
        ]
instance
  ( Renderable v1
  , Renderable v2
  , Renderable v3
  , Renderable v4
  , Renderable v5
  , Renderable v6
  ) => Renderable
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    ) where
      manifest (a, b, c, d, e, f) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        , renderKeyVal e
        , renderKeyVal f
        ]
instance
  ( Renderable v1
  , Renderable v2
  , Renderable v3
  , Renderable v4
  , Renderable v5
  , Renderable v6
  , Renderable v7
  ) => Renderable
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    ) where
      manifest (a, b, c, d, e, f, g) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        , renderKeyVal e
        , renderKeyVal f
        , renderKeyVal g
        ]
instance
  ( Renderable v1
  , Renderable v2
  , Renderable v3
  , Renderable v4
  , Renderable v5
  , Renderable v6
  , Renderable v7
  , Renderable v8
  ) => Renderable
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    , l8 := v8
    ) where
      manifest (a, b, c, d, e, f, g, h) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        , renderKeyVal e
        , renderKeyVal f
        , renderKeyVal g
        , renderKeyVal h
        ]
instance
  ( Renderable v1
  , Renderable v2
  , Renderable v3
  , Renderable v4
  , Renderable v5
  , Renderable v6
  , Renderable v7
  , Renderable v8
  , Renderable v9
  ) => Renderable
    ( l1 := v1
    , l2 := v2
    , l3 := v3
    , l4 := v4
    , l5 := v5
    , l6 := v6
    , l7 := v7
    , l8 := v8
    , l9 := v9
    ) where
      manifest (a, b, c, d, e, f, g, h, i) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        , renderKeyVal e
        , renderKeyVal f
        , renderKeyVal g
        , renderKeyVal h
        , renderKeyVal i
        ]
instance
  ( Renderable v1
  , Renderable v2
  , Renderable v3
  , Renderable v4
  , Renderable v5
  , Renderable v6
  , Renderable v7
  , Renderable v8
  , Renderable v9
  , Renderable v10
  ) => Renderable
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
    ) where
      manifest (a, b, c, d, e, f, g, h, i, j) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        , renderKeyVal e
        , renderKeyVal f
        , renderKeyVal g
        , renderKeyVal h
        , renderKeyVal i
        , renderKeyVal j
        ]
instance
  ( Renderable v1
  , Renderable v2
  , Renderable v3
  , Renderable v4
  , Renderable v5
  , Renderable v6
  , Renderable v7
  , Renderable v8
  , Renderable v9
  , Renderable v10
  , Renderable v11
  ) => Renderable
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
    ) where
      manifest (a, b, c, d, e, f, g, h, i, j, k) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        , renderKeyVal e
        , renderKeyVal f
        , renderKeyVal g
        , renderKeyVal h
        , renderKeyVal i
        , renderKeyVal j
        , renderKeyVal k
        ]
instance
  ( Renderable v1
  , Renderable v2
  , Renderable v3
  , Renderable v4
  , Renderable v5
  , Renderable v6
  , Renderable v7
  , Renderable v8
  , Renderable v9
  , Renderable v10
  , Renderable v11
  , Renderable v12
  ) => Renderable
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
    ) where
      manifest (a, b, c, d, e, f, g, h, i, j, k, l) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        , renderKeyVal e
        , renderKeyVal f
        , renderKeyVal g
        , renderKeyVal h
        , renderKeyVal i
        , renderKeyVal j
        , renderKeyVal k
        , renderKeyVal l
        ]
instance
  ( Renderable v1
  , Renderable v2
  , Renderable v3
  , Renderable v4
  , Renderable v5
  , Renderable v6
  , Renderable v7
  , Renderable v8
  , Renderable v9
  , Renderable v10
  , Renderable v11
  , Renderable v12
  , Renderable v13
  ) => Renderable
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
    ) where
      manifest (a, b, c, d, e, f, g, h, i, j, k, l, m) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        , renderKeyVal e
        , renderKeyVal f
        , renderKeyVal g
        , renderKeyVal h
        , renderKeyVal i
        , renderKeyVal j
        , renderKeyVal k
        , renderKeyVal l
        , renderKeyVal m
        ]
instance
  ( Renderable v1
  , Renderable v2
  , Renderable v3
  , Renderable v4
  , Renderable v5
  , Renderable v6
  , Renderable v7
  , Renderable v8
  , Renderable v9
  , Renderable v10
  , Renderable v11
  , Renderable v12
  , Renderable v13
  , Renderable v14
  ) => Renderable
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
    ) where
      manifest (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        , renderKeyVal e
        , renderKeyVal f
        , renderKeyVal g
        , renderKeyVal h
        , renderKeyVal i
        , renderKeyVal j
        , renderKeyVal k
        , renderKeyVal l
        , renderKeyVal m
        , renderKeyVal n
        ]
instance
  ( Renderable v1
  , Renderable v2
  , Renderable v3
  , Renderable v4
  , Renderable v5
  , Renderable v6
  , Renderable v7
  , Renderable v8
  , Renderable v9
  , Renderable v10
  , Renderable v11
  , Renderable v12
  , Renderable v13
  , Renderable v14
  , Renderable v15
  ) => Renderable
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
    ) where
      manifest (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = recordBlock
        [ renderKeyVal a
        , renderKeyVal b
        , renderKeyVal c
        , renderKeyVal d
        , renderKeyVal e
        , renderKeyVal f
        , renderKeyVal g
        , renderKeyVal h
        , renderKeyVal i
        , renderKeyVal j
        , renderKeyVal k
        , renderKeyVal l
        , renderKeyVal m
        , renderKeyVal n
        , renderKeyVal o
        ]

renderKeyVal :: forall l v. (Renderable v) => (l := v) -> (Text, P.Doc)
renderKeyVal (l := v) = (key, manifest v)
  where
    key :: Text
    key = Text.pack $ symbolVal (Proxy @l)


-- + --------------------------------------------------------------------------
-- + Output/Display Utils
-- + --------------------------------------------------------------------------

renderMode :: Doc -> P.SimpleDoc
renderMode = P.renderPretty 0.8 80

display :: Renderable a => a -> Text
display = P.displayTStrict <<< renderMode <<< manifest

indentedDisplay :: Renderable a => a -> Text
indentedDisplay = P.displayTStrict <<< renderMode <<< f <<< manifest
  where
    f = P.indent 4


logIO :: Renderable a => a -> IO ()
logIO = Text.putStrLn <<< indentedDisplay

logIO' :: Renderable a => a -> IO ()
logIO' = Text.putStrLn <<< display


-- + --------------------------------------------------------------------------
-- + Dev
-- + --------------------------------------------------------------------------


sampleData :: IO
  ( "alpha" :=
    ( "x" := UUID, "y" := UUID, "z" := UUID )
  , "derivatives" := [ ("lorem" := Text, "path" := ( "bucket" := Text, "key" := Text )) ]
  , "gamma" := Int
  )
sampleData = do
  alpha <- mkAlpha
  bs <- M.replicateM 10 UUID.nextRandom <&> map (mkBeta "sample-bucket" . UUID.toText)
  return
    ( #alpha := alpha
    , #derivatives := bs
    , #gamma := 0
    )
  where
    mkAlpha = do
      x <- UUID.nextRandom
      y <- UUID.nextRandom
      z <- UUID.nextRandom
      return (#x := x, #y := y, #z := z)
    mkBeta b k =
      ( #lorem := "Lorem ipsum dolor sit amet, consectetur adipisicing elit, \nsed do eiusmod tempor incididunt ut"
      , #path := (#bucket := b, #key := k)
      )


test :: IO ()
test = do
  sample <- sampleData
  logIO sample

