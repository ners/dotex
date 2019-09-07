module Dotex
    ( Options(..)
    , PageOptions(..)
    , GridOptions(..)
    , LogoOptions(..)
    , document
    ) where

import BasicPrelude
import Codec.Picture.Types   ( PixelRGBA8 (..) )
import Data.Fixed            ( mod' )
import Data.Monoid           ( mempty )
import Data.Text             ( unpack )
import Graphics.Svg
    ( Document (..)
    , DrawAttributes (..)
    , Group (..)
    , Transformation (..)
    , Tree (..)
    , applyCSSRules
    , defaultSvg
    , documentSize
    , parseSvgFile
    )
import Graphics.Svg.CssTypes
    ( CssDeclaration (..)
    , CssDescriptor (..)
    , CssElement (..)
    , CssRule (..)
    , CssSelector (..)
    )
import Lucid.Svg

data Options =
    Options
        { pageOptions :: PageOptions
        , gridOptions :: GridOptions
        , logoOptions :: Maybe LogoOptions
        }

data PageOptions =
    PageOptions
        { pageSize    :: (Double, Double)
        , pageMargins :: (Double, Double, Double, Double)
        , pageDpi     :: Int
        }

data GridOptions =
    GridOptions
        { gridWidth  :: Double
        , gridMargin :: Double
        , gridFill   :: (Word8, Word8, Word8, Double)
        }

data LogoOptions =
    LogoOptions
        { logoDocument  :: Document
        , logoMaxWidth  :: Double
        , logoMaxHeight :: Double
        , logoMargin    :: Double
        , logoOpacity   :: Float
        }

type Quad = (Double, Double, Double, Double)

colour :: (Word8, Word8, Word8, Double) -> PixelRGBA8
colour (a, b, c, d) = PixelRGBA8 a b c $ round (d * 255)

map2 :: (a -> b) -> (a, a) -> (b, b)
map2 f (x, y) = (f x, f y)

show2 :: (Show a) => (a, a) -> (Text, Text)
show2 = map2 tshow

svgToDoc :: Svg () -> Maybe Document
svgToDoc svg = parseSvgFile "" $ fromString $ unpack $ tshow svg

effectiveMargins :: Options -> Quad
effectiveMargins Options {..} = (t + y, r + x, b + y, l + x)
  where
    m            = gridMargin gridOptions
    (t, r, b, l) = pageMargins pageOptions
    (w, h)       = pageSize pageOptions
    x            = (w `mod'` m) / 2
    y            = (h `mod'` m) / 2

page :: Options -> Svg ()
page options@Options {..} = do
    doctype_
    with (svg11_ grid) [width_ tpw, height_ tph]
  where
    (pw , ph )   = pageSize pageOptions
    (tpw, tph)   = show2 (pw, ph)
    (t, r, b, l) = effectiveMargins options
    (w, h)       = (pw - l - r, ph - t - b)
    m            = gridMargin gridOptions
    dots = [ (x * m + l, y * m + t) | x <- [0 .. w / m], y <- [0 .. h / m] ]
    dot (x, y) = circle_
        [ cx_ $ tshow x
        , cy_ $ tshow y
        , r_ $ tshow $ gridWidth gridOptions
        , class_ "dot"
        ]
    grid = mapM_ dot dots

addStyles :: Document -> Options -> Document
addStyles doc Options {..} = doc { _styleRules = [rule] }
  where
    rule = CssRule [selector] [fill]
    selector = [AllOf [OfClass "dot"]]
    fill = CssDeclaration "fill" [[CssColor $ colour $ gridFill gridOptions]]

applyStyles :: Document -> Document
applyStyles doc = applyCSSRules doc { _styleRules = [] }

addLogo :: Document -> Options -> LogoOptions -> Document
addLogo doc options@Options {..} LogoOptions {..} = doc
    { _elements   = _elements doc ++ [logoGroup]
    , _styleRules = _styleRules doc ++ _styleRules logo
    }
  where
    logo      = applyStyles logoDocument
    dpi       = pageDpi pageOptions
    logoGroup = GroupTree $ Group
        { _groupChildren       = _elements logo
        , _groupDrawAttributes = mempty
            { _attrClass    = ["logo"]
            , _transform    = Just [Translate x y, Scale logoScale Nothing]
            , _groupOpacity = Just logoOpacity
            }
        , _groupViewBox        = Nothing
        , _groupAspectRatio    = defaultSvg
        }
    (_, _, b, l) = effectiveMargins options
    (_ , ph)     = pageSize pageOptions
    (lw, lh)     = map2 fromIntegral $ documentSize dpi logo
    (sX, sY)     = (lw / logoMaxWidth, lh / logoMaxHeight)
    logoScale    = 1 / max sX sY
    lh'          = lh * logoScale
    (x, y)       = (l + logoMargin, ph - b - lh' - logoMargin)

document :: Options -> Maybe Document
document options@Options {..} = case doc3 of
    Just _  -> doc3
    Nothing -> doc2
  where
    doc1 = svgToDoc $ page options
    doc2 = addStyles <$> doc1 <*> pure options
    doc3 = addLogo <$> doc2 <*> pure options <*> logoOptions
    -- doc4 = applyStyles <$> doc3
