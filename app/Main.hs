module Main where

import           Control.Exception        ( throw )
import           Control.Monad            ( when )
import           Data.ByteString.Lazy     ( writeFile )
import           Data.Char                ( toLower )
import           Data.Functor             ( (<&>) )
import           Data.Maybe               ( fromJust, isNothing )
import           Graphics.Rasterific.Svg  ( pdfOfSvgDocument )
import           Graphics.Svg             ( Document, loadSvgFile )
import           Graphics.Text.TrueType   ( emptyFontCache )
import           Prelude                  hiding ( writeFile )
import           System.Console.ParseArgs
import           System.Exit

import qualified Dotex                    as D
    ( GridOptions (..)
    , LogoOptions (..)
    , Options (..)
    , PageOptions (..)
    , document
    )

data Options
    = OptionHelp
    | OptionOutputFile
    | OptionLogoFile
    | OptionDocSize
    | OptionDotMargin
    | OptionPageMargin
    deriving (Ord, Eq, Show)

data DocumentSize
    = A4Paper
    | A5Paper
    | A6Paper
    | LetterPaper
    | TestPaper
    deriving (Ord, Eq, Show)

argd :: [Arg Options]
argd =
    [ Arg
        { argIndex = OptionOutputFile
        , argName  = Just "out"
        , argAbbr  = Just 'o'
        , argData  = argDataDefaulted "output file name" ArgtypeString "out"
        , argDesc  = "Output file."
        }
    , Arg
        { argIndex = OptionLogoFile
        , argName  = Just "logo"
        , argAbbr  = Just 'l'
        , argData  = argDataOptional "logo file" ArgtypeString
        , argDesc  = "Optional logo SVG."
        }
    , Arg
        { argIndex = OptionDocSize
        , argName  = Just "document-size"
        , argAbbr  = Just 'd'
        , argData  = argDataDefaulted "size" ArgtypeString "a4"
        , argDesc  = "Document size, e.g. a4, a5, letter. Default is a4."
        }
    , Arg
        { argIndex = OptionDotMargin
        , argName  = Just "dot-margin"
        , argAbbr  = Just 'm'
        , argData  = argDataDefaulted "margin" ArgtypeDouble 5
        , argDesc  = "Margin between dots in mm. Default is 5."
        }
    , Arg
        { argIndex = OptionPageMargin
        , argName  = Just "page-margin"
        , argAbbr  = Just 'p'
        , argData  = argDataDefaulted "margin" ArgtypeDouble 10
        , argDesc  = "Margin between dots in mm. Default is 5."
        }
    , Arg
        { argIndex = OptionHelp
        , argName  = Just "help"
        , argAbbr  = Just 'h'
        , argData  = Nothing
        , argDesc  = "Print this help output."
        }
    ]

documentSize :: String -> (Double, Double)
documentSize size
    | size' == "a4" = (210, 297)
    | size' == "a5" = (148, 210)
    | size' == "a6" = (105, 148)
    | size' == "letter" = (216, 279)
    | size' == "test" = (10, 10)
    | otherwise = throw
    $ ParseArgsException "" ("Unknown document size: " ++ size)
    where size' = map toLower size

maybeLogo :: Maybe String -> IO (Maybe Document)
maybeLogo Nothing      = return Nothing
maybeLogo (Just fname) = loadSvgFile fname

options :: Args Options -> Maybe Document -> D.Options
options args logo = D.Options
    { D.pageOptions = D.PageOptions size (margin, margin, margin, margin) 100
    , D.gridOptions = D.GridOptions 0.15 5 (0, 0, 0, 0.5)
    , D.logoOptions = logo <&> \d -> D.LogoOptions d 30 30 2 0.9
    }
  where
    size   = documentSize $ fromJust $ getArgString args OptionDocSize
    margin = fromJust $ getArgDouble args OptionPageMargin

makeDoc :: D.Options -> IO Document
makeDoc opts@D.Options {..} = do
    when (isNothing evnPage) $ die "Could not create document!"
    return $ fromJust evnPage
  where
    evnPage = D.document opts
    -- oddPage = D.document opts { D.logoOptions = Nothing }

saveDoc :: Document -> String -> IO ()
saveDoc doc fname = do
    (pdf, _) <- pdfOfSvgDocument emptyFontCache Nothing 100 doc
    writeFile fname pdf
    putStrLn $ "Saved file " <> fname <> "."

main :: IO ()
main = do
    args <- parseArgsIO (ArgsParseControl ArgsComplete ArgsHardDash) argd
    when (gotArg args OptionHelp) (putStrLn (argsUsage args) >> exitSuccess)
    logo <- maybeLogo $ getArgString args OptionLogoFile
    doc  <- makeDoc $ options args logo
    saveDoc doc $ fromJust $ getArgString args OptionOutputFile
