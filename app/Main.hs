module Main where

import CV
import Data.Text as T (pack)
import Protolude
import System.Environment (getArgs)
import Text.Blaze.Html.Renderer.Pretty

main :: IO ()
main = do
  args <- getArgs
  let write = case args of
        [f] -> writeFile f
        _ -> putStr
  write . T.pack $ renderHtml site
