module Main where

import CV
import Data.Text.Lazy as T (toStrict)
import Protolude
import System.Environment (getArgs)
import Text.Blaze.Html.Renderer.Text

main :: IO ()
main = do
  args <- getArgs
  let write = case args of
        [f] -> writeFile f
        _ -> putStr
  write $ T.toStrict $ renderHtml site
