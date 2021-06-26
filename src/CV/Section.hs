module CV.Section (CV.Section.section) where

import CV.Prelude
import Data.String (fromString)
import qualified Data.Text as T
import Protolude
import Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

-- Section definition, to be used in each section.
section :: Text -> Html -> Html
section title body = B.section
  ! A.class_ "space-y-4"
  $ do
    h2 ! A.class_ "group text-4xl" $ do
      toHtml title
      span_ "group-hover:inline hidden text-sm align-middle" $ B.a ! c_ "hover:underline" ! A.href ("#" <> anchor) $ "🔗"
    body
    pure ()
  where
    anchor = fromString $ T.unpack $ toAnchor title

-- | Convert text into anchors
toAnchor :: Text -> Text
toAnchor = T.replace " " "-" . T.toLower