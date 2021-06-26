module CV.Prelude where

import Data.String (IsString (..))
import qualified Data.Text as T
import Protolude
import Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

-- | Render with a list of styles,
-- very useful if you want to build a list of styles
-- with the `"style" -- ! comment` syntax.
style_ :: [AttributeValue] -> Attribute
style_ = A.class_ . intercalate
  where
    intercalate [] = ""
    intercalate [a] = a
    intercalate (a : b) = a <> " " <> intercalate b

-- | Represents a link with content.
data Link = Link
  { linkSrc :: AttributeValue,
    linkContent :: Markup
  }

-- | Displays a link with the given hover color.
link_ :: AttributeValue -> Link -> Html
link_ hover Link {..} =
  B.a ! A.class_ hover ! A.target "_blank" ! A.href linkSrc $ linkContent

-- | A simple div with classes.
div_ :: AttributeValue -> Html -> Html
div_ classes = B.div ! A.class_ classes

-- | A simple span with classes.
span_ :: AttributeValue -> Html -> Html
span_ classes = B.span ! A.class_ classes

-- | A shortcut for @A.class_@.
c_ :: AttributeValue -> Attribute
c_ = A.class_

-- | Card CSS attributes.
card_ :: AttributeValue -> Attribute
card_ = c_ . flip (<>) " space-y-4 shadow hover:shadow-xl transition-shadow border rounded p-4"

-- | Card Title CSS attributes
cardTitle_ :: AttributeValue -> Attribute
cardTitle_ = c_ . flip (<>) " text-2xl"

-- | Convert any value that satisfies @Show@ into an @AttributeValue@.
-- Does NOT work with @Text@, use @textAttr@ instead.
toAttr :: Show a => a -> AttributeValue
toAttr = fromString . show

-- | Convert text into an @AttributeValue@.
textAttr :: Text -> AttributeValue
textAttr = fromString . T.unpack