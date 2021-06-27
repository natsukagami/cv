module CV.Header (CV.Header.header) where

import CV.Prelude
import Protolude
import Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

header :: Html
header = B.header $ name >> contacts

name :: Html
name =
  h1 ! A.class_ "text-6xl" $
    "Nguyen "
      >> B.b "Pham"

contacts :: Html
contacts =
  section
    ! B.customAttribute "aria-roledescription" "contacts"
    ! A.class_ "flex flex-row gap-6 pt-3 flex-wrap"
    $ sequence_
      [ toContact "Email" "hover:text-green-400" "📧" "mailto:nki@nkagami.me" "nki@nkagami.me",
        toContact "Personal Website" "hover:text-red-400" "🌐" "https://blog.nkagami.me" "blog.nkagami.me",
        toContact
          "GitHub"
          "hover:text-blue-400"
          ( B.img ! A.style "width: 1rem"
              ! A.class_ "inline"
              ! A.src "img/github.png"
          )
          "https://github.com/natsukagami"
          "natsukagami"
      ]

toContact ::
  AttributeValue ->
  AttributeValue ->
  Html ->
  AttributeValue ->
  Markup ->
  Html
toContact title hover icon href content = B.div
  ! A.title title
  $ link_ hover
    . Link href
    $ do
      span icon
      " "
      content
      pure ()
