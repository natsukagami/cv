module CV (site) where

import CV.Header as CV
import CV.Prelude
import qualified CV.Section.Awards as CV
import CV.Section.Education as CV
import CV.Section.Work as CV
import Protolude
import Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

-- | The main site.
site :: Html
site = docTypeHtml $ do
  CV.head
  CV.body

-- | Head object
head :: Html
head = do
  B.head $ do
    title "Nguyen Pham - CV"
    -- Standard responsive set up
    meta ! A.charset "UTF-8"
    meta
      ! A.name "viewport"
      ! A.content "width=device-width, initial-scale=1.0"
    -- CSS file loading
    B.link
      ! A.href "./index.css"
      ! A.rel "stylesheet"

body :: Html
body = B.body
  ! style_
    [ "container font-sans", -- ! Base styles
      "py-8 px-4 lg:px-0", -- ! Padding
      "mx-auto", -- ! Margin
      "space-y-8" -- ! Spacing
    ]
  $ do
    CV.header
    -- Sections one by one
    CV.workExperience
    CV.personalProjects
    CV.education
    CV.awards
    pure ()
