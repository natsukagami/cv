module CV (site) where

import CV.Header as CV
import CV.Prelude
import qualified CV.Section.Awards as CV
import CV.Section.Education as CV
import qualified CV.Section.Skills as CV
import CV.Section.Work as CV
import Protolude
import Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

-- | The main site.
site :: Html
site = docTypeHtml $ do
  CV.head
  CV.body
  CV.footer

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
    CV.education
    CV.workExperience
    CV.personalProjects
    CV.awards
    CV.skills
    pure ()

footer = B.footer ! c_ "text-center text-xs mt-4 text-indigo-400" $ do
  "This CV was written using "
  link_
    "hover:text-indigo-800 text-indigo-600"
    $ Link "https://hackage.haskell.org/package/blaze-html" "blaze-html"
  " and "
  link_ "hover:text-indigo-800 text-indigo-600" $
    Link "https://tailwindcss.org" "TailwindCSS,"
  " source code is available on "
  link_ "hover:text-indigo-800 text-indigo-600" $
    Link "https://github.com/natsukagami/cv" "my GitHub repo"
