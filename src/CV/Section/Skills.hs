module CV.Section.Skills (skills) where

import CV.Prelude
import CV.Section as CV
import Protolude
import Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

skills :: Html
skills =
  CV.section "Skills" $
    div_ "grid lg:grid-cols-2 grid-cols-1 space-y-2" $ pure ()