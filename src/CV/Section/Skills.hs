{-# LANGUAGE TupleSections #-}

module CV.Section.Skills (skills) where

import CV.Prelude
import CV.Section as CV
import Protolude
import Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

skills :: Html
skills =
  CV.section "Skills" $
    div_ "grid lg:grid-cols-2 grid-cols-1 lg:space-x-2 space-y-2 lg:space-y-0" $ appDev >> foundational

appDev :: Html
appDev =
  skillCard
    "Application Development"
    [ ("Languages", languages),
      ("Tooling and Frameworks", tooling),
      ("Databases", databases),
      ("Paradigms", paradigms)
    ]
  where
    languages = [master "Go", master "Rust", good "Haskell", good "C++", good "TypeScript", ok "HTML", meh "CSS"]
    tooling = [master "React", good "Vue.js", ok "gRPC", ok "Polysemy"]
    databases = [good "MySQL/MariaDB", ok "PostgreSQL"]
    paradigms = [good "Functional", ok "Object-Oriented"]

foundational =
  skillCard
    "Foundational Knowledge"
    [ ("Theoretical", theoretical),
      ("Tooling", tooling),
      ("General", general)
    ]
  where
    theoretical = [master "Type Systems", good "Formal Languages", good "Compilers", good "Category Theory", ok "Operating Systems"]
    tooling = [good "Linux", good "git", good "GitHub Actions"]
    general = [good "Linear Algebra", good "Ethereum", ok "Cryptography", ok "Japanese"]

ok :: t -> (t, SkillLevel)
ok = (,OK)

master :: t -> (t, SkillLevel)
master = (,Master)

good :: t -> (t, SkillLevel)
good = (,Good)

meh :: t -> (t, SkillLevel)
meh = (,Meh)

skillCard :: Html -> [(Html, [(Text, SkillLevel)])] -> Html
skillCard title sets = B.div
  ! card_ ""
  $ do
    B.h3 ! cardTitle_ "" $ title
    B.ul ! c_ "list-inside list-disc" $
      mapM_ (uncurry skillSet) sets

skillSet :: Html -> [(Text, SkillLevel)] -> Html
skillSet category skills = B.li $ do
  B.b $ toHtml $ category <> ": "
  runSkills skills
  where
    runSkills [] = pure ()
    runSkills [(a, b)] = skill a b
    runSkills ((a, b) : r) = skill a b >> ", " >> runSkills r

data SkillLevel
  = Master
  | Good
  | OK
  | Meh

skill ::
  Text ->
  SkillLevel ->
  Html
skill text level = div_ "inline-block align-text-bottom" ! A.title (levelDisp level <> textAttr text) $
  div_ "flex flex-col items-center leading-none" $ do
    div_ "text-xs font-mono" $ levelBar level
    toHtml text
  where
    levelDisp Master = "Familiar with "
    levelDisp Good = "Good at "
    levelDisp OK = "Capable of "
    levelDisp Meh = "Had experience with "
    levelBar Master = span_ "text-green-700" "oooo"
    levelBar Good = span_ "text-green-300" "ooo" >> span_ "text-gray-300" "o"
    levelBar OK = span_ "text-yellow-400" "oo" >> span_ "text-gray-300" "oo"
    levelBar Meh = span_ "text-red-500" "o" >> span_ "text-gray-300" "ooo"