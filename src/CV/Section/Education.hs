module CV.Section.Education (education) where

import CV.Prelude
import CV.Section as CV
import Protolude
import Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

education :: Html
education = CV.section "Education" $
  B.section ! card_ "group" $ do
    div_ "flex flex-row" $ do
      -- Icon on hover
      B.img
        ! A.style "height: 10vh"
        ! c_ "lg:group-hover:block hidden"
        ! A.src "https://uwaterloo.ca/brand/sites/ca.brand/files/universityofwaterloo_logo_vert_rgb.png"
      B.header ! c_ "flex-grow flex flex-col justify-center" $ do
        h3 ! cardTitle_ "flex flex-row justify-between items-baseline" $ do
          div_ "" $ link_ "hover:text-blue-600" $ Link "https://cs.uwaterloo.ca" "University of Waterloo"
          div_ "text-base text-indigo-800 text-right" "Waterloo, ON, Canada"
        div_ "flex flex-row justify-between text-gray-800" $ do
          div_ "" "Candidate for Honours Bachelors in Computer Science"
          div_ "italic text-right" "2018 - 2021 (currently 4A)"
    B.main $ do
      div_ "text-lg" $ stat B.div $ ClassStat "Overall GPA" "" 93
      div_ "mb-2 text-lg text-bold lg:group-hover:text-center" "Relevant Classes:"
      B.ul ! c_ "list-inside list-disc grid lg:grid-cols-2 grid-cols-1" $ do
        mapM_ (stat B.li) classes
  where
    classes =
      [ ClassStat "CS 442" "Principles of Programming Languages" 96,
        ClassStat "CS 444" "Compilers" 96,
        ClassStat "CS 365" "Models of Computations (Enriched)" 93,
        ClassStat "CS 343" "Concurrent and Parallel Programming" 92
      ]

data ClassStat = ClassStat
  { classCode :: Html,
    className :: Text,
    classPerf :: Int
  }

stat elem s = do
  -- Only for large screen and on-hover
  div_ "lg:group-hover:block hidden col-span-2" $ statWithBar elem s
  -- For everything else
  div_ "lg:group-hover:hidden" $ statSimple elem s

statSimple :: (Html -> Html) -> ClassStat -> Html
statSimple elem ClassStat {..} = elem ! c_ "col-span-2" $ do
  B.b classCode
  case className of
    "" -> pure ()
    _ -> toHtml $ ": " <> className
  toHtml $ (" - " :: Text) <> show classPerf <> "%"

statWithBar :: (Html -> Html) -> ClassStat -> Html
statWithBar elem ClassStat {..} = elem ! c_ "grid grid-cols-3 space-y-2" $ do
  div_ "col-span-2" $ do
    B.b classCode
    toHtml $ ": " <> className
  div_ "flex flex-row items-center" $ do
    B.progress
      ! c_ "w-full flex-grow mr-1"
      ! A.max "100"
      ! A.value (toAttr classPerf)
      $ pure ()
    div_ "font-mono" $ toHtml $ show classPerf <> ("%" :: Text)