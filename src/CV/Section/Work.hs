module CV.Section.Work (workExperience, personalProjects) where

import CV.Prelude
import CV.Section as CV
import Protolude
import Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

workExperience :: Html
workExperience = CV.section "Work Experience" $ mapM_ job_ jobs
  where
    jobs = [dotty, multibaas]
    dotty =
      Job
        (Link "https://github.com/lampepfl/dotty" "Dotty")
        "The official Scala 3 Compiler"
        "Part-time Contributor"
        Nothing
        "January - April 2021"
        [ "Contributed as part of University of Waterloo's " >> B.i "Undergraduate Research Assistant" >> " program.",
          "Improved the initialization checker of the " >> hoverB "Scala 3" >> " compiler on early promotion cases.",
          "Reduced the number of false-positions by the checker by 40% on common codebases."
        ]
    multibaas =
      Job
        (Link "https://curvegrid.com" "Curvegrid MultiBaas")
        "Software development playform on Ethereum Blockchain"
        "Blockchain Software Engineer"
        (Just "Tokyo, Japan")
        "Summer 2019-2021"
        [ "Developed a friendly REST API over the Blockchain API using " >> hoverB "Go" >> ", deployable as a service.",
          "Designed an intuitive Blockchain and Smart Contracts management UI in " >> hoverB "Vue.",
          "Contributed bugfixes and new features to the " >> hoverB "go‑ethereum and OpenAPI" >> " open source repositories."
        ]

personalProjects :: Html
personalProjects = CV.section "Personal Projects" $ mapM_ job_ projects
  where
    projects = [codefun, kjudge]
    codefun =
      Job
        (Link "https://codefun.vn" "Codefun.vn")
        "A Programming Practice website for Students"
        "Website Creator and Maintainer"
        Nothing
        "2015 - Present"
        [ "Created a website that hosts programming challenges and automatically judges codes based on case-testing.",
          "Integrated modern architecture with REST API using " >> hoverB "Go" >> " and " >> hoverB "Rust" >> "; serving thousands of connections in milliseconds.",
          "Leveraged " >> hoverB "React" >> " for a smooth single page application experience.",
          "Used currently by " >> hoverB "5000+ high-school and university students" >> " in Vietnam."
        ]
    kjudge =
      Job
        (Link "https://github.com/natsukagami/kjudge" "kjudge")
        "Minimal Platform for Competitive Programming contests"
        "Creator and Maintainer"
        Nothing
        "2020 - Present"
        [ "Written completely in " >> hoverB "Go" >> " for a simple one-binary service.",
          "Leverages " >> hoverB "Docker" >> " for an integrated one-command deployment.",
          "Supports multiple competitive programming styles (IOI, ICPC) with flexibility in problem design.",
          "Used in many local programming contests in Vietnam as a replacement for the " >> link_ "hover:text-blue-400 text-blue-600" (Link "https://github.com/ioi/cms" "official IOI platform") >> "."
        ]

data Job = Job
  { jobTitle :: Link,
    jobSubtitle :: Html,
    jobPosition :: Html,
    jobLocation :: Maybe Html,
    jobTime :: Html,
    jobDesc :: [Html]
  }

job_ :: Job -> Html
job_ Job {..} = B.section ! card_ "group" $ header >> main
  where
    header = B.header $ do
      -- Job title and Subtitle
      h3 ! cardTitle_ "space-x-2 flex flex-row items-baseline" $ do
        link_ "hover:text-blue-600" jobTitle
        div_ "text-gray-600 text-base" jobSubtitle
      -- Job position and location/time
      div_ "flex flex-row text-sm text-gray-800" $ do
        -- Job position
        div_ "pr-2 uppercase" jobPosition
        div_ "block flex-grow" $ pure () -- Splitter
        div_ "text-indigo-800 text-right" $ do
          -- Job location, if exists
          case jobLocation of
            Just loc -> span_ "pr-2 md:pl-2 border-r-2" loc
            Nothing -> pure ()
          span_ "pl-2 md:pr-2" jobTime
    main =
      B.main $
        B.ul ! c_ "list-disc list-inside text-justify" $
          mapM_ B.li jobDesc

hoverB :: Html -> Html
hoverB = span_ "group-hover:font-bold"