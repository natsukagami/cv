module CV.Section.Awards (awards) where

import CV.Prelude
import CV.Section as CV
import Protolude
import Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A

awards :: Html
awards = CV.section "Awards and Scholarships" $ mapM_ award awards
  where
    awards =
      [ -- IOI
        Award
          (Link "http://stats.ioinformatics.org/people/6056" "International Olympiad in Informatics")
          [ Badge "http://stats.ioinformatics.org/results/2016" "🥇" (badgeFmt "Russia 2016" "Gold Medal"),
            Badge "http://stats.ioinformatics.org/results/2017" "🥉" (badgeFmt "Iran 2017" "Bronze Medal")
          ]
          [ "Participated in the world's most prestigious programming competition for high school students.",
            "Placed in top 30 out of more than 300 participants."
          ],
        -- ACM ICPC
        Award
          ( Link "https://icpc.global/worldfinals/teams" $
              B.div $ do
                acmICPC >> " - World Finals"
                div_ "text-sm text-gray-600" $ "team: " >> B.b "unsigned"
          )
          [Badge "https://icpc.global/community/results-2018" "🌏 #14" (badgeFmt "Beijing 2018" "")]
          [ "Participated in the world's most prestigious programming competition for university students.",
            "Placed in top 15 out of more than 100 teams."
          ],
        -- Scholarship
        Award
          (Link "https://uwaterloo.ca/news/news/mike-and-ophelia-lazaridis-pledge-21-million-science-and" $ B.span ! A.title "Original scholarship link is down" $ scholarship)
          [Badge "" "💰" "$200,000"]
          [ "Received full scholarship for all academic terms.",
            "One of the only four receipients of the 2018 scholarship."
          ]
      ]
    acmICPC = do
      span_ "lg:inline hidden" "ACM International Collegiate Programming Contest"
      span_ "lg:hidden" ! A.title "ACM International Collegiate Programming Contest" $ "ACM ICPC"
    scholarship = do
      span_ "lg:inline hidden" "Michael and Ophelia Lazaridis Olympiad Scholarship"
      div_ "lg:hidden" $ do
        div_ "" "Olympiad Scholarship"
        div_ "text-sm text-gray-600" $ do
          "Michael and Ophelia Lazaridis Olympiad Scholarship"
          div_ "md:hidden" "Estimated Amount $200,000"
    badgeFmt location value = do
      span_ "text-indigo-800" location
      span_ "lg:inline hidden" $ do
        " - "
        value

data Award = Award
  { awardTitle :: Link,
    awardBadges :: [Badge],
    awardDesc :: [Html]
  }

data Badge = Badge
  { badgeLink :: Text,
    badgeIcon :: Html,
    badgeText :: Html
  }

award :: Award -> Html
award Award {..} = B.section ! card_ "" $ do
  div_ "flex flex-row justify-between items-center" $ do
    -- Title
    B.div ! cardTitle_ "" $ link_ "hover:text-blue-600" awardTitle
    -- Badges
    div_ "flex flex-row space-x-2" $ mapM_ badge awardBadges
  B.ul ! c_ "list-inside list-disc" $ mapM_ B.li awardDesc

badge :: Badge -> Html
badge Badge {..} = ifLink $
  div_ "group rounded-full p-2 border hover:shadow-xl hover:border-blue-600 transition-shadow" $ do
    -- Only show text on hover + sm-screens, or large screens
    span_ "sm:group-hover:inline lg:inline hidden" badgeText
    -- Always show icon
    badgeIcon
    pure ()
  where
    ifLink = case badgeLink of
      "" -> identity
      _ -> link_ "" . Link (textAttr badgeLink)