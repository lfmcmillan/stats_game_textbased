welcome_question <- "Welcome to Stats Island v1.0. What would you like to do?"
intro_text <- c("This is Stats Island. You and 100 other people crash-landed near
                the island and swam to shore. You discovered that the island is
                deserted, but there is a hut that has some scientific equipment
                and historical records. It appears that this island was once a
                research outpost.",
                "A crate washes ashore containing some luggage, including a laptop
                and a portable solar panel charger. You can use the hut to perform
                scientific experiments, and analyse data on the laptop. The
                existing data, and any new experiments, will help you work out
                how to survive on the island.",
                "None of the other survivors knows anything about statistics, but
                they will help as much as they can.")

correct_answer_text <- "Yes! Correct answer."
wrong_answer_text <- "No, wrong answer."

level_text <- list("1"=c("A few hours after you find the hut, one of the survivors
                         comes to find you. 'We've been exploring the island,' she
                         says, 'and there are two streams, but we don't know which
                         one is better to drink from. Are there any records that
                         could help?'",
                         "You look through the historical data in the hut, and find
                         a set of data from the two streams: samples taken from
                         each of the two streams once per month for a year. Each
                         time, the researchers measured the mass of contaminants
                         found in every 100ml of water from the stream. Analyse the
                         results to see which stream is less contaminated."),
                   "2"=c("There are all sorts of info about the island in the hut,
                         but you'll need to work out how to analyse each type of
                         data."),
                   "3"=c("Somehow, the survivors all find places to sleep, in the
                         few existing huts or in makeshift shelters. At least the
                         nights don't seem to be too cold here. You start to
                         wonder what the typical weather is like on this island.",
                         "Digging through the old records, you find that the
                         scientists who were here kept thorough records for years
                         of the daily temperature, rainfall and highest wind
                         speed. So you can look through them to see what the
                         island's weather patterns are like."),
                   "4"=c("You are still examining the island's weather data,
                         now looking at the daily wind speed patterns for this
                         month."),
                   "5"=c("Later in the day, another survivor comes to find you.
                         We've got two different materials we can build shelter
                         roofs from: palm leaves, or some kind of tree that looks
                         like a variety of willow. Is there any data on which one
                         is better for building?",
                         "Looking through the hut datasets, you find that the
                         scientists who were here called the willow-like tree
                         'pacific etang', and they tested both it and palm leaves
                         by building shelters out of them and then testing how much
                         rain got in on each rainy day. That won't account for
                         how well-built the shelters were, but it would be a good
                         place to start."),
                   "6"=c("A few days later, on the tenth day since the crash,
                         the psychiatrist comes to find you.",
                         "I want to evaluate how traumatised all of the survivors
                         are, but it's very time consuming. I could use the
                         survivors' self-assessments of how they are feeling,
                         to decide who to prioritise, but I don't know how
                         accurate their self-assessments are.",
                         "Here are the self-assessments and my assessments for a
                         subset of 30 survivors. They completed the
                         self-assessments before I did my assessments. Each
                         assessment is a score from 1 meaning 'Feeling very well'
                         to 10 meaning 'Feeling very bad'. Would you please
                         compare them, to see whether the self-assessments would
                         be good enough for me to use for triage?"),
                   "7"=c("A survivor comes to find you, saying 'We have been
                         picking fruit from the trees to eat, but some of it rots
                         quickly. We'd like to know if one type of fruit rots
                         more quickly than another, so we picked 20 fruits of
                         each type all on the same day, and kept them all in the
                         same place, but with each fruit separated from the others,
                         and then checked them frequently to see how many days it
                         took them to develop brown patches or mould.",
                         "You can look at the data for the two types of fruit to
                         answer the question."),
                   "8"=c("The survivors have been trying to work out where the
                         best place is to set up storage huts. You can compare
                         the recorded wind data for the west and east points of
                         the island to see which site has calmer weather.")
)

