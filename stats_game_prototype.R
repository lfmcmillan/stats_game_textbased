source("stats_game_worlddata.R")
source("basic_statistics_generator.R")
source("basic_plot_generator.R")

library(cli)
library(RColorBrewer)
library(truncnorm)
library(greekLetters)
library(heavy) # For the truncated gamma distribution functions

game <- function() {
    user_response <- menu(c("New user","Load user"),
                          title="Welcome to Stats Island v1.0. What would you like to do?")
    if (user_response == 1) {
        user_name <- create_user()
    } else if (user_response == 2) {
        user_name <- load_user()
    }

    quit <- FALSE
    while (!quit) {
        quit <- run_level(user_name)
    }

    return("Game Complete!")
}

create_user <- function() {
    user_name <- readline(prompt="Please enter the username you want:")

    while (file.exists(paste0(user_name,".Rdata"))) {
        user_name <- readline(prompt="Sorry, that username already exists. Please enter a different one:")
    }

    cat(paste("Thanks! Creating new user",user_name,"now.\n"))
    progress <- setup_user_progress()

    world <- generate_user_world(user_name)
    save(progress,world,file=paste0(user_name,".Rdata"))

    intro(user_name, progress, world)

    user_name
}

setup_user_progress <- function() {
    list(level=0,
         percent_correct_overall=0,
         percent_correct_basic_stats=0)
}

intro <- function(user_name, progress, world) {
    show_text(c("This is Stats Island. You and 100 other people crash-landed near
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
               they will help as much as they can."))

    readline(prompt="Please press [Enter] to continue")
    writeLines(cli::rule(line = 2))

    progress$done_intro <- TRUE
    progress$level <- 1
    save(progress, world, file=paste0(user_name,".Rdata"))
}

load_user <- function() {
    user_name <- readline(prompt="Please enter the username to load:")

    if (!file.exists(paste0(user_name,".Rdata"))) {
        user_response <- menu(c("New user","Load user"),
                              title="Sorry, no user found with that username. Please try a different username or create a new one:")
        if (user_response == 1) {
            user_name <- create_user()
        } else if (user_response == 2) {
            user_name <- load_user()
        }
    }

    user_name
}

run_level <- function(user_name) {
    load(paste0(user_name,".Rdata"))

    quit <- FALSE

    switch(as.character(progress$level),
           "1" = {
               show_text(c("A few hours after you find the hut, one of the survivors
                     comes to find you. 'We've been exploring the island,' she
                     says, 'and there are two streams, but we don't know which
                     one is better to drink from. Are there any records that
                     could help?'",
                     "You look through the historical data in the hut, and find
                     a set of data from the two streams: samples taken from
                     each of the two streams once per month for a year. Each
                     time, the researchers measured the mass of contaminants
                     found in every 100ml of water from the stream. Analyse the
                     results to see which stream is less contaminated."))

               samples <- generate_samples(1, 2, world$streams)
               generated <- generate_sample_question_set(samples, "lowest")
               plot_stats_samples(samples)
               correct_answer <- show_questions(generated)
               update_progress(user_name, progress, world, correct_answer)
           },
           "2" = {
               show_text(c("Somehow, the survivors all find places to sleep, in the
                         few existing huts or in makeshift shelters. At least the
                         nights don't seem to be too cold here. You start to
                         wonder what the typical weather is like on this island.",
                         "Digging through the old records, you find that the
                         scientists who were here kept thorough records for years
                         of the daily temperature, rainfall and highest wind
                         speed. So you can look through them to see what the
                         island's weather patterns are like."))
               correct_answer <- show_weather_questions(world$weather)
               update_progress(user_name, progress, world, correct_answer)
           },
           "3" = {
               show_text(c("Later in the day, another survivor comes to find you.
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
                         place to start."))
               samples <- generate_samples(1, 2, world$shelter_materials)
               generated <- generate_boxplot_question_set(samples)
               plot_boxplot(samples, "Total rain leaked (mm)")
               correct_answer <- show_questions(generated)
               update_progress(user_name, progress, world, correct_answer)
           },
           "4" = {
               show_text(c("A few days later, on the tenth day since the crash,
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
                         be good enough for me to use for triage?"))
               plot_scatterplot(world$trauma_assessments)
               correct_answer <- show_scatterplot_questions(world$trauma_assessments, linear=TRUE)
               update_progress(user_name, progress, world, correct_answer)
           },
           "5" = {
               ### BAR CHARTS for 2 variables? Or 1 var and > 2 categories?
           },
           "6" = {
               ### ESTIMATES VS. PARAMETERS [normal distribution]
           },
           "7" = {
               ### MIXTURE OF PLOTS (different factors related to trauma):
               # age
               # number of flights taken/countries visited in the past
               # height
               # weight
               # income
               # education level
           },
           "8" = {
               ### ESTIMATES VS. PARAMETERS [proportion] (rainy vs. dry days)
           },
           {
               writeLines("Sorry, that's the end of the game, you can't go any further yet.")
               writeLines(cli::rule(line = 2))
               quit <- TRUE
           })
    quit
}

update_progress <- function(user_name, progress, world, correct_answer) {
    percent_correct <- sum(correct_answer)/length(correct_answer)*100
    progress$level <- progress$level+1
    progress$percent_correct_overall <- percent_correct
    progress$percent_correct_basic_stats <- percent_correct
    save(progress, world, file=paste0(user_name,".Rdata"))
}

show_text <- function(display_text) {
    writeLines(strwrap(display_text, width=100))
}