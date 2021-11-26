source("stats_game_worlddata.R")
source("basic_statistics_generator.R")

library(cli)

game <- function() {
    user_response <- menu(c("New user","Load user"),title="Welcome to Stats Island v1.0. What would you like to do?")
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
    show_text("This is Stats Island. You and 100 other people crash-landed near
               the island and swam to shore. You discovered that the island is
               deserted, but there is a hut that has some scientific equipment
               and historical records. It appears that this island was once a
               research outpost.")
    show_text("A crate washes ashore containing some luggage, including a laptop
               and a portable solar panel charger. You can use the hut to perform
               scientific experiments, and analyse data on the laptop. The
               existing data, and any new experiments, will help you work out
               how to survive on the island.")
    show_text("None of the other survivors knows anything about statistics, but
              they will help as much as they can.")

    readline(prompt="Please press [Enter] to continue")
    writeLines(cli::rule(line = 2))

    progress$done_intro <- TRUE
    progress$level <- 1
    save(progress, world, file=paste0(user_name,".Rdata"))
}

load_user <- function() {
    user_name <- readline(prompt="Please enter the username to load:")

    if (!file.exists(paste0(user_name,".Rdata"))) {
        print("Sorry, no user found with that username. Please create a new user account instead.")
        create_user()
    }

    user_name
}

run_level <- function(user_name) {
    load(paste0(user_name,".Rdata"))

    quit <- FALSE

    switch(as.character(progress$level),
           "1" = {
               show_text("A few hours after you find the hut, one of the survivors
                     comes to find you. 'We've been exploring the island,' she
                     says, 'and there are two streams, but we don't know which
                     one is better to drink from. Are there any records that
                     could help?'")
               show_text("You look through the historical data in the hut, and find
                     a set of data from the two streams: samples taken from
                     each of the two streams once per month for a year. Each
                     time, the researchers measured the mass of contaminants
                     found in every 100ml of water from the stream. Analyse the
                     results to see which stream is less contaminated.")

               samples <- generate_samples(1, 2, world$streams)
               generated <- generate_sample_question_set(samples, "lowest")
               plot_stats_samples(samples)
               correct_answer <- show_questions(generated)
               percent_correct <- calc_percent_correct(correct_answer)
               progress$level <- progress$level+1
               progress$percent_correct_overall <- percent_correct
               progress$percent_correct_basic_stats <- percent_correct
               save(progress, world, file=paste0(user_name,".Rdata"))
           },
           {
               writeLines("Sorry, that's the end of the game, you can't go any further yet.")
               writeLines(cli::rule(line = 2))
               quit <- TRUE
           })
    quit
}

calc_percent_correct <- function(correct_answer) {
    sum(correct_answer)/length(correct_answer)*100
}

show_text <- function(display_text) {
    writeLines(strwrap(display_text, width=100))
}