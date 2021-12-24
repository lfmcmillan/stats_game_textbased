source("stats_game_text_elements.R")
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
                          title=welcome_question)
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
    show_text(intro_text)

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
               show_text(level_text[[1]])
               samples <- generate_samples(1, 2, world$streams)
               generated <- generate_sample_question_set(samples, "lowest")
               plot_stats_samples(samples)
               correct_answer <- show_questions(generated)
               update_progress(user_name, progress, world, correct_answer)
           },
           "2" = {
               show_text(level_text[[2]])
               correct_answer <- show_weather_questions(world$weather)
               update_progress(user_name, progress, world, correct_answer)
           },
           "3" = {
               show_text(level_text[[3]])
               samples <- generate_samples(1, 2, world$shelter_materials)
               generated <- generate_boxplot_question_set(samples)
               plot_boxplot(samples, "Total rain leaked (mm)")
               correct_answer <- show_questions(generated)
               update_progress(user_name, progress, world, correct_answer)
           },
           "4" = {
               show_text(level_text[[4]])
               plot_scatterplot(world$trauma_assessments)
               correct_answer <- show_scatterplot_questions(world$trauma_assessments, linear=TRUE)
               update_progress(user_name, progress, world, correct_answer)
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

show_questions <- function(generated) {
    correct_answer <- rep(NA, nrow(generated$answers))

    writeLines(generated$display)
    for (i in 1:nrow(generated$answers)) {
        # displayed_answers <- sample(unlist(generated$answers[i,]),ncol(generated$answers))
        displayed_answers <- sort(unlist(generated$answers[i,]))
        user_response <- menu(displayed_answers,title=generated$questions[i])
        if (displayed_answers[user_response] == generated$answers[i,1]) {
            correct_answer[i] <- TRUE
            writeLines(correct_answer_text)
        } else {
            correct_answer[i] <- FALSE
            writeLines(wrong_answer_text)
        }
    }

    correct_answer
}