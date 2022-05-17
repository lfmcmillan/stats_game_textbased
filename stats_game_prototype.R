source("stats_game_text_elements.R")
source("stats_game_worlddata.R")
source("basic_statistics_generator.R")
source("basic_plot_generator.R")

library(cli)
library(greekLetters)
library(RColorBrewer)
library(truncnorm)
library(TruncatedDistributions)
library(heavy) # For the truncated gamma distribution functions

game <- function(first_level=1) {
    user_response <- menu(c("New user","Load user"),
                          title=welcome_question)
    if (user_response == 1) {
        username <- create_user(first_level)
    } else if (user_response == 2) {
        username <- load_user()
    }

    quit <- FALSE
    while (!quit) {
        quit <- run_level(username)
    }

    return("Game Complete!")
}

create_user <- function(first_level) {
    username <- readline(prompt="Please enter the username you want:")

    while (file.exists(paste0(username,".Rdata"))) {
        username <- readline(prompt="Sorry, that username already exists. Please enter a different one:")
    }

    cat(paste("Thanks! Creating new user",username,"now.\n"))
    progress <- setup_user_progress()

    world <- generate_user_world(username)
    save(progress,world,file=paste0(username,".Rdata"))

    intro(username, progress, world, first_level)

    username
}

setup_user_progress <- function() {
    list(level=0,
         percent_correct_overall=0,
         percent_correct_basic_stats=0)
}

intro <- function(username, progress, world, first_level=1) {
    show_text(intro_text)

    readline(prompt="Please press [Enter] to continue")
    writeLines(cli::rule(line = 2))

    progress$done_intro <- TRUE
    progress$level <- first_level
    save(progress, world, file=paste0(username,".Rdata"))
}

load_user <- function() {
    username <- readline(prompt="Please enter the username to load:")

    if (!file.exists(paste0(username,".Rdata"))) {
        user_response <- menu(c("New user","Load user"),
                              title="Sorry, no user found with that username. Please try a different username or create a new one:")
        if (user_response == 1) {
            username <- create_user()
        } else if (user_response == 2) {
            username <- load_user()
        }
    }

    username
}

run_level <- function(username) {
    load(paste0(username,".Rdata"))

    quit <- FALSE

    max_level <- 6
    if (progress$level <= max_level) show_text(level_text[[progress$level]])
    switch(as.character(progress$level),
           "1" = {
               samples <- generate_samples(1, 2, world$streams)
               generated <- generate_sample_question_set(samples, "lowest")
               plot_summary_stat_barplots(samples)
               correct_answer <- show_questions(generated)
           },
           "2" = {
               correct_answer <- show_weather_questions(world$monthly_weather)
           },
           "3" = {
               generated <- generate_histogram_question_set(world$daily_weather$windspeed)
               plot_histogram(world$daily_weather$windspeed)
               correct_answer <- show_questions(generated)
           },
           "4" = {
               samples <- generate_samples(1, 2, world$shelter_materials)
               generated <- generate_boxplot_question_set(samples)
               plot_boxplot(samples, "Total rain leaked (mm)")
               correct_answer <- show_questions(generated)
           },
           "5" = {
               plot_scatterplot(world$trauma_assessments)
               generated <- generate_scatterplot_question_set(world$trauma_assessments, linear=TRUE)
               correct_answer <- show_questions(generated)
           },
           "6" = {
               samples <- generate_samples(1, 2, world$fruit_rot)
               plot_dotplot(samples, "Days from picking till first rot")
               generated <- generate_dotplot_question_set(samples,
                                    direction = sample(c("lowest","highest"),1))
               correct_answer <- show_questions(generated)
           },
           {
               writeLines("Sorry, that's the end of the game, you can't go any further yet.")
               writeLines(cli::rule(line = 2))
               quit <- TRUE
           })

    if (progress$level <= max_level) update_progress(username, progress, world, correct_answer)
    writeLines(cli::rule(line = 2))
    quit
}

update_progress <- function(username, progress, world, correct_answer) {

    ## THIS IS WRONG! DOESN'T ACCUMULATE OVER MULTIPLE LEVELS!!

    percent_correct <- sum(correct_answer)/length(correct_answer)*100
    progress$level <- progress$level+1
    progress$percent_correct_overall <- percent_correct
    progress$percent_correct_basic_stats <- percent_correct
    save(progress, world, file=paste0(username,".Rdata"))
}

show_text <- function(display_text) {
    writeLines(strwrap(display_text, width=100))
}

show_questions <- function(generated) {
    ## 'qna' stands for 'Question and Answers' i.e. the combined question and its answers and distractors
    qna <- generated$qna
    numq <- length(qna)
    correct_answer <- rep(NA, numq)

    writeLines(generated$display)
    for (i in 1:numq) {
        displayed_answers <- sort(c(qna[[i]]$answers,qna[[i]]$distractors))
        user_response <- menu(displayed_answers,title=qna[[i]]$question)
        if (displayed_answers[user_response] %in% qna[[i]]$answers) {
            correct_answer[i] <- TRUE
            writeLines(correct_answer_text)
        } else {
            correct_answer[i] <- FALSE
            writeLines(wrong_answer_text)
        }
    }

    correct_answer
}