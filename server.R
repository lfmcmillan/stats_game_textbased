source("stats_game_text_elements.R")
source("stats_game_worlddata.R")
source("basic_statistics_generator.R")
source("basic_numerical_categorical.R")
source("weather_plot_generator.R")

library(cli)
library(greekLetters)
library(RColorBrewer)
library(truncnorm)
library(TruncatedDistributions)
# library(truncdist) ## IMPORTANT: CANNOT use the truncdist package because it uses the "stats4" package which clashes with shiny and shinyjs, and you can't just load shiny and shinyjs libraries after loading truncdist, that doesn't help
# library(cascsim) # For the truncated gamma distribution functions
library(shinyvalidate)
library(rjson)
library(shinyjs)

setup_question_stats <- function() {
    stats <- c("num_correct","num_attempted","num_questions","percent_correct","percent_attempted")
    topics <- c("overall","basic_stats")
    ntopics <- length(topics)
    question_stats <- as.data.frame(matrix(rep(0,ntopics*5), nrow=ntopics, dimnames=list(topics,stats)))
}

update_question_stats <- function(question_stats, level_answers, level_topic) {
    current <- unlist(question_stats['overall',])
    current[1:3] <- current[1:3] +
        c(sum(level_answers, na.rm=TRUE),
          sum(!is.na(level_answers)),
          length(level_answers))
    current[4:5] <- current[1:2]/current[3]*100
    question_stats['overall',] <- current

    topic <- unlist(question_stats[level_topic,])
    topic[1:3] <- topic[1:3] +
        c(sum(level_answers, na.rm=TRUE),
          sum(!is.na(level_answers)),
          length(level_answers))
    topic[4:5] <- topic[1:2]/topic[3]*100
    question_stats[level_topic,] <- topic

    question_stats
}

server <- function(input, output, session){

    # Retrieving files needed
    source("user.R", local=TRUE)

    iv <- InputValidator$new()
    iv$add_rule("username", sv_required())
    iv$add_rule("username", function(value) {
        if (grepl("[^A-Za-z]", value)) {
            "Username can only contain letters"
        }
    })

    max_level <- 6
    first_level <- 6

    # Option for whether to instantly display the question answers or not
    level_answers_immediate_display <- TRUE

    # Set up the world object and a few question objects at session level
    world <- generate_user_world("")
    level_topic <- NULL
    generated_questions <- NULL
    displayed <- vector
    level_answers <- vector()

    progress <- reactiveValues(level=first_level, done_intro=FALSE,
                               question_stats=setup_question_stats())

    # plots <- list()
    plot_details <- list()
    nplots <- reactiveVal(0)
    current_plot <- reactiveVal(1)
    nquestions <- reactiveVal(0)
    current_question <- reactiveVal(1)

    output$introText <- renderText({intro_text})
    output$endText <- renderText({"Sorry, that's the end of the game, you can't go any further yet. If you want to practise more, you can start again as a new user, and you'll get different answers for the questions."})

    output$progressText <- renderText({
        paste0("You have got ",round(progress$question_stats['overall','percent_correct']),
               "% of the questions correct overall and ",
               round(progress$question_stats['basic_stats','percent_correct']),
               "% correct for the basic stats levels.")
    })

    observeGameStartButton <- observe({
        # show("userPage")
        show("levelPage")
        hide("startPage")
    })
    bindEvent(observeGameStartButton, input$gameStartButton)

    # Setup level ----
    observeNewLevel <- observe({

        if (progress$level <= max_level) {
            levelText <- renderText({level_text[[progress$level]]})
            # plots <- list()

            source("levels.R", local=TRUE)

            current_question(0)
            nquestions(length(generated_questions$qna))
            level_answers <<- rep(NA, nquestions())

            current_plot(1)
            # nplots(length(plots))
            nplots(length(plot_details))

            if (nplots() < 2) {
                hide("plotLeftButton")
                hide("plotRightButton")
            } else {
                show("plotLeftButton")
                show("plotRightButton")
            }

            # use renderUI to create a dynamic number of output ui elements
            output$plots_ui <- renderUI({
                nump <- nplots()

                if (nump == 1) {

                    output$plot_1 <- renderPlot({
                        # plot(1:10)
                        # plots[[1]]
                        plot_any_type(plot_details[[1]])
                    })
                    tagList(
                        plotOutput("plot_1")
                    )

                } else if (nump > 1) {

                    lapply(1:nump, function(i) {
                        output_name <- paste0("plot_", i)
                        output[[output_name]] <- renderPlot({
                            # plot.new()
                            # replayPlot(plots[[i]])
                            # plots[[i]]
                            plot_any_type(plot_details[[i]])
                        })
                        if (i == 1) {
                            tagList(
                                plotOutput(
                                    outputId = paste0("plot_", i),
                                ))
                        } else {
                            tagList(
                                hidden(plotOutput(
                                    outputId = paste0("plot_", i),
                                )))
                        }

                    })
                }
            })

        } else {
            show("endPage")
            hide("levelPage")
        }

        output$levelText <- renderText({level_text[[progress$level]]})
        show("levelPage")
        hide("introPage")
        hide("progressPage")
    })
    ## IMPORTANT: as soon as you're binding an observe to more than one actionButton,
    ## need to use ignoreInit=TRUE to stop the observe being triggered at the start
    ## of the application before you've clicked either button!
    # bindEvent(observeNewLevel, input$progressNextButton, input$introNextButton, ignoreInit=TRUE)
    bindEvent(observeNewLevel, input$progressNextButton, input$introNextButton,
              input$gameStartButton, ignoreInit=TRUE)

    observePlotLeftButton <- observe({
        i <- current_plot()
        if (i > 1) {
            current_plot(i-1)
            hide(paste0("plot_",i))
            show(paste0("plot_",i-1))
        }
        if (i == 2) {
            disable("plotLeftButton")
        }
        enable("plotRightButton")
    })
    bindEvent(observePlotLeftButton, input$plotLeftButton)
    observePlotRightButton <- observe({
        i <- current_plot()
        if (i < nplots()) {
            current_plot(i+1)
            hide(paste0("plot_",i))
            show(paste0("plot_",i+1))
        }
        if (i == (nplots() - 1)) {
            disable("plotRightButton")
        }
        enable("plotLeftButton")
    })
    bindEvent(observePlotRightButton, input$plotRightButton)

    # Submit answer ----
    observeSubmitButton <- observe({

        if (is.null(input$answerSelect)) {
            output$answerText <- renderText({"You have not selected an option. Please select an option."})
        } else {

            i <- current_question()
            if (displayed[as.numeric(input$answerSelect)] %in% generated_questions$qna[[i]]$answers) {
                level_answers[i] <<- TRUE
            } else {
                level_answers[i] <<- FALSE
            }

            if (level_answers_immediate_display) {
                if (level_answers[i]) {
                    output$answerText <- renderText({correct_answer_text})
                } else {
                    output$answerText <- renderText({wrong_answer_text})
                }
            }

            disable("answerSelect")
            disable("submitButton")
        }
    })
    bindEvent(observeSubmitButton, input$submitButton)

    # Next question ----
    observeQuestionNextButton <- observe({

        qq <- current_question()

        if (qq < nquestions()) {
            # Note: do NOT want to change the value of "current_question()" in
            # submitButton observer, because navigating between the different
            # questions in the level should not depend on whether the user has
            # answered them
            qq <- qq+1
            current_question(qq)

            qna <- generated_questions$qna
            displayed <<- c(qna[[qq]]$answers, qna[[qq]]$distractors)
            if ((length(generated_questions$shuffle) == 1 && generated_questions$shuffle == TRUE)|
                (length(generated_questions$shuffle) > 1 && generated_questions$shuffle[qq] == TRUE)) {
                displayed <<- sample(displayed, length(displayed))
            } else {
                displayed <<- sort(displayed)

                if (any(displayed == "2020") && length(displayed) > 4) browser()
            }

            ## Note that locally-run apps don't mind the radiobuttons being given
            ## numeric choiceNames, but the shiny Server doesn't like numeric
            ## entries for choiceNames, so convert them to character before passing
            updateRadioButtons(session, inputId = "answerSelect",
                               label = qna[[qq]]$question, choiceNames = as.character(displayed),
                               choiceValues = 1:length(displayed), selected = character(0))
            output$answerText <- renderText({""})
            enable("answerSelect")
            enable("submitButton")
            hide("levelPage")
            show("questionPage")

        } else {
            hide("questionPage")
            show("progressPage")
            progress$question_stats <- update_question_stats(progress$question_stats,
                                                             level_answers, level_topic)
            progress$level <- progress$level + 1

            if (progress$level > max_level) {
                show("progressPage")
                hide("progressNextButton")
                show("endPage")
            }
        }
    })
    bindEvent(observeQuestionNextButton, input$questionNextButton, input$startLevelButton, ignoreInit=TRUE)

    # Back question ----
    observeQuestionBackButton <- observe({
        qq <- current_question()
        if (qq > 1) {
            qq <- qq - 1
            current_question(qq)

            qna <- generated_questions$qna
            displayed <<- c(qna[[qq]]$answers, qna[[qq]]$distractors)
            if ((length(generated_questions$shuffle) == 1 && generated_questions$shuffle == TRUE)|
                (length(generated_questions$shuffle) > 1 && generated_questions$shuffle[qq] == TRUE)) {
                displayed <<- sample(displayed, length(displayed))
            } else {
                displayed <<- sort(displayed)
            }

            ## Note that locally-run apps don't mind the radiobuttons being given
            ## numeric choiceNames, but the shiny Server doesn't like numeric
            ## entries for choiceNames, so convert them to character before passing
            updateRadioButtons(session, inputId = "answerSelect",
                               label = qna[[qq]]$question, choiceNames = as.character(displayed),
                               choiceValues = 1:length(displayed), selected = character(0))
            disable("answerSelect")

        } else {
            show("levelPage")
            hide("questionPage")
            current_question(1)
        }

    })
    bindEvent(observeQuestionBackButton, input$questionBackButton)

    # Restart game ----
    observeGameRestartButton <- observe({
        show("userPage")
        hide("progressPage")
        hide("endPage")
    })
    bindEvent(observeGameRestartButton, input$gameRestartButton)
}
