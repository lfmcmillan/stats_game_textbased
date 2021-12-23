plot_weather_timeseries <- function(series) {
    if(!is.null(dev.list())) dev.off()

    colours <- palette("Dark2")

    df <- series$df
    years <- unique(df$year)
    y_max <- max(df$value[df$year %in% years])
    y_min <- min(df$value[df$year %in% years])
    plot(df$month_num[df$year == years[1]], df$value[df$year == years[1]],
         type="l", ylim=c(y_min, y_max), col=colours[1], lwd=2, xlab="Month", xaxt='n',
         ylab=series$ylab, main=series$plot_title)
    axis(side=1, at=unique(df$month_num), labels=unique(df$month_name))
    legend("topleft",legend=years, col=colours[1:length(years)], lwd=2)
    for (i in 2:length(years)) {
        lines(df$month_num[df$year == years[i]],
              df$value[df$year == years[i]],
              col=colours[i], lwd=2)
    }
}

generate_weather_value_question <- function(series) {

    question <- paste0("What is the approximate ",series$direction,
                       " value of ", series$question_name,"?")

    max_val <- max(series$df$value)
    min_val <- min(series$df$value)
    rounding <- switch(tolower(series$type),
                       "rainfall"=-1, 0)
    if (series$direction == "highest") {
        answer <- round(max_val,rounding)
        all_distractors <- round(c(max_val + (max_val - min_val)/3,
                                   max_val + (max_val - min_val)/5.5,
                                   max_val - (max_val - min_val)/3.5,
                                   max_val - (max_val - min_val)/5), rounding)
        distractors <- sample(all_distractors,2)
    } else {
        answer <- round(min_val,rounding)
        all_distractors <- round(c(min_val + (max_val - min_val)/3,
                                   min_val + (max_val - min_val)/6,
                                   max(min_val - (max_val - min_val)/4,0),
                                   max(min_val - (max_val - min_val)/5,0)), rounding)
        distractors <- sample(all_distractors,2)
    }

    list(question=question, answer=answer, distractors=distractors)
}

generate_weather_year_question <- function(series) {

    question <- paste0("Which year had the ",series$direction,
                       " value of ", series$question_name,"?")

    years <- unique(series$df$year)
    if (series$direction == "highest") {
        answer <- series$df$year[which.max(series$df$value)]
    } else {
        answer <- series$df$year[which.min(series$df$value)]
    }
    distractors <- years[years != answer]

    list(question=question, answer=answer, distractors=distractors)
}

generate_weather_month_question <- function(series) {

    question <- paste0("Which six month period typically has the ",
                       series$direction," ", series$question_name,"?")

    if (tolower(series$type) == "rainfall") {
        answers <- c("April to September","October to March")
    } else {
        answers <- c("May to October","November to April")
    }

    if (series$direction == "highest") {
        answer <- answers[1]
        distractors <- answers[2]
    } else {
        answer <- answers[2]
        distractors <- answers[1]
    }

    list(question=question, answer=answer, distractors=distractors)
}

show_weather_questions <- function(weather) {
    nquestion <- 3
    nweather <- length(weather)
    correct_answer <- rep(NA, nweather*nquestion)
    for (i in 1:nweather) {
        plot_weather_timeseries(weather[[i]])

        for (j in 1:nquestion) {
            generated <- switch(j,
                                generate_weather_value_question(weather[[i]]),
                                generate_weather_year_question(weather[[i]]),
                                generate_weather_month_question(weather[[i]]))

            if (j == 2) {
                displayed_answers <- sort(c(generated$answer,generated$distractors))
            } else {
                displayed_answers <- shuffle_answers(c(generated$answer,generated$distractors))
            }
            user_response <- menu(displayed_answers,title=generated$question)
            if (displayed_answers[user_response] == generated$answer) {
                correct_answer[(i-1)*nquestion+j] <- TRUE
                writeLines("Yes! Correct answer.")
            } else {
                correct_answer[(i-1)*nquestion+j] <- FALSE
                writeLines("No, wrong answer.")
            }
        } ## End of this question
    } ## End of questions for this weather plot

    correct_answer
}

plot_scatterplot <- function(scatter_data) {
    if(!is.null(dev.list())) dev.off()

    plot(scatter_data$x, scatter_data$y, xlab=scatter_data$xlab,
         ylab=scatter_data$ylab)
}

show_scatterplot_questions <- function(scatter_data, linear) {
    actual_correlation <- cor(scatter_data$x,scatter_data$y)
    if (-0.5 < actual_correlation & actual_correlation < 0.5) stop("Correlation is too low for linear answers")

    linearity_question <- paste0("Is there a linear relationship between ",
                                 scatter_data$xlab," and ",scatter_data$ylab,"?")
    if (linear) {
        linearity_answer <- "Yes"
        linearity_distractor <- "No"

        direction_question <- paste0("Is the relationship positive or negative?")
        if (actual_correlation > 0) {
            direction_answer <- "Positive"
            direction_distractor <- "Negative"
        } else {
            direction_answer <- "Negative"
            direction_distractor <- "Positive"
        }

        correlation_question <- paste0("Which of these correlations do you think is closest to the true correlation?")
        correlation_answer <- round(actual_correlation,1)
        if (actual_correlation > 0.5) {
            correlation_distractor <- round(runif(1,0,0.3),1)
        } else if (actual_correlation < -0.5) {
            correlation_distractor <- round(runif(1,-0.3,0),1)
        }

        questions <- c(linearity_question, direction_question, correlation_question)
        answers <- c(linearity_answer, direction_answer, correlation_answer)
        distractors <- c(linearity_distractor, direction_distractor, correlation_distractor)
    } else {
        questions <- linearity_question
        answers <- c("Yes")
        distractors <- c("No")
    }

    num <- length(questions)
    correct_answer <- rep(NA,num)
    for (i in 1:num) {
        displayed_answers <- c(answers[i], distractors[i])
        user_response <- menu(displayed_answers,title=questions[i])
        if (displayed_answers[user_response] == answers[i]) {
            correct_answer[i] <- TRUE
            writeLines("Yes! Correct answer.")
        } else {
            correct_answer[i] <- FALSE
            writeLines("No, wrong answer.")
        }
    }
    correct_answer
}

shuffle_answers <- function(answers) {
    sample(answers, length(answers))
}
