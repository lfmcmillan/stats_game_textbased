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
                                   max_val + (max_val - min_val)/4,
                                   max_val - (max_val - min_val)/3.5,
                                   max_val - (max_val - min_val)/5), rounding)
        distractors <- sample(all_distractors,2)
    } else {
        answer <- round(min_val,rounding)
        all_distractors <- round(c(min_val + (max_val - min_val)/3,
                               min_val + (max_val - min_val)/4,
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

shuffle_answers <- function(answers) {
    sample(answers, length(answers))
}
