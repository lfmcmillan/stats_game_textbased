prepare_weather_timeseries <- function(weather) {
    lapply(weather, function(series) {
        df <- series$df
        years <- unique(df$year)
        y_max <- max(df$value[df$year %in% years])
        y_min <- min(df$value[df$year %in% years])

        xvals <- lapply(years, function(year) {
            df$month_num[df$year == year]
        })
        yvals <- lapply(years, function(year) {
            df$value[df$year == year]
        })

        list(plot_type="weather_timeseries",x=xvals, y=yvals, ylim=c(y_min, y_max), xlab="Month", ylab=series$ylab,
             title=series$plot_title, legend_text=years,
             axis_at=unique(df$month_num), axis_labels=unique(df$month_name))
    })
}

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
        answers <- round(max_val,rounding)
        all_distractors <- round(c(max_val + (max_val - min_val)/3,
                                   max_val + (max_val - min_val)/5.5,
                                   max_val - (max_val - min_val)/3.5,
                                   max_val - (max_val - min_val)/5), rounding)
        distractors <- sample(all_distractors,2)
    } else {
        answers <- round(min_val,rounding)
        all_distractors <- round(c(min_val + (max_val - min_val)/3,
                                   min_val + (max_val - min_val)/6,
                                   max(min_val - (max_val - min_val)/4,0),
                                   max(min_val - (max_val - min_val)/5,0)), rounding)
        distractors <- sample(all_distractors,2)
    }

    list(question=question, answers=answers, distractors=distractors)
}

generate_weather_year_question <- function(series) {

    question <- paste0("Which year had the ",series$direction,
                       " value of ", series$question_name,"?")

    ## Round the entire series, so that when checking for min or max, find all
    ## entries that, when rounded, are equal to the rounded min or max
    rounding <- switch(tolower(series$type),
                       "rainfall"=-1, 0)
    vals <- round(series$df$value, rounding)

    years <- unique(series$df$year)
    if (series$direction == "highest") {
        max_val <- max(vals)
        answers <- unique(series$df$year[vals == max_val])
    } else {
        min_val <- min(vals)
        answers <- unique(series$df$year[vals == min_val])
    }
    distractors <- years[!(years %in% answers)]
    list(question=question, answers=answers, distractors=distractors)
}

generate_weather_month_question <- function(series) {

    question <- paste0("Which six month period typically has the ",
                       series$direction," ", series$question_name,"?")

    if (tolower(series$type) == "rainfall") {
        raw_answers <- c("April to September","October to March")
    } else {
        raw_answers <- c("May to October","November to April")
    }

    if (series$direction == "highest") {
        answers <- raw_answers[1]
        distractors <- raw_answers[2]
    } else {
        answers <- raw_answers[2]
        distractors <- raw_answers[1]
    }

    list(question=question, answers=answers, distractors=distractors)
}

plot_weather_plots <- function(weather) {
    plots <- list()
    for (i in 1:length(weather)) {
        plot_weather_timeseries(weather[[i]])
        plots[[i]] <- recordPlot()
    }
    plots
}

generate_weather_questions <- function(weather) {
    nquestion_type <- 3
    nweather <- length(weather)
    # correct_answer <- rep(NA, nweather*nquestion_type)
    qna <- list()
    q <- 0
    for (i in 1:nweather) {

        # plot_weather_timeseries(weather[[i]])

        for (j in 1:nquestion_type) {
            q <- q+1
            qna[[q]] <- switch(j,
                               generate_weather_value_question(weather[[i]]),
                               generate_weather_year_question(weather[[i]]),
                               generate_weather_month_question(weather[[i]]))

            # if (j == 2) {
            #     displayed_answers <- sort(c(qna$answers,qna$distractors))
            # } else {
            #     displayed_answers <- shuffle_answers(c(qna$answers,qna$distractors))
            # }
            # user_response <- menu(displayed_answers,title=qna$question)
            # if (displayed_answers[user_response] %in% qna$answers) {
            #     correct_answer[(i-1)*nquestion_type+j] <- TRUE
            #     writeLines(correct_answer_text)
            # } else {
            #     correct_answer[(i-1)*nquestion_type+j] <- FALSE
            #     writeLines(wrong_answer_text)
            # }
        } ## End of this question
    } ## End of qna for this weather plot

    shuffle <- rep(TRUE,nweather*nquestion_type)
    shuffle[(2+(0:(nweather-1))*nquestion_type)] <- FALSE

    # correct_answer
    list(display="", qna=qna, shuffle=shuffle)
}

show_weather_questions <- function(weather) {
    nquestion_type <- 3
    nweather <- length(weather)
    correct_answer <- rep(NA, nweather*nquestion_type)
    for (i in 1:nweather) {

        plot_weather_timeseries(weather[[i]])

        for (j in 1:nquestion_type) {
            qna <- switch(j,
                          generate_weather_value_question(weather[[i]]),
                          generate_weather_year_question(weather[[i]]),
                          generate_weather_month_question(weather[[i]]))

            if (j == 2) {
                displayed_answers <- sort(c(qna$answers,qna$distractors))
            } else {
                displayed_answers <- shuffle_answers(c(qna$answers,qna$distractors))
            }
            user_response <- menu(displayed_answers,title=qna$question)
            if (displayed_answers[user_response] %in% qna$answers) {
                correct_answer[(i-1)*nquestion_type+j] <- TRUE
                writeLines(correct_answer_text)
            } else {
                correct_answer[(i-1)*nquestion_type+j] <- FALSE
                writeLines(wrong_answer_text)
            }
        } ## End of this question
    } ## End of qna for this weather plot

    correct_answer
}

shuffle_answers <- function(answers) {
    sample(answers, length(answers))
}
