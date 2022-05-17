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

plot_scatterplot <- function(scatter_data) {
    if(!is.null(dev.list())) dev.off()

    plot(scatter_data$x, scatter_data$y, xlab=scatter_data$xlab,
         ylab=scatter_data$ylab)

    plots <- list()
    plots[[1]] <- recordPlot()
    plots
}

generate_scatterplot_question_set <- function(scatter_data, linear) {
    actual_correlation <- cor(scatter_data$x,scatter_data$y)
    if (-0.5 < actual_correlation & actual_correlation < 0.5) stop("Correlation is too low for linear answers")

    linearity_question <- paste0("Is there a linear relationship between ",
                                 scatter_data$xlab," and ",scatter_data$ylab,"?")
    if (linear) {
        linearity_answers <- "Yes"
        linearity_distractors <- "No"

        direction_question <- paste0("Is the relationship positive or negative?")
        if (actual_correlation > 0) {
            direction_answers <- "Positive"
            direction_distractors <- "Negative"
        } else {
            direction_answers <- "Negative"
            direction_distractors <- "Positive"
        }

        correlation_question <- paste0("Which of these correlations do you think is closest to the true correlation?")
        correlation_answers <- round(actual_correlation,1)
        if (actual_correlation > 0.5) {
            correlation_distractors <- c(round(runif(1,0,0.3),1),
                                         round(runif(1,-0.4,0),1),
                                         round(runif(1,-1,-0.6),1))
        } else if (actual_correlation < -0.5) {
            correlation_distractors <- c(round(runif(1,-0.3,0),1),
                                         round(runif(1,0,0.4),1),
                                         round(runif(1,0.6,1),1))
        }

        linearity_qna <- list(question=linearity_question,
                              answers=linearity_answers,
                              distractors=linearity_distractors)
        direction_qna <- list(question=direction_question,
                              answers=direction_answers,
                              distractors=direction_distractors)
        correlation_qna <- list(question=correlation_question,
                                answers=correlation_answers,
                                distractors=correlation_distractors)
        qna <- list(linearity_qna, direction_qna, correlation_qna)
    } else {
        linearity_answers <- "No"
        linearity_distractors <- "Yes"
        qna <- list(list(question=linearity_question, answers=linearity_answers,
                         distractors=linearity_distractors))
    }
    list(display="",qna=qna, shuffle=TRUE)
}

plot_histogram <- function(hist_data) {
    if(!is.null(dev.list())) dev.off()

    hist(hist_data$df$value, xlab=hist_data$xlab, main=hist_data$plot_title, col="lightblue")

    plots <- list()
    plots[[1]] <- recordPlot()
    plots
}

generate_histogram_question_set <- function(hist_data) {
    value <- hist_data$df$value

    h <- hist(hist_data$df$value, plot=FALSE)
    min_approx <- h$breaks[1]
    max_approx <- tail(h$breaks,1)
    range_approx <- max_approx - min_approx
    skewed <- hist_data$skewed
    question_name <- hist_data$question_name

    mode_question <- paste0("What is the approximate mode for ",question_name,"?")
    mode_bin_idx <- which.max(h$density)
    mode_bin_name <- paste(h$breaks[mode_bin_idx],"to",h$breaks[mode_bin_idx+1])
    mode_answers <- mode_bin_name

    other_bin_idxs <- sample((1:length(h$density))[-mode_bin_idx],2)
    mode_distractors <- sapply(other_bin_idxs, function(idx) {
        paste(h$breaks[idx],"to",h$breaks[idx+1])
    })
    mode_qna <- list(question=mode_question, answers=mode_answers,
                     distractors=mode_distractors)

    min_question <- paste0("What is the approximate minimum ",question_name,"?")
    min_answers <- min_approx
    all_possible_min_distractors <- round(unique(c(max(0,min_approx - range_approx/4),
                                                   max(0,min_approx - range_approx/6),
                                                   min_approx + range_approx/6,
                                                   min_approx + range_approx/4)))
    all_possible_min_distractors <- all_possible_min_distractors[all_possible_min_distractors != min_approx]
    min_distractors <- sample(all_possible_min_distractors, 2)
    min_qna <- list(question=min_question, answers=min_answers,
                    distractors=min_distractors)

    max_question <- paste0("What is the approximate maximum ",question_name,"?")
    max_answers <- max_approx
    all_possible_max_distractors <- round(c(max_approx - range_approx/4,
                                            max_approx - range_approx/6,
                                            max_approx + range_approx/6,
                                            max_approx + range_approx/4))
    max_distractors <- sample(all_possible_max_distractors, 2)
    max_qna <- list(question=max_question, answers=max_answers,
                    distractors=max_distractors)

    use_skewed <- sample(c("skewed","symmetric"),1)
    if (use_skewed == "skewed") {
        skewed_question <- "Is this distribution skewed?"
        skewed_answers <- ifelse(skewed,"Yes","No")
        skewed_distractors <- ifelse(skewed,"No","Yes")
    } else {
        skewed_question <- "Is this distribution approximately symmetric?"
        skewed_answers <- ifelse(skewed,"No","Yes")
        skewed_distractors <- ifelse(skewed,"Yes","No")
    }
    skewed_qna <- list(question=skewed_question, answers=skewed_answers,
                       distractors=skewed_distractors)

    qna <- list(mode_qna, min_qna, max_qna, skewed_qna)
    list(display="",qna=qna, shuffle=TRUE)
}

shuffle_answers <- function(answers) {
    sample(answers, length(answers))
}
