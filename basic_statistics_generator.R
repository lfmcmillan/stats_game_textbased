generate_samples <- function(seed=1, num=2, params=NULL, summary_stats=TRUE) {

    set.seed(seed)
    results_ok <- FALSE

    while(!results_ok) {
        if (is.null(params)) {
            sizes <- sample(seq(10,50,10),num, replace=TRUE)

            means <- runif(num, min=0, max=10)
            SDs <- runif(num, min=1, max=10)

            distribution_type <- sample(c("truncated normal","normal","poisson","exponential"),1,prob=c(0.3,0.3,0.2,0.2))
        } else {
            num <- length(params$means)
            distribution_type <- params$distribution
            sizes <- params$sizes
            means <- params$means
            SDs <- params$SDs
        }

        switch (distribution_type,
                "poisson"={
                    samples <- mapply(rpois, sizes, means, SIMPLIFY=FALSE)
                },
                "exponential"={
                    samples <- mapply(rexp, sizes, 1/means, SIMPLIFY=FALSE)
                },
                "truncated exponential"={
                    samples <- mapply(rtexp, sizes, 1/means,
                                      a=rep(params$lower_bound,num),
                                      b=rep(params$upper_bound,num),
                                      SIMPLIFY=FALSE)
                },
                "normal"={
                    samples <- mapply(rnorm, sizes, means, SDs, SIMPLIFY=FALSE)
                },
                "truncated normal"={
                    # The first and second parameters of truncnorm are the
                    # lower and upper bounds for truncation
                    if (is.null(params$lower_bound)) params$lower_bound <- 0
                    if (is.null(params$upper_bound)) params$upper_bound <- Inf
                    samples <- mapply(rtruncnorm, sizes,
                                      rep(params$lower_bound,num),
                                      rep(params$upper_bound,num),
                                      means, SDs, SIMPLIFY=FALSE)
                }
        )

        scale <- mean(abs(unlist(lapply(samples, mean))))/4

        if (!summary_stats) {
            stats_to_check <- "mean"
        } else {
            stats_to_check <- c("mean","median","min","max")
        }
        for (stat in stats_to_check) {
            stat_vals <- unlist(lapply(samples, stat))

            diffs_mat <- outer(stat_vals, stat_vals, "-")

            ## Find all diffs of pairs of different samples
            diffs <- abs(diffs_mat[upper.tri(diffs_mat, diag=FALSE)])

            if (all(diffs > scale)) {
                if (stat == tail(stats_to_check,1)) results_ok <- TRUE
            } else {
                seed <- seed+1
                set.seed(seed)
                break
            }
        }

        if (is.null(params$names)) {
            names(samples) <- paste("Sample",LETTERS[1:num])
        } else {
            names(samples) <- params$names
        }
    }

    samples
}

generate_sample_question <- function(samples, stat="mean", direction="highest",
                                     stat_text=stat) {
    num <- length(samples)

    stat_vals <- unlist(lapply(samples, stat))

    question <- paste0("Which of the samples has the ",direction," ",stat_text,"?")

    if (direction == "highest") {
        answers <- names(samples)[which.max(stat_vals)]
        distractors <- names(samples)[-which.max(stat_vals)]
    } else {
        answers <- names(samples)[which.min(stat_vals)]
        distractors <- names(samples)[-which.min(stat_vals)]
    }

    list(question=question, answers=answers, distractors=distractors)
}

generate_sample_question_set <- function(samples, direction, display_is_plot=TRUE) {
    num <- length(samples)

    qna <- list()

    stats <- sample(c("mean","median","min","max"),4)
    for (i in 1:4) {
        qna[[i]] <- generate_sample_question(samples, stats[i], direction)
    }

    if (display_is_plot) {
        display <- strwrap(paste("There are",num,"samples. The plots show the means,
                    medians, minima and maxima of the samples. Navigate between
                    the plots using the forward and back buttons in the plot
                    window. Use the plots to answer the following questions."), width=100)
    } else {
        display <- strwrap(paste("There are",num,"samples. Use the table of
                                 summary statistics above to answer the following
                                 questions."), width=100)
    }

    list(display=display, qna=qna, shuffle=FALSE)
}

prepare_summary_stat_barplots <- function(samples) {
    lapply(c("mean","median","min","max"), function(stat) {
        stat_vals <- unlist(lapply(samples, stat))
        list(plot_type="summary_bar",y=stat_vals, x=names(samples), title=stat, ylab=stat, xlab="", plot_type="bar")
    })
}

plot_summary_stat_barplots <- function(samples) {
    if(!is.null(dev.list())) dev.off()
    plots <- list()

    num <- length(samples)
    i <- 0
    for (stat in c("mean","median","min","max")) {
        i <- i+1
        stat_vals <- unlist(lapply(samples, stat))

        barplot(stat_vals, names.arg=names(samples),
                main=stat, ylab=stat, col="darkblue")
        plots[[i]] <- recordPlot()
    }
    plots
}

table_stats_samples <- function(samples) {
    stats <- c("mean","median","min","max","sd")
    stat_vals <- lapply(stats, function(stat) {
        round(unlist(lapply(samples, stat)),1)
    })
    tab <- do.call(cbind, stat_vals)
    rownames(tab) <- names(samples)
    colnames(tab) <- c("Mean","Median","Min","Max","SD")
    print(tab)
}

prepare_boxplot <- function(samples, ylab) {
    list(list(plot_type="box",y=samples, x=names(samples), ylab=ylab, xlab="", title="", plot_type="box"))
}

plot_boxplot <- function(samples, ylab) {
    if(!is.null(dev.list())) dev.off()

    boxplot(samples, names=names(samples), col="lightblue", ylab=ylab)

    plots <- list()
    plots[[1]] <- recordPlot()
    plots
}

generate_boxplot_question_set <- function(samples) {
    num <- length(samples)

    qna <- list()

    stats <- sample(c("median","IQR","min","max"),4)

    for (i in 1:4) {
        stat <- stats[i]
        if (stat == 'min') direction <- "lowest"
        else direction <- "highest"
        qna[[i]] <- generate_sample_question(samples, stat, direction)
    }

    display <- strwrap(paste("There are",num,"samples. This is a boxplot of
                              the samples. Use the boxplot to answer the
                              following questions."), width=100)

    list(display=display, qna=qna, shuffle=FALSE)
}

prepare_dotplot <- function(samples, xlab) {
    values <- do.call(c, samples)
    values <- round(values, 1)
    sample_names_list <- lapply(1:length(samples), function(s) {
        rep(names(samples)[s], times=length(samples[[s]]))
    })
    sample_names <- unlist(sample_names_list)

    list(list(plot_type="dot",y=values, x=sample_names, ylab="", xlab=xlab, title="", plot_type="dot"))
}

plot_dotplot <- function(samples, xlab) {
    if(!is.null(dev.list())) dev.off()

    values <- do.call(c, samples)
    values <- round(values, 1)
    sample_names_list <- lapply(1:length(samples), function(s) {
        rep(names(samples)[s], times=length(samples[[s]]))
    })
    sample_names <- do.call(c, sample_names_list)
    df <- data.frame(y=values, g=sample_names)

    stripchart(values ~ sample_names, data=df, xlab=xlab, pch=1, cex=1.3, method="stack")

    plots <- list()
    plots[[1]] <- recordPlot()
    plots
}

generate_dotplot_question_set <- function(samples, direction) {
    num <- length(samples)

    qna <- list()

    random_order <- sample(1:2,2)
    stats <- c("mean","sd")[random_order]
    stats_text <- c("average","spread")[random_order]

    for (i in 1:length(stats)) {
        stat <- stats[i]
        qna[[i]] <- generate_sample_question(samples, stats[i], direction,
                                             stat_text=stats_text[i])
    }

    skewed_question <- "Which sample is the most skewed?"
    means <- unlist(lapply(samples,mean))
    medians <- unlist(lapply(samples,median))
    diffs <- abs(means - medians)
    skewed_answers <- names(samples)[which.max(diffs)]
    skewed_distractors <- names(samples)[-which.max(diffs)]

    qna[[length(stats)+1]] <- list(question=skewed_question, answers=skewed_answers,
                                   distractors=skewed_distractors)

    display <- strwrap(paste("There are",num,"samples. These are dot plots of
                              the samples. Use the dot plots to answer the
                              following questions."), width=100)

    list(display=display, qna=qna, shuffle=FALSE)
}

prepare_scatterplot <- function(scatter_data) {
    list(list(plot_type="scatter",x=scatter_data$x, y=scatter_data$y,
         xlab=scatter_data$xlab, ylab=scatter_data$ylab))
}

plot_scatterplot <- function(scatter_data) {
    if(!is.null(dev.list())) dev.off()

    plot(scatter_data$x, scatter_data$y, xlab=scatter_data$xlab,
         ylab=scatter_data$ylab)

    # plot.new()
    # dev.control("enable")
    plots <- list()
    plots[[1]] <- recordPlot()
    # dev.off()
    plots
}

generate_scatterplot_question_set <- function(scatter_data, linear) {
    actual_correlation <- cor(scatter_data$x,scatter_data$y)
    if (abs(actual_correlation) < 0.5) stop("Correlation is too low for linear answers")

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

prepare_histogram <- function(hist_data) {
    list(list(plot_type="hist",x=hist_data$df$value, xlab=hist_data$xlab, title=hist_data$plot_title))
}

plot_histogram <- function(hist_data) {
    if(!is.null(dev.list())) dev.off()

    hist(hist_data$df$value, xlab=hist_data$xlab, main=hist_data$plot_title, col="lightblue")

    plots <- list()
    plots[[1]] <- recordPlot()
    plots
}

prepare_two_histograms <- function(hist_data) {
    all_vals <- c(hist_data$df1$value,hist_data$df2$value)
    list(list(plot_type="two_hist",x1=hist_data$df1$value, x2=hist_data$df2$value,
              xlab1=hist_data$xlab1, xlab2=hist_data$xlab2,
              xlim=c(min(all_vals),max(all_vals))))
}

plot_two_histograms <- function(hist_data) {
    if(!is.null(dev.list())) dev.off()

    max_val <- max(c(hist_data$df1$value,hist_data$df2$value))
    min_val <- min(c(hist_data$df1$value,hist_data$df2$value))

    par(mfrow=c(2,1))

    hist(hist_data$df1$value, xlab=hist_data$xlab1, main="", col="lightblue", xlim=c(min_val,max_val))
    hist(hist_data$df2$value, xlab=hist_data$xlab2, main="", col="lightblue", xlim=c(min_val,max_val))

    plots <- list()
    plots[[1]] <- recordPlot()

    par(mfrow=c(1,1))
    plots
}

generate_histogram_question_set <- function(hist_data) {
    value <- hist_data$df$value

    h <- hist(value, plot=FALSE)
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

generate_two_histograms_question_set <- function(hist_data) {
    var1 <- hist_data$var1
    var2 <- hist_data$var2
    value1 <- hist_data$df1$value
    value2 <- hist_data$df2$value

    h1 <- hist(value1, plot=FALSE)
    h2 <- hist(value2, plot=FALSE)
    mode1 <- h1$mids[which.max(h1$density)]
    mode2 <- h2$mids[which.max(h2$density)]
    min_approx1 <- h1$breaks[1]
    min_approx2 <- h2$breaks[1]
    max_approx1 <- tail(h1$breaks,1)
    max_approx2 <- tail(h2$breaks,1)
    range_approx1 <- max_approx1 - min_approx1
    range_approx2 <- max_approx2 - min_approx2

    qna <- list()
    i <- 1
    if (mode1 != mode2) {
        switch(sample(1:2,1),
               {
                   mode_question <- paste0("Which has the higher mode?")
                   if (mode1 > mode2) {
                       mode_qna <- list(question=mode_question, answers=var1,
                                        distractors=var2)
                   } else {
                       mode_qna <- list(question=mode_question, answers=var2,
                                        distractors=var1)
                   }
               }, {
                   mode_question <- paste0("Which has the lower mode?")
                   if (mode1 < mode2) {
                       mode_qna <- list(question=mode_question, answers=var1,
                                        distractors=var2)
                   } else {
                       mode_qna <- list(question=mode_question, answers=var2,
                                        distractors=var1)
                   }
               },)
        qna[[i]] <- mode_qna
        i <- i+1
    }

    if (min_approx1 != min_approx2) {

        min_question <- paste0("Which has the lower minimum?")
        if (min_approx1 < min_approx2) {
            min_qna <- list(question=min_question, answers=var1, distractors=var2)
        } else {
            min_qna <- list(question=min_question, answers=var2, distractors=var1)
        }
        qna[[i]] <- min_qna
        i <- i+1
    }

    if (max_approx1 != max_approx2) {
        max_question <- paste0("Which has the higher maximum?")
        if (max_approx1 > max_approx2) {
            max_qna <- list(question=max_question, answers=var1, distractors=var2)
        } else {
            max_qna <- list(question=max_question, answers=var2, distractors=var1)
        }
        qna[[i]] <- max_qna
        i <- i+1
    }

    if (range_approx1 != range_approx2) {
        range_question <- paste0("Which has the greater spread?")
        if (range_approx1 > range_approx2) {
            range_qna <- list(question=range_question, answers=var1, distractors=var2)
        } else {
            range_qna <- list(question=range_question, answers=var2, distractors=var1)
        }
        qna[[i]] <- range_qna
    }

    list(display="",qna=qna, shuffle=FALSE)
}