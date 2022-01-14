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
                    samples <- mapply(rtrunc, sizes, spec="exp",
                                      a=rep(params$lower_bound,num),
                                      b=rep(params$upper_bound,num),
                                      1/means, SIMPLIFY=FALSE)
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

    list(display=display, qna=qna)
}

plot_summary_stat_barplots <- function(samples) {
    if(!is.null(dev.list())) dev.off()

    num <- length(samples)
    for (stat in c("mean","median","min","max")) {
        stat_vals <- unlist(lapply(samples, stat))

        barplot(stat_vals, names.arg=names(samples),
                main=stat, ylab=stat, col="darkblue")
    }
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

plot_boxplot <- function(samples, ylab) {
    if(!is.null(dev.list())) dev.off()

    boxplot(samples, names=names(samples), col="lightblue", ylab=ylab)
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

    list(display=display, qna=qna)
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

    list(display=display, qna=qna)
}


# samples <- generate_samples(1, 3)
# generated <- generate_sample_question_set(samples, direction="highest", display_is_plot=TRUE)
# plot_summary_stat_barplots(samples)
# show_questions(generated)
#
# table_stats_samples(samples)
# generated <- generate_sample_question_set(samples, direction="highest", display_is_plot=FALSE)
# show_questions(generated)
#
# samples <- generate_samples(10, 3)
# generated <- generate_boxplot_question_set(samples)
# plot_boxplot(samples, ylab="Amount (mm)")
# show_questions(generated)
