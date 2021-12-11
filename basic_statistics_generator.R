generate_samples <- function(seed=1, num=2, params=NULL) {

    set.seed(seed)
    results_ok <- FALSE

    while(!results_ok) {
        if (is.null(params)) {
            sizes <- sample(seq(10,50,10),num, replace=TRUE)

            means <- runif(num, min=0, max=10)
            SDs <- runif(num, min=1, max=10)

            distribution_type <- sample(c("poisson","exponential","normal"),1,prob=c(0.3,0.2,0.5))
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
                "normal"={
                    samples <- mapply(rnorm, sizes, means, SDs, SIMPLIFY=FALSE)
                }
        )

        scale <- mean(abs(unlist(lapply(samples, mean))))/4

        for (stat in c("mean","median","min","max")) {
            stat_vals <- unlist(lapply(samples, stat))

            diffs_mat <- outer(stat_vals, stat_vals, "-")

            ## Find all diffs of pairs of different samples
            diffs <- abs(diffs_mat[upper.tri(diffs_mat, diag=FALSE)])

            if (all(diffs > 1)) {
                if (stat == "max") results_ok <- TRUE
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

generate_sample_question <- function(samples, stat="mean", direction="highest") {
    num <- length(samples)

    stat_vals <- unlist(lapply(samples, stat))

    question <- paste0("Which of the samples has the ",direction," ",stat,"?")

    if (direction == "highest") {
        answer <- names(samples)[which.max(stat_vals)]
        distractors <- names(samples)[-which.max(stat_vals)]
    } else {
        answer <- names(samples)[which.min(stat_vals)]
        distractors <- names(samples)[-which.min(stat_vals)]
    }

    list(question=question, answer=answer, distractors=distractors)
}

generate_sample_question_set <- function(samples, direction, display_is_plot=TRUE) {
    num <- length(samples)

    questions <- rep(NA,4)
    answers <- rep(NA,4)
    distractors <- matrix(rep(NA,4*(num-1)),nrow=4)

    stats <- sample(c("mean","median","min","max"),4)

    for (i in 1:4) {
        stat <- stats[i]
        generated_question <- generate_sample_question(samples, stat, direction)
        questions[i] <- generated_question$question
        answers[i] <- generated_question$answer
        distractors[i,] <- generated_question$distractors
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

    list(display=display, questions=questions, answers=cbind(answers,distractors))
}

plot_stats_samples <- function(samples) {
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

show_questions <- function(generated) {
    correct_answer <- rep(NA, nrow(generated$answers))

    writeLines(generated$display)
    for (i in 1:nrow(generated$answers)) {
        # displayed_answers <- sample(unlist(generated$answers[i,]),ncol(generated$answers))
        displayed_answers <- sort(unlist(generated$answers[i,]))
        user_response <- menu(displayed_answers,title=generated$questions[i])
        if (displayed_answers[user_response] == generated$answers[i,1]) {
            correct_answer[i] <- TRUE
            writeLines("Yes! Correct answer.")
        } else {
            correct_answer[i] <- FALSE
            writeLines("No, wrong answer.")
        }
    }

    correct_answer
}

# samples <- generate_samples(1, 3)
# generated <- generate_sample_question_set(samples, direction="highest", display_is_plot=TRUE)
# plot_stats_samples(samples)
# show_questions(generated)
#
# table_stats_samples(samples)
# generated <- generate_sample_question_set(samples, direction="highest", display_is_plot=FALSE)
# show_questions(generated)
