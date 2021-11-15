generate_two_samples <- function(seed=1) {

    set.seed(seed)
    results_ok <- FALSE

    while(!results_ok) {

        sample1_size <- sample(c(10,20,30,40,50),1)
        sample2_size <- sample(c(10,20,30,40,50),1)

        sample1_mean <- runif(1, min=0, max=10)
        sample2_mean <- runif(1, min=0, max=10)

        distribution_type <- sample(c("poisson","exponential","normal"),1,prob=c(0.3,0.2,0.5))

        switch (distribution_type,
                "poisson"={
                    sample1 <- rpois(sample1_size, sample1_mean)
                    sample2 <- rpois(sample2_size, sample2_mean)
                },
                "exponential"={
                    sample1 <- rexp(sample1_size, 1/sample1_mean)
                    sample2 <- rexp(sample2_size, 1/sample2_mean)
                },
                "normal"={
                    sample1_sd <- runif(1, min=1, max=10)
                    sample2_sd <- runif(1, min=1, max=10)

                    sample1 <- rnorm(sample1_size, sample1_mean, sample1_sd)
                    sample2 <- rnorm(sample2_size, sample2_mean, sample2_sd)
                }
        )

        scale <- mean(abs(c(mean(sample1), mean(sample2))))

        mean_diff_prop <- abs(mean(sample1) - mean(sample2))/scale
        median_diff_prop <- abs(median(sample1) - median(sample2))/scale
        min_diff_prop <- abs(min(sample1) - min(sample2))/scale
        max_diff_prop <- abs(max(sample1) - max(sample2))/scale

        diffs <- c(mean_diff_prop, median_diff_prop, min_diff_prop, max_diff_prop)

        if (all(diffs == 0 | diffs > 1)) {
            results_ok <- TRUE
        } else {
            seed <- seed+1
            set.seed(seed)
        }
    }

    list(sample1=sample1, sample2=sample2)
}

generate_two_sample_question <- function(samples, stat="mean") {

    sample1 <- samples$sample1
    sample2 <- samples$sample2

    values <- switch(stat,
                     "mean"=c(mean(sample1),mean(sample2)),
                     "median"=c(median(sample1),median(sample2)),
                     "min"=c(min(sample1),min(sample2)),
                     "max"=c(max(sample1),max(sample2)))

    question <- paste0("Which of the samples has a higher ",stat,"?")
    if (values[1] == values[2]) {
        answer <- paste0("Both have the same ",stat)
        distractors <- c("Sample A","Sample B")
    } else {
        answer <- c("Sample A", "Sample B")[which.max(values)]
        distractors <- c(paste0("Both have the same ",stat),
                         c("Sample A", "Sample B")[which.min(values)])
    }

    list(question=question, answer=answer, distractors=distractors)
}

generate_two_sample_question_set <- function(samples) {
    questions <- rep(NA,4)
    answers <- rep(NA,4)
    distractors <- matrix(rep(NA,8),nrow=4)

    stats <- sample(c("mean","median","min","max"),4)

    for (i in 1:4) {
        stat <- stats[i]
        generated_question <- generate_two_sample_question(samples, stat)
        questions[i] <- generated_question$question
        answers[i] <- generated_question$answer
        distractors[i,] <- generated_question$distractors
    }

    display = "There are two samples, A and B. The plots show the mean, median, minimum and maximum of the two samples. Use the plots to answer the following questions."

    list(display=display, questions=questions, answers=cbind(answers,distractors))
}

plot_stats_two_samples <- function(samples) {
    barplot(c(mean(samples$sample1), mean(samples$sample2)), main="Mean",
            names.arg=c("Sample A", "Sample B"), ylab="Mean", col="darkblue")
    barplot(c(median(samples$sample1), median(samples$sample2)), main="Median",
            names.arg=c("Sample A", "Sample B"), ylab="Median", col="darkblue")
    barplot(c(min(samples$sample1), min(samples$sample2)), main="Minimum value",
            names.arg=c("Sample A", "Sample B"), ylab="Minimum", col="darkblue")
    barplot(c(max(samples$sample1), max(samples$sample2)), main="Maximum value",
            names.arg=c("Sample A", "Sample B"), ylab="Maximum", col="darkblue")
}

generate_samples <- function(num=2, seed=1) {

    set.seed(seed)
    results_ok <- FALSE

    while(!results_ok) {

        sizes <- sample(seq(10,50,10),num)
        means <- runif(num, min=0, max=10)

        distribution_type <- sample(c("poisson","exponential","normal"),1,prob=c(0.3,0.2,0.5))

        switch (distribution_type,
                "poisson"={
                    samples <- mapply(rpois, sizes, means)
                },
                "exponential"={
                    samples <- mapply(rexp, sizes, 1/means)
                },
                "normal"={
                    sds <- runif(num, min=1, max=10)
                    samples <- mapply(rnorm, sizes, means, sds)
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
    }

    samples
}

generate_sample_question <- function(samples, stat="mean") {
    num <- length(samples)

    stat_vals <- unlist(lapply(samples, stat))

    question <- paste0("Which of the samples has the highest ",stat,"?")

    answer <- paste("Sample",LETTERS[1:num])[which.max(stat_vals)]
    distractors <- paste("Sample",LETTERS[1:num])[-which.max(stat_vals)]

    list(question=question, answer=answer, distractors=distractors)
}

generate_sample_question_set <- function(samples) {
    num <- length(samples)

    questions <- rep(NA,4)
    answers <- rep(NA,4)
    distractors <- matrix(rep(NA,4*(num-1)),nrow=4)

    stats <- sample(c("mean","median","min","max"),4)

    for (i in 1:4) {
        stat <- stats[i]
        generated_question <- generate_sample_question(samples, stat)
        questions[i] <- generated_question$question
        answers[i] <- generated_question$answer
        distractors[i,] <- generated_question$distractors
    }

    display <- paste("There are",num,"samples. The plots show the means, medians, minima and maxima of the samples. Use the plots to answer the following questions.")

    list(display=display, questions=questions, answers=cbind(answers,distractors))
}

plot_stats_samples <- function(samples) {
    num <- length(samples)
    for (stat in c("mean","median","min","max")) {
        stat_vals <- unlist(lapply(samples, stat))

        barplot(stat_vals, names.arg=paste("Sample",LETTERS[1:num]),
                main=stat, ylab=stat, col="darkblue")
    }
}

show_questions <- function(generated) {
    print(generated$display)
    for (i in 1:nrow(generated$answers)) {
        # displayed_answers <- sample(unlist(generated$answers[i,]),ncol(generated$answers))
        displayed_answers <- sort(unlist(generated$answers[i,]))
        user_response <- menu(displayed_answers,title=generated$questions[i])
        if (displayed_answers[user_response] == generated$answers[i,1]) {
            print("Yes! Correct answer.")
        } else {
            print("No, wrong answer.")
        }
    }
}

samples <- generate_samples(3, 1)
generated <- generate_sample_question_set(samples)
plot_stats_samples(samples)
show_questions(generated)
