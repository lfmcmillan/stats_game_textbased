golf <- read.csv("golf_data.csv", header=TRUE)

sample_size <- 50

generate_questions <- function(seed=1) {

    set.seed(seed)
    results_ok <- FALSE

    while(!results_ok) {
        golf_subset <- golf[sample(1:nrow(golf),sample_size,replace=FALSE),]

        golf_fit <- lm(Final_round ~ Round_1, data=golf_subset)

        if (summary(golf_fit)$coefficients[2,4] < 0.01 | summary(golf_fit)$coefficients[2,4] > 0.1) {
            results_ok <- TRUE
        } else {
            seed <- seed+1
            set.seed(seed)
        }
    }

    beta0 <- summary(golf_fit)$coefficients[1,1]
    beta1 <- summary(golf_fit)$coefficients[2,1]
    beta0_text <- sprintf("%.3f",beta0)
    beta1_text <- sprintf("%.3f",beta1)
    se_beta0 <- summary(golf_fit)$coefficients[1,2]
    se_beta1 <- summary(golf_fit)$coefficients[2,2]
    se_beta0_text <- sprintf("%.3f",se_beta0)
    se_beta1_text <- sprintf("%.3f",se_beta1)
    t_beta0 <- summary(golf_fit)$coefficients[1,3]
    t_beta1 <- summary(golf_fit)$coefficients[2,3]
    t_beta0_text <- sprintf("%.3f",t_beta0)
    t_beta1_text <- sprintf("%.3f",t_beta1)
    correlation <- cor(golf_subset$Round_1,golf_subset$Final_round)
    correlation_text <- sprintf("%.3f",correlation)
    p_value_beta0 <- sprintf("%.3f",summary(golf_fit)$coefficients[1,4])
    p_value_beta1 <- sprintf("%.3f",summary(golf_fit)$coefficients[2,4])

    questions <- vector("character",6)

    questions[1] <- "Which of the following is the correct equation for the regression model?"
    equation_answer <- sample(c(paste("Average Final =",beta0_text,"+",beta1_text,"x Round1"),
                                paste("Predicted Final =",beta0_text,"+",beta1_text,"x Round1")),1)
    equation_distractors_set1 <- c(sample(c(paste("Final =",beta0_text,"+",beta1_text,"Average Round1"),
                                            paste("Final =",beta0_text,"+",beta1_text,"Predicted Round1")),1),
                                   sample(c(paste("Average Round1 =",beta0_text,"+",beta1_text,"x Final"),
                                            paste("Predicted Round1 =",beta0_text,"+",beta1_text,"x Final")),1),
                                   paste("Average Final =",beta0_text,"+",beta1_text,"x Predicted Round1"),
                                   paste("Round1 =",beta0_text,"+",beta1_text,"x Predicted Final"))
    equation_distractors_set2 <- c(paste("Average Final =",beta0_text,"+",beta1_text),
                                   paste("Predicted Final =",beta0_text,"x Round1"),
                                   paste("Average Round1 =",beta0_text,"x Final"),
                                   paste("Average Final =",beta0_text,"+",beta1_text,"x Round1"),
                                   paste("Predicted Round1 =",beta0_text,"+",beta1_text,"x Final"))
    equation_distractors_set3 <- c(paste("Predicted Final =",beta0_text,"+",se_beta0_text,"x Round1"),
                                   paste("Average Final =",beta1_text,"+",se_beta1_text,"x Round1"),
                                   paste("Predicted Final =",correlation_text,"x Round1"),
                                   paste("Average Final =",beta0_text,"+",correlation_text,"x Round1"))

    equation_distractors <- c(sample(equation_distractors_set1,2),
                              sample(equation_distractors_set2,1),
                              sample(equation_distractors_set3,1))

    prediction_ok <- FALSE
    round1_options <- sample(-2:3,6)
    iter <- 1
    while(!prediction_ok & iter <= 6) {
        prediction_round1 <- round1_options[iter]
        prediction_answer <- beta0 + beta1*prediction_round1

        incorrect_prediction <- c(beta1 + beta0*prediction_round1,
                                          beta1 + se_beta1*prediction_round1,
                                          beta0 + se_beta0*prediction_round1,
                                          beta1*prediction_round1,
                                          beta0 + prediction_round1,
                                          beta0 + correlation*prediction_round1,
                                          beta1 + correlation*prediction_round1,
                                          t_beta0 + t_beta1*prediction_round1)

        answers_vec <- c(prediction_answer, incorrect_prediction)
        pairwise_diffs <- vector()
        for (i in 1:8) {
            pairwise_diffs <- c(pairwise_diffs,
                                incorrect_prediction[i]-answers_vec[1:i])
        }
        if (all(abs(pairwise_diffs) > 0.05)) {
            prediction_ok <- TRUE
        }
        iter <- iter + 1
    }
    if (iter > 6 & !prediction_ok) stop("None of the round1 values work for the prediction question. Try another seed.")
    questions[2] <- paste("Predict the Final round score if the Round1 score was",prediction_round1)
    prediction_answer <- sprintf("%.3f",prediction_answer)
    prediction_distractors <- sample(sprintf(rep("%.3f",8),incorrect_prediction),4)

    changes_by_ok <- FALSE
    change_options <- sample(1:4,4)
    iter <- 1
    while(!changes_by_ok & iter <= 4) {
        change <- change_options[iter]
        change_answer <- beta1*change

        incorrect_change <- c(beta0 + beta1*change,
                                      beta1,
                                      beta0*change,
                                      correlation*change,
                                      se_beta0*change,
                                      se_beta1*change,
                                      correlation,
                                      beta1 + beta0*change,
                                      change)

        answers_vec <- c(change_answer, incorrect_change)
        pairwise_diffs <- vector()
        for (i in 1:9) {
            pairwise_diffs <- c(pairwise_diffs,
                                incorrect_change[i]-answers_vec[1:i])
        }
        if (all(abs(pairwise_diffs) > 0.05)) {
            change_ok <- TRUE
        }
        iter <- iter + 1
    }
    if (iter > 4 & !change_ok) stop("None of the change values work for the change question. Try another seed.")
    questions[3] <- paste("If the Round1 score changes by",change,"units, how much will the predicted Final round score change by?")
    change_answer <- sprintf("%.3f",change_answer)
    change_distractors <- sample(sprintf(rep("%.3f",9),incorrect_change),4)

    residual_ok <- FALSE
    residual_options <- sample(1:sample_size,sample_size)
    iter <- 1
    while(!residual_ok & iter <= sample_size) {
        idx <- residual_options[iter]
        residual_round1 <- golf_subset$Round_1[idx]
        residual_predicted <- beta0 + beta1*residual_round1
        residual_final <- golf_subset$Final_round[idx]
        residual_answer <- residual_final - residual_predicted

        incorrect_residual <- c(residual_predicted - residual_final,
                                        residual_predicted,
                                        residual_final - residual_round1,
                                        residual_round1 - residual_final,
                                        residual_final - (beta1*residual_round1),
                                        (beta1*residual_round1) - residual_final,
                                        residual_round1 - (beta0 + beta1*residual_final),
                                        (beta0 + beta1*residual_final) - residual_round1)

        answers_vec <- c(residual_answer, incorrect_residual)
        pairwise_diffs <- vector()
        for (i in 1:8) {
            pairwise_diffs <- c(pairwise_diffs,
                                incorrect_residual[i]-answers_vec[1:i])
        }
        if (all(abs(pairwise_diffs) > 0.05)) {
            residual_ok <- TRUE
        }
        iter <- iter + 1
    }
    if (iter > sample_size & !residual_ok) stop("None of the observation values work for residuals question. Try another seed.")
    questions[4] <- paste("What is the residual for point",idx,"with Round1 =",residual_round1,
                          "and Final =",residual_final)
    residual_answer <- sprintf("%.3f",residual_answer)
    residual_distractors <- sample(sprintf(rep("%.3f",8),incorrect_residual),4)

    if (p_value_beta1 < 0.01) {
        evidence1 <- "strong"
        evidence2 <- ""
        opposite1 <- "no"
        opposite2 <- "not"

    } else {
        evidence1 <- "no"
        evidence2 <- "not"
        opposite1 <- "strong"
        opposite2 <- ""
    }

    correct_interpret <- c(paste("There is",evidence1,"evidence of a linear relationship between Round1 and Final."),
                           paste("At the 5% level of significance, we may claim there is",evidence2,"a linear relationship between Round1 and Final."),
                           paste("We have",evidence1,"evidence that the slope of the true line is not zero."),
                           paste("We have",evidence1,"evidence of a linear association between Round1 and Final."),
                           paste("At the 5% level of significance, we may claim there is",evidence2,"a linear association between Round1 and Final."),
                           paste("There is",evidence1,"evidence that players with different round 1 scores would be expected to have different final round scores, on average."))

    incorrect_interpret <- c("There is evidence of a strong linear relationship between Round1 and Final.",
                               "At the 5% level of significance, we may claim there is a strong linear relationship between Round1 and Final.",
                               paste("There is",opposite1,"evidence of a linear relationship between Round1 and Final."),
                               paste("At the 5% level of significance, we may",opposite2,"claim there is a linear relationship between Round1 and Final."),
                               paste("There is",evidence1,"evidence of no linear relationship between Round1 and Final."),
                               paste("There is",opposite1,"evidence of no linear relationship between Round1 and Final."),
                               paste("We have",opposite1,"evidence that the slope of the true line is not zero."),
                               paste("We have",evidence1,"evidence that the slope of the true line is zero."),
                               paste("We have",opposite1,"evidence of a linear association between Round1 and Final."),
                               paste("At the 5% level of significance, we may",opposite2,"claim there is a linear association between Round1 and Final."),
                               sample(c(paste("We have",evidence1,"evidence that the intercept of the true line is not zero."),
                                        paste("We have",opposite1,"evidence that the intercept of the true line is not zero.")),1,),
                               sample(c(paste("We have",evidence1,"evidence that the players who score 0 on round 1 will also score 0 for the final round."),
                                        paste("We have",opposite1,"evidence that the players who score 0 on round 1 will not score 0 for the final round.")),1))

    excluded_distractors <- list(correct1=3,correct2=4,correct3=c(7,8),
                                 correct4=9,correct5=10,correct6=vector())

    if (runif(1) < 0.7) {
        questions[5] <- paste("Which one of the following is NOT a correct interpretation of the P-value of",p_value_beta1,"in the row for Round1 in the output above?")
        correct_idxs <- sample(1:6,4)
        interpret_distractors <- correct_interpret[correct_idxs]
        incorrect_idxs <- 1:12
        excluded_this_time <- vector()
        for (i in 1:4) {
            if (length(excluded_distractors[[correct_idxs[i]]]) > 0) {
                excluded_this_time <- c(excluded_this_time,excluded_distractors[[correct_idxs[i]]])
            }
        }
        incorrect_idxs <- incorrect_idxs[-excluded_this_time]
        interpret_answer <- incorrect_interpret[sample(incorrect_idxs,1)]
    } else {
        questions[5] <- paste("Which one of the following is a correct interpretation of the P-value of",p_value_beta1,"in the row for Round1 in the output above?")
        correct_idx <- sample(1:6,1)
        interpret_answer <- correct_interpret[correct_idx]
        incorrect_idxs <- 1:12
        if (length(excluded_distractors[[correct_idx]]) > 0) {
            incorrect_idxs <- incorrect_idxs[-excluded_distractors[[correct_idx]]]
        }
        interpret_distractors <- incorrect_interpret[sample(incorrect_idxs,4)]
    }

    intercept_lower <- round(beta0 + se_beta0*qt(0.025,df=sample_size-2),2)
    intercept_upper <- round(beta0 + se_beta0*qt(0.975,df=sample_size-2),2)
    if (intercept_lower < 0) {
        if (intercept_upper < 0) {
            intercept_both <- paste(abs(intercept_lower),"and",abs(intercept_upper),"lower")
            intercept_both_backwards <- paste(abs(intercept_upper),"and",abs(intercept_lower),"higher")
            intercept_both_double <- paste(2*abs(intercept_lower),"and",2*abs(intercept_upper),"lower")
        } else {
            intercept_both <- paste(abs(intercept_lower),"lower and",intercept_upper,"higher")
            intercept_both_backwards <- paste(intercept_upper,"lower and",abs(intercept_lower),"lower")
            intercept_both_double <- paste(2*abs(intercept_lower),"lower and",2*intercept_upper,"higher")
        }
    } else {
        intercept_both <- paste(intercept_lower,"and",intercept_upper,"higher")
        intercept_both_backwards <- paste(intercept_lower,"and",intercept_upper,"lower")
        intercept_both_double <- paste(2*intercept_lower,"and",2*intercept_upper,"higher")
    }
    if (beta0 < 0) {
        intercept_text <- paste(beta0,"lower")
    } else {
        intercept_text <- paste(beta0,"higher")
    }

    slope_upper <- round(beta1 + se_beta1*qt(0.975,df=sample_size-2),2)
    slope_lower <- round(beta1 + se_beta1*qt(0.025,df=sample_size-2),2)
    slope_diff <- slope_upper - slope_lower
    if (slope_lower < 0) {
        if (slope_upper < 0) {
            slope_both <- paste(abs(slope_lower),"and",abs(slope_upper),"lower")
            slope_both_backwards <- paste(abs(slope_upper),"and",abs(slope_lower),"higher")
            slope_both_double <- paste(2*abs(slope_lower),"and",2*abs(slope_upper),"lower")
        } else {
            slope_both <- paste(abs(slope_lower),"lower and",slope_upper,"higher")
            slope_both_backwards <- paste(slope_upper,"lower and",abs(slope_lower),"lower")
            slope_both_double <- paste(2*abs(slope_lower),"lower and",2*slope_upper,"higher")
        }
    } else {
        slope_both <- paste(slope_lower,"and",slope_upper,"higher")
        slope_both_backwards <- paste(slope_lower,"and",slope_upper,"lower")
        slope_both_double <- paste(2*slope_lower,"and",2*slope_upper,"higher")
    }
    if (slope_upper < 0) {
        slope_upper_text <- paste(slope_upper,"lower")
    } else {
        slope_upper_text <- paste(slope_upper,"higher")
    }
    if (beta1 < 0) {
        slope_text <- paste(beta1,"lower")
    } else {
        slope_text <- paste(beta0,"higher")
    }
    if (slope_diff < 0) {
        slope_diff_text <- paste(slope_diff,"lower")
    } else {
        slope_diff_text <- paste(slope_diff,"higher")
    }

    ci_start <- "With 95% confidence, we estimate that"

    correct_ci <- c(paste0("each additional stroke scored in round 1 of the tournament is associated with an average final score being between ",slope_both,"."),
                    paste0("for each stroke lower scored in round 1 of the tournament, the average final score is between ",slope_both_backwards,"."),
                    paste0("the slope of the true regression line is between ",slope_lower," and ",slope_upper,"."),
                    paste0("each additional two strokes scored in round 1 of the tournament are associated with an average final score being between ",slope_both_double,"."),
                    paste0("players who score 0 on round 1 will have an average final score between ",intercept_lower," and ",intercept_upper,"."),
                    paste0("the y-intercept of the true regression line is between ",intercept_lower," and ",intercept_upper,"."))

    incorrect_ci <- c(paste0("each additional stroke scored in round 1 of the tournament is associated with an average final score being between ",slope_both_backwards,"."),
                      paste0("for each stroke lower scored in round 1 of the tournament, average final score is between ",slope_both,"."),
                      paste0("each additional stroke scored in round 1 of the tournament is associated with an average final score being between ",slope_upper_text,"."),
                      paste0("each additional stroke scored in round 1 of the tournament is associated with an average final score being between ",slope_diff_text,"."),
                      paste0("the y-intercept of the true regression line is between ",slope_lower," and ",slope_upper,"."),
                      paste0("players who score 0 on round 1 will have an average final score between ",slope_lower," and ",slope_upper,"."),
                      paste0("players who score between ",intercept_lower," and ",intercept_upper," on round 1 will have an average final score of 0."),
                      paste0("each additional ",slope_both," higher scored in round 1 of the tournament is associated with an average final score being 1 stroke higher."),
                      paste0("each additional stroke scored in round 1 of the tournament is associated with an average final score being between ",intercept_both,"."),
                      paste0("for each stroke lower scored in round 1 of the tournament, average final score is between ",intercept_both,"."),
                      paste0("each additional stroke scored in round 1 of the tournament is associated with an average final score being ",slope_text,"."),
                      paste0("players who score 0 on round 1 will have an average final score of ",intercept_text,"."))

    excluded_distractors <- list(correct1=1,correct2=2,correct3=vector(),
                                 correct4=vector(),correct5=vector(),correct6=vector())

    if (runif(1) < 0.7) {
        questions[6] <- paste("Which one of the following is NOT a correct interpretation of the confidence intervals in the output above?")
        correct_idxs <- sample(1:6,4)
        ci_distractors <- correct_ci[correct_idxs]
        incorrect_idxs <- 1:12
        excluded_this_time <- vector()
        for (i in 1:4) {
            if (length(excluded_distractors[[correct_idxs[i]]]) > 0) {
                excluded_this_time <- c(excluded_this_time,excluded_distractors[[correct_idxs[i]]])
            }
        }
        incorrect_idxs <- incorrect_idxs[-excluded_this_time]
        ci_answer <- incorrect_ci[sample(incorrect_idxs,1)]
    } else {
        questions[5] <- paste("Which one of the following is the best interpretation of the confidence intervals in the output above?")
        correct_idx <- sample(1:6,1)
        ci_answer <- correct_ci[correct_idx]
        incorrect_idxs <- 1:12
        if (length(excluded_distractors[[correct_idx]]) > 0) {
            incorrect_idxs <- incorrect_idxs[-excluded_distractors[[correct_idxs]]]
        }
        ci_distractors <- incorrect_ci[sample(incorrect_idxs,4)]
    }

    answers <- as.data.frame(rbind(c(equation_answer,equation_distractors),
                     c(prediction_answer,prediction_distractors),
                     c(change_answer,change_distractors),
                     c(residual_answer,residual_distractors),
                     c(interpret_answer,interpret_distractors),
                     c(ci_answer,ci_distractors)))

    names(answers) <- c("correct","distractor1","distractor2","distractor3","distractor4")

    display <- as.data.frame(rbind(c("Model","Unstandardized","Unstandardized","Standardized","","","95% Confidence Interval","95% Confidence Interval"),
                     c("","B","Std. Error","Beta","t","Sig.","Lower Bound","Upper Bound"),
                     c("(Constant)",beta0_text,se_beta0_text,"",t_beta0_text,p_value_beta0,intercept_lower,intercept_upper),
                     c("Round1",beta1_text,se_beta1_text,correlation_text,t_beta1_text,p_value_beta1,slope_lower,slope_upper)))

    list(final_seed=seed, final_fit=golf_fit, questions=questions, answers=answers, display=display)
}

generated <- generate_questions(1)

show_questions <- function(generated) {
    print(generated$display)
    for (i in 1:nrow(generated$answers)) {
        displayed_answers <- sample(unlist(generated$answers[i,]),5)
        user_response <- menu(displayed_answers,title=generated$questions[i])
        if (displayed_answers[user_response] == generated$answers[i,1]) {
            print("Yes! Correct answer.")
        } else {
            print("No, wrong answer.")
        }
    }

}