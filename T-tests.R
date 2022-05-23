
findAlpha <- function(difficulty){

    alpha <-  switch(difficulty,
                     0.05,
                     sample(c(0.1, 0.05, 0.01), 1),
                     round(runif(1, 0.01, 0.1), 2))

    return(alpha)

}

findN <- function(difficulty){

    n <- switch(difficulty,
                100,
                (round(runif(1, 6, 15), 0))**2,
                round(runif(1, 30, 250), 0))

    return(n)
}

findMu <- function(number, difficulty){

    mu <- switch(difficulty,
                       switch(number,
                              sample(c(70, 75, 80), 1),
                              sample(c(70, 75, 80), 1),
                              sample(c(70, 75, 80), 1),

                              sample(c(15, 20, 25), 1),
                              sample(c(8, 10, 12), 1),
                              sample(c(15, 20, 25), 1),

                              sample(c(75, 80, 85), 1),
                              sample(c(10, 15, 20), 1),
                              sample(c(75, 80, 85), 1)),
                       switch(number,
                              round(runif(1, 70, 80), 1),
                              round(runif(1, 70, 80), 1),
                              round(runif(1, 70, 80), 1),

                              round(runif(1, 15, 25), 1),
                              round(runif(1, 8, 12), 1),
                              round(runif(1, 15, 25), 1),

                              round(runif(1, 75, 85), 1),
                              round(runif(1, 10, 20), 1),
                              round(runif(1, 75, 85), 1)),
                       switch(number,
                              round(runif(1, 50, 95), 2),
                              round(runif(1, 50, 95), 2),
                              round(runif(1, 50, 95), 2),

                              round(runif(1, 10, 30), 2),
                              round(runif(1, 5, 15), 2),
                              round(runif(1, 10, 30), 2),

                              round(runif(1, 60, 100), 2),
                              round(runif(1, 5, 25), 2),
                              round(runif(1, 60, 120), 2))
    )

    return(mu)
}

findMu2 <- function(number, mu, tail, difficulty){

    mu2 <- sample(c(mu, mu + 0.5*abs(mu), mu - 0.5*abs(mu)),1)

    return(mu2)
}

findXbar <- function(number, mu, tail, difficulty){

    tailVar <- switch(tail,
                      -1,
                      1,
                      sample(c(0.5, -0.5), 1))

    difficultyVar <- switch(difficulty,
                            0.05,
                            sample(c(0.05, 0.1, 0.15), 1),
                            round(runif(1, 0.01, 0.2), 2))

    xbar <- mu*(1 + tailVar*difficultyVar)

    # Confirm that generated number fits context
    xbar <- switch(number,
                   min(xbar, 100),
                   min(xbar, 100),
                   min(xbar, 100),
                   max(xbar, 0),
                   max(xbar, 0),
                   max(xbar, 0),
                   max(xbar, 120),
                   max(xbar, 0),
                   max(xbar, 120))

    round(xbar, (difficulty - 1))
}

findS <- function(number, mu, difficulty){

    difficultyVar <- switch(difficulty,
                            sample(c(0.9, 0.95, 1, 1.05, 1.1), 1),
                            runif(1, 0.9, 1.1),
                            runif(1, 0.8, 1.2))

    numberVar <- switch(number,
                        100 - mu,
                        100 - mu,
                        100 - mu,
                        mu,
                        mu,
                        mu,
                        120 - mu,
                        mu,
                        120 - mu)

    s <- round(difficultyVar*numberVar, max(difficulty - 1, 1))

    return(s)
}

# Tail 1: <
# Tail 2: >
# Tail 3: !=

findTail <- function(){

    tail <- sample(1:3, 1)

}

ParametersMu <- function(number, difficulty){

    alpha <- findAlpha(difficulty)

    n <- findN(difficulty)

    tail <- findTail()

    mu <- findMu(number, difficulty)

    xbar <- findXbar(number, mu, tail, difficulty)

    s <- findS(number, mu, difficulty)

    parameters <- list(n = n, alpha = alpha, xbar = xbar, mu = mu, s = s, tail = tail)

    return(parameters)
}

ParametersP <- function(number, difficulty){

    alpha <- findAlpha(difficulty)

    n <- findN(difficulty)

    tail <- findTail()

    p <- findMu(number, difficulty)

    phat <- findXbar(number, p, tail, difficulty)

    s <- findS(number, p, difficulty)

    parameters <- list(n = n, alpha = alpha, phat = phat, p = p, s = s, tail = tail)

    return(parameters)
}

ParametersDiff <- function(number, difficulty){

    alpha <- findAlpha(difficulty)

    n <- findN(difficulty)
    n2 <- findN(difficulty)

    tail <- findTail()

    mu <- findMu(number, difficulty)
    mu2 <- findMu2(number, mu, tail, difficulty)

    xbar <- findXbar(number, mu, tail, difficulty)
    xbar2 <- findXbar(number, mu2, tail, difficulty)

    s <- findS(number, mu, difficulty)
    s2 <- findS(number, mu2, difficulty)

    sSquared <- s^2
    s2Squared <- s2^2

    parameters <- list(n = n, n2 = n2, alpha = alpha, xbar = xbar, xbar2 = xbar2, s = s, s2 = s2,
                       sSquared = sSquared, s2Squared = s2Squared, tail = tail, mu = mu, mu2 = mu2)

    return(parameters)
}

Parameters <- function(number, type, difficulty){

    parameters <- switch(type,
                         ParametersMu(number, difficulty),
                         ParametersP(number, difficulty),
                         ParametersDiff(number, difficulty)
    )

    return(parameters)
}

questionStage <- function(parameters, type, difficulty, stage){

    solutionMethod <- sample(1:2, 1)

    AnswerArray <- switch(difficulty$Concept,
                          switch(stage,
                                 AskParameter(type),
                                 AskH0(parameters, type, difficulty$Calculation),
                                 AskH1(parameters, type, difficulty$Calculation),
                                 AskTFormula(type),
                                 AskPFormula(parameters, type),
                                 AskT(parameters, type),
                                 AskT(parameters, type),
                                 switch(solutionMethod,
                                        AskRejectionT(parameters, type),
                                        AskRejectionP(parameters, type))),
                          switch(stage,
                                 AskH0(parameters, type, difficulty$Calculation),
                                 AskH1(parameters, type, difficulty$Calculation),
                                 AskTFormula(type),
                                 AskPFormula(parameters, type),
                                 AskT(parameters, type),
                                 AskT(parameters, type),
                                 switch(solutionMethod,
                                        AskRejectionT(parameters, type),
                                        AskRejectionP(parameters, type))),
                          switch(stage,
                                 AskH1(parameters, type, difficulty$Calculation),
                                 AskT(parameters, type),
                                 AskT(parameters, type),
                                 switch(solutionMethod,
                                        AskRejectionT(parameters, type),
                                        AskRejectionP(parameters, type)))
    )


    return(AnswerArray)
}

PrintQuestion <- function(number, type, parameters, questions){

    tailString <- switch(number,
                         switch(parameters$tail,
                                "a worsening",
                                "an improvement",
                                "a difference"),
                         switch(parameters$tail,
                                "a worsening",
                                "an improvement",
                                "a difference"),
                         switch(parameters$tail,
                                "a worsening",
                                "an improvement",
                                "a difference"),
                         switch(parameters$tail,
                                "worse",
                                "better",
                                "differently"),
                         switch(parameters$tail,
                                "only",
                                "at least",
                                "approximately"),
                         switch(parameters$tail,
                                "improved",
                                "worsened",
                                "changed"),
                         switch(parameters$tail,
                                "low",
                                "high",
                                "abnormal"),
                         switch(parameters$tail,
                                "low",
                                "high",
                                "abnormal"),
                         switch(parameters$tail,
                                "low",
                                "high",
                                "abnormal")

    )

    string <- switch(number,
                     paste0(questions[1, number], tailString,
                            questions[2, number], parameters$n,
                            questions[3, number], parameters$alpha*100,
                            questions[4, number], parameters$xbar,
                            questions[5, number], parameters$mu,
                            questions[6, number], parameters$s,
                            questions[7, number]),
                     paste0(questions[1, number], tailString,
                            questions[2, number], parameters$n,
                            questions[3, number], parameters$alpha*100,
                            questions[4, number], parameters$phat,
                            questions[5, number], parameters$p/100,
                            questions[6, number], parameters$s,
                            questions[7, number]),
                     paste0(questions[1, number], tailString,
                            questions[2, number], parameters$n,
                            questions[3, number], parameters$alpha*100,
                            questions[4, number], parameters$xbar,
                            questions[5, number], parameters$s,
                            questions[6, number], parameters$n2,
                            questions[7, number], parameters$xbar2,
                            questions[8, number], parameters$s2,
                            questions[9, number]),
                     paste0(questions[1, number], parameters$n,
                            questions[2, number], parameters$alpha,
                            questions[3, number], tailString,
                            questions[4, number], parameters$xbar,
                            questions[5, number], parameters$mu,
                            questions[6, number], parameters$s,
                            questions[7, number]),
                     paste0(questions[1, number], parameters$n,
                            questions[2, number], parameters$alpha*100,
                            questions[3, number], tailString,
                            questions[4, number], parameters$p,
                            questions[5, number], parameters$s,
                            questions[6, number], round(parameters$phat*parameters$n/100, 0),
                            questions[7, number]),
                     paste0(questions[1, number], parameters$n,
                            questions[2, number], parameters$alpha*100,
                            questions[3, number], parameters$n2,
                            questions[4, number], tailString,
                            questions[5, number], parameters$xbar,
                            questions[6, number], parameters$s,
                            questions[7, number], parameters$xbar2,
                            questions[8, number], parameters$s2,
                            questions[9, number]),
                     paste0(questions[1, number], tailString,
                            questions[2, number], parameters$alpha*100,
                            questions[3, number], parameters$n,
                            questions[4, number], parameters$xbar,
                            questions[5, number], parameters$mu,
                            questions[6, number], parameters$s,
                            questions[7, number]),
                     paste0(questions[1, number], tailString,
                            questions[2, number], parameters$alpha*100,
                            questions[3, number], parameters$n,
                            questions[4, number], parameters$phat,
                            questions[5, number], tailString,
                            questions[6, number], parameters$p,
                            questions[7, number], parameters$s,
                            questions[8, number]),
                     paste0(questions[1, number], tailString,
                            questions[2, number], parameters$alpha*100,
                            questions[3, number], parameters$n,
                            questions[4, number], parameters$n2,
                            questions[5, number], parameters$xbar,
                            questions[6, number], parameters$s,
                            questions[7, number], parameters$xbar2,
                            questions[8, number], parameters$s2,
                            questions[9, number])
    )

    return(noquote(string))
}

calcT <- function(parameters, type) {
    switch(type,
           (parameters$xbar - parameters$mu)/(parameters$s/sqrt(parameters$n)),
           (parameters$phat - parameters$p)/(parameters$s/sqrt(parameters$n)),
           (parameters$xbar - parameters$xbar2)/sqrt(parameters$s^2/parameters$n+parameters$s2^2/parameters$n2)
    )
}

AskParameter <- function(type){
    library("greekLetters")

    parameterNames <- switch(type,
                             c("alpha", "n", "xbar", "mu", "s", "sSquared", "sigma", "sigmaSquared"),
                             c("alpha", "n", "phat", "p", "s", "sSquared", "sigma", "SigmaSquared"),
                             c("alpha", "n", "n2", "xbar", "xbar2", "mu", "mu2", "s", "s2", "sigma", "sigma2"))

    parameterIndex <- sample(1:length(parameterNames), 1)

    QuestionString <- paste0("What is ", switch(type,
                                                switch(parameterIndex,
                                                       greeks("alpha"),
                                                       "n",
                                                       "xbar",
                                                       greeks("mu"),
                                                       "s",
                                                       "s\u00B2",
                                                       greeks("sigma"),
                                                       paste0(greeks("sigma"), "\u00B2")),
                                                switch(parameterIndex,
                                                       greeks("alpha"),
                                                       "n",
                                                       "phat",
                                                       "p",
                                                       "s",
                                                       "s\u00B2",
                                                       greeks("sigma"),
                                                       paste0(greeks("sigma"), "\u00B2")),
                                                switch(parameterIndex,
                                                       greeks("alpha"),
                                                       "n\u2081",
                                                       "n\u2082",
                                                       "xbar\u2081",
                                                       "xbar\u2082",
                                                       paste0(greeks("mu"),"\u2081"),
                                                       paste0(greeks("mu"),"\u2082"),
                                                       "s\u2081",
                                                       "s\u2082",
                                                       paste0(greeks("sigma"),"\u2081"),
                                                       paste0(greeks("sigma"),"\u2082"))),
                             "?")

    Answer1 <- switch(type,
                      switch(parameterIndex,
                             "The significance level",
                             "The sample size",
                             "The sample mean",
                             "The population mean",
                             "The sample standard deviation",
                             "The sample variance",
                             "The population standard deviation",
                             "The population variance"),
                      switch(parameterIndex,
                             "The significance level",
                             "The sample size",
                             "The sample proportion",
                             "The population proportion",
                             "The sample standard deviation",
                             "The sample variance",
                             "The population standard deviation",
                             "The population variance"),
                      switch(parameterIndex,
                             "The significance level",
                             "The sample size of group 1",
                             "The sample size of group 2",
                             "The sample mean of group 1",
                             "The sample mean of group 2",
                             "The population mean of group 1",
                             "The population mean of group 2",
                             "The sample standard deviation of group 1",
                             "The sample standard deviation of group 2",
                             "The population standard deviation of group 1",
                             "The population standard deviation of group 2"))

    if (type %in% c(1,2)) {
        possibleAnswers <- c("The significance level",
                             "The sample size",
                             "The sample mean",
                             "The population mean",
                             "The sample standard deviation",
                             "The sample variance",
                             "The population standard deviation",
                             "The population variance",
                             "The sample proportion",
                             "The population proportion")
        parameterAnswersIndex <- 1:10
    } else if (type == 3) {
        possibleAnswers <- c("The significance level",
                                 "The sample size",
                                 "The sample mean",
                                 "The sample standard deviation",
                                 "The population mean",
                                 "The population standard deviation",
                                 "The sample size of group 1",
                                 "The sample size of group 2",
                                 "The sample mean of group 1",
                                 "The sample mean of group 2",
                                 "The population mean of group 1",
                                 "The population mean of group 2",
                                 "The sample standard deviation of group 1",
                                 "The sample standard deviation of group 2",
                                 "The population standard deviation of group 1",
                                 "The population standard deviation of group 2")
        parameterAnswersIndex <- 1:16
    }

    parameterCorrectAnswerIndex <- switch(type, (1:8)[parameterIndex],
                                          c(1:2,9:10,5:8)[parameterIndex],
                                          c(1,7:16)[parameterIndex])

    parameterWrongAnswersIndex <- sample(parameterAnswersIndex[which(parameterAnswersIndex != parameterCorrectAnswerIndex)], 3)

    Answer2 <- possibleAnswers[parameterWrongAnswersIndex[1]]
    Answer3 <- possibleAnswers[parameterWrongAnswersIndex[2]]
    Answer4 <- possibleAnswers[parameterWrongAnswersIndex[3]]

    AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
    AnswersIndex = sample(c(1, 2, 3, 4), 4)
    Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])

    correctAnswer <- Answer1

    correctAnswerIndex <- which(Answers == correctAnswer)

    answerArray = list(question = QuestionString,
                       answers = Answers,
                       correct = correctAnswer,
                       index = correctAnswerIndex)

    return(answerArray)
}

AskH0 <- function(parameters, type, difficulty){

    QuestionString <- paste0("What is H", "\u2080", "?")

    Answer1 <- switch(type,
                      paste0(greeks("mu"), " = ", parameters$mu),
                      paste0("p", " = ", parameters$p),
                      paste0(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082")
    )

    Answer2 <- switch(type,
                      paste0(greeks("mu"), " < ", parameters$mu),
                      paste0("p", " < ", parameters$p),
                      paste0(greeks("mu"), "\u2081", " < ", greeks("mu"), "\u2082")
    )

    Answer3 <- switch(type,
                      paste0(greeks("mu"), " > ", parameters$mu),
                      paste0("p", " > ", parameters$p),
                      paste0(greeks("mu"), "\u2081", " > ", greeks("mu"), "\u2082")
    )

    Answer4 <- switch(type,
                      paste0(greeks("mu"), " ", greeks("notEqual"), " ", parameters$mu),
                      paste0("p", " ", greeks("notEqual"), " ", parameters$p),
                      paste0(greeks("mu"), "\u2081", " ", greeks("notEqual"), " ", greeks("mu"), "\u2082")
    )

    AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
    AnswersIndex = sample(c(1, 2, 3, 4), 4)
    Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])

    correctAnswer <- Answer1

    correctAnswerIndex <- which(Answers == correctAnswer)

    answerArray = list(question = QuestionString,
                       answers = Answers,
                       correct = correctAnswer,
                       index = correctAnswerIndex)

    return(answerArray)
}

AskH1 <- function(parameters, type, difficulty){

    H0String <- switch(type,
                       paste0(greeks("mu"), " = ", parameters$mu),
                       paste0("p = ", parameters$p),
                       paste0(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082")
    )

    QuestionString <- switch(difficulty,
                             paste0("H", "\u2080", " is ", H0String, ". What is H", "\u2081", "?"),
                             paste0("H", "\u2080", " is ", H0String, ". What is H", "\u2081", "?"),
                             paste0("What is H", "\u2081", "?")
    )

    Answer1 <- switch(type,
                      paste0(greeks("mu"), " = ", parameters$mu),
                      paste0("p = ", parameters$p),
                      paste0(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082")
    )

    Answer2 <- switch(type,
                      paste0(greeks("mu"), " < ", parameters$mu),
                      paste0("p < ", parameters$p),
                      paste0(greeks("mu"), "\u2081", " < ", greeks("mu"), "\u2082")
    )

    Answer3 <- switch(type,
                      paste0(greeks("mu"), " > ", parameters$mu),
                      paste0("p > ", parameters$p),
                      paste0(greeks("mu"), "\u2081", " > ", greeks("mu"), "\u2082")
    )

    Answer4 <- switch(type,
                      paste0(greeks("mu"), " ", greeks("notEqual"), " ", parameters$mu),
                      paste0("p ", greeks("notEqual"), " ", parameters$p),
                      paste0(greeks("mu"), "\u2081", " ", greeks("notEqual"), " ", greeks("mu"), "\u2082")
    )

    AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
    AnswersIndex = sample(c(1, 2, 3, 4), 4)
    Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])

    correctAnswer <- AnswersBase[parameters$tail + 1]
    correctAnswerIndex <- which(Answers == correctAnswer)

    answerArray = list(question = QuestionString,
                       answers = Answers,
                       correct = correctAnswer,
                       index = correctAnswerIndex)

    return(answerArray)
}


AskTFormula <- function(type){

    typeString <- switch(type,
                         "single mean",
                         "proportion",
                         "difference of means")

    QuestionString <- paste0("How do you calculate t for a ", typeString, " t-test?")

    Answer1 <- switch(type,
                      paste0("(xbar - ", greeks("mu"), "\u2080)/(s/\U221an)"),
                      paste0("(sample_p - ", greeks("p"), "\u2080)/(s/\U221an)"),
                      paste0("(xbar\u2081 - xbar\u2082)/\U221a(s\u2081\u00B2/n\u2081+s\u2082\u00B2/n\u2082)")
    )

    Answer2 <- switch(type,
                      paste0("(xbar -",greeks("mu"), " - ", greeks("mu"), "\u2080)/(s\U00b2/n)"),
                      paste0("(sample_p - ", greeks("p"), "\u2080)/(s\U00b2/\U221an)"),
                      paste0("(xbar\u2081 - xbar\u2082)/(s\U00b2/n)")
    )

    Answer3 <- switch(type,
                      paste0("(xbar - ", greeks("mu"), "\u2080)/s"),
                      paste0("(sample_p - ", greeks("p"), "\u2080)/s"),
                      paste0("(xbar\u2081 - xbar\u2082)/(s\u2081 - s\u2082)")
    )

    Answer4 <- switch(type,
                      paste0("xbar/(s\U221an)"),
                      paste0("sample_p/(s\U221an)"),
                      paste0("xbar\u2081/(s\u2081\U221an)")
    )

    AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
    AnswersIndex = sample(c(1, 2, 3, 4), 4)
    Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])

    correctAnswer <- Answer1
    correctAnswerIndex <- which(Answers == correctAnswer)

    answerArray = list(question = QuestionString,
                       answers = Answers,
                       correct = correctAnswer,
                       index = correctAnswerIndex)

    return(answerArray)
}

AskPFormula <- function(parameters, type){

    typeString <- switch(type,
                         "single mean",
                         "proportion",
                         "difference of means")

    QuestionString <- paste0("What does the P-value for a ", typeString, " t-test represent?")

    Tvalue <- switch(type,
                     (parameters$xbar - parameters$mu)/(parameters$s/sqrt(parameters$n)),
                     (parameters$phat - parameters$p)/(parameters$s/sqrt(parameters$n)),
                     (parameters$xbar - parameters$xbar2)/sqrt(parameters$s^2/parameters$n + parameters$s2^2/parameters$n2)
    )

    tailString <- switch(parameters$tail,
                         "<",
                         ">",
                         greeks("notEqual"))

    Answer1 <- switch(type,
                      paste0("P(|t| ", greeks("geq"), " ", signif(abs(Tvalue),3), " | ", greeks("mu")," = ",signif(parameters$xbar,3),")"),
                      paste0("P(|t| ", greeks("geq"), " ", signif(abs(Tvalue),3), " | p = ", signif(parameters$phat,3), ")"),
                      paste0("P(|t| ", greeks("geq"), " ", signif(abs(Tvalue),3), " | ", greeks("mu"), "\u2081-", greeks("mu"), "\u2082 = ", signif(parameters$xbar-parameters$xbar2,3), ")"))

    Answer2 <- switch(type,
                      paste0("P(|t| > ", signif(parameters$xbar,3), " | ", greeks("mu")," = ",signif(parameters$xbar,3),")"),
                      paste0("P(|t| > ", signif(parameters$phat,3), " | p = ", signif(parameters$phat,3), ")"),
                      paste0("P(|t| > ", signif(parameters$xbar2,3), " | ", greeks("mu"), "\u2081-", greeks("mu"), "\u2082 = ", signif(parameters$xbar-parameters$xbar2,3), ")"))

    Answer3 <- switch(type,
                      paste0("P(t ", greeks("geq"), " ", signif(abs(Tvalue),3), " | ", greeks("mu")," ",greeks("notEqual")," ",signif(parameters$xbar,3),")"),
                      paste0("P(t ", greeks("geq"), " ", signif(abs(Tvalue),3), " | p ",greeks("notEqual")," ", signif(parameters$phat,3), ")"),
                      paste0("P(t ", greeks("geq"), " ", signif(abs(Tvalue),3), " | ", greeks("mu"), "\u2081-", greeks("mu"), "\u2082 ",greeks("notEqual")," ", signif(parameters$xbar-parameters$xbar2,3), ")"))

    Answer4 <- switch(type,
                      paste0("P(t = ", signif(abs(Tvalue),3), " | ", greeks("mu")," = ",signif(parameters$xbar,3),")"),
                      paste0("P(t = ", signif(abs(Tvalue),3), " | p = ", signif(parameters$phat,3), ")"),
                      paste0("P(t = ", signif(abs(Tvalue),3), " | ", greeks("mu"), "\u2081-", greeks("mu"), "\u2082 = ", signif(parameters$xbar-parameters$xbar2,3), ")"))

    AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
    AnswersIndex = sample(c(1, 2, 3, 4), 4)
    Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])

    correctAnswer <- Answer1
    correctAnswerIndex <- which(Answers == correctAnswer)

    answerArray <- list(question = QuestionString,
                        answers = Answers,
                        correct = correctAnswer,
                        index = correctAnswerIndex)

    return(answerArray)
}

AskT <- function(parameters, type){

    QuestionString <- paste0('What is T?')

    Answer1 <- signif(calcT(parameters, type),2)

    Answer2 <- signif(sample(c(1, -1), 1)*runif(1, 0.95, 1.1)*Answer1, 2)

    Answer3 <- signif(sample(c(1, -1), 1)*runif(1, 0.9, 1.05)*Answer1, 2)

    Answer4 <- signif(sample(c(1, -1), 1)*runif(1, 0.95, 1.05)*Answer1, 2)

    AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
    AnswersIndex = sample(c(1, 2, 3, 4), 4)
    Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])

    correctAnswer <- Answer1
    correctAnswerIndex <- which(Answers == correctAnswer)

    answerArray <- list(question = QuestionString,
                        answers = Answers,
                        correct = correctAnswer,
                        index = correctAnswerIndex)

    return(answerArray)
}

AskRejectionT <- function(parameters, type){

    t <- calcT(parameters, type)

    criticalProb <- switch(parameters$tail,
                    parameters$alpha,
                    parameters$alpha,
                    parameters$alpha/2)

    criticalT <- round(qnorm(p = criticalProb), 2)

    solution <- ifelse(abs(t) >= abs(criticalT), 1, 2)

    if(parameters$tail == 3){
        criticalT <- paste0("\u00B1", abs(criticalT))}

    QuestionString <- paste0("If the critical value for the test statistic is ", criticalT, ", what is the conclusion?")

    Answer1 <- switch(solution,
                      "Reject H\u2080",
                      "Do not reject H\u2080")

    Answer2 <- switch(solution,
                      "Accept H\u2081",
                      "Accept H\u2080")

    Answer3 <- switch(solution,
                      "Reject H\u2081",
                      "Do not reject H\u2081")

    Answer4 <- switch(solution,
                      "Accept H\u2080",
                      "Accept H\u2081")

    AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
    AnswersIndex = sample(c(1, 2, 3, 4), 4)
    Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])

    correctAnswer <- switch(solution,
                            c(Answer1, Answer2),
                            Answer1)

    correctAnswerIndex <- switch(solution,
                                 c(which(Answers == AnswersBase[1]), which(Answers == AnswersBase[2])),
                                 which(Answers == (AnswersBase[1]))
    )

    answerArray = list(question = QuestionString,
                       answers = Answers,
                       correct = correctAnswer,
                       index = correctAnswerIndex)

    return(answerArray)
}


AskRejectionP <- function(parameters, type){

    t <- calcT(parameters, type)

    criticalP <- switch(parameters$tail,
                    parameters$alpha,
                    parameters$alpha,
                    parameters$alpha/2)

    pval <- signif(1 - pnorm(q = abs(t), lower.tail = FALSE), 2)

    solution <- ifelse(abs(pval) < abs(criticalP), 1, 2)

    QuestionString <- paste0("If the P-value is ", pval, ", what is the conclusion?")

    Answer1 <- switch(solution,
                      "Reject H\u2080",
                      "Do not reject H\u2080")

    Answer2 <- switch(solution,
                      "Accept H\u2081",
                      "Accept H\u2080")

    Answer3 <- switch(solution,
                      "Reject H\u2081",
                      "Do not reject H\u2081")

    Answer4 <- switch(solution,
                      "Accept H\u2080",
                      "Accept H\u2081")

    AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
    AnswersIndex = sample(c(1, 2, 3, 4), 4)
    Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])

    correctAnswer <- switch(solution,
                            c(Answer1, Answer2),
                            Answer1)

    correctAnswerIndex <- switch(solution,
                                 c(which(Answers == AnswersBase[1]), which(Answers == AnswersBase[2])),
                                 which(Answers == (AnswersBase[1]))
    )

    answerArray = list(question = QuestionString,
                       answers = Answers,
                       correct = correctAnswer,
                       index = correctAnswerIndex)

    return(answerArray)
}