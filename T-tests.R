QuestionSelect <- function(){

    QuestionString <- "Would you like to select a particular question?"
    answers <- c("Yes", "No")

    input <- menu(answers, title = QuestionString)

    if(input == "1"){

        QuestionString <- "Please enter the question number"
        print(noquote(QuestionString))

        while(TRUE){
            input2 = readline()

            ifelse(as.numeric(input2) %in% 1:9,
                   break,
                   print(noquote("Please select a valid option"))
            )
        }
        questionNumber <- input2

    } else(questionNumber <- sample(c(1:9), 1))


    return(as.numeric(questionNumber))

}

difficultyInitialise <- function(){

    QuestionString <- "Which difficulty setting would you like?"
    difficulties <- c("Easy", "Medium", "Hard")

    input <- menu(difficulties, title = QuestionString)

    difficultyAnswer <- input
    difficultyConcept <- difficultyAnswer
    difficultyCalculation <- difficultyAnswer

    QuestionString <- "Would you like your difficulty to automatically change as you answer questions?"
    answers <- c("Yes", "No")

    input2 <- menu(answers, title = QuestionString)

    updateAnswer <- input2

    difficulty <- list(Concept = difficultyConcept,
                       Calculation = difficultyCalculation,
                       UpdateAnswer = updateAnswer)
    return(difficulty)
}

updateDifficultyAuto <- function(difficulty, difficultyTemp){

    difficulty$Concept <- difficulty$Concept + max((difficultyTemp[1] - 2), -1)
    difficulty$Concept <- min(difficulty$Concept, 1)
    difficulty$Concept <- max(difficulty$Concept, 3)

    difficulty$Calculation <- difficulty$Calculation + max((difficultyTemp[1] - 2), -1)
    difficulty$Calculation <- min(difficulty$Calculation, 1)
    difficulty$Calculation <- max(difficulty$Calculation, 3)

    return(difficulty)
}

updateDifficultyManual <- function(difficulty, difficultyTemp){

    QuestionString <- paste0(c("You answered ", difficultyTemp[1], " out of ", difficultyTemp[3], " concept questions correctly. How did you find those questions?"), collapse = "")
    answers <- c("Too easy", "Just right", "Too hard")

    input <- menu(answers, title = QuestionString)

    difficulty$Concept <- difficulty$Concept - (input - 2)
    difficulty$Concept <- min(difficulty$Concept, 1)
    difficulty$Concept <- max(difficulty$Concept, 3)

    QuestionString <- paste0(c("You answered ", difficultyTemp[2], " out of ", difficultyTemp[4], " calculation questions correctly. How did you find those questions?"), collapse = "")
    answers <- c("Too easy", "Just right", "Too hard")

    input2 <- menu(answers, title = QuestionString)

    difficulty$Calculation <- difficulty$Calculation - (input2 - 2)
    difficulty$Calculation <- min(difficulty$Calculation, 1)
    difficulty$Calculation <- max(difficulty$Calculation, 3)


    return(difficulty)
}

findAlpha <- function(difficulty){

    alpha <-  switch(difficulty,
                     0.95,
                     sample(c(0.9, 0.95, 0.99), 1),
                     round(runif(1, 0.8, 1), 2))

    return(alpha)

}

findN <- function(difficulty){

    n <- switch(difficulty,
                100,
                (round(runif(1, 6, 15), 0))**2,
                round(runif(1, 30, 250), 0))

    return(n)
}

findMu0 <- function(number, difficulty){

    muNought <- switch(difficulty,
                       switch(number,
                              sample(c(70, 75, 80), 1),
                              sample(c(70, 75, 80), 1),
                              sample(c(70, 75, 80), 1),

                              sample(c(15, 20, 25), 1),
                              sample(c(8, 10, 12), 1),
                              sample(c(15, 20, 25), 1),

                              sample(c(75, 80, 85), 1),
                              sample(c(10, 15, 20), 1),
                              sample(75, 80, 85), 1),
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

    return(muNought)
}

findMu <- function(number, muNought, tail, difficulty){

    tailVar <- switch(tail,
                      -1,
                      1,
                      sample(c(0.5, -0.5), 1))

    difficultyVar <- switch(difficulty,
                            0.05,
                            sample(c(0.05, 0.1, 0.15), 1),
                            round(runif(1, 0.01, 0.2), 2))

    mu <- muNought*(1 + tailVar*difficultyVar)

    # Confirm that generated number fits context
    mu <- switch(number,
                 min(mu, 100),
                 min(mu, 100),
                 min(mu, 100),
                 max(mu, 0),
                 max(mu, 0),
                 max(mu, 0),
                 max(mu, 120),
                 max(mu, 0),
                 max(mu, 120))

    round(mu, (difficulty - 1))
}

findMu2 <- function(number, mu, tail, difficulty){

    tail <- switch(tail,
                   2,
                   1,
                   3)

    mu2 <- findMu(number, mu, tail, difficulty)
    return(mu2)

}

findSigma <- function(number, muNought, difficulty){

    difficultyVar <- switch(difficulty,
                            sample(c(0.9, 0.95, 1, 1.05, 1.1), 1),
                            runif(1, 0.9, 1.1),
                            runif(1, 0.8, 1.2))

    numberVar <- switch(number,
                        100 - muNought,
                        100 - muNought,
                        100 - muNought,
                        muNought,
                        muNought,
                        muNought,
                        120 - muNought,
                        muNought,
                        120 - muNought)

    sigma <- round(difficultyVar*numberVar, max(difficulty - 1, 1))

    return(sigma)
}

# Tail 1: <
# Tail 2: >
# Tail 3: !=

findTail <- function(difficulty){

    tail <- sample(1:difficulty, 1)

}

ParametersMu <- function(number, difficulty){

    alpha <- findAlpha(difficulty)

    n <- findN(difficulty)

    tail <- findTail(difficulty)

    muNought <- findMu0(number, difficulty)

    mu <- findMu(number, muNought, tail, difficulty)

    sigma <- findSigma(number, muNought, difficulty)

    parameters <- list(n = n, alpha = alpha, mu = mu, muNought = muNought, sigma = sigma, tail = tail)

    return(parameters)
}

ParametersP <- function(number, difficulty){

    alpha <- findAlpha(difficulty)

    n <- findN(difficulty)

    tail <- findTail(difficulty)

    pNought <- findMu0(number, difficulty)

    p <- findMu(number, pNought, tail, difficulty)

    sigma <- findSigma(number, pNought, difficulty)


    parameters <- list(n = n, alpha = alpha, p = p, pNought = pNought, sigma = sigma, tail = tail)

    return(parameters)
}

ParametersDiff <- function(number, difficulty){

    alpha <- findAlpha(difficulty)

    n <- findN(difficulty)
    n2 <- findN(difficulty)

    tail <- findTail(difficulty)

    muNought <- findMu0(number, difficulty)
    muNought2 <- findMu2(number, muNought, tail, difficulty)

    mu <- findMu(number, muNought, tail, difficulty)
    mu2 <- findMu(number, muNought2, tail, difficulty)

    s <- findSigma(number, muNought, difficulty)
    s2 <- findSigma(number, muNought2, difficulty)

    parameters <- list(n = n, n2 = n2, alpha = alpha, muNought = muNought, muNought2 = muNought2, mu = mu, mu2 = mu2, s = s, s2 = s2, tail = tail)

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

Game <- function(number, difficulty, questionsDF){

    type <- ((number - 1)%%3) + 1
    parameters <- Parameters(number, type, difficulty$Calculation)

    print(noquote(""))
    questionString <- strwrap(PrintQuestion(number, type, parameters, questionsDF))
    print(questionString)
    print(noquote(""))

    difficultyTemp <- c(0, 0, 3, 3)
    difficultyTemp[3] <- switch(difficulty$Concept,
                                4,
                                3,
                                0)
    for(stage in 1:7){

        questionType <- switch(stage,
                               1,
                               1,
                               2,
                               1,
                               1,
                               2,
                               2)

        solutionMethod <- sample(1:2, 1)

        correctAnswerArray <- switch(stage,
                                     if(difficulty$Concept < 2){
                                         AskParameter(type)}else(next),
                                     if(difficulty$Concept < 3){
                                         AskH0(parameters, type)}else(next),
                                     AskH1(parameters, type, difficulty$Calculation),
                                     if(difficulty$Concept < 3){
                                         AskTFormula(type)}else(next),
                                     if(difficulty$Concept < 3){
                                         AskPFormula(parameters, type)}else(next),
                                     AskT(parameters, type),
                                     switch(solutionMethod,
                                            AskRejectionT(parameters, type),
                                            AskRejectionP(parameters, type))
        )

        checkArray <- AnswerCheck(correctAnswerArray)

        point <- checkArray$point
        string <- checkArray$string

        print(string)

        difficultyTemp[questionType] <- difficultyTemp[questionType] + point

        print(noquote(""))
    }



    return(difficultyTemp)
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
                           questions[4, number], parameters$mu,
                           questions[5, number], parameters$muNought,
                           questions[6, number], parameters$sigma,
                           questions[7, number]),
                     paste0(questions[1, number], tailString,
                           questions[2, number], parameters$n,
                           questions[3, number], parameters$alpha*100,
                           questions[4, number], parameters$p,
                           questions[5, number], parameters$pNought/100,
                           questions[6, number], parameters$sigma,
                           questions[7, number]),
                     paste0(questions[1, number], tailString,
                           questions[2, number], parameters$n,
                           questions[3, number], parameters$alpha*100,
                           questions[4, number], parameters$mu,
                           questions[5, number], parameters$mu2,
                           questions[6, number],  parameters$sigmaDiff,
                           questions[7, number]),
                     paste0(questions[1, number], parameters$n,
                           questions[2, number], parameters$alpha,
                           questions[3, number], tailString,
                           questions[4, number], parameters$mu,
                           questions[5, number], parameters$muNought,
                           questions[6, number], parameters$sigma,
                           questions[7, number]),
                     paste0(questions[1, number], parameters$n,
                           questions[2, number], parameters$alpha*100,
                           questions[3, number], tailString,
                           questions[4, number], parameters$pNought,
                           questions[5, number], parameters$sigma,
                           questions[6, number], round(parameters$p*parameters$n/100, 0),
                           questions[7, number]),
                     paste0(questions[1, number], parameters$n,
                           questions[2, number], parameters$alpha*100,
                           questions[3, number], tailString,
                           questions[4, number], parameters$mu2,
                           questions[5, number], parameters$mu,
                           questions[6, number], parameters$sigmaDiff,
                           questions[7, number]),
                     paste0(questions[1, number], tailString,
                           questions[2, number], parameters$alpha*100,
                           questions[3, number], parameters$n,
                           questions[4, number], parameters$mu,
                           questions[5, number], parameters$muNought,
                           questions[6, number], parameters$sigma,
                           questions[7, number]),
                     paste0(questions[1, number], tailString,
                           questions[2, number], parameters$alpha*100,
                           questions[3, number], parameters$n,
                           questions[4, number], parameters$p,
                           questions[5, number], tailString,
                           questions[6, number], parameters$pNought,
                           questions[7, number], parameters$sigma,
                           questions[8, number]),
                     paste0(questions[1, number], tailString,
                           questions[2, number], parameters$alpha*100,
                           questions[3, number], parameters$n,
                           questions[4, number], parameters$mu,
                           questions[5, number], parameters$mu2,
                           questions[6, number], parameters$sigmaDiff,
                           questions[7, number])
    )

    return(noquote(string))
}

AnswerCheck <- function(answerArray){

    input <- menu(answerArray$answers, title = answerArray$question)

    result <- ifelse(input %in% answerArray$index, "Correct", "Incorrect")

    correctString <- ifelse(result == "Incorrect",
                            switch(length(answerArray$correct),
                                   paste0("The correct answer is ", answerArray$correct),
                                   paste0("The correct answer is ", answerArray$correct[1], " or ", answerArray$correct[2])),
                            "")

    answerString <- noquote(paste0(result, ". ", correctString))

    checkArray <- list(string = answerString, point = ifelse(result == "Incorrect", 0, 1))
    return(checkArray)

}

calcT <- function(parameters, type) {
    switch(type,
           (parameters$mu - parameters$muNought)/(parameters$sigma/sqrt(parameters$n)),
           (parameters$p - parameters$pNought)/(parameters$sigma/sqrt(parameters$n)),
           (parameters$mu - parameters$mu2)/sqrt(parameters$s^2/parameters$n+parameters$s2^2/parameters$n)
    )
}

AskParameter <- function(type){
    library("greekLetters")

    parameterNames <- switch(type,
                             c("n", "alpha", "mu", "muNought", "sigma", "sigmaSquared"),
                             c("n", "alpha", "p", "pNought", "sigma", "SigmaSquared"),
                             c("n", "alpha", "mu", "mu2", "sigmaDiff", "SigmaSquared"))

    parameterIndex <- sample(1:length(parameterNames), 1)

    QuestionString <- paste0("What is ", switch(type,
                                                  switch(parameterIndex,
                                                         "n",
                                                         greeks("alpha"),
                                                         greeks("mu"),
                                                         paste0(greeks("mu"), "\u2080"),
                                                         greeks("sigma"),
                                                         paste0(greeks("sigma"), "\u00B2")),
                                                  switch(parameterIndex,
                                                         "n",
                                                         greeks("alpha"),
                                                         "p",
                                                         "p\u2080"),
                                                         greeks("sigma"),
                                                         paste0(greeks("sigma"), "\u00B2")),
                                                  switch(parameterIndex,
                                                         "n",
                                                         greeks("alpha"),
                                                         paste0(greeks("mu"), "\u2081"),
                                                         paste0(greeks("mu"), "\u2082"),
                                                         greeks("sigma"),
                                                         paste0(greeks("sigma"), "\u00B2")),
                               "?")

    Answer1 <- switch(type,
                      switch(parameterIndex,
                             "The sample size",
                             "The significance level",
                             "The sample mean",
                             "The mean",
                             "The population standard deviation",
                             "The population variance"),
                      switch(parameterIndex,
                             "The sample size",
                             "The significance level",
                             "The sample proportion",
                             "The proportion",
                             "The population standard deviation",
                             "The population variance"),
                      switch(parameterIndex,
                             "The sample size",
                             "The significance level",
                             "The sample mean of group 1",
                             "The sample mean of group 2",
                             "The population standard deviation",
                             "The population variance"))

    parameterCorrectAnswerIndex <- switch(type,
                                          switch(parameterIndex,
                                                 1,
                                                 2,
                                                 7,
                                                 8,
                                                 5,
                                                 6),
                                          switch(parameterIndex,
                                                 1,
                                                 2,
                                                 9,
                                                 10,
                                                 5,
                                                 6),
                                          switch(parameterIndex,
                                                 1,
                                                 2,
                                                 3,
                                                 4,
                                                 5,
                                                 6))

    parameterAnswers <- c("The sample size", "The significance level", "The sample mean of group 1", "The sample mean of group 2", "The standard deviation", "The variance", "The mean", "The sample mean", "The proportion", "The sample proportion")
    parameterAnswersIndex <- 1:10
    parameterWrongAnswersIndex <- sample(parameterAnswersIndex[which(parameterAnswersIndex != parameterCorrectAnswerIndex)], 3)

    Answer2 <- parameterAnswers[parameterWrongAnswersIndex[1]]

    Answer3 <- parameterAnswers[parameterWrongAnswersIndex[2]]

    Answer4 <- parameterAnswers[parameterWrongAnswersIndex[3]]

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
                      paste0(greeks("mu"), " = ", parameters$muNought),
                      paste0("p\u2080", " = ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082")
    )

    Answer2 <- switch(type,
                      paste0(greeks("mu"), " < ", parameters$muNought),
                      paste0("p\u2080", " < ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " < ", greeks("mu"), "\u2082")
    )

    Answer3 <- switch(type,
                      paste0(greeks("mu"), " > ", parameters$muNought),
                      paste0("p\u2080", " > ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " > ", greeks("mu"), "\u2082")
    )

    Answer4 <- switch(type,
                      paste0(greeks("mu"), " =/= ", parameters$muNought),
                      paste0("p\u2080", " =/= ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " =/= ", greeks("mu"), "\u2082")
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
                       paste0(greeks("mu"), " = ", parameters$muNought),
                       paste0("p\u2080", " = ", parameters$pNought),
                       paste0(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082")
    )

    QuestionString <- switch(difficulty,
                             paste0("H", "\u2080", " is ", H0String, ". What is H", "\u2081", "?"),
                             paste0("H", "\u2080", " is ", H0String, ". What is H", "\u2081", "?"),
                             paste0("What is H", "\u2081", "?")
    )

    Answer1 <- switch(type,
                      paste0(greeks("mu"), " = ", parameters$muNought),
                      paste0("p\u2080", " = ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082")
    )

    Answer2 <- switch(type,
                      paste0(greeks("mu"), " < ", parameters$muNought),
                      paste0("p\u2080", " < ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " < ", greeks("mu"), "\u2082")
    )

    Answer3 <- switch(type,
                      paste0(greeks("mu"), " > ", parameters$muNought),
                      paste0("p\u2080", " > ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " > ", greeks("mu"), "\u2082")
    )

    Answer4 <- switch(type,
                      paste0(greeks("mu"), " =/= ", parameters$muNought),
                      paste0("p\u2080", " =/= ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " =/= ", greeks("mu"), "\u2082")
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
                         greeks("mu"),
                         "proportion",
                         "paired")

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
                         greeks("mu"),
                         "proportion",
                         "paired")

    QuestionString <- paste0("What does the P-value for a ", typeString, " t-test represent?")

    Tvalue <- round(switch(type,
                           (parameters$mu - parameters$muNought)/(parameters$sigma/(parameters$n^0.5)),
                           (parameters$p - parameters$pNought)/(parameters$sigma/(parameters$n^0.5)),
                           (parameters$mu - parameters$mu2)/(parameters$sigmaDiff/(parameters$n^0.5))
    ), 2)

    tailString <- switch(parameters$tail,
                         "<",
                         ">",
                         "=/=")

    Answer1 <- switch(type,
                      paste0("P(|t| >= ", signif(abs(Tvalue),3), " | ", greeks("mu")," = ",signif(parameters$mu,3),")"),
                      paste0("P(|t| >= ", signif(abs(Tvalue),3), " | p = ", signif(parameters$p,3), ")"),
                      paste0("P(|t| >= ", signif(abs(Tvalue),3), " | ", greeks("mu"), "\u2081-", greeks("mu"), "\u2082 = ", signif(parameters$mu-parameters$mu2,3), ")"))

    Answer2 <- switch(type,
                      paste0("P(|t| > ", signif(parameters$mu,3), " | ", greeks("mu")," = ",signif(parameters$mu,3),")"),
                      paste0("P(|t| > ", signif(parameters$p,3), " | p = ", signif(parameters$p,3), ")"),
                      paste0("P(|t| > ", signif(parameters$mu2,3), " | ", greeks("mu"), "\u2081-", greeks("mu"), "\u2082 = ", signif(parameters$mu-parameters$mu2,3), ")"))

    Answer3 <- switch(type,
                      paste0("P(t >= ", signif(abs(Tvalue),3), " | ", greeks("mu")," =/= ",signif(parameters$mu,3),")"),
                      paste0("P(t >= ", signif(abs(Tvalue),3), " | p =/= ", signif(parameters$p,3), ")"),
                      paste0("P(t >= ", signif(abs(Tvalue),3), " | ", greeks("mu"), "\u2081-", greeks("mu"), "\u2082 =/= ", signif(parameters$mu-parameters$mu2,3), ")"))

    Answer4 <- switch(type,
                      paste0("P(t = ", signif(abs(Tvalue),3), " | ", greeks("mu")," = ",signif(parameters$mu,3),")"),
                      paste0("P(t = ", signif(abs(Tvalue),3), " | p = ", signif(parameters$p,3), ")"),
                      paste0("P(t = ", signif(abs(Tvalue),3), " | ", greeks("mu"), "\u2081-", greeks("mu"), "\u2082 = ", signif(parameters$mu-parameters$mu2,3), ")"))

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

    probT <- switch(parameters$tail,
                    1 - parameters$alpha,
                    parameters$alpha,
                    1 - (1 - parameters$alpha)/2)

    criticalT <- round(qnorm(p = probT), 2)

    solution <- ifelse(abs(t) > abs(criticalT), 1, 2)

    if(parameters$tail == 3){
        criticalT <- paste0("\u00B1", abs(criticalT))}

    QuestionString <- paste0("If the critical value for the test statistic is ", criticalT, ", what is the conclusion?")

    Answer1 <- switch(solution,
                      "Reject H\u2080",
                      "Reject H\u2081")

    Answer2 <- switch(solution,
                      "Accept H\u2081",
                      "Accept H\u2080")

    Answer3 <- switch(solution,
                      "Reject H\u2081",
                      "Reject H\u2080")

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

    probP <- switch(parameters$tail,
                    1 - parameters$alpha,
                    1 - parameters$alpha,
                    (1 - parameters$alpha)/2)

    criticalP <- round(pnorm(q = abs(t), lower.tail = FALSE)*100, 2)/100

    solution <- ifelse(abs(probP) > abs(criticalP), 1, 2)

    QuestionString <- paste0("If the P-value is ", criticalP, ", what is the conclusion?")

    Answer1 <- switch(solution,
                      "Reject H\u2080",
                      "Reject H\u2081")

    Answer2 <- switch(solution,
                      "Accept H\u2081",
                      "Accept H\u2080")

    Answer3 <- switch(solution,
                      "Reject H\u2081",
                      "Reject H\u2080")

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

Run <- function(){

    library("greekLetters")
    library("rjson")
    library("shiny")

    difficulty <- difficultyInitialise()
    difficultyOverall <- c(0, 0, 0, 0)

    questions <- data.frame(fromJSON(file = "C:\\Users\\pearsoluke\\Desktop\\Project\\stats_game_textbased\\questions.json"))

    while(TRUE){

        number <- QuestionSelect()

        difficultyTemp <- Game(number, difficulty, questions)
        difficulty <- switch(difficulty$UpdateAnswer,
                             updateDifficultyAuto(difficulty, difficultyTemp),
                             updateDifficultyManual(difficulty, difficultyTemp))

        difficultyOverall <- difficultyTemp + difficultyOverall

        QuestionString <- "Would you like to continue?"
        answers <- c("Yes", "No")

        switch(menu(answers, title = QuestionString),
               next,
               break)
    }

    ConclusionString1 <- paste0(c("You answered ", difficultyOverall[1],
                                  " out of ", difficultyOverall[3], " concept questions correctly."), collapse = "")
    ConclusionString2 <- paste0(c("You also answered ", difficultyOverall[2], " out of ",
                                  difficultyOverall[4], " calculation questions correctly."), collapse = "")

    print(noquote(ConclusionString1))
    print(noquote(ConclusionString2))
}

AnswerCheckShiny <- function(correct, index, input){

    result <- ifelse(input %in% index, "Correct", "Incorrect")

    correctString <- ifelse(result == "Incorrect",
                            switch(length(correct),
                                   paste0(c("The correct answer is ", correct), collapse = ""),
                                   paste0(c("The correct answer is ", correct[1], " or ", correct[2]), collapse = "")),
                            "")

    answerString <- noquote(paste0(c(result, ". ", correctString), collapse = ""))

    checkArray <- list(string = answerString, point = ifelse(result == "Incorrect", 0, 1))
    return(checkArray)

}


serverQuestion <- function(input, output, questionString, answerArray){

    output$context <- renderText(questionString)
    output$question <- renderText(answerArray$question)

    check <- reactiveValues(string = NULL, point = 0)
    id <- NULL

    observeEvent(input$select,{
        check$string <- AnswerCheckShiny(answerArray, input$button)$string

        if(!is.null(id)){
            removeNotification(id)}
        id <- showNotification(renderText(check$string))
    })

    observeEvent(input$close, {
        point <- AnswerCheckShiny(answerArray, input$button)$point
        stopApp(point)})
}

shinyQuestion <- function(questionString, answerArray){

    uiQuestion <- fluidPage(

        textOutput("context"),

        inputPanel(radioButtons("button", label = answerArray$question,
                                choiceNames = answerArray$answers,
                                choiceValues = 1:4,
                                selected = character(0)),

                   actionButton("select", "Select")),
        actionButton("close", "Close")

    )

    appQuestion <- shinyApp(uiQuestion, serverQuestion)
    point <- runApp(appQuestion)

    return(point)
}

serverInitialise <- function(input, output, session){

    difficulty <- reactiveValues(concept = NULL, calculation = NULL, update = FALSE)
    number <- NULL

    updateFalse <- "Select to have your difficulty automatically adjust to your answers"
    updateTrue <- "Select to manually adjust your difficulty"

    observeEvent(input$difficultyUpdate, {
        difficulty$update = !difficulty$update
        if(difficulty$update){
            updateActionButton(inputId = "difficultyUpdate", label = updateTrue)}
        if(!difficulty$update){
            updateActionButton(inputId = "difficultyUpdate", label = updateFalse)}
    })

    observeEvent(input$numberRandom, {
        randomNumber <- sample(1:9, 1)
        updateNumericInput(session = session, inputId = "numberRandom", value = randomNumber)
    })

    observeEvent(input$go, {
        difficulty$concept = input$difficultySelect
        difficulty$calculation = input$difficultySelect
        number <- input$numberSelect

        Initialise <- list(diffConcept = as.numeric(difficulty$concept),
                           diffCalculation = as.numeric(difficulty$calculation),
                           diffUpdate = difficulty$update,
                           number = number)
        stopApp(Initialise)
    })
}

shinyInitialise <- function(){

    uiInitialise <- fluidPage(

        inputPanel(radioButtons("difficultySelect", "Choose a difficulty",
                                choiceNames = c("Easy", "Medium", "Hard"),
                                choiceValues = 1:3, selected = 2),
                   actionButton("difficultyUpdate", "Select to have your difficulty automatically adjust to your answers")),
        inputPanel(numericInput("numberSelect", "Enter question number here", value = 1, min = 1, max = 9),
                   actionButton("numberRandom", "Select to generate a random question number")),
        actionButton("go", "Go"),

    )

    appInitialise <- shinyApp(uiInitialise, serverInitialise)
    initialiseArray <- runApp(appInitialise)

    return(initialiseArray)
}

