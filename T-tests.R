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

findMuNought <- function(number, difficulty){

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

    return(muNought)
}

findXbar <- function(number, muNought, tail, difficulty){

    tailVar <- switch(tail,
                      -1,
                      1,
                      sample(c(0.5, -0.5), 1))

    difficultyVar <- switch(difficulty,
                            0.05,
                            sample(c(0.05, 0.1, 0.15), 1),
                            round(runif(1, 0.01, 0.2), 2))

    xbar <- muNought*(1 + tailVar*difficultyVar)

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

findXbar2 <- function(number, muNought2, tail, difficulty){

    tail <- switch(tail,
                   2,
                   1,
                   3)

    xbar2 <- findXbar(number, muNought2, tail, difficulty)
    return(xbar2)

}

findS <- function(number, muNought, difficulty){

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

    s <- round(difficultyVar*numberVar, max(difficulty - 1, 1))

    return(s)
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

    muNought <- findMuNought(number, difficulty)

    xbar <- findXbar(number, muNought, tail, difficulty)

    s <- findS(number, muNought, difficulty)

    parameters <- list(n = n, alpha = alpha, xbar = xbar, muNought = muNought, s = s, tail = tail)

    return(parameters)
}

ParametersP <- function(number, difficulty){

    alpha <- findAlpha(difficulty)

    n <- findN(difficulty)

    tail <- findTail(difficulty)

    pNought <- findMuNought(number, difficulty)

    phat <- findXbar(number, pNought, tail, difficulty)

    s <- findS(number, pNought, difficulty)

    parameters <- list(n = n, alpha = alpha, phat = phat, pNought = pNought, s = s, tail = tail)

    return(parameters)
}

ParametersDiff <- function(number, difficulty){

    alpha <- findAlpha(difficulty)

    n <- findN(difficulty)
    n2 <- findN(difficulty)

    tail <- findTail(difficulty)

    muNought <- findMuNought(number, difficulty)
    muNought2 <- findXbar2(number, muNought, tail, difficulty)

    xbar <- findXbar(number, muNought, tail, difficulty)
    xbar2 <- findXbar(number, muNought2, tail, difficulty)

    s <- findS(number, muNought, difficulty)
    s2 <- findS(number, muNought2, difficulty)

    sSquared <- s^2
    s2Squared <- s2^2

    parameters <- list(n = n, n2 = n2, alpha = alpha, xbar = xbar, xbar2 = xbar2, s = s, s2 = s2,
                       sSquared = sSquared, s2Squared = s2Squared, tail = tail)

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
                                     AskT(parameters, type),
                                     if(difficulty$Concept < 3){
                                         AskPFormula(parameters, type)}else(next),
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
                            questions[4, number], parameters$xbar,
                            questions[5, number], parameters$muNought,
                            questions[6, number], parameters$s,
                            questions[7, number]),
                     paste0(questions[1, number], tailString,
                            questions[2, number], parameters$n,
                            questions[3, number], parameters$alpha*100,
                            questions[4, number], parameters$phat,
                            questions[5, number], parameters$pNought/100,
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
                            questions[5, number], parameters$muNought,
                            questions[6, number], parameters$s,
                            questions[7, number]),
                     paste0(questions[1, number], parameters$n,
                            questions[2, number], parameters$alpha*100,
                            questions[3, number], tailString,
                            questions[4, number], parameters$pNought,
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
                            questions[5, number], parameters$muNought,
                            questions[6, number], parameters$s,
                            questions[7, number]),
                     paste0(questions[1, number], tailString,
                            questions[2, number], parameters$alpha*100,
                            questions[3, number], parameters$n,
                            questions[4, number], parameters$phat,
                            questions[5, number], tailString,
                            questions[6, number], parameters$pNought,
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
           (parameters$xbar - parameters$muNought)/(parameters$s/sqrt(parameters$n)),
           (parameters$phat - parameters$pNought)/(parameters$s/sqrt(parameters$n)),
           (parameters$xbar - parameters$xbar2)/sqrt(parameters$s^2/parameters$n+parameters$s2^2/parameters$n)
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
                      paste0(greeks("mu"), " = ", parameters$muNought),
                      paste0("p", " = ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082")
    )

    Answer2 <- switch(type,
                      paste0(greeks("mu"), " < ", parameters$muNought),
                      paste0("p", " < ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " < ", greeks("mu"), "\u2082")
    )

    Answer3 <- switch(type,
                      paste0(greeks("mu"), " > ", parameters$muNought),
                      paste0("p", " > ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " > ", greeks("mu"), "\u2082")
    )

    Answer4 <- switch(type,
                      paste0(greeks("mu"), " ", greeks("notEqual"), " ", parameters$muNought),
                      paste0("p", " ", greeks("notEqual"), " ", parameters$pNought),
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
                       paste0(greeks("mu"), " = ", parameters$muNought),
                       paste0("p = ", parameters$pNought),
                       paste0(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082")
    )

    QuestionString <- switch(difficulty,
                             paste0("H", "\u2080", " is ", H0String, ". What is H", "\u2081", "?"),
                             paste0("H", "\u2080", " is ", H0String, ". What is H", "\u2081", "?"),
                             paste0("What is H", "\u2081", "?")
    )

    Answer1 <- switch(type,
                      paste0(greeks("mu"), " = ", parameters$muNought),
                      paste0("p = ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082")
    )

    Answer2 <- switch(type,
                      paste0(greeks("mu"), " < ", parameters$muNought),
                      paste0("p < ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " < ", greeks("mu"), "\u2082")
    )

    Answer3 <- switch(type,
                      paste0(greeks("mu"), " > ", parameters$muNought),
                      paste0("p > ", parameters$pNought),
                      paste0(greeks("mu"), "\u2081", " > ", greeks("mu"), "\u2082")
    )

    Answer4 <- switch(type,
                      paste0(greeks("mu"), " ", greeks("notEqual"), " ", parameters$muNought),
                      paste0("p ", greeks("notEqual"), " ", parameters$pNought),
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
                     (parameters$xbar - parameters$muNought)/(parameters$s/sqrt(parameters$n)),
                     (parameters$phat - parameters$pNought)/(parameters$s/sqrt(parameters$n)),
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

