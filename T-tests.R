QuestionSelect <- function(){
  
  QuestionString <- "Would you like to select a particular question?"
  answers <- c("Yes", "No")
  
  print(noquote(QuestionString))
  for(i in 1:2){
    print(noquote(paste0(c(i, ". ", answers[i]), collapse = "")))
  }
  
  while(TRUE){
    input = readline()
    
    ifelse(input == "1" || input == "2", 
           break,
           print(noquote("Please select a valid option"))
    )
  }  
  
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
  
  print(noquote(QuestionString))
  for(i in 1:3){
    print(noquote(paste0(c(i, ". ", difficulties[i]), collapse = "")))
  }
  
  while(TRUE){
    input = readline()
    
    ifelse(input == "1" || input == "2" || input == "3", 
           break,
           print(noquote("Please select a valid option"))
    )
  }
  
  difficultyAnswer <- as.numeric(input)
  difficultyConcept <- difficultyAnswer
  difficultyCalculation <- difficultyAnswer
  
  QuestionString <- "Would you like your difficulty to change as you answer questions?"
  answers <- c("Yes", "No")
  
  print(noquote(QuestionString))
  for(i in 1:2){
    print(noquote(paste0(c(i, ". ", answers[i]), collapse = "")))
  }
  
  while(TRUE){
    input = readline()
    
    ifelse(input == "1" || input == "2", 
           break,
           print(noquote("Please select a valid option"))
    )
  }
  
  updateAnswer <- as.numeric(input)
  
  difficulty <- data.frame(c(difficultyConcept, difficultyCalculation, updateAnswer), row.names = c("Concept", "Calculation", "UpdateAnswer"))
  return(difficulty)
}

updateDifficultyAuto <- function(difficulty, difficultyTemp){
  
  difficulty["Concept",] <- difficulty["Concept",] + min((difficultyTemp[1] - 2), -1)
  difficulty["Concept",] <- min(difficulty["Concept",], 1)
  difficulty["Concept",] <- max(difficulty["Concept",], 3)
  
  difficulty["Calculation",] <- difficulty["Calculation",] + min((difficultyTemp[1] - 2), -1)
  difficulty["Calculation",] <- min(difficulty["Calculation",], 1)
  difficulty["Calculation",] <- max(difficulty["Calculation",], 3)
  
  return(difficulty)
}

updateDifficultyManual <- function(difficulty, difficultyTemp){
  
  QuestionString <- paste0(c("You answered ", difficultyTemp[1], " out of ", difficultyTemp[3], " concept questions correctly. How did you find those questions?"), collapse = "")
  answers <- c("Too easy", "Just right", "Too hard")
  
  print(noquote(QuestionString))
  for(i in 1:3){
    print(noquote(paste0(c(i, ". ", answers[i]), collapse = "")))
  }
  
  while(TRUE){
    input = readline()
    
    ifelse(input == "1" || input == "2" || input == "3", 
           break,
           print(noquote("Please select a valid option"))
    )
  }  
  
  difficulty["Concept",] <- difficulty["Concept",] - (as.numeric(input) - 2)
  difficulty["Concept",] <- min(difficulty["Concept",], 1)
  difficulty["Concept",] <- max(difficulty["Concept",], 3)
  
  QuestionString <- paste0(c("You answered ", difficultyTemp[2], " out of ", difficultyTemp[4], " calculation questions correctly. How did you find those questions?"), collapse = "")
  answers <- c("Too easy", "Just right", "Too hard")
  
  print(noquote(QuestionString))
  for(i in 1:3){
    print(noquote(paste0(c(i, ". ", answers[i]), collapse = "")))
  }
  
  while(TRUE){
    input = readline()
    
    ifelse(input == "1" || input == "2" || input == "3", 
           break,
           print(noquote("Please select a valid option"))
    )
    
    difficulty["Calculation",] <- difficulty["Calculation",] - (as.numeric(input) - 2)
    difficulty["Calculation",] <- min(difficulty["Calculation",], 1)
    difficulty["Calculation",] <- max(difficulty["Calculation",], 3)
  }  
  
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
  
  muNaught <- switch(difficulty,
                     switch(number,
                            sample(c(70, 75, 80), 1),
                            sample(c(70, 75, 80), 1),
                            sample(c(70, 75, 80), 1),
                            sample(c(15, 20, 25), 1),
                            sample(c(8, 10, 12), 1),
                            sample(c(15, 20, 25), 1)),
                     switch(number,
                            round(runif(1, 70, 80), 1),
                            round(runif(1, 70, 80), 1),
                            round(runif(1, 70, 80), 1),
                            round(runif(1, 15, 25), 1),
                            round(runif(1, 8, 12), 1),
                            round(runif(1, 15, 25), 1)),
                     switch(number,
                            round(runif(1, 50, 95), 2),
                            round(runif(1, 50, 95), 2),
                            round(runif(1, 50, 95), 2),
                            round(runif(1, 10, 30), 2),
                            round(runif(1, 5, 15), 2),
                            round(runif(1, 10, 30), 2))
  )
  
  return(muNaught)
}

findMu <- function(number, muNaught, tail, difficulty){
  
  mu <- switch(difficulty,
               switch(number,
                            switch(tail,
                                   muNaught - (0.05*muNaught),
                                   muNaught + (0.05*muNaught),
                                   muNaught + sample(c((0.05*muNaught), -(0.05*muNaught)), 1)),
                            switch(tail,
                                   muNaught - (0.05*muNaught),
                                   muNaught + (0.05*muNaught),
                                   muNaught + sample(c((0.05*muNaught), -(0.05*muNaught)), 1)),
                            switch(tail,
                                   muNaught + (0.05*muNaught),
                                   muNaught - (0.05*muNaught),
                                   muNaught + sample(c((0.05*muNaught), -(0.05*muNaught)), 1)),
                            switch(tail,
                                   muNaught - (0.1*muNaught),
                                   muNaught + (0.1*muNaught),
                                   muNaught + sample(c((0.1*muNaught), -(0.1*muNaught)), 1)),
                            switch(tail,
                                   muNaught - (0.1*muNaught),
                                   muNaught + (0.1*muNaught),
                                   muNaught + sample(c((0.1*muNaught), -(0.1*muNaught)), 1)),
                            switch(tail,
                                   muNaught + (0.1*muNaught),
                                   muNaught - (0.1*muNaught),
                                   muNaught + sample(c((0.1*muNaught), -(0.1*muNaught)), 1))),
               switch(number,
                            switch(tail,
                                   (muNaught - (sample(c(0.01, 0.05, 0.1), 1)*muNaught)),
                                   (muNaught + (sample(c(0.01, 0.05, 0.1), 1)*muNaught)),
                                   (muNaught + sample(c((sample(c(0.01, 0.05, 0.1), 1)*muNaught), -(sample(c(0.01, 0.05, 0.1), 1)*muNaught)), 1))),
                            switch(tail,
                                   (muNaught - (sample(c(0.01, 0.05, 0.1), 1)*muNaught)),
                                   (muNaught + (sample(c(0.01, 0.05, 0.1), 1)*muNaught)),
                                   (muNaught + sample(c((sample(c(0.01, 0.05, 0.1), 1)*muNaught), -(sample(c(0.01, 0.05, 0.1), 1)*muNaught)), 1))),
                            switch(tail,
                                   (muNaught + (sample(c(0.01, 0.05, 0.1), 1)*muNaught)),
                                   (muNaught - (sample(c(0.01, 0.05, 0.1), 1)*muNaught)),
                                   (muNaught + sample(c((sample(c(0.01, 0.05, 0.1), 1)*muNaught), -(sample(c(0.01, 0.05, 0.1), 1)*muNaught)), 1))),
                            switch(tail,
                                   (muNaught - (sample(c(0.05, 0.1, 0.15), 1)*muNaught)),
                                   (muNaught + (sample(c(0.05, 0.1, 0.15), 1)*muNaught)),
                                   (muNaught + sample(c((sample(c(0.05, 0.1, 0.15), 1)*muNaught), -(sample(c(0.01, 0.05, 0.1), 1)*muNaught)), 1))),
                            switch(tail,
                                   (muNaught - (sample(c(0.05, 0.1, 0.15), 1)*muNaught)),
                                   (muNaught + (sample(c(0.05, 0.1, 0.15), 1)*muNaught)),
                                   (muNaught + sample(c((sample(c(0.05, 0.1, 0.15), 1)*muNaught), -(sample(c(0.01, 0.05, 0.1), 1)*muNaught)), 1))),
                            switch(tail,
                                   (muNaught + (sample(c(0.05, 0.1, 0.15), 1)*muNaught)),
                                   (muNaught - (sample(c(0.05, 0.1, 0.15), 1)*muNaught)),
                                   (muNaught + sample(c((sample(c(0.05, 0.1, 0.15), 1)*muNaught), -(sample(c(0.01, 0.05, 0.1), 1)*muNaught)), 1)))),
               switch(number,
                            switch(tail,
                                   (muNaught - runif(1, 0.01, 0.1)*muNaught),
                                   (muNaught + runif(1, 0.01, 0.1)*muNaught),
                                   (muNaught + sample(c(runif(1, 0.01, 0.1), -runif(1, 0.01, 0.1)), 1)*muNaught)),
                            switch(tail,
                                   (muNaught - runif(1, 0.01, 0.1)*muNaught),
                                   (muNaught + runif(1, 0.01, 0.1)*muNaught),
                                   (muNaught + sample(c(runif(1, 0.01, 0.1), -runif(1, 0.01, 0.1)), 1)*muNaught)),
                            switch(tail,
                                   (muNaught + runif(1, 0.01, 0.1)*muNaught),
                                   (muNaught - runif(1, 0.01, 0.1)*muNaught),
                                   (muNaught + sample(c(runif(1, 0.01, 0.1), -runif(1, 0.01, 0.1)), 1)*muNaught)),
                            switch(tail,
                                   (muNaught - runif(1, 0.05, 0.15)*muNaught),
                                   (muNaught + runif(1, 0.05, 0.15)*muNaught),
                                   (muNaught + sample(c(runif(1, 0.05, 0.15), -runif(1, 0.01, 0.1)), 1)*muNaught)),
                            switch(tail,
                                   (muNaught - runif(1, 0.05, 0.15)*muNaught),
                                   (muNaught + runif(1, 0.05, 0.15)*muNaught),
                                   (muNaught + sample(c(runif(1, 0.05, 0.15), -runif(1, 0.01, 0.1)), 1)*muNaught)),
                            switch(tail,
                                   (muNaught + runif(1, 0.05, 0.15)*muNaught),
                                   (muNaught - runif(1, 0.05, 0.15)*muNaught),
                                   (muNaught + sample(c(runif(1, 0.05, 0.15), -runif(1, 0.01, 0.1)), 1)*muNaught)))
  )
  
  # Confirm that generated number fits context
  mu <- switch(number,
               min(mu, 100),
               min(mu, 100),
               min(mu, 100),
               max(mu, 0),
               max(mu, 0),
               max(mu, 0))
  
  round(as.numeric(mu), as.numeric(difficulty - 1))
}

findSigma <- function(number, muNaught, difficulty){
  
  sigma <- switch(difficulty,
                  switch(number,
                         round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(100 - muNaught)/3, 1),
                         round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(100 - muNaught)/3, 1),
                         round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(muNaught)/3, 1),
                         round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(muNaught)/3, 1),
                         round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(muNaught)/3, 1),
                         round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(muNaught)/3, 1)),
                  switch(number,
                         round(runif(1, 0.9, 1.1)*(100 - muNaught)/3, 1),
                         round(runif(1, 0.9, 1.1)*(100 - muNaught)/3, 1),
                         round(runif(1, 0.9, 1.1)*(muNaught)/3, 1),
                         round(runif(1, 0.9, 1.1)*(muNaught)/3, 1),
                         round(runif(1, 0.9, 1.1)*(muNaught)/3, 1),
                         round(runif(1, 0.9, 1.1)*(muNaught)/3, 1)),
                  switch(number,
                         round(runif(1, .9, 1.1)*(100 - muNaught)/3, 2),
                         round(runif(1, .9, 1.1)*(100 - muNaught)/3, 2),
                         round(runif(1, .9, 1.1)*(muNaught)/3, 2),
                         round(runif(1, .9, 1.1)*(muNaught)/3, 2),
                         round(runif(1, .9, 1.1)*(muNaught)/3, 2),
                         round(runif(1, .9, 1.1)*(muNaught)/3, 2))
  )
  
  return(sigma)
}

# Tail 1: <
# Tail 2: >
# Tail 3: !=

findTail <- function(number, difficulty){
  
  tail <- switch(difficulty,
                 1,
                 sample(c(1,2), 1),
                 sample(c(1,2,3), 1)
  )
  
  return(tail)
  
}

ParametersMu <- function(number, difficulty){
  
  alpha <- findAlpha(difficulty)
  
  n <- findN(difficulty)
  
  tail <- findTail(number, difficulty)
  
  muNaught <- findMu0(number, difficulty)
  
  mu <- findMu(number, muNaught, tail, difficulty)
  
  sigma <- findSigma(number, muNaught, difficulty)
  
  parameters <- data.frame(c(n, alpha, mu, muNaught, sigma, tail), 
   row.names = c("n", "alpha", "mu", "muNaught", "sigma", "tail"))
  
  return(parameters)
}

ParametersP <- function(number, difficulty){
  
  alpha <- findAlpha(difficulty)
  
  n <- findN(difficulty)
  
  tail <- findTail(number, difficulty)
  
  pNaught <- findMu0(number, difficulty)
  
  p <- findMu(number, pNaught, tail, difficulty)
  
  sigma <- findSigma(number, pNaught, difficulty)
  
  
  parameters <- data.frame(c(n, alpha, p/100, pNaught/100, sigma/100, tail), 
                           row.names = c("n", "alpha", "p", "pNaught", "sigma", "tail"))
  return(parameters)
}

ParametersDiff <- function(number, difficulty){
  
  alpha <- findAlpha(difficulty)
  
  n <- findN(difficulty)
  
  tail <- findTail(number, difficulty)
  
  muNaught <- findMu0(number, difficulty)
  
  mu <- findMu(number, muNaught, tail, difficulty)
  
  mu2 <- findMu(number, mu, tail, difficulty)
  
  sigmaDiff <- findSigma(number, muNaught, difficulty)
  
  parameters <- data.frame(c(n, alpha, mu, mu2, sigmaDiff, tail), 
                           row.names = c("n", "alpha", "mu", "mu2", "sigmaDiff", "tail")) 
  
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

Game <- function(number, difficulty){
  
  type <- ((number - 1)%%3) + 1
  parameters <- Parameters(number, type, difficulty["Calculation",])
  
  print(noquote(""))
  print(PrintQuestion(number, type, parameters))
  print(noquote(""))
  
  difficultyTemp <- c(0, 0, 3, 3)
  difficultyTemp[3] <- switch(difficulty["Concept",],
                              3,
                              2,
                              0)
  for(stage in 1:6){
    
    questionType <- switch(stage,
                           1,
                           1,
                           2,
                           1,
                           2,
                           2)
    
    correctAnswerArray <- switch(stage,
                                 if(difficulty["Concept",] < 2){
                                        AskParameter(type)}else(next),
                                 if(difficulty["Concept",] < 3){
                                        AskH0(parameters, type)}else(next),
                                 AskH1(parameters, type, difficulty["Calculation",]),
                                 if(difficulty["Concept",] < 3){
                                        AskTFormula(type)}else(next),
                                 AskT(parameters, type),
                                 AskRejection(parameters, type))

    point <- AnswerCheck(correctAnswerArray)
    
    difficultyTemp[questionType] <- difficultyTemp[questionType] + point
    
    print(noquote(""))
  }
  
  
  
  return(difficultyTemp)
}


PrintQuestion <- function(number, type, parameters){
  
  tailString <- switch(number,
                       switch(parameters["tail",],
                              "a worsening",
                              "an improvement",
                              "a difference"),
                       switch(parameters["tail",],
                              "a worsening",
                              "an improvement",
                              "a difference"),
                       switch(parameters["tail",],
                              "a worsening",
                              "an improvement",
                              "a difference"),
                       switch(parameters["tail",],
                              "worse",
                              "better",
                              "differently"),
                       switch(parameters["tail",],
                              "only",
                              "at least",
                              "approximately"),
                       switch(parameters["tail",],
                              "improved",
                              "worsened",
                              "changed")
  )
  
  string <- switch(number,
                   paste0(
                     c(questions[1, number], tailString,
                       questions[2, number], parameters["n",],
                       questions[3, number], parameters["alpha",]*100,
                       questions[4, number], parameters["mu",],
                       questions[5, number], parameters["muNaught",],
                       questions[6, number], parameters["sigma",], 
                       questions[7, number]), collapse = ""),
                   paste0(
                     c(questions[1, number], tailString,
                       questions[2, number], parameters["n",],
                       questions[3, number], parameters["alpha",]*100,
                       questions[4, number], parameters["p",]*100,
                       questions[5, number], parameters["pNaught",],
                       questions[6, number], parameters["sigma",], 
                       questions[7, number]), collapse = ""),
                   paste0(
                     c(questions[1, number], tailString,
                       questions[2, number], parameters["n",],
                       questions[3, number], parameters["alpha",]*100,
                       questions[4, number], parameters["mu",],
                       questions[5, number], parameters["mu2",],
                       questions[6, number],  parameters["sigmaDiff",], 
                       questions[7, number]), collapse = ""),
                   paste0(
                     c(questions[1, number], parameters["n",],
                       questions[2, number], parameters["alpha",],
                       questions[3, number], tailString,
                       questions[4, number], parameters["mu",],
                       questions[5, number], parameters["muNaught",],
                       questions[6, number], parameters["sigma",],
                       questions[7, number]), collapse = ""),
                   paste0(
                     c(questions[1, number], parameters["n",],
                       questions[2, number], parameters["alpha",]*100,
                       questions[3, number], tailString, " ", parameters["pNaught",]*100,
                       questions[4, number], parameters["sigma",],
                       questions[5, number], round(parameters["p",]*parameters["n",], 0),
                       questions[6, number]), collapse = ""),
                   paste0(
                     c(questions[1, number], parameters["n",],
                       questions[2, number], parameters["alpha",]*100,
                       questions[3, number], tailString,
                       questions[4, number], parameters["mu2",],
                       questions[5, number], parameters["mu",], 
                       questions[6, number], parameters["sigmaDiff",], 
                       questions[7, number]), collapse = "")
  )
  
  return(noquote(string))
}

AnswerCheck <- function(answerArray){
  
  input <- readline()
  
  result <- ifelse(input %in% answerArray[2:length(answerArray)], "Correct", "Incorrect")
  
  answerString <- ifelse(result == "Incorrect",
                        paste0(c("The correct answer is ", answerArray[1]), collapse = ""),
                         "")
  
  print(noquote(paste0(c(result, ". ", answerString), collapse = "")))
  
  return(ifelse(result == "Incorrect", 0, 1))
  
}

AskParameter <- function(type){
  library("greekLetters")
  
  parameterNames <- switch(type,
                           c("n", "alpha", "mu", "muNaught", "sigma", "sigmaSquared"),
                           c("n", "alpha", "p", "pNaught", "sigma", "SigmaSquared"),
                           c("n", "alpha", "mu", "mu2", "sigmaDiff", "SigmaSquared"))
  
  parameterIndex <- sample(1:length(parameterNames), 1)
  
  
  
  QuestionString <- paste0(c("What is ", switch(type,
                                                switch(parameterIndex,
                                                       "n",
                                                       greeks("alpha"),
                                                       greeks("mu"),
                                                       paste0(c(greeks("mu"), "\u2080"), collapse = ""),
                                                       greeks("sigma"),
                                                       paste0(c(greeks("sigma"), "\u00B2"), collapse = "")),
                                                switch(parameterIndex,
                                                       "n",
                                                       greeks("alpha"),
                                                       "p",
                                                       paste0(c("p", "\u2080"), collapse = ""),
                                                       greeks("sigma"),
                                                       paste0(c(greeks("sigma"), "\u00B2"), collapse = "")),
                                                switch(parameterIndex,
                                                       "n",
                                                       greeks("alpha"),
                                                       paste0(c(greeks("mu"), "\u2081"), collapse = ""),
                                                       paste0(c(greeks("mu"), "\u2082"), collapse = ""),
                                                       greeks("sigma"),
                                                       paste0(c(greeks("sigma"), "\u00B2"), collapse = ""))),
                             "?"), collapse = "")
  
  Answer1 <- switch(type,
                    switch(parameterIndex,
                           "The sample size",
                           "The significance level",
                           "The sample mean",
                           "The mean",
                           "The standard deviation",
                           "The variance"),
                    switch(parameterIndex,
                           "The sample size",
                           "The significance level",
                           "The sample proportion",
                           "The proportion",
                           "The standard deviation",
                           "The variance"),
                    switch(parameterIndex,
                           "The sample size",
                           "The significance level",
                           "The sample mean of group 1",
                           "The sample mean of group 2",
                           "The standard deviation",
                           "The variance"))
  
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
  
  print(noquote(QuestionString))
  
  for(i in 1:4){
    print(noquote(paste0(i, ". ", Answers[i])))
  }
  
  return(c(correctAnswer, correctAnswerIndex))
}

AskH0 <- function(parameters, type, difficulty){
  
  QuestionString <- paste0(c("What is H", "\u2080", "?"), collapse = "")
  
  
  Answer1 <- switch(type,
                    paste0(
                      c(greeks("mu"), " = ", parameters["muNaught",]), collapse = ""),
                    paste0(
                      c("p", " = ", parameters["pNaught",]), collapse = ""),
                    paste0(
                      c(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082"), collapse = "")
  )
  
  Answer2 <- switch(type,
                    paste0(
                      c(greeks("mu"), " < ", parameters["muNaught",]), collapse = ""),
                    paste0(
                      c("p", " < ", parameters["pNaught",]), collapse = ""),
                    paste0(
                      c(greeks("mu"), "\u2081", " < ", greeks("mu"), "\u2082"), collapse = "")
  )
  
  Answer3 <- switch(type,
                    paste0(
                      c(greeks("mu"), " > ", parameters["muNaught",]), collapse = ""),
                    paste0(
                      c("p", " > ", parameters["pNaught",]), collapse = ""),
                    paste0(
                      c(greeks("mu"), "\u2081", " > ", greeks("mu"), "\u2082"), collapse = "")
  )
  
  Answer4 <- switch(type,
                    paste0(
                      c(greeks("mu"), " =/= ", parameters["muNaught",]), collapse = ""),
                    paste0(
                      c("p", " =/= ", parameters["pNaught",]), collapse = ""),
                    paste0(
                      c(greeks("mu"), "\u2081", " =/= ", greeks("mu"), "\u2082"), collapse = "")
  )
  
  AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
  AnswersIndex = sample(c(1, 2, 3, 4), 4)
  Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])
  
  correctAnswer <- Answer1
  correctAnswerIndex <- which(Answers == correctAnswer)
  
  print(noquote(QuestionString))
  
  for(i in 1:4){
    print(noquote(paste0(i, ". ", Answers[i])))
  }
  
  return(c(correctAnswer, correctAnswerIndex))
}

AskH1 <- function(parameters, type, difficulty){
  
  H0String <- switch(type,
                     paste0(
                       c(greeks("mu"), " = ", parameters["muNaught",]), collapse = ""),
                     paste0(
                       c("p", " = ", parameters["pNaught",]), collapse = ""),
                     paste0(
                       c(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082"), collapse = "")
  )
  
  QuestionString <- switch(difficulty,
                           paste0(
                             c("H", "\u2080", " is ", H0String, ". What is H", "\u2081", "?"), collapse = ""),
                           paste0(
                             c("H", "\u2080", " is ", H0String, ". What is H", "\u2081", "?"), collapse = ""),
                           paste0(
                             c("What is H", "\u2081", "?"), collapse = "")
  )
  
  Answer1 <- switch(type,
                    paste0(
                      c(greeks("mu"), " = ", parameters["muNaught",]), collapse = ""),
                    paste0(
                      c("p", " = ", parameters["pNaught",]), collapse = ""),
                    paste0(
                      c(greeks("mu"), "\u2081", " = ", greeks("mu"), "\u2082"), collapse = "")
  )
  
  Answer2 <- switch(type,
                    paste0(
                      c(greeks("mu"), " < ", parameters["muNaught",]), collapse = ""),
                    paste0(
                      c("p", " < ", parameters["pNaught",]), collapse = ""),
                    paste0(
                      c(greeks("mu"), "\u2081", " < ", greeks("mu"), "\u2082"), collapse = "")
  )
  
  Answer3 <- switch(type,
                    paste0(
                      c(greeks("mu"), " > ", parameters["muNaught",]), collapse = ""),
                    paste0(
                      c("p", " > ", parameters["pNaught",]), collapse = ""),
                    paste0(
                      c(greeks("mu"), "\u2081", " > ", greeks("mu"), "\u2082"), collapse = "")
  )
  
  Answer4 <- switch(type,
                    paste0(
                      c(greeks("mu"), " =/= ", parameters["muNaught",]), collapse = ""),
                    paste0(
                      c("p", " =/= ", parameters["pNaught",]), collapse = ""),
                    paste0(
                      c(greeks("mu"), "\u2081", " =/= ", greeks("mu"), "\u2082"), collapse = "")
  )
  
  AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
  AnswersIndex = sample(c(1, 2, 3, 4), 4)
  Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])
  
  correctAnswer <- AnswersBase[parameters["tail",] + 1]
  correctAnswerIndex <- which(Answers == correctAnswer)
  
  print(noquote(QuestionString))
  
  for(i in 1:4){
    print(noquote(paste0(i, ". ", Answers[i])))
  }
  
  return(c(correctAnswer, correctAnswerIndex))
}


AskTFormula <- function(type){
  
  typeString <- switch(type,
                       greeks("mu"),
                       "proportion",
                       "paired")
  
  QuestionString <- paste0(c("How do you calculate t for a ", typeString, " t-test"), collapse = "")
  
  
  Answer1 <- switch(type,
                    paste0(
                      c("(",greeks("mu"), " - ", greeks("mu"), "\u2080)/(", greeks("sigma"), "/\U221an)"), collapse = ""),
                    paste0(
                      c("(",greeks("p"), " - ", greeks("p"), "\u2080)/(", greeks("sigma"), "/\U221an)"), collapse = ""),
                    paste0(
                      c("(",greeks("mu"), "\u2081 - ", greeks("mu"), "\u2082)/(", greeks("sigma"), "/\U221an)"), collapse = "")
  )
  
  Answer2 <- switch(type,
                    paste0(
                      c("(",greeks("mu"), " - ", greeks("mu"), "\u2080)/(", greeks("sigma"), "\U00b2/n)"), collapse = ""),
                    paste0(
                      c("(",greeks("p"), " - ", greeks("p"), "\u2080)/(", greeks("sigma"), "\U00b2/\U221an)"), collapse = ""),
                    paste0(
                      c("(",greeks("mu"), "\u2081 - ", greeks("mu"), "\u2082)/(", greeks("sigma"), "\U00b2/n)"), collapse = "")
  )
  
  Answer3 <- switch(type,
                    paste0(
                      c("(",greeks("mu"), " - ", greeks("mu"), "\u2080)/", greeks("sigma")), collapse = ""),
                    paste0(
                      c("(",greeks("p"), " - ", greeks("p"), "\u2080)/", greeks("sigma")), collapse = ""),
                    paste0(
                      c("(",greeks("mu"), "\u2081 - ", greeks("mu"), "\u2082)/", greeks("sigma")), collapse = "")
  )
  
  Answer4 <- switch(type,
                    paste0(
                      c("(",greeks("mu"), ")/(", greeks("sigma"), "\U221an)"), collapse = ""),
                    paste0(
                      c("(",greeks("p"), ")/(", greeks("sigma"), "\U221an)"), collapse = ""),
                    paste0(
                      c("(",greeks("mu"), "\u2081)/(", greeks("sigma"), "\U221an)"), collapse = "")
  )
  
  AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
  AnswersIndex = sample(c(1, 2, 3, 4), 4)
  Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])
  
  correctAnswer <- Answer1
  correctAnswerIndex <- which(Answers == correctAnswer)
  
  print(noquote(QuestionString))
  
  for(i in 1:4){
    print(noquote(paste0(i, ". ", Answers[i])))
    
    
  }
  return(c(correctAnswer, correctAnswerIndex))
}

AskT <- function(parameters, type){
  
  QuestionString <- paste0('What is T?')
  
  Answer1 <- round(switch(type,
                          (parameters["mu",] - parameters["muNaught",])/(parameters["sigma",]/(parameters["n",]^0.5)),
                          (parameters["p",] - parameters["pNaught",])/(parameters["sigma",]/(parameters["n",]^0.5)),
                          (parameters["mu",] - parameters["mu2",])/(parameters["sigmaDiff",]/(parameters["n",]^0.5))
  ), 2)
  
  
  Answer2 <- round(sample(c(1, -1), 1)*runif(1, 0.95, 1.1)*Answer1, 2)
  
  Answer3 <- round(sample(c(1, -1), 1)*runif(1, 0.9, 1.05)*Answer1, 2)
  
  Answer4 <- round(sample(c(1, -1), 1)*runif(1, 0.95, 1.05)*Answer1, 2)
  
  AnswersBase <- c(Answer1, Answer2, Answer3, Answer4)
  AnswersIndex = sample(c(1, 2, 3, 4), 4)
  Answers <- c(AnswersBase[AnswersIndex[1]], AnswersBase[AnswersIndex[2]], AnswersBase[AnswersIndex[3]], AnswersBase[AnswersIndex[4]])
  
  correctAnswer <- Answer1
  correctAnswerIndex <- which(Answers == correctAnswer)
  
  print(noquote(QuestionString))
  
  for(i in 1:4){
    print(noquote(paste0(i, ". ", Answers[i])))
  }
  
  return(c(correctAnswer, correctAnswerIndex))
}

AskRejection <- function(parameters, type){
  
  t <- switch(type,
              (parameters["mu",] - parameters["muNaught",])/(parameters["sigma",]/(parameters["n",]^0.5)),
              (parameters["p",] - parameters["pNaught",])/(parameters["sigma",]/(parameters["n",]^0.5)),
              (parameters["mu",] - parameters["mu2",])/(parameters["sigmaDiff",]/(parameters["n",]^0.5))
  )
  
  prob <- switch(parameters["tail",],
                 1 - parameters["alpha",],
                 parameters["alpha",],
                 1 - (1 - parameters["alpha",])/2)
  
  critical <- round(qnorm(prob), 2)
  solution <- ifelse(abs(t) > abs(critical), 1, 2)
  
  if(parameters["tail",] == 3){ 
    critical <- paste0(c("\u00B1", abs(critical)), collapse = "")}
  
  QuestionString <- paste0(c("If the critical value is ", critical, ", what is the conclusion"), collapse = "")
  
  
  
  Answer1 <- switch(solution, 
                    "Reject H\u2080",
                    " Reject H\u2081")
  
  Answer2 <- switch(solution,
                    "Accept H\u2081",
                    " Accept H\u2080")
  
  Answer3 <- switch(solution,
                    "Reject H\u2081",
                    " Reject H\u2080")
  
  Answer4 <- switch(solution,
                    "Accept H\u2080",
                    " Accept H\u2081")
  
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
  
  print(noquote(QuestionString))
  
  for(i in 1:4){
    print(noquote(paste0(i, ". ", Answers[i])))
  }
  
  return(c(correctAnswer, correctAnswerIndex))
}

Run <- function(){
  library("greekLetters")
  library("rjson")
  
  difficulty <- difficultyInitialise()
  difficultyOverall <- c(0, 0, 0, 0)
  
  questions <- data.frame(fromJSON(file = "questions.js"))
  
  while(TRUE){
    
    number <- QuestionSelect()
    
    difficultyTemp <- Game(number, difficulty)
    difficulty <- switch(difficulty["UpdateAnswer",],
                         updateDifficultyAuto(difficulty, difficultyTemp),
                         updateDifficultyManual(difficulty, difficultyTemp))
    
    difficultyOverall <- difficultyTemp + difficultyOverall
    
    QuestionString <- "Would you like to continue?"
    answers <- c("Yes", "No")
    
    print(noquote(QuestionString))
    for(i in 1:2){
      print(noquote(paste0(c(i, ". ", answers[i]), collapse = "")))
    }
    
    while(TRUE){
      input = readline()
      
      ifelse(input == "1" || input == "2", 
             break,
             print(noquote("Please select a valid option"))
      )
    }
    ifelse(input == "2", break, next)
  }
  
  ConclusionString1 <- paste0(c("You answered ", difficultyOverall[1], 
                                " out of ", difficultyOverall[3], " concept questions correctly."), collapse = "")
  ConclusionString2 <- paste0(c("You also answered ", difficultyOverall[2], " out of ", 
                                difficultyOverall[4], " calculation questions correctly."), collapse = "")
  
  print(noquote(ConclusionString1))
  print(noquote(ConclusionString2))
}

Run()

library("rjson")
questions <- data.frame(fromJSON(file = "questions.json"))