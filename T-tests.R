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
  
  return(round(mu, difficulty - 1))
}

findSigma <- function(number, muDiff, difficulty){
  
  sigma <- switch(difficulty,
         switch(number,
                      round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(100 - muDiff)/3, 1),
                      round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(1 - muDiff)/3, 1),
                      round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(100 - muDiff)/3, 1),
                      round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(50 - muDiff)/3, 1),
                      round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(muDiff)/3, 1),
                      round(sample(c(0.9, 0.95, 1, 1.05, 1.1), 1)*(50 - muDiff)/3, 1)),
         switch(number,
                      round(runif(1, 0.9, 1.1)*(100 - muDiff)/3, 1),
                      round(runif(1, 0.9, 1.1)*(1 - muDiff)/3, 1),
                      round(runif(1, 0.9, 1.1)*(100 - muDiff)/3, 1),
                      round(runif(1, 0.9, 1.1)*(50 - muDiff)/3, 1),
                      round(runif(1, 0.9, 1.1)*(muDiff)/3, 1),
                      round(runif(1, 0.9, 1.1)*(50 - muDiff)/3, 1)),
         switch(number,
                      round(runif(1, .9, 1.1)*(100 - muDiff)/3, 2),
                      round(runif(1, .9, 1.1)*(1 - muDiff)/3, 2),
                      round(runif(1, .9, 1.1)*(100 - muDiff)/3, 2),
                      round(runif(1, .9, 1.1)*(50 - muDiff)/3, 2),
                      round(runif(1, .9, 1.1)*(muDiff)/3, 2),
                      round(runif(1, .9, 1.1)*(50 - muDiff)/3, 2))
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
  
  sigma <- findSigma(number, abs(muNaught - mu), difficulty)
  
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
  
  sigma <- findSigma(number, abs(pNaught - p), difficulty)


parameters <- data.frame(c(n, alpha, p/100, pNaught/100, sigma, tail), 
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
  
  sigmaDiff <- findSigma(number, abs(mu2 - mu), difficulty)
 
  parameters <- data.frame(c(n, alpha, mu, mu2, sigmaDiff, tail), 
  row.names = c("n", "alpha", "mu", "mu2", "sigmaDiff", "tail")) 
  
  return(parameters)
}

Parameters <- function(number, type, difficulty){
  
  switch(type,
        parameters <- ParametersMu(number, difficulty),
        parameters <- ParametersP(number, difficulty),
        parameters <- ParametersDiff(number, difficulty)
        )
  
  return(parameters)
}

Run <- function(){
  library("greekLetters")
  
  number <- QuestionSelect()
  difficulty <- difficultyInitialise()
  
  type <- ((number - 1)%%3) + 1
  parameters <- Parameters(number, type, difficulty["Calculation",])
  
  print(noquote(""))
  print(PrintQuestion(number, type, parameters))
  print(noquote(""))
  
  difficultyChangeTotal <- c(0, 0)
  
  for(stage in 1:6){
    
      
      correctAnswerArray <- switch(stage,
                              ifelse(difficulty["Concept",] < 2,
                                AskParameter(type),
                                next),
                              ifelse(difficulty["Concept",] < 3,
                              AskH0(parameters, type),
                              next),
                              AskH1(parameters, type, difficulty["Calculation",]),
                              ifelse(difficulty["Concept",] < 3,
                              AskTFormula(type),
                              next),
                              AskT(parameters, type),
                              AskRejection(parameters, type))
      
      correctAnswer <- correctAnswerArray[1]
      correctAnswerIndex <- correctAnswerArray[2]
      
      questionType <- switch(stage,
                             1,
                             1,
                             2,
                             1,
                             2,
                             2)
      
      AnswerCheck(correctAnswer, correctAnswerIndex)
      print(noquote(""))
        }
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
                  c("In 2020, the pandemic caused a series of disruptions to teaching. A STAT 193 lecturer wants to test whether this has led to ", tailString,
                    " in results in his class of ", parameters["n",],
                    " students. They will be testing at a ", parameters["alpha",]*100,
                    "% significance level. The average score on the STAT 193 final exam was ", parameters["mu",],
                    ". Typically, the average score is ", parameters["muNaught",],
                    ", with a standard deviation of ", parameters["sigma",], "."), collapse = ""),
            paste0(
                 c("In 2020, the pandemic caused a series of disruptions to teaching. A STAT 193 lecturer wants to test whether this has led to ", tailString,
                   " in results in his class of ", parameters["n",],
                   " students. They will be testing at a ", parameters["alpha",]*100,
                   "% significance level. After the final exam has been marked, the lecturer notes that ", parameters["p",]*100,
                   "% passed the course, when the typical pass rate is ", parameters["pNaught",],
                   ", with a standard deviation of ", parameters["sigma",], "."), collapse = ""),
            paste0(
                 c("In 2020, the pandemic caused a series of disruptions to teaching. A STAT 193 lecturer wants to test whether this has led to ", tailString,
                   " in results in his class of ", parameters["n",],
                   " students. They will be testing at a ", parameters["alpha",]*100,
                   "% significance level. The average score on the STAT 193 final exam was ", parameters["mu",],
                   ". Another lecturer teaches a different section of STAT 193. They notice the students get an average score of ", parameters["mu2",],
                   ", with a standard deviation of ",  parameters["sigmaDiff",], "."), collapse = ""),
            paste0(
                  c("A company wants to test a new line of manufacturing batteries. They sample ", parameters["n",],
                    " batteries at a significance level of ", parameters["alpha",],
                    ". They want to check whether the batteries perform ", tailString,
                    " than their other lines. The sample lasts for an average of ", parameters["mu",],
                    " hours, compared to a typical lifetime of ", parameters["muNaught",],
                    " hours and a standard deviation of ", parameters["sigma",],
                    "."), collapse = ""),
            paste0(
              c("A company wants to test a new line of manufacturing batteries. They sample ", parameters["n",],
                " batteries at a significance level of ", parameters["alpha",]*100,
                "%. According to safety regulations, it is assumed that ", tailString, " ", parameters["pNaught",]*100,
                "% of the batteries can be defective, With an expected standard deviation of ", parameters["sigma",],
                ". Out of the sample, the company noticed that there were ", round(parameters["p",]*parameters["n",], 0),
                " defective batteries."), collapse = ""),
            paste0(
              c("A company wants to test a new line of manufacturing batteries. They sample ", parameters["n",],
                " batteries at a significance level of ", parameters["alpha",]*100,
                ". To test the batteries, they use a battery testing device. When using a second testing device to see whether the results ", tailString,
                ". They notice that on average the batteries lasted ", parameters["mu2",],
                " hours using the second device, compared to the average on the first device of ", parameters["mu",], 
                " hours, with a standard deviation of ", parameters["sigmaDiff",]), collapse = "")
            )

  return(noquote(string))
}

AnswerCheck <- function(answer, answerIndex){
  
  input <- readline()
  
  result <- ifelse(input %in% answerIndex, "Correct", "Incorrect")
  
  answerString <- ifelse(result == "Incorrect",
    ifelse(length(answer) > 1,
      paste0(c("The correct answer is ", answer[1], " or ", answer[2]), collapse = ""),
      paste0(c("The correct answer is ", answer), collapse = "")),
    "")
  
  print(noquote(paste0(c(result, ". ", answerString), collapse = "")))
  
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

Run()

PrintQuestion(5, 2, Parameters(5, 2, 2))
