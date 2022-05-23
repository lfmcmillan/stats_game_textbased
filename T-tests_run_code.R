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

    QuestionString <- paste0("You answered ", difficultyTemp[1], " out of ", difficultyTemp[3], " concept questions correctly. How did you find those questions?")
    answers <- c("Too easy", "Just right", "Too hard")

    input <- menu(answers, title = QuestionString)

    difficulty$Concept <- difficulty$Concept - (input - 2)
    difficulty$Concept <- min(difficulty$Concept, 1)
    difficulty$Concept <- max(difficulty$Concept, 3)

    QuestionString <- paste0("You answered ", difficultyTemp[2], " out of ", difficultyTemp[4], " calculation questions correctly. How did you find those questions?")
    answers <- c("Too easy", "Just right", "Too hard")

    input2 <- menu(answers, title = QuestionString)

    difficulty$Calculation <- difficulty$Calculation - (input2 - 2)
    difficulty$Calculation <- min(difficulty$Calculation, 1)
    difficulty$Calculation <- max(difficulty$Calculation, 3)


    return(difficulty)
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

Run <- function(){

    library("greekLetters")
    library("rjson")
    library("shiny")

    difficulty <- difficultyInitialise()
    difficultyOverall <- c(0, 0, 0, 0)

    questions <- data.frame(fromJSON(file = "questions.json"))

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

    ConclusionString1 <- paste0("You answered ", difficultyOverall[1],
                                " out of ", difficultyOverall[3], " concept questions correctly.")
    ConclusionString2 <- paste0("You also answered ", difficultyOverall[2], " out of ",
                                difficultyOverall[4], " calculation questions correctly.")

    print(noquote(ConclusionString1))
    print(noquote(ConclusionString2))
}

AnswerCheckShiny <- function(correct, index, input){

    result <- ifelse(input %in% index, "Correct", "Incorrect")

    correctString <- ifelse(result == "Incorrect",
                            switch(length(correct),
                                   paste0("The correct answer is ", correct),
                                   paste0("The correct answer is ", correct[1], " or ", correct[2])),
                            "")

    answerString <- noquote(paste0(result, ". ", correctString))

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