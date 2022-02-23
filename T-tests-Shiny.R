
library(shiny)
library(shinyjs)
library(greekLetters)
library(rjson)

## uiT ####
uiT <- fluidPage(

    useShinyjs(),

    conditionalPanel(condition = "output.phase == 1",
                     radioButtons("difficultySelect", "Choose a difficulty",
                                  choiceNames = c("Easy", "Medium", "Hard"),
                                  choiceValues = 1:3, selected = 2),
                     actionButton("difficultyUpdateAuto", "Select to have your difficulty automatically adjust to your answers")),

    conditionalPanel(condition = "output.phase == 4 && output.manual",
                     radioButtons("difficultyUpdateConcept", label = "How did you find the concept questions?",
                                  choiceNames = c("Too hard", "Just right", "Too easy"),
                                  choiceValues = 1:3, selected = 2),
                     radioButtons("difficultyUpdateCalculation", label = "How did you find the calculation questions?",
                                  choiceNames = c("Too hard", "Just right", "Too easy"),
                                  choiceValues = 1:3, selected = 2)),

    conditionalPanel(condition = "output.phase == 1 || output.phase == 4",
                     numericInput("numberSelect", "Enter question number here", value = 1, min = 1, max = 9),
                     actionButton("numberRandom", "Select to generate a random question number"),
                     actionButton("goButton", "Go")),

    conditionalPanel(condition = "output.phase == 2 || output.phase == 3",
                     textOutput("context")),

    conditionalPanel(condition = "output.phase == 2",
                     radioButtons("answerSelect", label = NULL, choices = ""),
                     actionButton("select", "Select")),

    conditionalPanel(condition = "output.phase == 3",
                     plotOutput("plot")),

    conditionalPanel(condition = "output.phase == 3",
                     sliderInput("slider", label = "Critical value A",
                                 min = -3, max = 3, value = 0, step = 0.01),
                     actionButton("selectSlider", "Select")),

    actionButton("nextButton", "Next")
)


serverT <- function(input, output, session){

    # Retrieving files needed
    source("T-tests.R")
    questions <- data.frame(fromJSON(file = "questions.json"))

    # Difficulty values
    difficulty <- reactiveValues(Concept = NULL, Calculation = NULL, UpdateAnswer = FALSE)
    output$auto <- reactive(difficulty$UpdateAnswer)
    # Answer array values
    answerArray <- reactiveValues(question = NULL, answers = NULL, correct = NULL, index = NULL)
    # Parameter values
    parameters <- reactiveValues(n = NULL, alpha = NULL, mu = NULL, muNaught = NULL,
                                 sigma = NULL, tail = NULL)

    # Question number and type
    number <- reactiveVal(1)
    type <- reactiveVal(1)

    # Boolean whether question has been answered or not
    answered <- reactiveVal(FALSE)
    hide("nextButton")

    # Phase of conditionalPanels
    phase <- reactiveVal(1)
    output$phase <- reactive(phase())
    outputOptions(output, "phase", suspendWhenHidden=FALSE)

    # Stage of particular question
    stage <- reactiveVal(1)

    check <- reactiveValues(string = NULL, pointConcept = 0, pointCalculation = 0)
    id <- NULL

    updateFalse <- "Select to have your difficulty automatically adjust to your answers"
    updateTrue <- "Select to manually adjust your difficulty"

    slider <- reactive(input$slider)

    questionString <- reactiveVal("")

    output$context <- renderText(questionString())

    observeEvent(input$difficultyUpdateAuto, {
        difficulty$UpdateAnswer = !difficulty$UpdateAnswer
        if(difficulty$UpdateAnswer){
            updateActionButton(inputId = "difficultyUpdateAuto", label = updateTrue)}
        if(!difficulty$UpdateAnswer){
            updateActionButton(inputId = "difficultyUpdateAuto", label = updateFalse)}
    })

    observeEvent(input$numberRandom, {
        randomNumber <- sample(1:9, 1)
        updateNumericInput(session = session, inputId = "numberSelect", value = randomNumber)
    })

    ## Go from initialise to question ####
    observeEvent(input$goButton, {

        if(phase() == 1){
            difficulty$Concept = as.numeric(input$difficultySelect)
            difficulty$Calculation = as.numeric(input$difficultySelect)
        }

        if(phase() == 4){
            ifelse(difficulty$UpdateAnswer,{
                difficulty$Concept <- difficulty$Concept + max((check$pointConcept - 2), -1)
                difficulty$Concept <- min(difficulty$Concept, 1)
                difficulty$Concept <- max(difficulty$Concept, 3)

                difficulty$Calculation <- difficulty$Calculation + max((check$pointCalculation - 2), -1)
                difficulty$Calculation <- min(difficulty$Calculation, 1)
                difficulty$Calculation <- max(difficulty$Calculation, 3)
            },{
                difficulty$Concept <- difficulty$Concept + (as.numeric(input$difficultyUpdateConcept) - 2)
                difficulty$Concept <- min(difficulty$Concept, 1)
                difficulty$Concept <- max(difficulty$Concept, 3)

                difficulty$Calculation <- difficulty$Calculation + (as.numeric(input$difficulty$UpdateCalculation) - 2)
                difficulty$Calculation <- min(difficulty$Calculation, 1)
                difficulty$Calculation <- max(difficulty$Calculation, 3)
            })

            check$pointConcept <- 0
            check$pointCalculation <- 0
        }

        number(input$numberSelect)
        type((number() - 1)%%3 + 1)

        temp <- Parameters(number(), type(), difficulty$Calculation)
        parameters$n <- temp$n; parameters$alpha <- temp$alpha;
        parameters$mu <- temp$mu; parameters$muNaught <- temp$muNaught;
        parameters$sigma <- temp$sigma; parameters$tail <- temp$tail;

        questionString(PrintQuestion(number(), type(), parameters, questions))

        answerList <- questionStage(parameters, type(), difficulty, 1)
        answerArray$question <- answerList$question
        answerArray$answers <- answerList$answers
        answerArray$correct <- answerList$correct
        answerArray$index <- answerList$index

        updateRadioButtons(session, inputId = "answerSelect",
                           label = answerArray$question, choiceNames = answerArray$answers,
                           choiceValues = 1:4, selected = character(0))

        phase(2)

        hide("goButton")
        show("nextButton")
    })

    observeEvent(input$select,{
        ## Select answer ####
        if(!answered()){

            # Determining whether calculation (1) or concept (2) question
            questionType <- switch(difficulty$Concept,
                                   switch(stage(),
                                          2,
                                          2,
                                          1,
                                          2,
                                          2,
                                          1,
                                          1),
                                   switch(stage(),
                                          2,
                                          1,
                                          2,
                                          2,
                                          1,
                                          1),
                                   1)

            switch(questionType,
                   check$pointCalculation <- as.numeric(check$pointCalculation) + AnswerCheckShiny(answerArray$correct, answerArray$index,
                                                                                                   as.numeric(input$answerSelect))$point,
                   check$pointConcept <- as.numeric(check$pointConcept) + AnswerCheckShiny(answerArray$correct, answerArray$index,
                                                                                           as.numeric(input$answerSelect))$point)
            answered(TRUE)
        }

        showNotification(renderText(AnswerCheckShiny(answerArray$correct, answerArray$index,
                                                     as.numeric(input$answerSelect))$string))
    })

    observeEvent(input$selectSlider, {
        if(!isolate(answered())){
            check$pointCalculation <- as.numeric(check$pointCalculation) + AnswerCheckShiny(answerArray$correct, answerArray$index,
                                                                                            as.numeric(input$slider))$point
            answered(TRUE)
        }

        showNotification(renderText(AnswerCheckShiny(answerArray$correct, answerArray$index,
                                                     as.numeric(input$slider))$string))

    })

    observeEvent(input$nextButton, {
        ## Go to next question ####
        if(answered()){
            stageValue <- as.numeric(stage())

            condition1 <- switch(difficulty$Concept,
                                 stageValue < 7,
                                 stageValue < 6,
                                 stageValue < 4)
            if(condition1) {

                stageValue <- as.numeric(stage()) + 1
                stage(stageValue)
                answered(FALSE)

                answerList <- questionStage(parameters, type(), difficulty, stage())
                answerArray$question <- answerList$question
                answerArray$answers <- answerList$answers
                answerArray$correct <- answerList$correct
                answerArray$index <- answerList$index

                condition2 <- switch(difficulty$Concept,
                                     stageValue == 6,
                                     stageValue == 5,
                                     stageValue == 2)

                if(condition2){

                    phase(3)

                    updateRadioButtons(session, inputId = "answerSelect",
                                       label = answerArray$question, choiceNames = answerArray$answers,
                                       choiceValues = 1:4, selected = character(0))
                    updateSliderInput(session, "slider", label = answerArray$Question)
                } else {
                    updateRadioButtons(session, inputId = "answerSelect",
                                       label = answerArray$question, choiceNames = answerArray$answers,
                                       choiceValues = 1:4, selected = character(0))
                }

            } else {
                phase(4)

                if(!difficulty$UpdateAnswer){

                    questionsConcept <- switch(difficulty$Concept,
                                               4,
                                               3,
                                               0)
                    questionsCalculation <- 3
                    stringConcept <- paste0(c("You correctly answered ", point$pointConcept,
                                              " out of ", questionsConcept,
                                              " concept questions correctly. How did you find them?"),
                                            collapse = "")
                    stringCalculation <- paste0(c("You correctly answered ", point$pointCalculation,
                                                  " out of ", questionsCalculation,
                                                  " calculation questions correctly. How did you find them?"),
                                                collapse = "")

                    updateActionButton(session, "difficultyUpdateConcept",
                                       label = stringQuestion)
                    updateActionButton(session, "difficultyUpdateConcept",
                                       label = stringQuestion)
                }
            }
        }
    })

    output$plot <- renderPlot({
        xA <- slider()
        xB <- slider()
        yA <- 0
        yB <- dnorm(xA)

        xNorm <- seq(-3, 3, by = 0.01)
        yNorm <- dnorm(xNorm)

        plot(xNorm, yNorm)
        lines(c(xA, xB), c(yA, yB))

    })
}

shinyApp(uiT, serverT)


uiPlot <- fluidPage(

  plotOutput("plot"),
  sliderInput("slider", "Slide to change line", value = 0, min = -3, max = 3, step=0.01),
  actionButton("select", "Select Answer"),
  textOutput("result")
)

serverPlot <- function(input, output){

  slider <- reactive(input$slider)
  output$plot <- renderPlot({
    xA <- slider()
    xB <- slider()
    yA <- 0
    yB <- dnorm(xA)

    xNorm <- seq(-3, 3, by = 0.01)
    yNorm <- dnorm(xNorm)

    plot(xNorm, yNorm,
         xlab="", xaxt="n", ylab="", yaxt="n")
    lines(c(xA, xB), c(yA, yB))

    observeEvent(input$select, {
      browser()
      answer <- slider()
      output$result <- renderText(ifelse(answer == 1,
                                         "Correct",
                                         "Incorrect, the correct answer is 1"))
    })

  })

}

shinyApp(uiPlot, serverPlot)
