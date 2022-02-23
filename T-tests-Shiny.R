## Libraries ####
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
  
  conditionalPanel(condition = "output.phase == 4 && output.auto",
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

  conditionalPanel(condition = "output.phase == 2",
                   textOutput("contextPhase2")),
  
  conditionalPanel(condition = "output.phase == 2",
                   radioButtons("answerSelect"),
                   actionButton("select", "Select")),
  
  conditionalPanel(condition = "output.phase == 3",
                   textOutput("contextPhase3")),
  
  conditionalPanel(condition = "output.phase == 3",
                   plotOutput("plot")),
  
  conditionalPanel(condition = "output.phase == 3",
                   sliderInput("slider", label = "What is T?", 
                               min = -3, max = 3, value = 0, step = 0.01),
                   actionButton("selectSlider", "Select")),
  
  conditionalPanel(condition == "output.phase == 1 || output.phase == 4",
                  actionButton("nextButton", "Next"))
)

serverT <- function(input, output, session){
  
  # Retrieving files needed
  source("T-tests.R")
  questions <- data.frame(fromJSON(file = "questions.json"))
  
  # Difficulty values
  difficulty <- reactiveValues(Concept = NULL, Calculation = NULL, UpdateAnswer = FALSE)
  output$auto <- reactive(!difficulty$UpdateAnswer)
  
  # Answer array values
  answerArray <- reactiveValues(question = NULL, answers = NULL, correct = NULL, index = NULL)
  
  # Question number and type
  number <- reactiveVal(1)
  type <- reactiveVal(1)
  
  # Parameters
  parameters <- reactiveValues(n = NULL, alpha = NULL, mu = NULL, 
                               muNaught = NULL, sigma = NULL, tail = NULL,
                               p = NULL, pNaught = NULL, mu2 = NULL,
                               sigmaDiff = NULL)
  # Context strings
  contextString <- reactiveVal(NULL)
  
  # Boolean whether question has been answered or not
  answered <- reactiveVal(FALSE)
  
  # Phase of conditionalPanels
  phase <- reactiveVal(1)
  output$phase <- reactive(phase())
  outputOptions(output, "phase", suspendWhenHidden=FALSE)
  
  # Stage of particular question
  stage <- reactiveVal(1)
  
  # Answer values
  check <- reactiveValues(string = NULL, pointConcept = 0, pointCalculation = 0)
  id <- NULL
  
  # Slider object
  slider <- reactive(input$slider)
  
  hide("nextButton")
  
  updateFalse <- "Select to have your difficulty automatically adjust to your answers"
  updateTrue <- "Select to manually adjust your difficulty"
  
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
    stage(1)
    
    paramList <- Parameters(number(), type(), difficulty$Calculation)
    parameters$n <- paramList$n
    parameters$alpha <- paramList$alpha
    parameters$mu <- paramList$mu
    parameters$muNaught <- paramList$muNaught
    parameters$sigma <- paramList$sigma
    parameters$tail <- paramList$tail
    parameters$p <- paramList$p
    parameters$pNaught <- paramList$pNaught
    parameters$mu2 <- paramList$mu2
    parameters$sigmaDiff <- paramList$sigmaDiff
    
    contextString(PrintQuestion(number(), type(), paramList, questions))
    output$contextPhase2 <- renderText(contextString())
    output$contextPhase3 <- renderText(contextString())
    
    answerList <- questionStage(parameters, type(), difficulty, stage())
    answerArray$question <- answerList$question
    answerArray$answers <- answerList$answers
    answerArray$correct <- answerList$correct
    answerArray$index <- answerList$index
    
    updateRadioButtons(session, inputId = "answerSelect",
                       label = answerArray$question, choiceNames = answerArray$answers,
                       choiceValues = 1:4, selected = character(0))
    
    phase(2)
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
    if(!answered()){
      check$pointCalculation <- as.numeric(check$pointCalculation) + AnswerCheckShiny(answerArray$correct, answerArray$index,
                                                                as.numeric(input$slider))$point
      answered(TRUE)
    }
    
    showNotification(renderText(AnswerCheckShiny(answerArray$correct, answerArray$index,
                                                 as.numeric(input$slider))$string))
    
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
    
  observeEvent(input$nextButton, {
    ## Go to next question ####
    
    if(answered()){
      if(switch(difficulty$Concept,
                    stage() < 7,
                    stage() < 6,
                    stage() < 4)){
                      
                      stageValue <- as.numeric(stage()) + 1
                      stage(stageValue)
                      answered(FALSE)
                      
                      paramList <- reactiveValuesToList(parameters)
                      
                      answerList <- questionStage(paramList, type(), difficulty, stage())
                      answerArray$question <- answerList$question
                      answerArray$answers <- answerList$answers
                      answerArray$correct <- answerList$correct
                      answerArray$index <- answerList$index
                      
                      phase(2)
                      
                      if(switch(difficulty$Concept,
                                stage() == 6,
                                stage() == 5,
                                stage() == 2)){
                        
                        phase(3)
                        }
                        
                      updateRadioButtons(session, inputId = "answerSelect",
                                         label = answerArray$question, choiceNames = answerArray$answers,
                                         choiceValues = 1:4, selected = character(0))  
                      }else{
                      phase(4)
                      
                      if(!difficulty$UpdateAnswer){

                      questionsConcept <- switch(difficulty$Concept,
                                                 4,
                                                 3,
                                                 0)
                      questionsCalculation <- 3
                      
                      stringConcept <- paste0(c("You correctly answered ", check$pointConcept,
                                                " out of ", questionsConcept,
                                                " concept questions correctly. How did you find them?"),
                                              collapse = "")
                      stringCalculation <- paste0(c("You correctly answered ", check$pointCalculation,
                                                    " out of ", questionsCalculation,
                                                    " calculation questions correctly. How did you find them?"),
                                                  collapse = "")
                        
                      updateActionButton(session, "difficultyUpdateConcept",
                                         label = stringConcept)
                      updateActionButton(session, "difficultyUpdateCalculation",
                                         label = stringCalculation)
                      
                      hide("nextButton")
                      }
                    }
    }
  })
}

shinyApp(uiT, serverT)

## UiPlot ####
uiPlot <- fluidPage(
  
  plotOutput("plot"),
  sliderInput("slider", "Slide to change line", value = 0, min = -3, max = 3, step=0.01),
  actionButton("select", "Select Answer"),
  textOutput("selected"),
  textOutput("result")
)

serverPlot <- function(input, output){
  
  slider <- reactive(input$slider)
  answer <- reactive("")
  
  output$selected <- renderText(paste0(c("The selected value is ", slider()), collapse = ""))
  output$result <- reactive(
    renderText(answer())
  )
  
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
      answer(ifelse(slider() == 1,
             "Correct",
             "Incorrect, the correct answer is 1"))
    })
    
  })
  
}

shinyApp(uiPlot, serverPlot)
