
library(shiny)
library(shinyjs)
library(greekLetters)
library(rjson)

## uiT ####
uiT <- fluidPage(

    useShinyjs(),

    conditionalPanel(condition = "output.stage == 1",
                     radioButtons("difficultySelect", "Choose a difficulty",
                                  choiceNames = c("Easy", "Medium", "Hard"),
                                  choiceValues = 1:3, selected = 2),
                     actionButton("difficultyUpdateAuto", "Select to have your difficulty automatically adjust to your answers")),

    conditionalPanel(condition = "output.stage == 4",
                     radioButtons("difficultyUpdateContext", label = "How did you find the concept questions?",
                                  choiceNames = c("Too hard", "Just right", "Too easy"),
                                  choiceValues = 1:3, selected = 2),
                     radioButtons("difficultyUpdateCalculation", label = "How did you find the calculation questions?",
                                  choiceNames = c("Too hard", "Just right", "Too easy"),
                                  choiceValues = 1:3, selected = 2)),

    conditionalPanel(condition = "output.stage == 1 || output.stage == 4",
                     numericInput("numberSelect", "Enter question number here", value = 1, min = 1, max = 9),
                     actionButton("numberRandom", "Select to generate a random question number")),

    actionButton("go", "Go"),

    conditionalPanel(condition = "output.stage == 2",
                     textOutput("contextStage2")),

    conditionalPanel(condition = "output.stage == 2",
                     radioButtons("button", label = NULL, choices = ""),
                     actionButton("select", "Select")),

    conditionalPanel(condition = "output.stage == 3",
                     textOutput("contextStage3")),

    conditionalPanel(condition = "output.stage == 3",
                     plotOutput("plot")),

    conditionalPanel(condition = "output.stage == 3",
                     sliderInput("slider1", label = "Critical value A",
                                 min = -3, max = 3, value = 0, step = 0.01),
                     sliderInput("slider2", label = "Critical value B",
                                 min = -3, max = 3, value = 0, step = 0.01),
                     actionButton("selectSlider", "Select")),

    actionButton("nextButton", "Next")
)


serverT <- function(input, output, session){

    source("T-tests.R")

    questions <- data.frame(fromJSON(file = "questions.json"))

    output$contextStage2 <- renderText("")

    difficulty <- reactiveValues(Concept = NULL, Calculation = NULL, UpdateAnswer = FALSE)
    answerArray <- reactiveValues(question = NULL, answers = NULL, correct = NULL, index = NULL)
    number <- reactiveVal(1)
    type <- reactiveVal(1)

    hide("nextButton")

    stage <- reactiveVal(1)
    output$stage <- reactive(stage())
    outputOptions(output, "stage", suspendWhenHidden=FALSE)

    # outputOptions(output, "initialise", suspendWhenHidden=FALSE)
    # outputOptions(output, "main", suspendWhenHidden=FALSE)
    check <- reactiveValues(string = NULL, point = 0)
    id <- NULL

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
    observeEvent(input$go, {

        stage(2)

        hide("go")
        show("nextButton")

        difficulty$Concept = as.numeric(input$difficultySelect)
        difficulty$Calculation = as.numeric(input$difficultySelect)

        number <- input$numberSelect
        type <- (number() - 1)%%3 + 1

        parameters <- Parameters(number, type, difficulty$Calculation)

        questionString <- PrintQuestion(number, type, parameters, questions)
        output$contextStage2 <- renderText(questionString)

        answerList <- questionStage(parameters, type, difficulty, 1)
        answerArray$question <- answerList$question
        answerArray$answers <- answerList$answers
        answerArray$correct <- answerList$correct
        answerArray$index <- answerList$index

        updateRadioButtons(session, inputId = "button",
                           label = answerArray$question, choiceNames = answerArray$answers,
                           choiceValues = 1:4, selected = character(0))
    })


    observeEvent(input$select,{
        ## Select answer ####
        showNotification(renderText(AnswerCheckShiny(answerArray$correct, answerArray$index,
                                                     as.numeric(input$button))$string))
    })

    observeEvent(input$nextButton, {
        ## Go to next question ####

        if(switch(difficulty$Concept,
                  stage() < 7,
                  stage() < 6,
                  stage() < 4)){
            browser()
            stageValue <- as.numeric(stage()) + 1
            stage(stageValue)

            check$point <- as.numeric(check$point) + AnswerCheckShiny(answerArray$correct, answerArray$index,
                                                                      as.numeric(input$button))$point

            answerArray$question <- questionStage(parameters, type(), difficulty, stage())$question
            answerArray$answers <- questionStage(parameters, type(), difficulty, stage())$answers
            answerArray$correct <- questionStage(parameters, type(), difficulty, stage())$correct
            answerArray$index <- questionStage(parameters, type(), difficulty, stage())$index

            updateRadioButtons(session, inputId = "button",
                               label = answerArray$question, choiceNames = answerArray$answers,
                               choiceValues = 1:4, selected = character(0))
        }
    })
}

shinyApp(uiT, serverT)



uiPlot <- fluidPage(

  plotOutput("plot"),
  sliderInput("slider", "Slide to change line", value = 0, min = -3, max = 3, step=0.01)

)

sliderInput()


serverPlot <- function(input, output){

  slider <- reactive(input$slider)
  browser()
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

shinyApp(uiPlot, serverPlot)
