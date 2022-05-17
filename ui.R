library(shiny)
library(shinyjs)

ui <- fluidPage(

    useShinyjs(),

    ## Include CSS via tags rather than by changing the Shiny theme
    ## because we don't want to overwrite all elements of the standard theme
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "stats_game_styles.css")
    ),

    # Application title
    titlePanel("  Surviving Stats"),

    div(id="startPage",
        actionButton("gameStartButton", "Start")
    ),

    hidden(div(id="userPage",
               textInput("username", label="Username", value="", placeholder="Enter your username"),
               div(textOutput("userMessage"),class="text_block"),
               actionButton("newUserButton", "New User"),
               actionButton("loadUserButton", "Load User")
               )),

    hidden(div(id="introPage",
               div(textOutput("introText"),class="text_block"),
               actionButton("introNextButton", "Next")
    )),

    hidden(div(id="progressPage",
               div(textOutput("progressText"),class="text_block"),
               actionButton("progressNextButton", "Next")
    )),

    hidden(div(id="levelPage",
               div(textOutput("levelText"),class="text_block"),
               actionButton("startLevelButton", "Start Level")
    )),

    hidden(div(id="questionPage",
        uiOutput("plots_ui"),
        fluidRow(
            disabled(actionButton("plotLeftButton", "", icon = icon("angle-left"))),
            actionButton("plotRightButton", "", icon = icon("angle-right"))
        ),
        fluidRow(
            column(12,
                   radioButtons("answerSelect", label = "Dummy label",
                                choiceNames = c("Dummy A", "Dummy B", "Dummy C", "Dummy D"),
                                choiceValues = 1:4, width="100%"),
                   actionButton("submitButton", "Submit")
            )),

        div(textOutput("answerText"),class="text_block"),

        fluidRow(
            column(2,
                   actionButton("questionBackButton", "Back")
            ),
            column(2,
                   actionButton("questionNextButton", "Next"),
                   offset=8
            )
        )
    )),

    hidden(div(id="endPage",
               div(textOutput("endText"),class="text_block"),
               actionButton("gameRestartButton", "Start again")
    ))
)
