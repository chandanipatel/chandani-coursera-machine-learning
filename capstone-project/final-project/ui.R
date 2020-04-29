library(shinydashboard)

ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Swift Keyboard", titleWidth = "100%"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            column(width = 2),
            column(width = 8,
                box(
                    title = "Search the text", solidHeader = TRUE, status = "info", width = 12,
                    textInput("inputText", "Enter your Word/Sentence:")
                )
            )),
        fluidRow(
            column(width = 2),
            column(width = 8,
                box(
                    title = "Output", solidHeader = TRUE, status = "success", width = 12,
                    h4("Predicted Word:"),
                    tags$h5(textOutput("predictedText")),
                    hr(),
                    h4("Your text:"),
                    tags$h5(textOutput("enteredText"))
                )
            )
        )
    )
)