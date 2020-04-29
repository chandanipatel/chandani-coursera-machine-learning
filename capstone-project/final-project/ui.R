library(shinydashboard)

ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Swift Keyboard", titleWidth = "100%"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            column(width = 4,
                box(
                    title = "Search the text", solidHeader = TRUE, status = "info", width = 12,
                    textInput("inputText", "Enter your Word/Sentence:")
                )
            ),
            column(width = 8,
                box(
                    title = "Output", solidHeader = TRUE, status = "success", width = 12,
                    
                    tags$h3(textOutput("predictedText")),
                    strong(textOutput("enteredText"))
                )
            )
        )
    )
)