library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Heart Disease Analysis"),
    dashboardSidebar(
        sidebarMenu(
            selectInput("sex", "Choose a Sex to filter histogram by Sex",
                        choices = list("All"=3, "Male"=1, "Female"=0), selected = 3),
            sliderInput("age", "Age:",
                        min = 0, max = 100, value = c(0, 100),
                        animate = animationOptions(interval = 300, loop = TRUE))
        )
    ),
    dashboardBody(
        column(width = 12,
               valueBoxOutput("totalStatestics"),
               valueBoxOutput("maleStatestics"),
               valueBoxOutput("femaleStatestics")
        ),
        # Sex wise histogram
        column(width = 12,
           box(
               title = "Histogram Based on Age & Sex", width = NULL, solidHeader = TRUE, status = "primary",
                plotOutput("sexwisePlot", height = 300)
           ),
        ),
        # Other wise histogram
        column(width = 6,
               box(
                   title = "Count based on Gender", width = NULL, solidHeader = TRUE, status = "primary",
                   plotOutput("genderPlot", height = 200)
               ),
        ),
        # Other wise histogram
        column(width = 6,
                   
                   box(
                       title = "Chest Pain Type", width = NULL, solidHeader = TRUE, status = "primary",
                       plotOutput("chestpainPlot", height = 200)
                   )
        )
    )
)