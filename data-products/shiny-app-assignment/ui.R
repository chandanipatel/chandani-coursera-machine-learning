#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
        # Sex wise histogram
        column(width = 12,
           box(
               title = "Histogram Based on Age & Sex", width = NULL, solidHeader = TRUE, status = "primary",
                plotOutput("sexwisePlot", height = 350,
                  click = "plot_click",
                  dblclick = dblclickOpts(
                      id = "plot_dblclick"
                  ),
                  hover = hoverOpts(
                      id = "plot_hover"
                  ),
                  brush = brushOpts(
                      id = "plot_brush"
                  )
                )
           ),
        ),
        
        # Other wise histogram
        column(width = 6,
               box(
                   title = "Count based on Gender", width = NULL, solidHeader = TRUE, status = "primary",
                   plotOutput("genderPlot", height = 200,
                              click = "plot_click",
                              dblclick = dblclickOpts(
                                  id = "plot_dblclick"
                              ),
                              hover = hoverOpts(
                                  id = "plot_hover"
                              ),
                              brush = brushOpts(
                                  id = "plot_brush"
                              )
                   )
               ),
        ),
        # Other wise histogram
        column(width = 6,
               box(
                   title = "Chest Pain Type", width = NULL, solidHeader = TRUE, status = "primary",
                   plotOutput("chestpainPlot", height = 200)
               ),
        )
    )
)