#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!require('shiny')) install.packages("shiny")
if (!require('shinydashboard')) install.packages("shinydashboard")
if (!require('ggplot2')) install.packages("ggplot2")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(scales)

server <- function(input, output) {
    heart_diease_uci <- read.csv("heart-disease-uci.csv", header = TRUE)
    
    output$minAgeSliderValue <- renderTable({
        sliderValues()
    })
    
    histogram_Sex <- reactive({
        sexFilter <- as.numeric(input$sex)
    })
    histogram_Age <- reactive({
        values <- c(min(input$age), max(input$age))
    })
    
    dataGenderwise <- reactive({
        ageFilter = histogram_Age()
        min=ageFilter[1]
        max=ageFilter[2]
        
        filter_data = heart_diease_uci[heart_diease_uci$age>min, ]
        filter_data = filter_data[filter_data$age<max, ]
        sexFilter = histogram_Sex()
        if(sexFilter != 3) {
            filter_data = filter_data[filter_data$sex == sexFilter,]
        }
        sex=filter_data$sex
        sex_wise_data=data.frame(table(unlist(sex)))
    })
    
    output$sexwisePlot <- renderPlot({
        sexFilter = histogram_Sex()
        
        ageFilter = histogram_Age()
        min=ageFilter[1]
        max=ageFilter[2]
        
        filter_data = heart_diease_uci[heart_diease_uci$age>min, ]
        filter_data = filter_data[filter_data$age<max, ]
        
        if(sexFilter == "3") {
            age=filter_data$age
        } else {
            age=filter_data[filter_data$sex == sexFilter,]['age']
        }
        sex_wise_data=data.frame(table(unlist(age)))
        colnames(sex_wise_data)[1]<-'age'
        ggplot(data=sex_wise_data, aes(x=age, y=Freq)) +
            geom_bar(stat="identity", fill="steelblue") +
            theme_minimal()+
            geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)
    })
    
    output$genderPlot <- renderPlot({
        ageFilter = histogram_Age()
        min=ageFilter[1]
        max=ageFilter[2]
        
        filter_data = heart_diease_uci[heart_diease_uci$age>min, ]
        filter_data = filter_data[filter_data$age<max, ]
        sexFilter = histogram_Sex()
        if(sexFilter != 3) {
            filter_data = filter_data[filter_data$sex == sexFilter,]
        }
        sex=filter_data$sex
        sex_wise_data=data.frame(table(unlist(sex)))
        colnames(sex_wise_data)[1]<-'sex'
        sex_wise_data$sex <- with(sex_wise_data, ifelse(sex==1, 'Male', "Female"))
        
        ggplot(sex_wise_data, aes(x="", y=Freq, fill=sex)) +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0) +
            scale_fill_manual(values=c("#EF746C", "#4DBB37"))
    })
    
    output$chestpainPlot <- renderPlot({
        ageFilter = histogram_Age()
        min=ageFilter[1]
        max=ageFilter[2]
        
        filter_data = heart_diease_uci[heart_diease_uci$age>min, ]
        filter_data = filter_data[filter_data$age<max, ]
        
        sexFilter = histogram_Sex()
        if(sexFilter != 3) {
            filter_data = filter_data[filter_data$sex == sexFilter,]
        }
        
        cp=filter_data$cp
        cp_wise_data=data.frame(table(unlist(cp)))
        colnames(cp_wise_data)[1]<-'cp'
        
        ggplot(cp_wise_data, aes(x="", y=Freq, fill=cp)) +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0) +
            scale_fill_manual(values=c("#EF746C", "#4DBB37", "#F4C050", "#327791"))
    })
    
    output$maleStatestics <- renderValueBox({
        sex_wise_data = dataGenderwise()
        male = sex_wise_data$Freq[1]
        total = sum(sex_wise_data$Freq)
        malepercent = percent(male/total)
                valueBox(
            "Male", paste0("Total: ", male, " Percentage: ", malepercent), icon = icon("male"),
            color = "orange"
        )
    })
    
    output$femaleStatestics <- renderValueBox({
        sex_wise_data = dataGenderwise()
        print(sex_wise_data)
        total = sum(sex_wise_data$Freq)
        female = sex_wise_data$Freq[2]
        femalepercent = percent(female/total)
        
        valueBox(
            "Female", paste0("Total: ", female, " Percentage: ", femalepercent), icon = icon("female"),
            color = "olive"
        )
    })
    
    output$totalStatestics <- renderValueBox({
        sex_wise_data = dataGenderwise()
        
        total = sum(sex_wise_data['Freq'])
        text
        valueBox(
            "Total", paste0("Total: ", total), icon = icon("list"),
            color="blue"
        )
    })
}