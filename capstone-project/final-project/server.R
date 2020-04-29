source("./app-utill.R")
final4Data <- readRDS(file="./ngram-data/final4Data.RData")
final3Data <- readRDS(file="./ngram-data/final3Data.RData")
final2Data <- readRDS(file="./ngram-data/final2Data.RData")


shinyServer(function(input, output) {
  
  wordPrediction <- reactive({
    text <- input$inputText
    textInput <- cleanInput(text)
    wordCount <- length(textInput)
    wordPrediction <- nextWordPrediction(wordCount,textInput)
  })
  
  output$predictedText <- renderText({
    test <- wordPrediction()
    test
  })
  output$enteredText <- renderText({ input$inputText }, quoted = FALSE)
})