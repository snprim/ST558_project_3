#
# Author: Shih-Ni Prim
# Course: ST 558
# Project 3
# Date: 2020-11-1
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)


server <- shinyServer(function(input, output, session) { 
    # set up three datasets: first one full
    getData <- reactive({
      breast <- read_csv("../data.csv") %>% select(-X33)
    })
    # this one with only continuous variables
    getData1 <- reactive({
      breast1C <- getData() %>% select(-id, -diagnosis)
    })
    # Visualization page
    # create bar plot for diagnosis
    output$bar <- renderPlot({
        ggplot(getData(), aes(x = diagnosis)) + geom_bar() + ggtitle("Bar Plot of Diagnosis")
    })
    # create histogram
    output$plotHist <- renderPlot({
      hist(getData()[[input$histg]], main = paste0("Histogram of ", input$histg), xlab = input$histg, breaks = input$breaks)
    })
    # summary statistics
    sum <- reactive({
       req(input$histg)
       x <- getData() %>% select(input$histg) 
    })
    output$sumz <- DT::renderDataTable({
        x <- sum() %>% apply(MARGIN = 2, FUN = summary)
        DT::datatable(x)
        })
    # Cluster page
    # Model page
    choice <- eventReactive(input$runModel, {
      input$modelName
    })
    output$modeling <- DT::renderDataTable({
      data <- getData()
      train <- sample(1:nrow(data), size = nrow(data)*0.8)
      test <- setdiff(1:nrow(data), train)
      trainBreast <- data[train,]
      testBreast <- data[-train,]
      # run models
      if (choice() == "log"){
        fit <- train(diagnosis ~ ., data = trainBreast, method = "glm", family = "binomial", trControl = trainControl(method = "cv", number = 10))
        # summary(fit)
        pred <- predict(fit, newdata = testBreast)
        mat <- confusionMatrix(as.factor(testBreast$diagnosis), reference = pred)
      } else if (choice() == "knn"){
        fit <- train(diagnosis ~ ., data = trainBreast, method = "knn", tuneGrid = expand.grid(data.frame(k=input$k)), trControl = trainControl(method = "cv", number = 10))
        # summary(fit)
        pred <- predict(fit, newdata = testBreast)
        mat <- confusionMatrix(as.factor(testBreast$diagnosis), reference = pred)
      } else if (choice() == "ranfor"){
        fit <- train(diagnosis ~ ., data = trainBreast, method = "rf", tuneGrid = expand.grid(data.frame(mtry = input$mtry)), trControl = trainControl(method = "cv", number = 10))
        # summary(fit)
        pred <- predict(fit, newdata = testBreast)
        mat <- confusionMatrix(as.factor(testBreast$diagnosis), reference = pred)
      }
      tab <- round(as.matrix(mat, what = "overall"), 3)
      colnames(tab) <- c("rate")
      return(tab)
    })
    # Data page
    # show data table--full or subset
    output$tab <- DT::renderDataTable({
        DT::datatable(subset(), options = list(scrollX = TRUE))
    })
    # subset table if varzSelected is clicked
    subset <- reactive({
        breast2 <- getData() %>% select(variablez())
    })
    # save dataset--full or subset
    subset <- reactive({
        if (input$varzSelected){
            breast2 <- getData() %>% select(input$varz)
        } else {
            breast2 <- getData()
        }
    })
    # select all variables if selectAll is clicked
    observeEvent(input$selectAll, {
        updateCheckboxGroupInput(session, "varz", selected = colnames(getData()))
    })
    # unselect variables if selectNone is clicked
    observeEvent(input$selectNone, {
        updateCheckboxGroupInput(session, "varz", selected = NA)
    })
    # download csv file
    output$download <- downloadHandler(
        filename = function(){
            "breastCancerSubset.csv"
            },
        content = function(file){
            write.csv(subset(), file, row.names = FALSE)
            }
    )

})