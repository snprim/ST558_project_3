#
# Author: Shih-Ni Prim
# Course: ST 558
# Project 3
# Date: 2020-11-5
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(ggfortify)
library(caret)
library(plotly)


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
    # summary statistic for response 
    output$diagSum <- DT::renderDataTable({
      x <- getData() %>% select(diagnosis) %>% apply(MARGIN = 2, FUN = summary)
      DT::datatable(x)
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
    pca <- eventReactive(input$PCAVarzSelected, {
      req(input$PCAVarz)
      sub <- getData1() %>% select(input$PCAVarz)
      PCs <- prcomp(sub, scale = TRUE)
      plot2 <- autoplot(PCs, data = getData(), colour = "diagnosis", loadings = TRUE, loadings.colour = "blue", loadings.label = TRUE, loadings.label.size = 4)
      list(sub = sub, PCs = PCs, plot2 = plot2)
    })
    # select all variables if selectAll is clicked
    observeEvent(input$selectAllP, {
      updateSelectInput(session, "PCAVarz", selected = colnames(getData1()))
    })
    
    observeEvent(input$clearAllP, {
      updateSelectInput(session, "PCAVarz", selected = NA)
    })
    # output$pca1 <- renderPlot({
    #   biplot(pca()$PCs, xlabs = rep(".", nrow(pca()$sub)), cex = 1.2)
    # })
    output$pca2 <- renderPlot({
      pca()$plot2
    })
    
    # Model page
    model <- eventReactive(input$runModel, {
      data <- getData() %>% select(input$modelVarz, diagnosis)
      train <- sample(1:nrow(data), size = nrow(data)*0.8)
      test <- setdiff(1:nrow(data), train)
      trainBreast <- data[train,]
      testBreast <- data[-train,]
      # run models
      if (input$modelName == "log"){
        fit <- train(diagnosis ~ ., data = trainBreast, method = "glm", family = "binomial", trControl = trainControl(method = "cv", number = 10))
        # summary(fit)
      } else if (input$modelName == "knn"){
        fit <-train(diagnosis ~ ., data = trainBreast, method = "knn", tuneGrid = expand.grid(data.frame(k=input$k)), trControl = trainControl(method = "cv", number = 10))
        # summary(fit)
      } else if (input$modelName == "ranfor"){
        fit <- train(diagnosis ~ ., data = trainBreast, method = "rf", tuneGrid = expand.grid(data.frame(mtry = input$mtry)), trControl = trainControl(method = "cv", number = 10))
        # summary(fit)
      }
      pred <- predict(fit, newdata = testBreast)
      mat <- confusionMatrix(as.factor(testBreast$diagnosis), reference = pred)
      tab <- round(as.matrix(mat, what = "overall"), 3)
      colnames(tab) <- c("rate")
      list(tab = tab, fit = fit)
    })
    
    output$selections <- renderTable({
      paste(input$modelVarz, collapse = ",")
    })
    
    observeEvent(input$selectAllM, {
        updateSelectInput(session, "modelVarz", selected = colnames(getData1()))
    })
    
    observeEvent(input$clearAllM, {
      updateSelectInput(session, "modelVarz", selected = NA)
    })
    
    output$predictors <- renderText({
      model()$fit$coefnames
    })
    
    output$accuracy <- DT::renderDataTable({
      model()$tab
    })
    
    plotz <- eventReactive(input$runModel, {
      if (input$modelName == "knn"){
        plot(model()$fit)
      } else {
        plot(model()$fit$finalModel)
      }
    })
    
    output$modelPlot <- renderPlot({
      plotz()
    })
    
    # Data page
    # show data table--full or subset
    output$tab <- DT::renderDataTable({
        input$datVarzSelected
        subset <- getData() %>% select(isolate(input$datVarz))
        DT::datatable(subset, options = list(scrollX = TRUE))
    })
    # select all variables if selectAll is clicked
    observeEvent(input$selectAllD, {
      updateSelectInput(session, "datVarz", selected = colnames(getData1()))
    })
    
    observeEvent(input$clearAllD, {
      updateSelectInput(session, "datVarz", selected = NA)
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