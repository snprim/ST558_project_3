#
# Author: Shih-Ni Prim
# Course: ST 558
# Project 3
# Date: 2020-11-11
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
      breast <- read_csv("../data.csv") %>% select(-X33) %>% rename(concave_points_mean = `concave points_mean`, concave_points_se = `concave points_se`, concave_points_worst = `concave points_worst`)
    })
    # this one with only continuous variables
    getData1 <- reactive({
      breast1C <- getData() %>% select(-id, -diagnosis) 
    })
    vals <- reactiveValues()
    observe(vals$data <- getData())
    # Visualization page
    # create bar plot for diagnosis
    output$bar <- renderPlot({
        ggplot(getData(), aes(x = diagnosis)) + geom_bar() + ggtitle("Barplot of Diagnosis")
    })
    # create histogram
    output$plotHist <- renderPlot({
      hist(getData()[[input$histg]], main = paste0("Histogram of ", input$histg), xlab = input$histg, breaks = input$breaks)
    })
    # create scatterplot
    scat <- eventReactive(input$plotScatter, {
      req(input$scatter)
      data <- getData() %>% select(input$scatter)
      plot_ly(data, x = ~ data[[1]], y = ~ data[[2]]) %>% layout(xaxis = list(title = colnames(data[,1])), yaxis = list(title = colnames(data[,2])))
    })
    output$scatterP <- renderPlotly({
      scat()
    })
    # summary statistic for response 
    output$diagSum <- DT::renderDataTable({
      x <- getData() %>% select(diagnosis) %>% apply(MARGIN = 2, FUN = summary)
      DT::datatable(x, caption = "Summary statistics of diagnosis")
    })
    # summary statistics for selected variable
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
      plot <- autoplot(PCs, data = getData(), colour = "diagnosis", loadings = TRUE, loadings.colour = "blue", loadings.label = TRUE, loadings.label.size = 4)
      list(sub = sub, PCs = PCs, plot = plot)
    })
    # select all variables if selectAllP is clicked
    observeEvent(input$selectAllP, {
      updateSelectInput(session, "PCAVarz", selected = colnames(getData1()))
    })
    # clear all variables if clearAllP is clicked
    observeEvent(input$clearAllP, {
      updateSelectInput(session, "PCAVarz", selected = NA)
    })
    output$pca <- renderPlot({
      pca()$plot
    })
    
    # Model page
    # run the chosen model
    model <- eventReactive(input$runModel, {
      req(input$modelVarz)
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
        fit <-train(diagnosis ~ ., data = trainBreast, method = "knn", tuneGrid = expand.grid(data.frame(k=input$k[1]:input$k[2])), trControl = trainControl(method = "cv", number = 10))
        # summary(fit)
      } else if (input$modelName == "ranfor"){
        fit <- train(diagnosis ~ ., data = trainBreast, method = "rf", tuneGrid = expand.grid(data.frame(mtry = input$mtry)), trControl = trainControl(method = "cv", number = 10))
        # summary(fit)
      }
      pred <- predict(fit, newdata = testBreast)
      mat <- confusionMatrix(as.factor(testBreast$diagnosis), reference = pred)
      tab <- round(as.matrix(mat, what = "overall"), 3)
      colnames(tab) <- c("rate")
      list(mat = mat, tab = tab, fit = fit)
    })
    # select all variables if selectAllM is clicked
    observeEvent(input$selectAllM, {
        updateSelectInput(session, "modelVarz", selected = colnames(getData1()))
    })
    # clear all variables if clearAllM is clicked
    observeEvent(input$clearAllM, {
      updateSelectInput(session, "modelVarz", selected = NA)
    })
    # print out the predictors user selected
    output$predictors <- renderText({
      preds <- paste(model()$fit$coefnames, collapse = ", ")
      paste0("The variables in your model are: ", preds)
    })
    # accuracy table
    output$accuracy <- DT::renderDataTable({
      model()$tab
    })
    # plot for the final model
    plotz <- eventReactive(input$runModel, {
      if (input$modelName == "knn"){
        plot <- plot(model()$fit, main = "")
      } else if (input$modelName == "log" || input$modelName == "ranfor") {
        plot <- plot(model()$fit$finalModel, main = "")
      }
      vals$plot <- plot
      print(plot)
    })
    # model plot
    output$modelPlot <- renderPlot({
      plotz()
    })
    # confusion matrix
    output$confusion <- renderTable({
      model()$mat$table
    })
    # download model plot
    output$downloadP <- downloadHandler(
      filename = function(){
        "modeling.png"
      },
      content = function(file){
        png(file)
        if (input$modelName == "knn"){
          print(vals$plot)
        } else {
          plot(model()$fit$finalModel, main = "")
        }
        dev.off()
      }
    )
    
    # user input prediction
    pred <- eventReactive(input$predictNow, {
      req(model())
      newdata <- data.frame(radius_mean = input$radius_mean, 
                            texture_mean = input$texture_mean,
                            perimeter_mean = input$perimeter_mean,
                            area_mean = input$area_mean,
                            smoothness_mean = input$smoothness_mean,
                            compactness_mean = input$compactness_mean,
                            concavity_mean = input$concavity_mean,
                            concave_points_mean = input$concave_points_mean,
                            symmetry_mean = input$symmetry_mean,
                            fractal_dimension_mean = input$fractal_dimension_mean,
                            radius_se = input$radius_se, 
                            texture_se = input$texture_se,
                            perimeter_se = input$perimeter_se,
                            area_se = input$area_se,
                            smoothness_se = input$smoothness_se,
                            compactness_se = input$compactness_se,
                            concavity_se = input$concavity_se,
                            concave_points_se = input$concave_points_se,
                            symmetry_se = input$symmetry_se,
                            fractal_dimension_se = input$fractal_dimension_se,
                            radius_worst = input$radius_worst, 
                            texture_worst = input$texture_worst,
                            perimeter_worst = input$perimeter_worst,
                            area_worst = input$area_worst,
                            smoothness_worst = input$smoothness_worst,
                            compactness_worst = input$compactness_worst,
                            concavity_worst = input$concavity_worst,
                            concave_points_worst = input$concave_points_worst,
                            symmetry_worst = input$symmetry_worst,
                            fractal_dimension_worst = input$fractal_dimension_worst
                            )
      result <- predict(model()$fit, newdata)
    })
    # print out prediction result
    output$newPredRes <- renderText({
      input$predictNow
      req(pred())
      ifelse(pred() == "B", "The prediction is Benign.", "The prediction is Malignant.")
    })
    # prediction result dataframe
    output$newPred <- renderTable({
      data.frame(Prediction = pred())
    })
    # Data page
    # show data table
    
    observeEvent(input$datVarzSelected, {
      req(input$datVarz)
      vals$data <- getData() %>% select(isolate(input$datVarz))
    })
    output$tab <- DT::renderDataTable({
      DT::datatable(vals$data, options = list(scrollX = TRUE))
    })
    # subset <- eventReactive(input$datVarzSelected, {
    #   req(input$datVarz)
    #   subset <- getData() %>% select(isolate(input$datVarz))
    # })
    # output$tab <- DT::renderDataTable({
    #     input$datVarzSelected
    #     DT::datatable(subset(), options = list(scrollX = TRUE))
    # })
    # select all variables if selectAllD is clicked
    observeEvent(input$selectAllD, {
      updateSelectInput(session, "datVarz", selected = colnames(getData()))
    })
    # clear all variables if selectAllD is clicked
    observeEvent(input$clearAllD, {
      updateSelectInput(session, "datVarz", selected = NA)
    })
    # download csv file
    output$download <- downloadHandler(
        filename = function(){
            "breastCancerSubset.csv"
            },
        content = function(file){
            write.csv(vals$data, file, row.names = FALSE)
            }
    )
})