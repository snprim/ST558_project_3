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

breast <- read_csv("../data.csv")
breast1 <- breast %>% select(-X33)
colnames(breast1)
breast1C <- breast1 %>% select(-id, -diagnosis)

server <- shinyServer(function(input, output, session) { 
    # Visualization page
    # create bar plot for diagnosis
    output$bar <- renderPlot({
        ggplot(breast1, aes(x = diagnosis)) + geom_bar() + ggtitle("Bar Plot of Diagnosis")
    })
    # create histogram
    output$plotHist <- renderPlot({
      hist(breast1[[input$histg]], main = paste0("Histogram of ", input$histg), xlab = input$histg, breaks = input$breaks)
    })
    # summary statistics
    sum <- reactive({
       req(input$histg)
       x <- breast1C %>% select(input$histg) 
    })
    output$sumz <- DT::renderDataTable({
        x <- sum() %>% apply(MARGIN = 2, FUN = summary)
        DT::datatable(x)
        })
    # Cluster page
    # Model page
    # Data page
    # show data table--full or subset
    output$tab <- DT::renderDataTable({
        DT::datatable(subset(), options = list(scrollX = TRUE))
    })
    # subset table if varzSelected is clicked
    subset <- reactive({
        breast2 <- breast1 %>% select(variablez())
    })
    # save dataset--full or subset
    subset <- reactive({
        if (input$varzSelected){
            breast2 <- breast1 %>% select(input$varz)
        } else {
            breast2 <- breast1
        }
    })
    # select all variables if selectAll is clicked
    observeEvent(input$selectAll, {
        updateCheckboxGroupInput(session, "varz", selected = colnames(breast1))
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