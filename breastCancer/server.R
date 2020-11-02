#
# Author: Shih-Ni Prim
# Course: ST 558
# Project 3
# Date: 2020-10-31
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

breast <- read_csv("../data.csv")
breast1 <- breast %>% select(-X33)
colnames(breast1)

server <- shinyServer(function(input, output, session) { 
    
    # show full data table if no variables are selected
    output$tab <- DT::renderDataTable({
        DT::datatable(subset(), options = list(scrollX = TRUE))
    })
    # output$tab <- DT::renderDataTable({
    #     if (input$varzSelected){
    #         DT::datatable(breast2(), options = list(scrollX = TRUE))
    #     } else {
    #         DT::datatable(breast1, options = list(scrollX = TRUE))
    #     }
    # })
    subset <- reactive({
        if (input$varzSelected){
            breast2 <- breast1 %>% select(input$varz)
        } else {
            breast2 <- breast1
        }
    })
    # create a subset if action button "varzSelected" is clicked
    # breast2 <- eventReactive(input$varzSelected, {
    #     breast2 <- breast1 %>% select(input$varz)
    # })
    # download button for .csv
    output$download <- downloadHandler(
        filename = function(){
            "breastCancerSubset.csv"
            },
        content = function(file){
            write.csv(subset())
            },
        contentType = "csv"
    )
    
})