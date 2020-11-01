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

server <- shinyServer(function(input, output, session) { 
    
    output$tab <- DT::renderDataTable({
        DT::datatable(breast1, options = list(scrollX = TRUE))
    })
    
    
    })