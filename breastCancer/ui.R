#
# Author: Shih-Ni Prim
# Course: ST 558
# Project 3
# Date: 2020-10-31
#

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
                    dashboardHeader(
                        title = "Breast Cancer Prognosis Prediction",
                        titleWidth = 1000
                        
                    ),
                    #sidebar items
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("About", tabName = "about"),
                            menuItem("Visualization", tabName = "visual"),
                            menuItem("Clustering/PCA", tabName = "cluster"),
                            menuItem("Modeling", tabName = "model"),
                            menuItem("Data", tabName = "dat")
                        )
                    ),
                    
                    #main body of the app
                    dashboardBody(
                        tabItems(
                            tabItem(
                                tabName = "about",
                                fluidRow(
                                    withMathJax(),
                                    column(6, 
                                           h3("About this datasetg")),
                                    column(6, 
                                           h3("How to use this app"))
                                )
                            ),
                            tabItem(
                                tabName = "visual",
                                fluidRow(
                                    withMathJax(),
                                    column(2,
                                           box(width = 12,
                                               h4("Choose one variable"),
                                               radioButtons("var", "Select one variable", choices = list("test", "test2"))),
                                           box(width = 12,
                                               checkboxGroupInput("varz", "Select Varaibles", choices = list("test", "test2")))),
                                    column(10,
                                           box(width = 12,
                                               dataTableOutput("tab"))
                                           )
                                )
                            )
                        )
                    )
                    
)