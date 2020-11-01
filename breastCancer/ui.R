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
                                           h3("About this datasetg"),
                                           h4("This dataset can be found on Kaggle's ", tags$a(href = "https://www.kaggle.com/uciml/breast-cancer-wisconsin-data", "breast cancer dataset"), " which includes 569 data points.", "The same data set is listed on ", tags$a(href = "https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)", span(" UCI Machine Learning Repository.", style = "font-style:italic")))),
                                    column(6, 
                                           h3("How to use this app"),
                                           h4("On the left hand side, you can see various tabs, which lead to different pages with various options.", span("On top there is a button where you can click to make the tabs disappear or appear.", style = "font-style:italic"), "By closing the tabs, the main body of the app appears larger.")
                                           ),
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
                                               checkboxGroupInput("varz", "Select Varaibles", choices = list("test", "test2")))
                                           ),
                                    column(10,
                                           box(width = 12,
                                               DT::dataTableOutput("tab"))
                                           )
                                )
                            )
                        )
                    )
                    
)