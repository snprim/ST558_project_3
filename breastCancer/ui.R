#
# Author: Shih-Ni Prim
# Course: ST 558
# Project 3
# Date: 2020-11-1
#

library(shiny)
library(shinydashboard)
library(tidyverse)

breast <- read_csv("../data.csv")
breast1C <- breast %>% select(-id, -diagnosis)

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
                                  column(3,
                                         box(width = 12,
                                             selectInput("histg", "Select a continuous variable for histogram and summary Statistics", choices = colnames(breast1C)),
                                             sliderInput("breaks", "Select", value = 50, min = 30, max = 100)
                                             )
                                         ),
                                  column(9,
                                         box(width = 12,
                                             plotOutput("bar"),
                                             plotOutput("plotHist"),
                                             DT::dataTableOutput("sumz")
                                             )
                                         )
                                )
                            ),
                            tabItem(
                              tabName = "cluster",
                              h3("Test2")
                            ),
                            tabItem(
                              tabName = "model",
                              h3("Test3")
                            ),
                            tabItem(
                                tabName = "dat",
                                fluidRow(
                                    withMathJax(),
                                    column(3,
                                           box(width = 12,
                                               actionButton("varzSelected", "Subset Datatable"),
                                               actionButton("selectAll", "Select All"),
                                               actionButton("selectNone", "Select None"),
                                               h6("If you would like to download a subset, select variables, click 'Subset Datatable', and click the 'Download File' button below."),
                                               downloadButton("download", "Download File"),
                                               checkboxGroupInput("varz", "Select Varaibles", choices = colnames(breast))
                                               )
                                           ),
                                    column(9,
                                           box(width = 12,
                                               DT::dataTableOutput("tab"))
                                           )
                                )
                            )
                        )
                    )
                    
)