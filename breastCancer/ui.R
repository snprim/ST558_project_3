#
# Author: Shih-Ni Prim
# Course: ST 558
# Project 3
# Date: 2020-11-5
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)


breast <- read_csv("../data.csv") %>% select(-X33)
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
                            menuItem("Data Summaries", tabName = "visual"),
                            menuItem("Principal Component Analysis (PCA)", tabName = "cluster"),
                            menuItem("Modeling", tabName = "model"),
                            menuItem("Subset and Save Data", tabName = "dat")
                        )
                    ),
                    
                    #main body of the app
                    dashboardBody(
                        tabItems(
                            tabItem(
                                tabName = "about",
                                fluidRow(
                                    column(6, 
                                           h3("About this dataset"),
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
                                  column(3,
                                         box(width = 12,
                                             selectInput("histg", "Select a continuous variable for histogram and summary Statistics", choices = colnames(breast1C)),
                                             sliderInput("breaks", "Select", value = 50, min = 30, max = 100)
                                             )
                                         ),
                                  column(9,
                                         box(width = 6,
                                             plotOutput("bar")),
                                         box(width = 6,
                                             DT::dataTableOutput("diagSum"),
                                             # equation
                                             withMathJax("$$\\beta^2$$")),
                                         box(width = 12,
                                             plotOutput("plotHist"),
                                             DT::dataTableOutput("sumz")
                                             )
                                         )
                                )
                            ),
                            tabItem(
                              tabName = "cluster",
                              fluidRow(
                                column(3,
                                       box(width = 12,
                                           selectInput("PCAVarz", "Select Variables and click 'Run PCA'", choices = colnames(breast1C), multiple = TRUE),
                                           actionButton("PCAVarzSelected", "Run PCA now"),
                                           actionButton("selectAllP", "Select All"),
                                           actionButton("clearAllP", "Clear All")
                                           
                                           
                                           # checkboxGroupInput("pcaVarz", "Select Variables and click 'Run PCA'", choices = colnames(breast1C)),
                                           # actionButton("runPCA", "Run PCA now")
                                           )
                                       ),
                                column(9,
                                       box(width = 12, 
                                       plotOutput("pca2")
                                           )
                                       )
                              )
                            ),
                            tabItem(
                              tabName = "model",
                              fluidRow(
                                column(3,
                                       box(width = 12,
                                           radioButtons("modelName", "Select a model", choices = c("logistic regression" = "log", "kNN" = "knn", "random forest" = "ranfor")),
                                           conditionalPanel(condition = "input.modelName == 'knn'", sliderInput("k", "Enter k range", min = 2, max = 10, value = c(5,6))),
                                           conditionalPanel(condition = "input.modelName == 'ranfor'", sliderInput("mtry", "Enter mtry", min = 2, max = 10, value = 5) ),
                                           selectInput("modelVarz", "Choose variables for the model", choices = colnames(breast1C), multiple = TRUE),
                                           actionButton("runModel", "Run"),
                                           actionButton("selectAllM", "Select All"),
                                           actionButton("clearAllM", "Clear All")
                                           )
                                       ),
                                column(9,
                                       box(width = 12,
                                           h4("The variables in your model are: "),
                                           textOutput("predictors"),
                                           br(),
                                           DT::dataTableOutput("accuracy"),
                                           plotOutput("modelPlot")
                                           )
                                       )
                              )
                            ),
                            tabItem(
                                tabName = "dat",
                                fluidRow(
                                    column(3,
                                           box(width = 12,
                                               selectInput("datVarz", "Choose variables for the model", choices = colnames(breast1C), multiple = TRUE),
                                               actionButton("datVarzSelected", "View Datatable"),
                                               actionButton("selectAllD", "Select All"),
                                               actionButton("clearAllD", "Clear All"),
                                               downloadButton("download", "Download File")
                                               # checkboxGroupInput("varz", "To download the dataset, select variables, click 'View Datatable', and then 'Download File'.", choices = colnames(breast))
                                               )
                                           ),
                                    column(9,
                                           h4("Please select variables below and then click 'View Datatable'"),
                                           box(width = 12,
                                               DT::dataTableOutput("tab"))
                                           )
                                )
                            )
                        )
                    )
                    
)