#
# Author: Shih-Ni Prim
# Course: ST 558
# Project 3
# Date: 2020-11-8
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)


breast <- read_csv("../data.csv") %>% select(-X33)
breast1C <- breast %>% select(-id, -diagnosis) %>% rename(concave_points_mean = `concave points_mean`, concave_points_se = `concave points_se`, concave_points_worst = `concave points_worst`)

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
                                           h4("This dataset from 1995 contains data from Wisconsin about breast cancer. It can be found on Kaggle's ", tags$a(href = "https://www.kaggle.com/uciml/breast-cancer-wisconsin-data", "breast cancer dataset"), " which includes 569 data points.", "The same data set is listed on ", tags$a(href = "https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)", span(" UCI Machine Learning Repository.", style = "font-style:italic"))),
                                           h4("Each row presents measurements of a fine needle aspirate (FNA) of a breast mass and describes 10 characteristics of the cell nuclei, including radius, texture, perimeter, area, smoothness, compactness, concavity, concave points, symmetry, and fractal dimension. Means, standards, largest (worst) values of these characteristics are included. Thus there are 30 predictors in total and 1 response variable. The goal is to use these predictors to predict whether the mass is benign or malignant, which is indicated in the variable", strong("diagnosis."))
                                           ),
                                    column(6, 
                                           h3("How to use this app"),
                                           h4("On the lefthand side, you can see five tabs, which lead to different pages with various options.", span("On top there is a button where you can click to make the tabs disappear or appear.", style = "font-style:italic"), "By closing the tabs, the main body of the app appears larger."),
                                           h4("Each tab provides options for different information or functions. The first tab", strong("About"), "provides information about the dataset and how to use this Shiny app, as you are reading."),
                                           h4("The second tab,", strong("Data Summaries,"), "first presents a bar plot and summaries of the response variable", em("diagnosis."), "Then you can choose one variable to see its histogram and summary statistics. You can also choose two variables to see their scatterplot. Notice that the scatterplot is interactive; you can hover over points or zoom in as needed."),
                                           h4("The third tab,", strong("Principal Component Analysis (PCA),"), "leads to a page where you can choose a number of variables to see how they contribute to the first two principal components."),
                                           h4("The fourth tab,", strong("Modeling,"), "first offers options to choose a model, variables, and output type (confusion matrix, accuracy statistics, or both). The users can then set values of predictors to see the prediction and accuracy statistics."),
                                           h4("The last tab,", strong("Subset and Save Data,"), "is where you can choose the variables to be included in the dataset and then download it. If you would like a full dataset, choose", em("select all"), "and then download it.")
                                           )
                                           )
                            ),
                            tabItem(
                                tabName = "visual",
                                fluidRow(
                                  column(3,
                                         box(width = 12,
                                             selectInput("histg", "Select a continuous variable for histogram and summary Statistics", choices = colnames(breast1C)),
                                             sliderInput("breaks", "Select number of breaks for the histogram", value = 50, min = 30, max = 100),
                                             selectizeInput("scatter", "Select two variables to plot a scatterplot", choices = colnames(breast1C), selected = NULL, multiple = TRUE, options = list(maxItems = 2, placeholder = 'select 2 variables')),
                                             actionButton("plotScatter", "Create Scatterplot")
                                             )
                                         ),
                                  column(9,
                                         box(width = 6,
                                             plotOutput("bar")),
                                         box(width = 6,
                                             DT::dataTableOutput("diagSum"),
                                         box(width = 12,
                                             plotOutput("plotHist"),
                                             DT::dataTableOutput("sumz")),
                                         box(width = 12,
                                             plotlyOutput("scatterP"))
                                         )
                                )
                            )),
                            tabItem(
                              tabName = "cluster",
                              fluidRow(
                                column(3,
                                       box(width = 12,
                                           selectInput("PCAVarz", "Select Variables and click 'Run PCA'", choices = colnames(breast1C), multiple = TRUE),
                                           actionButton("PCAVarzSelected", "Run PCA now"),
                                           actionButton("selectAllP", "Select All"),
                                           actionButton("clearAllP", "Clear All")
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
                                           conditionalPanel(condition = "input.modelName == 'log'", h6("Logistic regression is suitable for binary responses. The basic logistic regression model:", withMathJax("$$P(success)=\\dfrac{e^{\\beta_0+\\beta_1x}}{1+e^{\\beta_0+\\beta_1x}}$$"))),
                                           conditionalPanel(condition = "input.modelName == 'knn'", h6("K Nearest Neighbor uses the closest k observations in terms of distance (usually Euclidean) to make prediction.")),
                                           conditionalPanel(condition = "input.modelName == 'ranfor'", h6("The random forest model random chooses a subset of (m) predictors from the p variables for the model. A convention is to choose", withMathJax("$$m=\\sqrt{p}$$"), "for classification.")),
                                           radioButtons("outputType", "What type of text output would you like to see?", choices = c("Confusion matrix" = "conMat", "Accuracy statistics" = "acc", "Both" = "both")),
                                           conditionalPanel(condition = "input.modelName == 'knn'", sliderInput("k", "Enter k range", min = 2, max = 10, value = c(5,6))),
                                           conditionalPanel(condition = "input.modelName == 'ranfor'", sliderInput("mtry", "Enter mtry", min = 2, max = 10, value = 5) ),
                                           selectInput("modelVarz", "Choose variables for the model", multiple = TRUE, choices = c("radius_mean" = "radius_mean",
                           "texture_mean" = "texture_mean",
                           "perimeter_mean" = "perimeter_mean",
                           "area_mean" = "area_mean",
                           "smoothness_mean" = "smoothness_mean",
                           "compactness_mean" = "compactness_mean",
                           "concavity_mean" = "concavity_mean",
                           "concave_points_mean" = "concave_points_mean",
                           "symmetry_mean" = "symmetry_mean",
                           "fractal_dimension_mean" = "fractal_dimension_mean",
                           "radius_se" = "radius_se",
                           "texture_se" = "texture_se",
                           "perimeter_se" = "perimeter_se",
                           "area_se" = "area_se",
                           "smoothness_se" = "smoothness_se",
                           "compactness_se" = "compactness_se",
                           "concavity_se" = "concavity_se",
                           "concave_points_se" = "concave_points_se",
                           "symmetry_se" = "symmetry_se",
                           "fractal_dimension_se" = "fractal_dimension_se",
                           "radius_worst" = "radius_worst",
                           "texture_worst" = "texture_worst",
                           "perimeter_worst" = "perimeter_worst",
                           "area_worst" = "area_worst",
                           "smoothness_worst" = "smoothness_worst",
                           "compactness_worst" = "compactness_worst",
                           "concavity_worst" = "concavity_worst",
                           "concave_points_worst" = "concave_points_worst",
                           "symmetry_worst" = "symmetry_worst",
                           "fractal_dimension_worst" = "fractal_dimension_worst"
                           )),
                                           actionButton("runModel", "Run"),
                                           actionButton("selectAllM", "Select All"),
                                           actionButton("clearAllM", "Clear All"),
                                           downloadButton("downloadP", "Download Plot")
                                           ),
                                       box(width = 12,
                                           conditionalPanel(condition = "input.modelVarz.indexOf('radius_mean') > -1", sliderInput("radius_mean", "radius_mean", min = 6, max = 28, step = 0.1, value = 14)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('texture_mean') > -1", sliderInput("texture_mean", "texture_mean", min = 9, max = 40, step = 0.1, value = 19)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('perimeter_mean') > -1", sliderInput("perimeter_mean", "perimeter_mean", min = 43, max = 190, step = 0.1, value = 92)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('area_mean') > -1", sliderInput("area_mean", "area_mean", min = 143, max = 2501, step = 0.1, value = 655)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('smoothness_mean') > -1", sliderInput("smoothness_mean", "smoothness_mean", min = 0.05, max = 0.2, step = 0.01, value = 0.1)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('compactness_mean') > -1", sliderInput("compactness_mean", "compactness_mean", min = 0.01, max = 0.34, step = 0.01, value = 0.1)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('concavity_mean') > -1", sliderInput("concavity_mean", "concavity_mean", min = 0, max = 0.42, step = 0.01, value = 0.09)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('concave_points_mean') > -1", sliderInput("concave_points_mean", "concave_points_mean", min = 0, max = 0.2, step = 0.01, value = 0.05)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('symmetry_mean') > -1", sliderInput("symmetry_mean", "symmetry_mean", min = 0.1, max = 0.3, step = 0.01, value = 0.18)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('fractal_dimension_mean') > -1", sliderInput("fractal_dimension_mean", "fractal_dimension_mean", min = 0.04, max = 0.1, step = 0.01, value = 0.06)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('radius_se') > -1", sliderInput("radius_se", "radius_se", min = 0.1, max = 2.9, step = 0.01, value = 0.4)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('texture_se') > -1", sliderInput("texture_se", "texture_se", min = 0.36, max = 4.9, step = 0.01, value = 1.22)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('perimeter_se') > -1", sliderInput("perimeter_se", "perimeter_se", min = 0.8, max = 22, step = 0.1, value = 2.9)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('area_se') > -1", sliderInput("area_se", "area_se", min = 6, max = 543, step = 0.1, value = 40)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('smoothness_se') > -1", sliderInput("smoothness_se", "smoothness_se", min = 0, max = 0.3, step = 0.001, value = 0.007)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('compactness_se') > -1", sliderInput("compactness_se", "compactness_se", min = 0, max = 0.15, step = 0.01, value = 0.2)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('concavity_se') > -1", sliderInput("concavity_se", "concavity_se", min = 0, max = 0.4, step = 0.01, value = 0.03)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('concave_points_se') > -1", sliderInput("concave_points_se", "concave_points_se", min = 0, max = 0.05, step = 0.01, value = 0.01)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('symmetry_se') > -1", sliderInput("symmetry_se", "symmetry_se", min = 0, max = 0.08, step = 0.01, value = 0.02)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('fractal_dimension_se') > -1", sliderInput("fractal_dimension_se", "fractal_dimension_se", min = 0, max = 0.03, step = 0.001, value = 0.003)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('radius_worst') > -1", sliderInput("radius_worst", "radius_worst", min = 7.9, max = 36.1, step = 0.1, value = 16.3)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('texture_worst') > -1", sliderInput("texture_worst", "texture_worst", min = 12, max = 50, step = 0.1, value = 25.7)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('perimeter_worst') > -1", sliderInput("perimeter_worst", "perimeter_worst", min = 50, max = 252, step = 0.1, value = 107.3)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('area_worst') > -1", sliderInput("area_worst", "area_worst", min = 185, max = 4255, step = 1, value = 881)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('smoothness_worst') > -1", sliderInput("smoothness_worst", "smoothness_worst", min = 0.07, max = 0.22, step = 0.01, value = 0.13)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('compactness_worst') > -1", sliderInput("compactness_worst", "compactness_worst", min = 0.02, max = 1.06, step = 0.01, value = 0.25)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('concavity_worst') > -1", sliderInput("concavity_worst", "concavity_worst", min = 0, max = 1.25, step = 0.01, value = 0.27)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('concave_points_worst') > -1", sliderInput("concave_points_worst", "concave_points_worst", min = 0, max = 0.3, step = 0.01, value = 0.11)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('symmetry_worst') > -1", sliderInput("symmetry_worst", "symmetry_worst", min = 0.1, max = 0.7, step = 0.01, value = 0.3)),
                                           conditionalPanel(condition = "input.modelVarz.indexOf('fractal_dimension_worst') > -1", sliderInput("fractal_dimension_worst", "fractal_dimension_worst", min = 0.05, max = 0.21, step = 0.01, value = 0.08)),
                                           actionButton("predictNow", "Create Prediction")
                                           )
                                       ),
                                column(9,
                                       box(width = 12,
                                           h4("The variables in your model are: "),
                                           textOutput("predictors"),
                                           br(),
                                           conditionalPanel(condition = "input.outputType == 'conMat' || input.outputType == 'both'", tableOutput("confusion")),
                                           conditionalPanel(condition = "input.outputType == 'acc' || input.outputType == 'both'", DT::dataTableOutput("accuracy")),
                                           plotOutput("modelPlot")
                                           ),
                                       box(width = 12,
                                           h4("The prediction from your input is: "),
                                           tableOutput("newPred"))
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