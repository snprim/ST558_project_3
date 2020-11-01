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
                            menuItem("Visualization", tabName = "visual")
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
                                           h3("About this dataset")),
                                    column(6, 
                                           h3("How to use this app"))
                                )
                            ),
                            tabItem(
                                tabName = "visual",
                            )
                        )
                    )
                    
)