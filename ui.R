## -----
## ui.R
## -----

library(shiny)
library(markdown)
library(tidyverse)

shinyUI(fluidPage(sidebarLayout(

    sidebarPanel(fileInput('infile', label = NULL),
                 uiOutput("department")
                ),
                 
    mainPanel(
              
              tabsetPanel(type = "tabs",
                          tabPanel("ReadMe", includeMarkdown("README.md")),
                          tabPanel("Department", tableOutput("contents"),plotOutput(outputId = "riskPlot")),
                          tabPanel("Corporate", tableOutput("cityrisk"),plotOutput(outputId = "cityplot"),plotOutput(outputId = "ordplot"),plotOutput(outputId = "corplot"))
                          
    )
))))
