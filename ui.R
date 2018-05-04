## -----
## ui.R
## -----

library(shiny)
library(markdown)
library(tidyverse)
library(plotly)
library(DT)

shinyUI(fluidPage(sidebarLayout(

    sidebarPanel(fileInput('infile', label = NULL),
                 uiOutput("department")
                ),
                 
    mainPanel(
              
              tabsetPanel(type = "tabs",
                          tabPanel("ReadMe", includeHTML("index.html"),includeMarkdown("README.md")),
                          tabPanel("Department", tableOutput("contents"),plotOutput(outputId = "riskPlot"),plotOutput(outputId = "consPlot")),
                          tabPanel("Corporate", 
                                   fluidPage(
                                     fluidRow(tableOutput("cityrisk"),plotOutput(outputId = "cityplot"),plotOutput(outputId = "ordplot"),
                                              plotlyOutput(outputId = "corplot"),verbatimTextOutput("info"),
                                              plotOutput(outputId="networkplot")),
                                     # Create a new Row in the UI for selectInputs
                                     fluidRow(
                                       column(3,
                                              selectInput("riskType",
                                                          "Risk Type:",
                                                          c("Max","Sum"))
                                       ),
                                       column(3,
                                              selectInput("CurrentRisk",
                                                          "Current Risk:",
                                                          c("All",
                                                            unique(as.character(df$max.current.qual))))
                                       ),
                                       column(3,
                                              selectInput("FutureRisk",
                                                          "Future Risk:",
                                                          c("All",
                                                            unique(as.character(df$max.future.qual))))
                                       )
                                     ),
                                     # Create a new row for the table.
                                     fluidRow(
                                       DT::dataTableOutput("table")
                                     )
                                   )
))))))
