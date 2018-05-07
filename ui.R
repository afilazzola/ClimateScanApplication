## -----
## ui.R
## -----

library(shiny)
library(markdown)
library(tidyverse)
library(plotly)
library(DT)
library(shinythemes)
library(shinycssloaders)

shinyUI(fluidPage(theme = shinytheme("flatly"),sidebarLayout(

    sidebarPanel(fileInput('infile', label = NULL),
                 uiOutput("department")
                ),
                 
    mainPanel(
              
              tabsetPanel(type = "tabs",
                          tabPanel("ReadMe", 
                                   fluidPage(htmlOutput("inc"),includeMarkdown("README.md"))),
                          tabPanel("Department", withSpinner(tableOutput("contents"), type=6, color="#18BC9D"),plotOutput(outputId = "riskPlot"),includeMarkdown("frequencyplot.md"),
                                   plotOutput(outputId = "consPlot"),includeMarkdown("consWordcloud.md"),plotOutput(outputId = "wordcloud")),
                          tabPanel("Corporate", 
                                   fluidPage(
                                     fluidRow(tableOutput("cityrisk"),withSpinner(plotOutput(outputId = "cityplot"), type=6, color="#18BC9D"),includeMarkdown("frequencyplot.md"),
                                              plotOutput(outputId = "ordplot"),includeMarkdown("ordination.md"),
                                              plotlyOutput(outputId = "corplot"),verbatimTextOutput("info"),includeMarkdown("interaction.md"),
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
