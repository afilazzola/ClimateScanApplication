## -----
## ui.R
## -----



shinyUI(fluidPage(theme = shinytheme("flatly"),sidebarLayout(

    sidebarPanel(fileInput('infile', label = "Choose a File"),
                 selectInput("department","Department",c(Choose="")),
                 selectInput("division","Division",c(Choose="")),
                 uiOutput("download")
                              ),
                 
    mainPanel(
              
              tabsetPanel(type = "tabs",
                          tabPanel("ReadMe", 
                                   fluidPage( includeMarkdown("index.md"),
                                             includeMarkdown("README.md"))),
                          tabPanel("Department", withSpinner(tableOutput("contentsDept"), type=6, color="#18BC9D"),plotOutput(outputId = "riskPlotDept"),includeMarkdown("frequencyplot.md"),
                                   plotOutput(outputId = "consPlotDept"),includeMarkdown("consWordcloud.md"),plotOutput(outputId = "wordcloudDept")
                          ),
                          tabPanel("Division", withSpinner(tableOutput("contents"), type=6, color="#18BC9D"),plotOutput(outputId = "riskPlot"),includeMarkdown("frequencyplot.md"),
                                   plotOutput(outputId = "consPlot"),includeMarkdown("consWordcloud.md"),plotOutput(outputId = "wordcloud")
                                   ),
                          tabPanel("Corporate",
                                   fluidPage(
                                     fluidRow(tableOutput("cityrisk"),withSpinner(plotOutput(outputId = "cityplot"), type=6, color="#18BC9D"),includeMarkdown("frequencyplot.md"),
                                              plotOutput(outputId = "ordplot"),includeMarkdown("ordination.md"),
                                              plotlyOutput(outputId = "corplot"),includeMarkdown("interaction.md")
                                              ),
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
                                                          c("All","Low Risk","Medium Risk","High Risk","Extreme Risk"))
                                       ),
                                       column(3,
                                              selectInput("FutureRisk",
                                                          "Future Risk:",
                                                          c("All","Low Risk","Medium Risk","High Risk","Extreme Risk"))
                                       )
                                     ),
                                     # Create a new row for the table.
                                     fluidRow(
                                       DT::dataTableOutput("table")
                                     )
                                    )
                          ))
))))
