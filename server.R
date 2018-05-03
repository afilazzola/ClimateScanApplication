## --------
## server.R
## --------

library(shiny)
library(tools)
library(vegan)

server <- function(input, output) {
  
  ## Classify departemnts
  output$department <- renderUI({
    req(input$infile)
    df <- read.csv(input$infile$datapath)
    department <- paste(df[,"Division"],df[,"Sub.Identifier"], sep=":")
    department <- department[!duplicated(department)]
    selectInput("department", "Choose Department", department)
  })
  
  ## department risk table
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$infile)
    
    df <- read.csv(input$infile$datapath)
    df[,"department"] <- paste(df[,"Division"],df[,"Sub.Identifier"], sep=":")
    
    climate.div <- df  %>% group_by(If,department) %>% summarize(freq.risk=length(Future.risk.quant)) 
    climate.div <- data.frame(climate.div)
    climate.div <- subset(climate.div, department==input$department)
    
  })
  ## department plot
  output$riskPlot <- renderPlot({
    
    req(input$infile)
    
    df <- read.csv(input$infile$datapath)
    df[,"department"] <- paste(df[,"Division"],df[,"Sub.Identifier"], sep=":")
    
    climate.div <- df  %>% group_by(If,department) %>% summarize(freq.risk=length(Future.risk.quant)) 
    climate.div <- data.frame(climate.div)
    climate.div <- subset(climate.div, department==input$department)
    
    ggplot(climate.div, aes(x=If, y=freq.risk))+geom_bar(stat="identity") + coord_flip() + ylab("number of risks") + xlab("")+theme_bw()
  })
  ## Municipal Wide Risks
  output$cityrisk <- renderTable({
    
    req(input$infile)
    
    df <- read.csv(input$infile$datapath)
    
    climate.div <- df  %>% group_by(If) %>% summarize(freq.risk=length(Future.risk.quant)) 
    climate.div <- data.frame(climate.div)
    return(climate.div)
    
  })
  ## Municipal Wide Risks Plot
  output$cityplot <- renderPlot({
    
    req(input$infile)
    
    df <- read.csv(input$infile$datapath)
    
    climate.div <- df  %>% group_by(If) %>% summarize(freq.risk=length(Future.risk.quant)) 
    ggplot(climate.div, aes(x=If, y=freq.risk))+geom_bar(stat="identity") + coord_flip() + ylab("number of risks") + xlab("")+theme_bw()
    
  })
  
  ## Ordination Plot
  output$ordplot <- renderPlot({
  
  ## load dataset
  req(input$infile)
  df <- read.csv(input$infile$datapath)
  df[,"department"] <- paste(df[,"Division"],df[,"Sub.Identifier"], sep=":")
  
  ## Calculate mean consequences per department
  data.mean <- df %>% group_by(department) %>% 
    select(Financial,Damage.Property.Technology,People,Environment,Business.Continuity,Reputation,Critical.Infra) %>% ## drop extra columns 
    summarize_all(funs(mean(., na.rm = TRUE)))
    ord.dat <- data.mean[,2:ncol(data.mean)] ## select only consequence columns
  
  
  ##conduct NMDS
  nmds1 <- metaMDS(ord.dat, k=2)
 
  ## enhance plot
  par(mar=c(4.5,5.5,.5,.5))
  ordiplot(nmds1, type="n", xlim=c(-1,0.5))
  orditorp(nmds1, display="species", col="#E69F00", cex=1.4, air=0.1)
  orditorp(nmds1, display="sites", col="#56B4E9", cex=1.2, pch="")
  
  
})
}
  
  
  