## --------
## server.R
## --------

library(shiny)
library(tools)
library(vegan)
library(corrplot)

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
  
  output$corplot <- renderPlot({
  ## Correlation between departments
  ### Check the number of interacting deparmtents
  
  ## need code to connect empty dataframes
  source("cbind.r")
  
  ## load data
  req(input$infile)
  df <- read.csv(input$infile$datapath)
  depts <- unique(df$Division.combined)
  
  ## empty data frames to load correlation values into
  
  intval2 <- data.frame(matrix(ncol=length(depts),nrow=length(depts)))
  
  for(j in 1:length(depts)){
    intval <- data.frame() ## empty row dataframe
    for(i in 1:length(depts)){
      dept.1 <- subset(df, Division.combined==depts[j], select=c("If","Then.simplified")) ## select risks from department "j"
      dept.1 <- paste(dept.1$If,dept.1$Then.simplified) ## combine into 1 column
      dept.i <- subset(df, Division.combined==depts[i], select=c("If","Then.simplified")) ## select risks from department "i"
      dept.i <- paste(dept.i$If,dept.i$Then.simplified) ## combine into 1 column
      val <- data.frame(dept=length(intersect(dept.1,dept.i))) ## count number of duplicates between two departments
      rownames(val) <- paste(depts[i]) # attach department "i" name
      intval <- rbind(intval,val) ## attach to empty dataframe
    }
    intval2[,j] <- intval ## attach completed row into empty dataframe
    colnames(intval2)[j] <- paste(depts[j]) ##attach department "j" name
    
  }
  intval2[,"dept1"] <- colnames(intval2)
  cordata <- gather(intval2, dept2, value, 1:(ncol(intval2)-1))
  ggplot(data = cordata, aes(x=dept1, y=dept2, fill=value)) + 
    geom_tile()+ 
    scale_fill_gradient(low = "white", high = "red") + ylab("") + xlab("")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
})
}
  
  
  