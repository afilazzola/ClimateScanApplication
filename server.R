## --------
## server.R
## --------

library(shiny)
library(tools)
library(vegan)
library(corrplot)
library(igraph)
library(statnet)
library(plotly)
source("riskcalculations.r")

server <- function(input, output) {
  ## load data for server for multiple uses
  filedata <- reactive({
    req(input$infile) ## requires the input of a file
    df <- risk.calc(input$infile$datapath)
    df[,"Future.risk.quant"] <- df[,"sum.future.quant"]
    #df <- read.csv(input$infile$datapath) ## reads a csv file
    df[,"department"] <- paste(df[,"Division"],df[,"Sub.Identifier"], sep=":") ## creates a unique column that combines division and subidentifier
    df
  })
  
  
  ## Classify departemnts
  output$department <- renderUI({
    df <- filedata() ## loads main file
    department <- df$department ## selects department column only
    department <- department[!duplicated(department)] ## removes duplicates to get list of departments
    selectInput("department", "Choose Department", department) ## creates a UI based on department names
  })
  
  ## department risk table
  output$contents <- renderTable({
    df <- filedata() ## loads main file
    climate.div <- df  %>% group_by(If,department) %>% summarize(freq.risk=length(Future.risk.quant))  ## counts number of risks
    climate.div <- data.frame(climate.div) ## puts it into a traditional data frame
    climate.div <- subset(climate.div, department==input$department) ## subsets based on department identified from dropdown
    
  })
  
  ## department plot based on risks
  output$riskPlot <- renderPlot({
    
    df <- filedata() ## loads main file
    
    ## Counts number of risks
    climate.div <- df  %>% group_by(If,department) %>% summarize(freq.risk=length(Future.risk.quant)) 
    climate.div <- data.frame(climate.div) ## puts it into a traditional data frame
    climate.div <- subset(climate.div, department==input$department) 
    ## Plots the risks identifed within each department
    ggplot(climate.div, aes(x=If, y=freq.risk))+geom_bar(stat="identity") + coord_flip() + ylab("number of risks") + xlab("")+theme_bw()
  })
  
  ## department consequence rankings
  output$consPlot <- renderPlot({
    
    df <- filedata() ## loads main file
    
    ## calulates the mean consequences
    consequence <- df  %>% filter(department==input$department) %>% ## subset to only specific department
      gather(consequence, value, Financial:Critical.Infra) %>% group_by(consequence) %>%  ## extract columns for consequences and identify as groups
      summarize(avg=mean(value))
    ggplot(consequence, aes(x=consequence, y=avg))+geom_bar(stat="identity") + coord_flip() + ylab("average consequence ranking") + xlab("")+theme_bw()
      
  })
  ## Municipal Wide Risks rendered into a table
  output$cityrisk <- renderTable({
    
    df <- filedata() ## loads main file
    
    ## Counts number of climate risks
    climate.div <- df  %>% group_by(If) %>% summarize(freq.risk=length(Future.risk.quant)) 
    climate.div <- data.frame(climate.div) ## puts it into a traditional data frame
    return(climate.div) ## returned dataframe of risks for corporate to be rendered into a table
    
  })
  ## Municipal Wide Risks Plot
  output$cityplot <- renderPlot({
    
    df <- filedata() ## loads main file
    
    ## counts the number of identified risks
    climate.div <- df  %>% group_by(If) %>% summarize(freq.risk=length(Future.risk.quant))  
    ## plots the risk for the department
    ggplot(climate.div, aes(x=If, y=freq.risk))+geom_bar(stat="identity") + coord_flip() + ylab("number of risks") + xlab("")+theme_bw()
    
  })
  
  ## Ordination Plot
  output$ordplot <- renderPlot({
  
  ## load dataset
  df <- filedata() ## loads main file
  
  ## Calculate mean consequences per department
  data.mean <- df %>% group_by(department) %>% 
    select(Financial,Damage.Property.Technology,People,Environment,Business.Continuity,Reputation,Critical.Infra) %>% ## drop extra columns 
    summarize_all(funs(mean(., na.rm = TRUE)))
    ord.dat <- data.mean[,2:ncol(data.mean)] ## select only consequence columns
  
  
  ##conduct NMDS
  nmds1 <- metaMDS(ord.dat, k=2) ## conduct a NMDS (non-parametric multidimensional scaling)
 
  ## enhance plot
  par(mar=c(4.5,5.5,.5,.5)) ## set margins
  ordiplot(nmds1, type="n", xlim=c(-1,0.5)) ## plot ordination without points
  orditorp(nmds1, display="species", col="#E69F00", cex=1.4, air=0.1) ## add consequences
  orditorp(nmds1, display="sites", col="#56B4E9", cex=1.2, pch="") ## add departments
  })
  
  ## Create an interaction dataset for correlation and network analysis
  network <- reactive({
  
  ## load data
  df <- filedata() ## loads main file
  depts <- unique(df$Division.combined) ## identify unique departments
  
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
  intval2[,"dept1"] <- colnames(intval2) ## rename column with depatment name
  cordata <- gather(intval2, dept2, value, 1:(ncol(intval2)-1)) ## switch data from wide to long
  cordata 
})
  
  ## Plot a correlation plot
    output$corplot <- renderPlotly({
    cordata <- network() ## loads network data
      
    plot1 <- ggplot(data = cordata, aes(x=dept1, y=dept2, fill=value)) +  ## plot correlation 
    geom_tile()+ 
    scale_fill_gradient(low = "white", high = "red") + ylab("") + xlab("")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
    plot1
})
    
    
    ## Plot network analysis
    output$networkplot <- renderPlot({
    cordata <- network() ## loads network data
    colnames(cordata) <- c("To","From","correlation") ## rename columns for network analysis
    ## drop correlations within department (i.e. self correlations)
    cordata[cordata$To==cordata$From, "correlation"] <- 0
    
    ## top three interacting departments
    network.reduced <- cordata %>%  group_by(To)%>% top_n(3, correlation) 
    network.reduced <- data.frame(network.reduced)
    
    ## graph network
    net <- graph_from_data_frame(network.reduced [1:2], directed=TRUE)
    
    ## plot network circle
    la <- layout_in_circle(net,  order = V(net)$name)
    

    ## plot network analysis
    par(mar=c(3,3,3,3))
    plot(net, layout=la, cex=0.7,edge.width=log(network.reduced$correlation),
         edge.arrow.size=0, ## reduce arrow size
         edge.curved=T)
    
    
    })
    
    # Filter data based on Risks
    output$table <- DT::renderDataTable(DT::datatable({
      df <- filedata() ## loads main file
      if(input$riskType=="Max"){
      if (input$CurrentRisk != "All") {
        df <- df[df$max.current.qual == input$CurrentRisk,]
      }
      if (input$FutureRisk != "All") {
        df <- df[df$max.future.qual == input$FutureRisk,]
      }} else{
        if (input$CurrentRisk != "All") {
          df <- df[df$sum.current.qual == input$CurrentRisk,]
        }
        if (input$FutureRisk != "All") {
          df <- df[df$sum.future.qual == input$FutureRisk,]
        }
      }
      df[,c("Department","Division","If","Then","So","max.current.qual","max.future.qual","sum.current.qual","sum.future.qual")]
    }))
    
}

  
  
  