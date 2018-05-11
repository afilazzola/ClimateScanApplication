## --------
## server.R
## --------

# options(shiny.maxRequestSize=30*1024^2) 
# options(shiny.trace=TRUE)

server <- function(input, output, session) {
  # 
  # getPage<-function() {
  #   return(includeHTML("index.html"))
  # }
  # output$inc<-renderUI({getPage()})

  ## load data for server with no sample dataset
  filedata <- reactive({
    req(input$infile)
    df.in <- risk.calc(input$infile$datapath)
    df.in[,"Future.risk.quant"] <- df.in[,"sum.future.quant"]
    df.in[,"division"] <- paste(df.in[,"Department"],df.in[,"Division"], sep=":") ## creates a unique column that combines division and subidentifier
    df.in
  })

  
  output$demo <- downloadHandler(
    filename = paste0("ClimScan", Sys.Date(), ".csv", sep=""),
    content = function(file) {
      write.csv(risk.calc(input$infile$datapath), file)
    })

  output$download <- renderUI({
    if(!is.null(input$infile))  {
      downloadButton('demo', 'Download Output File')
    }
  })

  
  # ## load data for server with sample dataset
  # filedata <- reactive({
  #   if (is.null(input$inFile$datapath)) {
  #     df.in <- risk.calc("test.csv")
  #     df.in[,"Future.risk.quant"] <- df.in[,"sum.future.quant"]
  #     df.in[,"department"] <- paste(df.in[,"Department"],df.in[,"Division"], sep=":") ## creates a unique column that combines division and subidentifier
  #     return(df.in)
  #   } else {
  #     df.in <- risk.calc(input$infile$datapath)
  #     df.in[,"Future.risk.quant"] <- df.in[,"sum.future.quant"]
  #     df.in[,"department"] <- paste(df.in[,"Department"],df.in[,"Division"], sep=":") ## creates a unique column that combines division and subidentifier
  #     return(df.in)
  #   }
  # })
  
  ## Classify departemnts
  observe({
    df.in <- filedata() ## loads main file
    div.options <- as.vector(unique(df.in$division))
    updateSelectInput(session, "division", choices = div.options)
  })
  
  ## Classify departemnts
  observe({
    df.in <- filedata() ## loads main file
    dept.options <- as.vector(unique(df.in$Department))
    updateSelectInput(session, "department", choices = dept.options)
  })
    
  ## division risk table
  output$contents <- renderTable({
    
    ## require division input
    req(input$division)
    ## loads main file
    df.in <- filedata() 
    
    climate.div <- df.in  %>% group_by(If,division) %>% summarize(freq.risk=length(Future.risk.quant))  ## counts number of risks
    climate.div <- data.frame(climate.div) ## puts it into a traditional data frame
    climate.div <- subset(climate.div, division==input$division) ## subsets based on division identified from dropdown

  })
  
  ## department risk table
  output$contentsDept <- renderTable({
    
    ## require division input
    req(input$department)
    ## loads main file
    df.in <- filedata() 
    
    climate.div <- df.in  %>% group_by(If,Department) %>% summarize(freq.risk=length(Future.risk.quant))  ## counts number of risks
    climate.div <- data.frame(climate.div) ## puts it into a traditional data frame
    climate.div <- subset(climate.div, Department==input$department) ## subsets based on department identified from dropdown
    
  })
  
  ## division plot based on risks
  output$riskPlot <- renderPlot({
    
    ## require division input
    req(input$division)
    ## loads main file
    df.in <- filedata() 
    
    ## Counts number of risks
    climate.div <- df.in  %>% group_by(If,division) %>% summarize(freq.risk=length(Future.risk.quant)) 
    climate.div <- data.frame(climate.div) ## puts it into a traditional data frame
    dept.clim <- subset(climate.div, division==input$division) 
    ymax <- max(dept.clim$freq.risk)+2
    ## Plots the risks identifed within each division
    ggplot(dept.clim, aes(x=If, y=freq.risk))+geom_bar(stat="identity") + coord_flip() + ylab("number of risks") + xlab("")+
      ## improve plot look
      theme_bw(base_size = 16)+scale_y_continuous(expand = c(0, 0), limits=c(0,ymax))  +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  })
  
  ## department plot based on risks
  output$riskPlotDept <- renderPlot({
    
    ## require division input
    req(input$department)
    ## loads main file
    df.in <- filedata() 
    
    ## Counts number of risks
    climate.div <- df.in  %>% group_by(If,Department) %>% summarize(freq.risk=length(Future.risk.quant)) 
    climate.div <- data.frame(climate.div) ## puts it into a traditional data frame
    dept.clim <- subset(climate.div, Department==input$department) 
    ymax <- max(dept.clim$freq.risk)+2
    ## Plots the risks identifed within each Department
    ggplot(dept.clim, aes(x=If, y=freq.risk))+geom_bar(stat="identity") + coord_flip() + ylab("number of risks") + xlab("")+
      ## improve plot look
      theme_bw(base_size = 16)+scale_y_continuous(expand = c(0, 0), limits=c(0,ymax))  +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  
  
  ## division consequence rankings
  output$consPlot <- renderPlotly({
    
    ## require division input
    req(input$division)
    ## loads main file
    df.in <- filedata() 
    
    ## calulates the mean consequences
    consequence <- df.in  %>% filter(division==input$division) %>% ## subset to only specific division
      gather(consequence, value, Financial:Critical.Infra) %>% group_by(consequence) %>%  ## extract columns for consequences and identify as groups
      summarize(avg=mean(value))
    ymax <- max(consequence$avg)+1
    ggplot(consequence, aes(x=consequence, y=avg))+geom_bar(stat="identity") + coord_flip() + ylab("average consequence ranking") + xlab("")+
    ## improve plot look
    theme_bw(base_size = 16)+scale_y_continuous(expand = c(0, 0), limits=c(0,ymax))  +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    
  })

  ## Department consequence rankings
  output$consPlotDept <- renderPlotly({
    
    ## require department input
    req(input$department)
    ## loads main file
    df.in <- filedata() 
    
    ## calulates the mean consequences
    consequence <- df.in  %>% filter(Department==input$department) %>% ## subset to only specific division
      gather(consequence, value, Financial:Critical.Infra) %>% group_by(consequence) %>%  ## extract columns for consequences and identify as groups
      summarize(avg=mean(value))
    ymax <- max(consequence$avg)+1
    ggplot(consequence, aes(x=consequence, y=avg))+geom_bar(stat="identity") + coord_flip() + ylab("average consequence ranking") + xlab("")+
      ## improve plot look
      theme_bw(base_size = 16)+scale_y_continuous(expand = c(0, 0), limits=c(0,ymax))  +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  

  ## Municipal Wide Risks rendered into a table
  output$cityrisk <- renderTable({

    df.in <- filedata() ## loads main file

  # Counts number of climate risks
    climate.corp <- df.in  %>% group_by(If) %>% summarize(freq.risk=length(Future.risk.quant))
    climate.corp <- data.frame(climate.corp) ## puts it into a traditional data frame
    return(climate.corp) ## returned dataframe of risks for corporate to be rendered into a table

  })
  ## Municipal Wide Risks Plot
  output$cityplot <- renderPlot({

    df.in <- filedata() ## loads main file

    ## counts the number of identified risks
    climate.corp <- df.in  %>% group_by(If) %>% summarize(freq.risk=length(Future.risk.quant))
    ## plots the risk for the division
    ymax <- max(climate.corp$freq.risk)+2
    ggplot(climate.corp, aes(x=If, y=freq.risk))+geom_bar(stat="identity") + coord_flip() + ylab("number of risks") + xlab("")+
      ## improve plot look
      theme_bw(base_size = 16)+scale_y_continuous(expand = c(0, 0), limits=c(0,ymax))  +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  })

  ## Ordination Plot
  output$ordplot <- renderPlot({

  ## load dataset
  df.in <- filedata() ## loads main file

  ## Calculate mean consequences per division
  data.mean <- df.in %>% group_by(division) %>%
    select(Financial,Damage.Property.Technology,People,Environment,Business.Continuity,Reputation,Critical.Infra) %>% ## drop extra columns
    summarize_all(funs(mean(., na.rm = TRUE)))
    ord.dat <- data.mean[,2:ncol(data.mean)] ## select only consequence columns


  ##conduct NMDS
  nmds1 <- metaMDS(ord.dat, k=2) ## conduct a NMDS (non-parametric multidimensional scaling)

  ## enhance plot
  par(mar=c(4.5,5.5,.5,.5)) ## set margins
  ordiplot(nmds1, type="n") ## plot ordination without points
  orditorp(nmds1, display="species", col="#E69F00", cex=1.4, air=0.1) ## add consequences
  orditorp(nmds1, display="sites", col="#56B4E9", cex=1.2, pch="") ## add divisions
  })

  ## Create an interaction dataset for correlation and network analysis
  network <- reactive({

  ## load data
  df.in <- filedata() ## loads main file
  dept.short <- substr(as.character(df.in$Department),1,5) ## creates shortened depatment name
  div.short <- substr(as.character(df.in$Division),1,10) ## creates shortened division name
  df.in[,"div.simple"] <- paste(dept.short,div.short, sep=":") ## combines shortned names to simplified total
  depts <- unique(df.in$div.simple) ## identify unique departments

  ## empty data frames to load correlation values into
  intval2 <- data.frame(matrix(ncol=length(depts),nrow=length(depts)))

  for(j in 1:length(depts)){
    intval <- data.frame() ## empty row dataframe
    for(i in 1:length(depts)){
      dept.1 <- subset(df.in, div.simple==depts[j], select=c("If","Then")) ## select risks from department "j"
      dept.1 <- paste(dept.1$If,dept.1$Then.simplified) ## combine into 1 column
      dept.i <- subset(df.in, div.simple==depts[i], select=c("If","Then")) ## select risks from department "i"
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

    # Filter data based on Risks
    output$table <- DT::renderDataTable(DT::datatable({
      df.in <- filedata() ## loads main file
      if(input$riskType=="Max"){
      if (input$CurrentRisk != "All") {
        df.in <- df.in[df.in$max.current.qual == input$CurrentRisk,]
      }
      if (input$FutureRisk != "All") {
        df.in <- df.in[df.in$max.future.qual == input$FutureRisk,]
      }} else{
        if (input$CurrentRisk != "All") {
          df.in <- df.in[df.in$sum.current.qual == input$CurrentRisk,]
        }
        if (input$FutureRisk != "All") {
          df.in <- df.in[df.in$sum.future.qual == input$FutureRisk,]
        }
      }
      df.in[,c("Department","Division","If","Then","So","max.current.qual","max.future.qual","sum.current.qual","sum.future.qual")]
    }))


    ### WordClouds
    output$wordcloud <- renderPlot({

    ## require division input
    req(input$division)
    ## loads main file
    df.in <- filedata() 

    ## subset based on departemtn
    dept.words <- subset(df.in, division==input$division)

    ## load text from Then and So
    text_df <- data_frame(text=as.character(dept.words$Then),as.character(dept.words$So))
    word.count <- text_df %>% unnest_tokens(word,text) %>%  dplyr::count(word, sort=T) ## count words

    ## list stopwords
    stopwords <- c(stop_words$word, "risk","e.g.","e.g","due","increases","decreases","impacts","impact","lack","risks", "increase","increased","increasing","decrease","decreased","decreasing","lower","low","high","higher")

    ## remove stopwords
    stop1 <- text_df %>% unnest_tokens(word,text) ## separate words into separate lines
    stop2 <- stop1[!stop1$word %in% stopwords,] ## remove stop words
    stop3 <- stop2 %>% dplyr::count(word, sort=T) ## sort words by frequency
    par(mar=c(0,0,0,0))
    wordcloud(stop3$word, stop3$n, max.words=60, min.freq=1)
    })

    ### WordClouds
    output$wordcloudDept <- renderPlot({
      
      ## require department input
      req(input$department)
      ## loads main file
      df.in <- filedata() 
      
      ## subset based on departemt
      dept.words <- subset(df.in, Department==input$department)
      
      ## load text from Then and So
      text_df <- data_frame(text=as.character(dept.words$Then),as.character(dept.words$So))
      word.count <- text_df %>% unnest_tokens(word,text) %>%  dplyr::count(word, sort=T) ## count words
      
      ## list stopwords
      stopwords <- c(stop_words$word, "risk","e.g.","e.g","due","increases","decreases","impacts","impact","lack","risks", "increase","increased","increasing","decrease","decreased","decreasing","lower","low","high","higher")
      
      ## remove stopwords
      stop1 <- text_df %>% unnest_tokens(word,text) ## separate words into separate lines
      stop2 <- stop1[!stop1$word %in% stopwords,] ## remove stop words
      stop3 <- stop2 %>% dplyr::count(word, sort=T) ## sort words by frequency
      
      par(mar=c(0,0,0,0))
      wordcloud(stop3$word, stop3$n, max.words=60, min.freq=1)
    })
}



