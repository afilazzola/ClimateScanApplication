risk.calc <- function(x){
df <- read.csv(x, stringsAsFactors = FALSE)
df[is.na(df)] <- 0

## Calculate the maximum and summed values for all consequences
df[,"max.consequence"] <- df %>% select(Financial,Damage.Property.Technology,People,Environment,Business.Continuity,Reputation,Critical.Infra) %>% apply(., 1, max)
df[,"sum.consequence"] <- df %>% select(Financial,Damage.Property.Technology,People,Environment,Business.Continuity,Reputation,Critical.Infra) %>% apply(., 1, sum, na.rm=T)


## Create risk data frames with value and quantification
current.risk.vals <- data.frame(CurrentScenario=c("Rare","Unlikely","Likely","Very Likely","Almost Certain"), current.val=c(1,2,3,4,5))
future.risk.vals <- data.frame(FutureScenario=c("Rare","Unlikely","Likely","Very Likely","Almost Certain"),  future.val=c(1,2,3,4,5))
df1 <- merge(df, current.risk.vals, by=c("CurrentScenario"))
df2 <- merge(df1, future.risk.vals, by=c("FutureScenario"))


## calculates maximum risk rating qualitative
max.riskrating <- function(x, y) {
  ## consequence 1
  ifelse(x==1 & y <5, "Low Risk",
         ifelse(x==1 & y==5, "Moderate Risk",
                ## consequence 2
                ifelse(x==2 & y<3, "Low Risk",
                       ifelse(x==2 & y>2,"Moderate Risk",
                              ## consequence 3
                              ifelse(x==3 & y==1, "Low Risk",
                                     ifelse(x==3 & y<4, "Moderate Risk",
                                            ifelse(x==3 & y>3, "High Risk",
                                                   ## consequence 4
                                                   ifelse(x==4 & y==1, "Low Risk",
                                                          ifelse(x==4 & y==2, "Moderate Risk",
                                                                 ifelse(x==4 & y <5, "High Risk",
                                                                        ifelse(x==4 & y==5, "Extreme Risk",
                                                                               ## consequence 5
                                                                               ifelse(x==5 & y<4, "High Risk","Extreme Risk")
                                                                        )))))))))))
}

sum.riskrating <- function(x, y) {
  ## consequence 1
  ifelse(x<8 & y <5, "Low Risk",
         ifelse(x<8 & y>4, "Moderate Risk",
                ## consequence 2
                ifelse(x<15 & y<3, "Low Risk",
                       ifelse(x<15 & y>2,"Moderate Risk",
                              ## consequence 3
                              ifelse(x<22 & y<2, "Low Risk",
                                     ifelse(x<22 & y<4, "Moderate Risk",
                                            ifelse(x<22 & y>3, "High Risk",
                                                   ## consequence 4
                                                   ifelse(x<29 & y==1, "Low Risk",
                                                          ifelse(x<29 & y==2, "Moderate Risk",
                                                                 ifelse(x<29 & y ==3, "High Risk",
                                                                        ifelse(x<29 & y ==4, "High Risk",
                                                                               ifelse(x<29 & y==5, "Extreme Risk",
                                                                                      ## consequence 5
                                                                                      ifelse(x<36 & y<4, "High Risk","Extreme Risk")
                                                                               ))))))))))))
}





## Maximum current risk rating qualitative
for(i in 1:nrow(df2)){
  df2[,"max.current.qual"] <- max.riskrating(x=df2$max.consequence,y=df2$current.val)
}
## Maximum future risk rating qualitative
for(i in 1:nrow(df2)){
  df2[,"max.future.qual"] <- max.riskrating(x=df2$max.consequence,y=df2$future.val)
}
## Sum current risk rating qualitative
for(i in 1:nrow(df2)){
  df2[,"sum.current.qual"] <- sum.riskrating(x=df2$sum.consequence,y=df2$current.val)
}
## Sum future risk rating qualitative
for(i in 1:nrow(df2)){
  df2[,"sum.future.qual"] <- sum.riskrating(x=df2$sum.consequence,y=df2$future.val)
}


## Convert qualitative risks to quantitiative (numbers)
risk.values <- data.frame(risk.qual=c("No risk","Low Risk","Moderate Risk","High Risk","Extreme Risk"), risk.quant=c(1,2,3,4,5))

risk.num <- function(x){ ## write a function to find and replace text with word
risk.values$risk.quant[risk.values$risk.qual  %in%  x] ## find and replace risk wording with a numbers
}

## use function for each column  of max and su,
df2[,"max.current.quant"] <- unlist(lapply(df2$max.current.qual,risk.num))  ## max current 
df2[,"max.future.quant"] <- unlist(lapply(df2$max.future.qual,risk.num))  ## max future
df2[,"sum.current.quant"] <- unlist(lapply(df2$sum.current.qual,risk.num))  ## sum current 
df2[,"sum.future.quant"] <- unlist(lapply(df2$sum.future.qual,risk.num))  ## sum future

return(df2)
}