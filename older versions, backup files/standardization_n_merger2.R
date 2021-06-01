### 1. LIBRARIES ###

install.packages("curl") 
install.packages("magrittr") 
install.packages("dplyr")
install.packages("htmlwidgets")
install.packages("scales")
install.packages("reshape")
install.packages("tidyverse")
install.packages("RSQLite")
install.packages("lattice")
install.packages("corrplot")
install.packages("ggthemes")
install.packages("ggrepel")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("ISLR")
install.packages("ggplot2")
install.packages("lmtest")
install.packages("aTSA")
install.packages("forecast")
install.packages("plotly")
install.packages("ggfortify")
install.packages("tseries")
install.packages("gridExtra")
install.packages("docstring")
install.packages("readr")
install.packages("here")
install.packages("EnvStats")
install.packages("rugarch")
install.packages("GGally")
#install.packages("gtrendsR")
install.packages("plyr")
install.packages("lubridate")
install.packages("dplyr")
install.packages("gtools")
install.packages("neuralnet")
devtools::install_github("PMassicotte/gtrendsR")
#devtools::install_github("PMassicotte/gtrendsR@low-search-volume")
devtools::install_github('diplodata/gtrendsR')
#devtools::install_github("unitroot/gtrendsR")
install.packages("iotools")

library(curl)
library(gtrendsR)
library(magrittr) 
library(dplyr)
library(htmlwidgets)
library(scales)
library(reshape)
library(tidyverse)
library(RSQLite)
library(lattice)
library(corrplot)
library(ggthemes)
library(ggrepel)
library(quantmod)
library(PerformanceAnalytics)
library(ISLR)
library(ggplot2)
library(lmtest)
library(aTSA)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
library(EnvStats)
library(rugarch)
library(GGally)
library(gtrendsR)
library(plyr)
library(lubridate)
library(dplyr)
library(gtools)
library(neuralnet)
library(iotools)



  
  ######################################################
  ######################################################
  ######################################################
  
  key_words <- c('booming')


  kw=key_words



  
for (w in 1:length(kw) )  {
    print(kw[w])
    gt_daily <- read.csv.raw(file= paste(as.character( kw[w] ), "_daily.csv", sep="") , 
                         header=TRUE, sep="," ) ## RAW!!!!!!!!!!!
    names(gt_daily) <- c("time","hit")
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
    
    
    if (k==1) {
      gt_train <- gt_df
      #gt_train$interest <- na.approx(gt_train$interest, na.rm=FALSE)
      train_returns <- merge(train_returns, gt_train, by="time", all.x = TRUE)
      print("train data completed")
    } else {
      gt_test <- filter( gt_df, time >= as.Date(test_dates[1]) )
      #gt_test$interest <- na.approx(gt_test$interest, na.rm=FALSE)
      test_returns <- merge(test_returns, gt_test, by="time", all.x = TRUE)
      print("test data completed")
    }


  ### 6. DOW JONES INDEX - RETURNS ### 
  
  ticker <- "DJI"
  security <- getSymbols(Symbols = ticker, auto.assign = FALSE, from="2004-01-01", to="2019-12-31",
                         getSymbols.warning4.0 = FALSE) %>% na.approx()
  returns <- Return.calculate(security, method = "log")
  returns <- returns[-1, ]              #remove first row
  returns <- data.frame(time = as.Date(index(returns)), coredata(returns))  %>% 
    select(time, DJI.Adjusted)
  returns <- mutate(returns,DJI.Adjusted_lead = lead(DJI.Adjusted))
  returns <- returns[-nrow(returns),]   #remove last row
  returns <- returns %>% select(time, DJI.Adjusted_lead)                     
  names(returns) <- c("time","y_lead")
  
  last_train_date <- train_dates[length(train_dates)] %>% as.Date()
  train_returns <- returns %>% filter(time <= last_train_date )
  test_returns <- returns %>% filter(time > last_train_date )