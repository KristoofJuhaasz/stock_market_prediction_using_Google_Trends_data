setHandleParameters(user = NULL, password = NULL, domain = NULL, proxyhost = NULL, proxyport = 8080, proxyauth = 15)

# Start with a fresh handle
h <- new_handle()

# Ask server to set some cookies
req <- curl_fetch_memory("https://eu.httpbin.org/cookies/set?foo=123&bar=ftw", handle = h)
req <- curl_fetch_memory("https://eu.httpbin.org/cookies/set?baz=moooo", handle = h)
handle_cookies(h)


### 1. LIBRARIES ###

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
  install.packages("gtrendsR")
  install.packages("plyr")
  install.packages("lubridate")
  install.packages("dplyr")
  install.packages("gtools")
  install.packages("neuralnet")
  devtools::install_github('diplodata/gtrendsR')
  
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



### 2. INPUT ###

  train_dates = c(
                  "2004-01-01 2004-01-31", "2004-02-01 2004-02-28", "2004-03-01 2004-03-31",
                  "2004-04-01 2004-04-30", "2004-05-01 2004-05-31", "2004-06-01 2004-06-30",
                  "2004-07-01 2004-07-31", "2004-08-01 2004-08-31", "2004-09-01 2004-09-30",
                  "2004-10-01 2004-10-31", "2004-11-01 2004-11-30", "2004-12-01 2004-12-31",
                  
                  "2005-01-01 2005-01-31", "2005-02-01 2005-02-28", "2005-03-01 2005-03-31",
                  "2005-04-01 2005-04-30", "2005-05-01 2005-05-31", "2005-06-01 2005-06-30",
                  "2005-07-01 2005-07-31", "2005-08-01 2005-08-31", "2005-09-01 2005-09-30",
                  "2005-10-01 2005-10-31", "2005-11-01 2005-11-30", "2005-12-01 2005-12-31",

                  "2006-01-01 2006-01-31", "2006-02-01 2006-02-28", "2006-03-01 2006-03-31",
                  "2006-04-01 2006-04-30", "2006-05-01 2006-05-31", "2006-06-01 2006-06-30",
                  "2006-07-01 2006-07-31", "2006-08-01 2006-08-31", "2006-09-01 2006-09-30",
                  "2006-10-01 2006-10-31", "2006-11-01 2006-11-30", "2006-12-01 2006-12-31",

                  "2007-01-01 2007-01-31", "2007-02-01 2007-02-28", "2007-03-01 2007-03-31",
                  "2007-04-01 2007-04-30", "2007-05-01 2007-05-31", "2007-06-01 2007-06-30",
                  "2007-07-01 2007-07-31", "2007-08-01 2007-08-31", "2007-09-01 2007-09-30",
                  "2007-10-01 2007-10-31", "2007-11-01 2007-11-30", "2007-12-01 2007-12-31",

                  "2008-01-01 2008-01-31", "2008-02-01 2008-02-28", "2008-03-01 2008-03-31",
                  "2008-04-01 2008-04-30", "2008-05-01 2008-05-31", "2008-06-01 2008-06-30",
                  "2008-07-01 2008-07-31", "2008-08-01 2008-08-31", "2008-09-01 2008-09-30",
                  "2008-10-01 2008-10-31", "2008-11-01 2008-11-30", "2008-12-01 2008-12-31",

                  "2009-01-01 2009-01-31", "2009-02-01 2009-02-28", "2009-03-01 2009-03-31",
                  "2009-04-01 2009-04-30", "2009-05-01 2009-05-31", "2009-06-01 2009-06-30",
                  "2009-07-01 2009-07-31", "2009-08-01 2009-08-31", "2009-09-01 2009-09-30",
                  "2009-10-01 2009-10-31", "2009-11-01 2009-11-30", "2009-12-01 2009-12-31",

                  "2010-01-01 2010-01-31", "2010-02-01 2010-02-28", "2010-03-01 2010-03-31",
                  "2010-04-01 2010-04-30", "2010-05-01 2010-05-31", "2010-06-01 2010-06-30",
                  "2010-07-01 2010-07-31", "2010-08-01 2010-08-31", "2010-09-01 2010-09-30",
                  "2010-10-01 2010-10-31", "2010-11-01 2010-11-30", "2010-12-01 2010-12-31",

                  "2011-01-01 2011-01-31", "2011-02-01 2011-02-28", "2011-03-01 2011-03-31",
                  "2011-04-01 2011-04-30", "2011-05-01 2011-05-31", "2011-06-01 2011-06-30",
                  "2011-07-01 2011-07-31", "2011-08-01 2011-08-31", "2011-09-01 2011-09-30",
                  "2011-10-01 2011-10-31", "2011-11-01 2011-11-30", "2011-12-01 2011-12-31",

                  "2012-01-01 2012-01-31", "2012-02-01 2012-02-28", "2012-03-01 2012-03-31",
                  "2012-04-01 2012-04-30", "2012-05-01 2012-05-31", "2012-06-01 2012-06-30",
                  "2012-07-01 2012-07-31", "2012-08-01 2012-08-31", "2012-09-01 2012-09-30",
                  "2012-10-01 2012-10-31", "2012-11-01 2012-11-30", "2012-12-01 2012-12-31",

                  "2013-01-01 2013-01-31", "2013-02-01 2013-02-28", "2013-03-01 2013-03-31",
                  "2013-04-01 2013-04-30", "2013-05-01 2013-05-31", "2013-06-01 2013-06-30",
                  "2013-07-01 2013-07-31", "2013-08-01 2013-08-31", "2013-09-01 2013-09-30",
                  "2013-10-01 2013-10-31", "2013-11-01 2013-11-30", "2013-12-01 2013-12-31",

                  "2014-01-01 2014-01-31", "2014-02-01 2014-02-28", "2014-03-01 2014-03-31",
                  "2014-04-01 2014-04-30", "2014-05-01 2014-05-31", "2014-06-01 2014-06-30",
                  "2014-07-01 2014-07-31", "2014-08-01 2014-08-31", "2014-09-01 2014-09-30",
                  "2014-10-01 2014-10-31", "2014-11-01 2014-11-30", "2014-12-01 2014-12-31",

                  "2015-01-01 2015-01-31", "2015-02-01 2015-02-28", "2015-03-01 2015-03-31",
                  "2015-04-01 2015-04-30", "2015-05-01 2015-05-31", "2015-06-01 2015-06-30",
                  "2015-07-01 2015-07-31", "2015-08-01 2015-08-31", "2015-09-01 2015-09-30",
                  "2015-10-01 2015-10-31", "2015-11-01 2015-11-30", "2015-12-01 2015-12-31"

                  )
  
  test_dates = c(
    "2004-01-01 2004-01-31", "2004-02-01 2004-02-28", "2004-03-01 2004-03-31",
    "2004-04-01 2004-04-30", "2004-05-01 2004-05-31", "2004-06-01 2004-06-30",
    "2004-07-01 2004-07-31", "2004-08-01 2004-08-31", "2004-09-01 2004-09-30",
    "2004-10-01 2004-10-31", "2004-11-01 2004-11-30", "2004-12-01 2004-12-31",
    
    "2005-01-01 2005-01-31", "2005-02-01 2005-02-28", "2005-03-01 2005-03-31",
    "2005-04-01 2005-04-30", "2005-05-01 2005-05-31", "2005-06-01 2005-06-30",
    "2005-07-01 2005-07-31", "2005-08-01 2005-08-31", "2005-09-01 2005-09-30",
    "2005-10-01 2005-10-31", "2005-11-01 2005-11-30", "2005-12-01 2005-12-31",
    
    "2006-01-01 2006-01-31", "2006-02-01 2006-02-28", "2006-03-01 2006-03-31",
    "2006-04-01 2006-04-30", "2006-05-01 2006-05-31", "2006-06-01 2006-06-30",
    "2006-07-01 2006-07-31", "2006-08-01 2006-08-31", "2006-09-01 2006-09-30",
    "2006-10-01 2006-10-31", "2006-11-01 2006-11-30", "2006-12-01 2006-12-31",

    "2007-01-01 2007-01-31", "2007-02-01 2007-02-28", "2007-03-01 2007-03-31",
    "2007-04-01 2007-04-30", "2007-05-01 2007-05-31", "2007-06-01 2007-06-30",
    "2007-07-01 2007-07-31", "2007-08-01 2007-08-31", "2007-09-01 2007-09-30",
    "2007-10-01 2007-10-31", "2007-11-01 2007-11-30", "2007-12-01 2007-12-31",

    "2008-01-01 2008-01-31", "2008-02-01 2008-02-28", "2008-03-01 2008-03-31",
    "2008-04-01 2008-04-30", "2008-05-01 2008-05-31", "2008-06-01 2008-06-30",
    "2008-07-01 2008-07-31", "2008-08-01 2008-08-31", "2008-09-01 2008-09-30",
    "2008-10-01 2008-10-31", "2008-11-01 2008-11-30", "2008-12-01 2008-12-31",

    "2009-01-01 2009-01-31", "2009-02-01 2009-02-28", "2009-03-01 2009-03-31",
    "2009-04-01 2009-04-30", "2009-05-01 2009-05-31", "2009-06-01 2009-06-30",
    "2009-07-01 2009-07-31", "2009-08-01 2009-08-31", "2009-09-01 2009-09-30",
    "2009-10-01 2009-10-31", "2009-11-01 2009-11-30", "2009-12-01 2009-12-31",

    "2010-01-01 2010-01-31", "2010-02-01 2010-02-28", "2010-03-01 2010-03-31",
    "2010-04-01 2010-04-30", "2010-05-01 2010-05-31", "2010-06-01 2010-06-30",
    "2010-07-01 2010-07-31", "2010-08-01 2010-08-31", "2010-09-01 2010-09-30",
    "2010-10-01 2010-10-31", "2010-11-01 2010-11-30", "2010-12-01 2010-12-31",

    "2011-01-01 2011-01-31", "2011-02-01 2011-02-28", "2011-03-01 2011-03-31",
    "2011-04-01 2011-04-30", "2011-05-01 2011-05-31", "2011-06-01 2011-06-30",
    "2011-07-01 2011-07-31", "2011-08-01 2011-08-31", "2011-09-01 2011-09-30",
    "2011-10-01 2011-10-31", "2011-11-01 2011-11-30", "2011-12-01 2011-12-31",

    "2012-01-01 2012-01-31", "2012-02-01 2012-02-28", "2012-03-01 2012-03-31",
    "2012-04-01 2012-04-30", "2012-05-01 2012-05-31", "2012-06-01 2012-06-30",
    "2012-07-01 2012-07-31", "2012-08-01 2012-08-31", "2012-09-01 2012-09-30",
    "2012-10-01 2012-10-31", "2012-11-01 2012-11-30", "2012-12-01 2012-12-31",

    "2013-01-01 2013-01-31", "2013-02-01 2013-02-28", "2013-03-01 2013-03-31",
    "2013-04-01 2013-04-30", "2013-05-01 2013-05-31", "2013-06-01 2013-06-30",
    "2013-07-01 2013-07-31", "2013-08-01 2013-08-31", "2013-09-01 2013-09-30",
    "2013-10-01 2013-10-31", "2013-11-01 2013-11-30", "2013-12-01 2013-12-31",

    "2014-01-01 2014-01-31", "2014-02-01 2014-02-28", "2014-03-01 2014-03-31",
    "2014-04-01 2014-04-30", "2014-05-01 2014-05-31", "2014-06-01 2014-06-30",
    "2014-07-01 2014-07-31", "2014-08-01 2014-08-31", "2014-09-01 2014-09-30",
    "2014-10-01 2014-10-31", "2014-11-01 2014-11-30", "2014-12-01 2014-12-31",

    "2015-01-01 2015-01-31", "2015-02-01 2015-02-28", "2015-03-01 2015-03-31",
    "2015-04-01 2015-04-30", "2015-05-01 2015-05-31", "2015-06-01 2015-06-30",
    "2015-07-01 2015-07-31", "2015-08-01 2015-08-31", "2015-09-01 2015-09-30",
    "2015-10-01 2015-10-31", "2015-11-01 2015-11-30", "2015-12-01 2015-12-31",

                  "2016-01-01 2016-01-31", "2016-02-01 2016-02-28", "2016-03-01 2016-03-31",
                  "2016-04-01 2016-04-30", "2016-05-01 2016-05-31", "2016-06-01 2016-06-30",
                  "2016-07-01 2016-07-31", "2016-08-01 2016-08-31", "2016-09-01 2016-09-30",
                  "2016-10-01 2016-10-31", "2016-11-01 2016-11-30", "2016-12-01 2016-12-31",

                  "2017-01-01 2017-01-31", "2017-02-01 2017-02-28", "2017-03-01 2017-03-31",
                  "2017-04-01 2017-04-30", "2017-05-01 2017-05-31", "2017-06-01 2017-06-30",
                  "2017-07-01 2017-07-31", "2017-08-01 2017-08-31", "2017-09-01 2017-09-30",
                  "2017-10-01 2017-10-31", "2017-11-01 2017-11-30", "2017-12-01 2017-12-31",

                  "2018-01-01 2018-01-31", "2018-02-01 2018-02-28", "2018-03-01 2018-03-31",
                  "2018-04-01 2018-04-30", "2018-05-01 2018-05-31", "2018-06-01 2018-06-30",
                  "2018-07-01 2018-07-31", "2018-08-01 2018-08-31", "2018-09-01 2018-09-30",
                  "2018-10-01 2018-10-31", "2018-11-01 2018-11-30", "2018-12-01 2018-12-31",

                  "2019-01-01 2019-01-31", "2019-02-01 2019-02-28", "2019-03-01 2019-03-31",
                  "2019-04-01 2019-04-30", "2019-05-01 2019-05-31", "2019-06-01 2019-06-30"

        )

  # kw <- c("bankrupt")
  # w=1
  
  strategy <- function(data_frame, limit_a, limit_c) 
  {
    temp_df <- 
      mutate( data_frame , bb_ind = ifelse( interest <= limit_a , 1 , 
                                            ifelse( interest > limit_c , -1 , NA )
      )
      )
    if (is.na( temp_df[ 1 , "bb_ind" ] )) { temp_df[ 1 , "bb_ind" ] <- 0  }
    temp_df$bb_ind <- na.locf( temp_df$bb_ind )
    temp_df <- mutate( temp_df , y_sen = y_lead * bb_ind )
    sum_y_sen <- sum( temp_df$y_sen)
    return( sum_y_sen )
  }
  
  
  
  
  key_word_loop <- function(kw) 
  {
    for (w in 1:length(kw) ) 
    {
      ### 4.  ###   
      ### 4a. MONTHLY INTERESTS FOR WHOLE INTERVAL ###
      ### 4b. DAILY INTERESTS WITHIN MONTHS ###    
      ### 4c. COMBINING & WEIGHING THE 2 TABLES ###  
      
      print(kw[w])
      
      for (k in 1:2) {
          if (k==1) {
            t = train_dates
            print("TRAIN SET")
          } else {
            t = test_dates        #has to contain the interval of the train dates as well for scaling purposes!
            print("TEST SET")
          }
        
        
        first_month <- t[1] %>% substr(1, 10)
        last_month <- t[length(t)] %>% substr(12, 21)
        gt_monthly <- gtrendsR::gtrends(keyword = kw[w], geo = "US", time = paste(first_month, last_month, sep=" "), 
                              low_search_volume = FALSE)[1] %>% data.frame(stringsAsFactors=TRUE)
        gt_monthly <- gt_monthly %>% select(1:2)
        names(gt_monthly) <- c("time","monthly_val")
        print("monthly data downloaded")
        
    
        gt_daily <- gtrendsR::gtrends(keyword = kw[w], geo = "US", time = t[1], low_search_volume = FALSE)[1] %>% 
                    data.frame(stringsAsFactors=TRUE)
        #print(t[1])
        for (i in 2:length(t)) {
          #print(t[i])
          df <- gtrendsR::gtrends(keyword = kw[w], geo = "US", time = t[i], low_search_volume = FALSE)[1] %>% 
                data.frame(stringsAsFactors=TRUE)
          gt_daily <- rbind(gt_daily, df)
        }
        names(gt_daily) <- c("time","hit","geo","interval","keyword","gprop","category")
        print("daily data")
    
      
        gt_df <- merge(gt_daily, gt_monthly, by="time", all.x = TRUE) %>% fill(monthly_val) %>% 
                mutate(interest = hit * monthly_val / 100) %>% select(time,interest)
        gt_df$time <- as.Date(gt_df$time)
        print("monthly and daily data merged")
        
          if (k==1) {
            gt_train <- gt_df
            print("train data completed")
          } else {
            gt_test <- filter( gt_df, time >= as.Date(test_dates[1]) )
            print("test data completed")
          }
      }
    
      gt_train$interest <- na.approx(gt_train$interest)
      gt_test$interest <- na.approx(gt_test$interest)
    
      
    ### 6. DOW JONES INDEX - RETURNS ### 
      
      ticker <- "DJI"
      security <- getSymbols(Symbols = ticker, auto.assign = FALSE, from="2004-01-01", to="2019-06-30",
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
      
      
    ### 7. MERGE GOOGLETRENDS & RETURNS
    
      train_df <- merge(train_returns, gt_train, by="time", all.x = TRUE)
      test_df <- merge(test_returns, gt_test, by="time", all.x = TRUE)
      
      train_df$interest <- na.approx(train_df$interest, na.rm=FALSE)
      test_df$interest <- na.approx(test_df$interest, na.rm=FALSE)
      
    
    
    ### 8. FINDING THE OPTIMAL LIMITS IN TRAIN
      

      
      dim <- 0.1
      result_df <- data.frame(matrix(NA, nrow = 100, ncol = 100))
      
      for ( i in 1:(100) )
      {
        for ( j in 1:(100) )
        {
          if ( i > j ) 
          { result_df[ i , j ] <- NA }
          else
          { result_df[ i , j ] <- strategy( train_df , i , j ) }
           
        }
      }
      
    
      result_m <- mutate(result_df, limit_a = rownames(result_df))
      result_m <- melt(result_m, na.rm = FALSE)
        names(result_m) <- c("limit_a","limit_c", "value")
        #result_m[] <- lapply(result_m, gsub, pattern='X', replacement='')
        result_m$limit_c <- as.numeric(result_m$limit_c)
        result_m$limit_a <- as.numeric(result_m$limit_a)
        
        
      # ggplot(result_m, aes(limit_a, limit_c, fill= value)) + 
      #   geom_tile() + 
      #   scale_fill_gradient(low = "white", high = "red")
      # plot_ly(x = result_m$limit_a, y = result_m$limit_c, z = result_m$value, 
      #         type = 'surface')
      
    
      limit_a <- filter(result_m, value == max(value, na.rm=TRUE))[1] %>% as.numeric()
      limit_c <- filter(result_m, value == max(value, na.rm=TRUE))[2] %>% as.numeric()
      diag <- filter(result_m, limit_a == limit_c)
      limit <-  filter(diag, value == max(value))[1] %>% as.numeric()
      
      
      buy_n_hold <- sum(test_df$y_lead)
      strat_1 <- strategy(test_df, limit, limit)
      strat_2 <- strategy(test_df, limit_a, limit_c)
      strat_3 <- strategy(test_df, 31, 65)
      
      print(buy_n_hold)
      print(strat_1)
      print(strat_2)
      print(strat_3)
      
      
      print(result_m)
      print(test_df)


    }
    return(0)
  }
  
  
  key_words <- c("debt", "crisis", "decline", "recession", "unemployment", "bankrupt", "deficit", 
                 "banana", "cinderella")
  key_words <- c("crisis")
  
  key_word_loop(key_words)
  
  
  
  
  
  
              # train_nn <- mutate(train_df, ind = ifelse( y_lead > 0 , 1 , -1 ))
              # train_nn$ind <- factor(train_nn$ind)
              # 
              # 
              # strat_logit <- glm(ind ~ interest, data = train_nn, family = "binomial")
              # l <- predict(strat_logit, newdata = test_df[,-2], type = "response")
              # logit_results <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T))
              # names(logit_results) <- c("predictition")
              # logit_results <- cbind(test_df,logit_results)
              # filter(logit_results, predictition < 0.5)
              # 
              # 
              # strat_nn <- neuralnet(
              #               ind ~ interest,
              #               data = train_nn,
              #               hidden = 1,
              #               err.fct = "ce",
              #               linear.output = FALSE,
              #               rep = 1
              #             )
              # plot(strat_nn)
              # 
              # result_nn <- neuralnet::compute( strat_nn, test_df[,-2] )
              
  
  
  
  
  

  
  
  
### 9. STRATEGIES IN TEST  
  
  final_df$bb_ind_lag <- na.approx(final_df$bb_ind_lag)
  final_df$interest <- na.approx(final_df$interest)
  final_df <- mutate(final_df, 
                     y_sen_1 = bb_ind_lag * y, 
                     y_sen_2 = bb_ind_lag * y * (1+abs(interest-separator)/separator) )
  final_df <- mutate(final_df, 
                     tr = ifelse( (bb_ind_lag - lag(bb_ind_lag, default = first(bb_ind_lag))) != 0, 1, 0)
                     )
  
  buy_n_hold <- sum(final_df$y)
  strat_1 <- sum(final_df$y_sen_1)
  strat_2 <- sum(final_df$y_sen_2)
  list("buy_n_hold" = paste( round(buy_n_hold*100,2) , "%" , sep=""), 
       "strat_1" = paste( round(strat_1*100,2) , "%" , sep=""), 
       "strat_2" =   paste( round(strat_2*100,2) , "%" , sep=""))

  
  
  
  
  
  
  
  
  
  
  
  
### 8. CORRELATION ###  
  
  ggplot(final_df, aes(x=time))  +
    geom_line(aes(y=interest), size=0.7, col="red", alpha=0.7 ) +
    geom_hline(yintercept=separator, colour="black", size=0.7) +
    geom_text(aes(x=as.Date("2004-01-20"), y=separator, vjust=-0.5, hjust=0.7, label=separator), size=2.5, col="black") +
    scale_x_date(name="date", date_breaks = "1 year", date_labels = paste("'", "%y",sep="")) +
    theme(
      panel.background = element_rect(fill = "grey85", colour = "gray21"),
      panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "grey75"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey85")
    ) +
    scale_colour_brewer(palette = "Set1") 


  ggplot(returns, aes(x=time))  +
    geom_line(aes(y=dji_close), size=0.7, col="blue", alpha=0.7 ) +
    scale_y_continuous(name="DJI adjusted Close Price") +
    scale_x_date(name="date", date_breaks = "1 year", date_labels = paste("'", "%y",sep="")) +
    theme(
      panel.background = element_rect(fill = "grey85", colour = "gray21"),
      panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "grey75"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey85")
    ) +
    scale_colour_brewer(palette = "Set1") 
  
  
    corr_df <- final_df %>% select(time,y,interest)
    corr_df_2 <- corr_df[-1, ]
    corr_df_2 <- cbind(corr_df_2, diff_interest=diff(corr_df$interest))
    ccf(corr_df_2$y, corr_df_2$diff_interest, lag.max = 50, type = "correlation", plot = TRUE)
  
  
  
### 9. PLOT ###   
  
  sum_tr <- sum(final_df$tr=="Y")
  capital=30000
  tr_cost=5
  
  capital_1 <- (tr_cost*sum_tr)/(1- buy_n_hold/strat_1)
  capital_2 <- (tr_cost*sum_tr)/(1- buy_n_hold/strat_2)
  
  
  cost <- data.frame(capital = numeric(0), net_y_strat_1 = numeric(0), net_y_strat_2 = numeric(0))
  for ( i in 1:(capital/1000) ) {
    cost[i,1] <- i*1000
    full_tr_cost <- (tr_cost*sum_tr)/(i*1000)
    cost[i,2] <- strat_1*(1-full_tr_cost)
    cost[i,3] <- strat_2*(1-full_tr_cost)
  }
  
  ggplot(cost, aes(x=capital))  +
    geom_line(aes(y=net_y_strat_1, col="Strategy 1"), size=1.2) +
    geom_line(aes(y=net_y_strat_2, col="Strategy 2"), size=1.2) +
    geom_hline(yintercept=buy_n_hold, colour="green1", size=1.2) +
    scale_y_continuous(name="net yield", limits=c(-1,1.5), breaks = seq(-1, 1.5, by = 0.5), labels = scales::percent) +
    geom_text(aes(0, buy_n_hold, label = scales::percent(buy_n_hold), vjust=-0.5), size=2.5, col="black") +
    theme(
      panel.background = element_rect(fill = "grey85", colour = "gray21"),
      panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "grey75"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey75")
      ) +
    scale_colour_brewer(palette = "Set1") +
    labs(color='')

