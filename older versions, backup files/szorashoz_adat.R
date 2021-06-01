
#install.packages("tidyverse")
#install.packages("lattice")
#install.packages("tseries")
#install.packages("gtrendsR")
#install.packages("dplyr")

library(tidyverse)
library(lattice)
library(tseries)
library(gtrendsR)
library(dplyr)




    
for ( x in 1:20 ) {
      
  
        test_dates = c(
          
          "2011-02-01 2011-02-28",
          "2012-04-01 2012-04-30",
          "2009-03-01 2009-03-31", 
          "2007-09-01 2007-09-30",
          "2014-06-01 2014-06-30",
          
          "2010-12-01 2010-12-31", #itt volt gond
          "2016-10-01 2016-10-31", 
          "2017-01-01 2017-01-31", #itt volt gond
          "2018-08-01 2018-08-31", 
          "2019-10-01 2019-10-31",
          
          "2017-09-01 2017-09-30",
          "2015-05-01 2015-05-31",
          "2013-11-01 2013-11-30"
          
        )
        
        
        
        key_words <- c("reforms", 
                       "boost", 
                       "consolidate", 
                       "construction", 
                       "stabilized",
                       
                       "liberalize", 
                       "modernization", 
                       "outperform", 
                       "savings", 
                       "progress",
                       
                       "booming",
                       "accrue",
                       "surpassed"
        )
        
        
        kw=key_words
        
        t = test_dates  
      
      gt_daily <- gtrendsR::gtrends(keyword = kw[1], geo = "US", time = t[1], low_search_volume = FALSE)[1] %>% 
        data.frame(stringsAsFactors=TRUE)
      print(kw[1])
      
      for (i in 2:length(kw)) {
        df <- gtrendsR::gtrends(keyword = kw[i], geo = "US", time = t[i], low_search_volume = FALSE)[1] %>% 
          data.frame(stringsAsFactors=TRUE)
        print(kw[i])
        gt_daily <- rbind(gt_daily, df)
      }
      
      gt_daily <- gt_daily %>% select(1:2)
      names(gt_daily) <- c("time","hit")
      
      file_name <- paste(as.character(x), "_positive.csv", sep="")
      write.csv(gt_daily, file = file_name, row.names=FALSE)
      
      print(x)
      print("daily is ready")
      
      
      

      
      
        test_dates = c(
          "2004-01-01 2019-12-31"
        )
        
        key_words <- c("reforms", 
                       "boost", 
                       "consolidate", 
                       "construction", 
                       "stabilized",
                       
                       "liberalize", 
                       "modernization", 
                       "outperform", 
                       "savings", 
                       "progress",
                       
                       "booming",
                       "accrue",
                       "surpassed"
        )
        
        kw <- key_words
        
        t = test_dates  
      
        
        print(x)
        print(kw[1])
        gt_monthly <- gtrendsR::gtrends(keyword = kw[1], geo = "US", time = t[1], low_search_volume = FALSE)[1] %>% 
          data.frame(stringsAsFactors=TRUE)
        gt_monthly <- gt_monthly %>% select(1:2)
        names(gt_monthly) <- c("time","debt")
        for ( w in 2:length(kw) ) {
          print(kw[w])
          df <- gtrendsR::gtrends(keyword = kw[w], geo = "US", time = t[1], low_search_volume = FALSE)[1] %>% 
            data.frame(stringsAsFactors=TRUE)
          df <- df %>% select(1:2)
          names(df) <- c("time", as.character(kw[w]) )
          gt_monthly <- merge(gt_monthly, df, by="time", all.x = TRUE)
        }
        
        
        file_name <- paste(as.character(x), "_monthly_positive.csv", sep="")
        write.csv(gt_monthly, file = file_name, row.names=FALSE)
        
        print("monthly is ready" )
      
      
      

      print( Sys.time() )

      
      
      
Sys.sleep(3800)

}



