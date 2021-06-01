
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

return_df <- list()
funds <- c('VNQ', 'XLK', 'XLB', 'XLV', 'XLI', 'XLP', 'XLE', 'XLY', 'VOX', 'XLU', 'XLF')
funds <- c( 'XLF')
training_from <- 1


for (fund_num in 1:11)  {
  
for (sd_num in 1:2) { #sd_num=1 overall;    sd_num=2 individual

  sd_num<-2
  if (sd_num==1) { 
    
  st_err <- c(
    3.76,
    3.76,
    
    3.76,
    3.76,
    
    3.76,
    3.76,
    
    3.76,
    3.76
    )
  
  } else {
    
st_err <- c(
      1.9 ,
      6.13 ,
      
      2.61 ,
      1.33 ,
      
      2.81 ,
      1.34 ,
      
      4.03 ,
      0.83 
  
    )
    
  }
  
  neg <- c(
                -1 ,
                -1 ,
                
                -1 ,
                -1 ,
                
                1 ,
                1 ,
                
                1 ,
                1 
                )

  
   key_words <- c(
                       'boost' ,
                       'consolidate' ,
                       
                       'banana' ,
                       'phone' ,
                       
                       'debt' ,
                       'collapse' ,
                       
                       'bycicle' ,
                       'chicago' 
                )
  
   kw <- key_words  
  


strategy <- function(data_frame, limit_a, limit_c) 
{ 
  temp_df <- data_frame
  temp_df <- mutate( temp_df , ctrl = ifelse( abs(interest-lag(interest)) > st_err[w] , 1 , 0) )  #st_err or st_err[w]!!!
  temp_df <- mutate( temp_df , ctrl_dt = ifelse( interest <= limit_a & lag(interest) <= limit_a  , 1 , 
                                                ifelse( interest > limit_c & lag(interest) > limit_c , 1 , 0 ) ) )
  temp_df <- mutate( temp_df , bb_ind = ifelse( interest <= limit_a & ctrl == 1  , neg[w] , 
                                                ifelse( interest > limit_c & ctrl == 1 , -neg[w] , NA ) ) )
  temp_df <- mutate( temp_df , bb_ind_dt = ifelse( is.na(bb_ind) & ctrl_dt == 0, 0, bb_ind ) )
  
  if (is.na( temp_df[ 1 , "ctrl" ] )) { temp_df[ 1 , "ctrl" ] <- 0  }
  if (is.na( temp_df[ 1 , "ctrl_dt" ] )) { temp_df[ 1 , "ctrl_dt" ] <- 0  }
  if (is.na( temp_df[ 1 , "bb_ind" ] )) { temp_df[ 1 , "bb_ind" ] <- 0  }
  if (is.na( temp_df[ 1 , "bb_ind_dt" ] )) { temp_df[ 1 , "bb_ind_dt" ] <- 0  }
  temp_df$bb_ind <- na.locf( temp_df$bb_ind )
  temp_df$bb_ind_dt <- na.locf( temp_df$bb_ind_dt )
  
  temp_df <- mutate( temp_df, tr = ifelse( (bb_ind - lag(bb_ind, default = first(bb_ind))) != 0 & 
                                             interest != 0, 2, 0) )
  temp_df <- mutate( temp_df, tr_dt = ifelse( (bb_ind_dt - lag(bb_ind_dt, default = first(bb_ind_dt))) == 0 |
                                                bb_ind_dt == 0 | interest == 0, 0, 2) )
  temp_df <- mutate( temp_df , y_sen = ifelse( interest == 0, 0, y_lead * bb_ind ) ) #if any 0s remain which should not
  temp_df <- mutate( temp_df , y_sen_dt = ifelse( interest == 0, 0, y_lead * bb_ind_dt ) )  #if any 0s remain which should not
  
  sum_y_sen <- sum( temp_df$y_sen )
  count_tr <- sum( temp_df$tr )
  sum_y_sen_dt <- sum( temp_df$y_sen_dt )
  count_tr_dt <- sum( temp_df$tr_dt )
  results <- c(sum_y_sen, count_tr, sum_y_sen_dt, count_tr_dt)
  return( results )
} 

sec_file_name <- paste( funds[fund_num], ".csv", sep="")
security <- read.csv(file= sec_file_name, header=TRUE, sep=",") %>% select(Date, Adj.Close)
rownames(security) = security[[1]]
security <- select(security, Adj.Close)
security <- as.xts(security)
returns <- Return.calculate(security, method = "log")
returns <- returns[-1, ]              #remove first row
returns <- data.frame(time = as.Date(index(returns)), coredata(returns))  %>% select(time, Adj.Close)
returns <- mutate(returns, Adj.Close_lead = lead(Adj.Close, 3)) #3-as lead
returns <- returns[-( nrow(returns):(nrow(returns)-2 )) ,]   #remove last 3 rows
returns <- returns %>% select(time, Adj.Close_lead)  
if (training_from==2) {
  returns <- returns %>% filter(time >= "2008-08-06")
} 

names(returns) <- c("time","y_lead")
return_df[fund_num] <- returns
print('returns are ready')
hist(returns$y_lead, breaks=500)




summary_df <- data.frame(matrix(NA, nrow = 6, ncol = 41))
names(summary_df) <- key_words

########################################################################################################


for ( w in 1:length(kw) ) {
  
  
  df <- read.csv(file= paste("C:/Users/hunter5/Desktop/final_df/", as.character( kw[w] ), "_final.csv", sep="") , 
                header=TRUE, sep=",")
  df$time <- as.Date(df$time)
  years <- list()
  year_names <- list()
  final_df <- data.frame(matrix(NA, nrow = 6, ncol = 10))

  for (h in 12:2) {
      temp_df <- df %>% select(1, h) %>% na.omit
      year_names[[h-1]] <- names(df[h])
        last_row_num <- nrow(temp_df)
        last_date <- temp_df$time[last_row_num]
      temp_returns <- filter( returns, time <= last_date )
      temp_df <- merge(temp_returns, temp_df, by="time", all.x = TRUE)
      names(temp_df) <- c("time","y_lead", "interest")
      temp_df <- mutate( temp_df , interest = ifelse(interest == 0, NA, interest) ) #dealing with the 0 interests
      temp_df$interest <- na.approx(temp_df$interest, na.rm = FALSE) #na.rm=FALSE for not removing NA-s in the beginning
      temp_df$interest <- na.locf(temp_df$interest, fromLast = TRUE, na.rm = FALSE) #fill backward NA-s in the beginning
      temp_df$interest <- na.locf(temp_df$interest, fromLast = FALSE, na.rm = FALSE) #fill forward NA-s in the end
      years[[h-1]] <- temp_df
  }

  for (k in 11:2) {
    
  if (training_from==1) {
    train_df <- years[[k]] 
  } else {
    train_df <- years[[k]] %>% filter(time >= "2008-08-06")
  }
    
      last_row_num <- nrow(train_df)
      last_date <- train_df$time[last_row_num]
    test_df <- years[[k-1]] %>% filter(time >= last_date)
    
#strat_1
if (1==1) {
    result_df <- data.frame(matrix(NA, nrow = 100, ncol = 100))
    
    for ( i in 1:(100) )
    {
      for ( j in 1:(100) )
      {
        if ( i > j ) 
        { result_df[ i , j ] <- NA }
        else
        { result_df[ i , j ] <- strategy( train_df , i , j )[1] }
        
      }
    }

    
    result_m <- mutate(result_df, limit_a_1 = rownames(result_df))
    result_m <- melt(result_m, na.rm = FALSE)
    names(result_m) <- c("limit_a_1","limit_c_1", "value")
    #result_m[] <- lapply(result_m, gsub, pattern='X', replacement='')

    result_m$limit_c_1 <- as.numeric(result_m$limit_c_1)
    result_m$limit_a_1 <- as.numeric(result_m$limit_a_1)
    limit_a_1 <- min( filter(result_m, value == max(value, na.rm=TRUE))[1] ) %>% as.numeric()
    limit_c_1 <- max( filter(result_m, value == max(value, na.rm=TRUE))[2] ) %>% as.numeric()
    
    #diag <- filter(result_m, limit_a_1 == limit_c_1)
    #limit <-  max( filter(diag, value == max(value))[1] ) %>% as.numeric()
} 

    if (1==0) {
#strat_2
    result_df <- data.frame(matrix(NA, nrow = 100, ncol = 100))

    for ( i in 1:(100) )
    {
      for ( j in 1:(100) )
      {
        if ( i > j )
        { result_df[ i , j ] <- NA }
        else
        { result_df[ i , j ] <- ifelse( strategy( train_df , i , j )[2] == 0, 0,
                                        strategy( train_df , i , j )[1] / strategy( train_df , i , j )[2] ) }

      }
    }

    result_m <- mutate(result_df, limit_a_2 = rownames(result_df))
    result_m <- melt(result_m, na.rm = FALSE)
    names(result_m) <- c("limit_a_2","limit_c_2", "value")
    #result_m[] <- lapply(result_m, gsub, pattern='X', replacement='')

    result_m$limit_c_2 <- as.numeric(result_m$limit_c_2)
    result_m$limit_a_2 <- as.numeric(result_m$limit_a_2)
    limit_a_2 <- min( filter(result_m, value == max(value, na.rm=TRUE))[1] ) %>% as.numeric()
    limit_c_2 <- max( filter(result_m, value == max(value, na.rm=TRUE))[2] ) %>% as.numeric()
}
if (1==1) {
  #strat_3
    result_df <- data.frame(matrix(NA, nrow = 100, ncol = 100))

    for ( i in 1:(100) )
    {
      for ( j in 1:(100) )
      {
        if ( i > j )
        { result_df[ i , j ] <- NA }
        else
        { result_df[ i , j ] <- strategy( train_df , i , j )[3] }

      }
    }

    result_m <- mutate(result_df, limit_a_3 = rownames(result_df))
    result_m <- melt(result_m, na.rm = FALSE)
    names(result_m) <- c("limit_a_3","limit_c_3", "value")
    #result_m[] <- lapply(result_m, gsub, pattern='X', replacement='')

    result_m$limit_c_3 <- as.numeric(result_m$limit_c_3)
    result_m$limit_a_3 <- as.numeric(result_m$limit_a_3)
    limit_a_3 <- min( filter(result_m, value == max(value, na.rm=TRUE))[1] ) %>% as.numeric()
    limit_c_3 <- max( filter(result_m, value == max(value, na.rm=TRUE))[2] ) %>% as.numeric()
}  
    if (1==0) {
#strat_4
    result_df <- data.frame(matrix(NA, nrow = 100, ncol = 100))

    for ( i in 1:(100) )
    {
      for ( j in 1:(100) )
      {
        if ( i > j )
        { result_df[ i , j ] <- NA }
        else
        { result_df[ i , j ] <- ifelse( strategy( train_df , i , j )[4] == 0, 0,
                                        strategy( train_df , i , j )[3] / strategy( train_df , i , j )[4] ) }

      }
    }

    result_m <- mutate(result_df, limit_a_4 = rownames(result_df))
    result_m <- melt(result_m, na.rm = FALSE)
    names(result_m) <- c("limit_a_4","limit_c_4", "value")
    #result_m[] <- lapply(result_m, gsub, pattern='X', replacement='')

    result_m$limit_c_4 <- as.numeric(result_m$limit_c_4)
    result_m$limit_a_4 <- as.numeric(result_m$limit_a_4)
    limit_a_4 <- min( filter(result_m, value == max(value, na.rm=TRUE))[1] ) %>% as.numeric()
    limit_c_4 <- max( filter(result_m, value == max(value, na.rm=TRUE))[2] ) %>% as.numeric()
}   
    
    
    buy_n_hold <- sum(test_df[-1,2])
    strat_0 <- strategy(test_df, 30, 70)[1]               #fix limits
    strat_1 <- strategy(test_df, limit_a_1, limit_c_1)[1] #maximizing yield - original
    #strat_2 <- strategy(test_df, limit_a_2, limit_c_2)[1] #maximizing yield while minimizing tr - original
    strat_2 <- 0
    strat_3 <- strategy(test_df, limit_a_3, limit_c_3)[1] #maximizing yield - DT
    #strat_4 <- strategy(test_df, limit_a_4, limit_c_4)[1] #maximizing yield while minimizing tr - DT
    strat_4 <- 0
    print(year_names[[k-1]])
    names(final_df[12-k]) <- year_names[[k-1]]
    final_df[12-k] <- c(buy_n_hold, strat_0, strat_1, strat_2, strat_3, strat_4) #we could save the limits as well

  }
  
  file_name <- paste(as.character( kw[w] ), "FINAL_RESULTS.csv", sep="_")
  #write.csv(final_df, file = file_name, row.names=FALSE)
  sum_buy_n_hold <- sum(final_df[1,])
  sum_strat_0 <- sum(final_df[2,])
  sum_strat_1 <- sum(final_df[3,]) 
  sum_strat_2 <- sum(final_df[4,]) 
  sum_strat_3 <- sum(final_df[5,])
  sum_strat_4 <- sum(final_df[6,])
  print(kw[w])
  print(paste("buy_n_hold: ", sum_buy_n_hold , sep="") )
  print(paste("strat_0: ", sum_strat_0 , sep="") )
  print(paste("strat_1: ", sum_strat_1 , sep="") )
  print(paste("strat_2: ", sum_strat_2 , sep="") )
  print(paste("strat_3: ", sum_strat_3 , sep="") )
  print(paste("strat_4: ", sum_strat_4 , sep="") )
  print("pasting results into summary_df")
  print( st_err[w] )
  summary_df[w] <- c(sum_buy_n_hold, sum_strat_0, sum_strat_1, sum_strat_2, sum_strat_3, sum_strat_4)
  print(summary_df)
}

file_name <- paste( as.character(sd_num), funds[fund_num], "SUMMARY_DF_FINAL.csv", sep="_")
write.csv(summary_df, file = file_name, row.names=FALSE)
print(funds[fund_num])
print(sd_num)
print("summary_df is ready")


}
  
}
