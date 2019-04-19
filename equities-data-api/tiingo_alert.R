library(stringr)
library(quantmod)

# Remove all variables in workspace
rm(list=ls())

# Set Data Directory
data_dir <- "D:\\equities\\"
setwd(data_dir)

# Function to import tiingo price data from a specific date
getdata_tiingo <- function(x){
  setDefaults(getSymbols.tiingo, api.key=" ")
  getSymbols(x,src="tiingo",from ="2015-01-01", to = Sys.Date(),auto.assign=F, adjust = TRUE)
}

# Process dataframe
process_tiingo <- function(x){
  y <- na.locf(x) 
  y <- aggregate(y,time(y),mean)
  y <- y[,c(4,2,3,1,5)]
  name <- substr(colnames(y)[2],1,str_length(colnames(y)[2])-5)
  colnames(y) <- c(paste0(name,".Open"),paste0(name,".High"),paste0(name,".Low"),paste0(name,".Close"),paste0(name,".Volume"))
  return(y)    
}

# Execution: export adjusted price data and volume on selected ETFs
ETF_table <- read.csv(file="ZZ_ETF.csv",header=T,sep=",",stringsAsFactors=F)
ETF_list <- ETF_table$Symbol
ETF_data <- lapply(ETF_list,getdata_tiingo)
ETF_adjdata <- lapply(ETF_data,process_tiingo)

# Execution: export adjusted price data and volume on S&P 500 constituents
chips_table <- read.csv(file="ZZ_chips.csv",header=T,sep=",",stringsAsFactors=F)
chips_list <- chips_table$Symbol
chips_sector <- chips_table$Sector
chips_name <- chips_table$Security
chips_industry <- chips_table$GICS.Sub.Industry
chips_data <- lapply(chips_list,getdata_tiingo)
chips_adjdata <- lapply(chips_data,process_tiingo)

# Calculate quantile statistics
TA_recap <-function(symbolist,assetclass){
  # Empty columns for TA stats
  Close_LTD <- c()
  Turnover_LTD <- c()
  TrendMA_LTD <- c()
  YearMA_LTD <- c()
  RSI_LTDrank <- c()
  CCI_LTDrank <- c()
  ATRpct_LTD <- c()
  ATRpct_LTDrank <- c()
  Numfromhigh_LTD <- c()
  MACDATR_LTD <- c()
  MACDhist_LTD <- c()
  MACDtrend_LTD <- c()

  
  for (j in 1:length(symbolist)) #
  {
    if (assetclass=='US'){
      price_df <- chips_adjdata[[j]]
    } else if (assetclass=='ETF'){
      price_df <- ETF_adjdata[[j]]
    } else {
      price_df <- hk_adjdata[[j]]
    }
    # Parameters for fast & slow periods
    fast_len <- min(length(price_df[,4])-1,8)
    slow_len <- min(length(price_df[,4])-1,21)
    trend_len <- min(length(price_df[,4])-1,64)
    year_len <- min(length(price_df[,4])-1,256)
    rank_len <- min(length(price_df[,4])-1,1024)
    
    # Volume Turnover
    Close_val <- tail(price_df[,4],1)
    Turnover_val <- mean(tail(price_df[,5]*price_df[,4],fast_len))
    
    # Trend Moving Averages
    TrendMA_val <- tail(SMA(price_df[,4],trend_len),1)
    YearMA_val <- tail(SMA(price_df[,4],year_len),1)
    
    # RSI value & percentiles
    RSI_val <- as.numeric(tail(RSI(price_df[,4],n=fast_len),rank_len))
    RSI_rank <- 100 * tail(rank(RSI_val),1)/rank_len
    
    # CCI value & percentiles
    CCI_val <- as.numeric(tail(CCI(price_df[,4],n=slow_len),rank_len))
    CCI_rank <- 100 * tail(rank(CCI_val),1)/rank_len
  
    # Average True Range (ATR) of LTD
    ATRpct_val <- as.numeric(100*tail(ATR(price_df[,2:4],n=slow_len)$atr/SMA(price_df[,4],slow_len),rank_len))
    ATRpct_LTDval <- tail(ATRpct_val,1)
    ATRpct_rank <- 100 * tail(rank(ATRpct_val),1)/rank_len
    
    # NACD & Signal
    MACDATR_val <- tail(MACD(price_df[,4],nFast=fast_len,nSlow=slow_len,nSig=fast_len,
                          maType="SMA",percent=TRUE)$macd,1)/ATRpct_LTDval 
    MACDATR_sig <- tail(MACD(price_df[,4],nFast=fast_len,nSlow=slow_len,nSig=fast_len,
                          maType="SMA",percent=TRUE)$signal,1)/ATRpct_LTDval 
    MACDATR_hist <- MACDATR_val - MACDATR_sig
    MACDtrend_val <- tail(MACD(price_df[,4],nFast=slow_len,nSlow=trend_len,nSig=fast_len,
                               maType="SMA",percent=TRUE)$macd,1)/ATRpct_LTDval 
    
    
    # Number of ATRs from 52 week high
    yearhigh <- max(tail(lag(price_df[,2],-1),year_len))
    Numfromhigh_val <- 100 * (yearhigh / (tail(price_df[,4],1))-1) / ATRpct_LTDval
    
    # Attach the individual statistics to the stat columns
    Close_LTD <- c(Close_LTD,Close_val)
    Turnover_LTD <- c(Turnover_LTD,Turnover_val)
    TrendMA_LTD <- c(TrendMA_LTD,TrendMA_val)
    YearMA_LTD <- c(YearMA_LTD,YearMA_val)
    RSI_LTDrank <- c(RSI_LTDrank,RSI_rank)
    CCI_LTDrank <- c(CCI_LTDrank,CCI_rank)
    ATRpct_LTD <- c(ATRpct_LTD,ATRpct_LTDval)
    ATRpct_LTDrank <- c(ATRpct_LTDrank,ATRpct_rank)
    Numfromhigh_LTD <- c(Numfromhigh_LTD,Numfromhigh_val)
    MACDATR_LTD <- c(MACDATR_LTD,MACDATR_val)
    MACDhist_LTD <- c(MACDhist_LTD,MACDATR_hist)
    MACDtrend_LTD <- c(MACDtrend_LTD,MACDtrend_val)
  }

  
  # Summary Dataframe
  TA_recapdf <- cbind(Close_LTD,Turnover_LTD,
                      TrendMA_LTD,YearMA_LTD,
                      RSI_LTDrank,CCI_LTDrank,
                      ATRpct_LTD,ATRpct_LTDrank,Numfromhigh_LTD,
                      MACDATR_LTD,MACDhist_LTD,MACDtrend_LTD)
  
  TA_recapdf <- data.frame(TA_recapdf)
  
  colnames(TA_recapdf) <- c("Close","Turnover",
                            "TrendMA","YearMA",
                            "RSI_rank","CCI_rank",
                            "ATRpct","ATRpct_rank","Numfrom52WH",
                            "MACDATR","MACDhist","MACDtrend")
  return(TA_recapdf)
}

# Execute TA statistics calculation on S&P 500, ETF and HK and Export
TA_recapdf_ETF <- TA_recap(ETF_list,'ETF')
TA_recapdf_ETF <- cbind(ETF_list,TA_recapdf_ETF)
write.csv(TA_recapdf_ETF,file = paste0("TAstats_ETF_",Sys.Date(),".csv"),row.names=F)

TA_recapdf_chips <- TA_recap(chips_list,'US')
TA_recapdf_chips <- cbind(chips_list,chips_name,chips_sector,chips_industry,TA_recapdf_chips)
TA_recapdf_chips <- TA_recapdf_chips[order(-TA_recapdf_chips$Numfrom52WH),]
write.csv(TA_recapdf_chips,file = paste0("TAstats_US_",Sys.Date(),".csv"),row.names=F)  


