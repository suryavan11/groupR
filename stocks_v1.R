###### Libraries #########################################
library(quantmod)
library(TTR)
library(QuantTools)
library(Quandl)
library(readr)
library(forecast)
library(lubridate)
library(caret)
library(ROCR)
library(MASS)
library(e1071)
library(TSdist)
library(parallelDist)
library(stringr)
library(ggrepel)
library(gridExtra)
library(httr)
library(RJSONIO)
#library(openxlsx)
library(googledrive)
library(Boruta)
### library(devtools)
### install_github("krose/robinhoodr")  ## https://rdrr.io/github/krose/robinhoodr/f/README.md
## library(robinhoodr)  ## good for future models, intraday etc, but doesnt have authentication
### https://api.robinhood.com/quotes/historicals/?symbols=MSFT,FB,TSLA&interval=day
library(xgboost)
library(ROCR)
library(partykit)
library(rpart)
library(signal)

# # ### if anaconda is already installed, then  do the following in Rstudio in anaconda:
# devtools::install_github("rstudio/keras")
# library(keras)
# install_keras()

library(keras)

library(dplyr)

setwd('C:\\Users\\asuryav1\\OneDrive - T-Mobile USA\\personal\\stocks')
options(httr_oauth_cache=T) ## allows httr package to store authorization key on local computer
#################### functions #####################
#### gets yahoo historical data and appends latest quote for the day  
GetDataAppendLatest <- function(symbol.list) {
  
  getdatalocal <- function(symbol) {
    ticker = getSymbols(symbol,src="yahoo", auto.assign = FALSE)
    quote = getQuote(symbol,src="yahoo")
    quote1 = quote[,c('Open', 'High', 'Low', 'Last', 'Volume', 'Last')]
    colnames(quote1) = colnames(ticker)
    if(any(quote1==0) | !any(sapply(quote1,is.finite)) ) {
      print(paste0('latest quote not available for ',symbol,'. Returning only historical data...' )  )
      return(ticker)
    }
    if(!as.Date(quote[, 1]) %in% index(ticker) ) {ticker = rbind(ticker, xts(quote1, as.Date(quote[, 1]) )) }
    return(ticker)
  }
  
  ticker.data = list()
  for (i in seq_along(symbol.list)) {
    possibleError <- tryCatch({
      ticker = getdatalocal(symbol.list[i]) 
      ticker = na.omit(ticker)
      ticker = ticker['2011-01-01/']
      ticker.data[[paste0('ticker',i)]] = ticker
    }, error = function(e) {
      print(paste('warning:', e, " itr:",i))
    })
  }
  return(ticker.data)
 
}

### ensure some type of standardization etc for response and inputs
## https://beyondbacktesting.com/2017/07/09/normalization-standardization-percent-rank/
## https://pdfs.semanticscholar.org/f412/4953553981e32c39273bb2745a140311d160.pdf
## https://lilianweng.github.io/lil-log/2017/07/08/predict-stock-prices-using-RNN-part-1.html#normalization
## http://lup.lub.lu.se/luur/download?func=downloadFile&recordOId=8911069&fileOId=8911070
  
######## Features (x) 

SCHAFF <- function(Cl, nfast=23, nslow=50, nperiod=10 ){
  ## https://c.mql5.com/forextsd/forum/70/the_schaff_trend_cycle.pdf
  
  # pff = SCHAFF(Cl(ticker),23,50,10)
  #     
  #   plot.start.date = '2017-06-05'
  #   temp = data.frame(Cl(ticker)[paste0(as.Date(plot.start.date),"/"),])
  #   colnames(temp) = c('Adjusted')
  #   temp$dates = as.Date(row.names(temp))
  #   # temp$low.prob = as.numeric(pff[paste0(as.Date(plot.start.date),"/"),] ) 
  #   temp$low.prob = as.numeric(lag.xts( atan(diff.xts(EMA(Cl(ticker),10),4))  ,-4)[paste0(as.Date(plot.start.date),"/"),] ) 
  #   
  #   ggplot(temp, aes(dates,Adjusted)) +
  #     geom_line(col = 'darkgrey', lwd = 1) +
  #     geom_point(aes(col = low.prob), size= 2 ) +
  #     scale_colour_gradient2(low = "red2", mid = "darkgrey",
  #                            high = "green", midpoint = 0.5, space = "Lab",
  #                            na.value = "grey50", guide = "colourbar", aesthetics = "colour") +
  #    # scale_x_date(date_breaks = xrangetext, date_labels =  "%d %b %Y")  + ### 2 day, 1 month
  #     theme(axis.line = element_line(colour = "black"),
  #           axis.text.x=element_text(angle=60, hjust=1, size = 12),
  #           panel.grid.major = element_line(colour = "#F0F0F0",size=0.25),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_blank(),
  #           panel.background = element_blank()) 
  #   
  #   
  #    pts = 630
  #    chart_Series(tail(Cl(ticker),pts))
  #    add_TA(tail(Cl(ticker),pts), col = 'black', lwd = 2, on = 1)
  #    #add_TA(tail(EMA(Cl(ticker),10),pts) , col = 'red', lwd = 2, on = 1)
  #    add_TA(tail(lag.xts(EMA(Cl(ticker),10),-4),pts)  , col = 'orange', lwd = 2, on = 1)
  #    add_TA(tail(pff,pts), col = 'darkgrey',lwd = 2)
  #    add_TA(tail(pff*0+0.75,pts), col = 'green',lwd = 2, on = 2)
  #    add_TA(tail(pff*0+0.25,pts), col = 'red',lwd = 2, on = 2)
  #    add_TA(tail( lag.xts( atan(diff.xts(EMA(Cl(ticker),10),4))  ,-4) ,pts), col = 'darkgrey',lwd = 2)
  #    
  #   
  #  # 10,23,50 period short long
  
  xmac = EMA(Cl, nfast) - EMA(Cl, nslow)
  # xmac = MACD(Cl(ticker),nfast,nslow,9,'EMA')
  pf = stoch(xmac[,1], nperiod,nperiod,3,'EMA')$fastD ## fastK and fastD are used
  pff = stoch(pf, nperiod,nperiod,3,'EMA')$fastD
  colnames(pff) = c('schaff')
  return(pff)
}


GetFeatures <- function(ticker.data) {
  
  FeaturesDataBuilder <- function(ticker) {
    returns = ticker
    colnames(returns) = c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
    
    ### schaff (0 to 1) ##########
      ind.temp = SCHAFF(Ad(ticker),23,50,10)
      colnames(ind.temp) = paste0('schaff235010')
      returns = merge(returns, ind.temp , join = 'left')

      ind.temp = SCHAFF(Ad(ticker),7,14,5)
      colnames(ind.temp) = paste0('schaff235010')
      returns = merge(returns, ind.temp , join = 'left')
      
      ind.temp = SCHAFF(Ad(ticker),14,80,10)
      colnames(ind.temp) = paste0('schaff235010')
      returns = merge(returns, ind.temp , join = 'left')
      

      
    ### aroon (-100 to 100) ##########
      for (i in c(seq(1,30,2)  )){
        ind.temp = aroon(HLC(ticker)[,-3], i)[,3]
        colnames(ind.temp) = paste0('aroon',i)
        returns = merge(returns, ind.temp , join = 'left')
      }      
      
      
    ### rsi (0-100)  ##########
      for (i in c(seq(1,30,2)  )){
        ind.temp = RSI(Ad(ticker), n = i)
        colnames(ind.temp) = paste0('rsi',i)
        returns = merge(returns, ind.temp , join = 'left')
      }      
      
      
    ### ChaikinMoneyFlow (-1 to 1)  ##########
    for (i in c(seq(1,30,2)  )){
      ind.temp = CMF(HLC(ticker), Vo(ticker), n = i)
      colnames(ind.temp) = paste0('ChaikinMoneyFlow',i)
      returns = merge(returns, ind.temp , join = 'left')
    }  
      
    ### stoch fastK (0 to 1)  ##########
    for (i in c(seq(1,30,2)  )){
      ind.temp = stoch(HLC(ticker), nFastK=i )[,1]
      colnames(ind.temp) = paste0('fastK',i)
      returns = merge(returns, ind.temp , join = 'left')
    }  
    
    ### cmo (-100 to 100) ##########
      for (i in c(seq(1,30,2)  )){
        ind.temp = CMO(Ad(ticker),n=i)
        colnames(ind.temp) = paste0('cmo',i)
        returns = merge(returns, ind.temp , join = 'left')
      } 
      
    ### adx 4 components (0 to 100) ##########

    for (i in c(seq(3,30,2) ) ){
      ind.temp = ADX(HLC(ticker), n = i) 
      colnames(ind.temp) = paste0(c('adx.dip','adx.din','adx.dx','adx.adx'),i)
      returns = merge(returns, ind.temp , join = 'left')
    }
    

    ### mfi (0 - 100) ###############
      
      for (i in c(seq(1,30,2)  )){
        ind.temp = MFI(HLC(ticker), Vo(ticker), n = i )
        colnames(ind.temp) = paste0('mfi',i)
        returns = merge(returns, ind.temp , join = 'left')
      } 
      
    ### cci (around -100 + 100 but will vary with period, avoid period1) ##########
    for (i in c(seq(2,30,2)  )){
      ind.temp = CCI(HLC(ticker),n=i)
      colnames(ind.temp) = paste0('cci',i)
      returns = merge(returns, ind.temp , join = 'left')
    }
    
    ### tdi.tdi and tdi.ti (can be in a wide range +/-5 to +/-100 plus, has outliers) ##########
      for (i in c(seq(1,30,2)  )){
        ind.temp = TDI(Ad(ticker),n=i)
        colnames(ind.temp) = paste0(c('tdi.tdi','tdi.ti'),i)
        returns = merge(returns, ind.temp , join = 'left')
      }
      
      
    ### rate.of.change (small numbers, should be independent of price, has outliers)  ##########
      for (i in c(seq(1,30,2),60,90,150,200 )){
        ind.temp = ROC(Ad(ticker), n = i, type = 'continuous')
        colnames(ind.temp) = paste0('rate.of.change',i)
        returns = merge(returns, ind.temp , join = 'left')
      }
      
    ### momentum (roughly -20 to 20, should be independent of price, has outliers) ##########
      for (i in c(seq(1,30,2)  )){
        ind.temp = momentum(Ad(ticker), n = i)
        colnames(ind.temp) = paste0('momentum',i)
        returns = merge(returns, ind.temp , join = 'left')
      }
      
    ### trix (both move around -0.02 to 0.02 but not symmetrical, has outliers) ###############
      
      for (i in c(seq(1,30,2)  )){
        ind.temp = TRIX(Ad(ticker), n = i )
        colnames(ind.temp) = paste0(c('trix.trix','trix.signal'),i)
        returns = merge(returns, ind.temp , join = 'left')
      } 
      
    ### macd (-2 to 2, does not change with period? , has outliers) ##########
      for (i in c(seq(3,30,2)  ) ){
        ind.temp = MACD(Ad(ticker),fast = i, slow = i*2, sig = round((i + i*2)/2) )
        ind.temp = ind.temp[,1]
        colnames(ind.temp) = paste0('macd',i)
        returns = merge(returns, ind.temp , join = 'left')
      }
      
    ### macd signal (-2 to 2, does not change with period? )  ##########
      for (i in c(seq(3,30,2)  ) ){
        ind.temp = MACD(Ad(ticker),fast = i, slow = i*2, sig = round((i + i*2)/2) )
        ind.temp = ind.temp[,2]
        colnames(ind.temp) = paste0('macdSig',i)
        returns = merge(returns, ind.temp , join = 'left')
      }
      
    ### macd diff (-1 to 1) ##########
      for (i in c(seq(3,30,2)  ) ){
        ind.temp = MACD(Ad(ticker),fast = i, slow = i*2, sig = round((i + i*2)/2) )
        ind.temp = ind.temp[,1]-ind.temp[,2]
        colnames(ind.temp) = paste0('macdDiff',i)
        returns = merge(returns, ind.temp , join = 'left')
      }
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    ### chaikin volatility (-0.5 to 0.5+) avoid outliers  ##########
      for (i in c(seq(1,30,2)  )){
        ind.temp = chaikinVolatility(HLC(ticker)[,-3],n=i)
        colnames(ind.temp) = paste0('chaikinvolatility',i)
        returns = merge(returns, ind.temp , join = 'left')
      }
      
      
      
      
      
      
    # ### bbands (tied to stock price) ###############
    # 
    # for (i in c(seq(1,30,2),60,90,150,200 )){
    #   ind.temp = BBands(HLC(ticker), n = i )
    #   colnames(ind.temp) = paste0(c('bbands.dn','bbands.mavg','bbands.up','bbands.pctB'),i)
    #   returns = merge(returns, ind.temp , join = 'left')
    # } 
    
    # ### emv (very small between +/- 1e-3) ###############
    # 
    # for (i in c(seq(1,30,2)  )){
    #   ticker.temp = ticker
    #   ticker.temp[,2] <- ticker.temp[,2]+1e-6
    #   ticker.temp[,5] <- ticker.temp[,5]+1e-6
    #   ind.temp = EMV(HLC(ticker.temp)[,-3], Vo(ticker.temp), n = i )
    #   colnames(ind.temp) = paste0(c('emv.emv','emv.ma'),i)
    #   returns = merge(returns, ind.temp , join = 'left')
    # } 

    # ### price.roc.cumsum (small, order of +/-0.02, probably independent of price, think of other ways) ##########
    # for (i in c(seq(1,30,2),60,90,150,200 )){
    #   ind.temp = ROC(Ad(ticker), n = 1, type = 'continuous') 
    #   ind.temp = runSum(ind.temp,n=i)
    #   colnames(ind.temp) = paste0('price.roc.cumsum',i)
    #   returns = merge(returns, ind.temp , join = 'left')
    # }
    
    # ### price.roc.diff.cumsum (larger numbers +/5 plus)  ##########
    # for (i in c(seq(1,30,2),60,90,150,200 )){
    #   ind.temp = ROC(Ad(ticker), n = 1, type = 'continuous') 
    #   ind.temp = diff.xts(Ad(ticker), n = 1 ) 
    #   ind.temp = runSum(ind.temp,n=i)
    #   colnames(ind.temp) = paste0('price.roc.diff.cumsum',i)
    #   returns = merge(returns, ind.temp , join = 'left')
    # } 
    
    # ### disparity  (95 - 115 ish) ###############
    # for (i in c(seq(1,30,4)  )){
    #   ind.temp = Ad(ticker)/SMA(Ad(ticker), n = i)*100
    #   colnames(ind.temp) = paste0('disparity',i)
    #   returns = merge(returns, ind.temp , join = 'left')
    # } 
    
    
    
    
    
    
    ########################
  #  returns = returns[,!colnames(returns) %in% c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')]
    return(returns)
  }
  
  features = list()
  for (i in seq_along(ticker.data)) {
    
    possibleError <- tryCatch({
      ticker = ticker.data[[paste0('ticker',i)]]
      ticker.temp = ticker
      ticker.temp[,str_detect(colnames(ticker.temp),'Adjusted')] = ticker.temp[,str_detect(colnames(ticker.temp),'Close')]
      features.temp = FeaturesDataBuilder(ticker.temp)
      features.temp = features.temp[,!apply(features.temp,2,function(x) {all(is.na(x))} )]  ### eliminate any columns with all NAs
      features[[paste0('ticker',i)]] = features.temp
    }, error = function(e) {
      print(paste('warning:', e, " itr:",i))
    })
  }
  
  
  return(features)
  
  
  
  
}


############ response (y)  

GetResponses <- function(ticker.data, features) {
  
  
  responsedata <- function(ticker) {
    
    ## this is probably second best
    # y = lag.xts( atan(diff.xts(EMA(Cl(ticker),10),4))  ,-4)
    
    ## this works well but conservative on upturn
    # y = lag.xts( atan(diff.xts(EMA(Cl(ticker),10),4))  ,-2)
    
    ## works well , probably best, -1 works too
    y = lag.xts( atan(diff.xts(xts(sgolayfilt(Cl(ticker)),index(ticker)), 4 ))  ,-2)
    

    #y = lag.xts( atan(diff.xts(xts(sgolayfilt(Cl(ticker)),index(ticker)), 4 ))  ,-1)
    
    # y1 =  (lag.xts(Cl(ticker),-2)- Cl(ticker))/Cl(ticker) 
    y1 = lag.xts( atan(diff.xts(EMA(Cl(ticker),10),4))  ,-4)
    temp = data.frame(coredata(ticker), 'dates' = index(ticker))
    temp$y = as.numeric(y)
    temp$y1 = as.numeric(y1)
    ggplot(temp%>%filter(dates>=as.Date('2018-06-01'))) + 
      geom_line(aes(dates,y1)) + 
      geom_line(aes(dates,y),col = 'red')
    
    colnames(y) = c('y')
    return(y)
   }
  
  
  
  
  for (i in seq_along(ticker.data)) {
    
    possibleError <- tryCatch({
      ticker = ticker.data[[paste0('ticker',i)]]
      ticker.temp = ticker
      ticker.temp[,str_detect(colnames(ticker.temp),'Adjusted')] = ticker.temp[,str_detect(colnames(ticker.temp),'Open')]
      returns = merge(features[[paste0('ticker',i)]], responsedata(ticker.temp) , join = 'left') 
      returns = returns[,!apply(returns,2,function(x) {all(is.na(x))} )]  ### eliminate any columns with all NAs
      features[[paste0('ticker',i)]] = returns
    }, error = function(e) {
      print(paste('warning:', e, " itr:",i))
    })
    
  }
  
  return(features)
  
  
}


########### rule and returns
plot.output <- function(output, plot.tkr = 'all') {
  g1 = ggplot(output%>%{if (plot.tkr != "all") filter(., ticker==plot.tkr) else filter(.,) } , 
         aes(date,Adjusted)) + ## 
    facet_wrap(~ticker, scales = 'free_y', nrow = 3)+
    geom_line(col = 'darkgrey', lwd = 1) +
    geom_point(aes(col = ifelse(buy.sell.signal == 'BuySPX',1,-1)  ), size= 2 ) +
    scale_colour_gradient2(low = "red2", mid = "grey",
                           high = "green", midpoint = 0.25, space = "Lab",
                           na.value = "grey50", guide = "colourbar", aesthetics = "colour") +
    # scale_x_date(date_breaks = xrangetext, date_labels =  "%d %b %Y")  + ### 2 day, 1 month
    theme(axis.line = element_line(colour = "black"),
          axis.text.x=element_text(angle=60, hjust=1, size = 12),
          panel.grid.major = element_line(colour = "#F0F0F0",size=0.25),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  return(g1)

}

 
######## model 



############ main
set.seed(100)
# symbol.list = c('SPY', 'RELIANCE.NS', 'AAPL',
#                 'BAC','GOOG', 'AMZN' ,
#                'IBULHSGFIN.NS',   'HAVELLS.NS', 'M&M.NS'
#                 ) ##

symbol.list = c('SPY') ## 


# symbol.list = c('TATAPOWER.NS', 'SUNPHARMA.NS', 'FSL.NS',
#                 'ADSL.NS',   'JKIL.NS'
# )  # 

features = list()

ticker.data = GetDataAppendLatest(symbol.list) 
features = GetFeatures(ticker.data)
features = GetResponses(ticker.data, features) ### response column 'y' is added to features 



# date.vector = seq.Date(as.Date('2017-04-01'),as.Date('2018-11-22'), by = 'day')
# date.vector = date.vector[date.vector %in% index(ticker.data$ticker1)]
# date.index = 1


modeldf = data.frame()
for (i in seq_along(features)){
  temp = data.frame(date=index(features[[i]]), coredata(features[[i]]))
  temp$ticker = names(features)[i]
  modeldf =rbind(modeldf,temp)
}

modeldf = modeldf[complete.cases(modeldf%>%select(-y)),]
#modeldf = modeldf%>%na.omit()
max(modeldf$date)



# ##### some plots 
# 
# temp = modeldf%>%filter(ticker == 'ticker1')
# ggplot(modeldf) + geom_point(aes(Adjusted,aroon13 )) +
#   facet_wrap(~ticker)
# 
# ggplot(modeldf, aes(x = ticker, y = Adjusted)) +
#   geom_boxplot(fill = "grey80", colour = "blue")

# ###### Boruta ####################################
# 
# boruta.train <- Boruta(y~., data = modeldf%>%na.omit() ,maxRuns = 20, doTrace = 2)
# # boruta.train <- Boruta(y~., data = modeldf%>%
# #                          select(-date)%>%
# #                          group_by(ticker)%>%
# #                          mutate(y = mean(Adjusted) )%>%
# #                          ungroup(),
# #                        maxRuns = 20, doTrace = 2)
# 
# print(boruta.train)
# plot(boruta.train)
# 
# 
# # par(mar=c(13.1, 4.1, 4.1, 2.1))  ## default = 5.1 4.1 4.1 2.1
# par(mar=c(5.1, 4.1, 4.1, 2.1))  ## default = 5.1 4.1 4.1 2.1
# 
# plot(boruta.train, xlab = "", xaxt = "n" )
# lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
#   boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
# 
# new.names = data.frame(names = colnames(boruta.train$ImpHistory), order = seq(1:length(colnames(boruta.train$ImpHistory))) )
# names(lz) <- new.names$names
# Labels <- sort(sapply(lz,median))
# axis(side = 1,las=2,labels = names(Labels),
#      at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
# 
# final.boruta <- boruta.train # TentativeRoughFix(boruta.train)
# print(final.boruta)
# 
# boruta.sel.cols = getSelectedAttributes(final.boruta, withTentative = F)
# boruta.df <- attStats(final.boruta)
# 
# 
# # saveRDS(boruta.df%>%tibble::rownames_to_column('features'), paste0('borutadf_',today(),'.RDS' ) )

####################################################

feature.list = readRDS('borutadf_2019-05-13.RDS')%>%
  filter(decision == "Confirmed")%>%
  dplyr::top_n(100,medianImp)%>%
  pull(features)

feature.list = feature.list[!feature.list %in% c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted', 'date', 'ticker')]

date.split = as.Date('2018-06-01')
traindf = modeldf%>%filter(date<=date.split)
testdf = modeldf%>%filter(date>date.split)

xgb.grid <-  expand.grid(nrounds = c( 100), ## tune this
                         max_depth = c(6), ## 6
                         eta = c( 0.01), ## 0.005
                         gamma = 0,
                         colsample_bytree=c(0.8),
                         min_child_weight=1,
                         subsample = 0.8)

xgb.train.control <- trainControl(method="cv", number=5, #  classProbs = T,
                                  search = "grid" ) #repeats=1, sampling = "down"

close.linear.model.xgb <- caret::train(x = as.matrix(traindf[,colnames(traindf) %in% feature.list ] ),
                          y = traindf$y ,
                          method="xgbTree",
                          metric = "RMSE" ,  ### RMSE
                          objective= 'reg:linear' ,
                          trControl = xgb.train.control,
                          verbose = TRUE,
                          tuneGrid = xgb.grid)

# saveRDS(close.linear.model.xgb, file = paste0('models/linear_xgbmodel_', today() )  )


# close.linear.model.xgb = readRDS("models/linear_xgbmodel_2019-05-13" )

plot(varImp(close.linear.model.xgb))

ypred = predict(close.linear.model.xgb, newdata = as.matrix(testdf[,colnames(testdf) %in% feature.list ] ) )
output = cbind(testdf,'ypred' = ypred)
output$buy.sell.signal = ifelse(output$ypred>=0.25,'BuySPX','SellSPX')

for (i in seq_along(symbol.list)) {
output$ticker[output$ticker == paste0('ticker',i) ] = symbol.list[i]
}

# ##############################
# 
# 
# model.list = list(
#   'SCHAFF' = deparse(SCHAFF),  ### newMyFunc <- eval(parse(text=funcAsText)), newMyFunc("foo")
#   'GetFeatures' = deparse(GetFeatures),
#   'GetResponses' = deparse(getResponses),
#   'GetDataAppendLatest' = deparse(GetDataAppendLatest),
#   'plot.output' = deparse(plot.output),
#   'feature.list' = feature.list,
#   'predict.model.xgb' = close.linear.model.xgb
# )
# 
# saveRDS(model.list, file = paste0('models/SPXBuySellModel_',str_replace_all(today(),'-','' ))  )
# 
# model.list = readRDS("models/SPXBuySellModel_20190517" )


ggplot(output%>%filter(ticker=='SPY'), aes(date,Adjusted)) + ## 
  facet_wrap(~ticker, scales = 'free_y', nrow = 3)+
  geom_line(col = 'darkgrey', lwd = 1) +
  geom_point(aes(col = ifelse(buy.sell.signal == 'BuySPX',1,-1)  ), size= 2 ) +
  scale_colour_gradient2(low = "red2", mid = "grey",
                         high = "green", midpoint = 0.25, space = "Lab",
                         na.value = "grey50", guide = "colourbar", aesthetics = "colour") +
  # scale_x_date(date_breaks = xrangetext, date_labels =  "%d %b %Y")  + ### 2 day, 1 month
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(angle=60, hjust=1, size = 12),
        panel.grid.major = element_line(colour = "#F0F0F0",size=0.25),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


ggplot(output%>%filter(ticker=='SPY')) +
  facet_wrap(~ticker, scales = 'free_y', nrow = 3)+
  geom_line( aes(date,y), col = 'darkgrey', lwd = 1) +
  geom_point(aes(date,y), col = 'darkgrey', size= 2 ) +
  geom_line( aes(date,ypred ), col = 'red2', lwd = 1) +
  geom_point(aes(date,ypred ), col = 'red2', size= 2 ) +
  # scale_colour_gradient2(low = "red2", mid = "grey",
  #                        high = "green", midpoint = 0.3, space = "Lab",
  #                        na.value = "grey50", guide = "colourbar", aesthetics = "colour") +
    # scale_x_date(date_breaks = xrangetext, date_labels =  "%d %b %Y")  + ### 2 day, 1 month
  theme(axis.line = element_line(colour = "black"),
        axis.text.x=element_text(angle=60, hjust=1, size = 12),
        panel.grid.major = element_line(colour = "#F0F0F0",size=0.25),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


temp = output
temp$daily.nextday.returns = (lag.xts(temp$Adjusted,-1)-temp$Adjusted)/temp$Adjusted

sum(temp$daily.nextday.returns[temp$ypred >= 0.25], na.rm=T) +
  sum(-temp$daily.nextday.returns[temp$ypred < 0.25], na.rm=T)   

sum(temp$daily.nextday.returns, na.rm=T)



# ############# backtest #############
# 
# backtest.dates = seq.Date(today()-100, today(), by = 'day')
# backtest.dates = backtest.dates[backtest.dates %in% index(ticker.data[[1]])  ]
# 
# backtest.output = data.frame()
# 
# for (i in seq_along(backtest.dates)) {
#   print(paste0(i,': ',backtest.dates[i]))
#   ticker.data.trunc = lapply(ticker.data, 
#                              function(x)  x[paste0('/',backtest.dates[i]) ]   
#                              )
#   
#   features.trunc = GetFeatures(ticker.data.trunc)
#   features.trunc = GetResponses(ticker.data.trunc, features.trunc) ### response column 'y' is added to features 
#   
#   modeldf.trunc = data.frame()
#   for (j in seq_along(features.trunc)){
#     temp = data.frame(date=index(features.trunc[[j]]), coredata(features.trunc[[j]]))
#     temp$ticker = names(features.trunc)[j]
#     modeldf.trunc =rbind(modeldf.trunc,temp)
#   }
#   
#   modeldf.trunc = modeldf.trunc[complete.cases(modeldf.trunc%>%select(-y)),]
# 
#   ypred = predict(close.linear.model.xgb, 
#                   newdata = as.matrix(modeldf.trunc%>%select(.dots = feature.list )) )
#   output.trunc = cbind(modeldf.trunc,'ypred' = ypred)
#   
#   for (k in seq_along(symbol.list)) {
#     output.trunc$ticker[output.trunc$ticker == paste0('ticker',k) ] = symbol.list[k]
#   }
#   
#   backtest.output =rbind(backtest.output, 
#                          output.trunc[output.trunc$date == backtest.dates[i],,drop=F] 
#                          )
#   
# }
#  
# 
# backtest.output1 = rbind( output[output$date <= min(backtest.dates),,drop=F ] ,
#                            backtest.output)
# 
# 
# ggplot(backtest.output1, aes(date,Adjusted)) +
#   facet_wrap(~ticker, scales = 'free_y', nrow = 3)+
#   geom_line(col = 'darkgrey', lwd = 1) +
#   geom_point(aes(col = ypred  ), size= 2 ) +
#   scale_colour_gradient2(low = "red2", mid = "grey",
#                          high = "green", midpoint = 0.3, space = "Lab",
#                          na.value = "grey50", guide = "colourbar", aesthetics = "colour") +
#   # scale_x_date(date_breaks = xrangetext, date_labels =  "%d %b %Y")  + ### 2 day, 1 month
#   theme(axis.line = element_line(colour = "black"),
#         axis.text.x=element_text(angle=60, hjust=1, size = 12),
#         panel.grid.major = element_line(colour = "#F0F0F0",size=0.25),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank())





# ########### LSTM Keras
# ## https://stackoverflow.com/questions/51824944/r-keras-lstm-input-shape
# ## https://blog.usejournal.com/stock-market-prediction-by-recurrent-neural-network-on-lstm-model-56de700bff68
# ## https://stackoverflow.com/questions/38714959/understanding-keras-lstms
# ## https://datascience.stackexchange.com/questions/17024/rnns-with-multiple-features
# ## https://datascience.stackexchange.com/questions/27563/multi-dimentional-and-multivariate-time-series-forecast-rnn-lstm-keras?rq=1
# 
# lstm.modeldf = modeldf # modeldf[,colnames(modeldf) %in% c(feature.list, 'y')] 
# # lstm.modeldf$y = (lstm.modeldf$Adjusted - lag.xts(lstm.modeldf$Adjusted,-4) ) / lstm.modeldf$Adjusted
# 
# # lstm.modeldf = lstm.modeldf%>%
# #   group_by(ticker)%>%
# #   mutate(y = (lstm.modeldf$Adjusted - lag.xts(lstm.modeldf$Adjusted,-4) ) / lstm.modeldf$Adjusted)%>%
# #   ungroup()
# 
# 
# # lstm.modeldf = lstm.modeldf%>%
# #   group_by(ticker)%>%
# #   arrange(date,.by_group = TRUE)%>%
# #   mutate(win = row_number()%/% 30 )%>%
# #   ungroup()%>%
# #   group_by(ticker,win)%>%
# #   mutate(y = scale(y))%>%
# #   ungroup()%>%
# #   select(-win)
# 
# ### standardize the selected features
# lstm.modeldf[,colnames(modeldf) %in% c(feature.list)] =
#   scale(lstm.modeldf[,colnames(modeldf) %in% c(feature.list)])
# 
# ### normalize -0.5 to 0.5 after standardization
# lstm.modeldf[,colnames(modeldf) %in% c(feature.list)] =
#  apply( lstm.modeldf[,colnames(modeldf) %in% c(feature.list)], 2,
#         function(x) scales::rescale(x, c(-0.5,0.5)) )
# 
# 
# # https://machinelearningmastery.com/faq/single-faq/what-is-the-difference-between-samples-timesteps-and-features-for-lstm-input/
# # https://machinelearningmastery.com/use-different-batch-sizes-training-predicting-python-keras/
# 
# datalags = 10 ## should be multiple of batchsize
# # num_samples = several samples
# num_timesteps = 50
# num_features = length(feature.list)
# batch_size = 20 
# 
# ##train is 60% of total, val is 20%, test is 20%. these are sequential splits
# train.split = 0.6 
# val.split = 0.2  
# 
# x.train = array(numeric(),c(0,num_timesteps, length(feature.list) )) 
# y.train = array(numeric(),c(0,num_timesteps, 1 )) 
# x.val = array(numeric(),c(0,num_timesteps, length(feature.list) ))
# y.val = array(numeric(),c(0,num_timesteps, 1 ))
# x.test = array(numeric(),c(0,num_timesteps, length(feature.list) )) 
# y.test = array(numeric(),c(0,num_timesteps, 1 )) 
# 
# 
# for (tkr in unique(lstm.modeldf$ticker) ) {
#   temp = lstm.modeldf%>%
#     filter(ticker == tkr)%>%
#     top_n(floor(nrow(.)/num_timesteps)*num_timesteps, date)%>%
#     arrange(date)
#   
#   temp.list = lapply(seq(1,nrow(temp)-num_timesteps+1,datalags),
#                      function(x) temp[x:(x+num_timesteps-1),] )
#   
#   temp.3d = abind::abind(temp.list, along = 3, 
#                          new.names = paste0('b_',tkr,'_', 1:length(temp.list) ) )
#   temp.3d = aperm(temp.3d, c(3,1,2) )  ### reorder 1:batches, 2: timesteps, 3: features
#   dimnames(temp.3d)[[2]] = seq(1:num_timesteps)
#   
#   ### the dimensions for temp.3d are batch/seq, timesteps, features
#   ### the output y is 2 dimensional (there is only one output per sequence)
#   x.train = abind::abind(x.train,
#                          temp.3d[1:floor(dim(temp.3d)[1]*train.split),,
#                                  feature.list],
#                          along = 1)
#     
#   y.train = abind::abind(y.train,
#                          temp.3d[1:floor(dim(temp.3d)[1]*train.split),,
#                                  'y',drop=F],
#                          along = 1)
#   
#  
#   x.val = abind::abind(x.val,
#                          temp.3d[ (floor(dim(temp.3d)[1]*train.split) + 1):
#                                    floor(dim(temp.3d)[1]*(train.split + val.split) ),,
#                                  feature.list],
#                          along = 1)
# 
#   y.val = abind::abind(y.val,
#                          temp.3d[(floor(dim(temp.3d)[1]*train.split) + 1):
#                                    floor(dim(temp.3d)[1]*(train.split + val.split) ),,
#                                  'y',drop=F],
#                          along = 1)
#   
#   x.test = abind::abind(x.test,
#                          temp.3d[( floor(dim(temp.3d)[1]*(train.split + val.split ) )+1):
#                                    (dim(temp.3d)[1]) ,,
#                                  feature.list],
#                          along = 1)   
# 
#   y.test = abind::abind(y.test,
#                         temp.3d[( floor(dim(temp.3d)[1]*(train.split + val.split) )+1):
#                                   (dim(temp.3d)[1]),,
#                                 'y',drop=F],
#                         along = 1) 
#     
# }
# 
# x.train = x.train[(dim(x.train)[1] - floor(dim(x.train)[1]/batch_size)*batch_size + 1 ):(dim(x.train)[1]),,]
# y.train = abind::adrop(y.train[,num_timesteps,1,drop=F],3)
# y.train = y.train[(dim(x.train)[1] - floor(dim(x.train)[1]/batch_size )*batch_size + 1):(dim(x.train)[1]),,drop=F]
# 
# x.val = x.val[(dim(x.val)[1] - floor(dim(x.val)[1]/batch_size)*batch_size + 1 ):(dim(x.val)[1]),,]
# y.val = abind::adrop(y.val[,num_timesteps,1,drop=F],3)
# y.val = y.val[(dim(x.val)[1] - floor(dim(x.val)[1]/batch_size )*batch_size + 1):(dim(x.val)[1]),,drop=F]
# 
# y.test = abind::adrop(y.test[,num_timesteps,1,drop=F],3)
# 
# class(x.train) <- "numeric"
# class(y.train) <- "numeric"
# class(x.val) <- "numeric"
# class(y.val) <- "numeric"
# class(x.test) <- "numeric"
# class(y.test) <- "numeric"
# 
# # https://machinelearningmastery.com/prepare-univariate-time-series-data-long-short-term-memory-networks/
# ## https://rpubs.com/andreasme/keras-lstm-notebook
# 
# ## http://rwanjohi.rbind.io/2018/04/05/time-series-forecasting-using-lstm-in-r/
# 
# X_shape2 = dim(x.train)[2]
# X_shape3 = dim(x.train)[3]
# 
# rm(model)
# model <- keras_model_sequential()%>%
#   layer_lstm(units = 6, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= FALSE)%>% ## 32
#   layer_dropout(rate = 0.2) %>% 
#   layer_dense(units = 1)%>% 
#   compile(
#   loss = 'mean_squared_error',
#   optimizer = optimizer_adam( lr= 0.0002, decay = 1e-7 ) #,  ## 0.02, 1e-6
#  # metrics = c('mse')
#   )
# 
# 
# summary(model)
# 
# # abind::adrop(y.train[,50,1,drop=F],3)
# 
# history = model%>%fit(x.train, y.train, batch_size = batch_size,
#             validation_data = list(x.val, y.val),
#             view_metrics = T,
#             # validation_split = floor(dim(x.train)[1]*0.25/batch_size)*batch_size/(dim(x.train)[1]),
#             epochs=20, verbose=1)
# 
# 
# 
# plot(history)
# 
# 
# 
# batch1.model <- keras_model_sequential()%>%
#   layer_lstm(units = 6, batch_input_shape = c(1, X_shape2, X_shape3), stateful= FALSE)%>% ## 32
#   layer_dropout(rate = 0.2) %>% 
#   layer_dense(units = 1)%>% 
#   compile(
#     loss = 'mean_squared_error',
#     optimizer = optimizer_adam( lr= 0.0002, decay = 1e-7 ) #,  ## 0.02, 1e-6
#     # metrics = c('mse')
#   )%>%
#   set_weights(model%>%get_weights())
# 
# 
# 
# yhat = batch1.model %>% predict(x.test, batch_size=1)*10-1
# 
# 
# ggplot() + geom_line(aes(seq(1:171),yhat[1:171])) +
#   geom_line(aes(seq(1:171),as.numeric(y.test)[1:171] ), col = 'red')
# 
# 
# 
# # plot(lstm.modeldf$y[lstm.modeldf$ticker == 'ticker1'])
# 
# 
# 
# # 
# # L = length(x_test)
# # scaler = Scaled$scaler
# # predictions = numeric(L)
# # 
# # for(i in 1:L){
# #   X = x_test[i]
# #   dim(X) = c(1,1,1)
# #   yhat = model %>% predict(X, batch_size=batch_size)
# #   # invert scaling
# #   yhat = invert_scaling(yhat, scaler,  c(-1, 1))
# #   # invert differencing
# #   yhat  = yhat + Series[(n+i)]
# #   # store
# #   predictions[i] <- yhat
# # }
# #