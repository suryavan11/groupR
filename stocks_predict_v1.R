library(quantmod)
library(TTR)
library(QuantTools)
library(Quandl)
library(readr)
library(stringr)
library(lubridate)
library(xgboost)
library(caret)
library(signal)
library(dplyr)

setwd('C:\\Users\\asuryav1\\OneDrive - T-Mobile USA\\personal\\stocks')
model.list = readRDS("models/SPXBuySellModel_20190517" )


SCHAFF <- eval(parse(text=model.list$SCHAFF)) 
GetDataAppendLatest <- eval(parse(text=model.list$GetDataAppendLatest)) 
GetFeatures <- eval(parse(text=model.list$GetFeatures)) 
GetResponses <- eval(parse(text=model.list$GetResponses)) 
plot.output <- eval(parse(text=model.list$plot.output)) 


set.seed(100)
symbol.list = c('SPY')
ticker.data = GetDataAppendLatest(symbol.list) 
features = GetFeatures(ticker.data)
features = GetResponses(ticker.data, features) ### response column 'y' is added to features 

modeldf = data.frame()
for (i in seq_along(features)){
  temp = data.frame(date=index(features[[i]]), coredata(features[[i]]))
  temp$ticker = names(features)[i]
  modeldf =rbind(modeldf,temp)
}

modeldf = modeldf[complete.cases(modeldf%>%select(-y)),]
modeldf = modeldf%>%filter(date >= today()- 365 )

ypred = predict(model.list$predict.model.xgb, 
                newdata = as.matrix(modeldf[,colnames(modeldf) %in% model.list$feature.list ] ) )
output = cbind(modeldf,'ypred' = ypred)
output$buy.sell.signal = ifelse(output$ypred>=0.25,'BuySPX','SellSPX')
plot.output(output)

