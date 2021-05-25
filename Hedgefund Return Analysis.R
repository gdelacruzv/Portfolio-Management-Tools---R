####################
#Libraries#####
library (PerformanceAnalytics)
library(quantmod)
library(RColorBrewer)
library(lubridate)
library(Quandl)

############
#Parameters#
############

BENCHMARK<-"SPY"

Quandl.api_key("ZTi54zY_nvoUsJj-Riqx")

eDate<- Sys.Date()
sDate<- eDate-years(100)


Bench.Prices<-Ad(getSymbols(BENCHMARK,from=sDate,to=eDate,auto.assign = F))

####################
# Hedge Fund Data###
###################
# Quandl tags
Funds <- c("EUREKA/650", "EUREKA/648", "EUREKA/647", "EUREKA/646","EUREKA/645","EUREKA/644","EUREKA/638","EUREKA/485","EUREKA/483","EUREKA/481","EUREKA/480","EUREKA/479","EUREKA/478","EUREKA/477","EUREKA/476","EUREKA/474","EUREKA/473")
# Fund type labels
FundNames <- c("Commodity","FX","Trend.Following","Equity.Short.Bias","Equity.Long.Bias","Equity.Market.Neutral","Frontier.Markets","Relative.Value","Multi.Strategy","Macro","Equity.Long.Short","Fixed.Income","Event.Driven","Distressed.Debt","CTA.Managed.Futures","Arbitrage","Hedge.Funds")

FundData<- Quandl(Funds,type="xts",start_date=sDate)/100
colnames(FundData) <- FundNames
head(FundData)
length(FundNames)

#####################
##Merge Data Sets###
##################
colnames(Bench.Prices)<- BENCHMARK
allData<-na.omit(merge(FundData,Bench.Prices))
head(allData)
allData[,BENCHMARK]<-ROC(allData[,BENCHMARK], type="discrete")
allData<-na.omit(allData)
summary(allData)

#################
#Visualize Data#
################

# Set up color palette#
myCols <- c("#000000", brewer.pal(9, "Set1"))
myCols[7]<-"darkgreen"
myFunds<-c(BENCHMARK,"Commodity","FX","Trend.Following","Equity.Short.Bias","Equity.Long.Bias","Equity.Market.Neutral","Frontier.Markets","Relative.Value","Fixed.Income")
length(myFunds)
length(myCols)
# Plot cumulative returns

chart.Bar(allData[,myFunds], colorset=myCols, main="Hedge Fund Periodic Returns", ylab="Periodic Returns", legend.loc = "bottomright")
# Plot drawdowns
chart.Drawdown(allData[,myFunds], geometric = T, main="Hedge Fund Drawdown", colorset=myCols, legend.loc = "bottomright", ylab="Drawdown")
# Plot rolling performance
charts.RollingPerformance(allData[,myFunds], width=12, main="Rolling 12-Month Hedge Fund Performance", colorset=myCols, legend.loc = "left")
table.AnnualizedReturns(allData[,myFunds], scale=12)
