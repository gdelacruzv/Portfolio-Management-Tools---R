MSFT.prices<-Ad(getSymbols("MSFT",auto.assign = F))
MSFT.prices
maxdate<-"2000-01-01"  #Setting first day to pull
MSFT.prices<-Ad(getSymbols("MSFT",auto.assign = F, from=maxdate))
MSFT.prices
head(MSFT.prices)
MSFT.rets<-dailyReturn(MSFT.prices)
MSFT.rets

#Calculating Var (Value at risk)
VaR(MSFT.rets,p=0.95,method="historical")

#Calculating cVaR (conditional Value at risk)
CVaR(MSFT.rets,p=0.99,method="historical")

#Creating Vector of stocks & Calculating Var
tickers<-c("MSFT","AAPL","AMZN")
weights<-c(0.5,0.10,0.40)
getSymbols(tickers, from=maxdate)

Port.prices<-merge(Ad(MSFT),Ad(AAPL),Ad(AMZN))
head(Port.prices)
Port.prices<-na.omit(merge(Ad(MSFT),Ad(AAPL),Ad(AMZN)))
port.returns<-ROC(Port.prices)
head(port.returns)
port.returns<-ROC(Port.prices,type="discrete")[-1,]  #eliminates empty first row
head(port.returns)
colnames(port.returns)<-tickers
head(port.returns)

VaR(port.returns,p=.99,weights=weights,portfolio_method = "component",method = "modified")  #Modified is Cornish Fisher Var
ETL(port.returns,p=.99,weights=weights,portfolio_method = "component",method = "modified")


#Different Methods of Caluclating Var
Var.Hist<-VaR(port.returns,p=.95,weights = NULL,portfolio_method = "single", method = "historical")
Var.Hist<-VaR(port.returns,p=.95,weights = NULL,portfolio_method = "single", method = "gaussian")
Var.Mod<-VaR(port.returns,p=.95,weights = NULL,portfolio_method = "single", method = "modified")
all.var<-data.frame(rbind(Var.Hist,Var.Hist,Var.Mod))
all.var
#Updating row names
rownames(all.var)<-c("Hist","Gauss","Mod")
all.var

PortVar.Hist<-VaR(port.returns,p=.95,weights = weights,portfolio_method = "component", method = "historical") $hVaR[1]  
PortVar.Hist
PortVar.Gaus<-VaR(port.returns,p=.95,weights = weights,portfolio_method = "component", method = "gaussian")$VaR[1]  #Adding $VaR[1] to only get port var
PortVar.Mod<-VaR(port.returns,p=.95,weights = weights,portfolio_method = "component", method = "modified")$MVaR[1]

all.var$Portfolio<-0   #Adding new column
all.var$Portfolio<-c(PortVar.Hist,PortVar.Gaus,PortVar.Mod)
all.var<-abs(all.var)
all.var
all.var$Type<-c("Hist","Gaus","Mod")
all.var

install.packages(Reshape2)

library(reshape2)
library(ggplot2)

plotvar<-melt(all.var,variable.name = "ticker",value.name = "Var")
plotvar
ggplot(plotvar, aes(x=Type,y=Var,fill=ticker))+geom_bar(stat="identity",position="dodge")
