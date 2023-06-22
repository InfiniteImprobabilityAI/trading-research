###################################################
### The Available Data
###################################################

install.packages(xts)
install.packages(zoo)
install.packages('Metrics')
library(DMwR)
library(quantmod)
library(xts)
library(zoo)
library(TTR)
library(Metrics)
library(caret)

##########################################
### Reading the data from the CSV file####

#### Reading .CSV ####
IXIC <- as.xts(read.csv.zoo('IXIC.csv',header=T))
dim(IXIC)
head(IXIC)

names(IXIC) <- c("Open","High","Low","Close","Adjusted","Volume")

head(IXIC)

tail(IXIC)

IXIC["1984-10-09/1984-10-12"]

IXIC <- as.xts(IXIC["1984-10-11/"])
dim(IXIC)

#### Candlestick chart ####
candleChart(last(IXIC,'12 months'),theme='white',TA=NULL)
candleChart(last(IXIC,'12 months'),minor.ticks = FALSE,theme = chartTheme("white"))
avgPrice <- function(p) apply(HLC(p),1,mean)
addAvgPrice <- newTA(FUN=avgPrice,col=1,legend='AvgPrice')

######################################
#### Defining the Prediction Tasks ####

#### K-Days T indicator ####
T.ind <- function(quotes,tgt.margin=0.025,n.days=10) {
  v <- apply(HLC(quotes),1,mean)
  
  r <- matrix(NA,ncol=n.days,nrow=NROW(quotes))
  for(x in 1:n.days) r[,x] <- Next(Delt(Cl(quotes),v,k=x),x)
  
  x <- apply(r,1,function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(quotes)) xts(x,time(quotes)) else x
}

T.ind(IXIC)

#### New Candlestick Chart ####
candleChart(last(IXIC,'12 months'),theme='white',TA=NULL)
candleChart(last(IXIC,'12 months'),minor.ticks = FALSE,theme = chartTheme("white"))
avgPrice <- function(p) apply(HLC(p),1,mean)
addAvgPrice <- newTA(FUN=avgPrice,col=1,legend='AvgPrice')
addT.ind <- newTA(FUN=T.ind,col='red',legend='tgtRet')
addAvgPrice(on=1)
addT.ind()

### Using Random Forest to Choose Optimal Technical Indicators ####
myATR <- function(x) ATR(HLC(x))[,'atr']
mySMI <- function(x) SMI(HLC(x))[,'SMI']
myADX <- function(x) ADX(HLC(x))[,'ADX']
myAroon <- function(x) aroon(x[,c('High','Low')])$oscillator
myBB <- function(x) BBands(HLC(x))[,'pctB']
myChaikinVol <- function(x) Delt(chaikinVolatility(x[,c("High","Low")]))[,1]
myCLV <- function(x) EMA(CLV(HLC(x)))[,1]
myEMV <- function(x) EMV(x[,c('High','Low')],x[,'Volume'])[,2]
myMACD <- function(x) MACD(Cl(x))[,2]
myMFI <- function(x) MFI(x[,c("High","Low","Close")], x[,"Volume"])
mySAR <- function(x) SAR(x[,c('High','Close')]) [,1]
myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1]
myWill   <- function(x) williamsAD(HLC(x))
myRSI <- function(x) RSI(Cl(x))[,1]


library(randomForest)
rfImp.model <- specifyModel(T.ind(IXIC) ~ Delt(Cl(IXIC),k=1:10) + 
                             myWill(IXIC) + myATR(IXIC) + mySMI(IXIC) + myADX(IXIC) + myAroon(IXIC) + 
                             myBB(IXIC)  + myChaikinVol(IXIC) + myCLV(IXIC) + 
                             CMO(Cl(IXIC)) + EMA(Delt(Cl(IXIC))) + myEMV(IXIC) + 
                             myVolat(IXIC)  + myMACD(IXIC) + myMFI(IXIC) + RSI(Cl(IXIC)) +
                             mySAR(IXIC) + runMean(Cl(IXIC)) + runSD(Cl(IXIC)))

set.seed(1234)
rf <- buildModel(rfImp.model,method='randomForest',
                 training.per=c(start(IXIC),index(IXIC["2019-02-08"])),
                 ntree=50, importance=T)

varImpPlot(rf@fitted.model,type=1)
imp <- importance(rf@fitted.model,type=1)
rownames(imp)[which(imp > 10)]

#### Updated Model ####
rfImp.model <- specifyModel(T.ind(IXIC) ~ 
                              myADX(IXIC) +    
                              myATR(IXIC) + 
                              myVolat(IXIC) + 
                              myMACD(IXIC) + 
                              mySMI(IXIC) +
                              CMO(Cl(IXIC)) +
                              myWill(IXIC) + 
                              runMean(Cl(IXIC)) ) 

Tform <- as.formula('T.ind.IXIC ~ .')

#### Splitting Data####
Tdata.train <- as.data.frame(modelData(rfImp.model,
                                       data.window=c('1984-10-11','2017-02-10')))

Tdata.val <- na.omit(as.data.frame(modelData(rfImp.model,
                                              data.window=c('2017-02-11','2019-02-07'))))

Tdata.test <- na.omit(as.data.frame(modelData(rfImp.model, 
                                              data.window=c('2019-02-08','2021-02-02'))))

###################################################
### The Prediction Models
###################################################

#### Normalizing data ####
library(nnet)
norm.data <- scale(Tdata.train)
norm.vali <- scale(Tdata.val)
norm.test <- scale(Tdata.test)
best.network <- matrix(c(5, 0.5))
best.rmse <- 2


#### Determining the Optimal Parameters ####
for(i in 5:15) {
    for (j in 1:3) {
      set.seed(1234)
      nn.fit <- nnet(Tform,norm.data,size= i ,decay=0.01 * j ,maxit=1000,linout=T,trace=F)
      norm.preds <- predict(nn.fit,norm.vali)
      nn.rmse <- rmse(as.numeric(norm.vali),as.numeric(norm.preds))
      if (nn.rmse<best.rmse) {
        best.network[1, 1] <- i
        best.network[2, 1] <- j
        best.rmse <- nn.rmse
      }
    } 
}
best.network


#### Building the Model with optimal Nodes and Weight ####
set.seed(1234)
nn.fit <- nnet(Tform,norm.test,size=best.network[1, 1],decay=0.01 * best.network[2, 1],maxit=1000,linout=T,trace=F)
norm.preds <- predict(nn.fit,norm.test )
nn <- unscale(norm.preds,norm.test)


#### Evaluate Signals ####
sigs.nn <- trading.signals(nn,0.1,-0.1)
true.sigs <- trading.signals(Tdata.test[,"T.ind.IXIC"],0.1,-0.1)

sigs.PR(sigs.nn, true.sigs)

confusionMatrix(sigs.nn ,true.sigs)


#### Setting up a simulated market.
# learning the model and obtaining its signal predictions
date <- rownames(Tdata.test[1,])
market <- IXIC[paste(date,'/',sep='')][1:500]



### Defining the Simulation Policies
policy.1 <- function(signals,market,opened.pos,money,
                     bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05
)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b' && !nOs) {
    quant <- round(bet*money/market[d,'Close'],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         market[d,'Close']*(1+exp.prof),
                                         market[d,'Close']*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's' && !nOs) {
    # this is the nr of stocks we already need to buy 
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*market[d,'Close']
    quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         market[d,'Close']*(1-exp.prof),
                                         market[d,'Close']*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  # Now lets check if we need to close positions
  # because their holding time is over
  if (nOs) 
    for(i in 1:nOs) {
      if (d - opened.pos[i,'Odate'] >= hold.time)
        orders <- rbind(orders,
                        data.frame(order=-opened.pos[i,'pos.type'],
                                   order.type=1,
                                   val = NA,
                                   action = 'close',
                                   posID = rownames(opened.pos)[i]
                        )
        )
    }
  
  orders
}



policy.2 <- function(signals,market,opened.pos,money,
                     bet=0.2,exp.prof=0.025, max.loss= 0.05
)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b') {
    quant <- round(bet*money/market[d,'Close'],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         market[d,'Close']*(1+exp.prof),
                                         market[d,'Close']*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's') {
    # this is the money already committed to buy stocks
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*market[d,'Close']
    quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         market[d,'Close']*(1-exp.prof),
                                         market[d,'Close']*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  orders
}

### Using The Simulated Trader ANN sig with policy.1

t1 <- trading.simulator(market,sig,
                        'policy.1',list(exp.prof=0.05,bet=0.2,hold.time=30))
t1
tradingEvaluation(t1)
plot(t1,market,theme='white',name='IXIC')

#Using the Simulated Trader on true.sigs with policy.1
true.t1 <- trading.simulator(market,true.sigs,
                             'policy.1',list(exp.prof=0.05,bet=0.2,hold.time=30))
true.t1

tradingEvaluation(true.t1)
plot(true.t1,market,theme='white',name='IXIC')


### Using The Simulated Trader ANN sig with policy.2

t2 <- trading.simulator(market,sig,
                        'policy.2',list(exp.prof=0.05,bet=0.2))
summary(t2)

tradingEvaluation(t2)
plot(t2,market,theme='white',name='IXIC')

#Using the Simulated Trader on true.sigs with policy.2
true.t2 <- trading.simulator(market,true.sigs,
                             'policy.2',list(exp.prof=0.05,bet=0.2))
summary(true.t2)

tradingEvaluation(true.t2)
plot(true.t2,market,theme='white',name='IXIC')

