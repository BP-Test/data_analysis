##Setting working environment etc 
setwd("C:/Users/tatsu/Desktop/r_projects/data")
dir()
attach(sony)
sony <- read.csv("6758_Historical_Data.csv")
sony[,1] <- as.Date(sony$Date)
sony[,6] <- as.numeric(sony$Vol.M)
str(sony)
library(fUnitRoots)
library(ggplot2)
library(cowplot)
library(xtable)
library(rugarch)
library("vars")
# １）データの出所、期間、平均、分散、最大値、最小値、特徴がわかるグラフを作成しなさい
## Plotting related data for Q.1
### 期間
#Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#"2014-01-10" "2015-07-08" "2017-01-10" "2017-01-06" "2018-07-04" "2020-01-10" 

###Mean Median Variance, Max, Min in latex format
sony_summary <- summary(sony)
xtable(sony_summary, label="tb-ref", caption="Data Summary for this Report")

price_1 <- ggplot(sony,aes(x=Date,y=Price))+
  geom_point()+
  theme_bw()
price_2 <- ggplot(sony,aes(x=Date,y=Price))+
  geom_line()+
  theme_bw()
plot_grid(price_1,price_2)
sonyrate1 = diff(Price)/lag(price, k=-1)*100
sonyrate2 = diff(log(Price))*100
par(mfcol=c(2,1))
plot(sonyrate1, type="l")
plot(sonyrate2, type="l")

## 
par(mfcol=c(1,1))
plot(sonyrate1,type="l",ylab="rate",ylim=c(-20,20))
par(new=T)
plot(sonyrate2+5,type="l",col=2,ylab="rate",ylim=c(-20,20))

#標本自己相関
acf(sonyrate2,lag.max=20,main="sony 変化率のコレログラム")
acfsonyrate2=acf(sonyrate2,lag.max=20)
acf(sonyrate2,type="covariance",lag.max=20,main="sony 変化率の共分散")
acvfsonyrate2=acf(sonyrate2,type= "covariance",lag.max=20)
acvfsonyrate2=acvfsonyrate2$acf

var(sonyrate2)
sonyrate2_length <- length(sonyrate2)

acvfsonyrate2 = ((sonyrate2_length -1)/(sonyrate2_length))*var(sonyrate2)*acvfsonyrate2
acvfsonyrate2

# LB検定
Box.test(sonyrate2, lag=20, type="Ljung-Box")

# ARModel
sony2ar1 = arima(sonyrate2,order=c(1,0,0))
sony2ar1aic = sony2ar1$aic
sony2arma21= arima(sonyrate2, order=c(2,0,1))

# BIC == AIC –2k + log(T )k
sony2ar1bic = sony2ar1$aic-2*3+log(sonyrate2_length)*3
sony2ar1bic
sony2ar1.pred = predict(sony2ar1,n.ahead=80)
sony2ar1.pred

sonyrate2.3y = ts(sonyrate2[328:363],start=c(2002,5),frequency=12)
plot(sonyrate2.3y,type="l",xlim=c(2002,2007),ylab="%", xlab="year",
       main="SONY の変化率")
lines(sony2arma21hat,lty=1,col=2)

sig =sony2arma21.pred$se
sony2arma21hat = sony2arma21.pred$pred

sony2arma21hatL = sony2arma21hat-1.96*sig
lines(sony2arma21hatL, lty=1, col=4)
sony2arma21hatU = sony2arma21hat+1.96*sig
lines(sony2arma21hatU, lty=1, col=4)
## 単位根過程 ADF検定等々
sonyrate = diff(log(sony$Price))*100
utresult2= unitrootTest(sonyrate, type="c", lags=2)
ADF_result <- utresult2@test
ADF_result
xtable(ADF_result, label="tb-ref", caption="ADF検定の結果")

## GARCH
sonyrate

## VAR \href{https://qiita.com/saltcooky/items/2d0119ea4a10bab6cff2}
VARselect(sony[,2:7], lag.max = 18)
#VARモデルの推定
sony_var <- VAR(sony[,2:7],p=VARselect(sony[,2:7],lag.max=4)$selection[1])
sony_var
summary(sony_var)
causality(sony_var,cause="U")
sony_irf<-irf(sony_var,n.ahead=14,ci=0.95)
sony_irf
plot(sony_irf)


