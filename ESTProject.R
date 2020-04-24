library(AER)
library(ggplot2)
library(dynlm)
library(fGarch)
library(rugarch)
library(rmgarch)
library(quantmod)
library(scales)
library(vars)
library(xts)
library(readxl)
library(tsibble)
library(dplyr)
library(padr)
library(oce)
library(tseries)
library(forecast)
library(vars)
library(aTSA)
library(PerformanceAnalytics)
library(Quandl)

###################################### PREPARING DATA ####################################################################

DJI_Data  <- read_excel("DJI_Data.xlsx")
Oil_Data  <- read_excel("Oil_Data.xls")
Gold_Data <- read_excel("Gold_Data.xlsx")

#A faire tourner les 3 as.Dates deux fois, la première fois donne une erreur timezone
DJI_Data$Date  <- as.Date(DJI_Data$Date, "%Y-%m-%d")
Oil_Data$Date  <- as.Date(Oil_Data$Date, "%Y-%m-%d")
Gold_Data$Date <- as.Date(Gold_Data$Date,"%Y-%m-%d")

DJI_Data$Date %>% get_interval()

DJI_Data  <- DJI_Data  %>% pad()
Oil_Data  <- Oil_Data  %>% pad()
Gold_Data <- Gold_Data %>% pad()

#interpolation linéaire entre les valeurs NA et leurs bornes
DJI_Data$Close  <- fillGap(DJI_Data$Close,  method=c("linear"))
Oil_Data$Close  <- fillGap(Oil_Data$Close,  method=c("linear"))
Gold_Data$Close <- fillGap(Gold_Data$Close, method=c("linear"))

DJI_Data <- DJI_Data[c('Date', 'Close')]

DJI_Data$Date  <- as.POSIXct(paste(DJI_Data$Date,  DJI_Data$Close),  format = "%Y-%m-%d")
Oil_Data$Date  <- as.POSIXct(paste(Oil_Data$Date,  Oil_Data$Close),  format = "%Y-%m-%d")
Gold_Data$Date <- as.POSIXct(paste(Gold_Data$Date, Gold_Data$Close), format = "%Y-%m-%d")

DJI  <- xts(x=DJI_Data$Close,  order.by = DJI_Data$Date)
Oil  <- xts(x=Oil_Data$Close,  order.by = Oil_Data$Date)
Gold <- xts(x=Gold_Data$Close, order.by = Gold_Data$Date)

chartSeries(DJI)
chartSeries(Oil)
chartSeries(Gold)






######################################## ARMA #################################################################

##DJI

t      <- DJI_Data$Date
Ydji   <- DJI_Data$Close
#On remarque une claire tendance et hétéroscédasticité donc on fait la difference premiere

L.Ydji <- log(DJI_Data$Close)
DL.Ydji<- diff(L.Ydji)
D.Ydji <- diff(Ydji)
L.Ydji <- L.Ydji[-length(L.Ydji)]
tdji   <- t[-length(t)]

ggplot() + geom_line(aes(tdji, DL.Ydji))
ggplot() + geom_line(aes(t, L.Ydji))

adf.test(Ydji, alternative="stationary", k=0)
adf.test(Ydji, alternative="explosive", k=0)

auto.arima(L.Ydji)
arima(L.Ydji, order=c(2,1,0))

kpss.test(Ydji)
#nous rejetons l'hypothèse alternative, donc nous avons pas de stationnarité, confirmé par le test de KPSS,
#les 2 tests implique la présence d'une racine unitaire.

acf(L.Ydji)
pacf(L.Ydji)

arima(L.Ydji, order=c(1,1,0))
arima(L.Ydji, order=c(2,1,0))
arima(L.Ydji, order=c(0,1,1))
arima(L.Ydji, order=c(0,1,2))
arima(L.Ydji, order=c(1,1,1))
arima(L.Ydji, order=c(1,1,2))
arima(L.Ydji, order=c(2,1,1))
arima(L.Ydji, order=c(2,1,2))

test    <- arima(L.Ydji, order=c(2,1,0))
armaDJI <- arima(L.Ydji, order=c(2,1,0))
armaDJI
ArmaDJI <- Arima(L.Ydji, order=c(2,1,0))
ArmaDJI

acf(resid(test))

jarque.bera.test(resid(armaDJI))
Box.test(resid(armaDJI), type="Ljung", lag=20)
arch.test(armaDJI)
#on remarque une claire structure reste présente dans les résidus du modèle ARMA, nous retrouvons pas un bruit blanc
#Donc nous allons nous réfugier aux modèles GARCH

#Les résidus de notre processus générateur des rendements de notre séries ne sont pas un Bruit Blanc, il y a une structure
#qui reste à modéliser dans les résidus de ce modèle.







####################################### UGARCH ###############################################

rDJI  <- dailyReturn(log(DJI))
rOil  <- dailyReturn(log(Oil))
rGold <- dailyReturn(log(Gold))

rX <- data.frame(rDJI,rOil,rGold)

names(rX)[1] <- "rDJI"
names(rX)[2] <- "rOil"
names(rX)[3] <- "rGold"

ug_spec1 <- ugarchspec(mean.model = list(armaOrder=c(2,0)))
ug_spec2 <- ugarchspec(mean.model = list(armaOrder=c(1,0)))
ug_spec3 <- ugarchspec(mean.model = list(armaOrder=c(3,0)))
ug_spec

ugfit <- ugarchfit(spec=ug_spec1, data=rDJI)
ugfit2 <- ugarchfit(spec=ug_spec2, data=rDJI)
ugfit3 <- ugarchfit(spec=ug_spec3, data=rDJI)
ugfit
ugfit2
ugfit3

#GARCH Avec ARMA(2,0) minimise le crirtère d'akaike comme trouvé précédamment

names(ugfit@model)
names(ugfit@fit)

ugfit@fit$coef
ug_var <- ugfit@fit$var
ug_res <- ugfit@fit$residuals
ug_res2 <- (ugfit@fit$residuals)^2

ggplot() + geom_line(aes(t, ug_res2), color="black") + geom_line(aes(t, ug_var), color="red")

ggplot() + geom_histogram(aes())

hist(ugfit@fit$residuals, freq=FALSE)
curve(dnorm(x, mean=0, sd = sqrt(var(ugfit@fit$residuals))), col="red", lwd=2, add=TRUE)

VaR(rDJI, p=0.95, method="historical")



########################################## VaR ################################################

rDJ <- dailyReturn(DJI)
rAU <- dailyReturn(Gold)
rDJ <- dailyReturn(Oil)

VaR(rDJ, p=0.95, method="historical")
VaR(rDJ, p=0.99, method="historical")

CVaR(rDJ, p=0.99, method="historical")




####################################### POST 1998 #############################################

dji90 <- subset(DJI_Data, Date > '1998-12-31')
View(dji90)

ggplot() + geom_line(aes(dji90$Date, dji90$Close))

t90      <- dji90$Date
Ydji90   <- dji90$Close

L.Ydji90 <- log(log(Ydji90))
DL.Ydji90 <- diff(log(Ydji90))
D.Ydji90  <- diff(Ydji90)
tdji90   <- t90[-length(t90)]

ggplot() + geom_line(aes(tdji90, D.Ydji90))

acf(D.Ydji90)
pacf(D.Ydji90)

auto.arima(L.Ydji90)

arima(DL.Ydji90, order=c(0,0,0))
arima(DL.Ydji90, order=c(1,0,0))
arima(DL.Ydji90, order=c(2,0,0))
arima(DL.Ydji90, order=c(0,0,1))
arima(DL.Ydji90, order=c(0,0,2))
arima(DL.Ydji90, order=c(1,0,1))
arima(DL.Ydji90, order=c(1,0,2))
arima(DL.Ydji90, order=c(2,0,1))
arima(DL.Ydji90, order=c(2,0,2))












