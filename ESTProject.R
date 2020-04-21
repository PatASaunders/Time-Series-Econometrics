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

###################################### PREPARING DATA ####################################################################

DJI_Data  <- read_excel("DJI_Data.xlsx")
Oil_Data  <- read_excel("Oil_Data.xls")
Gold_Data <- read_excel("Gold_Data.xlsx")

#A faire tourner les 3 ad.Dates deux fois, la première fois donne une erreur timezone
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

######################################## ECONOMETRICS #################################################################

##DJI

t      <- DJI_Data$Date
Ydji   <- DJI_Data$Close
#On remarque une claire tendance et hétéroscédasticité donc on fait la difference premiere

L.Ydji <- log(DJI_Data$Close)
DL.Ydji<- diff(L.Ydji)
L.Ydji <- L.Ydji[-length(L.Ydji)]
tdji   <- t[-length(t)]

ggplot() + geom_line(aes(tdji, DL.Ydji))

adf.test(Ydji, alternative="stationary", k=0)
adf.test(Ydji, alternative="explosive", k=0)

kpss.test(Ydji)
#nous rejetons l'hypothèse alternative, donc nous avons pas de stationnarité, confirmé par le test de KPSS,
#les 2 tests implique la présence d'une racine unitaire.

acf(DL.Ydji)
pacf(DL.Ydji)

arima(DL.Ydji, order=c(1,0,0))
arima(DL.Ydji, order=c(2,0,0))
arima(DL.Ydji, order=c(0,0,1))
arima(DL.Ydji, order=c(0,0,2))
arima(DL.Ydji, order=c(1,0,1))
arima(DL.Ydji, order=c(1,0,2))
arima(DL.Ydji, order=c(2,0,1))
arima(DL.Ydji, order=c(2,0,2))

summary(Ydji)
summary(D.Ydji)
summary(L.Ydji)

plot(t,L.Ydji)



