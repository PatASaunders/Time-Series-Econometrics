library(AER)
library(ggplot2)
library(readxl)
library(rugarch)
library(oce)
library(tseries)
library(forecast)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(PerformanceAnalytics)

############################################ NEW #########################################################################
######################################### PREPA DONNEE ##################################################################

#DJI_Data  <- read_excel("DJI_Data.xlsx")
#Oil_Data  <- read_excel("Oil_Data.xls")
#Gold_Data <- read_excel("Gold_Data.xlsx")

DJI_Data  <- read_excel("C:/Users/Clément/Desktop/EST/DJI_Data.xlsx")
Oil_Data  <- read_excel("C:/Users/Clément/Desktop/EST/Oil_Data.xls")
Gold_Data <- read_excel("C:/Users/Clément/Desktop/EST/Gold_Data.xlsx")

DJI_Data$Close  <- fillGap(DJI_Data$Close,  method=c("linear"))
Oil_Data$Close  <- fillGap(Oil_Data$Close,  method=c("linear"))
Gold_Data$Close <- fillGap(Gold_Data$Close, method=c("linear"))


qplot(x = DJI_Data$Date, y = DJI_Data$'Adj Close', geom='line') + geom_line(color='dark blue') +
  labs(x='Year', y='Dow Jones Industrial Index') + geom_hline(yintercept = mean(DJI_Data$'Adj Close'), color='red') +
  annotate("rect", xmin = as.POSIXct('2009-04-01'), xmax = as.POSIXct('2010-08-10'), ymin = -Inf, ymax = Inf,
           alpha = .2) +
  annotate("rect", xmin = as.POSIXct('2019-12-01'), xmax = as.POSIXct('2020-04-10'), ymin = -Inf, ymax = Inf,
           alpha = .2)

qplot(x = Oil_Data$Date, y = Oil_Data$'Close', geom='line') + geom_line(color='dark blue') +
  labs(x='Year', y='Oil') + geom_hline(yintercept = mean(Oil_Data$'Close'), color='red')

qplot(x = Gold_Data$Date, y = Gold_Data$'Close', geom='line') + geom_line(color='dark blue') +
  labs(x='Year', y='Gold') + geom_hline(yintercept = mean(Gold_Data$'Close'), color='red')

#On remarque une claire non-stationnartié en moyenne et en variance
#De ce fait nous allons faire la différence première stationnariser en moyenne.
#??yt=yt???y(t???1)

#Nous allons passer par une étude sur les rendments journaliers, exprimé de la manière suivante.

#rt=(pricet???price(t???1)) / price(t???1)

retsdji = diff(DJI_Data$'Adj Close')/DJI_Data$'Adj Close'[-length(DJI_Data$'Adj Close')]
datedji = DJI_Data$Date[-length(DJI_Data$Date)]

retsoil = diff(Oil_Data$'Close')/Oil_Data$'Close'[-length(Oil_Data$'Close')]
dateoil = Oil_Data$Date[-length(Oil_Data$Date)]

retsgold = diff(Gold_Data$'Close')/Gold_Data$'Close'[-length(Gold_Data$'Close')]
dategold = Gold_Data$Date[-length(Gold_Data$Date)]

###DJI
pdji1 = qplot(x=datedji, y=retsdji, geom='line') + 
  geom_line(color='dark blue') + 
  geom_hline(yintercept = mean(retsdji), color='red', size=1) + 
  labs(x='Date', y='Daily Returns on the DJI')

pdji2 = qplot(retsdji , geom = 'density') + coord_flip() + 
  geom_vline(xintercept = mean(retsdji) , color = 'red' , size = 1) +
  geom_density(fill = 'lightblue' , alpha = 0.4) + 
  labs(x = '')

grid.arrange(pdji1,pdji2, ncol=2)

###Oil
poil1 = qplot(x=dateoil, y=retsoil, geom='line') + 
  geom_line(color='dark blue') + 
  geom_hline(yintercept = mean(retsoil), color='red', size=1) + 
  labs(x='Date', y='Daily Returns on the oil')

poil2 = qplot(retsoil , geom = 'density') + coord_flip() + 
  geom_vline(xintercept = mean(retsoil) , color = 'red' , size = 1) +
  geom_density(fill = 'lightblue' , alpha = 0.4) + 
  labs(x = '')

grid.arrange(poil1,poil2, ncol=2)

###Gold
pgold1 = qplot(x=dategold, y=retsgold, geom='line') + 
  geom_line(color='dark blue') + 
  geom_hline(yintercept = mean(retsgold), color='red', size=1) + 
  labs(x='Date', y='Daily Returns on the gold')

pgold2 = qplot(retsgold , geom = 'density') + coord_flip() + 
  geom_vline(xintercept = mean(retsgold) , color = 'red' , size = 1) +
  geom_density(fill = 'lightblue' , alpha = 0.4) + 
  labs(x = '')

grid.arrange(poil1,poil2, ncol=2)

#########################################  STATIONNARITE ######################################################


#rlang::last_error()

#nous testons la non-stationnarité où l'hypothèse H0 indique une non-stationnarité
adf.test(retsdji)
adf.test(retsoil)
adf.test(retsgold)
#nous retrouvons une p-value < 0.01, donc la série est stationnaire

#Nous avons une série stationnaire donc nous pouvons passer par la méthodologie de Box-Jenkins


###################################### ARIMA ESTIMATION #########################################################


model.arima.dji = auto.arima(retsdji, max.order = c(5,0,5), stationary=TRUE, trace=T, ic='aic', allowmean
= FALSE )
model.arima.dji

model.arima.oil = auto.arima(retsoil, max.order = c(5,0,5), stationary=TRUE, trace=T, ic='aic', allowmean
= FALSE )
model.arima.oil

model.arima.gold = auto.arima(retsgold, max.order = c(5,0,5), stationary=TRUE, trace=T, ic='aic', allowmean
= FALSE )
model.arima.gold

################################ TESTS DE NORMALITE DES RESIDUS #####################################################


model.arima.dji$residuals %>% ggtsdisplay(plot.type='hist', lag.max=14)
model.arima.oil$residuals %>% ggtsdisplay(plot.type='hist', lag.max=14)
model.arima.gold$residuals %>% ggtsdisplay(plot.type='hist', lag.max=14)

#Nous remarquons clairement du volatility clustering dans les résidus du modèle ARIMA(1,0,1)
#Nous remarquons une forme tres leptokurtique des résidus par rapport à une loi N(0,sigmarésidus)

#Pour vérifier nous suppositions par rapport à l'autocorrélation présente dans les résidus, nous passons
#à un test de Ljung-Box

ar.res.dji = model.arima.dji$residuals
Box.test(model.arima.dji$residuals, lag=14, fitdf=2, type = 'Ljung-Box')

ar.res.oil = model.arima.oil$residuals
Box.test(model.arima.oil$residuals, lag=14, fitdf=2, type = 'Ljung-Box')

ar.res.gold = model.arima.gold$residuals
Box.test(model.arima.gold$residuals, lag=14, fitdf=2, type = 'Ljung-Box')

#Nous rejetons H0: Pas d'autocorrélation, il y a de l'autocorrélations dans les résidus de notre modèle ARIMA


###################################### GARCH ESTIMATION #############################################################


tsdisplay(ar.res.dji^2, main='Squared Residuals')
tsdisplay(ar.res.oil^2, main='Squared Residuals')
tsdisplay(ar.res.gold^2, main='Squared Residuals')

model.spec = ugarchspec(variance.model = list(model='sGARCH', garchOrder = c(1,1)),
                        mean.model=list(armaOrder=c(0,0)))

model.fit.dji = ugarchfit(spec=model.spec, data=ar.res.dji, solver='solnp')
model.fit.oil = ugarchfit(spec=model.spec, data=ar.res.oil, solver='solnp')
model.fit.gold = ugarchfit(spec=model.spec, data=ar.res.gold, solver='solnp')

options(scipen = 999)

model.fit.dji@fit$matcoef
model.fit.oil@fit$matcoef
model.fit.gold@fit$matcoef

#alpha et beta sont significativement différent de 0, donc nous pouvons supposer la volatilité des résidus varie
#beta < 1, donc l'effet des résidus diminu le plus de lag il y a.
#coefbeta^2  pour e(t-2), coefbeta^3 pour e(t-3)

#Value at Risk

VaR(retsdji, p=0.99, clean=c("none", "boudt"), method="historical")
VaR(retsdji, p=0.99, clean=c("none", "boudt"), method="modified")
VaR(retsdji, p=0.95, clean=c("none", "boudt"), method="historical")
VaR(retsdji, p=0.95, clean=c("none", "boudt"), method="modified")

VaR(retsoil, p=0.99, clean=c("none", "boudt"), method="historical")
VaR(retsoil, p=0.99, clean=c("none", "boudt"), method="modified")
VaR(retsoil, p=0.95, clean=c("none", "boudt"), method="historical")
VaR(retsoil, p=0.95, clean=c("none", "boudt"), method="modified")

VaR(retsgold, p=0.99, clean=c("none", "boudt"), method="historical")
VaR(retsgold, p=0.99, clean=c("none", "boudt"), method="modified")
VaR(retsgold, p=0.95, clean=c("none", "boudt"), method="historical")
VaR(retsgold, p=0.95, clean=c("none", "boudt"), method="modified")

qplot(retsdji, geom='histogram') + 
  geom_histogram(fill = 'lightblue', bins=30) +
  geom_histogram(aes(retsdji[retsdji<quantile(retsdji,0.05)]), fill = 'yellow', bins=30) +
  geom_histogram(aes(retsdji[retsdji<quantile(retsdji,0.01)]), fill = 'red', bins=30) +
  labs(x='Daily Returns')

qplot(retsdji, geom='histogram') + 
  geom_histogram(fill = 'lightblue', bins=30) +
  geom_histogram(aes(retsoil[retsoil<quantile(retsoil,0.05)]), fill = 'yellow', bins=30) +
  geom_histogram(aes(retsoil[retsoil<quantile(retsoil,0.01)]), fill = 'red', bins=30) +
  labs(x='Daily Returns')

qplot(retsdji, geom='histogram') + 
  geom_histogram(fill = 'lightblue', bins=30) +
  geom_histogram(aes(retsgold[retsgold<quantile(retsgold,0.05)]), fill = 'yellow', bins=30) +
  geom_histogram(aes(retsgold[retsgold<quantile(retsgold,0.01)]), fill = 'red', bins=30) +
  labs(x='Daily Returns')

#Pour estimer la VaR, nous devons proprement définer le quantile correspondant à la distribution supposé

#On effectu un test de Jarque Bera pour vérifier si les résidus sont distribué à partir d'une loi N

jarque.bera.test(retsdji)
jarque.bera.test(retsoil)
jarque.bera.test(retsgold)

###DJI
p2_1 = qplot(retsdji , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(retsdji))) , fill = 'green' , alpha = 0.25) + 
  labs(x = '')

p2_2 = qplot(retsdji , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(retsdji))) , fill = 'green' , alpha = 0.25) + 
  coord_cartesian(xlim = c(-0.15 , -0.02) , ylim = c(0 , 10)) + 
  geom_vline(xintercept = c(qnorm(p = c(0.01 , 0.05) , mean = mean(retsdji) , sd = sd(retsdji))) , 
             color = c('red' , 'yellow') , size = 1) + labs(x = 'Daily Returns')

grid.arrange(p2_1 , p2_2 , ncol = 1)

###Oil
p2_1 = qplot(retsoil , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(retsoil))) , fill = 'green' , alpha = 0.25) + 
  labs(x = '')

p2_2 = qplot(retsoil , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(retsoil))) , fill = 'green' , alpha = 0.25) + 
  coord_cartesian(xlim = c(-0.15 , -0.02) , ylim = c(0 , 10)) + 
  geom_vline(xintercept = c(qnorm(p = c(0.01 , 0.05) , mean = mean(retsdji) , sd = sd(retsoil))) , 
             color = c('red' , 'yellow') , size = 1) + labs(x = 'Daily Returns')

grid.arrange(p2_1 , p2_2 , ncol = 1)

###Gold
p2_1 = qplot(retsgold , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(retsgold))) , fill = 'green' , alpha = 0.25) + 
  labs(x = '')

p2_2 = qplot(retsgold , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(retsgold))) , fill = 'green' , alpha = 0.25) + 
  coord_cartesian(xlim = c(-0.15 , -0.02) , ylim = c(0 , 10)) + 
  geom_vline(xintercept = c(qnorm(p = c(0.01 , 0.05) , mean = mean(retsdji) , sd = sd(retsdji))) , 
             color = c('red' , 'yellow') , size = 1) + labs(x = 'Daily Returns')

grid.arrange(p2_1 , p2_2 , ncol = 1)

########################################## GARCH FORECAST ###################################################


model.fc.dji = ugarchforecast(model.fit, data=retsdji, n.ahead = 5000, n.start = 3135)
model.fc.oil = ugarchforecast(model.fit, data=retsoil, n.ahead = 5000, n.start = 3194)
model.fc.gold = ugarchforecast(model.fit, data=retsgold, n.ahead = 5000, n.start = 3421)

VaR95_td_dji = mean(retsdji) + model.fc.dji@forecast$sigmaFor*(-3)
VaR95_td_oil = mean(retsoil) + model.fc.oil@forecast$sigmaFor*(-3)
VaR95_td_gold = mean(retsgold) + model.fc.gold@forecast$sigmaFor*(-3)

p = c()
p[1] = pbinom(q = 0 , size = 250 , prob = 0.01)
for(i in 1:14){
  p[i] = (pbinom(q = (i-1) , size = 250 , prob = 0.01) - pbinom(q = (i-2) , size = 250 , prob = 0.01))
}
qplot(y = p , x = 1:14 , geom = 'line') + 
  scale_x_continuous(breaks = seq(0 , 14 , 2)) + 
  annotate('segment' , x = c(1 , 6) , xend = c(1 , 6) , y = c(0 , 0) , yend = p[c(1 , 6)] , color = 'red' , size = 1) + 
  labs(y = 'Probability' , x = 'Number of Exceptions') + 
  theme_light()

cat('Number of exceptions with GARCH approach: ' , (sum(retsdji[3135:8134] < VaR95_td_dji)) , sep = '')
cat('Number of exceptions with GARCH approach: ' , (sum(retsoil[3194:8193] < VaR95_td_oil)) , sep = '')
cat('Number of exceptions with GARCH approach: ' , (sum(retsgold[3421:8420] < VaR95_td_gold)) , sep = '')

qplot(y = VaR95_td_dji , x = 3135:8134 , geom = 'line') +
  geom_point(aes(x = 3135:8134 , y = retsdji[3135:8134] , color = as.factor(retsdji[3135:8134] < VaR95_td_dji)) , size = 1) + scale_color_manual(values = c('gray' , 'red')) + 
  labs(y = 'Daily Returns' , x = 'Test set Observation') + theme_light() + 
  theme(legend.position = 'none') +
  annotate("rect", xmin = 7826, xmax = 8134, ymin = -Inf, ymax = Inf,
           alpha = .2) +
  annotate("rect", xmin = 5689, xmax = 6452, ymin = -Inf, ymax = Inf,
           alpha = .2) +
  annotate("rect", xmin = 3930, xmax = 4226, ymin = -Inf, ymax = Inf,
           alpha = .2)

qplot(y = VaR95_td_oil , x = 3194:8193 , geom = 'line') +
  geom_point(aes(x = 3194:8193 , y = retsoil[3194:8193] , color = as.factor(retsoil[3194:8193] < VaR95_td_oil)) , size = 1) + scale_color_manual(values = c('gray' , 'red')) + 
  labs(y = 'Daily Returns' , x = 'Test set Observation') + theme_light() + 
  theme(legend.position = 'none') +
  annotate("rect", xmin = 7785, xmax = 8193, ymin = -Inf, ymax = Inf,
           alpha = .2) +
  annotate("rect", xmin = 5748, xmax = 6511, ymin = -Inf, ymax = Inf,
           alpha = .2) +
  annotate("rect", xmin = 3989, xmax = 4485, ymin = -Inf, ymax = Inf,
           alpha = .2)

qplot(y = VaR95_td_gold , x = 3421:8420 , geom = 'line') +
  geom_point(aes(x = 3421:8420 , y = retsgold[3421:8420] , color = as.factor(retsgold[3421:8420] < VaR95_td_gold)) , size = 1) + scale_color_manual(values = c('gray' , 'red')) + 
  labs(y = 'Daily Returns' , x = 'Test set Observation') + theme_light() + 
  theme(legend.position = 'none') +
  annotate("rect", xmin = 8112, xmax = 8420, ymin = -Inf, ymax = Inf,
           alpha = .2) +
  annotate("rect", xmin = 5975, xmax = 6738, ymin = -Inf, ymax = Inf,
           alpha = .2) +
  annotate("rect", xmin = 4216, xmax = 4712, ymin = -Inf, ymax = Inf,
           alpha = .2)

########################################## ROLLING FORECAST #############################################################


model.roll = ugarchroll(spec = model.spec , data = retsdji , n.start = 758 , refit.every = 50 ,
                        refit.window = 'moving')
VaR95_roll = mean(retsdji) + model.roll@forecast$density[,'Sigma']*(-3)

qplot(y = VaR95_roll , x = 759:8134 , geom = 'line') +
  geom_point(aes(x = 759:8134 , y = retsdji[759:8134] , color = as.factor(retsdji[759:8134] < VaR95_roll)) , size = 1) + scale_color_manual(values = c('gray' , 'red')) + 
  labs(y = 'Daily Returns' , x = 'Test set Observation') + theme_light() + 
  theme(legend.position = 'none')
