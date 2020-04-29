library(AER)
library(ggplot2)
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
DJI_Data  <- read_excel("DJI_Data.xlsx")
Oil_Data  <- read_excel("Oil_Data.xls")
Gold_Data <- read_excel("Gold_Data.xlsx")

DJI_Data$Close  <- fillGap(DJI_Data$Close,  method=c("linear"))
Oil_Data$Close  <- fillGap(Oil_Data$Close,  method=c("linear"))
Gold_Data$Close <- fillGap(Gold_Data$Close, method=c("linear"))

qplot(x = DJI_Data$Date, y = DJI_Data$'Adj Close', geom='line') + geom_line(color='dark blue') +
  labs(x='Year', y='Price ($)', title='Dow Jones Industrial Index') + geom_hline(yintercept = mean(DJI_Data$'Adj Close'), color='red')

#On remarque une claire non-stationnartié en moyenne et en variance
#De ce fait nous allons faire la différence première stationnariser en moyenne.
#Δyt=yt−y(t−1)

#Nous allons passer par une étude sur les rendments journaliers, exprimé de la manière suivante.

#rt=(pricet−price(t−1)) / price(t−1)

retsdji = diff(DJI_Data$'Adj Close')/DJI_Data$'Adj Close'[-length(DJI_Data$'Adj Close')]
datedji = DJI_Data$Date[-length(DJI_Data$Date)]

pdji1 = qplot(x=datedji, y=retsdji, geom='line') + 
        geom_line(color='dark blue') + 
        geom_hline(yintercept = mean(retsdji), color='red', size=1) + 
        labs(x='Date', y='Daily Returns on the DJI')

pdji2 = qplot(retsdji , geom = 'density') + coord_flip() + 
        geom_vline(xintercept = mean(retsdji) , color = 'red' , size = 1) +
        geom_density(fill = 'lightblue' , alpha = 0.4) + 
        labs(x = '')

grid.arrange(pdji1,pdji2, ncol=2)


#########################################  STATIONNARITE ######################################################


#rlang::last_error()

#nous testons la non-stationnarité où l'hypothèse H0 indique une non-stationnarité
adf.test(retsdji)
#nous retrouvons une p-value < 0.01, donc la série est stationnaire

#Nous avons une série stationnaire donc nous pouvons passer par la méthodologie de Box-Jenkins


###################################### ARIMA ESTIMATION #########################################################


model.arima = auto.arima(retsdji, max.order = c(5,0,5), stationary=TRUE, trace=T, ic='aic', allowmean = F)

model.arima

#Si notre modèle est correcte, avec des résidus iid et donc un bruit blanc, le processus générateur de la série est donc: 

#rt = 0.0909*r(t-1) + et + 0.1735e(t-1)   A VERIFIER!!!!!!!!!


################################ TESTS DE NORMALITE DES RESIDUS #####################################################


model.arima$residuals %>% ggtsdisplay(plot.type='hist', lag.max=14)

tsdisplay((model.arima$residuals), main='Residuals')

#Nous remarquons clairement du volatility clustering dans les résidus du modèle ARIMA(1,0,1)
#Nous remarquons une forme tres leptokurtique des résidus par rapport à une loi N(0,sigmarésidus)

#Pour vérifier nous suppositions par rapport à l'autocorrélation présente dans les résidus, nous passons
#à un test de Ljung-Box

ar.res = model.arima$residuals

Box.test(model.arima$residuals, lag=14, fitdf=2, type = 'Ljung-Box')

#Nous rejetons H0: Pas d'autocorrélation, il y a de l'autocorrélations dans les résidus de notre modèle ARIMA


###################################### GARCH ESTIMATION #############################################################


tsdisplay(ar.res^2, main='Residuals Squared')

model.spec = ugarchspec(variance.model = list(model='sGARCH', garchOrder = c(1,1)),
                        mean.model=list(armaOrder=c(0,0)))

model.fit = ugarchfit(spec=model.spec, data=ar.res, solver='solnp')

options(scipen = 999)

model.fit@fit$matcoef

#alpha et beta sont significativement différent de 0, donc nous pouvons supposer la volatilité des résidus varie
#beta < 1, donc l'effet des résidus diminu le plus de lag il y a.
#coefbeta^2  pour e(t-2), coefbeta^3 pour e(t-3)

#Value at Risk

VaR(retsdji, p=0.99, clean=c("none", "boudt"), method="historical")
VaR(retsdji, p=0.95, clean=c("none", "boudt"), method="historical")

qplot(retsdji, geom='histogram') + 
  geom_histogram(fill = 'lightblue', bins=30) +
  geom_histogram(aes(retsdji[retsdji<quantile(retsdji,0.05)]), fill = 'yellow', bins=30) +
  geom_histogram(aes(retsdji[retsdji<quantile(retsdji,0.01)]), fill = 'red', bins=30) +
  labs(x='Daily Returns')

#Pour estimer la VaR, nous devons proprement définer le quantile correspondant à la distribution supposé

#On effectu un test de Jarque Bera pour vérifier si les résidus sont distribué à partir d'une loi N

jarque.bera.test(retsdji)

p2_1 = qplot(retsdji , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(retsdji))) , fill = 'green' , alpha = 0.25) + 
  labs(x = '')

p2_2 = qplot(retsdji , geom = 'density') + geom_density(fill = 'blue' , alpha = 0.4) + 
  geom_density(aes(rnorm(200000 , 0 , sd(retsdji))) , fill = 'green' , alpha = 0.25) + 
  coord_cartesian(xlim = c(-0.15 , -0.02) , ylim = c(0 , 10)) + 
  geom_vline(xintercept = c(qnorm(p = c(0.01 , 0.05) , mean = mean(retsdji) , sd = sd(retsdji))) , 
             color = c('red' , 'yellow') , size = 1) + labs(x = 'Daily Returns')

grid.arrange(p2_1 , p2_2 , ncol = 1)


########################################## GARCH FORECAST ###################################################


model.fc = ugarchforecast(model.fit, data=retsdji, n.ahead = 5000, n.start = 3135)

VaR95_td = mean(retsdji) + model.fc@forecast$sigmaFor*(-3)

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

cat('Number of exceptions with GARCH approach: ' , (sum(retsdji[3135:8134] < VaR95_td)) , sep = '')

qplot(y = VaR95_td , x = 3135:8134 , geom = 'line') +
  geom_point(aes(x = 3135:8134 , y = retsdji[3135:8134] , color = as.factor(retsdji[3135:8134] < VaR95_td)) , size = 1) + scale_color_manual(values = c('gray' , 'red')) + 
  labs(y = 'Daily Returns' , x = 'Test set Observation') + theme_light() + 
  theme(legend.position = 'none')


########################################## ROLLING FORECAST #############################################################


model.roll = ugarchroll(spec = model.spec , data = retsdji , n.start = 200 , refit.every = 50 ,
                        refit.window = 'moving')
VaR95_roll = mean(retsdji) + model.roll@forecast$density[,'Sigma']*(-3)

qplot(y = VaR95_roll , x = 201:8134 , geom = 'line') +
  geom_point(aes(x = 201:8134 , y = retsdji[201:8134] , color = as.factor(retsdji[201:8134] < VaR95_roll)) , size = 1) + scale_color_manual(values = c('gray' , 'red')) + 
  labs(y = 'Daily Returns' , x = 'Test set Observation') + theme_light() + 
  theme(legend.position = 'none')

