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

DJI_Data  <- read.csv("DJI_Data.csv", header=TRUE)
Oil_Data  <- read_excel("Oil_Data.xls")
Gold_Data <- read_excel("Gold_Data.xlsx")

ggplot(DJI_Data, aes(x=Date, y=Close, group=1)) + geom_line() + ggtitle("Closing Price of DJ") + xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels="%b %y", date_breaks = "5 years")

DJI_Time  <- as.POSIXct(paste(DJI_Data$Date, DJI_Data$Close), format = "%Y-%m-%d")
Oil_Time  <- as.POSIXct(paste(Oil_Data$Date, Oil_Data$Close), format = "%Y-%m-%d")
Gold_Time <- as.POSIXct(paste(Gold_Data$Date, Gold_Data$Close), format = "%Y-%m-%d")

DJI  <- xts(x=DJI_Data$Close, order.by = DJI_Time)
Oil  <- xts(x=Oil_Data$Close, order.by = Oil_Time)
Gold <- xts(x=Gold_Data$Close, order.by = Gold_Time)

chartSeries(DJI)
chartSeries(Oil)
chartSeries(Gold)

rDJI <- dailyReturn(DJI)