#instalar pacotes
#install.packages(c("xts","quantmod","ggplot2","TTR","knitr","tidyquant","gridExtra"))
#Ativar os pacotes
library(xts)
library(quantmod)
library(ggplot2)
library(TTR)
library(knitr)
library(tidyquant)
library(gridExtra)
#carregar dados
getSymbols("SNE", from="2014-01-01", to="2017-01-01")
getSymbols("CMCSA", from="2014-01-01", to="2017-01-01")
getSymbols("FOX", from="2014-01-01", to="2017-01-01")
getSymbols("DIS", from="2014-01-01", to="2017-01-01")

getSymbols("TTWO", from="2014-01-01", to="2017-01-01")
getSymbols("EA", from="2014-01-01", to="2017-01-01")
getSymbols("ATVI", from="2014-01-01", to="2017-01-01")
getSymbols("MSFT", from="2014-01-01", to="2017-01-01")

bbands.SNE <- BBands(SNE[,6], sd = 2, n = 20)
bbands.CMCSA <- BBands(CMCSA[,6], sd = 2, n = 20)
bbands.FOX <- BBands(FOX[,6], sd = 2, n = 20)
bbands.DIS <- BBands(DIS[,6], sd = 2, n = 20)

bbands.TTWO <- BBands(TTWO[,6], sd = 2, n = 20)
bbands.EA <- BBands(EA[,6], sd = 2, n = 20)
bbands.ATVI <- BBands(ATVI[,6], sd = 2, n = 20)
bbands.MSFT <- BBands(MSFT[,6], sd = 2, n = 20)

bbands.SNE <- cbind(index(bbands.SNE), data.frame(bbands.SNE), data.frame(SNE[,-5]))
rownames(bbands.SNE) <- seq(1, nrow(bbands.SNE), 1)
names(bbands.SNE) <- paste(c("date", "dn", "mavg", "up", "pctB", "open","high","low","close","adjusted"))
bbands.SNE <- bbands.SNE[-c(1:20),]

bbands.CMCSA <- cbind(index(bbands.CMCSA), data.frame(bbands.CMCSA), data.frame(CMCSA[,-5]))
rownames(bbands.CMCSA) <- seq(1, nrow(bbands.CMCSA), 1)
names(bbands.CMCSA) <- paste(c("date", "dn", "mavg", "up", "pctB", "open","high","low","close","adjusted"))
bbands.CMCSA <- bbands.CMCSA[-c(1:20),]

bbands.FOX <- cbind(index(bbands.FOX), data.frame(bbands.FOX), data.frame(FOX[,-5]))
rownames(bbands.FOX) <- seq(1, nrow(bbands.FOX), 1)
names(bbands.FOX) <- paste(c("date", "dn", "mavg", "up", "pctB", "open","high","low","close","adjusted"))
bbands.FOX <- bbands.FOX[-c(1:20),]

bbands.DIS <- cbind(index(bbands.DIS), data.frame(bbands.DIS), data.frame(DIS[,-5]))
rownames(bbands.DIS) <- seq(1, nrow(bbands.DIS), 1)
names(bbands.DIS) <- paste(c("date", "dn", "mavg", "up", "pctB", "open","high","low","close","adjusted"))
bbands.DIS <- bbands.DIS[-c(1:20),]

bbands.TTWO <- cbind(index(bbands.TTWO), data.frame(bbands.TTWO), data.frame(TTWO[,-5]))
rownames(bbands.TTWO) <- seq(1, nrow(bbands.TTWO), 1)
names(bbands.TTWO) <- paste(c("date", "dn", "mavg", "up", "pctB", "open","high","low","close","adjusted"))
bbands.TTWO <- bbands.TTWO[-c(1:20),]

bbands.EA <- cbind(index(bbands.EA), data.frame(bbands.EA), data.frame(EA[,-5]))
rownames(bbands.EA) <- seq(1, nrow(bbands.EA), 1)
names(bbands.EA) <- paste(c("date", "dn", "mavg", "up", "pctB", "open","high","low","close","adjusted"))
bbands.EA <- bbands.EA[-c(1:20),]

bbands.ATVI <- cbind(index(bbands.ATVI), data.frame(bbands.ATVI), data.frame(ATVI[,-5]))
rownames(bbands.ATVI) <- seq(1, nrow(bbands.ATVI), 1)
names(bbands.ATVI) <- paste(c("date", "dn", "mavg", "up", "pctB", "open","high","low","close","adjusted"))
bbands.ATVI <- bbands.ATVI[-c(1:20),]

bbands.MSFT <- cbind(index(bbands.MSFT), data.frame(bbands.MSFT), data.frame(MSFT[,-5]))
rownames(bbands.MSFT) <- seq(1, nrow(bbands.MSFT), 1)
names(bbands.MSFT) <- paste(c("date", "dn", "mavg", "up", "pctB", "open","high","low","close","adjusted"))
bbands.MSFT <- bbands.MSFT[-c(1:20),]

#media movel
MSFT.mm <- ggplot(data = bbands.MSFT, aes(x = date, y = close)) + geom_candlestick(aes(x = date, open = open, high = high,low = low,close = close), color_up = "gray30", color_down = "gray30", fill_up = "green3", fill_down = "red") + theme_tq()
MSFT.mm <- MSFT.mm + geom_ma(ma_fun = SMA, n  = 50, na.rm = T, linetype = 1, colour = "Turquoise 1")
MSFT.mm <- MSFT.mm + geom_ma(ma_fun = SMA, n = 200, na.rm = T, linetype = 1, colour = "darkorchid1")
MSFT.mm + labs(title = "Média Móvel MSFT", x = "Ano", y = "Valor da Ação")

#bollinger bands
MSFT.bband <- ggplot(data = bbands.MSFT, aes(x = date, y = close, open = open, close = close, low = low, high = high)) + geom_candlestick(color_up = "gray30", color_down = "gray30", fill_up = "green3", fill_down = "red")#plotar a media movel
#MSFT.bband <- MSFT.bband + geom_line(mapping = aes(date, up), colour = "gray") #adicionar camada da banda superior
#MSFT.bband <- MSFT.bband + geom_line(mapping = aes(date, dn), colour = "gray") #adicionar camada da banda inferioi
MSFT.bband <- MSFT.bband + geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 2, size = 0.5, alpha = 0.2, fill = palette_light()[[1]], color_bands = palette_light()[[1]], color_ma = palette_light()[[2]])
MSFT.bband <- MSFT.bband + labs(title = "Bollinger Bands MSFT", x = NULL, y = "Valor da Ação")
MSFT.bband <- MSFT.bband + theme_tq() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
MSFT.bband <- MSFT.bband + scale_y_continuous()
MSFT.pctB <- ggplot(data = bbands.MSFT, aes(date, pctB)) + geom_line(colour ="darkgreen", size = 0.5)
MSFT.pctB <- MSFT.pctB + labs(x = "Ano", y = "%B")
MSFT.pctB <- MSFT.pctB + theme_tq()
grid.arrange(MSFT.bband,MSFT.pctB, nrow = 2, heights=c(2/3, 1/3))