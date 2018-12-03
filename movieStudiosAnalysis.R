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

#usar o TTR para calcular a banda inferior, superior, media movel e %B
bbands.SNE <- BBands(SNE[,"SNE.Adjusted"], sd = 2, n = 20)
bbands.CMCSA <- BBands(CMCSA[,"CMCSA.Adjusted"], sd = 2, n = 20)
bbands.FOX <- BBands(FOX[,"FOX.Adjusted"], sd = 2, n = 20)
bbands.DIS <- BBands(DIS[,"DIS.Adjusted"],sd = 2, n = 20)
#tratamos os dados para inserir a coluna contendo as datas e retiramos as primeiras vinte linhas
#já que usamos um periodo de 20 dias para a media movel
bbands.DIS <- cbind(index(bbands.DIS), data.frame(bbands.DIS), data.frame(DIS[,-c(5,6)]))
rownames(bbands.DIS) <- seq(1, nrow(bbands.DIS), 1)
names(bbands.DIS) <- paste(c("date", "dn", "mavg", "up", "pctB", "open","high","low","close"))
#bbands.DIS <- bbands.DIS[-c(1:20),]

bbands.SNE <- cbind(index(bbands.SNE), data.frame(bbands.SNE))
rownames(bbands.SNE) <- seq(1, nrow(bbands.SNE), 1)
names(bbands.SNE) <- paste(c("date", "dn", "mavg", "up", "pctB"))
bbands.SNE <- bbands.SNE[-c(1:20),]

bbands.CMCSA <- cbind(index(bbands.CMCSA), data.frame(bbands.CMCSA))
rownames(bbands.CMCSA) <- seq(1, nrow(bbands.CMCSA), 1)
names(bbands.CMCSA) <- paste(c("date", "dn", "mavg", "up", "pctB"))
bbands.CMCSA <- bbands.CMCSA[-c(1:20),]

bbands.FOX <- cbind(index(bbands.FOX), data.frame(bbands.FOX))
rownames(bbands.FOX) <- seq(1, nrow(bbands.FOX), 1)
names(bbands.FOX) <- paste(c("date", "dn", "mavg", "up", "pctB"))
bbands.FOX <- bbands.FOX[-c(1:20),]

#plotar o Bollinger Bands usando GGPlot2 e TidyQuant
#DIS
plotDIS <- ggplot(data = bbands.DIS, aes(x = date, y = close, open = open, close = close, low = low, high = high)) + geom_candlestick()#plotar a media movel
#plotDIS <- plotDIS + geom_line(mapping = aes(date, up), colour = "gray") #adicionar camada da banda superior
#plotDIS <- plotDIS + geom_line(mapping = aes(date, dn), colour = "gray") #adicionar camada da banda inferioi
plotDIS <- plotDIS + geom_bbands(ma_fun = EMA, sd = 2, n = 20, linetype = 2, size = 0.5, alpha = 0.2, fill = palette_light()[[1]], color_bands = palette_light()[[1]], color_ma = palette_light()[[2]])
plotDIS <- plotDIS + labs(title = "Bollinger Bands Disney", x = NULL, y = "Valor da Ação")
plotDIS <- plotDIS + theme_tq() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),plot.margin=unit(c(0.5,0.4,0,0.5), "cm"))
plotDIS <- plotDIS + scale_y_continuous()
plotDIS2 <- ggplot(data = bbands.DIS, aes(date, pctB)) + geom_line(colour = palette_light()[[1]])
plotDIS2 <- plotDIS2 + geom_hline(yintercept = 0:1, colour = "gray", linetype = 2,size = 0.5)
plotDIS2 <- plotDIS2 + labs(x = "Ano", y = "%B")
plotDIS2 <- plotDIS2 + theme_tq()+ theme(plot.margin=unit(c(-0.1,0.4,0.5,0.6), "cm"))
grid.arrange(plotDIS,plotDIS2, nrow = 2, heights=c(2/3, 1/3))

#FOX
plotDIS <- ggplot(data = bbands.FOX, aes(date, mavg)) + geom_line() #plotar a media movel
plotDIS <- plotDIS + geom_line(mapping = aes(date, up), colour = "gray") #adicionar camada da banda superior
plotDIS <- plotDIS + geom_line(mapping = aes(date, dn), colour = "gray") #adicionar camada da banda inferioi
plotDIS <- plotDIS + labs(title = "Bollinger Bands Fox", x = NULL, y = "Valor da Ação")
plotDIS <- plotDIS + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),plot.margin=unit(c(0.5,0.5,0,0.5), "cm"))
plotDIS2 <- ggplot(data = bbands.FOX, aes(date, pctB)) + geom_line()
plotDIS2 <- plotDIS2 + geom_hline(yintercept = 0:1, colour = "gray", linetype = 2,size = 0.5)
plotDIS2 <- plotDIS2 + labs(x = "Ano", y = "%B")
plotDIS2 <- plotDIS2 + theme(plot.margin=unit(c(0,0.5,0.5,0.2), "cm"))
grid.arrange(plotDIS,plotDIS2, nrow = 2, heights=c(2/3, 1/3))

#SNE
plotDIS <- ggplot(data = bbands.SNE, aes(date, mavg)) + geom_line() #plotar a media movel
plotDIS <- plotDIS + geom_line(mapping = aes(date, up), colour = "gray") #adicionar camada da banda superior
plotDIS <- plotDIS + geom_line(mapping = aes(date, dn), colour = "gray") #adicionar camada da banda inferioi
plotDIS <- plotDIS + labs(title = "Bollinger Bands Sony", x = NULL, y = "Valor da Ação")
plotDIS <- plotDIS + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),plot.margin=unit(c(0.5,0.4,0,0.5), "cm"))
plotDIS2 <- ggplot(data = bbands.SNE, aes(date, pctB)) + geom_line()
plotDIS2 <- plotDIS2 + geom_hline(yintercept = 0:1, colour = "gray", linetype = 2,size = 0.5)
plotDIS2 <- plotDIS2 + labs(x = "Ano", y = "%B")
plotDIS2 <- plotDIS2 + theme(plot.margin=unit(c(0,0.4,0.5,0.4), "cm"))
grid.arrange(plotDIS,plotDIS2, nrow = 2, heights=c(2/3, 1/3))

#CMCSA
plotDIS <- ggplot(data = bbands.CMCSA, aes(date, mavg)) + geom_line() #plotar a media movel
plotDIS <- plotDIS + geom_line(mapping = aes(date, up), colour = "gray") #adicionar camada da banda superior
plotDIS <- plotDIS + geom_line(mapping = aes(date, dn), colour = "gray") #adicionar camada da banda inferioi
plotDIS <- plotDIS + labs(title = "Bollinger Bands Comcast", x = NULL, y = "Valor da Ação")
plotDIS <- plotDIS + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),plot.margin=unit(c(0.5,0.4,0,0.5), "cm"))
plotDIS2 <- ggplot(data = bbands.CMCSA, aes(date, pctB)) + geom_line()
plotDIS2 <- plotDIS2 + geom_hline(yintercept = 0:1, colour = "gray", linetype = 2,size = 0.5)
plotDIS2 <- plotDIS2 + labs(x = "Ano", y = "%B")
plotDIS2 <- plotDIS2 + theme(plot.margin=unit(c(0,0.4,0.5,0.4), "cm"))
grid.arrange(plotDIS,plotDIS2, nrow = 2, heights=c(2/3, 1/3))