#incluir bibliotecas
library(xts)
library(quantmod)
library(ggplot2)
library(TTS)
library(knitr)
#carregar dados
getSymbols("SNE", from="2014-01-01", to="2017-01-01")
getSymbols("UVV", from="2014-01-01", to="2017-01-01")
getSymbols("FOX", from="2014-01-01", to="2017-01-01")
getSymbols("DIS", from="2014-01-01", to="2017-01-01")
#etapa 3
bbands.SNE<-BBands(SNE[,c("SNE.High","SNE.Low","SNE.Close")], sd = 2, n = 20)
bbands.UVV<-BBands(UVV[,c("UVV.High","UVV.Low","UVV.Close")], sd = 2, n = 20)
bbands.FOX<-BBands(FOX[,c("FOX.High","FOX.Low","FOX.Close")], sd = 2, n = 20)
bbands.DIS<-BBands(DIS[,c("DIS.High","DIS.Low","DIS.Close")], sd = 2, n = 20)

bbands.SNEAdjusted <- BBands(SNE[,"SNE.Adjusted"], sd = 2, n = 20)
bbands.UVVAdjusted <- BBands(UVV[,"UVV.Adjusted"], sd = 2, n = 20)
bbands.FOXAdjusted <- BBands(FOX[,"FOX.Adjusted"], sd = 2, n = 20)
bbands.DISAdjusted <- BBands(DIS[,"DIS.Adjusted"], sd = 2, n = 20)

bbands.DIS<-cbind(index(bbands.DIS),data.frame(bbands.DIS))
rownames(bbands.DIS)<-seq(1,nrow(bbands.DIS),1)
names(bbands.DIS)<-paste(c("date", "dn", "mavg", "up", "pctB"))
bbands.DIS<-bbands.DIS[-c(1:20),]

ggplot(data = bbands.DIS, aes(x = date, y = mavg)) + geom_line(color = "red") + geom_line(mapping = aes(x = date, y = up)) + geom_line(mapping = aes(x = date, y = dn))
