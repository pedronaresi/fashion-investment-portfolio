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

bbands.SNE <- cbind(index(bbands.SNE), data.frame(bbands.SNE[,4]), data.frame(SNE[,-5]))
rownames(bbands.SNE) <- seq(1, nrow(bbands.SNE), 1)
names(bbands.SNE) <- paste(c("date", "pctB", "open","high","low","close","adjusted"))
bbands.SNE <- bbands.SNE[-c(1:20),]

bbands.CMCSA <- cbind(index(bbands.CMCSA), data.frame(bbands.CMCSA[,4]), data.frame(CMCSA[,-5]))
rownames(bbands.CMCSA) <- seq(1, nrow(bbands.CMCSA), 1)
names(bbands.CMCSA) <- paste(c("date", "pctB", "open","high","low","close","adjusted"))
bbands.CMCSA <- bbands.CMCSA[-c(1:20),]

bbands.FOX <- cbind(index(bbands.FOX), data.frame(bbands.FOX[,4]), data.frame(FOX[,-5]))
rownames(bbands.FOX) <- seq(1, nrow(bbands.FOX), 1)
names(bbands.FOX) <- paste(c("date", "pctB", "open","high","low","close","adjusted"))
bbands.FOX <- bbands.FOX[-c(1:20),]

bbands.DIS <- cbind(index(bbands.DIS), data.frame(bbands.DIS[,4]), data.frame(DIS[,-5]))
rownames(bbands.DIS) <- seq(1, nrow(bbands.DIS), 1)
names(bbands.DIS) <- paste(c("date", "pctB", "open","high","low","close","adjusted"))
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
MSFT.mm <- ggplot(data = bbands.MSFT, aes(x = date, y = close)) + geom_line() + theme_tq()
MSFT.mm <- MSFT.mm + geom_ma(ma_fun = SMA, n  = 50, na.rm = T, linetype = 1, colour = "Turquoise 1")
MSFT.mm <- MSFT.mm + geom_ma(ma_fun = SMA, n = 200, na.rm = T, linetype = 1, colour = "darkorchid1")
MSFT.mm + labs(title = "Média Móvel MSFT", x = "Ano", y = "Valor da Ação")

#bollinger bands
MSFT.bband <- ggplot(data = bbands.MSFT, aes(x = date, y = close, open = open, close = close, low = low, high = high)) + geom_line(size = 1, alpha  = 0.5)
MSFT.bband <- MSFT.bband + geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 2, size = 0.5, alpha = 0.2, fill = palette_light()[[1]], color_bands = palette_light()[[1]], color_ma = palette_light()[[2]])
MSFT.bband <- MSFT.bband + labs(title = "Bollinger Bands MSFT", x = NULL, y = "Valor da Ação")
MSFT.bband <- MSFT.bband + theme_tq() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
MSFT.bband <- MSFT.bband + scale_y_continuous()
MSFT.pctB <- ggplot(data = bbands.MSFT, aes(date, pctB)) + geom_line(colour ="darkgreen", size = 0.5)
MSFT.pctB <- MSFT.pctB + labs(x = "Ano", y = "%B")
MSFT.pctB <- MSFT.pctB + theme_tq()
grid.arrange(MSFT.bband,MSFT.pctB, nrow = 2, heights=c(2/3, 1/3))


#momentum
momentum.FOX <- RSI(FOX$FOX.Close, n = 14)
momentum.FOX <- cbind(index(momentum.FOX), data.frame(momentum.FOX))
rownames(bbands.FOX) <- seq(1, nrow(bbands.FOX), 1)
momentum.FOX <- momentum.FOX[-c(1:14),]
names(momentum.FOX) <- paste(c("date","Momentum"))

FOX.momentum <- ggplot(data = momentum.FOX, aes (x = date, y = Momentum)) + geom_line() + theme_tq() + ylim(0, 100)
FOX.momentum + labs(title = "Momentum FOX",x = "Ano", y = "Momentum") 

Tb <- tq_get(c("ATVI","EA","MSFT","TTWO"), get = "stock.prices", complete_cases = TRUE, from="2014-01-01", to="2017-01-01")

# Get returns for individual stock components
mReturn <- "^GSPC" %>% 
  tq_get(get  = "stock.prices", from="2014-01-01", to="2017-01-01") %>%
  tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Rb")

monthly_returns_stocks <- Tb %>%
  group_by(symbol) %>%
  tq_transmute(adjusted, periodReturn, period = "monthly")

weights_df <- tibble(symbol = c("ATVI", "EA", "MSFT","TTWO"), weights = c(0.10, 0.40, 0.25, 0.25))
pReturn <- tq_portfolio(data = monthly_returns_stocks, assets_col = symbol, returns_col = monthly.returns, weights = weights_df, col_rename = "Ra", wealth.index = FALSE)

RaRb <- left_join(pReturn, mReturn, by = c("date" = "date"))
retorno <- ggplot(data = RaRb, aes(x = date, y = portfolio.returns)) + geom_line(colour ="darkorchid1")
retorno <- retorno + geom_line(aes(x = date, y = Rb), colour = "red")
retorno

Var <- RaRb%>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = VaR)

#multiplos portfolios
stock_returns_monthly <- c("SNE", "FOX", "DIS","CMCSA") %>%
  tq_get(get  = "stock.prices",
         from="2014-01-01", to="2017-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "monthly", col_rename = "Ra")

baseline_returns_monthly <- "^GSPC" %>%
  tq_get(get  = "stock.prices",
         from="2014-01-01", to="2017-01-01") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")

stock_returns_monthly_multi <- stock_returns_monthly %>%
  tq_repeat_df(n = 3)
stock_returns_monthly_multi

weights <- c(
  0.20, 0.15, 0.50, 0.15,
  0.15, 0.15, 0.20, 0.50,
  0.25, 0.25, 0.25, 0.25
)
stocks <- c("SNE", "FOX", "DIS","CMCSA")
weights_table <-  tibble(stocks) %>%
  tq_repeat_df(n = 3) %>%
  bind_cols(tibble(weights)) %>%
  group_by(portfolio)
weights_table

portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = weights_table, 
               col_rename  = "Ra")
portfolio_returns_monthly_multi

RaRb_multiple_portfolio <- left_join(portfolio_returns_monthly_multi, 
                                     baseline_returns_monthly,
                                     by = "date")
RaRb_multiple_portfolio

portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra, 
               weights      = weights_table, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth)
portfolio_growth_monthly_multi %>%
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio))) +
  geom_line(size = 2) +
  labs(title = "Portfolio Growth",
       subtitle = "Comparing Multiple Portfolios",
       caption = "Portfolio 3 is a Standout!",
       x = "", y = "Portfolio Value",
       color = "Portfolio") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)


### retorno
wts <- c(0.25, 0.25, 0.25, 0.25)
portfolio_returns_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = wts, 
               col_rename  = "Ra")

Rb <- "^GSPC" %>% 
  tq_get(get  = "stock.prices", from="2014-01-01", to="2017-01-01") %>%
  tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Rb")
portfolio_returns_monthly <- left_join(portfolio_returns_monthly, Rb, by = c("date" = "date"))

portfolio_returns_monthly %>%
  ggplot(aes(x = date, y = Ra)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]], alpha = 0.5) +
  geom_bar(aes(x = date, y= Rb), stat = "identity", fill = palette_light()[[2]], alpha = 0.5) +
  labs(title = "Portfolio Returns",
       subtitle = "25% ATVI, 25% TTWO, 25% EA and 25% MSFT",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

###CAPM

CAPM <- RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
CAPM

RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = VaR)
Sharpe<-RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = SharpeRatio)
Sharpe

RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns)
