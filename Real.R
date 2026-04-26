# Finanças Internacionais e Crises - Bruna, Luíza, Kevin,  Marcos e Caio



# Biblioteca -------------------------------------------------------------------
rm(list = ls())
cat('\014')
#install.packages("tseries")
#install.packages("urca")
#install.packages("dynlm")


library("readxl")
library(tseries)
library(urca)
library(mFilter)
library(lmtest)
library(dplyr)
library(zoo)
library(dynlm)
library(stargazer)


# Base -------------------------------------------------------------------
df <- read_excel("C:/Users/KEVIN/Downloads/Base_Junta_Real.xlsx")

df <- df[13:93, ]

# Gerando as variaveis
cambior   = ts(df$CAMBIOREAL, start=c(2005,1),end=c(2025,1),frequency=4)
cds       = ts(df$CDS_5_ANOS, start=c(2005,1),end=c(2025,1),frequency=4)
passe     = ts(df$PASSEXTERNO, start=c(2005,1),end=c(2025,1),frequency=4)
tots      = ts(df$TOTL, start=c(2005,1),end=c(2025,1),frequency=4)
vix       = ts(df$VIXFINAL, start=c(2005,1),end=c(2025,1),frequency=4)
div       = ts(df$DIVBRUTA, start=c(2005,1),end=c(2025,1),frequency=4)
swap      = ts(df$SWAPREAL, start=c(2005,1),end=c(2025,1),frequency=4)
crb       = ts(df$CRB, start=c(2005,1),end=c(2025,1),frequency=4)

ibov      = ts(df$IBOV, start=c(2005,1),end=c(2025,1),frequency=4)
sep       = ts(df$SeP, start=c(2005,1),end=c(2025,1),frequency=4)

D0804     = ts(df$D0804, start=c(2005,1),end=c(2025,1),frequency=4)
D1503     = ts(df$D1503, start=c(2005,1),end=c(2025,1),frequency=4)
D2002     = ts(df$D2002, start=c(2005,1),end=c(2025,1),frequency=4)
D1301     = ts(df$D1301, start=c(2005,1),end=c(2025,1),frequency=4)
D2018     = ts(df$D2018, start=c(2005,1),end=c(2025,1),frequency=4)
D2024     = ts(df$D2024, start=c(2005,1),end=c(2025,1),frequency=4)


# Graficos -------------------------------------------------------------------
# Gráficos individuais
par(mfrow = c(3, 3))
ts.plot(cambior, main = "Câmbio Real", xlab = "Tempo", ylab = "Câmbio Real")
ts.plot(cds, main = "CDS", xlab = "Tempo", ylab = "CDS")
ts.plot(passe, main = "Passivo Externo", xlab = "Tempo", ylab = "Passivo Externo")
ts.plot(tots, main = "Termos de Troca", xlab = "Tempo", ylab = "Termos de Troca")
ts.plot(vix, main = "VIX", xlab = "Tempo", ylab = "VIX")
ts.plot(swap, main = "Swap", xlab = "Tempo", ylab = "Swap")
ts.plot(crb, main = "CRB", xlab = "Tempo", ylab = "CRB")

par(mfrow = c(2, 1))
ts.plot(ibov, main = "IBOV", xlab = "Tempo", ylab = "IBOV")
ts.plot(sep, main = "S&P", xlab = "Tempo", ylab = "S&P")

par(mfrow = c(2, 3))

# Graficos em Dupla
# Câmbio & CDS
par (mar=rep(2,4))
ts.plot(log(cambior), log(cds), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio Real", "CDS"), col = c("black", "red"), lwd = 3)

# Câmbio & Passivo Externo
par (mar=rep(2,4))
ts.plot(log(cambior), log(passe), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio Real", "Passivo Externo"), col = c("black", "red"), lwd = 3)

# Câmbio & Termos de Troca
par (mar=rep(2,4))
ts.plot(log(cambior), log(tots), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "Termos de Troca"), col = c("black", "red"), lwd = 3)

#Câmbio & VIX
par (mar=rep(2,4))
ts.plot(log(cambior), log(vix), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio Real", "VIX"), col = c("black", "red"), lwd = 3)

#Câmbio & Swap
par (mar=rep(2,4))
ts.plot(log(cambior), log(1+swap), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio Real", "Swap"), col = c("black", "red"), lwd = 3)

#Câmbio & CRB
par (mar=rep(2,4))
ts.plot(log(cambior), log(crb), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio Real", "CRB"), col = c("black", "red"), lwd = 3)

par(mfrow = c(2, 1))

# Câmbio & IBOV
par (mar=rep(2,4))
ts.plot(log(cambior), log(ibov), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio Real", "IBOV"), col = c("black", "red"), lwd = 3)

# Câmbio & S&P
par (mar=rep(2,4))
ts.plot(log(cambior), log(sep), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio Real", "S&P"), col = c("black", "red"), lwd = 3)



# Regressões -------------------------------------------------------------------
par(mfrow = c(1, 1))

# Testes ADF
adf.test(log(cambior))
adf.test(log(cds))
adf.test(log(passe))
adf.test(log(tots))
adf.test(log(vix))
adf.test(log(1+swap))
adf.test(log(crb))
adf.test(log(ibov))
adf.test(sep)

# Modelos
dfols1 = data.frame(data=df$Data,cambior=cambior,cds=cds,crb=crb,vix=vix,passe=passe,tots=tots,div=div,swap=swap,crb=crb, ibov=ibov, sep=sep, D0804=D0804, D1503=D1503, D2002=D2002, D1301=D1301, D2018=D2018, D2024=D2024)
dfols1.ts = ts(dfols1)

ols1 = dynlm(d(log(cambior)) ~ L(log(cambior),1) + L(log(cambior),2) + log(cds) + L((swap),1) + L(log(tots),-1) + L(log(passe),2) + D0804 + D1503 + L(D2002,-1) + log(sep) + log(ibov), data = dfols1.ts)
summary(ols1)

ols2 = dynlm(d(log(cambior)) ~ L(log(cambior),1) + L(log(cambior),2) + log(cds) + L(log(cds),1) + L((swap),1) + L(log(tots),-1) + L(log(passe),2) + D0804 + D1503 + log(sep) + log(ibov), data = dfols1.ts)
summary(ols2)

ols3 = dynlm(d(log(cambior)) ~ L(log(cambior),1) + log(cds) + L((swap),1) + L(log(tots),-1) + L(log(passe),2) + D0804 + D1503 + log(sep) + log(ibov), data = dfols1.ts)
summary(ols3)

ols4 = dynlm(d(log(cambior)) ~ L(log(cambior),1) + L(log(cambior),2) + log(cds) + L((swap),2) + L(log(tots),-1) + L(log(passe),1) + log(div) + D0804 + D1503 + log(sep) + log(ibov), data = dfols1.ts)
summary(ols4)

olsfin = dynlm(d(log(cambior)) ~ L(log(cambior),1) + L(log(cambior),2) + log(cds) + L((swap),2) + L(log(tots),-1) + L(log(passe),1) + log(div) + log(vix) + D0804 + D1503 + log(sep) + log(ibov), data = dfols1.ts)
summary(olsfin)

# Fit do Modelo
plot.ts(olsfin$residuals)
abline(h = 0, lty = 2)
abline(h = -0.05, lty = 2, col = "red")   
abline(h = 0.05, lty = 2, col = "red")

cambio_lag_facp = pacf(olsfin$residuals,lag.max = NULL, main="Autocorrelacoes Parcial", ylab='',ylim=c(-1,1))
cambio_lag_fac = acf(olsfin$residuals,lag.max = NULL, main="Autocorrelacoes", ylab='',ylim=c(-1,1))

stargazer(olsfin)

