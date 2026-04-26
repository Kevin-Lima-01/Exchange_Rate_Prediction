# Finanças Internacionais e Crises - Bruna, Luíza, Kevin, Marcos e Caio



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
library("vars")



# Base -------------------------------------------------------------------
df <- read_excel("C:/Users/KEVIN/Downloads/base_junta_Nominal.xlsx")
# Gerando a coluna de dummies
df$D1302   = 0  
df$D1509   = 0  
df$DBolso  = 0  
df$D2203   = 0  
df$D2206   = 0  
df$D2501   = 0  

df$D1302[134]   = 1                    # fev/2013 - queda no cambio
df$D1509[165]   = 1                    # set/2015 - aumento do cambio
df$DBolso[202]  = 1                    # out/2018 - queda no cambio (Bolsonaro)
df$D2203[243]   = 1                    # mar/2022 - queda no cambio
df$D2206[246]   = 1                    # mar/2022 - aumento do cambio
df$D2501[277]   = 1                    # jan/2025 - queda do cambio (eleição de Trump)

df <- df[25:282, ]

# Gerando as variaveis
cambio    = ts(df$CAMBIO, start=c(2004,1),end=c(2025,6),frequency=12)
crb       = ts(df$CRB, start=c(2004,1),end=c(2025,6),frequency=12)
vix       = ts(df$VIX, start=c(2004,1),end=c(2025,6),frequency=12)
cds       = ts(df$CDS_5, start=c(2004,1),end=c(2025,6),frequency=12)
dxy       = ts(df$DXY, start=c(2004,1),end=c(2025,6),frequency=12)
fed       = ts(df$FEDFUNDS, start=c(2004,1),end=c(2025,6),frequency=12)/100 
selic     = ts(df$Selic, start=c(2004,1),end=c(2025,6),frequency=12)/100 
ipca      = ts(df$IPCA, start=c(2004,1),end=c(2025,6),frequency=12)/100 
cpi       = ts(df$CPI, start=c(2004,1),end=c(2025,6),frequency=12)/100 
swap_real = ts(df$swapreal360, start=c(2004,1),end=c(2025,6),frequency=12)
swap_real[swap_real == 0] <- 1e-3
df$swapreal360[df$swapreal360 == 0] <- 1e-3

dif1       = ts(selic-fed, start=c(2004,1),end=c(2025,6),frequency=12)
dif2       = ts(ipca-cpi, start=c(2004,1),end=c(2025,6),frequency=12)
fin        = ts(df$INDINCERT, start=c(2004,1),end=c(2025,6),frequency=12)
fin[fin == 0] <- 1e-3
df$INDINCERT[df$INDINCERT == 0] <- 1e-3

D1302     = ts(df$D1302, start=c(2004,1),end=c(2025,6),frequency=12)
D1509     = ts(df$D1509, start=c(2004,1),end=c(2025,6),frequency=12)
DBolso    = ts(df$DBolso, start=c(2004,1),end=c(2025,6),frequency=12)
D2203     = ts(df$D2203, start=c(2004,1),end=c(2025,6),frequency=12)
D2206     = ts(df$D2206, start=c(2004,1),end=c(2025,6),frequency=12)
D2501     = ts(df$D2501, start=c(2004,1),end=c(2025,6),frequency=12)


# Graficos -------------------------------------------------------------------
# Gráficos individuais
par(mfrow = c(3, 3))
ts.plot(cambio, main = "Câmbio", xlab = "Tempo", ylab = "Câmbio")
ts.plot(crb, main = "CRB", xlab = "Tempo", ylab = "CRB")
ts.plot(vix, main = "VIX", xlab = "Tempo", ylab = "VIX")
ts.plot(cds, main = "CDS 5 Anos", xlab = "Tempo", ylab = "CDS")
ts.plot(dxy, main = "DXY", xlab = "Tempo", ylab = "DXY")
ts.plot(swap_real, main = "Swap Real 360", xlab = "Tempo", ylab = "Swap Real 360")
ts.plot(fin, main = "INDINCERT", xlab = "Tempo", ylab = "INDINCERT")

par(mfrow = c(2, 3))
ts.plot(fed, main = "FED FUNDS", xlab = "Tempo", ylab = "Fed funds rate")
ts.plot(selic, main = "selic", xlab = "Tempo", ylab = "selic")
ts.plot(cpi, main = "CPI", xlab = "Tempo", ylab = "CPI")
ts.plot(ipca, main = "IPCA", xlab = "Tempo", ylab = "IPCA")
ts.plot(dif1, main = "dif1", xlab = "Tempo", ylab = "dif1")
ts.plot(dif2, main = "dif2", xlab = "Tempo", ylab = "dif2")

par(mfrow = c(2, 3))

# Graficos em Dupla
# Câmbio & CDS
par (mar=rep(2,4))
ts.plot(log(cambio), log(cds), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "CDS"), col = c("black", "red"), lwd = 3)

# Câmbio & CRB
par (mar=rep(2,4))
ts.plot(log(cambio), log(crb), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "CRB"), col = c("black", "red"), lwd = 3)

# Câmbio & DXY
par (mar=rep(2,4))
ts.plot(log(cambio), log(dxy), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "DXY"), col = c("black", "red"), lwd = 3)

#Câmbio & Swap
par (mar=rep(2,4))
ts.plot(log(cambio), log(1+swap_real), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "Swap real"), col = c("black", "red"), lwd = 3)

#Câmbio & INDINCERT
par (mar=rep(2,4))
ts.plot(log(cambio), log(fin), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "INDINCERT"), col = c("black", "red"), lwd = 3)

par(mfrow = c(2, 3))

# Câmbio & FED
par (mar=rep(2,4))
ts.plot(log(cambio), log(fed), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "FED"), col = c("black", "red"), lwd = 3)

# Câmbio & Selic
par (mar=rep(2,4))
ts.plot(log(cambio), log(selic), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "Selic"), col = c("black", "red"), lwd = 3)

# Câmbio & cpi
par (mar=rep(2,4))
ts.plot(log(cambio), log(cpi), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "CPI"), col = c("black", "red"), lwd = 3)

# Câmbio & ipca
par (mar=rep(2,4))
ts.plot(log(cambio), log(ipca), gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "IPCA"), col = c("black", "red"), lwd = 3)

# Câmbio & dif1
par (mar=rep(2,4))
ts.plot(log(cambio), dif1, gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "dif1"), col = c("black", "red"), lwd = 3)

# Câmbio & dif2
par (mar=rep(2,4))
ts.plot(log(cambio), dif2, gpars  = list(col=c('black', 'red'), lwd=c(3,3)))
legend("topright", legend = c("Câmbio", "dif2"), col = c("black", "red"), lwd = 3)



# Regressões -------------------------------------------------------------------
par(mfrow = c(1, 1))

# Testes ADF
adf.test(log(cambio))
adf.test(log(crb))
adf.test(log(vix))
adf.test(log(cds))
adf.test(log(dxy))
adf.test(log(1+swap_real))
adf.test(log(fed))
adf.test(log(selic))
adf.test(dif1)
adf.test(dif2)
adf.test(log(ipca))
adf.test(log(cpi))
adf.test(1+fin)

# Modelos
dfols1 = data.frame(data=df$observation_date, fed=fed,cambio=cambio, crb = crb, vix=vix, cds=cds, dxy=dxy, selic=selic, ipca=ipca, cpi=cpi, D1302=D1302, D1509=D1509, DBolso=DBolso, D2203=D2203, D2206=D2206, D2501=D2501, dif1=dif1, dif2=dif2, fin=fin, swap_real=swap_real)
dfols1.ts = ts(dfols1)



ols1 = dynlm(d(log(cambio)) ~ d(log(crb)) + d(log(vix)) + d(log(cds)) + d(log(dxy)) + d(log(1+swap_real)) + d(log(1+fin)) + d(log(fed)) + d(log(selic)) + dif1 + d(log(ipca)) + d(log(cpi)) + dif2 + D1302 + D1509 + DBolso + D2203 + D2206 + D2501, data = dfols1.ts)
summary(ols1)

ols2 = dynlm(d(log(cambio)) ~ d(log(crb)) + d(log(vix)) + d(log(cds)) + d(log(dxy)) + d(log(1+swap_real)) + d(log(1+fin)) + d(log(fed)) + d(log(selic)) + dif1 + D1302 + D1509 + DBolso + D2203 + D2206 + D2501, data = dfols1.ts)
summary(ols2)

ols3 = dynlm(d(log(cambio)) ~ d(log(crb)) + d(log(vix)) + d(log(cds)) + d(log(dxy)) + d(log(1+swap_real)) + d(log(1+fin)) + d(log(ipca)) + d(log(cpi)) + dif2 + D1302 + D1509 + DBolso + D2203 + D2206 + D2501, data = dfols1.ts)
summary(ols3)

ols4 = dynlm(d(log(cambio)) ~ d(log(crb)) + d(log(vix)) + d(log(cds)) + d(log(dxy)) + d(log(1+swap_real)) + d(log(1+fin)) + dif1 + dif2 + D1302 + D1509 + DBolso + D2203 + D2206 + D2501, data = dfols1.ts)
summary(ols4)

ols5 = dynlm(d(log(cambio)) ~ d(log(crb)) + d(log(vix)) + d(log(cds)) + d(log(dxy)) + d(log(1+swap_real)) + d(log(fed)) + d(log(selic)) + dif1 + d(log(ipca)) + d(log(cpi)) + dif2 + D1302 + D1509 + DBolso + D2203 + D2206 + D2501, data = dfols1.ts)
summary(ols5)

ols6 = dynlm(d(log(cambio)) ~ d(log(crb)) + d(log(vix)) + d(log(cds)) + d(log(dxy)) + d(log(1+swap_real)) + d(log(fed)) + d(log(selic)) + dif1 +  D1302 + D1509 + DBolso + D2203 + D2206 + D2501, data = dfols1.ts)
summary(ols6)

ols7 = dynlm(d(log(cambio)) ~ d(log(crb)) + d(log(vix)) + d(log(cds)) + d(log(dxy)) + d(log(1+swap_real)) + d(log(ipca)) + d(log(cpi)) + dif2 +  D1302 + D1509 + DBolso + D2203 + D2206 + D2501, data = dfols1.ts)
summary(ols7)

ols8 = dynlm(d(log(cambio)) ~ d(log(crb)) + d(log(vix)) + d(log(cds)) + d(log(dxy)) + d(log(1+swap_real)) + dif1 + dif2 +  D1302 + D1509 + DBolso + D2203 + D2206 + D2501, data = dfols1.ts)
summary(ols8)

ols9 = dynlm(d(log(cambio)) ~ d(log(crb)) + d(log(vix)) + d(log(cds)) + d(log(dxy)) + d(log(1+swap_real)) + d(log(fed)) + d(log(selic)) +  D1302 + D1509 + DBolso + D2203 + D2206 + D2501, data = dfols1.ts)
summary(ols9)

olsfin  = dynlm(d(log(cambio)) ~ d(log(crb))  + d(log(vix)) + d(log(cds)) + d(log(dxy)) + d(log(1+fin)) + d(log(fed)) + d(log(selic)) + dif1 +  D1302 + D1509 + DBolso + D2203 + D2206 + D2501, data = dfols1.ts)
summary(olsfin)



# Fit do Modelo
plot.ts(olsfin$residuals)
abline(h = 0, lty = 2)
abline(h = -0.05, lty = 2, col = "red")   
abline(h = 0.05, lty = 2, col = "red")

cambio_lag_facp = pacf(olsfin$residuals,lag.max = NULL, main="Autocorrelacoes Parcial", ylab='',ylim=c(-1,1))
cambio_lag_fac = acf(olsfin$residuals,lag.max = NULL, main="Autocorrelacoes", ylab='',ylim=c(-1,1))

stargazer(olsfin)



# VAR -------------------------------------------------------------------
# Base
lcamb=log(cambio)
lcrb=log(crb)
lvix=log(vix)
lcds=log(cds)
ldxy=log(dxy)
lfin=log(1+fin)
lfed=log(fed)
lselic=log(selic)
df2 <- cbind(diff(lcamb),diff(lcrb),diff(lvix),diff(lcds),diff(ldxy),diff(lfin),diff(lfed),diff(lselic))


# Modelo Var
VAR1 <- VAR(df2, lag.max = 8, season = 12) 
summary(VAR1)

plot(ts(residuals(VAR1)[,"diff.lcamb."]))
plot(ts(residuals(VAR1)[,"diff.lcrb."]))
plot(ts(residuals(VAR1)[,"diff.lvix."]))
plot(ts(residuals(VAR1)[,"diff.lcds."]))
plot(ts(residuals(VAR1)[,"diff.ldxy."]))
plot(ts(residuals(VAR1)[,"diff.lfin."]))
plot(ts(residuals(VAR1)[,"diff.lfed."]))
plot(ts(residuals(VAR1)[,"diff.lselic."]))

# Choques
imp_cambio <- irf(VAR1,impulse="diff.lselic.",response="diff.lcamb.",n.ahead = 10, ortho = TRUE, runs = 100,seed=12345)
plot(imp_cambio,bty="l")
imp_cambio <- irf(VAR1,impulse="diff.lfed.",response="diff.lcamb.",n.ahead = 10, ortho = TRUE, runs = 100,seed=12345)
plot(imp_cambio,bty="l")





