#### Setting the working directory
getwd()
setwd("C:/Users/TECNORED/Downloads")
data <- read.csv("proyectoEco3.csv")

attach(data)


#### Calling some libraries
librerias <- c("foreign","Quandl","tseries","MTS","urca","vars","ggfortify")
lapply(librerias, require, character.only = TRUE)


####Probando estacionariedad
##para el credito
df.test = ur.df(log(intcredit), type = c("drift"), lags = 0)
summary(df.test) #no estacionario

##Para inversion extranjera
df.test = ur.df(forinv, type = c("drift"), lags = 0)
summary(df.test) #estacionario

##para masa monetaria
df.test = ur.df(money, type = c("drift"), lags = 0)
summary(df.test) #estacionario

##Para ingreso por agro
df.test = ur.df(agro, type = c("drift"), lags = 0)
summary(df.test)  #no estacionario

##Para PIB precios actuales
df.test = ur.df(pibactual, type = c("drift"), lags = 0)
summary(df.test)  #estacionario

##Para FDI neto
adf.test = ur.df(netforinv, type = c("drift"), lags = 0)
summary(df.test)  #estacionario


###Volviendo a probar con las variables seleccionadas
Lintcredit <- ts(log(data$intcredit), start= 1977, end = 2018)
Lagro <- ts(log(data$agro), start= 1977, end = 2018)

seqplot.ts(Lintcredit, Lagro, xlab = "year", ylab = "level", colx = "black", 
           coly = "red", main = "Log of Credit and Agr. Income")
legend("topleft", legend = c("Lintcredit", "Lagro"), col = 1:2, lty = 1:1)

#### Verificando raiz unitaria
KPSS.testLintcredit <- ur.kpss(Lintcredit, type = "tau", lags = "short")
KPSS.testLagro <- ur.kpss(Lagro, type = "tau", lags = "short")

summary(KPSS.testLintcredit)
#KPSS test of 0.0642 for the log of intcredit, meaning that has a unit root
summary(KPSS.testLagro)
#KPSS test of 0.1243 for the log of incomes for agro, meaning that has a unit root

#### Unit Root Test - Series in Difference
dLintcredit <- diff(Lintcredit)
dLagro <- diff(Lagro)

ERS.testdLintcredit <- ur.ers(dLintcredit, type = "P-test", lag.max=12, model = "trend")
ERS.testdLagro <- ur.ers(dLagro, type = "P-test", lag.max=12, model = "trend")

summary(ERS.testdLintcredit)
#The statistic is 2.1201, meaning that Lintcredit has a I(1) process
summary(ERS.testdLagro)
#The statistic is 3.6389, meaning that Lagro has a I(1) process


#### Cointegration Analysis
tvar <- cbind(Lintcredit, Lagro)
var <- VARselect(tvar, type = "trend")
nlags <- var$selection["SC(n)"]
# nlags is the number of lags to include in cointegration analysis using SIC 

##Determine the number of lags to include in the C.A  --- VISUAL
VARorder(tvar, maxp=5)


##trace test
tvar.CA <- ca.jo(tvar, type = "trace", spec = "transitory", ecdet = "trend", K=(nlags))
summary(tvar.CA)
# The results show that null hypothesis of r=0 is rejected.
# The test statistic is 35.33 and the critical value at the 1% level is 30.45
# We fail to reject the test for r<=1 since the test statistic is 7.2
# and the critical value at the 10% level is 10.49. 

# The cointegration test suggests that r should be equal to 1.


#### LR-test for no linear trend
lttest(tvar.CA, r=1)
#The result of the test justify the specification of using another unit root test


tvar.VEC <- cajorls(tvar.CA, r = 1)
tvar.VEC

summary(tvar.VEC$rlm)

restricted.alpha <- matrix(c(1,0), c(2,1))
tvar.CA.restricted.alpha <- alrtest(tvar.CA, A = restricted.alpha, r = 1)
tvar.VEC.restricted <- cajorls(tvar.CA.restricted.alpha, r = 1)
tvar.VEC.restricted
summary(tvar.VEC.restricted)
#The intuition behind the restriction is the idea that Effects in Credits affects Agr. Income
#but Effects on Agr. Income do not affect Credits



tvar.VAR <- vec2var(tvar.CA, r=1)
tvar.VAR.fcst <- predict(tvar.VAR, n.ahead = 3)

#install.packages("ggfortify")
autoplot(tvar.VAR.fcst)
