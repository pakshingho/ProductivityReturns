library(plm)
library(sandwich)
library(lmtest)
library(stargazer)
library("texreg")

dfSIC4 <- read.csv('data/TFP_Compustat_keep.csv')
dfFF48 <- read.csv('data/TFP_Compustat_ff48_keep.csv')

panelSIC4 <- pdata.frame(dfSIC4, index=c('gvkey','fyear'), drop.index = TRUE, row.names=TRUE)
panelFF48 <- pdata.frame(dfFF48, index=c('gvkey','fyear'), drop.index = TRUE, row.names=TRUE)

stargazer(panelSIC4[is.finite(panelSIC4$emp_size),],
          omit=c('sic'),
          digits=4)

stargazer(panelFF48[is.finite(panelFF48$emp_size),],
          omit=c('sic','ffi48'),
          digits=4)

SIC1 <- plm('beme ~ gap + size', 
                 data=panelSIC4, index=c('gvkey','fyear'),
                 model='within',
                 effect=c("twoways"))

SIC2 <- plm('beme ~ gap + size + emp_size + lev + roa + roe + gpr', 
            data=panelSIC4[is.finite(panelSIC4$emp_size),], index=c('gvkey','fyear'),
            model='within',
            effect=c("twoways"))

SIC3 <- plm('beme ~ gap + size + emp_size + lev + roa + roe + gpr + rdi + adi', 
            data=panelSIC4[is.finite(panelSIC4$emp_size),], index=c('gvkey','fyear'),
            model='within',
            effect=c("twoways"))

FF1 <- plm('beme ~ gap + size', 
                 data=panelFF48, index=c('gvkey','fyear'),
                 model='within',
                 effect=c("twoways"))

FF2 <- plm('beme ~ gap + size + emp_size + lev + roa + roe + gpr', 
            data=panelFF48[is.finite(panelFF48$emp_size),], index=c('gvkey','fyear'),
            model='within',
            effect=c("twoways"))

FF3 <- plm('beme ~ gap + size + emp_size + lev + roa + roe + gpr + rdi + adi', 
           data=panelFF48[is.finite(panelFF48$emp_size),], index=c('gvkey','fyear'),
           model='within',
           effect=c("twoways"))

summary(SIC1, vcov=vcovHC(SIC1, cluster="group"))
summary(SIC1, vcov = vcovDC)

vcovHC(SIC1, cluster="group")

screenreg(list(fixedSIC4 = fixedSIC4, fixedFF48 = fixedFF48),
          digits = 4, omit.coef = NULL, vcov=vcovDC)

screenreg(list('fixed SIC4' = fixedSIC4, 
               'fixed SIC4 Clustered' = coeftest(fixedSIC4, vcov = vcovDC)
               ),
          digits = 4, omit.coef = NULL)

texreg(list(SIC1, SIC2, SIC3),
          digits = 4, omit.coef = NULL)

texreg(list(SIC1, SIC2, SIC3),
       digits = 4, omit.coef = NULL, vcov=vcovDC)

texreg(list(FF1, FF2, FF3),
       digits = 4, omit.coef = NULL)

texreg(list(FF1, FF2, FF3),
       digits = 4, omit.coef = NULL, vcov=vcovDC)

texreg(list(SIC1, SIC2, SIC3, FF1, FF2, FF3),
       digits = 4, omit.coef = NULL, vcov=vcovDC)

texreg(list(SIC1, SIC2, SIC3, FF1, FF2, FF3),
       digits = 3, omit.coef = NULL, vcov=vcovDC)
