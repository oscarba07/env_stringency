setwd('###')

# Data reading and cleaning ############################################################################
obe <- read.csv('data_final.csv')
str(obe)
#obe$gdppc2 <- obe$gdppc^2
library(plm)
pobe <- pdata.frame(obe, index = c('cname', 'year'))
#pobe$cer_gr <- pobe$cer_pind/lag(pobe$cer_pind,1)-1
pobe$veg_gr <- pobe$veg_pind/lag(pobe$veg_pind,1)-1
pobe$sug_gr <- pobe$avsug_pind/lag(pobe$avsug_pind,1)-1
pobe$foodpr_gr <- pobe$foodpr/lag(pobe$foodpr,1)-1
pobe$vs_gr <- pobe$veg_gr-pobe$sug_gr

obe$veg_gr <-as.vector( pobe$veg_gr)
obe$sug_gr <- as.vector(pobe$sug_gr)
obe$foodpr_gr <- as.vector(pobe$foodpr/lag(pobe$foodpr,1)-1)
obe$vs_gr <- as.vector(pobe$vs_gr)

# Underlying determinants for boys ####################################################################
olsb_u <- plm(ob_b ~ diet #+ I(diet^2)
              + log(gdppc) + I(log(gdppc)^2) 
              + wat #+ I(wat^2)
              + san #+ I(san^2)
              + fem_educ #+ I(fem_educ^2)
              + lfpr_f + I(lfpr_f^2)
              + lfpr_m + I(lfpr_m^2)
              + surv65_rt_fm #+ I(surv65_rt_fm^2)
              + vs_gr #+ I(vs_gr^2)
              + lag(ob_b,1:2) 
          ,data=pobe[pobe$inc_grp!='High income',] 
          ,model = 'pooling' 
#          ,effect = 'individual' 
          ,na.action = na.omit
)
summary(olsb_u)


# Especification tests ################################################################################
## Serial correlation
pbgtest(olsb_u, order = 2)

## plmtest. The null says that fixed effects are insignificant. If rejected, don't use pooling
### Honda
plmtest(olsb_u, effect = 'individual')
plmtest(olsb_u, effect = 'time')
plmtest(olsb_u, effect = 'twoways')
### BP
plmtest(olsb_u, effect = 'individual', type = 'bp')
plmtest(olsb_u, effect = 'time', type = 'bp')
plmtest(olsb_u, effect = 'twoways', type = 'bp')
### King and Wu
plmtest(olsb_u, effect = 'individual', type = 'kw')
plmtest(olsb_u, effect = 'time', type = 'kw')
plmtest(olsb_u, effect = 'twoways', type = 'kw')
### GHM
plmtest(olsb_u, effect = 'twoways', type = 'ghm')
### Wooldridge unobserved effects test. Null says there are no unobserved effects.
pwtest(olsb_u, effect = 'individual')
pwtest(olsb_u, effect = 'time')
## All results suggest both time and individual effects.

## Fixed and random effects ##################################################################
feb_u <- plm(ob_b ~ diet #+ I(diet^2)
             + log(gdppc) + I(log(gdppc)^2) 
             + wat #+ I(wat^2)
             + san #+ I(san^2)
             + fem_educ #+ I(fem_educ^2)
             + lfpr_f + I(lfpr_f^2)
             + lfpr_m + I(lfpr_m^2)
             + surv65_rt_fm #+ I(surv65_rt_fm^2)
             + vs_gr #+ I(vs_gr^2)
             + lag(ob_b,1:2)
             ,data=pobe[pobe$inc_grp!='High income',] 
             ,model = 'within' 
             ,effect = 'individual' 
             ,na.action = na.omit
)
summary(feb_u)

reb_u <- plm(ob_b ~ diet #+ I(diet^2)
             + log(gdppc) + I(log(gdppc)^2) 
             + wat #+ I(wat^2)
             + san #+ I(san^2)
             + fem_educ #+ I(fem_educ^2)
             + lfpr_f + I(lfpr_f^2)
             + lfpr_m + I(lfpr_m^2)
             + surv65_rt_fm #+ I(surv65_rt_fm^2)
             + vs_gr #+ I(vs_gr^2)
             + lag(ob_b,1:2)
             ,data=pobe[pobe$inc_grp!='High income',] 
             ,model = 'random' 
             ,effect = 'individual' 
             ,na.action = na.omit
)
summary(reb_u)

# FE especification tests #############################################################################
## Hausman test. Null says that the difference in coeff is not systematic. If rejected, use FE.
phtest(feb_u, reb_u)
## Null says OLS is better than FE. If rejected, then use FE
pFtest(feb_u, olsb_u) 

## Heteroskedasticity test
bpb_u <- lm(ob_b ~ diet #+ I(diet^2)
            + log(gdppc) + I(log(gdppc)^2) 
            + wat #+ I(wat^2)
            + san #+ I(san^2)
            + fem_educ #+ I(fem_educ^2)
            + lfpr_f + I(lfpr_f^2)
            + lfpr_m + I(lfpr_m^2)
            + surv65_rt_fm #+ I(surv65_rt_fm^2)
            + vs_gr #+ I(vs_gr^2)
            + lag(ob_b,1) + lag(ob_b,2)
            + factor(ccode) 
            ,data = pobe[pobe$inc_grp!='High income',]
            ,na.action = na.omit
)

summary(bpb_u)
library(sandwich)
library(lmtest)
### Breusch-Pagan test. Null syas homoskedasticity. If rejected, there is heteroskedasticity.
bptest(bpb_u)

#coeftest(feb_u, vcov. = vcovHC.plm(feb_u, method = 'arellano', cluster = 'group'))
#coeftest(bpb_u, vcov. = vcovHAC(bpb_u, method = 'arellano', type = 'HC3'))

resettest(bpb_u)


# B-P test for serial correlation in FE. If rejected, serial correlation. Assumes large T
pbgtest(feb_u, order = 2)
u <- residuals(feb_u)
bgtest(u~1, order = 2)

# Wooldridge test for serial correlation in FE. If rejected, there is serial correlation.
pwartest(feb_u)

# Cross sectional dependence tests. 
pcdtest(feb_u, test = 'lm')
pcdtest(feb_u, test = 'cd')


#### Serial correlation and cross-sectiondal dependence are a problem for large time series.
#### I only have 16 years. Justify!!!


## Instrumental variables ##########################################################################
library(AER)
febu_iv <- pgmm(ob_b ~   diet #+ I(diet^2)
                + log(gdppc) + I(log(gdppc)^2) 
                + wat #+ I(wat^2)
                + san #+ I(san^2)
                + fem_educ #+ I(fem_educ^2)
                + lfpr_f + I(lfpr_f^2)
                + lfpr_m + I(lfpr_m^2)
                + surv65_rt_fm #+ I(surv65_rt_fm^2)
                + vs_gr #+ I(vs_gr^2)
#                + lag(ob_b,1:2)
                | temp #+ I(temp^2)
                + csh_i + I(csh_i^2)
                + rain #+ I(rain^2)
                + inv_gdp #+ I(inv_gdp^2)
                + sec_ftch #+ I(sec_ftch^2)
                + f_par + I(f_par^2)
                + lfpr_m + I(lfpr_m^2)
                + flf_exp #+ I(flf_exp^2)
                + vs_gr #+ I(vs_gr^2)
#                + lag(ob_b,1) + lag(ob_b,2) 
             ,data=pobe[pobe$inc_grp!='High income',]
             ,model='twosteps'
             ,effect='individual'
             ,na.action = na.omit
             )

summary(febu_iv, diagnostics = TRUE)

febu_iv <- ivreg(ob_b ~ diet #+ I(diet^2)
                 + log(gdppc) + I(log(gdppc)^2) 
                 + wat #+ I(wat^2)
                 + san #+ I(san^2)
                 + fem_educ #+ I(fem_educ^2)
                 + lfpr_f + I(lfpr_f^2)
                 + lfpr_m + I(lfpr_m^2)
                 + surv65_rt_fm #+ I(surv65_rt_fm^2)
                 + vs_gr #+ I(vs_gr^2)
                 + lag(ob_b,1) + lag(ob_b,2)
                 + factor(ccode)
                 | temp #+ I(temp^2)
                 + csh_i + I(csh_i^2)
                 + rain #+ I(rain^2)
                 + inv_gdp #+ I(inv_gdp^2)
                 + sec_ftch #+ I(sec_ftch^2)
                 + f_par + I(f_par^2)
                 + lfpr_m + I(lfpr_m^2)
                 + flf_exp #+ I(flf_exp^2)
                 + vs_gr #+ I(vs_gr^2)
                + lag(ob_b,1) + lag(ob_b,2)
                 + factor(ccode)
                ,data=pobe[pobe$inc_grp!='High income',]
                ,na.action = na.omit
)
summary(febu_iv, diagnostics = TRUE)
