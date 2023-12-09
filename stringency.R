setwd('C:/Users/usuario/OneDrive/Documentos/UoM/Dissertation/data')
library(plyr)
# Data cleaning ####################################################################################
ghg <- read.csv('Data.csv', na.strings = '.')

## Create EPS index
for (i in 1:length(ghg$country)) {
  ghg$str_agr[i] <- 1-(ghg$ghg_va_agr[i]-min(ghg$ghg_va_agr[ghg$year==ghg$year[i]], na.rm = T))/
    (max(ghg$ghg_va_agr[ghg$year==ghg$year[i]], na.rm = T)-
       min(ghg$ghg_va_agr[ghg$year==ghg$year[i]], na.rm = T))
  
  ghg$str_ind[i] <- 1-(ghg$ghg_va_ind[i]-min(ghg$ghg_va_ind[ghg$year==ghg$year[i]], na.rm = T))/
    (max(ghg$ghg_va_ind[ghg$year==ghg$year[i]], na.rm = T)-
       min(ghg$ghg_va_ind[ghg$year==ghg$year[i]], na.rm = T))
  
  ghg$str_ser[i] <- 1-(ghg$ghg_va_ser[i]-min(ghg$ghg_va_ser[ghg$year==ghg$year[i]], na.rm = T))/
    (max(ghg$ghg_va_ser[ghg$year==ghg$year[i]], na.rm = T)-
       min(ghg$ghg_va_ser[ghg$year==ghg$year[i]], na.rm = T))
}

ghg$eps <- (ghg$str_agr + ghg$str_ind + ghg$str_ser)/3
ghg$ghg_tot <- ghg$ghg_serv + ghg$ghg_agr + ghg$ghg_ind

write.csv(ghg,file='data_final.csv')

# Expected results ####################################################################
## Correlation
levels(ghg$inc_grp)
ghg$inc_grp <- factor(ghg$inc_grp, levels = levels(ghg$inc_grp)[c(1,4,3,2)])
mns <- aggregate(eps~year+inc_grp, data=ghg, FUN=mean)


corsy <- round(ddply(ghg, factor('year'), function(x) cor(x$eps, x$TFPgrowth, use='pairwise.complete.obs')),2)
corsi <- ddply(ghg, 'inc_grp', function(x) cor(x$eps, x$TFPgrowth, use='pairwise.complete.obs'))
corsi[,2] <- round(corsi[,2],2)
corsr <- ddply(ghg, 'region', function(x) cor(x$eps, x$TFPgrowth, use='pairwise.complete.obs'))
corsr[,2] <- round(corsr[,2],2)


library(stargazer)
stargazer(corsr, type='latex', summary = FALSE)
stargazer(corsi, type='latex', summary = FALSE)
summary(ghg$eps)
aggregate(ghg$eps, FUN=mean, by=list(ghg$region), na.rm=TRUE)
stargazer(aggregate(ghg$eps, FUN=mean, by=list(ghg$inc_grp), na.rm=TRUE))

library(ggplot2)
library(ggcorrplot)
corr <- round(cor(ghg[,c('TFPgrowth', 'eps', 'education_index', 'innovation_index', 
                         'infrastructure_index', 'efficiency_index')], 
                  use = 'pairwise.complete'),2)
cr <- ggcorrplot(corr,
                 method = 'square',
                 type = 'upper', 
                 hc.order = TRUE,
                 lab=TRUE
                 )
#pdf('correlation matrix.pdf')
cr
#dev.off()
## Trends
p <- ggplot(data=mns, aes(x=year, y=eps, group=inc_grp))
p <- p + 
  geom_line(aes(color=inc_grp)) + 
  labs(x='year', y='EPS', color='Income group')
p



# Panel data Regressions ###########################################################################
## Convert data into Panel Data Frame
library(plm)
ghg_pd <- pdata.frame(ghg, index = c('country', 'year'))

## Regressions
### Pooled OLS
fitp <- plm(TFPgr_wb ~ eps + inn_in + ed_in + effi_in + infra_in + gov_in
            ,data = ghg_pd
            ,model = 'pooling'
            ,na.action = 'na.omit'
            #,subset = (inc_grp!='High income')
            )
summary(fitp)

### Between estimator
fitb <- plm(TFPgr_wb ~ eps + inn_in + ed_in + effi_in + infra_in + gov_in
            ,data = ghg_pd
            ,model = 'between'
            ,na.action = 'na.omit'
            ,index = c('country','year')
            #,subset = (inc_grp!='High income')
)
summary(fitb)

fitb_d <- plm(TFPgr_wb ~ eps + inn_in + ed_in + effi_in + infra_in + gov_in
            ,data = ghg_pd
            ,model = 'between'
            ,na.action = 'na.omit'
            ,subset = (inc_grp=='High income')
)
summary(fitb_d)

### Within estimator
fitw <- plm(TFPgr_wb ~ eps + inn_in + ed_in + effi_in + infra_in + gov_in
            ,data = ghg_pd
            ,model = 'within'
            ,na.action = 'na.omit'
            ,index = c('country', 'year')
            ,subset = (inc_grp!='High income')
)
summary(fitw)

fitwl <- plm(TFPgr_wb ~ eps + lag(eps) + inn_in + ed_in + effi_in + infra_in + gov_in
            ,data = ghg_pd
            ,model = 'within'
            ,na.action = 'na.omit'
            ,index = c('country', 'year')
            ,subset = (inc_grp!='High income')
)
summary(fitwl)

fitwll <- plm(TFPgr_wb ~ eps + lag(eps) + lag(eps,2) + inn_in + ed_in + effi_in + infra_in + gov_in
             ,data = ghg_pd
             ,model = 'within'
             ,na.action = 'na.omit'
             ,index = c('country', 'year')
             ,subset = (inc_grp!='High income')
)
summary(fitwll)

fitwlll <- plm(TFPgr_wb ~ eps + lag(eps) + lag(eps,2) + lag(eps,3) + inn_in + ed_in + effi_in + infra_in + gov_in
              ,data = ghg_pd
              ,model = 'within'
              ,na.action = 'na.omit'
              ,index = c('country', 'year')
              ,subset = (inc_grp!='High income')
)
summary(fitwlll)


### Random effects
fitr <- plm(TFPgr_wb ~ eps + inn_in + ed_in + effi_in + infra_in + gov_in
            ,data = ghg_pd
            ,model = 'random'
            ,na.action = 'na.omit'
            ,index = c('country', 'year')
            ,subset = (inc_grp!='High income')
)
summary(fitr)

