
rm(list = ls())
options(scipen = 999)
require(ergm)
require(btergm)
require(texreg)
require(parallel)

# setwd("~/Dropbox/GitHub/Korean2012ElectionProject")
source("dev/btergm helper-functions.R")
source("dev/btergm (1) data prep.R")

RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

require(tergm)

for (i in 1:3) {
 g[[i]] %n% "autoregression" <- g_autoregression[[i]]
 g[[i]] %n% "delrecip" <- g_delrecip[[i]]
 g[[i]] %n% "lagtransitivity" <- g_lagtransitivity[[i]]
 g[[i]] %n% "lagcyclic" <- g_lagcyclic[[i]]
 g[[i]] %n% "policy.pref.sim" <- policy.pref.sim[[i]]
 g[[i]] %n% "evaludative.criteria.sim" <- evaludative.criteria.sim[[i]]
 g[[i]] %n% "lag_shared_activity" <- g_lag_shared_activity[[i]]
 g[[i]] %n% "lag_shared_popularity" <- g_lag_shared_popularity[[i]]
}

stergm.model <- stergm(nw = g, formation =  ~ edges + 
                         nodeicov("age") + nodeocov("age") + 
                         nodeifactor("gender") + nodeofactor("gender") + nodematch("gender") + 
                         nodeicov("edu") + nodeocov("edu") + 
                         #nodeifactor("region_origin2") + 
                         #nodeofactor("region_origin2") + 
                         nodematch("region_origin2") +   
                         
                         ## political discussion-related controls
                         nodeicov("talk.freq") + nodeocov("talk.freq") + 
                         nodeicov("media.use.freq") + nodeocov("media.use.freq") + 
                         nodeicov("internal.efficacy") + nodeocov("internal.efficacy") +
                         nodeifactor("candidate.preference") + nodeofactor("candidate.preference") + 
                         
                         ## individual, motivation factor
                         nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
                         nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
                         nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
                         
                         ## dyadic, consistency
                         nodematch("candidate.preference") + 
                         edgecov("policy.pref.sim") +
                         
                         ## dyadic, understanding
                         edgecov("evaludative.criteria.sim") +
                         ## endogenous and lagged structural, control
                         isolates + mutual + 
                         edgecov("autoregression") + 
                         gwdsp(decay = 1, fixed = T) + 
                         
                         ## lagged structural, control
                         edgecov("delrecip") + 
                         edgecov("lagtransitivity") + ## lagged gwesp_OTP
                         edgecov("lagcyclic") + ## lagged gwesp_ITP
                         edgecov("lag_shared_activity") + ## lagged gwesp_OSP
                         edgecov("lag_shared_popularity") + ## lagged gwesp_ISP
                         nodeocov("lagged.sender.effect") + 
                         nodeicov("lagged.receiver.effect") + 
                         
                         ## endogenous structural
                         dgwesp(decay = 3.2, fixed = T, type = "OTP") + ## 3 or 1.5 understanding
                         dgwesp(decay = 3.2, fixed = T, type = "ITP") + ## 3 or 1.5 understanding
                         dgwesp(decay = 3.2, fixed = T, type = "OSP") + ## 3 or 1.5 consistency
                         dgwesp(decay = 2, fixed = T, type = "ISP") + ## 3 or 1.5 consistency
                         
                         gwodegree(decay = 2, fixed = T) + ## hedonic
                         gwidegree(decay = 3, fixed = T),
                       
                       dissolution = ~ edges, ## assumes homogenous dissolution, 
                       estimate = "CMLE",
                       times = 1:3, 
                       control = control.stergm(#init.form = coef(stergm.model)$formation,
                                                CMLE.control.form = control.ergm(MCMLE.maxit = 50),
                                                CMLE.MCMC.burnin=1000000,
                                                CMLE.MCMC.interval=50000, 
                                                #MCMC.samplesize=50000,
                                                parallel = 10, parallel.type = "PSOCK", seed = 12345
                                                ))
