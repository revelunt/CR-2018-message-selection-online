
rm(list = ls())
options(scipen = 999)
require(ergm)
# save(g, g_relrecip, g_autoregression, policy.pref.diff, evaludative.criteria.diff, file = "R_data/ergm.test.Rdata")
setwd("/Users/songh95/Dropbox/GitHub/Election2012Chatroom")
source("data prep.R")

RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

require(tergm)

stergm.model <- stergm(nw = g, formation =  ~ edges + nodeicov("age") + nodeocov("age") + 
                                                 nodeifactor("gender") + nodeofactor("gender") + nodematch("gender") + 
                                                 nodeicov("edu") + nodeocov("edu") + 
                                                 nodeicov("talk.freq") + nodeocov("talk.freq") + 
                                                 nodeicov("media.use.freq") + nodeocov("media.use.freq") + 
                                                 nodecov("internal.efficacy") + nodecov("external.efficacy") + 
                                                 edgecov(g_autoregression) + 
                                                 edgecov(g_delrecip) + 
                                                 edgecov(g_lagtransitivity) + 
                                                 edgecov(g_lagcyclic) + 
                                                 nodeocov("lagged.sender.effect") + 
                                                 nodeicov("lagged.receiver.effect") + 
                                                    isolates + 
                                                    mutual + 
                                                    dgwesp(decay = 1, fixed = T, type = "OTP") + 
                                                    dgwesp(decay = 1, fixed = T, type = "ITP") + 
                                                    dgwesp(decay = 1, fixed = T, type = "OSP") + 
                                                    dgwesp(decay = 1, fixed = T, type = "ISP") + 
                                                    
                                                    dgwdsp(decay = 1.5, fixed = T, type = "ITP") + 
                                                    dgwdsp(decay = 1.5, fixed = T, type = "OSP") + 
                                                    dgwdsp(decay = 1.5, fixed = T, type = "ISP") + 
                                                    
                                                    gwodegree(decay = 3, fixed = T) + 
                                                    gwidegree(decay = 2, fixed = T) + 
                                                    
                                                    nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
                                                    nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
                                                    nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
                                                    nodeicov("candidate.preference") + nodeocov("candidate.preference") + 
                                                    nodematch("candidate.preference") + 
                                                    edgecov(policy.pref.sim) + 
                                                    edgecov(evaludative.criteria.sim) + 
                                                    nodeifactor("region_origin2") + 
                                                    nodeofactor("region_origin2") + 
                                                    nodematch("region_origin2"),
                       dissolution = ~ edges + nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
                         nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
                         nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
                         nodeicov("candidate.preference") + nodeocov("candidate.preference") + 
                         nodematch("candidate.preference") + 
                         edgecov(policy.pref.sim) + 
                         edgecov(evaludative.criteria.sim),
                       estimate = "CMLE", times = 1:3, 
                       control = control.stergm(parallel = 8, parallel.type = "PSOCK", seed = 12345))
