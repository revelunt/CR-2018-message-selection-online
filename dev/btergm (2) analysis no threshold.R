

options(scipen = 999)
require(ergm)
require(btergm)
# setwd("~/Dropbox/(17) 2017 Spring/network QAP Korean election")
# source("btergm helper-functions.R")
source("dev/btergm (1) data prep no threshhold.R")


RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

final.model.nothreshold <- btergm(
           g ~ edges + ## intercept
           
           ## demographic controls
           nodeicov("age") + nodeocov("age") + 
           nodeifactor("gender") + nodeofactor("gender") + nodematch("gender") + 
           nodeicov("edu") + nodeocov("edu") + 
           nodeifactor("region_origin2") + 
           nodeofactor("region_origin2") + 
           nodematch("region_origin2") +   
           
           ## political discussion-related controls
           nodeicov("talk.freq") + nodeocov("talk.freq") + 
           nodeicov("media.use.freq") + nodeocov("media.use.freq") + 
           nodecov("internal.efficacy") + 
           
           ## individual, motivation factor
           nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
           nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
           nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
           
           ## dyadic, consistency
           nodeicov("candidate.preference") + nodeocov("candidate.preference") + 
           nodematch("candidate.preference") + 
           edgecov(policy.pref.sim) +
           edgecov(evaludative.criteria.sim) +
           
           ## endogenous and lagged structural, control
           isolates + mutual + 
           edgecov(g_autoregression) + 
           gwdsp(decay = 0.7, fixed = T) + 
           
           ## lagged structural, control
           edgecov(g_delrecip) + 
           edgecov(g_lagtransitivity) + ## lagged gwesp_OTP
           edgecov(g_lagcyclic) + ## lagged gwesp_ITP
           edgecov(g_lag_shared_activity) + ## lagged gwesp_OSP
           edgecov(g_lag_shared_popularity) + ## lagged gwesp_ISP
           nodeocov("lagged.sender.effect") + 
           nodeicov("lagged.receiver.effect") + 
           
           
           ## endogenous structural
           dgwesp(decay = 3, fixed = T, type = "OTP") + ## 3 or 1.5 understanding
           dgwesp(decay = 3, fixed = T, type = "ITP") + ## 3 or 1.5 understanding
           dgwesp(decay = 3, fixed = T, type = "OSP") + ## 3 or 1.5 consistency
           dgwesp(decay = 2, fixed = T, type = "ISP") + ## 3 or 1.5 consistency
           
           gwodegree(decay = 3.5, fixed = T) + ## hedonic
           gwidegree(decay = 1, fixed = T), ## hedonic 
         R = 1000, parallel = "multicore", ncpus = 10)

save(final.model.nothreshold, file = "final.model.nothreshold.Rdata")

gof.statistics <- c(dsp, odeg, ideg, esp, desp_OTP, desp_ITP, desp_OSP, desp_ISP,
                    geodesic, triad.directed, rocpr, walktrap.modularity)

final.model.nothreshold.gof <- gof(final.model.nothreshold, nsim = 300, 
                                   statistics = gof.statistics, parallel = "multicore", ncpus = 4)

plot(final.model.nothreshold.gof, mfrow = F, xlim = 40)

# Estimates and 95% confidence intervals:
#                                         Estimate    2.5%   97.5%
# edges                                 -1.87629925 -2.4032 -1.7340
# nodeicov.age                          -0.00131749 -0.0354  0.0488
# nodeocov.age                           0.03876339  0.0073  0.0574
# nodeifactor.gender.1                  -0.01893324 -0.0627  0.0483
# nodeofactor.gender.1                  -0.02108996 -0.0901  0.0511
# nodematch.gender                       0.01626029 -0.0045  0.0420
# nodeicov.edu                          -0.00292388 -0.0228  0.0324
# nodeocov.edu                          -0.02427923 -0.0395  0.0183
# nodeifactor.region_origin2.1          -0.14851700 -0.2502 -0.0454
# nodeofactor.region_origin2.1           0.05910426  0.0192  0.1401
# nodematch.region_origin2               0.01325759 -0.0135  0.0664
# nodeicov.talk.freq                     0.09592139  0.0381  0.1370
# nodeocov.talk.freq                     0.01970676 -0.0336  0.0328
# nodeicov.media.use.freq               -0.03119788 -0.0444 -0.0157
# nodeocov.media.use.freq                0.00087541 -0.0105  0.1658
# nodecov.internal.efficacy              0.02646593  0.0034  0.0475
# nodeicov.consistency.motivation        0.00901431 -0.0529  0.0639
# nodeocov.consistency.motivation       -0.02715410 -0.0608 -0.0056
# nodeicov.understanding.motivation     -0.06806945 -0.1036  0.0055
# nodeocov.understanding.motivation      0.04146812  0.0341  0.0562
# nodeicov.hedomic.motivation            0.03330739 -0.0166  0.0995
# nodeocov.hedomic.motivation           -0.00947489 -0.0130  0.0278
# nodeicov.candidate.preference          0.02284380 -0.1097  0.1017
# nodeocov.candidate.preference         -0.01869695 -0.0622  0.0240
# nodematch.candidate.preference         0.06851424  0.0513  0.0900
# edgecov.policy.pref.sim[[i]]          -0.00378837 -0.1169  0.0388
# edgecov.evaludative.criteria.sim[[i]]  0.05180158 -0.0219  0.0873
# isolates                               1.00666666  0.5309  1.8140
# mutual                                 1.09036290  0.8719  1.1450
# edgecov.g_autoregression[[i]]          0.21174686  0.1457  0.2235
# gwdsp.fixed.0.7                       -0.00227008 -0.0084  0.0010
# edgecov.g_delrecip[[i]]                0.03965881 -0.0358  0.0682
# edgecov.g_lagtransitivity[[i]]        -0.01252680 -0.0187 -0.0020
# edgecov.g_lagcyclic[[i]]               0.00333302  0.0027  0.0115
# edgecov.g_lag_shared_activity[[i]]     0.00383495 -0.0003  0.0063
# edgecov.g_lag_shared_popularity[[i]]  -0.02309350 -0.0313 -0.0169
# nodeocov.lagged.sender.effect          0.00852463  0.0050  0.0129
# nodeicov.lagged.receiver.effect        0.01859097  0.0162  0.0245
# gwesp.OTP.fixed.3                      0.01498839  0.0142  0.0294
# gwesp.ITP.fixed.3                     -0.00405089 -0.0124  0.0026
# gwesp.OSP.fixed.3                      0.01930961  0.0145  0.0222
# gwesp.ISP.fixed.2                     -0.01691771 -0.0279  0.0101
# gwodegree                             -4.09932010 -4.3369 -3.1490
# gwidegree                             -6.03061856 -6.4130 -5.7224
