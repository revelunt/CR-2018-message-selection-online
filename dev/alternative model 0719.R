
options(scipen = 999)
require(ergm)
require(btergm)
setwd("~/Dropbox/(17) 2017 Spring/network QAP Korean election")
source("btergm helper-functions.R")
source("btergm (1) data prep.R")


RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

## alternative model specification
asymmetrical.knowledge <- lapply(g, make_asymmetrical_adj, "knowledge")
asymmetrical.interest <- lapply(g, make_asymmetrical_adj, "interest")

alter_more_knowledgeable <- lapply(asymmetrical.knowledge, function(x) {
    mat <- 1 * (x > 0)
    diag(mat) <- 0
    mat
  }
)

alter_more_interested <- lapply(asymmetrical.interest, function(x) {
  mat <- 1 * (x > 0)
  diag(mat) <- 0
  mat
})




final.model <- btergm(g ~ edges + ## intercept
          
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
           nodecov("internal.efficacy") + nodecov("external.efficacy") + 

         ## individual, motivation factor
           nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
           nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
           nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
           
         ## individual, message factor
           #nodeicov("message.quality") + ## understanding
           #nodeicov("message.cue") + ## hedonic
           
         ## dyadic, consistency
           nodeicov("candidate.preference") + nodeocov("candidate.preference") + 
           nodematch("candidate.preference") + 
           edgecov(policy.pref.sim) +
           
         ## dyadic, understanding
           edgecov(alter_more_knowledgeable) + 
           edgecov(alter_more_interested) +  
           edgecov(evaludative.criteria.sim) +
           
         ## endogenous and lagged structural, control
           isolates + mutual + 
           edgecov(g_autoregression) + 
           dgwdsp(decay = 1, fixed = T, type = "OTP") + 
           dgwdsp(decay = 1, fixed = T, type = "ITP") + 
           dgwdsp(decay = 1, fixed = T, type = "OSP") + 
           dgwdsp(decay = 1, fixed = T, type = "ISP") + 

         ## lagged structural, control
           edgecov(g_delrecip) + 
           edgecov(g_lagtransitivity) + ## lagged gwesp_OTP
           edgecov(g_lagcyclic) + ## lagged gwesp_ITP
           #edgecov(g_lag_shared_activity) + ## lagged gwesp_OSP
           #edgecov(g_lag_shared_popularity) + ## lagged gwesp_ISP
           nodeocov("lagged.sender.effect") + 
           nodeicov("lagged.receiver.effect") + 
           

         ## endogenous structural
           dgwesp(decay = 3, fixed = T, type = "OTP") + ## 3 or 1.5 understanding
           dgwesp(decay = 3, fixed = T, type = "ITP") + ## 3 or 1.5 understanding
           dgwesp(decay = 3, fixed = T, type = "OSP") + ## 3 or 1.5 consistency
           dgwesp(decay = 2, fixed = T, type = "ISP") + ## 3 or 1.5 consistency
          
           gwodegree(decay = 2, fixed = T) + ## hedonic
           gwidegree(decay = 3, fixed = T), ## hedonic 

       R = 1000, parallel = "multicore", ncpus = 10)

save(final.model ,file = "final.model.July19th.Rdata")

gof.statistics <- c(dsp, odeg, ideg, esp, desp_OTP, desp_ITP, desp_OSP, desp_ISP,
                    geodesic, triad.directed, rocpr, walktrap.modularity)
final.model.gof <- gof(final.model, nsim = 300, statistics = gof.statistics, 
                      parallel = "multicore", ncpus = 4)
save(final.model.gof, file = "final.model.gof.Rdata")
