
options(scipen = 999)
require(ergm)
# save(g, g_relrecip, g_autoregression, policy.pref.diff, evaludative.criteria.diff, file = "R_data/ergm.test.Rdata")
setwd("~/Dropbox/(17) 2017 Spring/network QAP Korean election")
source("btergm (1) data prep threshold.R")

## --------------------------- ##
## Bootstrapped TERGM analysis ##
## ----------------------------##

require(btergm)
RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

## first, start with control only model
model1 <- btergm(g ~ edges +
                   
                   # ## controls
                   nodeicov("age") +
                   nodeocov("age") +
                   nodeifactor("gender") +
                   nodeofactor("gender") +
                   nodeicov("edu") +
                   nodeocov("edu") +
                   nodeicov("talk.freq") + ## those who talks a lot offline will have a higher activity
                   nodeocov("talk.freq") +
                   nodeicov("media.use.freq") + ## those who use media a lot will have a higher activity
                   nodeocov("media.use.freq") +
                   nodeicov("efficacy") + ## those with more efficacy would select more and be selected more by others
                   nodeocov("efficacy") + ## those with more efficacy would select more and be selected more by others
                   nodeifactor("region_origin2") +  ## regional differences
                   nodeofactor("region_origin2"), 
                   verbose = T, R = 100, ncpus = 4, parallel = "multicore"); summary(model1)


## next, we add structural effects
model2 <- btergm(update.formula(model1@formula, ~ . + 
                                  ## lagged effect
                                  edgecov(g_autoregression) +  ## previous communication within dyads ("autoregression")
                                  edgecov(g_relrecip) + ## delayed reciprocity
                                  edgecov(g_lagtransitivity) + ## delayed transivity
                                  edgecov(g_lagcyclic) + ## delayed cylic closure
                                  nodeocov("lagged.sender.effect") + ## persistent sender effect
                                  nodeicov("lagged.receiver.effect") + ## persistent receiver effect
                                  isolates + 
                                  ## concurrent effect
                                  ## dyadic level
                                  mutual + 
                                  ## triadic level
                                  dgwesp(decay = 0.8, fixed = T, type = "OTP") + ## transitive closure
                                  dgwesp(decay = 0.1, fixed = T, type = "ITP") + ## cyclic closure
                                  dgwesp(decay = 0.1, fixed = T, type = "OSP") + ## number of common "posters" between two viewrs
                                  dgwesp(decay = 0.1, fixed = T, type = "ISP") + ## number of common "viewers" between two posters
                                  
                                  ## hub/star structure
                                  ## "anti"-preferential attachments for receiving ties 
                                  gwidegree(decay = 0.6, fixed = T) + ## negative coef means positive preferential attachments
                                  ## "anti"-preferential attachments for outgoing ties
                                  gwodegree(decay = 1.5, fixed = T)) ## negative coef means uneven distribution of outdegrees
                                  ## (e.g., some people reads more than others)
                                  
                                  ## some control variables for goodness of fit 
                                  #dgwdsp(decay = 0.3, fixed = T, type = "OTP") + 
                                  #dgwdsp(decay = 0.2, fixed = T, type = "ITP"))
                                  ,
                                  verbose = T, R = 100, ncpus = 4, parallel = "multicore"); summary(model2)


## add motivations and dyadic homophily predictors
model3 <- btergm(update.formula(model2@formula, ~ . + 
                                  ## motivations
                                  nodeicov("consistency.motivation") +
                                  nodeocov("consistency.motivation") +
                                  nodeicov("understanding.motivation") +
                                  nodeocov("understanding.motivation") +
                                  nodeicov("hedomic.motivation") +
                                  nodeocov("hedomic.motivation") +
                                  
                                  ## homophily predictors
                                  nodeicov("pol.ideology") +
                                  nodeocov("pol.ideology") +
                                  absdiff("pol.ideology") +
                                  nodeifactor("candidate.preference") +
                                  nodeofactor("candidate.preference") +
                                  nodematch("candidate.preference") + 
                                  
                                  edgecov(policy.pref.diff) + ## policy preference difference on ED metric. expect negative relationship
                                  edgecov(evaludative.criteria.diff)),
                                  verbose = T, R = 100, ncpus = 4, parallel = "multicore"); summary(model3)

## here we need to estimate goodness of fit.
model3.gof <- gof(model3, statistics = c(dsp, esp, odeg, ideg, geodesic, rocpr), 
                  nsim = 400, verbose = TRUE, ncpus = 5, parallel = "multicore")
save(model3.gof, file = 'model3.gof.Rdata')
plot(model3.gof, mfrow = FALSE)


## we further add some interesting interaction terms
model4 <- btergm(update.formula(model3@formula, ~ . + 
                                  mutual("candidate.preference", diff = FALSE) + ## interaction: within same candidate pref.
                                  ttriple("candidate.preference") + ## transitivity within same candidate pref.
                                  ctriple("candidate.preference") + ## cyclic closure within same candidate pref.
                                  edgecov(eval.crtria.diff.int.consistency) + 
                                  edgecov(eval.crtria.diff.int.understanding)),
                                  verbose = T, R = 100, ncpus = 4, parallel = "multicore"); summary(model4)  
                         
model4.gof <- gof(model4, statistics = c(dsp, esp, odeg, ideg, geodesic, rocpr), 
                   nsim = 400, verbose = TRUE, ncpus = 4, parallel = "multicore")     
save(model4.gof, file = 'model4.gof.Rdata')
plot(model4.gof, mfrow = FALSE)


## all models so far at once
texreg::screenreg(list(model1, model2, model3, model4))

## eliminate insignificant interactions and tweak few things 
final.model <- btergm(update.formula(model4@formula, ~ . 
                                     - edgecov(eval.crtria.diff.int.consistency) 
                                     - edgecov(eval.crtria.diff.int.understanding)
                                     - gwodegree(decay = 2.8, fixed = T)
                                     + gwodegree(decay = 3.5, fixed = T)
                                     + odegree(0) + esp(0)),
                      verbose = T, R = 1000, ncpus = 4, parallel = "multicore"); summary(final.model) 

texreg::screenreg(list(model1, model2, model3, model4, final.model))

final.model.gof <- gof(final.model, statistics = c(dsp, esp, odeg, ideg, geodesic, rocpr), 
                       nsim = 400, verbose = TRUE, ncpus = 4, parallel = "multicore")
save(final.model.gof, file = "final.model.gof.Rdata")
pdf(file = "final.model.gof.pdf")
plot(final.model.gof, mfrow = FALSE)
dev.off()

save(model1, model2, model3, model4, final.model, file = "R_data/btergm.results.June 08th.Rdata")
save(model3.gof, model4.gof, final.model.gof, file = "R_data/btergm.gof.results.June 08th.Rdata")

# ## same as following:
# final.model <- btergm(g ~ edges +
#                    # ## controls
#                    nodeicov("age") +
#                    nodeocov("age") +
#                    nodeifactor("gender") +
#                    nodeofactor("gender") +
#                    nodeicov("edu") +
#                    nodeocov("edu") +
#                    nodeicov("talk.freq") + ## those who talks a lot offline will have a higher activity
#                    nodeocov("talk.freq") +
#                    nodeicov("media.use.freq") + ## those who use media a lot will have a higher activity
#                    nodeocov("media.use.freq") +
#                    nodeicov("efficacy") + ## those with more efficacy would select more and be selected more by others
#                    nodeocov("efficacy") + ## those with more efficacy would select more and be selected more by others
#                    nodeifactor("region_origin2") +  ## regional differences
#                    nodeofactor("region_origin2") +
# 
#                    ## motivations
#                    nodeicov("consistency.motivation") +
#                    nodeocov("consistency.motivation") +
#                    nodeicov("understanding.motivation") +
#                    nodeocov("understanding.motivation") +
#                    nodeicov("hedomic.motivation") +
#                    nodeocov("hedomic.motivation") +
# 
#                    ## lagged effect
#                    edgecov(g_autoregression) +  ## previous communication within dyads ("autoregression")
#                    edgecov(g_relrecip) + ## delayed reciprocity
#                    edgecov(g_lagtransitivity) + ## delayed transivity
#                    edgecov(g_lagcyclic) + ## delayed cylic closure
#                    nodeocov("lagged.sender.effect") + ## persistent sender effect
#                    nodeicov("lagged.receiver.effect") + ## persistent receiver effect
# 
#                    ## concurrent effect
#                    ## dyadic level
#                    mutual +
#                    mutual("candidate.preference", diff = FALSE) + ## interaction: within same candidate pref.
# 
#                    ## triadic level
#                    dgwesp(decay = 0.1, fixed = T, type = "ITP") + ## cyclic closure
#                    dgwesp(decay = 0.1, fixed = T, type = "OSP") + ## number of common "posters" between two viewrs
#                    ttriple("candidate.preference") + ## transitivity within same candidate pref.
#                    ctriple("candidate.preference") + ## cyclic closure within same candidate pref.
# 
#                    ## hub/star structure
#                    ## "anti"-preferential attachments for receiving ties
#                    gwidegree(decay = 0.4, fixed = T) + ## negative coef means positive preferential attachments
#                    ## "anti"-preferential attachments for outgoing ties
#                    gwodegree(decay = 3.5, fixed = T) + ## negative coef means uneven distribution of outdegrees
#                    ## (e.g., some people reads more than others)
# 
#                    ## some control variables for goodness of fit
#                    eps(0) + odegree(0) +
# 
#                    ## homophily predictors
#                    nodeicov("pol.ideology") +
#                    nodeocov("pol.ideology") +
#                    absdiff("pol.ideology") +
#                    nodeifactor("candidate.preference") +
#                    nodeofactor("candidate.preference") +
#                    nodematch("candidate.preference") +
#                    #absdiff("criteria.competence") +
#                    #absdiff("criteria.background") +
# 
#                    edgecov(policy.pref.diff) + ## policy preference difference on ED metric. expect negative relationship
#                    edgecov(evaludative.criteria.diff)  ## importance of evaluative criteria, difference on ED metric. expect negative relationship
# 
#                    , verbose = T, R = 1000, ncpus = 4, parallel = "multicore")


texreg::htmlreg(list(model1, model2, model3, model4, final.model), single.row = T, digits = 3, leading.zero = F,
                  custom.model.names = c("control only", "structural", "homophily", "interactions", "final"),
                  custom.coef.names = c("Intercept", "age (in-ties)", "age (out-ties)",
                                        "female (in-ties)", "female (out-ties)", "edu (in-ties)", "edu (out-ties)",
                                        "talk.freq (in-ties)", "talk.freq (out-ties)", "media.use (in-ties)", "media.use (out-ties)",
                                        "pol.efficacy (in-ties)", "pol.efficacy (out-ties)", 
                                        "region.origin = Seoul (in-ties)", "region.origin = Seoul (out-ties)",
                                        "previous communication", "delayed reciprocity", "delayed transitivity", 
                                        "delayed cyclic closure", "persistent sender (out-tie)", "persistent receiver (in-ties)",
                                        "reciprocity", "GWESP (out-two path, 0.8)", "GWESP (in-two path, 0.1)", 
                                        "GWESP (out-shared partner, 0.1)", "GWESP (in-shared partner, 0.1)", 
                                        "GWD-in (0.6)", "GWD-out (3.5)", "GWDSP (out-two path, 0.3)", "GWDSP (in-two path, 0.2)",
                                        "consistency motivation (in-ties)", "consistency motivation (out-ties)",
                                        "understanding motivation (in-ties)", "understanding motivation (out-ties)", 
                                        "hedonic motivation (in-ties)", "hedonic motivation (out-ties)", 
                                        "ideology (conservatism, in-ties)", "ideology (conservatism, out-ties)", 
                                        "heterophily in ideology", "candidate pref (in-ties)", "candidate pref (out-ties)",
                                        "homophily in candidate pref", "heterophily in policy pref", "evaluative criteria heterophily", 
                                        "reciprocity within same candidate pref", "transitivity within same candidate pref", "cyclic closure within same candidate pref",
                                        "evaluative criteria X consistency", "evaluative criteria X understanding",
                                        "out-0-degree", "edgewise shared partner = 0"),
                  custom.note = " * 0 outside the 95% confidence interval based on 1000 replications", 
                  reorder.coef = c(1:15, 50:51, 16:49),
                  groups = list("Controls" = 1:17, "Endogenous structural effects" = 18:32,
                                "Discussion motivations" = 33:38, 
                                "Homophily" = 39:46, "Interactions" = 47:51),
                file = "results.Table.doc")
                                
