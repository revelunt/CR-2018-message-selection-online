
## script for paper entitled as 
## "Effects of motivation, homophily, and endogenous network process on message exposure within online discussion forum"

options(scipen = 999)
require(ergm)
# save(g, g_relrecip, g_autoregression, policy.pref.diff, evaludative.criteria.diff, file = "R_data/ergm.test.Rdata")
setwd("~/Dropbox/(17) 2017 Spring/network QAP Korean election")
source("data prep.R")

## --------------------------- ##
## Bootstrapped TERGM analysis ##
## ----------------------------##

require(btergm)
RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

## few global options
gof.statistics <- c(dsp, esp, odeg, ideg, geodesic, triad.directed)
R <- 1000
parallel <- "multicore"
ncpus <- 10
gof_ncpus <- 4


## first, start with control only model
model1 <- btergm(g ~ edges +
                   
                   nodeicov("age") +
                   nodeocov("age") + 
                   nodeifactor("gender") +
                   nodeofactor("gender") +
                   nodematch("gender") + ## control for demographic homophily
                   nodeicov("edu") +
                   nodeocov("edu") +
                   nodeicov("talk.freq") + ## those who talks a lot offline will have a higher activity
                   nodeocov("talk.freq") +
                   nodeicov("media.use.freq") + ## those who use media a lot will have a higher activity
                   nodeocov("media.use.freq") +
                   nodecov("internal.efficacy") + ## those with more efficacy would select more and be selected more by others
                   nodecov("external.efficacy") + ## those with more efficacy would select more and be selected more by others
                   # 1 = Seoul, 2 = Busan, Ulsan & Kungnam, 3 = Tague & Kungbuk, 5 = Kwangju & Junnam/buck
                   nodeifactor("region_origin", base = c(4,6,7)) + 
                   nodeofactor("region_origin", base = c(4,6,7)) +
                   nodematch("region_origin", diff = T, keep = c(1,2,3,5)),
                   
                 verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(model1)

save(model1, file = "June23.model1.Rdata")

## next, we add pure structural effects
model2 <- btergm(update.formula(model1@formula, ~ . + 
                                  
                    ## lagged effect
                    edgecov(g_autoregression) +  ## previous communication within dyads ("autoregression")
                    edgecov(g_delrecip) + ## delayed reciprocity
                    edgecov(g_lagtransitivity) + ## delayed transivity
                    edgecov(g_lagcyclic) + ## delayed cylic closure
                    nodeocov("lagged.sender.effect") + ## persistent sender effect
                    nodeicov("lagged.receiver.effect") + ## persistent receiver effect
                    
                    ## concurrent effect
                    ## nodal effect
                    isolates + ## below-threshold activities 
                    
                    ## dyadic level
                    mutual + ## reciprocity. expected to be positive
                   
                    ## triadic level
                    dgwesp(decay = 1.5, fixed = T, type = "OTP") + ## transitive closure. expected to be positive
                    dgwesp(decay = 1.5, fixed = T, type = "ITP") + ## cyclic closure. expected to be negative
                    ## combined, represent highly hierarchical network structure 
                    
                    ## structural homophily
                    dgwesp(decay = 1.5, fixed = T, type = "OSP") + ## number of common "posters" (similarity of out-tie) between dyad
                    dgwesp(decay = 1.5, fixed = T, type = "ISP") + ## common "viewer" (similarity of in-ties) between dyad
                    
                    ## control for gwesp
                    dgwdsp(decay = 1, fixed = T, type = "ITP") + 
                    dgwdsp(decay = 1, fixed = T, type = "OSP") + 
                    dgwdsp(decay = 1, fixed = T, type = "ISP") + 
                    
                    ## hub/star structure
                    ## preferential attachments for receiving and sending ties 
                    gwodegree(decay = 2.5, fixed = T) + gwidegree(decay = 3, fixed = T)),
   
                 verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(model2)

save(model2, file = "June24.model2.Rdata")

## add motivations and dyadic homophily predictors
model3 <- btergm(update.formula(model2@formula, ~ . +

                    ## discussion motivations
                    nodeicov("consistency.motivation") +
                    nodeocov("consistency.motivation") +
                    nodeicov("understanding.motivation") +
                    nodeocov("understanding.motivation") +
                    nodeicov("hedomic.motivation") +
                    nodeocov("hedomic.motivation") +

                    ## political homophily predictors
                    nodeicov("candidate.preference") +
                    nodeocov("candidate.preference") +
                    nodematch("candidate.preference") +

                    edgecov(policy.pref.sim) + ## policy preference ED metric-based similarity.
                    edgecov(evaludative.criteria.sim)), ## candidate evaluative criteria ED metric-based similarity
   
                 verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(model3)

save(model3, file = "June24.model3.Rdata")

## here we estimate a goodness of fit for this interim model.
model3.gof <- gof(model3, statistics = gof.statistics,
                  nsim = 300, verbose = TRUE, ncpus = gof_ncpus, parallel = parallel)
plot(model3.gof, mfrow = FALSE, xlim = 20)
save(model3.gof, file = "June24.model3.gof.Rdata")


## all models so far at once
texreg::screenreg(list(model1, model2, model3), single.row = T)

## eliminate insignificant interactions and tweak few things 
final.model <- btergm(update.formula(model3@formula, ~ .
                                     
                    - nodeifactor("region_origin", base = c(4,6,7))  
                    - nodeofactor("region_origin", base = c(4,6,7)) 
                    - nodematch("region_origin", diff = T, keep = c(1,2,3,5))
                          
                    + nodeifactor("region_origin2")
                    + nodeofactor("region_origin2")
                    + nodematch("region_origin2")
                                
                    - dgwesp(decay = 2, fixed = T, type = "OTP")

                    ),
                      
               verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(final.model) 

save(final.model, file = "June24.final.model.Rdata")
texreg::screenreg(list(model1, model2, model3, final.model), single.row = T)

final.model.gof <- gof(final.model, statistics = gof.statistics, 
                       nsim = 300, verbose = TRUE, ncpus = gof_ncpus, parallel = parallel)  
save(final.model.gof, file = "June24.final.model.gof.Rdata")


pdf(file = "final.model.gof.pdf", width = 14, height = 10)
plot(final.model.gof$`Dyad-wise shared partners`, xlim = 20)
plot(final.model.gof$`Edge-wise shared partners`, xlim = 20)
plot(final.model.gof$Outdegree, xlim = 20)
plot(final.model.gof$Indegree, xlim = 20)
plot(final.model.gof$`Geodesic distances`)
dev.off()

save(model1, model2, model3, final.model, file = "R_data/btergm.results.June 24th.modified.Rdata")
save(model3.gof, final.model.gof, file = "R_data/btergm.gof.results.June 24th.modified.Rdata")

## degeneracy check
final.model.checkdegeneracy <- checkdegeneracy(final.model, nsim = 1000, 
                MCMC.interval = 1000, MCMC.burnin = 10000, verbose = FALSE)


# ## further optimize the structural terms
# final.model2 <- btergm(g ~ edges + 
#                          nodeicov("age") + nodeocov("age") + 
#                          nodeifactor("gender") + nodeofactor("gender") + nodematch("gender") + 
#                          nodeicov("edu") + nodeocov("edu") + 
#                          nodeicov("talk.freq") + nodeocov("talk.freq") +
#                          nodeicov("media.use.freq") + nodeocov("media.use.freq") +
#                          nodecov("internal.efficacy") + nodecov("external.efficacy") +
#                          nodeifactor("region_origin2") + nodeofactor("region_origin2") + nodematch("region_origin2") +
#                          
#                          edgecov(g_autoregression) + edgecov(g_delrecip) + edgecov(g_lagtransitivity) +
#                          edgecov(g_lagcyclic) + #nodeofactor("source") + nodeifactor("sink") +
#                          nodeocov("lagged.sender.effect") +  ## persistent sender effect
#                          nodeicov("lagged.receiver.effect") + 
#                          isolates + mutual + #idegreepopularity + odegreepopularity +
#                          
#                          dgwdsp(decay = 1, fixed = T, type = "ITP") + 
#                          dgwdsp(decay = 1, fixed = T, type = "OSP") + 
#                          dgwdsp(decay = 1, fixed = T, type = "ISP") +
#                          
#                          dgwesp(decay = 1.5, fixed = T, type = "ITP") + 
#                          dgwesp(decay = 1.5, fixed = T, type = "OSP") + 
#                          dgwesp(decay = 1.5, fixed = T, type = "ISP") + 
#                          gwodegree(decay = 2.5, fixed = T) + gwidegree(decay = 3, fixed = T) +
#                          
#                          nodeicov("consistency.motivation") + nodeocov("consistency.motivation") +
#                          nodeicov("understanding.motivation") + nodeocov("understanding.motivation") +
#                          nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") +
#                          nodeicov("candidate.preference") + nodeocov("candidate.preference") +
#                          nodematch("candidate.preference") + edgecov(policy.pref.sim) +
#                          edgecov(evaludative.criteria.sim),
#                          R = 100, ncpus = 4, parallel = "multicore")
# 
#                        summary(final.model2)
# 
#  final.model2.gof <- gof(final.model2, statistics = gof.statistics, 
#                         nsim = 333, verbose = TRUE, ncpus = gof_ncpus, parallel = parallel) 
#  
#  plot(final.model2.gof$`Dyad-wise shared partners`, xlim = 20)
#  plot(final.model2.gof$`Edge-wise shared partners`, xlim = 20)
#  plot(final.model2.gof$Outdegree, xlim = 20)
#  plot(final.model2.gof$Indegree, xlim = 20)
#  plot(final.model2.gof$`Geodesic distances`)
#  plot(final.model2.gof$`Tie prediction`)
#  
#  save(final.model2, file = "June23.final.model2.Rdata")
#  save(final.model2.gof, file = "June23.final.model2.gof.Rdata")



## all models to a file
texreg::htmlreg(list(model1, model2, model3, final.model), digits = 3, leading.zero = F, single.row = T,
                  custom.model.names = c("Control only", "Structural only", "Homophily", "Final Model"),
                  custom.coef.names = c("Edges (Intercept)", "Age (in-ties)", "Age (out-ties)",
                                        "Female (in-ties)", "Female (out-ties)", "Gender homophily",
                                        "Edu (in-ties)", "Edu (out-ties)",
                                        "Talk freq (in-ties)", "Talk freq (out-ties)", 
                                        "Media use (in-ties)", "Media use (out-ties)",
                                        "Internal efficacy", "External efficacy",
                                        "Regional origin = Seoul (in-ties)", "Regional origin = PK (in-ties)",
                                        "Regional origin = TK (in-ties)", "Regional origin = Honam (in-ties)",
                                        "Regional origin = Seoul (out-ties)", "Regional origin = PK (out-ties)",
                                        "Regional origin = TK (out-ties)", "Regional origin = Honam (out-ties)",
                                        "Regional homophily (Seoul)", "Regional homophily (PK)",
                                        "Regional homophily (TK)", "Regional homophily (Honam)",
                                        
                                        "Previous communication", "Delayed reciprocity", "Delayed transitivity", 
                                        "Delayed cyclic closure", "Persistent sender (out-tie)", "Persistent receiver (in-ties)",
                                        "Isolates", "reciprocity", "GWESP (out-two path, 1.5)", "GWESP (in-two path, 1.5)", 
                                        "GWESP (out-shared partner, 1.5)", "GWESP (in-shared partner, 1.5)",
                                        "GWDSP (in-two path, 1)", "GWDSP (out-shared partner, 1)", "GWDSP (in-shared partner, 1)",
                                        "GW-outdegree (2.5)", "GW-indegree (3)",
                                        "Consistency (in-ties)", "Consistency (out-ties)",
                                        "Understanding (in-ties)", "Understanding (out-ties)", 
                                        "Hedonic (in-ties)", "Hedonic (out-ties)", 
                                        "Candidate pref (in-ties)", "Candidate pref (out-ties)", "Same candidate pref", 
                                        "Similar policy pref", "Similar evaluative criteria", 
                                        "Regional origin = Seoul (in-ties)", "Regional origin = Seoul (out-ties)", 
                                        "Regional homophily (Seoul)"),
                  custom.note = " * 0 outside the 95% confidence interval based on 1000 replications", 
                  reorder.coef = c(44:54, 1:43),
                  groups = list("Motivation and Homophily" = 1:11, 
                                "Controls" = 12:37,
                                "Lagged structural effect" = 38:43,
                                "Endogenous structural effects" = 44:54),
                  bold = 0.5, doctype = T, html.tag = T, body.tag = T, indentation = "  ",
                  caption = "Statistical models",
                file = "results.Table.June25.doc")


## produce coefficient-CI plot

is.between <- function(x, lower, upper) {x > lower & x < upper}

ggdat_fig <- data.table(
  var = names(final.model@coef),
  coef = coef(final.model),
  q025 = apply(final.model@boot$t, 2, quantile, .025),
  q975 = apply(final.model@boot$t, 2, quantile, .975))

ggdat_fig <- ggdat_fig[c(1:14, 43:45, 32:42, 15:31)]

y <- c("Edges \n(intercept)", "Age in 10 years \n(in-ties)", "Age in 10 years \n(out-ties)", 
       "Gender: female \n(in-ties)", "Gender: female \n(out-ties)", "Gender \nhomophily", 
       "Education \n(in-ties)", "Education \n(out-ties)", 
       "Offline talk freq \n(in-ties)", "Offline talk freq \n(out-ties)", 
       "Media use freq \n(in-ties)", "Media use freq \n(out-ties)", 
       "Internal \nefficacy", "External \nefficacy", 
       "Origin = Seoul \n(in-ties)", "Origin = Seoul \n(out-ties)", "Origin = Seoul \nhomophily", 
       "Motivation \nconsistency (in-tie)", "Motivation \nconsistency (out-tie)", 
       "Motivation \nunderstanding (in-tie)", "Motivation \nunderstanding (out-tie)", 
       "Motivation \nhedonic (in-tie)", "Motivation \nhedonic (out-tie)",
       "Candidate preference \n(Moon, in-tie)", "Candidate preference \n(Moon, out-tie)", 
       "Candidate preference \nhomophily", "Policy preference \nhomophily", "Evaluative criteria \nhomophily",
       "Edge autoregression", "Delayed reciprocity", 
       "Delayed transitivity", "Delayed cylic closure", "Persistent sender", "Persistent receiver", 
       "Isolates", "Reciprocity",  
       "GWESP-OTP \n(alpha = 1.5)", "GWESP-ITP \n(alpha = 1.5)", "GWESP-OSP \n(alpha = 1.5)", "GWESP-ISP \n(alpha = 1.5)",
       "GWDSP-ITP \n(alpha = 1)", "GWDSP-OSP \n(alpha = 1)", "GWDSP-ISP \n(alpha = 1)",
       "GWD-outdegree \n(alpha = 2.5)", "GWD-indegree \n(alpha = 3)")

ggdat_fig[, y := factor(y, levels = rev(y))]
ggdat_fig[, sig := !(is.between(0, q025, q975))]

pdf(file = "final.model.coef.pdf", width = 14, height = 10)
## control variables
ggplot(ggdat_fig[2:17,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point() + geom_errorbarh(height = 0) +
  xlab("Coefficient") + ylab("") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none") +
  scale_colour_manual(values = c("grey", "red"))

## pure structural effects
ggplot(ggdat_fig[29:43,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point() + geom_errorbarh(height = 0) +
  xlab("Coefficient") + ylab("") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none") + 
  scale_colour_manual(values = c("grey", "red")) 

## discussion motivation and homophily effects
ggplot(ggdat_fig[18:28,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point() + geom_errorbarh(height = 0) +
  xlab("Coefficient") + ylab("") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none") + 
  scale_colour_manual(values = c("grey", "red"))
dev.off()
## alternatively, use plotreg in texreg packages
plotreg(final.model, omit.coef = "(edges)|(gwidegree)|(gwodegree)|(isolates)")



## interpretation of the homophily terms
source("btergm helper-functions.R")

test <- compute_dyadic_probabilities_btergm(final.model, "nodematch.candidate.preference", 1.16, n_sample = 50)
test <- calc_prob_diffs(test)
# create barplots from probability ratios and CIs
require("gplots")
bp <- barplot2(test[1:3, ], beside = TRUE, plot.ci = TRUE, 
               ci.l = test[4:6, ], ci.u = test[7:9, ], 
               col = c("tan", "tan2", "tan3"), ci.col = "grey40", 
               xlab = "Dyadic tie values", ylab = "Estimated Prob. minus Null Prob.")
mtext(1, at = bp, text = c("No ties", "Asymmetric tie", "Reciprocated ties"), line = 0, cex = 0.5)


## stuructural effects
## derive predicted probability plot using final model
## nodes who have exactly indegree of 0 to 9
degree.num <- 0:9
receiver.list.t1 <- lapply(degree.num, function(i) which(sna::degree(g[[1]], cmode="indegree") == i))
receiver.list.t2 <- lapply(degree.num, function(i) which(sna::degree(g[[2]], cmode="indegree") == i))
receiver.list.t3 <- lapply(degree.num, function(i) which(sna::degree(g[[3]], cmode="indegree") == i))

edgeprob.test <- btergm::edgeprob(final.model)
setDT(edgeprob.test)
index <- 1:312

out.t1 <- lapply(seq_len(length(receiver.list)), function(m) {
  d <- receiver.list.t1[[m]]
  out <- sapply(seq_len(length(d)), function(k) {
    index.to.sample <- index[-(network::get.neighborhood(g[[1]], d[k], "in"))]
    if (m == 1) {index.to.sample <- index}
    index.to.sample <- setdiff(index.to.sample, d[k])
    
    edgeprob.test[j==d[k] & (i %in% index.to.sample) & t == 1, probability]})
})
out.t2 <- lapply(seq_len(length(receiver.list)), function(m) {
  d <- receiver.list.t2[[m]]
  out <- sapply(seq_len(length(d)), function(k) {
    index.to.sample <- index[-(network::get.neighborhood(g[[2]], d[k], "in"))]
    if (m == 1) {index.to.sample <- index}
    index.to.sample <- setdiff(index.to.sample, d[k])
    
    edgeprob.test[j==d[k] & (i %in% index.to.sample) & t == 2, probability]})
})
out.t3 <- lapply(seq_len(length(receiver.list)), function(m) {
  d <- receiver.list.t3[[m]]
  out <- sapply(seq_len(length(d)), function(k) {
    index.to.sample <- index[-(network::get.neighborhood(g[[3]], d[k], "in"))]
    if (m == 1) {index.to.sample <- index}
    index.to.sample <- setdiff(index.to.sample, d[k])
    
    edgeprob.test[j==d[k] & (i %in% index.to.sample) & t == 3, probability]})
})

mean.pb <- data.frame(time = paste0("time ", rep(1:3, each = 10)),
                      indegree = rep(0:9, times = 3),
                      mean.pb = c(unlist(lapply(1:length(out.t1), function(i) mean(out.t1[[i]]))),
                                  unlist(lapply(1:length(out.t2), function(i) mean(out.t2[[i]]))),
                                  unlist(lapply(1:length(out.t3), function(i) mean(out.t3[[i]])))),
                      se = c(unlist(lapply(1:length(out.t1), function(i) sd(out.t1[[i]])/sqrt(length(out.t1[[i]])))),
                             unlist(lapply(1:length(out.t2), function(i) sd(out.t2[[i]])/sqrt(length(out.t2[[i]])))),
                             unlist(lapply(1:length(out.t3), function(i) sd(out.t3[[i]])/sqrt(length(out.t3[[i]])))))
)

ggplot(mean.pb, aes(x = indegree, y = mean.pb, fill = time)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = mean.pb - se, ymax = mean.pb + se),
                width = .1, position = position_dodge(.9)) + 
  xlab("Indegree") + ylab("P(at least one additional tie)") +
  theme_bw() + scale_fill_manual(values = c("grey80", "grey", "grey40")) + 
  scale_x_continuous(breaks = 0:9, labels = paste0("(", 0:9, ")"))

