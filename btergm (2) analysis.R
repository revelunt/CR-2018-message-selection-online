
## script for paper entitled as 
## "Effects of motivation, homophily, and endogenous network process on message exposure within online discussion forum"

options(scipen = 999)
require(ergm)
require(btergm)
#setwd("~/Dropbox/GitHub/Korean2012ElectionProject")
source("dev/btergm helper-functions.R")
source("dev/btergm (1) data prep.R")


RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

## --------------------------- ##
## Bootstrapped TERGM analysis ##
## ----------------------------##


## few global options
gof.statistics <- c(dsp, odeg, ideg, desp_OTP, desp_ITP, desp_OSP, desp_ISP,
                    geodesic, triad.directed, rocpr, walktrap.modularity)
R <- 1000
parallel <- "multicore"
ncpus <- 10
gof_ncpus <- 4


## first, start with control only model
model1 <- btergm(g ~ edges + ## intercept
                   
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
                   nodecov("internal.efficacy"),
                   
                 verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(model1)

save(model1, file = "R_results/July19.model1.Rdata")

## next, we add pure structural effects
model2 <- btergm(update.formula(model1@formula, ~ . + 
                                  
                                  ## endogenous and lagged structural, control
                                  isolates + mutual + 
                                  edgecov(g_autoregression) + 
                                  gwdsp(decay = 1, fixed = T) + 
                          
                                  
                                  ## lagged structural, control
                                  edgecov(g_delrecip) + 
                                  edgecov(g_lagtransitivity) + ## lagged gwesp_OTP
                                  edgecov(g_lagcyclic) + ## lagged gwesp_ITP
                                  edgecov(g_lag_shared_activity) + ## lagged gwesp_OSP
                                  edgecov(g_lag_shared_popularity) + ## lagged gwesp_ISP
                                  nodeocov("lagged.sender.effect") + 
                                  nodeicov("lagged.receiver.effect") + 
                                  
                                  
                                  ## endogenous structural
                                  dgwesp(decay = 3, fixed = T, type = "OTP") + ## understanding
                                  dgwesp(decay = 3, fixed = T, type = "ITP") + ## understanding
                                  dgwesp(decay = 3, fixed = T, type = "OSP") + ## consistency
                                  dgwesp(decay = 2, fixed = T, type = "ISP") + ## consistency
                                  
                                  gwodegree(decay = 2, fixed = T) + ## hedonic
                                  gwidegree(decay = 3, fixed = T)), ## hedonic 
   
                 verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(model2)

save(model2, file = "R_results/July19.model2.Rdata")

## add motivations and dyadic homophily predictors
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
                   nodecov("internal.efficacy") + 
                   
                   ## individual, motivation factor
                   nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
                   nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
                   nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
                  
                   ## dyadic, consistency
                   nodeicov("candidate.preference") + nodeocov("candidate.preference") + 
                   nodematch("candidate.preference") + 
                   edgecov(policy.pref.sim) +
                   
                   ## dyadic, understanding
                   edgecov(evaludative.criteria.sim) +
                   
                   ## endogenous and lagged structural, control
                   isolates + mutual + 
                   edgecov(g_autoregression) + 
                   gwdsp(decay = 1, fixed = T) + 
                   
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
                   
                   gwodegree(decay = 2, fixed = T) + ## hedonic
                   gwidegree(decay = 3, fixed = T), ## hedonic 
                 
                 verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(final.model)

save(final.model, file = "R_results/July19.final.model.Rdata")

## here we estimate a goodness of fit for this interim model.
final.model.gof <- gof(final.model, statistics = gof.statistics, 
                  nsim = 300, verbose = TRUE, ncpus = gof_ncpus, parallel = parallel)
plot(final.model.gof, mfrow = FALSE, xlim = 40)
save(final.model.gof, file = "July19.final.model.gof.Rdata")


## all models so far at once
texreg::screenreg(list(model1, model2, final.model), single.row = T)

pdf(file = "final.model.gof.pdf", width = 14, height = 10)

plot(final.model.gof$`Dyad-wise shared partners`, xlim = 40, relative = F)
plot(final.model.gof$Outdegree, xlim = 40, relative = F)
plot(final.model.gof$Indegree, xlim = 40, relative = F)
plot(final.model.gof$`Directed edge-wise shared partners of type OTP`, xlim = 40, relative = F)
plot(final.model.gof$`Directed edge-wise shared partners of type ITP`, xlim = 40, relative = F)
plot(final.model.gof$`Directed edge-wise shared partners of type OSP`, xlim = 40, relative = F)
plot(final.model.gof$`Directed edge-wise shared partners of type ISP`, xlim = 40, relative = F)
plot(final.model.gof$`Geodesic distances`)
plot(final.model.gof$`Triad census`)
plot(final.model.gof$`Tie prediction`)
plot(final.model.gof$`Modularity (walktrap)`)
dev.off()

save(model1, model2, final.model, file = "R_results/btergm.results.July 19th.Rdata")
save(final.model.gof, file = "R_results/btergm.gof.results.July 19th.Rdata")


## all models to a file
texreg::htmlreg(list(model1, model2, final.model), digits = 3, leading.zero = F, single.row = T,
                  custom.model.names = c("Control only", "Control + Structural", "Final Model"),
                  custom.coef.names = c("Edges (Intercept)", "Age (in-ties)", "Age (out-ties)",
                                        "Female (in-ties)", "Female (out-ties)", "Gender homophily",
                                        "Education (in-ties)", "Education (out-ties)",
                                        "Regional origin = Seoul (in-ties)",
                                        "Regional origin = Seoul (out-ties)",
                                        "Regional homophily (Seoul)",
                                        "Talk freq (in-ties)", "Talk freq (out-ties)", 
                                        "Media use (in-ties)", "Media use (out-ties)",
                                        "Internal efficacy", 
                                        "Isolates", "Reciprocity", "Previous communication",
                                        "Multiple two-paths (GWDSP, 1)", 
                                        "Delayed reciprocity", 
                                        "Delayed transitivity closure", "Delayed cyclic closure", 
                                        "Delayed activity closure", "Delayed popularity closure",
                                        "Persistent sender (out-tie)", "Persistent receiver (in-ties)",
                                        "Multiple path closure (GWESP-OTP, 3)", "Multiple cyclic closure (GWESP-ITP, 3)", 
                                        "Multiple activity closure (GWESP-OSP, 3)", "Multiple popularity closure (GWESP-ISP, 2)",
                                        "Activity spread (GW-outdegree, 2)", "Popularity spread (GW-indegree, 3)",
                                        "Consistency motivation (in-ties)", "Consistency motivation (out-ties)",
                                        "Understanding motivation (in-ties)", "Understanding motivation (out-ties)", 
                                        "Hedonic motivation (in-ties)", "Hedonic motivation (out-ties)", 
                                        "Candidate pref = Moon (in-ties)", "Candidate pref = Moon (out-ties)", "Same candidate pref", 
                                        "Similar policy pref", "Similar evaluative criteria"),
                  custom.note = " * 0 outside the 95% confidence interval based on 1000 replications", 
                  reorder.coef = c(1, 34:44, 17:18,28:31,20,32:33, 19,21:27, 2:16),
                  groups = list("Motivation and Homophily" = 2:12, 
                                "Endogenous structural effects" = 13:21,
                                "Lagged structural effects" = 22:29,
                                "Controls" = 30:44),
                  bold = 0.5, doctype = T, html.tag = T, body.tag = T, indentation = "  ",
                  caption = "",
                file = "results.Table.July20.doc")


## --------------------------- ##
## produce coefficient-CI plot ##
## --------------------------- ##


ggdat_fig <- data.table(
  var = names(final.model@coef),
  coef = coef(final.model),
  q025 = apply(final.model@boot$t, 2, quantile, .025),
  q975 = apply(final.model@boot$t, 2, quantile, .975))

ggdat_fig <- ggdat_fig[c(2:16, 17:27, 30,32:38, 1,28:29,31,39:44)]

y <- c("Age in 10 years \n(in-ties)", "Age in 10 years \n(out-ties)", 
       "Gender: female \n(in-ties)", "Gender: female \n(out-ties)", "Gender \nhomophily", 
       "Education \n(in-ties)", "Education \n(out-ties)", 
       "Origin = Seoul \n(in-ties)", "Origin = Seoul \n(out-ties)", "Origin = Seoul \nhomophily", 
       "Offline talk freq \n(in-ties)", "Offline talk freq \n(out-ties)", 
       "Media use freq \n(in-ties)", "Media use freq \n(out-ties)", 
       "Internal \nefficacy", 
       "Motivation \nconsistency (in-tie)", "Motivation \nconsistency (out-tie)", 
       "Motivation \nunderstanding (in-tie)", "Motivation \nunderstanding (out-tie)", 
       "Motivation \nhedonic (in-tie)", "Motivation \nhedonic (out-tie)",
       "Candidate preference \n(Moon, in-tie)", "Candidate preference \n(Moon, out-tie)", 
       "Candidate preference \nhomophily", "Policy preference \nhomophily", "Evaluative criteria \nhomophily",
       "Edge autoregression", "Delayed reciprocity", 
       "Delayed \ntransitivity closure", "Delayed \ncylic closure", 
       "Delayed \nactivity closure", "Delayed \npopularity closure",
       "Persistent sender", "Persistent receiver", 
       "Edges (intercept)", "Isolates", "Reciprocity",  
       "Multiple two-paths \n(GWDSP, 1)", 
       "Multiple path closure \n(GWESP-OTP, 3)", "Multiple cyclic closure \n(GWESP-ITP, 3)", 
       "Multiple activity closure \n(GWESP-OSP, 3)", "Multiple popularity closure \n(GWESP-ISP, 2)",
       "Activity spread \n(GW-outdegree, 2)", "Popularity spread \n(GW-indegree, 3)")

ggdat_fig[, y := factor(y, levels = rev(y))]
ggdat_fig[, sig := !(is.between(0, q025, q975))]

require(gridExtra)
pdf(file = "final.model.coef.pdf", width = 14, height = 10)
## control variables
p1 <- ggplot(ggdat_fig[1:15,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point(size = 2.5) + geom_errorbarh(height = 0, size = 1.5) +
  xlab("Coefficient") + ylab("") + ggtitle("Demographics and controls") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(values = c("grey", "red"))

## discussion motivation and homophily effects
p2 <- ggplot(ggdat_fig[16:26,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point(size = 2.5) + geom_errorbarh(height = 0, size = 1.5) +
  xlab("Coefficient") + ylab("") + ggtitle("Motivation and homophily") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +  
  scale_colour_manual(values = c("grey", "red"))

## pure lagged structural effects
p3 <- ggplot(ggdat_fig[27:34,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point(size = 2.5) + geom_errorbarh(height = 0, size = 1.5) +
  xlab("Coefficient") + ylab("") + ggtitle("Lagged structural effects") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(values = c("grey", "red")) 

## pure concurrent structural effects
p4 <- ggplot(ggdat_fig[35:44,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point(size = 2.5) + geom_errorbarh(height = 0, size = 1.5) +
  xlab("Coefficient") + ylab("") + ggtitle("Concurrent structural effects") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(values = c("grey", "red")) 

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
dev.off()
## alternatively, use plotreg in texreg packages
## plotreg(final.model, omit.coef = "(edges)|(gwidegree)|(gwodegree)|(isolates)")



## interpretation of the homophily terms

test <- compute_dyadic_probabilities_btergm(final.model, "nodematch.candidate.preference", 0.41, n_sample = 50)
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

out.t1 <- lapply(seq_len(length(receiver.list.t1)), function(m) {
  d <- receiver.list.t1[[m]]
  out <- sapply(seq_len(length(d)), function(k) {
    index.to.sample <- index[-(network::get.neighborhood(g[[1]], d[k], "in"))]
    if (m == 1) {index.to.sample <- index}
    index.to.sample <- setdiff(index.to.sample, d[k])
    
    edgeprob.test[j==d[k] & (i %in% index.to.sample) & t == 1, probability]})
})
out.t2 <- lapply(seq_len(length(receiver.list.t2)), function(m) {
  d <- receiver.list.t2[[m]]
  out <- sapply(seq_len(length(d)), function(k) {
    index.to.sample <- index[-(network::get.neighborhood(g[[2]], d[k], "in"))]
    if (m == 1) {index.to.sample <- index}
    index.to.sample <- setdiff(index.to.sample, d[k])
    
    edgeprob.test[j==d[k] & (i %in% index.to.sample) & t == 2, probability]})
})
out.t3 <- lapply(seq_len(length(receiver.list.t3)), function(m) {
  d <- receiver.list.t3[[m]]
  out <- sapply(seq_len(length(d)), function(k) {
    index.to.sample <- index[-(network::get.neighborhood(g[[3]], d[k], "in"))]
    if (m == 1) {index.to.sample <- index}
    index.to.sample <- setdiff(index.to.sample, d[k])
    
    edgeprob.test[j==d[k] & (i %in% index.to.sample) & t == 3, probability]})
})

mean.pb <- data.frame(time = paste0("time ", rep(1:3, each = 10)),
                      indegree = rep(0:9, times = 3),
                      mean.pb = c(unlist(lapply(1:length(out.t1), function(i) mean(out.t1[[i]], na.rm = T))),
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



## -------------------------------------------- ##
## Comparion with MRQAP and non-threshold model ##
## -------------------------------------------- ##

# source("MRQAP 20170718.R")
# source("btergm (2) analysis _ no threshold.R")

load("MRQAP.model.2017.07.18.Rdata")
load("final.model.nothreshold.July20.Rdata")

print(MRQAP.model)
summary(final.model.nothreshold)

tr.qap <- extract.netlm(MRQAP.model)

tr.qap@coef.names <- c("edges", 
                       "nodeicov.consistency.motivation",
                       "nodeocov.consistency.motivation",
                       "nodeicov.understanding.motivation",
                       "nodeocov.understanding.motivation",
                       "nodeicov.hedomic.motivation",
                       "nodeocov.hedomic.motivation",
                       "nodeicov.candidate.preference",
                       "nodeocov.candidate.preference",
                       "nodematch.candidate.preference",
                       "edgecov.policy.pref.sim[[i]]",
                       "edgecov.evaludative.criteria.sim[[i]]",
                       "nodeicov.age",
                       "nodeocov.age",
                       "nodeifactor.gender.1",
                       "nodeofactor.gender.1",
                       "nodematch.gender",
                       "nodeicov.edu",
                       "nodeocov.edu",
                       "nodeicov.talk.freq",
                       "nodeocov.talk.freq",
                       "nodeicov.media.use.freq",
                       "nodeocov.media.use.freq",
                       "nodecov.internal.efficacy",
                       "nodecov.external.efficacy",
                       "nodeifactor.region_origin2.1",
                       "nodeofactor.region_origin2.1",
                       "nodematch.region_origin2",
                       "edgecov.g_autoregression[[i]]",
                       "edgecov.g_delrecip[[i]]",
                       "mutual")


screenreg(list(tr.qap, final.model, final.model.nothreshold), digits = 3, 
          leading.zero = F, single.row = T,
          custom.model.names = c("MRQAP Model", "BTERG Model", "BTERGM non-threshold"),
          custom.coef.names = c(tr.qap@coef.names,
                                names(final.model@coef)[17:21],
                                "gwesp.OTP", "gwesp.ITP", "gwesp.OSP", "gwesp.ISP",
                                "gwdsp.ITP", "gwdsp.OSP", "gwdsp.ISP",
                                names(final.model@coef)[30:31],
                                "gwesp.OTP", "gwesp.ITP", "gwesp.OSP", "gwesp.ISP",
                                "gwdsp.ITP", "gwdsp.OSP", "gwdsp.ISP"))

## goodness of fit for MRQAP model

predictor.matrix.gof <- list()
dim <- dim(predictor.matrix)[1]
for (i in 1:dim) {
  predictor.matrix.gof[[i]] <- as.matrix(predictor.matrix[i, , ])
}

MRQAP.gof <- gof(net, predictor.matrix.gof, coef(MRQAP.model), statistics = gof.statistics, 
    nsim = 1000, verbose = TRUE, ncpus = gof_ncpus, parallel = parallel)  
