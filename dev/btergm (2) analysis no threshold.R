
## MODEL robustness check
options(scipen = 999)
require(ergm)
require(btergm)
# setwd("~/Dropbox/(17) 2017 Spring/network QAP Korean election")
source("btergm helper-functions.R")

## 1. no thresholded model
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
           gwdsp(decay = 1.5, fixed = T) + 
           
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
           gwidegree(decay = 3, fixed = T), ## hedonic 
         R = 1000, parallel = "multicore", ncpus = 10)

save(final.model.nothreshold, file = "final.model.nothreshold.Rdata")

gof.statistics <- c(dsp, odeg, ideg, esp, desp_OTP, desp_ITP, desp_OSP, desp_ISP,
                    geodesic, triad.directed, rocpr, walktrap.modularity)

final.model.nothreshold.gof <- gof(final.model.nothreshold, nsim = 300, 
                                   statistics = gof.statistics, parallel = "multicore", ncpus = 4)

plot(final.model.nothreshold.gof, mfrow = F, xlim = 40)

# Estimates and 95% confidence intervals:
#                                          Estimate    2.5%   97.5%
# edges                                 -0.298477499 -0.8514 -0.2455
# nodeicov.age                          -0.018025736 -0.0432  0.0113
# nodeocov.age                           0.039806622  0.0094  0.0550
# nodeifactor.gender.1                   0.019503625 -0.0089  0.0775
# nodeofactor.gender.1                  -0.013183369 -0.0936  0.0572
# nodematch.gender                       0.016033376 -0.0104  0.0421
# nodeicov.edu                          -0.008456863 -0.0204  0.0052
# nodeocov.edu                          -0.021875144 -0.0385  0.0227
# nodeifactor.region_origin2.1          -0.075625689 -0.1343 -0.0198
# nodeofactor.region_origin2.1           0.052078366  0.0160  0.1422
# nodematch.region_origin2               0.017061332 -0.0113  0.0742
# nodeicov.talk.freq                     0.041430840  0.0081  0.0619
# nodeocov.talk.freq                     0.021113806 -0.0290  0.0311
# nodeicov.media.use.freq               -0.007587022 -0.0314  0.0045
# nodeocov.media.use.freq               -0.000366238 -0.0149  0.1735
# nodecov.internal.efficacy              0.029050724  0.0084  0.0497
# nodeicov.consistency.motivation        0.018358319  0.0029  0.0504
# nodeocov.consistency.motivation       -0.022206295 -0.0569 -0.0016
# nodeicov.understanding.motivation     -0.061499848 -0.0852 -0.0165
# nodeocov.understanding.motivation      0.035402791  0.0331  0.0544
# nodeicov.hedomic.motivation            0.013919523 -0.0016  0.0316
# nodeocov.hedomic.motivation           -0.008975362 -0.0157  0.0260
# nodeicov.candidate.preference         -0.028200266 -0.1087  0.0100
# nodeocov.candidate.preference         -0.020259295 -0.0558  0.0227
# nodematch.candidate.preference         0.072596988  0.0595  0.0938
# edgecov.policy.pref.sim[[i]]           0.053643947 -0.0369  0.1006
# edgecov.evaludative.criteria.sim[[i]]  0.051185154  0.0090  0.0560
# isolates                               1.465428354  0.9256  2.2794
# mutual                                 0.901179738  0.7532  1.0057
# edgecov.g_autoregression[[i]]          0.239954456  0.1824  0.2589
# gwdsp.fixed.1.5                       -0.001074296 -0.0051  0.0009
# edgecov.g_delrecip[[i]]               -0.000072554 -0.0489  0.0630
# edgecov.g_lagtransitivity[[i]]        -0.005898102 -0.0085  0.0035
# edgecov.g_lagcyclic[[i]]              -0.001708884 -0.0048  0.0083
# edgecov.g_lag_shared_activity[[i]]    -0.001969754 -0.0055 -0.0009
# edgecov.g_lag_shared_popularity[[i]]  -0.013152648 -0.0162 -0.0113
# nodeocov.lagged.sender.effect          0.008938961  0.0049  0.0130
# nodeicov.lagged.receiver.effect        0.010425056  0.0075  0.0144
# gwesp.OTP.fixed.3                      0.020717608  0.0204  0.0311
# gwesp.ITP.fixed.3                     -0.007990299 -0.0155 -0.0016
# gwesp.OSP.fixed.3                      0.011325649  0.0073  0.0148
# gwesp.ISP.fixed.2                      0.010067321 -0.0003  0.0314
# gwodegree                             -4.096952047 -4.4426 -3.1948
# gwidegree                             -4.628015502 -4.8263 -4.3617

## imputed candidate preference at Wave 1

options(scipen = 999)

if(!("Amelia" %in% installed.packages()[,"Package"])) install.packages("Amelia")
require(Amelia)
require(ergm)
require(btergm)
require(texreg)
require(parallel)

# setwd("~/Dropbox/GitHub/Korean2012ElectionProject")
source("dev/btergm helper-functions.R")

## prepare data without subsetting nodes with missing covariates at W1 
source_lines("dev/btergm (1) data prep.R", 1:20)
source_lines("dev/btergm (1) data prep.R", 25:489)

## a total of 341 cases are included in this analysis (including 22 missing covariate cases)
## check the dimension of matrix
dim(as.matrix(g[[1]]))

##---------------------------------##
## create a dataset for imputation ##
##---------------------------------##

data_to_imp <- data.frame(
  ids = rep(network::network.vertex.names(g[[1]]), 3),
  waves = rep(1:3, each = 341),
  candidate.image.Park = c(dat[vids, pv194:pv208][, (Mean = rowMeans(.SD)), by = vids][,V1], 
                           dat[vids, kv7:kv21][, (Mean = rowMeans(.SD)), by = vids][,V1], 
                           dat[vids, hv32:hv46][, (Mean = rowMeans(.SD)), by = vids][,V1]),
  candidate.image.Moon = c(dat[vids, pv209:pv223][, (Mean = rowMeans(.SD)), by = vids][,V1], 
                           dat[vids, kv22:kv36][, (Mean = rowMeans(.SD)), by = vids][,V1], 
                           dat[vids, hv47:hv61][, (Mean = rowMeans(.SD)), by = vids][,V1]),
  candidate.preference = c(g[[1]] %v% "candidate.preference", g[[2]] %v% "candidate.preference", g[[3]] %v% "candidate.preference"),
  liberal.issue.stance = c(g[[1]] %v% "liberal.issue.stance", g[[2]] %v% "liberal.issue.stance", g[[3]] %v% "liberal.issue.stance"),
  conserv.issue.stance = c(g[[1]] %v% "conserv.issue.stance", g[[2]] %v% "conserv.issue.stance", g[[3]] %v% "conserv.issue.stance"),
  internal.efficacy = c(g[[1]] %v% "internal.efficacy", g[[2]] %v% "internal.efficacy", g[[3]] %v% "internal.efficacy"),
  media.use.freq = c(g[[1]] %v% "media.use.freq", g[[2]] %v% "media.use.freq", g[[3]] %v% "media.use.freq"),
  talk.freq = c(g[[1]] %v% "talk.freq", g[[2]] %v% "talk.freq", g[[3]] %v% "talk.freq")
)

## generate 10 imputed datasets using Amelia function, setting candidate.preference as nominal variable
set.seed(12345)
dat.imputed <- amelia(x = data_to_imp, m = 5, cs = "ids", ts = "waves", noms = "candidate.preference")

## check imputation patterns
imputation.pattern <- cbind(as.numeric(network.vertex.names(g[[1]])), 
                            (g[[1]] %v% "candidate.preference"), 
                            sapply(dat.imputed$imputations, function(i) i$candidate.preference[1:341]))
colnames(imputation.pattern) <- c("ids", "original values", paste("imp", 1:5))
imputation.pattern[!complete.cases(imputation.pattern),]

## check correlations across waves for time-varying variables
wide.dat <- reshape(data_to_imp, direction = 'wide', idvar = 'ids', timevar = 'waves')
setDT(wide.dat)

vars.to.check <- c("candidate.image.Park", "candidate.image.Moon", "candidate.preference", 
                   "liberal.issue.stance", "conserv.issue.stance", "internal.efficacy", 
                   "media.use.freq", "talk.freq")

test.cor.summary <- matrix(NA, ncol = 3, nrow = 8)
rownames(test.cor.summary) <- vars.to.check
colnames(test.cor.summary) <- c("mean", "min", "max")

vars.to.check <- paste(rep(vars.to.check, each = 3), 1:3, sep = ".")

for (i in 1:9) {
  k <- 3 * i
  index <- (k-2):k
  test.cor <- cor(wide.dat[, vars.to.check[index], with = F], use = "complete.obs")
  test.cor <- test.cor[lower.tri(test.cor)]
  test.cor.summary[i, 1] <- mean(test.cor)
  test.cor.summary[i, 2] <- min(test.cor)
  test.cor.summary[i, 3] <- max(test.cor)
}
print(test.cor.summary)
apply(test.cor.summary[1:5,], 2, range)

## assign imputed values to a origianl dataset
g_imp <- vector("list", 5)

for (i in 1:5) {
  g_imp[[i]] <- g
  g_imp[[i]][[1]] %v% "candidate.preference" <-  dat.imputed$imputations[[i]]$candidate.preference[1:341]
}

##---------------------------------##
## Estimate Btergm with imputation ##
##---------------------------------##

## prepare estimation
load("R_results/btergm.results.Aug 2nd.Rdata")
formula.btergm <- final.model@formula

estimate_btergm_with_imputed_data <- function(g, formula.btergm, R = 200) {
  
  model <- btergm::btergm(formula.btergm, R = R, parallel = "snow", ncpus = 4)
  coef <- model@coef
  boot <- model@boot
  
  return(list(model = model, coef = coef, boot = boot))
}

## estimate btergm model with imputated data
imputation.btergm.model <- lapply(1:5, function(i) estimate_btergm_with_imputed_data(g_imp[[i]], formula.btergm, R = 200))

pooled.btergm.model <- imputation.btergm.model[[1]]$model
pooled.btergm.model@coef <- apply(sapply(1:5, function(i) coef(imputation.btergm.model[[i]]$model)), 1, mean)
pooled.btergm.model@boot$t0 <- pooled.btergm.model@coef 
pooled.btergm.model@boot$t <- Reduce("rbind", lapply(1:5, function(i) imputation.btergm.model[[i]]$model@boot$t))
pooled.btergm.model@boot$R <- pooled.btergm.model@R <- 1000

## comparison of the estimated models (for robustness check) with the main model reported in the ms:
require(texreg)
screenreg(list(final.model, final.model.nothreshold, pooled.btergm.model), 
          leading.zero = F, single.row = T, digits = 3)
