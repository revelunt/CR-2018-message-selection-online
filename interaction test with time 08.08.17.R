

## interaction effect testing with time, interactin with same_candiate_preference, evaluative criteria, and policy.pref.sim.
source("dev/make_interaction_vars.R")

## same_candidate_preference interaction model
## same_candidate_preference interaction model

final.model4 <- btergm(g ~ edges + ## intercept
                         
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
                         nodeicov("internal.efficacy") + nodeocov("internal.efficacy") +
                         nodeifactor("candidate.preference") + nodeofactor("candidate.preference") + 
                         
                         ## individual, motivation factor
                         nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
                         nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
                         nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
                         
                         ## dyadic, consistency
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
                         gwidegree(decay = 3, fixed = T) + ## hedonic 
                         
                         ## interaction terms
                         edgecov(time_trends) + #timecov(transform = function(t) t - 1) +
                         edgecov(time.X.same.candidate.preference), # timecov(same_candidate_preference, transform = function(t) t - 1),
                       
                       R = 1000, parallel = "multicore", ncpus = 10); 


## does the candidate preferece homophily change differently over time depending on candidate?
final.model4.r <- btergm(g ~ edges + ## intercept
                         
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
                         nodeicov("internal.efficacy") + nodeocov("internal.efficacy") +
                         # nodeifactor("candidate.preference") + nodeofactor("candidate.preference") + 
                         
                         ## individual, motivation factor
                         nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
                         nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
                         nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
                         
                         ## dyadic, consistency
                         edgecov(same_candidate_preference_Park) + 
                         edgecov(same_candidate_preference_Moon) + 
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
                         gwidegree(decay = 3, fixed = T) + ## hedonic 
                         
                         ## interaction terms
                         edgecov(time_trends) +
                         edgecov(time.X.same.candidate.preference_Park) + 
                         edgecov(time.X.same.candidate.preference_Moon) ## significant! 
                       ,
                       R = 1000, parallel = "multicore", ncpus = 5); 

 ## summary.methods does not work in some cases, therefore bypass the summary methods and get the same info
cbind(coef(final.model4), t(apply(final.model4@boot$t, 2, quantile, c(0.025, 0.975), na.rm = T)))

# Estimates and 95% confidence intervals:
#                                                     Estimate    2.5%   97.5%
# edges                                              -1.7475029 -2.7466 -0.3872
# nodeicov.age                                       -0.0024648 -0.0255  0.0391
# nodeocov.age                                        0.0388303 -0.1873  0.0860
# nodeifactor.gender.1                                0.0088896 -0.0387  0.0710
# nodeofactor.gender.1                                0.0306040 -0.3380  0.3291
# nodematch.gender                                    0.0446009  0.0148  0.0873
# nodeicov.edu                                       -0.0089141 -0.0385  0.0173
# nodeocov.edu                                        0.0156914 -0.0151  0.0935
# nodeifactor.region_origin2.1                       -0.0823721 -0.1613  0.0420
# nodeofactor.region_origin2.1                       -0.1419545 -0.5949  0.3599
# nodematch.region_origin2                            0.0147253 -0.0143  0.0806
# nodeicov.talk.freq                                  0.0308668  0.0027  0.0379
# nodeocov.talk.freq                                 -0.0030359 -0.1318  0.1602
# nodeicov.media.use.freq                            -0.0174555 -0.0239  0.0213
# nodeocov.media.use.freq                             0.0244576 -0.0157  0.2892
# nodeicov.internal.efficacy                         -0.0125512 -0.0580  0.0546
# nodeocov.internal.efficacy                          0.0280216 -0.1035  0.1228
# nodeicov.consistency.motivation                     0.0370826 -0.0220  0.1173
# nodeocov.consistency.motivation                     0.0194295 -0.1188  0.0736
# nodeicov.understanding.motivation                  -0.0492897 -0.1102  0.0243
# nodeocov.understanding.motivation                   0.0384213  0.0162  0.0947
# nodeicov.hedomic.motivation                        -0.0119503 -0.0397  0.0002
# nodeocov.hedomic.motivation                         0.1033045  0.0944  0.1333
# edgecov.same_candidate_preference_Park[[i]]         0.0257782 -0.1505  0.6809
# edgecov.same_candidate_preference_Moon[[i]]        -0.1060835 -0.3283 -0.0080
# edgecov.policy.pref.sim[[i]]                       -0.0852138 -0.2172  0.0318
# edgecov.evaludative.criteria.sim[[i]]               0.3826920  0.1949  0.4053
# isolates                                            0.9956819  0.7979  1.2623
# mutual                                              0.7690782  0.5083  1.0703
# edgecov.g_autoregression[[i]]                       0.2203686  0.1848  0.2505
# gwdsp.fixed.1                                       0.0027172 -0.0072  0.0094
# edgecov.g_delrecip[[i]]                             0.0748650 -0.0699  0.3445
# edgecov.g_lagtransitivity[[i]]                      0.0325575  0.0187  0.0507
# edgecov.g_lagcyclic[[i]]                            0.0318598  0.0081  0.0564
# edgecov.g_lag_shared_activity[[i]]                 -0.0548970 -0.0672 -0.0352
# edgecov.g_lag_shared_popularity[[i]]               -0.0582168 -0.1091 -0.0331
# nodeocov.lagged.sender.effect                       0.0192357  0.0097  0.0286
# nodeicov.lagged.receiver.effect                     0.0233474  0.0185  0.0378
# gwesp.OTP.fixed.3                                   0.0571584 -0.0526  0.1246
# gwesp.ITP.fixed.3                                  -0.0653805 -0.0802 -0.0603
# gwesp.OSP.fixed.3                                   0.0350753  0.0325  0.0528
# gwesp.ISP.fixed.2                                   0.1135429  0.0821  0.2317
# gwodegree                                          -4.4053738 -4.5610 -4.0055
# gwidegree                                          -4.1301383 -5.3218 -3.2438
# edgecov.time_trends[[i]]                            0.0774395 -0.0604  0.2598
# edgecov.time.X.same.candidate.preference_Park[[i]] -0.0680975 -0.4268  0.2169
# edgecov.time.X.same.candidate.preference_Moon[[i]]  0.0848770  0.0117  0.1910

# three-way interaction (whether the pattern is more pronounced among those with higher consistency motivation?)
# if positive, then it suggests people may more rely on confirmative evidence to reduce uncertainty 
final.model4.r2 <- btergm(g ~ edges + ## intercept
                         
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
                            nodeicov("internal.efficacy") + nodeocov("internal.efficacy") +
                            #nodeifactor("candidate.preference") + nodeofactor("candidate.preference") + 
                            
                            ## individual, motivation factor
                            nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
                            nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
                            nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
                            
                            ## dyadic, consistency
                            edgecov(same_candidate_preference_Park) + 
                            edgecov(same_candidate_preference_Moon) + 
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
                            gwidegree(decay = 3, fixed = T) + ## hedonic 
                            
                            ## interaction terms
                            edgecov(time_trends) + edgecov(ocov.consistency.X.time) + 
                            #edgecov(time.X.same.candidate.preference_Park) + 
                            edgecov(time.X.same.candidate.preference_Moon) + 
                            #edgecov(ocov.consistency.X.preference.Park) + 
                            edgecov(ocov.consistency.X.preference.Moon) + 
                            #edgecov(ocov.consistency.X.time.X.preference.Park) + 
                            edgecov(ocov.consistency.X.time.X.preference.Moon)
                          ,
                         
                       R = 1000, parallel = "snow", ncpus = 6)

cbind(coef(final.model5), t(apply(final.model5@boot$t, 2, quantile, c(0.025, 0.975), na.rm = T)))

## interaction with understanding motivation? (THERE's NO interaction)
final.model4.r3 <- btergm(g ~ edges + ## intercept
                            
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
                            nodeicov("internal.efficacy") + nodeocov("internal.efficacy") +
                            #nodeifactor("candidate.preference") + nodeofactor("candidate.preference") + 
                            
                            ## individual, motivation factor
                            nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
                            nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
                            nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
                            
                            ## dyadic, consistency
                            edgecov(same_candidate_preference_Park) + 
                            edgecov(same_candidate_preference_Moon) + 
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
                            gwidegree(decay = 3, fixed = T) + ## hedonic 
                            
                            ## interaction terms
                            edgecov(time_trends) + edgecov(ocov.understanding.X.time) + 
                            #edgecov(time.X.same.candidate.preference_Park) + 
                            edgecov(time.X.same.candidate.preference_Moon) + 
                            #edgecov(ocov.consistency.X.preference.Park) + 
                            edgecov(ocov.understanding.X.preference.Moon) + 
                            #edgecov(ocov.consistency.X.time.X.preference.Park) + 
                            edgecov(ocov.understanding.X.time.X.preference.Moon)
                          ,
                          
                          R = 1000, parallel = "snow", ncpus = 6)

summary(final.model4.r3)
cbind(coef(final.model4.r3), t(apply(final.model4.r3@boot$t, 2, quantile, c(0.025, 0.975), na.rm = T)))



## all models at once
## screenreg(list(final.model4, final.model4r, final.model5), single.row = T, digits = 3)

# 
# n_time <- length(g)
# n_samps <- 1000
# model <- final.model4
# 
# ep <- edgeprob(final.model4)
# 
# est <- data.frame(t = rep(1:3, each = 2),
#                   match = rep(0:1, 3),
#                   est = rep(NA, 6),
#                   lower = rep(NA, 6),
#                   upper = rep(NA, 6)
#                   )
# 
# setDT(ep)
# 
# for (time in 1:n_time) {
# 
#   pn <- g[[time]]
#   pf <- same_candidate_preference[[time]]
#   d <- dim(as.matrix(pn))
#   size <- d[1] * d[2]
#   nw <- matrix(1:size, nrow = d[1], ncol = d[2])
# 
#   ## dyads with nodematch == 1
#   nw.1 <- nw[pf == 1]
#   #nw.1 <- nw.1[lower.tri(nw.1)]
#   nw.1 <- sample(nw.1, n_samps, replace = TRUE)
# 
#   ## dyads with nodematch == 0
#   nw.0 <- nw[pf == 0]
#   #nw.0 <- nw.0[lower.tri(nw.0)]
#   nw.0 <- sample(nw.0, n_samps, replace = TRUE)
# 
#   ## dyadic probability given the network and model, for dyads with nodematch == 1
#     est.match.1 <- mclapply(1:n_samps, function(n) {
#       dyad <- arrayInd(nw.1[n], d)
#       sender <- dyad[1, 1]
#       receiver <- dyad[1, 2]
#       int.est <- ep[i == sender & j == receiver & t == time, probability]
#       int.est
#     }, mc.cores = 4)
# 
#   ## dyadic probability given the network and model, for dyads with nodematch == 0
#     est.match.0 <- mclapply(1:n_samps, function(n) {
#       dyad <- arrayInd(nw.0[n], d)
#       sender <- dyad[1, 1]
#       receiver <- dyad[1, 2]
#       int.est <- ep[i == sender & j == receiver & t == time, probability]
#       int.est
#     }, mc.cores = 4)
# 
#     est[(time*2 - 1), 3] <- mean(unlist(est.match.0)) ## mean probability for match == 0
#     est[(time*2 - 1), 4:5] <- quantile(unlist(est.match.0), c(0.025, 0.975)) ## 95% CIs
# 
#     est[(time*2), 3] <- mean(unlist(est.match.1)) ## mean probability for match == 1
#     est[(time*2), 4:5] <- quantile(unlist(est.match.1), c(0.025, 0.975)) ## 95% CIs
# 
#   }

# 
# ggplot(data = est, aes(x = t, y = est, fill = factor(match))) + 
#   geom_bar(stat = "identity", position = position_dodge()) + 
#   xlab("Time") + ylab("Predicted edge probability") + 
#   ggtitle("Panel A: Main effect of candidate preference homophily")

## alternatively,
## get coef from the fitted model, clear up '[[i]]' in the name
co <- coef(final.model4)
for (i in 1:length(co)) {
  if (grepl("((edge)|(dyad))cov", names(co)[i])) {
    names(co)[i] <- substr(names(co)[i], 1, nchar(names(co))[i] - 5)}
}

## perform edge prediction using edgeprob function
ep <- edgeprob(final.model4)

## clear up '[[i]]' in the data frame
for (i in 1:ncol(ep)) {
  if (grepl("((edge)|(dyad))cov", colnames(ep)[i])) {
    colnames(ep)[i] <- substr(colnames(ep)[i], 1, nchar(colnames(ep)[i]) - 5)
  }
}

## identify change statistics for main vars and interaction var
var1 <- "nodematch.candidate.preference"
var2 <- "edgecov.timecov1"
inter <- "edgecov.timecov2.same_candidate_preference"

## get coefficients
beta1 <- co[match(var1, names(co))]
beta2 <- co[match(var2, names(co))]
beta3 <- co[match(inter, names(co))]

ep$beta1 <- beta1
ep$X1 <- ep$nodematch.candidate.preference

ep$beta2 <- beta2
ep$X2 <- ep$edgecov.timecov1

ep$inter <- beta3
ep$X1X2 <- ep$edgecov.timecov2.same_candidate_preference

ep$logodds <- with(ep, beta1 * X1 + beta2 * X2 + beta3 * X1X2) 
#intercept <- apply(ep[,2:46], 1, '%*%', coef(final.model4)[1:45])
intercept <- ep[,2] * coef(final.model4)[1]
ep$logodds <- intercept + ep$logodds ## add intercept
ep$predprob <- with(ep, c(1 / (1 + exp(-logodds))))

ep$X1 <- car::recode(ep$X1, "0 = 'Different'; 1 = 'Same'")

## model-implied predicted probability conditional on time trends
p.inter.mn <- ggplot(data = ep, aes(x = X2, y = predprob, colour = factor(X1))) + theme_bw() + 
  scale_colour_grey(start = 0.8, end = 0.2, guide = guide_legend(reverse=TRUE)) + 
  theme(legend.justification=c(1,0), legend.position=c(0.9,0.1)) +
  geom_line(stat = "summary", fun.y = "median", size = 1.5) + 
  #geom_bar(stat = "summary", fun.y = "median", position = "dodge") +
  labs(colour = "Candidate preference") + 
  xlab("Time trends") + ylab("Predicted edge probability") + 
  ggtitle("Panel A: Main effect of candidate preference homophily")

## how many unique change statistics in modertor?
v2 <- sort(unlist(unique(ep['edgecov.timecov1'])))
names(v2) <- NULL

## calculate theta, the logg odds of nodematch term on probability
delta1 = beta1 + beta3 * v2

## recover bootstrapped coefficients from fitted model
tempdta <- final.model4@boot$t
colnames(tempdta) <- names(co)

tempdta <- as.data.table(tempdta[, c(var1, var2, inter)])
tempdta[, t1 := nodematch.candidate.preference + (edgecov.timecov2.same_candidate_preference * 1)] # v2[1]
tempdta[, t2 := nodematch.candidate.preference + (edgecov.timecov2.same_candidate_preference * 2)] # v2[2]
tempdta[, t3 := nodematch.candidate.preference + (edgecov.timecov2.same_candidate_preference * 3)] # v2[3]

lower <- tempdta[complete.cases(tempdta), apply(.SD, 2, perc.ci, conf = 0.95), .SDcol = c("t1", "t2", "t3")][1,]
upper <- tempdta[complete.cases(tempdta), apply(.SD, 2, perc.ci, conf = 0.95), .SDcol = c("t1", "t2", "t3")][2,]

dta <- data.frame(time = v2, theta = delta1, lower = lower, upper = upper)

## interaction plot
p.inter.jn <- ggplot(data = dta, aes(x = time, y = theta)) + geom_line(color = "black") + theme_bw() + 
  geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), 
              alpha = 0.15, fill = "black") + geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Time trends") + ylab("Main effect of \nCandidate preference homophily") +
  ggtitle("Panel B: JN plot for interaction effect of preference homophily and time trends")

require(gridExtra)
grid.arrange(p.inter.mn, p.inter.jn, nrow = 1, ncol = 2)


## 
