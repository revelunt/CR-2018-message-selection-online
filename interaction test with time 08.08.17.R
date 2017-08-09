

## interaction effect testing with time, interactin with same_candiate_preference, evaluative criteria, and policy.pref.sim.

same_candidate_preference  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

same_candidate_preference_Park  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference", diff = TRUE, keep = c(1)), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

same_candidate_preference_Moon  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference", diff = TRUE, keep = c(2)), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

time_trends <- lapply(1:3, function(i) {
  dim <- dim(as.matrix(g[[i]]))
  outmat <- matrix(i - 1, nrow = dim[1], ncol = dim[2])
  diag(outmat) <- 0
  outmat
})

time.X.same.candidate.preference_Park <- lapply(1:3, function(i) {
  outmat <- time_trends[[i]] * same_candidate_preference_Park[[i]]
  diag(outmat) <- 0
  outmat
})

time.X.same.candidate.preference_Moon <- lapply(1:3, function(i) {
  outmat <- time_trends[[i]] * same_candidate_preference_Moon[[i]]
  diag(outmat) <- 0
  outmat
})

time.X.same.candidate.preference <- lapply(1:3, function(i) {
  outmat <- time_trends[[i]] * same_candidate_preference[[i]]
  diag(outmat) <- 0
  outmat
})

ocov.consistency  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodeocov("consistency.motivation"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

ocov.consistency.X.time.X.preference <- lapply(1:3, function(i) {
  temp1.t <- ocov.consistency[[i]] * time_trends[[i]] * same_candidate_preference[[i]]
  diag(temp1.t) <- 0
  temp1.t
})


## same_candidate_preference interaction model

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
                         edgecov(time.X.same.candidate.preference_Moon)
                       ,
                       R = 1000, parallel = "multicore", ncpus = 5); 

 ## summary.methods does not work in some cases, therefore bypass the summary methods and get the same info
cbind(coef(final.model4), t(apply(final.model4@boot$t, 2, quantile, c(0.025, 0.975), na.rm = T)))

# three-way interaction (not significant)
final.model5.r <- btergm(g ~ edges + ## intercept
                         
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
                         edgecov(time_trends) +
                         edgecov(time.X.same.candidate.preference) + 
                         edgecov(ocov.consistency.X.time.X.preference)
                       ,
                         
                       R = 1000, parallel = "multicore", ncpus = parallel::detectCores())

cbind(coef(final.model5), t(apply(final.model5@boot$t, 2, quantile, c(0.025, 0.975), na.rm = T)))



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
