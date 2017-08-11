

## interaction effect testing with time, interactin with same_candiate_preference, evaluative criteria, and policy.pref.sim.

same_candidate_preference  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

# time_trends <- lapply(1:3, function(i) {
#   dim <- dim(as.matrix(g[[i]]))
#   outmat <- matrix(i - 1, nrow = dim[1], ncol = dim[2])
#   diag(outmat) <- 0
#   outmat
# })
# 
# time.X.same.candidate.preference <- lapply(1:3, function(i) {
#   outmat <- time_trends[[i]] * same_candidate_preference[[i]]
#   diag(outmat) <- 0
#   outmat
# })


evaludative_criteria_sim <- evaludative.criteria.sim
policy_pref_sim <- policy.pref.sim 

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

 ## summary.methods does not work in some cases, therefore bypass the summary methods and get the same info
cbind(coef(final.model4), t(apply(final.model4@boot$t, 2, quantile, c(0.025, 0.975), na.rm = T)))

# evaludative_criteria_sim model
final.model5 <- btergm(g ~ edges + ## intercept
                         
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
                         gwidegree(decay = 3, fixed = T) +
                         
                         ## interaction terms
                         timecov(transform = function(t) t) +
                         timecov(evaludative_criteria_sim, transform = function(t) t),
                       
                       R = 1000, parallel = "multicore", ncpus = parallel::detectCores());

cbind(coef(final.model5), t(apply(final.model5@boot$t, 2, quantile, c(0.025, 0.975), na.rm = T)))


# policy.pref.sim model
final.model6 <- btergm(g ~ edges + ## intercept
                         
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
                         gwidegree(decay = 3, fixed = T) +
                         
                         ## interaction terms
                         timecov(transform = function(t) t) +
                         timecov(policy_pref_sim, transform = function(t) t),
                       
                       R = 1000, parallel = "multicore", ncpus = parallel::detectCores());

cbind(coef(final.model6), t(apply(final.model6@boot$t, 2, quantile, c(0.025, 0.975), na.rm = T)))

## all models at once
## screenreg(list(final.model4, final.model5, final.model6), single.row = T, digits = 3)
save(final.model4, final.model5, final.model6, file = "R_results/interaction.time.Aug.04.Rdata")

# ============================================================================================================================================
#                                                               Model 1                        Model 2                        Model 3                      
# --------------------------------------------------------------------------------------------------------------------------------------------
#   edges                                                -1.819 [-2.815; -0.304] *      -1.823 [-2.807; -0.304] *      -1.936 [-2.937; -0.304] *
#   nodeicov.age                                         -0.003 [-0.023;  0.035]        -0.003 [-0.022;  0.035]        -0.003 [-0.022;  0.035]  
#   nodeocov.age                                          0.040 [-0.192;  0.091]         0.040 [-0.192;  0.090]         0.040 [-0.192;  0.090]  
#   nodeifactor.gender.1                                  0.009 [-0.037;  0.071]         0.009 [-0.036;  0.071]         0.009 [-0.036;  0.071]  
#   nodeofactor.gender.1                                  0.029 [-0.348;  0.335]         0.029 [-0.348;  0.335]         0.029 [-0.348;  0.335]  
#   nodematch.gender                                      0.044 [ 0.015;  0.086] *       0.044 [ 0.015;  0.086] *       0.044 [ 0.015;  0.086] *
#   nodeicov.edu                                         -0.010 [-0.039;  0.019]        -0.010 [-0.039;  0.019]        -0.010 [-0.039;  0.018]  
#   nodeocov.edu                                          0.015 [-0.016;  0.091]         0.015 [-0.016;  0.091]         0.015 [-0.016;  0.091]  
#   nodeifactor.region_origin2.1                         -0.083 [-0.157;  0.044]        -0.084 [-0.157;  0.044]        -0.084 [-0.157;  0.044]  
#   nodeofactor.region_origin2.1                         -0.143 [-0.598;  0.350]        -0.142 [-0.598;  0.350]        -0.143 [-0.598;  0.350]  
#   nodematch.region_origin2                              0.015 [-0.014;  0.080]         0.015 [-0.014;  0.080]         0.015 [-0.014;  0.080]  
#   nodeicov.talk.freq                                    0.030 [ 0.002;  0.037] *       0.030 [ 0.002;  0.036] *       0.030 [ 0.002;  0.037] *
#   nodeocov.talk.freq                                   -0.005 [-0.143;  0.161]        -0.006 [-0.143;  0.161]        -0.006 [-0.143;  0.161]  
#   nodeicov.media.use.freq                              -0.018 [-0.024;  0.024]        -0.018 [-0.024;  0.024]        -0.018 [-0.024;  0.024]  
#   nodeocov.media.use.freq                               0.024 [-0.017;  0.287]         0.024 [-0.017;  0.287]         0.024 [-0.017;  0.287]  
#   nodeicov.internal.efficacy                           -0.012 [-0.058;  0.055]        -0.012 [-0.058;  0.055]        -0.012 [-0.058;  0.055]  
#   nodeocov.internal.efficacy                            0.030 [-0.102;  0.128]         0.031 [-0.102;  0.128]         0.031 [-0.102;  0.128]  
#   nodeifactor.candidate.preference.1                    0.006 [-0.008;  0.092]         0.004 [-0.008;  0.092]         0.003 [-0.008;  0.092]  
#   nodeofactor.candidate.preference.1                    0.017 [-0.123;  0.131]         0.017 [-0.123;  0.131]         0.016 [-0.123;  0.131]  
#   nodeicov.consistency.motivation                       0.037 [-0.021;  0.113]         0.037 [-0.021;  0.113]         0.037 [-0.021;  0.113]  
#   nodeocov.consistency.motivation                       0.019 [-0.112;  0.071]         0.019 [-0.112;  0.071]         0.019 [-0.112;  0.071]  
#   nodeicov.understanding.motivation                    -0.049 [-0.103;  0.022]        -0.049 [-0.103;  0.022]        -0.049 [-0.103;  0.022]  
#   nodeocov.understanding.motivation                     0.036 [ 0.012;  0.087] *       0.035 [ 0.011;  0.087] *       0.035 [ 0.011;  0.087] *
#   nodeicov.hedomic.motivation                          -0.012 [-0.038;  0.001]        -0.013 [-0.038;  0.001]        -0.013 [-0.038;  0.001]  
#   nodeocov.hedomic.motivation                           0.102 [ 0.094;  0.130] *       0.102 [ 0.094;  0.130] *       0.102 [ 0.094;  0.130] *
#   nodematch.candidate.preference                       -0.135 [-0.211;  0.047]        -0.033 [-0.079;  0.047]        -0.032 [-0.079;  0.047]  
#   edgecov.policy.pref.sim[[i]]                         -0.091 [-0.225;  0.042]        -0.090 [-0.230;  0.042]         0.094 [-0.764;  0.324]  
#   edgecov.evaludative.criteria.sim[[i]]                 0.385 [ 0.207;  0.404] *       0.295 [-0.359;  0.639]         0.389 [ 0.207;  0.405] *
#   isolates                                              1.003 [ 0.793;  1.264] *       1.005 [ 0.793;  1.264] *       1.005 [ 0.793;  1.264] *
#   mutual                                                0.768 [ 0.507;  1.068] *       0.768 [ 0.507;  1.068] *       0.768 [ 0.507;  1.068] *
#   edgecov.g_autoregression[[i]]                         0.220 [ 0.184;  0.250] *       0.220 [ 0.184;  0.250] *       0.219 [ 0.185;  0.250] *
#   gwdsp.fixed.1                                         0.003 [-0.007;  0.009]         0.003 [-0.007;  0.009]         0.003 [-0.007;  0.009]  
#   edgecov.g_delrecip[[i]]                               0.076 [-0.073;  0.344]         0.075 [-0.073;  0.344]         0.076 [-0.073;  0.344]  
#   edgecov.g_lagtransitivity[[i]]                        0.033 [ 0.019;  0.051] *       0.033 [ 0.019;  0.051] *       0.033 [ 0.019;  0.051] *
#   edgecov.g_lagcyclic[[i]]                              0.032 [ 0.008;  0.057] *       0.032 [ 0.008;  0.057] *       0.032 [ 0.008;  0.057] *
#   edgecov.g_lag_shared_activity[[i]]                   -0.055 [-0.067; -0.035] *      -0.055 [-0.067; -0.035] *      -0.055 [-0.067; -0.035] *
#   edgecov.g_lag_shared_popularity[[i]]                 -0.058 [-0.110; -0.034] *      -0.058 [-0.110; -0.034] *      -0.058 [-0.110; -0.034] *
#   nodeocov.lagged.sender.effect                         0.019 [ 0.010;  0.029] *       0.019 [ 0.010;  0.029] *       0.019 [ 0.010;  0.029] *
#   nodeicov.lagged.receiver.effect                       0.023 [ 0.018;  0.038] *       0.023 [ 0.018;  0.038] *       0.023 [ 0.018;  0.038] *
#   gwesp.OTP.fixed.3                                     0.057 [-0.053;  0.125]         0.057 [-0.053;  0.125]         0.057 [-0.053;  0.125]  
#   gwesp.ITP.fixed.3                                    -0.066 [-0.080; -0.061] *      -0.066 [-0.080; -0.061] *      -0.066 [-0.080; -0.061] *
#   gwesp.OSP.fixed.3                                     0.035 [ 0.033;  0.053] *       0.035 [ 0.033;  0.053] *       0.035 [ 0.033;  0.053] *
#   gwesp.ISP.fixed.2                                     0.113 [ 0.082;  0.232] *       0.113 [ 0.082;  0.232] *       0.113 [ 0.082;  0.232] *
#   gwodegree                                            -4.395 [-4.557; -3.994] *      -4.392 [-4.557; -3.994] *      -4.392 [-4.557; -3.994] *
#   gwidegree                                            -4.123 [-5.342; -3.259] *      -4.120 [-5.342; -3.259] *      -4.121 [-5.342; -3.259] *
#   edgecov.timecov1[[i]]                                 0.079 [-0.059;  0.262]         0.083 [ 0.021;  0.171] *       0.144 [ 0.063;  0.235] *
#   edgecov.timecov2.same_candidate_preference[[i]]       0.051 [ 0.038;  0.071] *                                                              
#   edgecov.timecov2.evaludative_criteria_sim[[i]]                                       0.046 [-0.176;  0.242]                                 
#   edgecov.timecov2.policy_pref_sim[[i]]                                                                              -0.095 [-0.253;  0.214]  
# --------------------------------------------------------------------------------------------------------------------------------------------
#               Num. obs.                                        291096                         291096                         291096                       
# ============================================================================================================================================
#   * 0 outside the confidence interval

texreg::htmlreg(list(final.model4, final.model5, final.model6), digits = 3, leading.zero = F, single.row = T,
                custom.model.names = c("candidate.pref.interaction", "eval.criteria.interaction", "policy.pref.interaction"),
                custom.coef.names = c("Edges (Intercept)", "Age (in-ties)", "Age (out-ties)",
                                      "Female (in-ties)", "Female (out-ties)", "Gender homophily",
                                      "Education (in-ties)", "Education (out-ties)",
                                      "Regional origin = Seoul (in-ties)",
                                      "Regional origin = Seoul (out-ties)",
                                      "Regional homophily (Seoul)",
                                      "Talk freq (in-ties)", "Talk freq (out-ties)", 
                                      "Media use (in-ties)", "Media use (out-ties)",
                                      "Internal efficacy (in-ties)", "Internal efficacy (out-ties)",
                                      "Candidate pref = Moon (in-ties)", "Candidate pref = Moon (out-ties)",
                                      "Consistency motivation (in-ties)", "Consistency motivation (out-ties)",
                                      "Understanding motivation (in-ties)", "Understanding motivation (out-ties)", 
                                      "Hedonic motivation (in-ties)", "Hedonic motivation (out-ties)", 
                                      "Same candidate pref", "Similar policy pref", "Similar evaluative criteria",
                                      "Isolates", "Reciprocity", "Previous communication",
                                      "Multiple two-paths (GWDSP, 1)", 
                                      "Delayed reciprocity", 
                                      "Delayed transitivity closure", "Delayed cyclic closure", 
                                      "Delayed activity closure", "Delayed popularity closure",
                                      "Persistent sender (out-tie)", "Persistent receiver (in-ties)",
                                      "Multiple path closure (GWESP-OTP, 3)", "Multiple cyclic closure (GWESP-ITP, 3)", 
                                      "Multiple activity closure (GWESP-OSP, 3)", "Multiple popularity closure (GWESP-ISP, 2)",
                                      "Activity spread (GW-outdegree, 2)", "Popularity spread (GW-indegree, 3)",
                                      "time trends (linear)", "time X same.canddiate.pref", 
                                      "time X evaluative criteria similarity", "time X policy pref similarity"),
                custom.note = " * 0 outside the 95% confidence interval", 
                
                bold = 0.5, doctype = T, html.tag = T, body.tag = T, indentation = "  ",
                caption = "",
                file = "results.Table2.Aug04.doc")



# 
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

ep$logodds <- with(ep, edges * coef(final.model4)[1] + beta1 * X1 + beta2 * X2 + beta3 * X1X2) ## add intercept
ep$predprob <- with(ep, c(1 / (1 + exp(-logodds))))

ep$X1 <- car::recode(ep$X1, "0 = 'Different'; 1 = 'Same'")

## model-implied predicted probability conditional on time trends
p.inter.mn <- ggplot(data = ep, aes(x = X2, y = predprob, colour = factor(X1))) + theme_bw() + 
  scale_colour_grey(start = 0.8, end = 0.2, guide = guide_legend(reverse=TRUE)) + 
  theme(legend.justification=c(1,0), legend.position=c(0.9,0.1)) +
  geom_line(stat = "identity", size = 1.5) + labs(colour = "Candidate preference") + 
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

