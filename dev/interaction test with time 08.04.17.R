

## interaction effect testing with time, interactin with same_candiate_preference, evaluative criteria, and policy.pref.sim.

same_candidate_preference  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

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
                         timecov(transform = function(t) t) +
                         timecov(same_candidate_preference, transform = function(t) t),
                       
                       R = 1000, parallel = "multicore", ncpus = parallel::detectCores()); 

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
