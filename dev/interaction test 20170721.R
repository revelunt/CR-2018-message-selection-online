
## more interaction test, combining two significant interactions
transitivity.X.alter.more.interested <- list()
shared.activity.X.same.candidate <- list()

for (i in 1:3) {
  temp1 <- ergmMPLE(g[[i]] ~ dgwesp(3, fixed = T, type = "OTP"), output = "array")$predictor[,,1]
  dimnames(temp1) <- NULL
  
  temp1.r <- ergmMPLE(g[[i]] ~ dgwesp(1.5, fixed = T, type = "OSP"), output = "array")$predictor[,,1]
  dimnames(temp1.r) <- NULL
  
  temp2 <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor[,,1]
  dimnames(temp2) <- NULL
  
  outmat1 <- temp1 * alter_more_interested[[i]]
  outmat2 <- temp1.r * temp2

  diag(outmat2) <- diag(outmat1) <- 0
  rownames(outmat1) <- colnames(outmat1) <- g[[i]] %v% "vertex.names"
  rownames(outmat2) <- colnames(outmat2) <- g[[i]] %v% "vertex.names"
  
  transitivity.X.alter.more.interested[[i]] <- outmat1
  shared.activity.X.same.candidate[[i]] <- outmat2

}

final.model4 <- btergm(
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
    dgwesp(decay = 3, fixed = T, type = "OTP") + ## understanding
    dgwesp(decay = 3, fixed = T, type = "ITP") + ## understanding
    dgwesp(decay = 3, fixed = T, type = "OSP") + ## consistency
    dgwesp(decay = 2, fixed = T, type = "ISP") + ## consistency
    
    gwodegree(decay = 2, fixed = T) + ## hedonic
    gwidegree(decay = 3, fixed = T) + 
    
    edgecov(transitivity.X.alter.more.interested) + 
    edgecov(shared.activity.X.same.candidate),
  
  R = 1000, parallel = "multicore", ncpus = parallel::detectCores())


# Estimates and 95% confidence intervals:
#                                                     Estimate    2.5%   97.5%
# edges                                             -1.8001969 -2.8524 -0.3670
# nodeicov.age                                       0.0035158 -0.0146  0.0270
# nodeocov.age                                       0.0474023 -0.2012  0.0891
# nodeifactor.gender.1                              -0.0048803 -0.0485  0.0598
# nodeofactor.gender.1                               0.0149284 -0.3330  0.3387
# nodematch.gender                                   0.0450340  0.0139  0.0865
# nodeicov.edu                                      -0.0102746 -0.0386  0.0173
# nodeocov.edu                                       0.0105264 -0.0225  0.0769
# nodeifactor.region_origin2.1                      -0.0936038 -0.1762  0.0418
# nodeofactor.region_origin2.1                      -0.1059201 -0.5942  0.3873
# nodematch.region_origin2                           0.0181620 -0.0152  0.0892
# nodeicov.talk.freq                                 0.0462208  0.0102  0.0515
# nodeocov.talk.freq                                 0.0145863 -0.1105  0.1494
# nodeicov.media.use.freq                           -0.0106892 -0.0185  0.0217
# nodeocov.media.use.freq                            0.0311060 -0.0150  0.2903
# nodecov.internal.efficacy                          0.0031566 -0.0466  0.0346
# nodeicov.consistency.motivation                    0.0304946 -0.0195  0.0896
# nodeocov.consistency.motivation                    0.0315183 -0.0959  0.0756
# nodeicov.understanding.motivation                 -0.0427686 -0.0909  0.0247
# nodeocov.understanding.motivation                  0.0145340 -0.0027  0.0689
# nodeicov.hedomic.motivation                       -0.0100953 -0.0292  0.0022
# nodeocov.hedomic.motivation                        0.0941668  0.0747  0.1169
# nodeicov.candidate.preference                      0.0132188  0.0092  0.0907
# nodeocov.candidate.preference                      0.0084069 -0.1316  0.0832
# nodematch.candidate.preference                    -0.1497325 -0.3420  0.0414
# edgecov.policy.pref.sim[[i]]                      -0.0942020 -0.2124  0.0563
# edgecov.evaludative.criteria.sim[[i]]              0.3928843  0.2055  0.3968
# isolates                                           0.9988884  0.7837  1.2141
# mutual                                             0.7623902  0.5002  1.0636
# edgecov.g_autoregression[[i]]                      0.2208193  0.1932  0.2507
# gwdsp.fixed.1                                      0.0029845 -0.0068  0.0088
# edgecov.g_delrecip[[i]]                            0.0697527 -0.0639  0.3454
# edgecov.g_lagtransitivity[[i]]                     0.0327856  0.0160  0.0544
# edgecov.g_lagcyclic[[i]]                           0.0346302  0.0074  0.0581
# edgecov.g_lag_shared_activity[[i]]                -0.0553211 -0.0676 -0.0340
# edgecov.g_lag_shared_popularity[[i]]              -0.0588479 -0.1101 -0.0304
# nodeocov.lagged.sender.effect                      0.0194528  0.0102  0.0285
# nodeicov.lagged.receiver.effect                    0.0232037  0.0181  0.0384
# gwesp.OTP.fixed.3                                  0.0543312 -0.0554  0.1213
# gwesp.ITP.fixed.3                                 -0.0687426 -0.0840 -0.0611
# gwesp.OSP.fixed.3                                  0.0285094  0.0176  0.0489
# gwesp.ISP.fixed.2                                  0.1136657  0.0807  0.2334
# gwodegree                                         -4.3684485 -4.5590 -4.0724
# gwidegree                                         -3.9858158 -5.3158 -3.1282
# edgecov.transitivity.X.alter.more.interested[[i]]  0.0157553  0.0056  0.0213
# edgecov.shared.activity.X.same.candidate[[i]]      0.0494191  0.0031  0.1014
