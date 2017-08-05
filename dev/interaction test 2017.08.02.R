
## interaction effect testing regarding structural effects
asymmetrical.expertise <- lapply(g, make_asymmetrical_adj, "expertise")
alter_more_expertise <- lapply(asymmetrical.expertise, function(x) {
  mat <- 1 * (x > 0)
  diag(mat) <- 0
  mat
})

transitivity.X.expertise_hierarchy_in_triads <- list()
cylic.closure.X.expertise_hierarchy_in_triads <- list()

## matrix multiplication of alter_more_expertise counts the number of k intermediate nodes
## who have more expertise level than ego i but less than alter j (i -> "k" -> j)
## (similar to calcuating two-paths between dyads, but based on asymmetrical expertise level)
expertise_hierarchy_in_triads <- lapply(1:3, function(i) {
  mat <- alter_more_expertise[[i]] * as.matrix(g[[i]]) ## ensure i -> k -> j relations actually exists
  outmat <- (mat %*% mat) * alter_more_expertise[[i]] ## this does not depends on whether i -> j exists
  diag(outmat) <- 0
  outmat })

for (i in 1:3) {
  temp1.t <- ergmMPLE(g[[i]] ~ dgwesp(3, fixed = T, type = "OTP"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  
  temp1.c <- ergmMPLE(g[[i]] ~ dgwesp(1.5, fixed = T, type = "ITP"), output = "array")$predictor[,,1]
  dimnames(temp1.c) <- NULL
  
  outmat1 <- temp1.t * expertise_hierarchy_in_triads[[i]]
  outmat2 <- temp1.c * expertise_hierarchy_in_triads[[i]]
  
  diag(outmat2) <- diag(outmat1) <- 0
  rownames(outmat1) <- colnames(outmat1) <- g[[i]] %v% "vertex.names"
  rownames(outmat2) <- colnames(outmat2) <- g[[i]] %v% "vertex.names"
  
  # extent of change in degree of transitivity when expertise hierarchy triad increases
  transitivity.X.expertise_hierarchy_in_triads[[i]] <- outmat1
  # extent of change in cyclic closure when expertise hierarchy triad increases
  cylic.closure.X.expertise_hierarchy_in_triads[[i]] <- outmat2
  
}

## what is the nature of hierarcical orgnaization of network? 
## one possibility is that such patterns are driven by asymmetrical political expertise levels.
## we model this possibility by interacting OTP/ITP gwesp with expertise hierarchy in triads..

final.model4 <- btergm(g ~ edges + 
                         
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
                         dgwesp(decay = 3, fixed = T, type = "OTP") + 
                         dgwesp(decay = 3, fixed = T, type = "ITP") + 
                         dgwesp(decay = 3, fixed = T, type = "OSP") + ## 3 consistency
                         dgwesp(decay = 2, fixed = T, type = "ISP") + ## 3 consistency
                         
                         gwodegree(decay = 2, fixed = T) + ## hedonic
                         gwidegree(decay = 3, fixed = T) +
                         
                         ## interaction terms
                         edgecov(expertise_hierarchy_in_triads) + 
                         edgecov(transitivity.X.expertise_hierarchy_in_triads) + 
                         edgecov(shared.activity.X.same.candidate),
                       
                       R = 1000, parallel = "multicore", ncpus = parallel::detectCores()); summary(final.model4)


# Bootstrapping sample size: 1000 
# 
# Estimates and 90% confidence intervals:
#                                                             Estimate      5%     95%
# edges                                                     -1.9296758 -2.8943 -0.8338
# nodeicov.age                                               0.0032321 -0.0174  0.0232
# nodeocov.age                                               0.0489161 -0.1053  0.0935
# nodeifactor.gender.1                                      -0.0037032 -0.0517  0.0392
# nodeofactor.gender.1                                       0.0145164 -0.1788  0.2598
# nodematch.gender                                           0.0459422  0.0248  0.0705
# nodeicov.edu                                              -0.0071705 -0.0302  0.0258
# nodeocov.edu                                               0.0107264 -0.0191  0.0768
# nodeifactor.region_origin2.1                              -0.0867426 -0.1335 -0.0345
# nodeofactor.region_origin2.1                              -0.1098649 -0.4391  0.1308
# nodematch.region_origin2                                   0.0186761 -0.0145  0.0511
# nodeicov.talk.freq                                         0.0480624  0.0229  0.0490
# nodeocov.talk.freq                                         0.0129890 -0.1019  0.1283
# nodeicov.media.use.freq                                   -0.0105189 -0.0192 -0.0046
# nodeocov.media.use.freq                                    0.0327285 -0.0041  0.0708
# nodeicov.internal.efficacy                                -0.0073271 -0.0304  0.0175
# nodeocov.internal.efficacy                                 0.0217031 -0.0715  0.0801
# nodeifactor.candidate.preference.1                         0.0139604  0.0125  0.0455
# nodeofactor.candidate.preference.1                         0.0144591 -0.0636  0.0680
# nodeicov.consistency.motivation                            0.0316161 -0.0147  0.0601
# nodeocov.consistency.motivation                            0.0315857 -0.0380  0.0801
# nodeicov.understanding.motivation                         -0.0458102 -0.0746 -0.0111
# nodeocov.understanding.motivation                          0.0136200 -0.0028  0.0549
# nodeicov.hedomic.motivation                               -0.0111853 -0.0303  0.0050
# nodeocov.hedomic.motivation                                0.1019743  0.0886  0.1335
# nodematch.candidate.preference                            -0.0390480 -0.1126  0.0236
# edgecov.policy.pref.sim[[i]]                              -0.0920991 -0.1381  0.0393
# edgecov.evaludative.criteria.sim[[i]]                      0.4067267  0.2513  0.4126
# isolates                                                   1.0283827  0.9219  1.1570
# mutual                                                     0.7663840  0.5612  0.9463
# edgecov.g_autoregression[[i]]                              0.2224025  0.1910  0.2516
# gwdsp.fixed.1                                              0.0030751 -0.0049  0.0075
# edgecov.g_delrecip[[i]]                                    0.0743835 -0.0087  0.2475
# edgecov.g_lagtransitivity[[i]]                             0.0328907  0.0187  0.0540
# edgecov.g_lagcyclic[[i]]                                   0.0332267  0.0183  0.0474
# edgecov.g_lag_shared_activity[[i]]                        -0.0546633 -0.0618 -0.0442
# edgecov.g_lag_shared_popularity[[i]]                      -0.0599435 -0.0878 -0.0432
# nodeocov.lagged.sender.effect                              0.0193125  0.0163  0.0242
# nodeicov.lagged.receiver.effect                            0.0236839  0.0186  0.0306
# gwesp.OTP.fixed.3                                          0.0556248  0.0202  0.0916
# gwesp.ITP.fixed.3                                         -0.0657904 -0.0762 -0.0587
# gwesp.OSP.fixed.3                                          0.0350694  0.0288  0.0476
# gwesp.ISP.fixed.2                                          0.1127112  0.0862  0.1446
# gwodegree                                                 -4.3317476 -4.5004 -4.1144
# gwidegree                                                 -3.9981278 -4.7552 -3.4595
# edgecov.expertise_hierarchy_in_triads[[i]]                 0.0213548 -0.0440  0.0759
# edgecov.transitivity.X.expertise_hierarchy_in_triads[[i]]  0.0015890 -0.0005  0.0063
# edgecov.shared.activity.X.same.candidate[[i]]              0.0012962 -0.0056  0.0082
