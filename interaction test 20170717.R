
## interaction test on final model
transitivity.X.same.candidate <- list()
cyclic.closure.X.same.candidate <- list()
mutual.X.same.candidate <- list()

for (i in 1:3) {
 temp1 <- ergmMPLE(g[[i]] ~ dgwesp(1.5, fixed = T, type = "OTP"), output = "array")$predictor[,,1]
 dimnames(temp1) <- NULL
 
 temp1.r <- ergmMPLE(g[[i]] ~ dgwesp(1.5, fixed = T, type = "ITP"), output = "array")$predictor[,,1]
 dimnames(temp1.r) <- NULL
 
 temp1.2r <- ergmMPLE(g[[i]] ~ mutual, output = "array")$predictor[,,1]
 dimnames(temp1.r) <- NULL
 
 temp2 <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor[,,1]
 dimnames(temp2) <- NULL
 
 outmat <- temp1 * temp2
 outmat2 <- temp1.r * temp2
 outmat3 <- temp1.2r * temp2
 
 diag(outmat3) <- diag(outmat2) <- diag(outmat) <- 0
 
 rownames(outmat) <- colnames(outmat) <- g[[i]] %v% "vertex.names"
 rownames(outmat2) <- colnames(outmat2) <- g[[i]] %v% "vertex.names"
 rownames(outmat3) <- colnames(outmat3) <- g[[i]] %v% "vertex.names"
 
 transitivity.X.same.candidate[[i]] <- outmat
 cyclic.closure.X.same.candidate[[i]] <- outmat2
 mutual.X.same.candidate[[i]] <- outmat3
}


final.model2 <- btergm(
g ~ edges + 
  nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
  nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
  nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
  nodeicov("candidate.preference") + nodeocov("candidate.preference") + 
  nodematch("candidate.preference") + edgecov(policy.pref.sim) + 
  edgecov(evaludative.criteria.sim) + 
  
  nodeicov("age") + nodeocov("age") + nodeifactor("gender") + 
  nodeofactor("gender") + nodematch("gender") + nodeicov("edu") + 
  nodeocov("edu") + nodeicov("talk.freq") + nodeocov("talk.freq") + 
  nodeicov("media.use.freq") + nodeocov("media.use.freq") + 
  nodecov("internal.efficacy") + nodecov("external.efficacy") + 
  nodeifactor("region_origin2") + 
  nodeofactor("region_origin2") + nodematch("region_origin2") + 
  
  edgecov(g_autoregression) + edgecov(g_delrecip) + edgecov(g_lagtransitivity) + 
  edgecov(g_lagcyclic) + nodeocov("lagged.sender.effect") + 
  nodeicov("lagged.receiver.effect") + 
  
  isolates + mutual + 
  dgwesp(decay = 1.5, fixed = T, type = "OTP") + dgwesp(decay = 1.5, fixed = T, type = "ITP") + 
  dgwesp(decay = 1.5, fixed = T,  type = "OSP") + dgwesp(decay = 1.5, fixed = T, type = "ISP") + 
  dgwdsp(decay = 1, fixed = T, type = "ITP") + dgwdsp(decay = 1,  fixed = T, type = "OSP") + 
  dgwdsp(decay = 1, fixed = T, type = "ISP") + 
  gwodegree(decay = 2.5, fixed = T) + gwidegree(decay = 3,fixed = T) + 
  
  edgecov(mutual.X.same.candidate) + edgecov(transitivity.X.same.candidate) + edgecov(cyclic.closure.X.same.candidate),

  R = 1000, parallel = "multicore", ncpus = 4)

# ===========================================================================
#                                                               Model 1                      
# ---------------------------------------------------------------------------
# edges                                             -1.217 [-2.172;   .573]  
# nodeicov.consistency.motivation                     .030 [ -.020;   .071]  
# nodeocov.consistency.motivation                    -.012 [ -.080;   .004]  
# nodeicov.understanding.motivation                  -.045 [ -.049;  -.013] *
# nodeocov.understanding.motivation                   .036 [  .025;   .116] *
# nodeicov.hedomic.motivation                        -.002 [ -.030;   .017]  
# nodeocov.hedomic.motivation                         .064 [  .020;   .100] *
# nodeicov.candidate.preference                       .020 [ -.045;   .098]  
# nodeocov.candidate.preference                       .003 [ -.137;   .100]  
# nodematch.candidate.preference                     -.017 [ -.157;   .306]  
# edgecov.policy.pref.sim[[i]]                       -.048 [ -.198;  -.012] *
# edgecov.evaludative.criteria.sim[[i]]               .188 [  .097;   .226] *
# nodeicov.age                                        .011 [ -.005;   .035]  
# nodeocov.age                                        .012 [ -.059;   .039]  
# nodeifactor.gender.1                                .031 [ -.005;   .049]  
# nodeofactor.gender.1                                .028 [ -.156;   .267]  
# nodematch.gender                                    .046 [  .016;   .082] *
# nodeicov.edu                                       -.005 [ -.017;   .006]  
# nodeocov.edu                                       -.007 [ -.015;   .022]  
# nodeicov.talk.freq                                  .050 [ -.015;   .064]  
# nodeocov.talk.freq                                  .015 [ -.114;   .069]  
# nodeicov.media.use.freq                            -.010 [ -.024;   .066]  
# nodeocov.media.use.freq                             .034 [  .010;   .130] *
# nodecov.internal.efficacy                           .007 [ -.035;   .022]  
# nodecov.external.efficacy                           .015 [  .001;   .098] *
# nodeifactor.region_origin2.1                       -.048 [ -.092;   .051]  
# nodeofactor.region_origin2.1                        .008 [ -.231;   .196]  
# nodematch.region_origin2                            .039 [  .003;   .070] *
# edgecov.g_autoregression[[i]]                       .205 [  .163;   .275] *
# edgecov.g_delrecip[[i]]                             .143 [ -.045;   .311]  
# edgecov.g_lagtransitivity[[i]]                      .009 [ -.029;   .014]  
# edgecov.g_lagcyclic[[i]]                           -.025 [ -.031;  -.014] *
# nodeocov.lagged.sender.effect                       .006 [  .003;   .019] *
# nodeicov.lagged.receiver.effect                     .004 [  .001;   .021] *
# isolates                                           1.166 [  .810;  1.170] *
# mutual                                              .900 [  .279;  1.290] *
# gwesp.OTP.fixed.1.5                                 .268 [ -.032;   .490]  
# gwesp.ITP.fixed.1.5                                -.171 [ -.205;  -.139] *
# gwesp.OSP.fixed.1.5                                 .164 [  .144;   .220] *
# gwesp.ISP.fixed.1.5                                 .111 [  .070;   .179] *
# gwdsp.ITP.fixed.1                                  -.006 [ -.012;  -.004] *
# gwdsp.OSP.fixed.1                                   .015 [ -.004;   .025]  
# gwdsp.ISP.fixed.1                                   .009 [ -.001;   .012]  
# gwodegree                                         -4.714 [-4.858; -4.654] *
# gwidegree                                         -4.407 [-5.362; -3.080] *
# edgecov.mutual.X.same.candidate[[i]]               -.130 [ -.571;   .444]  
# edgecov.transitivity.X.same.candidate[[i]]         -.012 [ -.121;   .070]  
# edgecov.cyclic.closure.X.same.candidate[[i]]        .022 [ -.005;   .066]  
# ---------------------------------------------------------------------------
#             Num. obs.                                     291096                       
# ===========================================================================
#   * 0 outside the confidence interval
 
## gof with 300 replications seems acceptable....
