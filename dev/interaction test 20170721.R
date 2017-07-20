
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
