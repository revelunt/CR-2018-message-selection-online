

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


## next, multiple shared popularity and shared activity,
## assuming such patterns are driven by similarty of ego-alter characteristics 
shared.popularity.X.same.candidate <- list()
shared.activity.X.same.candidate <- list()

for (i in 1:3) {
  temp1 <- ergmMPLE(g[[i]] ~ dgwesp(2, fixed = T, type = "ISP"), output = "array")$predictor[,,1]
  dimnames(temp1) <- NULL
  
  temp2 <- ergmMPLE(g[[i]] ~ dgwesp(3, fixed = T, type = "OSP"), output = "array")$predictor[,,1]
  dimnames(temp2) <- NULL
  
  temp3 <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor[,,1]
  dimnames(temp3) <- NULL
  
  outmat1 <- temp1 * temp3
  outmat2 <- temp2 * temp3
  
  diag(outmat2) <- diag(outmat1) <- 0
  rownames(outmat1) <- colnames(outmat1) <- g[[i]] %v% "vertex.names"
  rownames(outmat2) <- colnames(outmat2) <- g[[i]] %v% "vertex.names"
  
  shared.popularity.X.same.candidate[[i]] <- outmat1
  shared.activity.X.same.candidate[[i]] <- outmat2
  
}


final.model5 <- btergm(g ~ edges + 
                         
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
                         edgecov(shared.popularity.X.same.candidate) + 
                         edgecov(shared.activity.X.same.candidate),
                       
                       R = 1000, parallel = "multicore", ncpus = parallel::detectCores()); summary(final.model5)
