
can.image.sim <- cand.image <- list()

cand.image[[1]] <- sqrt(outer(g[[1]] %v% "candidate.image.Park", g[[1]] %v% "candidate.image.Park", "-")^2 + 
                                outer(g[[1]] %v% "candidate.image.Moon", g[[1]] %v% "candidate.image.Moon", "-")^2)

cand.image[[2]] <- sqrt(outer(g[[2]] %v% "candidate.image.Park", g[[2]] %v% "candidate.image.Park", "-")^2 + 
                                outer(g[[2]] %v% "candidate.image.Moon", g[[2]] %v% "candidate.image.Moon", "-")^2)

cand.image[[3]] <- sqrt(outer(g[[3]] %v% "candidate.image.Park", g[[3]] %v% "candidate.image.Park", "-")^2 + 
                                outer(g[[3]] %v% "candidate.image.Moon", g[[3]] %v% "candidate.image.Moon", "-")^2)

for (i in 1:3) {
  can.image.sim[[i]] <- 1 / (1 + cand.image[[i]])
  diag(can.image.sim[[i]]) <- 0}

model_qap <- btergm(g ~ edges + 
                      
                        edgecov(g_autoregression) + 
                        nodecov("knowledge") + 
                        nodecov("talk.freq") + 
                        nodecov("interest") + 
                        edgecov(can.image.sim) + 
                        edgecov(evaludative.criteria.sim) + 
                        absdiff("pol.ideology") + 
                        edgecov(policy.pref.sim) + 
                        
                      dgwesp(decay = 1.5, fixed = T, type = "OTP") + ## transitive closure. expected to be positive
                      dgwesp(decay = 1.5, fixed = T, type = "ITP") + ## cyclic closure. expected to be negative
                      ## combined, represent highly hierarchical network structure 
                      
                      ## structural homophily
                      dgwesp(decay = 1.5, fixed = T, type = "OSP") + ## number of common "posters" (similarity of out-tie) between dyad
                      dgwesp(decay = 1.5, fixed = T, type = "ISP") + ## common "viewer" (similarity of in-ties) between dyad
                      
                      ## control for gwesp
                      dgwdsp(decay = 1, fixed = T, type = "ITP") + 
                      dgwdsp(decay = 1, fixed = T, type = "OSP") + 
                      dgwdsp(decay = 1, fixed = T, type = "ISP") + 
                      
                      ## hub/star structure
                      ## preferential attachments for receiving and sending ties 
                      gwodegree(decay = 2.5, fixed = T) + gwidegree(decay = 3, fixed = T),
                    
                    verbose = T, R = 1000, ncpus = 10, parallel = "multicore"); summary(model_qap)


model.gap.gof <- gof(model_qap, statistics = c(dsp, esp, odeg, ideg, geodesic, triad.directed),
                  nsim = 200, verbose = TRUE, ncpus = 4, parallel = "multicore")




net <- g[[3]]
g_autoregression.3 <- g_autoregression[[3]]
g_delrecip.3 <- g_delrecip[[3]]
g_lagtransitivity.3 <- g_lagtransitivity[[3]]
g_lagcyclic.3 <- g_lagcyclic[[3]]
policy.pref.sim.3 <- policy.pref.sim[[3]]
evaludative.criteria.sim.3 <- evaludative.criteria.sim[[3]]

init.val <- enformulate.curved(test.model)
init.val$theta <- coef(test.model.2)

test.model.2 <- btergm(g ~ edges + nodeicov("age") + nodeocov("age") +
                     nodeifactor("gender") + nodeofactor("gender") + nodematch("gender") +
                     nodeicov("edu") + nodeocov("edu") +
                     nodeicov("talk.freq") + nodeocov("talk.freq") +
                     nodeicov("media.use.freq") + nodeocov("media.use.freq") +
                     nodecov("internal.efficacy") + nodecov("external.efficacy") +
                     nodeifactor("region_origin2") + nodeofactor("region_origin2") + nodematch("region_origin2") +
                     
                     edgecov(g_autoregression) + edgecov(g_delrecip) + edgecov(g_lagtransitivity) +
                     edgecov(g_lagcyclic) + #nodeofactor("source") + nodeifactor("sink") +
                     nodeocov("lagged.sender.effect") +  ## persistent sender effect
                     nodeicov("lagged.receiver.effect") +
                     isolates + mutual + #idegreepopularity + odegreepopularity +
                     
                     #twopath + 
                     dgwdsp(decay = 1, fixed = T, type = "ITP") +
                     dgwdsp(decay = 1, fixed = T, type = "OSP") +
                     dgwdsp(decay = 1, fixed = T, type = "ISP") +
                     
                     dgwesp(decay = 1.5, fixed = T, type = "ITP") +
                     dgwesp(decay = 1.5, fixed = T, type = "OSP") +
                     dgwesp(decay = 1.5, fixed = T, type = "ISP") +
                     #transitiveties + cyclicalties +    
                     gwodegree(decay = 2.5, fixed = T) + gwidegree(decay = 3, fixed = T) +
                     
                     nodeicov("consistency.motivation") + nodeocov("consistency.motivation") +
                     nodeicov("understanding.motivation") + nodeocov("understanding.motivation") +
                     nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") +
                     nodeicov("candidate.preference") + nodeocov("candidate.preference") +
                     nodematch("candidate.preference") + dyadcov(policy.pref.sim) +
                     dyadcov(evaludative.criteria.sim), 
                   
                   verbose = T, R = 100) 
