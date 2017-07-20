


## define helper function
make.edgecov <- function(g, i, model_terms) {
  ergm_formula <- as.formula(paste0("g[[i]] ~", model_terms))
  out <- ergm::ergmMPLE(ergm_formula, output = "array")$predictor[,,1]
  out <- as.matrix(out)
  dimnames(out) <- NULL
  colnames(out) <- network::get.vertex.attribute(g[[i]], "vertex.names")
  rownames(out) <- colnames(out)
  diag(out) <- NA
  return(out)
}

## interaction with candidate preference homophily
pref.match.X.consistency <- list()
pref.match.X.understanding <- list()
pref.match.X.hedonic <- list()

for (i in 1:3) {
  
out_match  <-  make.edgecov(g, i, "nodematch('candidate.preference')")
out_consistency <-  make.edgecov(g, i, "nodecov('consistency.motivation')")
out_understanding <- make.edgecov(g, i, "nodecov('understanding.motivation')")
out_hedonic <- make.edgecov(g, i, "nodecov('hedomic.motivation')")


pref.match.X.consistency[[i]] <- out_match * out_consistency
pref.match.X.understanding[[i]] <- out_match * out_understanding
pref.match.X.hedonic[[i]] <- out_match * out_hedonic

diag(pref.match.X.consistency[[i]]) <- 0
diag(pref.match.X.understanding[[i]]) <- 0
diag(pref.match.X.hedonic[[i]]) <- 0

}

final.model.test.1 <- btergm(update.formula(final.model@formula, ~. + edgecov(pref.match.X.consistency)),
                             R = 1000, parallel = "multicore", ncpus = 10)

final.model.test.2 <- btergm(update.formula(final.model@formula, ~. + edgecov(pref.match.X.understanding)),
                             R = 1000, parallel = "multicore", ncpus = 10)

final.model.test.3 <- btergm(update.formula(final.model@formula, ~. + edgecov(pref.match.X.hedonic)),
                             R = 1000, parallel = "multicore", ncpus = 10)

## nodematch cnadidate preference * three motivations -- NOT significant...
omit.coef = "(age)|(gender)|(edu)|(talk)|(media)|(efficacy)|(g_autoregression)|(g_lagcyclic)|(g_delrecip)|(g_lagtransitivity)|(lagged)|(gwesp)|(gwdsp)|(degree)|(region_origin2)|(isolates)|(mutual)"

texreg::screenreg(list(final.model.test.1, final.model.test.2, final.model.test.3), digits = 4,
                  omit.coef = omit.coef)

# ===================================================================================================
#                                           Consistency Int     Understanding Int     Hedonic Int           
# ---------------------------------------------------------------------------------------------------
#   edges                                        -1.1143             -1.4004 *           -1.0349       
#                                           [-2.1600;  0.5402]  [-2.0508; -0.3382]  [-1.8950;  0.4745]
# nodeicov.consistency.motivation               0.0175              0.0287              0.0296       
#                                           [-0.0632;  0.0787]  [-0.0196;  0.0707]  [-0.0193;  0.0703]
# nodeocov.consistency.motivation              -0.0239 *           -0.0126             -0.0113       
#                                           [-0.1222; -0.0043]  [-0.0770;  0.0010]  [-0.0765;  0.0031]
# nodeicov.understanding.motivation            -0.0441 *           -0.0261             -0.0452 *     
#                                           [-0.0489; -0.0130]  [-0.0642;  0.0674]  [-0.0492; -0.0134]
# nodeocov.understanding.motivation             0.0374 *            0.0551 *            0.0353 *     
#                                           [ 0.0246;  0.1270]  [ 0.0438;  0.1241]  [ 0.0242;  0.1241]
# nodeicov.hedomic.motivation                  -0.0017             -0.0013             -0.0197       
#                                           [-0.0324;  0.0158]  [-0.0316;  0.0167]  [-0.0557;  0.0096]
# nodeocov.hedomic.motivation                   0.0641 *            0.0643 *            0.0462       
#                                           [ 0.0157;  0.1000]  [ 0.0155;  0.0990]  [-0.0084;  0.1010]
# nodeicov.candidate.preference                 0.0212              0.0194              0.0206       
#                                           [-0.0458;  0.1339]  [-0.0369;  0.1138]  [-0.0475;  0.1201]
# nodeocov.candidate.preference                 0.0021              0.0070              0.0017       
#                                           [-0.1454;  0.1115]  [-0.1335;  0.1070]  [-0.1448;  0.1076]
# nodematch.candidate.preference               -0.2192              0.3428             -0.3172       
#                                           [-0.8070;  0.0599]  [-0.3930;  1.6071]  [-0.5837;  0.0712]
# edgecov.policy.pref.sim[[i]]                 -0.0432 *           -0.0438 *           -0.0490 *     
#                                           [-0.1823; -0.0023]  [-0.1554; -0.0058]  [-0.1850; -0.0156]
# edgecov.evaludative.criteria.sim[[i]]         0.1909 *            0.1917 *            0.1905 *     
#                                           [ 0.1018;  0.2370]  [ 0.0944;  0.2342]  [ 0.0989;  0.2364]
# edgecov.pref.match.X.consistency[[i]]         0.0210                                               
#                                           [-0.0153;  0.0811]                                        
# edgecov.pref.match.X.understanding[[i]]                          -0.0354                           
#                                                               [-0.1459;  0.0307]                    
# edgecov.pref.match.X.hedonic[[i]]                                                     0.0314       
#                                                                                   [-0.0025;  0.0578]
# ---------------------------------------------------------------------------------------------------
#   Num. obs.                                291096              291096              291096            
# ===================================================================================================
#   * 0 outside the confidence interval


## interaction with policy preference homophily
policy.pref.sim.X.consistency <- list()
policy.pref.sim.X.understanding <- list()
policy.pref.sim.X.hedonic <- list()
for (i in 1:3) {
  
  out_consistency <-  make.edgecov(g, i, "nodecov('consistency.motivation')")
  out_understanding <- make.edgecov(g, i, "nodecov('understanding.motivation')")
  out_hedonic <- make.edgecov(g, i, "nodecov('hedomic.motivation')")
  
  
  policy.pref.sim.X.consistency[[i]] <- policy.pref.sim[[i]] * out_consistency
  policy.pref.sim.X.understanding[[i]] <- policy.pref.sim[[i]] * out_understanding
  policy.pref.sim.X.hedonic[[i]] <- policy.pref.sim[[i]] * out_hedonic
  
  diag(policy.pref.sim.X.consistency[[i]]) <- 0
  diag(policy.pref.sim.X.understanding[[i]]) <- 0
  diag(policy.pref.sim.X.hedonic[[i]]) <- 0
  
}

final.model.test.4 <- btergm(update.formula(final.model@formula, ~. + edgecov(policy.pref.sim.X.consistency)),
                             R = 1000, parallel = "multicore", ncpus = 10)

final.model.test.5 <- btergm(update.formula(final.model@formula, ~. + edgecov(policy.pref.sim.X.understanding)),
                             R = 1000, parallel = "multicore", ncpus = 10)

final.model.test.6 <- btergm(update.formula(final.model@formula, ~. + edgecov(policy.pref.sim.X.hedonic)),
                             R = 1000, parallel = "multicore", ncpus = 10)

# not significant as well..
texreg::screenreg(list(final.model.test.4, final.model.test.5, final.model.test.6), digits = 4,
                  omit.coef = omit.coef)

# =======================================================================================================
#                                                 Consistency Int     Understanding Int     Hedonic Int          
# -------------------------------------------------------------------------------------------------------
#   edges                                             -1.6624             -1.7606            -1.2963       
#                                               [-2.8485;  0.8442]  [-2.8311; 0.2814]  [-2.3264;  0.8428]
# nodeicov.consistency.motivation                    0.0826              0.0299             0.0294       
#                                               [-0.0159;  0.1248]  [-0.0195; 0.0717]  [-0.0187;  0.0700]
# nodeocov.consistency.motivation                    0.0416             -0.0119            -0.0117       
#                                               [-0.0823;  0.0795]  [-0.0766; 0.0030]  [-0.0777;  0.0035]
# nodeicov.understanding.motivation                 -0.0442 *            0.0044            -0.0449 *     
#                                               [-0.0479; -0.0137]  [-0.0539; 0.0224]  [-0.0508; -0.0127]
# nodeocov.understanding.motivation                  0.0378 *            0.0876 *           0.0362 *     
#                                               [ 0.0244;  0.1206]  [ 0.0388; 0.1302]  [ 0.0229;  0.1217]
# nodeicov.hedomic.motivation                       -0.0015             -0.0014             0.0091       
#                                               [-0.0305;  0.0158]  [-0.0316; 0.0162]  [-0.0896;  0.0519]
# nodeocov.hedomic.motivation                        0.0642 *            0.0636 *           0.0748 *     
#                                               [ 0.0153;  0.0991]  [ 0.0156; 0.0995]  [ 0.0100;  0.1017]
# nodeicov.candidate.preference                      0.0155              0.0183             0.0184       
#                                               [-0.0463;  0.1036]  [-0.0465; 0.1139]  [-0.0471;  0.1099]
# nodeocov.candidate.preference                      0.0026              0.0035             0.0034       
#                                               [-0.1452;  0.1106]  [-0.1442; 0.1091]  [-0.1462;  0.1109]
# nodematch.candidate.preference                    -0.0368             -0.0324            -0.0341       
#                                               [-0.0852;  0.0492]  [-0.0800; 0.0488]  [-0.0798;  0.0474]
# edgecov.policy.pref.sim[[i]]                       1.2315              1.4011             0.2154       
#                                               [-1.2566;  3.3058]  [-0.4335; 1.8386]  [-2.1031;  1.9100]
# edgecov.evaludative.criteria.sim[[i]]              0.1947 *            0.1935 *           0.1902 *     
#                                               [ 0.0979;  0.2362]  [ 0.1013; 0.2364]  [ 0.0976;  0.2466]
# edgecov.policy.pref.sim.X.consistency[[i]]        -0.1446                                              
#                                               [-0.3787;  0.1209]                                       
# edgecov.policy.pref.sim.X.understanding[[i]]                          -0.1354                          
#                                                                   [-0.1772; 0.0346]                    
# edgecov.policy.pref.sim.X.hedonic[[i]]                                                   -0.0292       
#                                                                                       [-0.2212;  0.2224]
# -------------------------------------------------------------------------------------------------------
#   Num. obs.                                     291096              291096             291096            
# =======================================================================================================
#   * 0 outside the confidence interval


## interaction with evaluative criteria homophily
eval.criteria.sim.X.consistency <- list()
eval.criteria.sim.X.understanding <- list()
eval.criteria.sim.X.hedonic <- list()
for (i in 1:3) {
  
  out_consistency <-  make.edgecov(g, i, "nodecov('consistency.motivation')")
  out_understanding <- make.edgecov(g, i, "nodecov('understanding.motivation')")
  out_hedonic <- make.edgecov(g, i, "nodecov('hedomic.motivation')")
  
  
  eval.criteria.sim.X.consistency[[i]] <- evaludative.criteria.sim[[i]] * out_consistency
  eval.criteria.sim.X.understanding[[i]] <- evaludative.criteria.sim[[i]] * out_understanding
  eval.criteria.sim.X.hedonic[[i]] <- evaludative.criteria.sim[[i]] * out_hedonic
  
  diag(eval.criteria.sim.X.consistency[[i]]) <- 0
  diag(eval.criteria.sim.X.understanding[[i]]) <- 0
  diag(eval.criteria.sim.X.hedonic[[i]]) <- 0
  
}

final.model.test.7 <- btergm(update.formula(final.model@formula, ~. + edgecov(eval.criteria.sim.X.consistency)),
                             R = 1000, parallel = "multicore", ncpus = 10)

final.model.test.8 <- btergm(update.formula(final.model@formula, ~. + edgecov(eval.criteria.sim.X.understanding)),
                             R = 1000, parallel = "multicore", ncpus = 10)

final.model.test.9 <- btergm(update.formula(final.model@formula, ~. + edgecov(eval.criteria.sim.X.hedonic)),
                             R = 1000, parallel = "multicore", ncpus = 10)

texreg::screenreg(list(final.model.test.7, final.model.test.8, final.model.test.9), digits = 4,
                  omit.coef = omit.coef)

# ==========================================================================================================
#                                                    Consistency Int     Understanding Int     Hedonic Int           
# ----------------------------------------------------------------------------------------------------------
#   edges                                               -0.6341             -1.2973 *           -1.6749 *     
#                                                 [-1.6415;  0.3979]  [-2.0865; -0.0577]  [-2.6723; -0.4696]
# nodeicov.consistency.motivation                     -0.0350              0.0294              0.0287       
#                                                 [-0.1649;  0.0685]  [-0.0196;  0.0699]  [-0.0196;  0.0683]
# nodeocov.consistency.motivation                     -0.0775             -0.0116             -0.0120       
#                                                 [-0.2253;  0.0047]  [-0.0769;  0.0033]  [-0.0763;  0.0034]
# nodeicov.understanding.motivation                   -0.0446 *           -0.0366             -0.0451 *     
#                                                 [-0.0474; -0.0137]  [-0.0497;  0.0361]  [-0.0480; -0.0148]
# nodeocov.understanding.motivation                    0.0377 *            0.0447 *            0.0368 *     
#                                                 [ 0.0248;  0.1270]  [ 0.0403;  0.1343]  [ 0.0253;  0.1245]
# nodeicov.hedomic.motivation                         -0.0011             -0.0017              0.0497       
#                                                 [-0.0311;  0.0159]  [-0.0315;  0.0157]  [-0.0751;  0.1181]
# nodeocov.hedomic.motivation                          0.0650 *            0.0640 *            0.1155       
#                                                 [ 0.0162;  0.0994]  [ 0.0156;  0.1001]  [-0.0280;  0.2008]
# nodeicov.candidate.preference                        0.0206              0.0185              0.0183       
#                                                 [-0.0487;  0.1187]  [-0.0467;  0.1135]  [-0.0475;  0.1138]
# nodeocov.candidate.preference                        0.0054              0.0031              0.0030       
#                                                 [-0.1471;  0.1113]  [-0.1467;  0.1106]  [-0.1452;  0.1121]
# nodematch.candidate.preference                      -0.0343             -0.0337             -0.0341       
#                                                 [-0.0796;  0.0479]  [-0.0793;  0.0490]  [-0.0792;  0.0484]
# edgecov.policy.pref.sim[[i]]                        -0.0528 *           -0.0487 *           -0.0482 *     
#                                                 [-0.1825; -0.0240]  [-0.1789; -0.0149]  [-0.1800; -0.0135]
# edgecov.evaludative.criteria.sim[[i]]               -0.9355              0.3764 *            1.1209       
#                                                 [-2.4299;  0.8581]  [ 0.0653;  1.2066]  [-0.6290;  2.0646]
# edgecov.eval.criteria.sim.X.consistency[[i]]         0.1276                                               
#                                                 [-0.0857;  0.2861]                                        
# edgecov.eval.criteria.sim.X.understanding[[i]]                          -0.0173                           
#                                                                     [-0.1026;  0.0121]                    
# edgecov.eval.criteria.sim.X.hedonic[[i]]                                                    -0.1026       
#                                                                                         [-0.2066;  0.0850]
# ----------------------------------------------------------------------------------------------------------
#   Num. obs.                                       291096              291096              291096            
# ==========================================================================================================
#   * 0 outside the confidence interval
# 