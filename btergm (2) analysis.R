
## script for paper entitled as 
## "Effects of motivation, homophily, and endogenous network process on message exposure within online discussion forum"

options(scipen = 999)
require(ergm)
require(btergm)
require(texreg)
require(parallel)

# setwd("~/Dropbox/GitHub/Korean2012ElectionProject")
source("dev/btergm helper-functions.R")
source("dev/btergm (1) data prep.R")

RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")


##------------------------------------##
##  MR QAP using muticore processing  ##
##------------------------------------##

source("dev/MRQAP 20170718.R")

MRQAP.model <- netlm.multicore(net, predictor.matrix, intercept = TRUE, mode = "digraph", diag = FALSE, 
                               nullhyp = "qapspp", 
                               test.statistic = "t-value", reps = 1000, 
                               mc.cores = 10) 

MRQAP.model$names <- c("Intercept",
                       "consistency.motivation.in-ties",
                       "consistency.motivation.out-ties",
                       "understanding.motivation.in-ties",
                       "understanding.motivation.out-ties",
                       "hedonic.motivation.in-ties",
                       "hedonic.motivation.out-ties",
                       "candidate.preference.in-ties",
                       "candidate.preference.out-ties",
                       "same.candidate.preference",
                       "similar.policy.preference",
                       "similar.evaluative.criteria",
                       
                       "age.in-ties",
                       "age.out-ties",
                       "female.in-ties",
                       "female.out-ties",
                       "same.gender",
                       "education.in-ties",
                       "education.out-ties",
                       "talk.freqency.in-ties",
                       "talk.frequency.out-ties",
                       "media.use.in-ties",
                       "media.use.out-ties",
                       "internal.political.efficacy",
                       "regional.origin.Seoul.in-ties",
                       "regional.origin.Seoul.out-ties",
                       "same.regional.origin",
                       
                       "previous.communication",
                       "lagged.reciprocity",
                       "mutual")

# save(MRQAP.model, file = "R_results/MRQAP.model.2017.07.18.Rdata")

## goodness of fit for MRQAP model

# MRQAP.gof <- gof(net, predictor.matrix, coef(MRQAP.model), statistics = gof.statistics, 
#                  nsim = 1000, verbose = TRUE, ncpus = gof_ncpus, parallel = parallel)

print(MRQAP.model)

# OLS Network Model
# 
# Coefficients:
#                                  Estimate     Pr(<=b) Pr(>=b) Pr(>=|b|)
# Intercept                          1.136435834 0.875   0.125   0.246    
# consistency.motivation.in-ties     0.029604166 0.627   0.373   0.731    
# consistency.motivation.out-ties   -0.011562130 0.378   0.622   0.736    
# understanding.motivation.in-ties  -0.209034044 0.022   0.978   0.039    
# understanding.motivation.out-ties  0.137493206 1.000   0.000   0.000    
# hedonic.motivation.in-ties         0.098864675 0.896   0.104   0.192    
# hedonic.motivation.out-ties       -0.309703595 0.000   1.000   0.000    
# candidate.preference.in-ties      -0.020373837 0.431   0.569   0.895    
# candidate.preference.out-ties      0.297525386 1.000   0.000   0.000    
# same.candidate.preference          0.013857291 0.595   0.405   0.809    
# similar.policy.preference         -0.111014163 0.345   0.655   0.690    
# similar.evaluative.criteria        0.581737039 0.988   0.012   0.028    
# age.in-ties                        0.029475407 0.639   0.361   0.732    
# age.out-ties                       0.338954211 1.000   0.000   0.000    
# female.in-ties                    -0.062962660 0.375   0.625   0.692    
# female.out-ties                   -0.001681675 0.484   0.516   0.974    
# same.gender                        0.055390757 0.849   0.151   0.316    
# education.in-ties                 -0.107442301 0.077   0.923   0.146    
# education.out-ties                -0.235659167 0.000   1.000   0.000    
# talk.freqency.in-ties              0.206848711 0.992   0.008   0.015    
# talk.frequency.out-ties            0.049758120 0.914   0.086   0.157    
# media.use.in-ties                 -0.062429272 0.142   0.858   0.297    
# media.use.out-ties                -0.136069055 0.000   1.000   0.000    
# internal.political.efficacy        0.043077679 0.837   0.163   0.308    
# regional.origin.Seoul.in-ties     -0.407244170 0.000   1.000   0.005    
# regional.origin.Seoul.out-ties     0.464350407 1.000   0.000   0.000    
# same.regional.origin              -0.032265597 0.277   0.723   0.552    
# previous.communication             2.338941251 1.000   0.000   0.000    
# lagged.reciprocity                -0.228284640 0.000   1.000   0.002    
# mutual                             0.416014609 1.000   0.000   0.000    
# 
# Residual standard error: 7.472 on 76787 degrees of freedom
# F-statistic:  1294 on 29 and 76787 degrees of freedom, p-value:     0 
# Multiple R-squared: 0.3283 	Adjusted R-squared: 0.3281 


## --------------------------- ##
## Bootstrapped TERGM analysis ##
## ----------------------------##


## few global options
gof.statistics <- c(dsp, odeg, ideg, desp_OTP, desp_ITP, desp_OSP, desp_ISP,
                    geodesic, triad.directed, rocpr, walktrap.modularity)
R <- 1000
parallel <- "multicore"
ncpus <- 10
gof_ncpus <- 4


## first, start with control only model
model1 <- btergm(g ~ edges + ## intercept
                   
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
                   nodecov("internal.efficacy"),
                   
                 verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(model1)

save(model1, file = "R_results/July19.model1.Rdata")

## next, we add pure structural effects
model2 <- btergm(update.formula(model1@formula, ~ . + 
                                  
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
                                  gwidegree(decay = 3, fixed = T)), ## hedonic 
   
                 verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(model2)

save(model2, file = "R_results/July19.model2.Rdata")

## add motivations and dyadic homophily predictors
final.model <- btergm(g ~ edges + ## intercept
                   
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
                   dgwesp(decay = 3, fixed = T, type = "OTP") + ## 3 or 1.5 understanding
                   dgwesp(decay = 3, fixed = T, type = "ITP") + ## 3 or 1.5 understanding
                   dgwesp(decay = 3, fixed = T, type = "OSP") + ## 3 or 1.5 consistency
                   dgwesp(decay = 2, fixed = T, type = "ISP") + ## 3 or 1.5 consistency
                   
                   gwodegree(decay = 2, fixed = T) + ## hedonic
                   gwidegree(decay = 3, fixed = T), ## hedonic 
                 
                 verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(final.model)

save(final.model, file = "R_results/July19.final.model.Rdata")

## here we estimate a goodness of fit for this interim model.
final.model.gof <- gof(final.model, statistics = gof.statistics, 
                  nsim = 300, verbose = TRUE, ncpus = gof_ncpus, parallel = parallel)
plot(final.model.gof, mfrow = FALSE, xlim = 40)
# save(final.model.gof, file = "July19.final.model.gof.Rdata")

pdf(file = "final.model.gof.pdf", width = 14, height = 10)

plot(final.model.gof$`Dyad-wise shared partners`, xlim = 40, relative = F)
plot(final.model.gof$Outdegree, xlim = 40, relative = F)
plot(final.model.gof$Indegree, xlim = 40, relative = F)
plot(final.model.gof$`Directed edge-wise shared partners of type OTP`, xlim = 40, relative = F)
plot(final.model.gof$`Directed edge-wise shared partners of type ITP`, xlim = 40, relative = F)
plot(final.model.gof$`Directed edge-wise shared partners of type OSP`, xlim = 40, relative = F)
plot(final.model.gof$`Directed edge-wise shared partners of type ISP`, xlim = 40, relative = F)
plot(final.model.gof$`Geodesic distances`)
plot(final.model.gof$`Triad census`)
plot(final.model.gof$`Tie prediction`)
plot(final.model.gof$`Modularity (walktrap)`)
dev.off()

# save(model1, model2, final.model, file = "R_results/btergm.results.July 19th.Rdata")
# save(final.model.gof, file = "R_results/btergm.gof.results.July 19th.Rdata")

# ===========================================================================================================================================
#                                                       Control only                   Control + Structural           Final Model                  
# -------------------------------------------------------------------------------------------------------------------------------------------
# Edges (Intercept)                                 -4.662 [-6.615; -3.259] *      -1.150 [-2.185;   .292]        -1.854 [-2.904;  -.381] *
# Motivation and Homophily                                                                                                                   
#   Consistency motivation (in-ties)                                                                                .031 [ -.019;   .095]  
#   Consistency motivation (out-ties)                                                                               .028 [ -.107;   .073]  
#   Understanding motivation (in-ties)                                                                             -.052 [ -.104;   .021]  
#   Understanding motivation (out-ties)                                                                             .027 [  .005;   .076] *
#   Hedonic motivation (in-ties)                                                                                   -.008 [ -.029;   .004]  
#   Hedonic motivation (out-ties)                                                                                   .095 [  .074;   .119] *
#   Candidate pref = Moon (in-ties)                                                                                 .002 [ -.010;   .094]  
#   Candidate pref = Moon (out-ties)                                                                                .013 [ -.130;   .112]  
#   Same candidate pref                                                                                            -.032 [ -.079;   .048]  
#   Similar policy pref                                                                                            -.108 [ -.215;   .028]  
#   Similar evaluative criteria                                                                                     .407 [  .207;   .415] *
# Endogenous structural effects                                                                                                              
#   Isolates                                                                        1.019 [  .803;  1.250] *       1.019 [  .790;  1.262] *
#   Reciprocity                                                                      .765 [  .497;  1.066] *        .768 [  .507;  1.067] *
#   Multiple path closure (GWESP-OTP, 3)                                             .058 [ -.055;   .125]          .058 [ -.053;   .126]  
#   Multiple cyclic closure (GWESP-ITP, 3)                                          -.068 [ -.082;  -.060] *       -.066 [ -.080;  -.060] *
#   Multiple activity closure (GWESP-OSP, 3)                                         .035 [  .029;   .053] *        .035 [  .032;   .053] *
#   Multiple popularity closure (GWESP-ISP, 2)                                       .117 [  .080;   .240] *        .115 [  .082;   .233] *
#   Multiple two-paths (GWDSP, 1)                                                    .003 [ -.007;   .009]          .003 [ -.007;   .009]  
#   Activity spread (GW-outdegree, 2)                                              -4.401 [-4.701; -4.144] *      -4.351 [-4.557; -4.034] *
#   Popularity spread (GW-indegree, 3)                                             -4.056 [-5.271; -3.289] *      -4.047 [-5.313; -3.233] *
# Lagged structural effects                                                                                                                  
#   Previous communication                                                           .214 [  .182;   .256] *        .223 [  .194;   .253] *
#   Delayed reciprocity                                                              .082 [ -.059;   .352]          .073 [ -.072;   .344]  
#   Delayed transitivity closure                                                     .034 [  .017;   .057] *        .034 [  .019;   .055] *
#   Delayed cyclic closure                                                           .037 [  .009;   .057] *        .033 [  .007;   .057] *
#   Delayed activity closure                                                        -.057 [ -.068;  -.036] *       -.056 [ -.068;  -.036] *
#   Delayed popularity closure                                                      -.060 [ -.110;  -.035] *       -.059 [ -.110;  -.032] *
#   Persistent sender (out-tie)                                                      .019 [  .009;   .028] *        .019 [  .010;   .029] *
#   Persistent receiver (in-ties)                                                    .023 [  .019;   .036] *        .024 [  .018;   .038] *
# Controls                                                                                                                                   
#   Age (in-ties)                                     .086 [ -.030;   .166]          .004 [ -.015;   .032]         -.001 [ -.019;   .026]  
#   Age (out-ties)                                    .211 [ -.119;   .383]          .032 [ -.225;   .073]          .052 [ -.193;   .095]  
#   Female (in-ties)                                 -.185 [ -.304;  -.134] *       -.003 [ -.044;   .047]          .010 [ -.037;   .065]  
#   Female (out-ties)                                -.194 [ -.456;  -.122] *        .075 [ -.293;   .436]          .013 [ -.356;   .337]  
#   Gender homophily                                  .010 [ -.032;   .037]          .050 [  .020;   .095] *        .044 [  .019;   .086] *
#   Education (in-ties)                              -.120 [ -.182;  -.076] *       -.007 [ -.041;   .017]         -.013 [ -.039;   .014]  
#   Education (out-ties)                             -.123 [ -.234;   .055]          .028 [ -.009;   .097]          .018 [ -.013;   .083]  
#   Regional origin = Seoul (in-ties)                -.426 [ -.492;  -.300] *       -.077 [ -.135;   .054]         -.086 [ -.163;   .047]  
#   Regional origin = Seoul (out-ties)               -.179 [ -.382;   .005]         -.145 [ -.656;   .343]         -.120 [ -.608;   .366]  
#   Regional homophily (Seoul)                       -.021 [ -.053;   .029]          .013 [ -.022;   .080]          .017 [ -.014;   .080]  
#   Talk freq (in-ties)                               .110 [ -.128;   .272]          .045 [  .018;   .048] *        .042 [  .010;   .045] *
#   Talk freq (out-ties)                              .048 [ -.391;   .396]          .033 [ -.119;   .177]          .019 [ -.106;   .156]  
#   Media use (in-ties)                              -.058 [ -.111;   .516]         -.011 [ -.022;   .020]         -.010 [ -.019;   .032]  
#   Media use (out-ties)                             -.079 [ -.117;   .602]          .040 [  .004;   .288] *        .033 [ -.014;   .287]  
#   Internal efficacy                                 .124 [  .084;   .188] *       -.015 [ -.063;   .015]          .003 [ -.047;   .034]  
# -------------------------------------------------------------------------------------------------------------------------------------------
#           Num. obs.                                       291085                         291096                         291096                       
# ===========================================================================================================================================
  
## all models to a file
texreg::htmlreg(list(model1, model2, final.model), digits = 3, leading.zero = F, single.row = T,
                  custom.model.names = c("Control only", "Control + Structural", "Final Model"),
                  custom.coef.names = c("Edges (Intercept)", "Age (in-ties)", "Age (out-ties)",
                                        "Female (in-ties)", "Female (out-ties)", "Gender homophily",
                                        "Education (in-ties)", "Education (out-ties)",
                                        "Regional origin = Seoul (in-ties)",
                                        "Regional origin = Seoul (out-ties)",
                                        "Regional homophily (Seoul)",
                                        "Talk freq (in-ties)", "Talk freq (out-ties)", 
                                        "Media use (in-ties)", "Media use (out-ties)",
                                        "Internal efficacy", 
                                        "Isolates", "Reciprocity", "Previous communication",
                                        "Multiple two-paths (GWDSP, 1)", 
                                        "Delayed reciprocity", 
                                        "Delayed transitivity closure", "Delayed cyclic closure", 
                                        "Delayed activity closure", "Delayed popularity closure",
                                        "Persistent sender (out-tie)", "Persistent receiver (in-ties)",
                                        "Multiple path closure (GWESP-OTP, 3)", "Multiple cyclic closure (GWESP-ITP, 3)", 
                                        "Multiple activity closure (GWESP-OSP, 3)", "Multiple popularity closure (GWESP-ISP, 2)",
                                        "Activity spread (GW-outdegree, 2)", "Popularity spread (GW-indegree, 3)",
                                        "Consistency motivation (in-ties)", "Consistency motivation (out-ties)",
                                        "Understanding motivation (in-ties)", "Understanding motivation (out-ties)", 
                                        "Hedonic motivation (in-ties)", "Hedonic motivation (out-ties)", 
                                        "Candidate pref = Moon (in-ties)", "Candidate pref = Moon (out-ties)", "Same candidate pref", 
                                        "Similar policy pref", "Similar evaluative criteria"),
                  custom.note = " * 0 outside the 95% confidence interval based on 1000 replications", 
                  reorder.coef = c(1, 34:44, 17:18,28:31,20,32:33, 19,21:27, 2:16),
                  groups = list("Motivation and Homophily" = 2:12, 
                                "Endogenous structural effects" = 13:21,
                                "Lagged structural effects" = 22:29,
                                "Controls" = 30:44),
                  bold = 0.5, doctype = T, html.tag = T, body.tag = T, indentation = "  ",
                  caption = "",
                file = "results.Table.July20.doc")


## --------------------------- ##
## produce coefficient-CI plot ##
## --------------------------- ##


ggdat_fig <- data.table(
  var = names(final.model@coef),
  coef = coef(final.model),
  q025 = apply(final.model@boot$t, 2, quantile, .025),
  q975 = apply(final.model@boot$t, 2, quantile, .975))

ggdat_fig <- ggdat_fig[c(2:16, 17:27, 30,32:38, 1,28:29,31,39:44)]

y <- c("Age in 10 years \n(in-ties)", "Age in 10 years \n(out-ties)", 
       "Gender: female \n(in-ties)", "Gender: female \n(out-ties)", "Gender \nhomophily", 
       "Education \n(in-ties)", "Education \n(out-ties)", 
       "Origin = Seoul \n(in-ties)", "Origin = Seoul \n(out-ties)", "Origin = Seoul \nhomophily", 
       "Offline talk freq \n(in-ties)", "Offline talk freq \n(out-ties)", 
       "Media use freq \n(in-ties)", "Media use freq \n(out-ties)", 
       "Internal \nefficacy", 
       "Motivation \nconsistency (in-tie)", "Motivation \nconsistency (out-tie)", 
       "Motivation \nunderstanding (in-tie)", "Motivation \nunderstanding (out-tie)", 
       "Motivation \nhedonic (in-tie)", "Motivation \nhedonic (out-tie)",
       "Candidate preference \n(Moon, in-tie)", "Candidate preference \n(Moon, out-tie)", 
       "Candidate preference \nhomophily", "Policy preference \nhomophily", "Evaluative criteria \nhomophily",
       "Edge autoregression", "Delayed reciprocity", 
       "Delayed \ntransitivity closure", "Delayed \ncylic closure", 
       "Delayed \nactivity closure", "Delayed \npopularity closure",
       "Persistent sender", "Persistent receiver", 
       "Edges (intercept)", "Isolates", "Reciprocity",  
       "Multiple two-paths \n(GWDSP, 1)", 
       "Multiple path closure \n(GWESP-OTP, 3)", "Multiple cyclic closure \n(GWESP-ITP, 3)", 
       "Multiple activity closure \n(GWESP-OSP, 3)", "Multiple popularity closure \n(GWESP-ISP, 2)",
       "Activity spread \n(GW-outdegree, 2)", "Popularity spread \n(GW-indegree, 3)")

ggdat_fig[, y := factor(y, levels = rev(y))]
ggdat_fig[, sig := !(is.between(0, q025, q975))]

require(gridExtra)
pdf(file = "final.model.coef.pdf", width = 14, height = 10)
## control variables
p1 <- ggplot(ggdat_fig[1:15,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point(size = 2.5) + geom_errorbarh(height = 0, size = 1.5) +
  xlab("Coefficient") + ylab("") + ggtitle("Demographics and controls") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(values = c("grey", "red"))

## discussion motivation and homophily effects
p2 <- ggplot(ggdat_fig[16:26,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point(size = 2.5) + geom_errorbarh(height = 0, size = 1.5) +
  xlab("Coefficient") + ylab("") + ggtitle("Motivation and homophily") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +  
  scale_colour_manual(values = c("grey", "red"))

## pure lagged structural effects
p3 <- ggplot(ggdat_fig[27:34,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point(size = 2.5) + geom_errorbarh(height = 0, size = 1.5) +
  xlab("Coefficient") + ylab("") + ggtitle("Lagged structural effects") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(values = c("grey", "red")) 

## pure concurrent structural effects
p4 <- ggplot(ggdat_fig[35:44,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point(size = 2.5) + geom_errorbarh(height = 0, size = 1.5) +
  xlab("Coefficient") + ylab("") + ggtitle("Concurrent structural effects") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(values = c("grey", "red")) 

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
dev.off()
## alternatively, use plotreg in texreg packages
## plotreg(final.model, omit.coef = "(edges)|(gwidegree)|(gwodegree)|(isolates)")



## interpretation of the homophily terms

test <- compute_dyadic_probabilities_btergm(final.model, "nodematch.candidate.preference", 0.41, n_sample = 50)
test <- calc_prob_diffs(test)
# create barplots from probability ratios and CIs
require("gplots")
bp <- barplot2(test[1:3, ], beside = TRUE, plot.ci = TRUE, 
               ci.l = test[4:6, ], ci.u = test[7:9, ], 
               col = c("tan", "tan2", "tan3"), ci.col = "grey40", 
               xlab = "Dyadic tie values", ylab = "Estimated Prob. minus Null Prob.")
mtext(1, at = bp, text = c("No ties", "Asymmetric tie", "Reciprocated ties"), line = 0, cex = 0.5)


## stuructural effects
## derive predicted probability plot using final model
## nodes who have exactly indegree of 0 to 9
degree.num <- 0:9
receiver.list.t1 <- lapply(degree.num, function(i) which(sna::degree(g[[1]], cmode="indegree") == i))
receiver.list.t2 <- lapply(degree.num, function(i) which(sna::degree(g[[2]], cmode="indegree") == i))
receiver.list.t3 <- lapply(degree.num, function(i) which(sna::degree(g[[3]], cmode="indegree") == i))

edgeprob.test <- btergm::edgeprob(final.model)
setDT(edgeprob.test)
index <- 1:312

out.t1 <- lapply(seq_len(length(receiver.list.t1)), function(m) {
  d <- receiver.list.t1[[m]]
  out <- sapply(seq_len(length(d)), function(k) {
    index.to.sample <- index[-(network::get.neighborhood(g[[1]], d[k], "in"))]
    if (m == 1) {index.to.sample <- index}
    index.to.sample <- setdiff(index.to.sample, d[k])
    
    edgeprob.test[j==d[k] & (i %in% index.to.sample) & t == 1, probability]})
})
out.t2 <- lapply(seq_len(length(receiver.list.t2)), function(m) {
  d <- receiver.list.t2[[m]]
  out <- sapply(seq_len(length(d)), function(k) {
    index.to.sample <- index[-(network::get.neighborhood(g[[2]], d[k], "in"))]
    if (m == 1) {index.to.sample <- index}
    index.to.sample <- setdiff(index.to.sample, d[k])
    
    edgeprob.test[j==d[k] & (i %in% index.to.sample) & t == 2, probability]})
})
out.t3 <- lapply(seq_len(length(receiver.list.t3)), function(m) {
  d <- receiver.list.t3[[m]]
  out <- sapply(seq_len(length(d)), function(k) {
    index.to.sample <- index[-(network::get.neighborhood(g[[3]], d[k], "in"))]
    if (m == 1) {index.to.sample <- index}
    index.to.sample <- setdiff(index.to.sample, d[k])
    
    edgeprob.test[j==d[k] & (i %in% index.to.sample) & t == 3, probability]})
})

mean.pb <- data.frame(time = paste0("time ", rep(1:3, each = 10)),
                      indegree = rep(0:9, times = 3),
                      mean.pb = c(unlist(lapply(1:length(out.t1), function(i) mean(out.t1[[i]], na.rm = T))),
                                  unlist(lapply(1:length(out.t2), function(i) mean(out.t2[[i]]))),
                                  unlist(lapply(1:length(out.t3), function(i) mean(out.t3[[i]])))),
                      se = c(unlist(lapply(1:length(out.t1), function(i) sd(out.t1[[i]])/sqrt(length(out.t1[[i]])))),
                             unlist(lapply(1:length(out.t2), function(i) sd(out.t2[[i]])/sqrt(length(out.t2[[i]])))),
                             unlist(lapply(1:length(out.t3), function(i) sd(out.t3[[i]])/sqrt(length(out.t3[[i]])))))
)

ggplot(mean.pb, aes(x = indegree, y = mean.pb, fill = time)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = mean.pb - se, ymax = mean.pb + se),
                width = .1, position = position_dodge(.9)) + 
  xlab("Indegree") + ylab("P(at least one additional tie)") +
  theme_bw() + scale_fill_manual(values = c("grey80", "grey", "grey40")) + 
  scale_x_continuous(breaks = 0:9, labels = paste0("(", 0:9, ")"))


## ---------------------------- ##
## Additional interaction model ##
## ---------------------------- ##

source("dev/interaction test 20170721.R")
# load("R_results/final.model4 (interaction).Rdata")

texreg::htmlreg(list(model1, model2, final.model, final.model4), digits = 3, single.row = T,
              custom.model.names = c("Control only", "Control + Structural", "Final Model", "Interactions"),
              custom.coef.names = c("Edges (Intercept)", "Age (in-ties)", "Age (out-ties)",
                                    "Female (in-ties)", "Female (out-ties)", "Gender homophily",
                                    "Education (in-ties)", "Education (out-ties)",
                                    "Regional origin = Seoul (in-ties)",
                                    "Regional origin = Seoul (out-ties)",
                                    "Regional homophily (Seoul)",
                                    "Talk freq (in-ties)", "Talk freq (out-ties)", 
                                    "Media use (in-ties)", "Media use (out-ties)",
                                    "Internal efficacy", 
                                    
                                    "Isolates", "Reciprocity", "Previous communication",
                                    "Multiple two-paths (GWDSP, 1)", 
                                    "Delayed reciprocity", 
                                    "Delayed transitivity closure", "Delayed cyclic closure", 
                                    "Delayed activity closure", "Delayed popularity closure",
                                    "Persistent sender (out-tie)", "Persistent receiver (in-ties)",
                                    "Multiple path closure (GWESP-OTP, 3)", "Multiple cyclic closure (GWESP-ITP, 3)", 
                                    "Multiple activity closure (GWESP-OSP, 3)", "Multiple popularity closure (GWESP-ISP, 2)",
                                    "Activity spread (GW-outdegree, 2)", "Popularity spread (GW-indegree, 3)",
                                    
                                    "Consistency motivation (in-ties)", "Consistency motivation (out-ties)",
                                    "Understanding motivation (in-ties)", "Understanding motivation (out-ties)", 
                                    "Hedonic motivation (in-ties)", "Hedonic motivation (out-ties)", 
                                    "Candidate pref = Moon (in-ties)", "Candidate pref = Moon (out-ties)", "Same candidate pref", 
                                    "Similar policy pref", "Similar evaluative criteria",
                                    
                                    "Transitive closure X alter more interested", 
                                    "Multiple activity closure X same candidate pref"),
              custom.note = " * 0 outside the 95% confidence interval based on 1000 replications", 
              reorder.coef = c(1, 34:46, 17:18,20,28:33, 19,21:27, 2:16),
              groups = list("Motivation and Homophily" = 2:12,
                            "Strcture X dyadic interaction" = 13:14,
                            "Endogenous structural effects" = 15:23,
                            "Lagged structural effects" = 24:31,
                            "Controls" = 32:46),
              bold = 0.5, doctype = T, html.tag = T, body.tag = T, indentation = "  ",
              caption = "",
              file = "Table 1.July20.doc")


## -------------------------------------------- ##
## Comparion with MRQAP and non-threshold model ##
## -------------------------------------------- ##

# source("dev/btergm (2) analysis no threshold.R")

load("R_results/MRQAP.model.2017.07.18.Rdata")
load("R_results/final.model.nothreshold.July20.Rdata")

print(MRQAP.model)
summary(final.model.nothreshold)

tr.qap <- extract.netlm(MRQAP.model)

tr.qap@coef.names <- c("edges", 
                       "nodeicov.consistency.motivation",
                       "nodeocov.consistency.motivation",
                       "nodeicov.understanding.motivation",
                       "nodeocov.understanding.motivation",
                       "nodeicov.hedomic.motivation",
                       "nodeocov.hedomic.motivation",
                       "nodeicov.candidate.preference",
                       "nodeocov.candidate.preference",
                       "nodematch.candidate.preference",
                       "edgecov.policy.pref.sim[[i]]",
                       "edgecov.evaludative.criteria.sim[[i]]",
                       "nodeicov.age",
                       "nodeocov.age",
                       "nodeifactor.gender.1",
                       "nodeofactor.gender.1",
                       "nodematch.gender",
                       "nodeicov.edu",
                       "nodeocov.edu",
                       "nodeicov.talk.freq",
                       "nodeocov.talk.freq",
                       "nodeicov.media.use.freq",
                       "nodeocov.media.use.freq",
                       "nodecov.internal.efficacy",
                       "nodeifactor.region_origin2.1",
                       "nodeofactor.region_origin2.1",
                       "nodematch.region_origin2",
                       "edgecov.g_autoregression[[i]]",
                       "edgecov.g_delrecip[[i]]",
                       "mutual")


screenreg(list(tr.qap, final.model, final.model.nothreshold), digits = 3, 
          leading.zero = F, single.row = T,
          custom.model.names = c("MRQAP Model", "BTERG Model", "BTERGM non-threshold"),
          custom.coef.names = c("Edges (Intercept)",
                                "Consistency motivation (in-ties)", "Consistency motivation (out-ties)",
                                "Understanding motivation (in-ties)", "Understanding motivation (out-ties)", 
                                "Hedonic motivation (in-ties)", "Hedonic motivation (out-ties)", 
                                "Candidate pref = Moon (in-ties)", "Candidate pref = Moon (out-ties)", "Same candidate pref", 
                                "Similar policy pref", "Similar evaluative criteria",
                                "Age (in-ties)", "Age (out-ties)",
                                "Female (in-ties)", "Female (out-ties)", "Gender homophily",
                                "Education (in-ties)", "Education (out-ties)",
                                "Talk freq (in-ties)", "Talk freq (out-ties)", 
                                "Media use (in-ties)", "Media use (out-ties)",
                                "Internal efficacy", 
                                "Regional origin = Seoul (in-ties)",
                                "Regional origin = Seoul (out-ties)",
                                "Regional homophily (Seoul)",
                                "Previous communication", "Delayed reciprocity", 
                                "Reciprocity", 
                                "Isolates",
                                "Multiple two-paths (GWDSP, 1 / 0.7)", 
                                "Delayed transitivity closure", "Delayed cyclic closure", 
                                "Delayed activity closure", "Delayed popularity closure",
                                "Persistent sender (out-tie)", "Persistent receiver (in-ties)",
                                "Multiple path closure (GWESP-OTP, 3)", "Multiple cyclic closure (GWESP-ITP, 3)", 
                                "Multiple activity closure (GWESP-OSP, 3)", "Multiple popularity closure (GWESP-ISP, 2)",
                                "Activity spread (GW-outdegree, 2 / 3.5)", "Popularity spread (GW-indegree, 3 / 1)",
                                "Multiple two-paths (GWDSP, 1 / 0.7)"),
          custom.note = " * 0 outside the 95% confidence interval based on 1000 replications", 
          reorder.coef = c(1:12, 30:32,39:44, 28:29,33:38, 13:27),
          groups = list("Motivation and Homophily" = 2:12,
                        "Endogenous structural effects" = 13:21,
                        "Lagged structural effects" = 22:29,
                        "Controls" = 30:44))
          


