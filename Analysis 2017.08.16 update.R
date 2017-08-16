
## script for paper entitled as 
## "Effects of motivation, homophily, and endogenous network process on message exposure within online discussion forum"

list.of.packages <- c("car", "psych","ergm","btergm","texreg","rstudioapi","data.table", "haven")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = T)

## automatically setting working directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

## ----------------- ##
## Data descriptives ##
## ----------------- ##

require(psych)
## motivation variables
## consistency motivations
## alpha 
alpha(dat[vids, .(as.numeric(pv18), as.numeric(pv19), as.numeric(pv20),
                  as.numeric(pv21), as.numeric(pv23), as.numeric(pv24))])
## desciptives
dat[vids, .(rowMeans(.SD)), .SDcols = c("pv18", "pv19", "pv20", "pv21", "pv23", "pv24")][, 
          .(mean = mean(V1), sd = sd(V1), min = min(V1), max = max(V1))]

## understanding motivations
alpha(dat[vids, .(as.numeric(pv13), as.numeric(pv14), as.numeric(pv15), as.numeric(pv16))])
dat[vids, .(rowMeans(.SD)), .SDcols = c("pv13", "pv14", "pv15", "pv16")][,
  .(mean = mean(V1), sd = sd(V1), min = min(V1), max = max(V1))]

## hedonic motivations
alpha(dat[vids, .(as.numeric(pv27), as.numeric(pv28), as.numeric(pv29))])
dat[vids, .(rowMeans(.SD)), .SDcols = c("pv27", "pv28", "pv29")][,
   .(mean = mean(V1), sd = sd(V1), min = min(V1), max = max(V1))]


## preference homophily
## candidate choice
dat[vids, .(mean = mean(canpref1), sd = sd(canpref1))]
dat[vids, .(mean = mean(canpref2), sd = sd(canpref2))]
dat[vids, .(mean = mean(canpref3), sd = sd(canpref3))]

## ideological policy preference
## see line 177 to 196, and line 223 to 228 in "dev/btergm (1) data prep.R" for detailed coding
sapply(policy.pref.sim, function(x) {print(c(mean = mean(x), sd = sd(x)))})

## candidate evaluative criteria
## see line 203 to 218, and line 223 to 228 in "dev/btergm (1) data prep.R" for detailed coding
sapply(evaludative.criteria.sim, function(x) {print(c(mean = mean(x), sd = sd(x)))})


## control variables
dat[vids, .(gender = as.numeric(sex) - 1)][, .(mean = mean(gender))] ## 1 = female, 0 = male
dat[vids, .(age10 = as.numeric(age)/10)][, .(mean = mean(age10), sd = sd(age10))] ## age in ten years
dat[vids, .(edu = as.numeric(edu))][, .(mean = mean(edu), sd = sd(edu))] ## education level (1 = "less than elemantry" vs. 9 = "more than postgraduate")

## region of origin
## 1 = Seoul, 2 = Busan, Ulsan & Kungnam, 3 = Tague and Kungbuk
## 4 = Inchun, kunggi & Kangwon, 
## 5 = Kwangju & Junnam/buck, 6 = Daejun & Chungnam/buk
## 7 = Jeuju
dat[vids, region2 := recode(as.numeric(region1), 
                                      "1 = 1; 
                                       2 = 2; 14 = 2; 7 = 2;
                                       3 = 3; 13 = 3;
                                       4 = 4; 8 = 4;
                                       5 = 5; 11:12 = 5; 
                                       6 = 6; 9:10 = 6;
                                       15 = 7")][, table(region2)] 
## region of origin seoul vs all other
dat[vids, .(recode(as.numeric(region2), "1 = 1; else = 0"))][, mean(V1)] 

## offline talk frequency
dat[vids, .(as.numeric(pv322))][, .(mean = mean(V1), sd = sd(V1))]
dat[vids, .(as.numeric(kv217))][, .(mean = mean(V1), sd = sd(V1))]
dat[vids, .(as.numeric(hv276))][, .(mean = mean(V1), sd = sd(V1))]

## media use freqeuncy
## remove "NaN" in data
dat[is.na(pv311), pv311 := 0 ]
dat[is.na(pv313), pv313 := 0 ]
dat[is.na(pv317), pv317 := 0 ]
dat[is.na(kv194), kv194 := 0 ]
dat[is.na(kv196), kv196 := 0 ]
dat[is.na(kv200), kv200 := 0 ]
dat[is.na(hv253), hv253 := 0 ]
dat[is.na(hv255), hv255 := 0 ]
dat[is.na(hv259), hv259 := 0 ]

## add with hours, and creaet index
## W1
dat[vids, .(internet.news.use = (60*pv310 + pv311)/60,
            newspaper.use = (60*pv312 + pv313)/60, 
            tv.news.use = (60*pv316 + pv317)/60)][, 
    .(media.freq = rowMeans(.SD)), .SDcol = c("internet.news.use", "newspaper.use", "tv.news.use")][,
    .(mean = mean(media.freq), sd = sd(media.freq))]

# W2
dat[vids, .(internet.news.use = (60*kv193 + kv194)/60,
                         newspaper.use = (60*kv195 + kv196)/60, 
                         tv.news.use = (60*kv199 + kv200)/60)][, 
    .(media.freq = rowMeans(.SD)), .SDcol = c("internet.news.use", "newspaper.use", "tv.news.use")][,
    .(mean = mean(media.freq), sd = sd(media.freq))]

# W3
dat[vids, .(internet.news.use = (60*hv252 + hv253)/60,
                         newspaper.use = (60*hv254 + hv255)/60, 
                         tv.news.use = (60*hv258 + hv259)/60)][, 
    .(media.freq = rowMeans(.SD)), .SDcol = c("internet.news.use", "newspaper.use", "tv.news.use")][,
    .(mean = mean(media.freq), sd = sd(media.freq))]

## internal discussion efficacy
dat[vids, pv126:pv129][, rowMeans(.SD), by = vids][,.(mean = mean(V1), sd = sd(V1))]


## --------------------------- ##
## Bootstrapped TERGM analysis ##
## ----------------------------##

require(btergm)

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
                   nodeicov("internal.efficacy") + nodeocov("internal.efficacy") +
                   nodeifactor("candidate.preference") + nodeofactor("candidate.preference") + 
                   
                   offset(edgecov(offsmat)), ## hedonic 
                 
                 offset = T, verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(model1)

save(model1, file = "R_results/Aug02.model1.Rdata")

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
                                  gwidegree(decay = 3, fixed = T) + 
                                  
                                  offset(edgecov(offsmat))), 
                                
                                offset = T, verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(model2)

save(model2, file = "R_results/Aug02.model2.Rdata")

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
                   dgwesp(decay = 3, fixed = T, type = "OSP") + 
                   dgwesp(decay = 2, fixed = T, type = "ISP") + 
                   
                   gwodegree(decay = 2, fixed = T) + 
                   gwidegree(decay = 3, fixed = T) + 
                  
                   offset(edgecov(offsmat)), ## hedonic 
                 
                   offset = T, verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(final.model)

save(final.model, file = "R_results/Aug02.final.model.Rdata")

## here we estimate a goodness of fit for this final model.
final.model.gof <- gof(final.model, statistics = gof.statistics, 
                  nsim = 300, verbose = TRUE, ncpus = gof_ncpus, parallel = parallel)
plot(final.model.gof, mfrow = FALSE, xlim = 40)
# save(final.model.gof, file = "Aug02.final.model.gof.Rdata")

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

save(model1, model2, final.model, file = "R_results/btergm.results.Aug 2nd.Rdata")
save(final.model.gof, file = "R_results/btergm.gof.results.Aug 2nd.Rdata")

# ===========================================================================================================================================
#                                                       Control only                Control + Structural               Final Model                  
# -------------------------------------------------------------------------------------------------------------------------------------------
# Edges (Intercept)                                 -4.977 [-6.749; -3.726] *     -1.127 [-2.206;   .463]        -1.890 [-2.932;  -.304] *
# Motivation and Homophily                                                                                                                   
#  Consistency motivation (in-ties)                                                                                .034 [ -.021;   .113]  
#  Consistency motivation (out-ties)                                                                               .025 [ -.112;   .077]  
#  Understanding motivation (in-ties)                                                                             -.052 [ -.103;   .022]  
#  Understanding motivation (out-ties)                                                                             .028 [  .005;   .087] *
#  Hedonic motivation (in-ties)                                                                                   -.012 [ -.038;   .001]  
#  Hedonic motivation (out-ties)                                                                                   .102 [  .087;   .133] *
#  Same candidate pref                                                                                            -.032 [ -.079;   .047]  
#  Similar policy pref                                                                                            -.108 [ -.212;   .042]  
#  Similar evaluative criteria                                                                                     .407 [  .207;   .415] *
# Endogenous structural effects                                                                                                              
#  Isolates                                                                        1.021 [  .797;  1.256] *       1.019 [  .793;  1.264] *
#  Reciprocity                                                                      .765 [  .497;  1.062] *        .769 [  .507;  1.068] *
#  Multiple path closure (GWESP-OTP, 3)                                             .058 [ -.056;   .124]          .058 [ -.053;   .125]  
#  Multiple cyclic closure (GWESP-ITP, 3)                                          -.068 [ -.082;  -.060] *       -.066 [ -.080;  -.060] *
#  Multiple activity closure (GWESP-OSP, 3)                                         .035 [  .030;   .053] *        .036 [  .033;   .053] *
#  Multiple popularity closure (GWESP-ISP, 2)                                       .117 [  .082;   .237] *        .115 [  .082;   .232] *
#  Multiple two-paths (GWDSP, 1)                                                    .003 [ -.007;   .009]          .003 [ -.007;   .009]  
#  Activity spread (GW-outdegree, 2)                                              -4.399 [-4.669; -4.083] *      -4.350 [-4.557; -3.994] *
#  Popularity spread (GW-indegree, 3)                                             -4.056 [-5.343; -3.318] *      -4.049 [-5.342; -3.259] *
# Lagged structural effects                                                                                                                  
#  Previous communication                                                           .214 [  .182;   .255] *        .222 [  .192;   .253] *
#  Delayed reciprocity                                                              .082 [ -.067;   .352]          .074 [ -.073;   .344]  
#  Delayed transitivity closure                                                     .034 [  .018;   .057] *        .034 [  .020;   .055] *
#  Delayed cyclic closure                                                           .037 [  .010;   .057] *        .034 [  .008;   .057] *
#  Delayed activity closure                                                        -.058 [ -.068;  -.035] *       -.056 [ -.067;  -.035] *
#  Delayed popularity closure                                                      -.060 [ -.109;  -.036] *       -.059 [ -.110;  -.034] *
#  Persistent sender (out-tie)                                                      .019 [  .009;   .029] *        .019 [  .010;   .029] *
#  Persistent receiver (in-ties)                                                    .023 [  .019;   .037] *        .023 [  .018;   .038] *
# Controls                                                                                                                                   
#  Age (in-ties)                                     .101 [ -.012;   .173]          .003 [ -.017;   .036]          .001 [ -.020;   .035]  
#  Age (out-ties)                                    .218 [ -.097;   .382]          .031 [ -.224;   .078]          .052 [ -.192;   .093]  
#  Female (in-ties)                                 -.204 [ -.310;  -.146] *       -.001 [ -.046;   .056]          .005 [ -.036;   .071]  
#  Female (out-ties)                                -.169 [ -.446;  -.112] *        .075 [ -.308;   .428]          .014 [ -.348;   .335]  
#  Gender homophily                                  .010 [ -.032;   .037]          .051 [  .018;   .094] *        .044 [  .015;   .086] *
#  Education (in-ties)                              -.114 [ -.182;  -.076] *       -.008 [ -.042;   .020]         -.011 [ -.039;   .019]  
#  Education (out-ties)                             -.132 [ -.239;   .036]          .028 [ -.010;   .108]          .016 [ -.015;   .091]  
#  Regional origin = Seoul (in-ties)                -.418 [ -.501;  -.297] *       -.077 [ -.124;   .048]         -.084 [ -.157;   .044]  
#  Regional origin = Seoul (out-ties)               -.192 [ -.383;  -.021] *       -.143 [ -.635;   .315]         -.125 [ -.598;   .350]  
#  Regional homophily (Seoul)                       -.021 [ -.053;   .029]          .013 [ -.020;   .078]          .017 [ -.014;   .080]  
#  Talk freq (in-ties)                               .129 [ -.120;   .276]          .045 [  .004;   .050] *        .046 [  .002;   .049] *
#  Talk freq (out-ties)                              .025 [ -.428;   .385]          .034 [ -.173;   .186]          .014 [ -.143;   .161]  
#  Media use (in-ties)                              -.061 [ -.108;   .522]         -.011 [ -.021;   .016]         -.011 [ -.019;   .024]  
#  Media use (out-ties)                             -.070 [ -.110;   .648]          .040 [ -.003;   .285]          .033 [ -.017;   .287]  
#  Internal efficacy (in-ties)                       .051 [ -.045;   .135]         -.013 [ -.040;   .048]         -.013 [ -.058;   .055]  
#  Internal efficacy (out-ties)                      .187 [  .132;   .253] *       -.018 [ -.136;   .099]          .024 [ -.102;   .128]  
#  Candidate pref = Moon (in-ties)                   .174 [  .057;   .288] *       -.018 [ -.063;   .049]          .003 [ -.008;   .092]  
#  Candidate pref = Moon (out-ties)                  .315 [  .204;   .520] *       -.010 [ -.100;   .172]          .013 [ -.123;   .131]  
# -------------------------------------------------------------------------------------------------------------------------------------------
#   Num. obs.                                       291096                         291096                         291096                       
# ===========================================================================================================================================
#   * 0 outside the 95% confidence interval based on 1000 replications
  
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
                                        "Internal efficacy (in-ties)", "Internal efficacy (out-ties)",
                                        "Candidate pref = Moon (in-ties)", "Candidate pref = Moon (out-ties)",
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
                                        "Same candidate pref", "Similar policy pref", "Similar evaluative criteria"),
                  custom.note = " * 0 outside the 95% confidence interval based on 1000 replications", 
                  reorder.coef = c(1, 37:45, 20:21,31:34,23,35:36, 22,24:30, 2:19),
                  groups = list("Motivation and Homophily" = 2:10, 
                                "Endogenous structural effects" = 11:19,
                                "Lagged structural effects" = 20:27,
                                "Controls" = 28:45),
                  bold = 0.5, doctype = T, html.tag = T, body.tag = T, indentation = "  ",
                  caption = "",
                file = "results.Table.Aug02.doc")


## --------------------------- ##
## produce coefficient-CI plot ##
## --------------------------- ##


ggdat_fig <- data.table(
  var = names(final.model@coef),
  coef = coef(final.model),
  q025 = apply(final.model@boot$t, 2, quantile, .025),
  q975 = apply(final.model@boot$t, 2, quantile, .975))

ggdat_fig <- ggdat_fig[c(2:28, 31,33:39, 1,29:30,32,40:45)]

y <- c("Age in 10 years \n(in-ties)", "Age in 10 years \n(out-ties)", 
       "Gender: female \n(in-ties)", "Gender: female \n(out-ties)", "Gender \nhomophily", 
       "Education \n(in-ties)", "Education \n(out-ties)", 
       "Origin = Seoul \n(in-ties)", "Origin = Seoul \n(out-ties)", "Origin = Seoul \nhomophily", 
       "Offline talk freq \n(in-ties)", "Offline talk freq \n(out-ties)", 
       "Media use freq \n(in-ties)", "Media use freq \n(out-ties)", 
       "Internal efficacy \n(in-ties)", "Internal efficacy \n(out-ties)",
       "Candidate preference \n(Moon, in-tie)", "Candidate preference \n(Moon, out-tie)",
       "Motivation \nconsistency (in-tie)", "Motivation \nconsistency (out-tie)", 
       "Motivation \nunderstanding (in-tie)", "Motivation \nunderstanding (out-tie)", 
       "Motivation \nhedonic (in-tie)", "Motivation \nhedonic (out-tie)",
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
p1 <- ggplot(ggdat_fig[1:18,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point(size = 2.5) + geom_errorbarh(height = 0, size = 1.5) +
  xlab("Coefficient") + ylab("") + ggtitle("Demographics and controls") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(values = c("grey", "red"))

## discussion motivation and homophily effects
p2 <- ggplot(ggdat_fig[19:27,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point(size = 2.5) + geom_errorbarh(height = 0, size = 1.5) +
  xlab("Coefficient") + ylab("") + ggtitle("Motivation and homophily") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +  
  scale_colour_manual(values = c("grey", "red"))

## pure lagged structural effects
p3 <- ggplot(ggdat_fig[28:35,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
  geom_point(size = 2.5) + geom_errorbarh(height = 0, size = 1.5) +
  xlab("Coefficient") + ylab("") + ggtitle("Lagged structural effects") +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) +
  theme_bw() + theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(values = c("grey", "red")) 

## pure concurrent structural effects
p4 <- ggplot(ggdat_fig[36:45,], aes(x = coef, y = y, xmin = q025, xmax = q975, color = sig)) +
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

source("interaction test 2017.08.02.R")
summary(final.model4)

# save(final.model4, file = "R_results/final.model.interaction.(nonsig).Rdata")

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


htmlreg(list(tr.qap, final.model, final.model.nothreshold), digits = 3, 
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
                        "Controls" = 30:44),
        bold = 0.5, doctype = T, html.tag = T, body.tag = T, indentation = "  ",
        caption = "",
        file = "Results.table.2.doc")
          


