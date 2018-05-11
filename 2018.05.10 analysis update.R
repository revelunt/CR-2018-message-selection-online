
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

rm(list = ls())
source("dev/btergm helper-functions.R")
source("dev/btergm (1) data prep.R")

RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

## ------------------------- ##
## Quantities reported in ms ##
## ------------------------- ##

net <- read.csv("Dat/Reading_1113-1126_Participants(N=341)_Count(N=160836).csv")
net2 <- read.csv("Dat/Reading_1127-1219_Participants(N=341)_Count(N=160836).csv")
net2 <- data.frame(reading.time = net2$Reading.Time, reader.id = net2$Reader.Id, poster.id = net2$Poster.Id)
net <- rbind(net, net2)
setDT(net)
net[, reading.date := as.Date(reading.time, format = "%Y-%m-%d %H:%M:%S")]

## study period (27 days)
net[, length(unique(reading.date))]

## total sample (341)
dat[, .N]

## total eligible sample (312)
dat[vids, .N]

## mean number of posts made by respondents
dat[vids, mean(쓰기_sum, na.rm = T)]

## mean number of posts being read by respondents
net[, count := 1]
net[reader.id %in% vids & poster.id %in% vids, 
    sum(count), by = reader.id][, mean(V1)]

## mean number of unique post read by respondents
unique(net[reader.id %in% vids & poster.id %in% vids, ], 
       by = c("reader.id", "poster.id"))[, 
                                         sum(count), by = reader.id][, mean(V1)]

## footnote 2
dat[vids, median(age)]
dat[vids, mean(sex - 1)]

## footnote 4
## number of those who posts anything at all
table(vids %in% net[, unique(poster.id)])

## number of those who reads anything at all
table(vids %in% net[, unique(reader.id)])

## median of reading count divided by 27 days
net[(reader.id %in% vids) & (poster.id %in% vids), 
    sum(count), by = reader.id][, median(V1)/27]

## median of posting count divided by 27 days
dat[vids, median(쓰기_sum, na.rm = T)]/27

## unique number of users per day
net[reader.id %in% vids & poster.id %in% vids, 
    length(unique(c(unique(reader.id), unique(poster.id)))), 
    by = reading.date][, mean(V1)]

rm(net, net2)

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
sapply(g, function(x) {
  temp <- ergmMPLE(x ~ nodematch("candidate.preference"), output = "array")$predictor[,,1]
  dimnames(temp) <- NULL
  diag(temp) <- 0
  print(c(mean = mean(temp), sd = sd(temp)))
})

## cf. candidate choice
dat[vids, .(mean = mean(canpref1), sd = sd(canpref1))]
dat[vids, .(mean = mean(canpref2), sd = sd(canpref2))]
dat[vids, .(mean = mean(canpref3), sd = sd(canpref3))]

## ideological self-placement 
sapply(g, function(x) {
  temp <- ergmMPLE(x ~ absdiff("ideology"), output = "array")$predictor[,,1]
  dimnames(temp) <- NULL
  diag(temp) <- 0
  print(c(mean = mean(temp), sd = sd(temp)))
})

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
parallel <- "snow"
ncpus <- parallel::detectCores(logical = F)
gof_ncpus <- 4


## first, start with control only model
## set custom seed for reproducibility
set.seed(12345, kind = "L'Ecuyer-CMRG")
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
                   nodeifactor("candidate.preference") + nodeofactor("candidate.preference"), 
                 
                   verbose = T, R = R, ncpus = ncpus, parallel = parallel)

summary.1 <- summary(model1, type = "bca")

## next, we add pure structural effects
model2 <- btergm(g ~ edges + ## intercept
                   
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
                    gwidegree(decay = 3, fixed = T), 
                                
                    verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(model2)

summary.2 <- summary(model2, type = "bca")

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
                   gwidegree(decay = 3, fixed = T), ## hedonic 
                 
                   verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(final.model)

summary.final <- summary(final.model, type = "bca")

## estimate a goodness of fit for this final model.
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

## final model II, ideology homophily instead of candidate / policy preference
set.seed(12345, kind = "L'Ecuyer-CMRG")
final.model2 <- btergm(g ~ edges + ## intercept
                        
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
                        absdiff("ideology") + 
                        
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
                        gwidegree(decay = 3, fixed = T), ## hedonic 
                      
                      verbose = T, R = R, ncpus = ncpus, parallel = parallel); summary(final.model2)

summary.final2 <- summary(final.model2, type = "bca")


screenreg(list(model1, model2, final.model, final.model2), digits = 3, leading.zero = F, single.row = T, 
          override.ci.low = list(summary.1[,2], summary.2[,2], summary.final[,2], summary.final2[,2]), 
          override.ci.up = list(summary.1[,2], summary.2[,2], summary.final[, 3], summary.final2[,3]))

## all models to a file
texreg::htmlreg(list(model1, model2, final.model, final.model2), digits = 3, leading.zero = F, single.row = T,
                override.ci.low = list(summary.1[,2], summary.2[,2], summary.final[,2], summary.final2[,2]), 
                override.ci.up = list(summary.1[,2], summary.2[,2], summary.final[, 3], summary.final2[,3]),
                  custom.model.names = c("Control only", "Control + Structural", "Final Model I", "Final Model II"),
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
                                        "Same candidate pref", "Similar policy pref", "Similar evaluative criteria", "Similar ideology"),
                  custom.note = " * = zero outside the 95% bias-corrected and accelerated confidence interval based on 1000 replications", 
                  reorder.coef = c(1, 37:46, 20:21,31:34,23,35:36, 22,24:30, 2:19),
                  groups = list("Motivation and Homophily" = 2:11, 
                                "Endogenous structural effects" = 12:20,
                                "Lagged structural effects" = 21:28,
                                "Controls" = 29:46),
                  bold = 0.5, doctype = T, html.tag = T, body.tag = T, indentation = "  ",
                  caption = "",
                file = "results.Table.May10.doc")

# ==========================================================================================================================================================================
#                                                           Control only                  Control + Structural           Final Model I                  Final Model II               
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   Edges (Intercept)                                   -4.977 [-6.749; -6.749] *      -1.127 [-2.206; -2.206] *      -1.890 [-2.932; -1.392] *      -2.259 [-2.958; -1.732] *
# Motivation and Homophily                                                                                                                                                  
#   Consistency motivation (in-ties)                                                                                .034 [  .009;   .113] *        .062 [  .047;   .144] *
#   Consistency motivation (out-ties)                                                                               .025 [ -.044;   .077]          .046 [  .007;   .046] *
#   Understanding motivation (in-ties)                                                                             -.052 [ -.080;   .022]         -.078 [ -.118;  -.011] *
#   Understanding motivation (out-ties)                                                                             .028 [  .005;   .076] *        .056 [  .047;   .092] *
#   Hedonic motivation (in-ties)                                                                                   -.012 [ -.029;   .001]         -.026 [ -.039;  -.021] *
#   Hedonic motivation (out-ties)                                                                                   .102 [  .087;   .133] *        .043 [ -.009;   .114]  
#   Same candidate pref                                                                                            -.032 [ -.070;   .047]                                 
#   Similar policy pref                                                                                            -.108 [ -.212;   .006]                                 
#   Similar evaluative criteria                                                                                     .407 [  .399;   .415] *        .482 [  .398;   .482] *
#   Similar ideology                                                                                                                               .024 [ -.007;   .040]  
# Endogenous structural effects                                                                                                                                             
#   Isolates                                                                        1.021 [  .797;   .797] *       1.019 [  .908;  1.264] *        .825 [  .598;  1.224] *
#   Reciprocity                                                                      .765 [  .497;   .497] *        .769 [  .564;  1.068] *        .720 [  .504;  1.069] *
#   Multiple path closure (GWESP-OTP, 3)                                             .058 [ -.056;  -.056] *        .058 [ -.053;   .125]          .040 [ -.078;   .067]  
#   Multiple cyclic closure (GWESP-ITP, 3)                                          -.068 [ -.082;  -.082] *       -.066 [ -.080;  -.060] *       -.058 [ -.077;  -.047] *
#   Multiple activity closure (GWESP-OSP, 3)                                         .035 [  .030;   .030] *        .036 [  .033;   .045] *        .027 [  .026;   .033] *
#   Multiple popularity closure (GWESP-ISP, 2)                                       .117 [  .083;   .083] *        .115 [  .093;   .232] *        .129 [  .099;   .286] *
#   Multiple two-paths (GWDSP, 1)                                                    .003 [ -.005;  -.005] *        .003 [ -.007;   .007]          .008 [  .002;   .016] *
#   Activity spread (GW-outdegree, 2)                                              -4.399 [-4.669; -4.669] *      -4.350 [-4.557; -4.157] *      -3.923 [-4.124; -3.623] *
#   Popularity spread (GW-indegree, 3)                                             -4.056 [-5.343; -5.343] *      -4.049 [-5.342; -3.259] *      -4.828 [-5.429; -3.889] *
# Lagged structural effects                                                                                                                                                 
#   Previous communication                                                           .214 [  .182;   .182] *        .222 [  .192;   .253] *        .531 [  .494;   .549] *
#   Delayed reciprocity                                                              .082 [ -.067;  -.067] *        .074 [ -.073;   .194]          .002 [ -.197;   .139]  
#   Delayed transitivity closure                                                     .034 [  .018;   .018] *        .034 [  .020;   .055] *        .032 [  .026;   .037] *
#   Delayed cyclic closure                                                           .037 [  .010;   .010] *        .034 [  .008;   .057] *        .032 [  .003;   .035] *
#   Delayed activity closure                                                        -.058 [ -.068;  -.068] *       -.056 [ -.067;  -.046] *       -.061 [ -.071;  -.037] *
#   Delayed popularity closure                                                      -.060 [ -.089;  -.089] *       -.059 [ -.110;  -.043] *       -.062 [ -.124;  -.046] *
#   Persistent sender (out-tie)                                                      .019 [  .009;   .009] *        .019 [  .010;   .029] *        .510 [  .176;   .724] *
#   Persistent receiver (in-ties)                                                    .023 [  .019;   .019] *        .023 [  .018;   .038] *        .116 [  .020;   .165] *
# Controls                                                                                                                                                                  
#   Age (in-ties)                                     .101 [ -.012;  -.012] *        .003 [ -.017;  -.017] *        .001 [ -.020;   .022]         -.025 [ -.033;  -.006] *
#   Age (out-ties)                                    .218 [ -.097;  -.097] *        .031 [ -.224;  -.224] *        .052 [ -.105;   .093]          .101 [ -.046;   .141]  
#   Female (in-ties)                                 -.204 [ -.245;  -.245] *       -.001 [ -.038;  -.038] *        .005 [ -.036;   .041]         -.023 [ -.057;   .030]  
#   Female (out-ties)                                -.169 [ -.446;  -.446] *        .075 [ -.308;  -.308] *        .014 [ -.348;   .254]          .151 [ -.266;   .389]  
#   Gender homophily                                  .010 [ -.032;  -.032] *        .051 [  .018;   .018] *        .044 [  .023;   .086] *        .045 [  .021;   .100] *
#   Education (in-ties)                              -.114 [ -.182;  -.182] *       -.008 [ -.042;  -.042] *       -.011 [ -.039;   .019]         -.015 [ -.047;   .014]  
#   Education (out-ties)                             -.132 [ -.239;  -.239] *        .028 [ -.010;  -.010] *        .016 [ -.015;   .091]         -.044 [ -.096;   .029]  
#   Regional origin = Seoul (in-ties)                -.418 [ -.501;  -.501] *       -.077 [ -.116;  -.116] *       -.084 [ -.130;   .044]         -.109 [ -.140;  -.030] *
#   Regional origin = Seoul (out-ties)               -.192 [ -.383;  -.383] *       -.143 [ -.635;  -.635] *       -.125 [ -.438;   .350]         -.135 [ -.435;   .324]  
#   Regional homophily (Seoul)                       -.021 [ -.047;  -.047] *        .013 [ -.020;  -.020] *        .017 [ -.014;   .080]          .025 [ -.002;   .049]  
#   Talk freq (in-ties)                               .129 [ -.120;  -.120] *        .045 [  .021;   .021] *        .046 [  .024;   .049] *        .051 [  .048;   .051] *
#   Talk freq (out-ties)                              .025 [ -.428;  -.428] *        .034 [ -.173;  -.173] *        .014 [ -.099;   .161]          .010 [ -.111;   .206]  
#   Media use (in-ties)                              -.061 [ -.108;  -.108] *       -.011 [ -.021;  -.021] *       -.011 [ -.019;  -.003] *       -.006 [ -.012;   .004]  
#   Media use (out-ties)                             -.070 [ -.104;  -.104] *        .040 [  .004;   .004] *        .033 [ -.017;   .071]          .043 [ -.006;   .078]  
#   Internal efficacy (in-ties)                       .051 [ -.045;  -.045] *       -.013 [ -.040;  -.040] *       -.013 [ -.058;   .055]         -.030 [ -.072;  -.006] *
#   Internal efficacy (out-ties)                      .187 [  .132;   .132] *       -.018 [ -.098;  -.098] *        .024 [ -.102;   .128]          .115 [  .051;   .163] *
#   Candidate pref = Moon (in-ties)                   .174 [  .057;   .057] *       -.018 [ -.063;  -.063] *        .003 [ -.008;   .092]         -.003 [ -.056;   .085]  
#   Candidate pref = Moon (out-ties)                  .315 [  .216;   .216] *       -.010 [ -.100;  -.100] *        .013 [ -.123;   .066]          .080 [ -.004;   .100]  
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   Num. obs.                                       291096                         291096                         291096                         291096                       
# ==========================================================================================================================================================================
#   * = zero outside the 95% bias-corrected and accelerated confidence interval based on 1000 replications


## --------------------------- ##
## produce coefficient-CI plot ##
## --------------------------- ##

ggdat_fig <- data.table(
  var = names(final.model@coef),
  coef = coef(final.model),
  q025 = summary.final[,2],
  q975 = summary.final[,3])

ggdat_fig <- ggdat_fig[c(2:28, 31,33:39, 1,29:30,32,40:45)]

y <- c("Age \n(in-ties)", "Age \n(out-ties)", 
       "Female \n(in-ties)", "Female \n(out-ties)", "Gender \nhomophily", 
       "Education \n(in-ties)", "Education \n(out-ties)", 
       "Origin = Seoul \n(in-ties)", "Origin = Seoul \n(out-ties)", "Origin = Seoul \nhomophily", 
       "Offline talk freq \n(in-ties)", "Offline talk freq \n(out-ties)", 
       "Media use freq \n(in-ties)", "Media use freq \n(out-ties)", 
       "Internal efficacy \n(in-ties)", "Internal efficacy \n(out-ties)",
       "Candidate preference \n(Moon, in-tie)", "Candidate preference \n(Moon, out-tie)",
       "Cconsistency (in-tie)", "Consistency (out-tie)", 
       "Understanding (in-tie)", "Understanding (out-tie)", 
       "Hedonic (in-tie)", "Hedonic (out-tie)",
       "Candidate choice \nhomophily", "Policy preference \nhomophily", "Evaluative criteria \nhomophily",
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
  xlab("Coefficient") + ylab("") + ggtitle("Lagged structural effects (controls)") +
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

grid.arrange(p2, p4, nrow = 1, ncol = 2)
grid.arrange(p1, p3, nrow = 1, ncol = 2)
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
degree.num <- c(0, 3, 10)
receiver.list.t1 <- lapply(degree.num, function(i) which(sna::degree(g[[1]], cmode="indegree") == i))
receiver.list.t2 <- lapply(degree.num, function(i) which(sna::degree(g[[2]], cmode="indegree") == i))
receiver.list.t3 <- lapply(degree.num, function(i) which(sna::degree(g[[3]], cmode="indegree") == i))

edgeprob.test <- btergm::edgeprob(final.model)
setDT(edgeprob.test)
index <- 1:312
# 
## clear up '[[i]]' in the data frame
for (i in 1:ncol(edgeprob.test)) {
  if (grepl("((edge)|(dyad))cov", colnames(edgeprob.test)[i])) {
    colnames(edgeprob.test)[i] <- substr(colnames(edgeprob.test)[i], 1, nchar(colnames(edgeprob.test)[i]) - 5)
  }
}
# 
# edgeprob.test[, strzero := 0]
# edgeprob.test[i == which(colnames(as.matrix(g[[1]])) %in% off1) & t == 1, strzero := 1]
# edgeprob.test[j == which(colnames(as.matrix(g[[1]])) %in% off1) & t == 1, strzero := 1]
# edgeprob.test[i == which(colnames(as.matrix(g[[2]])) %in% off2) & t == 2, strzero := 1]
# edgeprob.test[j == which(colnames(as.matrix(g[[2]])) %in% off2) & t == 2, strzero := 1]
# edgeprob.test[i == which(colnames(as.matrix(g[[3]])) %in% off3) & t == 3, strzero := 1]
# edgeprob.test[j == which(colnames(as.matrix(g[[3]])) %in% off3) & t == 3, strzero := 1]
# 
# ggplot(edgeprob.test[tie == 1, ], 
#        aes(x = edgecov.evaludative.criteria.sim, y = probability)) + theme_bw() + 
#   geom_smooth(method = "lm", fullrange = T) #+ geom_hline(yintercept = 0, linetype = "dashed", color = "red") 


jdegree1 <- degree(g[[1]], cmode = "indegree")
jdegree2 <- degree(g[[2]], cmode = "indegree")
jdegree3 <- degree(g[[3]], cmode = "indegree")
names(jdegree3) <- names(jdegree2) <- names(jdegree1) <- 1:312

edgeprob.test[, indegree := 0]

for (jn in 1:312) {
  edgeprob.test[j == jn & t == 1, indegree := jdegree1[jn]]
  edgeprob.test[j == jn & t == 2, indegree := jdegree2[jn]]
  edgeprob.test[j == jn & t == 3, indegree := jdegree3[jn]]
}

ggplot(edgeprob.test, 
  aes(x = indegree, y = probability)) + theme_bw() + 
  geom_smooth(span = 0.8, fullrange = T) #+ geom_hline(yintercept = 0, linetype = "dashed", color = "red") 
       



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

mean.pb <- data.frame(time = paste0("time ", rep(1:3, each = length(degree.num))),
                      indegree = rep(degree.num, times = 3),
                      mean.pb = c(unlist(lapply(1:length(out.t1), function(i) mean(out.t1[[i]], na.rm = T))),
                                  unlist(lapply(1:length(out.t2), function(i) mean(out.t2[[i]]))),
                                  unlist(lapply(1:length(out.t3), function(i) mean(out.t3[[i]])))),
                      se = c(unlist(lapply(1:length(out.t1), function(i) sd(out.t1[[i]])/sqrt(length(out.t1[[i]])))),
                             unlist(lapply(1:length(out.t2), function(i) sd(out.t2[[i]])/sqrt(length(out.t2[[i]])))),
                             unlist(lapply(1:length(out.t3), function(i) sd(out.t3[[i]])/sqrt(length(out.t3[[i]])))))
)

ggplot(mean.pb, aes(x = as.factor(indegree), y = mean.pb, fill = time)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = mean.pb - se, ymax = mean.pb + se),
                width = .1, position = position_dodge(.9)) + 
  xlab("Indegree") + ylab("P(at least one additional tie)") +
  theme_bw() + scale_fill_manual(values = c("grey80", "grey", "grey40")) #+
  #geom_text(aes(label=mean.pb), position = position_dodge(width=0.9), vjust=-0.25)


## ---------------------------- ##
## Additional interaction model ##
## ---------------------------- ##

## interaction effect testing with time, interactin with same_candiate_preference, evaluative criteria, and policy.pref.sim.

same_candidate_preference  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

## interaction effect testing with time, interactin with similar ideology.

similar_ideology  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ absdiff("ideology"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

time_trends <- lapply(1:3, function(i) {
  dim <- dim(as.matrix(g[[i]]))
  outmat <- matrix(i - 1, nrow = dim[1], ncol = dim[2])
  diag(outmat) <- 0
  outmat
})

time.X.same.candidate.preference <- lapply(1:3, function(i) {
  outmat <- time_trends[[i]] * same_candidate_preference[[i]]
  diag(outmat) <- 0
  outmat
})


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
                         edgecov(time_trends) + #timecov(transform = function(t) t - 1) +
                         edgecov(time.X.same.candidate.preference), # timecov(same_candidate_preference, transform = function(t) t - 1),
                       
                       R = 1000, parallel = "multicore", ncpus = 10); 

summary.int1 <- summary(final.model4, type = "bca")

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
                       
                       R = 1000, parallel = "multicore", ncpus = parallel::detectCores())

summary.int2 <- summary(final.model5, type = "bca")

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

summary.int3 <- summary(final.model6, type = "bca")

# ideology homophily model
final.model7 <- btergm(g ~ edges + ## intercept
                         
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
                         absdiff("ideology") + 
                         
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
                         timecov(similar_ideology, transform = function(t) t),
                       
                       R = 1000, parallel = "multicore", ncpus = parallel::detectCores(logical = F));

summary.int4 <- summary(final.model7, type = "bca")

## all interaction results in one file
texreg::htmlreg(list(final.model4, final.model5, final.model6, final.model7), digits = 3, leading.zero = F, single.row = T,
                override.ci.low = list(summary.int1[,2], summary.int2[,2], summary.int3[,2], summary.int4[,2]), 
                override.ci.up = list(summary.int1[,3], summary.int2[,3], summary.int3[,3], summary.int4[,3]),
                custom.model.names = c("candidate.pref.interaction", "eval.criteria.interaction", "policy.pref.interaction", "ideology.interaction"),
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
                                      "Consistency motivation (in-ties)", "Consistency motivation (out-ties)",
                                      "Understanding motivation (in-ties)", "Understanding motivation (out-ties)", 
                                      "Hedonic motivation (in-ties)", "Hedonic motivation (out-ties)", 
                                      "Same candidate pref", "Similar policy pref", "Similar evaluative criteria",
                                      "Isolates", "Reciprocity", "Previous communication",
                                      "Multiple two-paths (GWDSP, 1)", 
                                      "Delayed reciprocity", 
                                      "Delayed transitivity closure", "Delayed cyclic closure", 
                                      "Delayed activity closure", "Delayed popularity closure",
                                      "Persistent sender (out-tie)", "Persistent receiver (in-ties)",
                                      "Multiple path closure (GWESP-OTP, 3)", "Multiple cyclic closure (GWESP-ITP, 3)", 
                                      "Multiple activity closure (GWESP-OSP, 3)", "Multiple popularity closure (GWESP-ISP, 2)",
                                      "Activity spread (GW-outdegree, 2)", "Popularity spread (GW-indegree, 3)",
                                      "time trends (linear)", "time X same.canddiate.pref", 
                                      "time X evaluative criteria similarity", "time X policy pref similarity", 
                                      "Similar ideology","time X Similar ideology"),
                custom.note = " * = zero outside the 95% bias-corrected and accelerated confidence interval based on 1000 replications", 
                
                bold = 0.5, doctype = T, html.tag = T, body.tag = T, indentation = "  ",
                caption = "",
                file = "results.Table2.May10.doc")


## Main results reported in MS Table 3
texreg::htmlreg(list(final.model, final.model2, final.model4, final.model7), 
                digits = 3, leading.zero = F, single.row = T,
                override.ci.low = list(summary.final[,2], summary.final2[,2], summary.int1[,2], summary.int4[,2]), 
                override.ci.up = list(summary.final[,3], summary.final2[,3], summary.int1[,3], summary.int4[,3]),
                custom.model.names = c("Final Model I", "Final Model II", "Interaction I", "Interaction II"),
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
                                      "Consistency motivation (in-ties)", "Consistency motivation (out-ties)",
                                      "Understanding motivation (in-ties)", "Understanding motivation (out-ties)", 
                                      "Hedonic motivation (in-ties)", "Hedonic motivation (out-ties)", 
                                      "Same candidate pref", "Similar policy pref", "Similar evaluative criteria",
                                      "Isolates", "Reciprocity", "Previous communication",
                                      "Multiple two-paths (GWDSP, 1)", 
                                      "Delayed reciprocity", 
                                      "Delayed transitivity closure", "Delayed cyclic closure", 
                                      "Delayed activity closure", "Delayed popularity closure",
                                      "Persistent sender (out-tie)", "Persistent receiver (in-ties)",
                                      "Multiple path closure (GWESP-OTP, 3)", "Multiple cyclic closure (GWESP-ITP, 3)", 
                                      "Multiple activity closure (GWESP-OSP, 3)", "Multiple popularity closure (GWESP-ISP, 2)",
                                      "Activity spread (GW-outdegree, 2)", "Popularity spread (GW-indegree, 3)",
                                      "Similar ideology", "time trends (linear)", 
                                      "time X same.canddiate.pref","time X Similar ideology"),
                custom.note = " * = zero outside the 95% bias-corrected and accelerated confidence interval based on 1000 replications", 
                reorder.coef = c(1, 20:23,26:27,46,28, 47:49, 29:30,32,40:45, 31,33:39, 2:19,24:25),
                groups = list("Motivation and Homophily" = 2:9, 
                              "Interactions" = 10:12,
                              "Endogenous structural effects" = 13:21,
                              "Lagged structural effects" = 22:29,
                              "Controls" = 30:49),
                bold = 0.5, doctype = T, html.tag = T, body.tag = T, indentation = "  ",
                caption = "",
                file = "results.Table3.May10.doc")








## get coef from the fitted model, clear up '[[i]]' in the name
co <- coef(final.model4)
for (i in 1:length(co)) {
  if (grepl("((edge)|(dyad))cov", names(co)[i])) {
    names(co)[i] <- substr(names(co)[i], 1, nchar(names(co))[i] - 5)}
}

## perform edge prediction using edgeprob function
ep <- edgeprob(final.model4)

## clear up '[[i]]' in the data frame
for (i in 1:ncol(ep)) {
  if (grepl("((edge)|(dyad))cov", colnames(ep)[i])) {
    colnames(ep)[i] <- substr(colnames(ep)[i], 1, nchar(colnames(ep)[i]) - 5)
  }
}

## identify change statistics for main vars and interaction var
var1 <- "nodematch.candidate.preference"
var2 <- "edgecov.timecov1"
inter <- "edgecov.timecov2.same_candidate_preference"

## get coefficients
beta1 <- co[match(var1, names(co))]
beta2 <- co[match(var2, names(co))]
beta3 <- co[match(inter, names(co))]

ep$beta1 <- beta1
ep$X1 <- ep$nodematch.candidate.preference

ep$beta2 <- beta2
ep$X2 <- ep$edgecov.timecov1

ep$inter <- beta3
ep$X1X2 <- ep$edgecov.timecov2.same_candidate_preference

ep$logodds <- with(ep, edges * coef(final.model4)[1] + beta1 * X1 + beta2 * X2 + beta3 * X1X2) ## add intercept
ep$predprob <- with(ep, c(1 / (1 + exp(-logodds))))

ep$X1 <- car::recode(ep$X1, "0 = 'Different'; 1 = 'Same'")

## model-implied predicted probability conditional on time trends
p.inter.mn <- ggplot(data = ep, aes(x = X2, y = predprob, colour = factor(X1))) + theme_bw() + 
  scale_colour_grey(start = 0.8, end = 0.2, guide = guide_legend(reverse=TRUE)) + 
  theme(legend.justification=c(1,0), legend.position=c(0.9,0.1)) +
  geom_line(stat = "identity", size = 1.5) + labs(colour = "Candidate preference") + 
  xlab("Time trends") + ylab("Predicted edge probability") + 
  ggtitle("Panel A: Main effect, candidate preference homophily")

## how many unique change statistics in modertor?
v2 <- sort(unlist(unique(ep['edgecov.timecov1'])))
names(v2) <- NULL

## calculate theta, the logg odds of nodematch term on probability
delta1 = beta1 + beta3 * v2

## recover bootstrapped coefficients from fitted model
tempdta <- final.model4@boot$t
colnames(tempdta) <- names(co)

tempdta <- as.data.table(tempdta[, c(var1, var2, inter)])
tempdta[, t1 := nodematch.candidate.preference + (edgecov.timecov2.same_candidate_preference * 1)] # v2[1]
tempdta[, t2 := nodematch.candidate.preference + (edgecov.timecov2.same_candidate_preference * 2)] # v2[2]
tempdta[, t3 := nodematch.candidate.preference + (edgecov.timecov2.same_candidate_preference * 3)] # v2[3]

lower <- tempdta[complete.cases(tempdta), apply(.SD, 2, perc.ci, conf = 0.95), .SDcol = c("t1", "t2", "t3")][1,]
upper <- tempdta[complete.cases(tempdta), apply(.SD, 2, perc.ci, conf = 0.95), .SDcol = c("t1", "t2", "t3")][2,]

dta <- data.frame(time = v2, theta = delta1, lower = lower, upper = upper)

## interaction plot
p.inter.jn <- ggplot(data = dta, aes(x = time, y = theta)) + geom_line(color = "black") + theme_bw() + 
  geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), 
              alpha = 0.15, fill = "black") + geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Time trends") + ylab("Main effect estimate") +
  ggtitle("Panel B: JN plot for interaction effect")

require(gridExtra)
grid.arrange(p.inter.mn, p.inter.jn, nrow = 1, ncol = 2)



## does the effect of evaluative criteria amplifies for candidate preference?

same_candidate_preference  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})


criteria.X.preference <- lapply(1:3, function(i) {
  temp <- evaludative.criteria.sim[[i]] * same_candidate_preference[[i]]
  diag(temp) <- 0
  temp
})


final.model8 <- btergm(g ~ edges + ## intercept
                          
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
                          edgecov(criteria.X.preference) + 
                          
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
                          gwidegree(decay = 3, fixed = T), 
                       
                          verbose = T, R = 1000, parallel = "snow", ncpus = parallel::detectCores())

summary(final.model8, type = "bca")

ep <- edgeprob(final.model8)
setDT(ep)

ggplot(ep, aes(x = edgecov.evaludative.criteria.sim, y = probability, colour = factor(nodematch.candidate.preference))) + 
  geom_smooth(method = "glm", fullrange = T) + theme_bw() + 
  labs(colour = "Same candidate preference") + 
  scale_colour_grey(start = 0.8, end = 0.2, guide = guide_legend(reverse=TRUE)) + 
  theme(legend.justification=c(1,0), legend.position=c(0.9,0.1)) + 
  xlab("Similarity in candidate evaluative criteria") + ylab("Probability") 
