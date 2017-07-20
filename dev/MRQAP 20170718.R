
#rm(list = ls())
options(scipen = 999)
require(ergm)
require(data.table)
require(sna)
# save(g, g_relrecip, g_autoregression, policy.pref.diff, evaludative.criteria.diff, file = "R_data/ergm.test.Rdata")
setwd("~/Dropbox/(17) 2017 Spring/network QAP Korean election")

## run custom functions
source("btergm helper-functions.R")

## load the dataset from SPSS and set as data.table object
dat <- haven::read_spss("Dat/DiscussionForumThreeWavePanel(N=341).sav")
setDT(dat)

## load the node dataset for subsetting the network
dat2 <- haven::read_spss("Dat/Survey Data for Network Matrix Data.sav")
setDT(dat2)
dat2 <- na.omit(dat2[, c('r_id', 'p_image', 'm_image', 'ide_self', 'evalcrit1', 'evalcrit2', 
                         'policy_c', 'policy_l', 'knowtotal', 'talk', 'interest', "female", "edu", "age", "opleader", "follower", "income")])

## this yeilds a total of 312 cases
lengthn <- dat2[, .N]
lengthn
vids <- dat2$r_id


## prepare dependent network 
net <- read.csv("Dat/Reading_1127-1219_Participants(N=341)_Count(N=160836).csv")
net <- data.frame(reading.time = net$Reading.Time, reader.id = net$Reader.Id, poster.id = net$Poster.Id)
setDT(net)
net[, reading.date := as.Date(reading.time, format = "%Y-%m-%d %H:%M:%S")]
net[, count :=1]
net[, sum(count), by = c("poster.id", "reader.id")][, mean(V1)] ## 5.223814
net <- igraph::graph.data.frame(net[, c("reader.id", "poster.id"), with = F], directed = TRUE, vertices = 1:341)
net <- igraph::induced_subgraph(net, vids = vids)
net <- as.matrix(igraph::as_adj(net))

##prepare predictor matrix
predictor.matrix <- list()

## motivations blocks for in- and out ties
### consistency motivaiton
consistency.motivation <- apply(dat[vids, .(as.numeric(pv18),
                                            as.numeric(pv19),
                                            as.numeric(pv20),
                                            as.numeric(pv21),
                                            as.numeric(pv23),
                                            as.numeric(pv24))], 1, mean)

## consistency.motivation, in-ties (receiver effect)
predictor.matrix[[1]] <- matrix(rep(consistency.motivation, lengthn), nrow = lengthn, byrow = T) 
## consistency.motivation, out-ties (sender effect)
predictor.matrix[[2]] <- matrix(rep(consistency.motivation, lengthn), nrow = lengthn, byrow = F) 

### understanding motivation
understanding.motivation <- apply(dat[vids, pv13:pv16], 1, mean)
## understanding, in-ties (receiver effect)
predictor.matrix[[3]] <- matrix(rep(understanding.motivation, lengthn), nrow = lengthn, byrow = T) 
## understanding, out-ties (sender effect)
predictor.matrix[[4]] <- matrix(rep(understanding.motivation, lengthn), nrow = lengthn, byrow = F) 

### hedonic motivation
hedomic.motivation <- apply(dat[vids, pv27:pv29], 1, mean)
## hedonic, in-ties (receiver effect)
predictor.matrix[[5]] <- matrix(rep(hedomic.motivation, lengthn), nrow = lengthn, byrow = T) 
## hedonic, out-ties (sender effect)
predictor.matrix[[6]] <- matrix(rep(hedomic.motivation, lengthn), nrow = lengthn, byrow = F) 

### candidate preference (defined as the "mode" of all candidate choice across three observations, higher value represent more support for Moon)
candidate.preference <- apply(dat[vids, .(canpref1, canpref2, canpref3)], 1, FUN = Mode, NaN.rm = TRUE)
## candidate preference, in-ties (receiver effect)
predictor.matrix[[7]] <- matrix(rep(candidate.preference, lengthn), nrow = lengthn, byrow = T) 
## candidate preference, out-ties (sender effect)
predictor.matrix[[8]] <- matrix(rep(candidate.preference, lengthn), nrow = lengthn, byrow = F) 
## candidate preference homophily 
predictor.matrix[[9]] <- 1 - abs(outer(candidate.preference, candidate.preference, "-")) ## 1 = match, 0 = mismatch


## similar policy preference
liberal.issue.stance <- apply(dat[vids, c("pv299", "pv303", "kv39", "kv43", "hv84", "hv88"), with = F], 1, mean)
conserv.issue.stance <- apply(dat[vids, c("pv300", "pv304", "kv40", "kv44", "hv85", "hv89"), with = F], 1, mean)
policy.pref.diff <- sqrt(outer(liberal.issue.stance, liberal.issue.stance, "-")^2 + 
                         outer(conserv.issue.stance, conserv.issue.stance, "-")^2)
predictor.matrix[[10]] <- 1 / (1 + policy.pref.diff)


## evaluative criteria difference
# criteria importance - competence (1 = "not at all important" vs 7 = "extremely important")
# there were no measurement in Wave2, so we average W1 and W3 measurements
# and treat the attributes to be invariant across measurement waves
criteria.competence <- dat[vids, c("pv267", "pv268", "pv269", "pv270", "hv19", "hv20", "hv21", "hv22"), with = F][ ,
                           (Mean = rowMeans(.SD)), by = vids][,V1]
# criteria importance - background (1 = "not at all important" vs 7 = "extremely important")
# there were no measurement in Wave2, so we average W1 and W3 measurements
# and treat the attributes to be invariant across measurement waves
criteria.background <- dat[vids, c("pv271", "pv272", "pv273", "pv274", "pv275", "hv23", "hv24", "hv25", "hv26", "hv27"), with = F][ ,
                           (Mean = rowMeans(.SD)), by = vids][,V1]
evaludative.criteria.diff <- sqrt(outer(criteria.competence, criteria.competence, "-")^2 + 
                                  outer(criteria.background, criteria.background, "-")^2) 
predictor.matrix[[11]] <- 1 / (1 + evaludative.criteria.diff)

### control variables
## age , in ties
predictor.matrix[[12]] <- matrix(rep(dat[vids, as.numeric(age)/10], lengthn), nrow = lengthn, byrow = T) 
## age , out ties
predictor.matrix[[13]] <- matrix(rep(dat[vids, as.numeric(age)/10], lengthn), nrow = lengthn, byrow = F) 
## gender, in ties
predictor.matrix[[14]] <- matrix(rep(dat[vids, as.numeric(sex) - 1], lengthn), nrow = lengthn, byrow = T) 
## gender, out ties
predictor.matrix[[15]] <- matrix(rep(dat[vids, as.numeric(sex) - 1], lengthn), nrow = lengthn, byrow = F)
## gender homophily
predictor.matrix[[16]] <- 1 - abs(outer(dat[vids, as.numeric(sex) - 1], dat[vids, as.numeric(sex) - 1], "-"))
## education, in ties 
predictor.matrix[[17]] <- matrix(rep(dat[vids, as.numeric(edu)], lengthn), nrow = lengthn, byrow = T) 
## education, out ties
predictor.matrix[[18]] <- matrix(rep(dat[vids, as.numeric(edu)], lengthn), nrow = lengthn, byrow = F) 
## talk frequency, in ties
predictor.matrix[[19]] <- matrix(rep(dat[vids, .(as.numeric(pv322), 
                                                   as.numeric(kv217), 
                                                   as.numeric(hv276))][, 
                                       rowMeans(.SD), by = vids][,V1], lengthn), nrow = lengthn, byrow = T)
## talk frequency, out ties
predictor.matrix[[20]] <- matrix(rep(dat[vids, .(as.numeric(pv322), 
                                                   as.numeric(kv217), 
                                                   as.numeric(hv276))][, 
                                       rowMeans(.SD), by = vids][,V1], lengthn), nrow = lengthn, byrow = F)
## media use, in ties
predictor.matrix[[21]] <- matrix(rep(
  apply(dat[vids, .(internet.news.use1 = (60*pv310 + pv311)/60,
                    newspaper.use1 = (60*pv312 + pv313)/60, 
                    tv.news.use1 = (60*pv316 + pv317)/60,
                    internet.news.use2 = (60*kv193 + kv194)/60,
                    newspaper.use2 = (60*kv195 + kv196)/60, 
                    tv.news.use2 = (60*kv199 + kv200)/60,
                    internet.news.use3 = (60*hv252 + hv253)/60,
                    newspaper.use3 = (60*hv254 + hv255)/60, 
                    tv.news.use3 = (60*hv258 + hv259)/60)], 1, mean, na.rm = T),
  lengthn), nrow = lengthn, byrow = T)
## media use, out ties
predictor.matrix[[22]] <- matrix(rep(
  apply(dat[vids, .(internet.news.use1 = (60*pv310 + pv311)/60,
                    newspaper.use1 = (60*pv312 + pv313)/60, 
                    tv.news.use1 = (60*pv316 + pv317)/60,
                    internet.news.use2 = (60*kv193 + kv194)/60,
                    newspaper.use2 = (60*kv195 + kv196)/60, 
                    tv.news.use2 = (60*kv199 + kv200)/60,
                    internet.news.use3 = (60*hv252 + hv253)/60,
                    newspaper.use3 = (60*hv254 + hv255)/60, 
                    tv.news.use3 = (60*hv258 + hv259)/60)], 1, mean, na.rm = F),
  lengthn), nrow = lengthn, byrow = F)
## internal efficacy, main effect
predictor.matrix[[23]] <- outer(dat[vids, pv126:pv129][, rowMeans(.SD), by = vids][,V1],
                           dat[vids, pv126:pv129][, rowMeans(.SD), by = vids][,V1], "+")

## regional origin, in-tie
## 1 = Seoul, 2 = Busan, Ulsan & Kungnam, 3 = Tague and Kungbuk
## 4 = Inchun, kunggi & Kangwon, 
## 5 = Kwangju & Junnam/buck, 6 = Daejun & Chungnam/buk
## 7 = Jeuju
region_origin <- dat[vids, recode(as.numeric(region1), "1 = 1; else = 0")] 
predictor.matrix[[24]] <- matrix(rep(region_origin, lengthn), nrow = lengthn, byrow = T) 
## regional origin, out tie
predictor.matrix[[25]] <- matrix(rep(region_origin, lengthn), nrow = lengthn, byrow = F)
## regional homophily
predictor.matrix[[26]] <- 1 - abs(outer(region_origin, region_origin, "-"))

## lagged effects
net2 <- read.csv("Dat/Reading_1113-1126_Participants(N=341)_Count(N=160836).csv")
net2 <- igraph::graph.data.frame(net2[, c("reader.id", "poster.id")], directed = TRUE, vertices = 1:341)
net2 <- igraph::induced_subgraph(net2, vids = vids)
predictor.matrix[[27]] <- as.matrix(igraph::as_adj(net2)) ## previous communication

## lagged transitivity
predictor.matrix[[28]] <- t(as.matrix(igraph::as_adj(net2)))

## mutual
predictor.matrix[[29]] <- t(net) ## transpose of Y

names(predictor.matrix) <- c("consistency.motivation.in-ties",
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

##------------------------------------##
#   MR QAP using muticore processing   #
##------------------------------------##

require(parallel)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

MRQAP.model <- netlm.multicore(net, predictor.matrix, intercept = TRUE, mode = "digraph", diag = FALSE, 
                                            nullhyp = "qapspp", 
                                            test.statistic = "t-value", reps = 1000, 
                                            mc.cores = 10) 

MRQAP.model$names <- 

save(MRQAP.model, file = "MRQAP.model.2017.07.18.Rdata")

print(MRQAP.model)


# OLS Network Model
# 
# Coefficients:
#                                   Estimate     Pr(<=b) Pr(>=b) Pr(>=|b|)
# Intercept                          1.392956318 0.909   0.091   0.169    
# consistency.motivation.in-ties     0.022252657 0.604   0.396   0.790    
# consistency.motivation.out-ties   -0.019976614 0.301   0.699   0.566    
# understanding.motivation.in-ties  -0.202355081 0.025   0.975   0.051    
# understanding.motivation.out-ties  0.146368042 1.000   0.000   0.000    
# hedonic.motivation.in-ties         0.097551294 0.891   0.109   0.200    
# hedonic.motivation.out-ties       -0.310731499 0.000   1.000   0.000    
# candidate.preference.in-ties      -0.008965929 0.470   0.530   0.956    
# candidate.preference.out-ties      0.309126569 1.000   0.000   0.000    
# same.candidate.preference          0.014874547 0.601   0.399   0.799    
# similar.policy.preference         -0.155374414 0.291   0.709   0.573    
# similar.evaluative.criteria        0.613231811 0.988   0.012   0.020    
# age.in-ties                        0.028693058 0.634   0.366   0.743    
# age.out-ties                       0.339077756 1.000   0.000   0.000    
# female.in-ties                    -0.061660750 0.378   0.622   0.697    
# female.out-ties                   -0.003827449 0.472   0.528   0.946    
# same.gender                        0.055315132 0.846   0.154   0.319    
# education.in-ties                 -0.104162188 0.081   0.919   0.154    
# education.out-ties                -0.232154018 0.000   1.000   0.000    
# talk.freqency.in-ties              0.219304830 0.995   0.005   0.010    
# talk.frequency.out-ties            0.060359123 0.944   0.056   0.105    
# media.use.in-ties                 -0.062080412 0.146   0.854   0.304    
# media.use.out-ties                -0.136108728 0.000   1.000   0.000    
# internal.political.efficacy        0.052658520 0.870   0.130   0.241    
# external.political.efficacy       -0.054441610 0.152   0.848   0.295    
# regional.origin.Seoul.in-ties     -0.410514854 0.000   1.000   0.006    
# regional.origin.Seoul.out-ties     0.458260673 1.000   0.000   0.000    
# same.regional.origin              -0.032435680 0.278   0.722   0.552    
# previous.communication             2.340779727 1.000   0.000   0.000    
# lagged.reciprocity                -0.226812171 0.000   1.000   0.002    
# mutual                             0.416162912 1.000   0.000   0.000    
# 
# Residual standard error: 7.472 on 76786 degrees of freedom
# F-statistic:  1251 on 30 and 76786 degrees of freedom, p-value:     0 
# Multiple R-squared: 0.3284 	Adjusted R-squared: 0.3281 
