
#rm(list = ls())
options(scipen = 999)
require(ergm)
require(data.table)
require(sna)

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



