
library(foreign)
library(plyr)

if(!("haven" %in% installed.packages()[,"Package"])) install.packages("haven")
if(!("data.table" %in% installed.packages()[,"Package"])) install.packages("data.table")
library(haven)
library(data.table)
require(igraph)
require(sna)
require(car)
require(parallel)

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

##---------------------------------##
## create a dependent network list ##
##---------------------------------##

## pre-wave: Nov 13 to Nov 26; Wave 1 survey: Nov 27 to Nov 29, 
## Wave 2 survey: Dec 11th to 13th, and Wave 3 survey: Dec 21th to 23th

g <- list()
g_pre <- list()

net <- read.csv("Dat/Reading_1113-1126_Participants(N=341)_Count(N=160836).csv")
net2 <- read.csv("Dat/Reading_1127-1219_Participants(N=341)_Count(N=160836).csv")
net2 <- data.frame(reading.time = net2$Reading.Time, reader.id = net2$Reader.Id, poster.id = net2$Poster.Id)
net <- rbind(net, net2)
setDT(net)
net[, reading.date := as.Date(reading.time, format = "%Y-%m-%d %H:%M:%S")]

## Wave 1 network from 11/27 to 12/04 (survey administered at Nov 27 to 29)
g[[1]] <- net[reading.date %between% c("2012-11-27", "2012-11-29"),]
g[[1]] <- data.frame(g[[1]][,2], g[[1]][,3]) ## reader: source, poster: target
setDT(g[[1]]); g[[1]][, count :=1]
g[[1]][, sum(count), by = c("poster.id", "reader.id")][poster.id %in% vids & reader.id %in% vids, mean(V1)] 
g[[1]] <- graph.data.frame(g[[1]], directed = TRUE, vertices = 1:341)
g[[1]] <- induced_subgraph(g[[1]], vids = vids)
g[[1]] <- as.matrix(as_adj(g[[1]]))
g[[1]] <- sna::event2dichot(g[[1]], method = "absolute", thresh = 2.508298)
g[[1]] <- as.network(g[[1]])

## Wave 2 network from 12/11 to 12/13 (survey administered at Dec 11th to 13th?)
g[[2]] <- net[reading.date %between% c("2012-12-11", "2012-12-13"),]
g[[2]] <- data.frame(g[[2]][,2], g[[2]][,3]) ## reader: source, poster: target
setDT(g[[2]]); g[[2]][, count :=1]
g[[2]][, sum(count), by = c("poster.id", "reader.id")][poster.id %in% vids & reader.id %in% vids, mean(V1)] 
g[[2]] <- induced_subgraph(graph.data.frame(g[[2]], directed = TRUE, vertices = 1:341), vids = vids)
g[[2]] <- as.matrix(as_adj(g[[2]]))
g[[2]] <- sna::event2dichot(g[[2]], method = "absolute", thresh = 2.909601)
g[[2]] <- as.network(g[[2]])


## Wave 3 network from 12/17 to 12/19 (*** survey administered at Dec 21th to 23th)
g[[3]] <- net[reading.date %between% c("2012-12-17", "2012-12-19"),]
g[[3]] <- data.frame(g[[3]][,2], g[[3]][,3]) ## reader: source, poster: target
setDT(g[[3]]); g[[3]][, count :=1]
g[[3]][, sum(count), by = c("poster.id", "reader.id")][poster.id %in% vids & reader.id %in% vids, mean(V1)] 
g[[3]] <- induced_subgraph(graph.data.frame(g[[3]], directed = TRUE, vertices = 1:341), vids = vids)
g[[3]] <- as.matrix(as_adj(g[[3]]))
g[[3]] <- sna::event2dichot(g[[3]], method = "absolute", thresh = 3.233531)
g[[3]] <- as.network(g[[3]])


## previous communication patterns
## Pre-wave (Wave 0) network from Nov 23 to Nov 26
g_pre[[1]] <- net[reading.date %between% c("2012-11-23", "2012-11-26"),]
g_pre[[1]] <- data.frame(g_pre[[1]][,2], g_pre[[1]][,3]) ## reader: source, poster: target
setDT(g_pre[[1]]); g_pre[[1]][, count :=1]
g_pre[[1]][, sum(count), by = c("poster.id", "reader.id")][poster.id %in% vids & reader.id %in% vids, mean(V1)] 
g_pre[[1]] <- graph.data.frame(g_pre[[1]], directed = TRUE, vertices = 1:341)
g_pre[[1]] <- induced_subgraph(g_pre[[1]], vids = vids)
#g_pre[[1]] <- simplify(g_pre[[1]], remove.multiple = T, remove.loops = T)
g_pre[[1]] <- as.matrix(as_adj(g_pre[[1]]))
g_pre[[1]] <- sna::event2dichot(g_pre[[1]], method = "absolute", thresh = 2.331749)
g_pre[[1]] <- as.network(g_pre[[1]])

## between Wave 1 and Wave 2
## network from Dec 05 to Dec 10
g_pre[[2]] <- net[reading.date %between% c("2012-12-05", "2012-12-10"),]
g_pre[[2]] <- data.frame(g_pre[[2]][,2], g_pre[[2]][,3]) ## reader: source, poster: target
setDT(g_pre[[2]]); g_pre[[2]][, count :=1]
g_pre[[2]][, sum(count), by = c("poster.id", "reader.id")][poster.id %in% vids & reader.id %in% vids, mean(V1)] 
g_pre[[2]] <- graph.data.frame(g_pre[[2]], directed = TRUE, vertices = 1:341)
g_pre[[2]] <- induced_subgraph(g_pre[[2]], vids = vids)
#g_pre[[2]] <- simplify(g_pre[[2]], remove.multiple = T, remove.loops = T)
g_pre[[2]] <- as.matrix(as_adj(g_pre[[2]]))
g_pre[[2]] <- sna::event2dichot(g_pre[[2]], method = "absolute", thresh = 3.122216)
g_pre[[2]] <- as.network(g_pre[[2]])

## between Wave 2 and Wave 3
## network from Dec 14 to Dec 16
g_pre[[3]] <- net[reading.date %between% c("2012-12-14", "2012-12-16"),]
g_pre[[3]] <- data.frame(g_pre[[3]][,2], g_pre[[3]][,3]) ## reader: source, poster: target
setDT(g_pre[[3]]); g_pre[[3]][, count :=1]
g_pre[[3]][, sum(count), by = c("poster.id", "reader.id")][poster.id %in% vids & reader.id %in% vids, mean(V1)] 
g_pre[[3]] <- graph.data.frame(g_pre[[3]], directed = TRUE, vertices = 1:341)
g_pre[[3]] <- induced_subgraph(g_pre[[3]], vids = vids)
#g_pre[[3]] <- simplify(g_pre[[3]], remove.multiple = T, remove.loops = T)
g_pre[[3]] <- as.matrix(as_adj(g_pre[[3]]))
g_pre[[3]] <- sna::event2dichot(g_pre[[3]], method = "absolute", thresh = 2.721771)
g_pre[[3]] <- as.network(g_pre[[3]])

##------------------------------------------##
## create a set of covariates for each wave ##
##------------------------------------------##

## motivation for using online forum
consistency.motivation <- dat[vids, .(as.numeric(pv18),
                                      as.numeric(pv19),
                                      as.numeric(pv20),
                                      as.numeric(pv21),
                                      as.numeric(pv23),
                                      as.numeric(pv24))]

g[[1]] %v% "consistency.motivation" <- apply(consistency.motivation, 1, mean)
g[[2]] %v% "consistency.motivation" <- apply(consistency.motivation, 1, mean)
g[[3]] %v% "consistency.motivation" <- apply(consistency.motivation, 1, mean)

understanding.motivation <- dat[vids, pv13:pv16]
g[[1]] %v% "understanding.motivation" <- apply(understanding.motivation, 1, mean)
g[[2]] %v% "understanding.motivation" <- apply(understanding.motivation, 1, mean)
g[[3]] %v% "understanding.motivation" <- apply(understanding.motivation, 1, mean)

hedomic.motivation <- dat[vids, pv27:pv29]
g[[1]] %v% "hedomic.motivation" <- apply(hedomic.motivation, 1, mean)
g[[2]] %v% "hedomic.motivation" <- apply(hedomic.motivation, 1, mean)
g[[3]] %v% "hedomic.motivation" <- apply(hedomic.motivation, 1, mean)

## CANDIDATE IMAGE
# # candidate image (Park), W1 to W3
# g[[1]] %v% "candidate.image.Park" <- dat[vids, pv194:pv208][, (Mean = rowMeans(.SD)), by = vids][,V1]
# g[[2]] %v% "candidate.image.Park" <- dat[vids, kv7:kv21][, (Mean = rowMeans(.SD)), by = vids][,V1]
# g[[3]] %v% "candidate.image.Park" <- dat[vids, hv32:hv46][, (Mean = rowMeans(.SD)), by = vids][,V1]
# 
# # candidate image (Moon), W1 to W3
# g[[1]] %v% "candidate.image.Moon" <- dat[vids, pv209:pv223][, (Mean = rowMeans(.SD)), by = vids][,V1]
# g[[2]] %v% "candidate.image.Moon" <- dat[vids, kv22:kv36][, (Mean = rowMeans(.SD)), by = vids][,V1]
# g[[3]] %v% "candidate.image.Moon" <- dat[vids, hv47:hv61][, (Mean = rowMeans(.SD)), by = vids][,V1]

## CANDIDATE PREFERENCE
g[[1]] %v% "candidate.preference" <- dat[vids, as.numeric(canpref1)] ## 0 = Park, 1 = Moon
g[[2]] %v% "candidate.preference" <- dat[vids, as.numeric(canpref2)] ## 0 = Park, 1 = Moon
g[[3]] %v% "candidate.preference" <- dat[vids, as.numeric(canpref3)] ## 0 = Park, 1 = Moon

# ## CANDIDATE PREFERENCE IN THERMOMETER_RATING FORM (Moon minus Park)
# g[[1]] %v% "thermo.diff" <- dat[vids, as.numeric(pv255) - as.numeric(pv254)]
# g[[2]] %v% "thermo.diff" <- dat[vids, as.numeric(kv38) - as.numeric(kv37)]
# g[[3]] %v% "thermo.diff" <- dat[vids, as.numeric(hv63) - as.numeric(hv62)]
# 
# ## preference strengths
# g[[1]] %v% "preference.strength" <- abs(g[[1]] %v% "thermo.diff")
# g[[2]] %v% "preference.strength" <- abs(g[[2]] %v% "thermo.diff")
# g[[3]] %v% "preference.strength" <- abs(g[[3]] %v% "thermo.diff")

## issue preference
## pv299 / kv39 / hv84: prefer big goverment and regulations
## pv303 / kv43 / hv88: prefer soft diplomatic stance with North Korea

## pv300 / kv40 / hv85: prefer small goverment and less regulations
## pv304 / kv44 / hv89: prefer hard solution with North Korea

g[[1]] %v% "liberal.issue.stance" <- apply(dat[vids, c("pv299", "pv303"), with = F], 1, mean)
g[[1]] %v% "conserv.issue.stance" <- apply(dat[vids, c("pv300", "pv304"), with = F], 1, mean)

g[[2]] %v% "liberal.issue.stance" <- apply(dat[vids, c("kv39", "kv43"), with = F], 1, mean)
g[[2]] %v% "conserv.issue.stance" <- apply(dat[vids, c("kv40", "kv44"), with = F], 1, mean)

g[[3]] %v% "liberal.issue.stance" <- apply(dat[vids, c("hv84", "hv88"), with = F], 1, mean)
g[[3]] %v% "conserv.issue.stance" <- apply(dat[vids, c("hv85", "hv89"), with = F], 1, mean)

## additionally make a list of dyadic ED matrix
policy.pref.diff <- list()

policy.pref.diff[[1]] <- sqrt(outer(g[[1]] %v% "liberal.issue.stance", g[[1]] %v% "liberal.issue.stance", "-")^2 + 
                                outer(g[[1]] %v% "conserv.issue.stance", g[[1]] %v% "conserv.issue.stance", "-")^2)

policy.pref.diff[[2]] <- sqrt(outer(g[[2]] %v% "liberal.issue.stance", g[[2]] %v% "liberal.issue.stance", "-")^2 + 
                                outer(g[[2]] %v% "conserv.issue.stance", g[[2]] %v% "conserv.issue.stance", "-")^2)

policy.pref.diff[[3]] <- sqrt(outer(g[[3]] %v% "liberal.issue.stance", g[[3]] %v% "liberal.issue.stance", "-")^2 + 
                                outer(g[[3]] %v% "conserv.issue.stance", g[[3]] %v% "conserv.issue.stance", "-")^2)


## CRITERIA IMPORTANCE
# criteria importance - competence (1 = "not at all important" vs 7 = "extremely important")
# there were no measurement in Wave2, so we average W1 and W3 measurements
# and treat the attributes to be invariant across measurement waves
criteria.competence <- dat[vids, c("pv267", "pv268", "pv269", "pv270", "hv19", "hv20", "hv21", "hv22"), with = F][ ,
                                                                                                                   (Mean = rowMeans(.SD)), by = vids][,V1]
g[[1]] %v% "criteria.competence" <- g[[2]] %v% "criteria.competence" <- g[[3]] %v% "criteria.competence" <- criteria.competence

# criteria importance - background (1 = "not at all important" vs 7 = "extremely important")
# there were no measurement in Wave2, so we average W1 and W3 measurements
# and treat the attributes to be invariant across measurement waves
criteria.background <- dat[vids, c("pv271", "pv272", "pv273", "pv274", "pv275", "hv23", "hv24", "hv25", "hv26", "hv27"), with = F][ ,
                                                                                                                                    (Mean = rowMeans(.SD)), by = vids][,V1]
g[[1]] %v% "criteria.background" <- g[[2]] %v% "criteria.background" <- g[[3]] %v% "criteria.background" <- criteria.background

## additionally make a list of dyadic ED matrix
evaludative.criteria.diff <- list()
evaludative.criteria.diff[[1]] <- sqrt(outer(criteria.competence, criteria.competence, "-")^2 + 
                                         outer(criteria.background, criteria.background, "-")^2) 
evaludative.criteria.diff[[3]] <- evaludative.criteria.diff[[2]] <- evaludative.criteria.diff[[1]]

## make distance based similarity measure by taking 1/(1+distance)
## the logic is, if the distance is zero, then similarity is 1,
## and as the distance gets larger, the similarity gets smaller to zero.
policy.pref.sim <- evaludative.criteria.sim <- list()
for (i in 1:3) {
  policy.pref.sim[[i]] <- 1 / (1 + policy.pref.diff[[i]])
  evaludative.criteria.sim[[i]] <- 1 / (1 + evaludative.criteria.diff[[i]]) 
  diag(policy.pref.sim[[i]]) <- diag(evaludative.criteria.sim[[i]]) <- 0
}

## KNOWLEDGE AND INTEREST
## knowledge batteries: pv177 pv178 pv179 pv180 pv181 pv182 pv183 pv184 pv185 pv185_1
# there were no measurement in Wave 2 and Wave 3, so we treat the attributes to be invariant across measurement waves
# g[[1]] %v% "knowledge" <- rowSums(dat[vids, .(KN1 = recode(pv177, "2 = 1; else = 0"),
#                                               KN2 = recode(pv178, "1 = 1; else = 0"),
#                                               KN3 = recode(pv179, "2 = 1; else = 0"),
#                                               KN4 = recode(pv180, "2 = 1; else = 0"),
#                                               KN5 = recode(pv181, "1 = 1; else = 0"),
#                                               KN6 = recode(pv182, "3 = 1; else = 0"),
#                                               KN7 = recode(pv183, "4 = 1; else = 0"),
#                                               KN8 = recode(pv184, "5 = 1; else = 0"), 
#                                               KN9 = recode(pv185, "2 = 1; else = 0"),
#                                               KN0 = recode(pv185_1, "4 = 1; else = 0"))])
# 
# g[[3]] %v% "knowledge" <- g[[2]] %v% "knowledge" <- g[[1]] %v% "knowledge"            

## interest baterries: pv165 & pv166
# there were no measurement in Wave 2 and Wave 3, so we treat the attributes to be invariant across measurement waves

# g[[1]] %v% "interest" <- dat[vids, pv165:pv166][, rowMeans(.SD), by = vids][,V1] 
# g[[3]] %v% "interest" <- g[[2]] %v% "interest" <- g[[1]] %v% "interest"    
# 
# 
# ## combining knowledge and interest to "expertise"
# g[[1]] %v% "expertise" <- apply(
#                                 cbind(scale(g[[1]] %v% "knowledge", scale = T),
#                                       scale(g[[1]] %v% "interest", scale = T)), 
#                                 1, mean)
# 
# g[[3]] %v% "expertise" <- g[[2]] %v% "expertise" <- g[[1]] %v% "expertise"


# ## external efficacy:
# g[[1]] %v% "external.efficacy" <- dat[vids, .(8 - pv163, 8 - pv164, pv165, pv167)][, rowMeans(.SD), by = vids][,V1]
# g[[2]] %v% "external.efficacy" <- dat[vids, .(8 - kv177, 8 - kv178, kv179, kv181)][, rowMeans(.SD), by = vids][,V1]
# g[[3]] %v% "external.efficacy" <- dat[vids, .(8 - hv232, 8 - hv233, hv238, hv239)][, rowMeans(.SD), by = vids][,V1]

## internal efficacy
g[[1]] %v% "internal.efficacy" <- dat[vids, pv126:pv129][, rowMeans(.SD), by = vids][,V1]
g[[3]] %v% "internal.efficacy" <- g[[2]] %v% "internal.efficacy" <- g[[1]] %v% "internal.efficacy" 
## news media use

## remove "NaN" in data -- W1
dat[is.na(pv311), pv311 := 0 ]
dat[is.na(pv313), pv313 := 0 ]
dat[is.na(pv317), pv317 := 0 ]

## add with hours, and creaet index
temp.test <- dat[vids, .(internet.news.use = (60*pv310 + pv311)/60,
                         newspaper.use = (60*pv312 + pv313)/60, 
                         tv.news.use = (60*pv316 + pv317)/60)]
g[[1]] %v% "media.use.freq" <- apply(temp.test, 1, mean)


## remove "NaN" in data -- W2
dat[is.na(kv194), kv194 := 0 ]
dat[is.na(kv196), kv196 := 0 ]
dat[is.na(kv200), kv200 := 0 ]

## add with hours, and creaet index
temp.test <- dat[vids, .(internet.news.use = (60*kv193 + kv194)/60,
                         newspaper.use = (60*kv195 + kv196)/60, 
                         tv.news.use = (60*kv199 + kv200)/60)]
g[[2]] %v% "media.use.freq" <- apply(temp.test, 1, mean)


## remove "NaN" in data -- W3
dat[is.na(hv253), hv253 := 0 ]
dat[is.na(hv255), hv255 := 0 ]
dat[is.na(hv259), hv259 := 0 ]

## add with hours, and creaet index
temp.test <- dat[vids, .(internet.news.use = (60*hv252 + hv253)/60,
                         newspaper.use = (60*hv254 + hv255)/60, 
                         tv.news.use = (60*hv258 + hv259)/60)]
g[[3]] %v% "media.use.freq" <- apply(temp.test, 1, mean)


## offline discussion frequency regarding politics (1 = "never" vs 7 = "always")
g[[1]] %v% "talk.freq" <- dat[vids, as.numeric(pv322)]
g[[2]] %v% "talk.freq" <- dat[vids, as.numeric(kv217)]
g[[3]] %v% "talk.freq" <- dat[vids, as.numeric(hv276)]

# ## political ideology (1 = "extremely liberal" to 7 = "extremely conservative")
# g[[1]] %v% "pol.ideology" <- dat[vids, as.numeric(pv258)]
# g[[2]] %v% "pol.ideology" <- dat[vids, as.numeric(kv49)]
# g[[3]] %v% "pol.ideology" <- dat[vids, as.numeric(hv104)]
# 
# ## opinionated personality
# g[[1]] %v% "opinionated" <- dat[vids, as.numeric(pv45)]
# g[[2]] %v% "opinionated" <- dat[vids, as.numeric(kv82)]
# g[[3]] %v% "opinionated" <- dat[vids, as.numeric(hv137)]
# 
# ## submissive personality
# g[[1]] %v% "submissive" <- dat[vids, as.numeric(pv47)]
# g[[2]] %v% "submissive" <- dat[vids, as.numeric(kv84)]
# g[[3]] %v% "submissive" <- dat[vids, as.numeric(hv139)]
# 

# ## message quality 
# g[[1]] %v% "message.quality" <- dat[vids, .(quality = rowMeans(as.matrix(.SD), na.rm = T)), 
#                                    .SDcols = c("툴민_주장_mean", "툴민_근거_mean",
#                                                "툴민_보장_mean", "툴민_보강_mean")][, 
#                                    recode(quality, "NA = 0; NaN = 0")]
# g[[3]] %v% "message.quality" <- g[[2]] %v% "message.quality" <- g[[1]] %v% "message.quality"
# 
# ## message cues 
# g[[1]] %v% "message.cue" <- dat[vids, .(quality = rowMeans(as.matrix(.SD), na.rm = T)), 
#                                     .SDcols = c("추천_mean", "댓글_mean", "반대_mean")][, 
#                                     recode(quality, "NA = 0; NaN = 0")]
# g[[3]] %v% "message.cue" <- g[[2]] %v% "message.cue" <- g[[1]] %v% "message.cue"

## demographics (time-invariant) covariates

for (i in 1:3) {
  g[[i]] %v% "age" <- dat[vids, as.numeric(age)/10] ## age in ten years
  g[[i]] %v% "gender" <- dat[vids, as.numeric(sex) - 1] ## 1 = female, 0 = male
  g[[i]] %v% "edu" <- dat[vids, as.numeric(edu)] ## education level (1 = "less than elemantry" vs. 9 = "more than postgraduate")
  
  ## region of origin
  ## 1 = Seoul, 2 = Busan, Ulsan & Kungnam, 3 = Tague and Kungbuk
  ## 4 = Inchun, kunggi & Kangwon, 
  ## 5 = Kwangju & Junnam/buck, 6 = Daejun & Chungnam/buk
  ## 7 = Jeuju
  g[[i]] %v% "region_origin" <- dat[vids, recode(as.numeric(region1), "1 = 1; 
                                                 2 = 2; 14 = 2; 7 = 2;
                                                 3 = 3; 13 = 3;
                                                 4 = 4; 8 = 4;
                                                 5 = 5; 11:12 = 5; 
                                                 6 = 6; 9:10 = 6;
                                                 15 = 7")] 
  ## region of origin2 -- seoul vs all other
  g[[i]] %v% "region_origin2" <- dat[vids, recode(as.numeric(region1), "1 = 1; else = 0")] 
}

## lagged sender and receiver effect
g[[1]] %v% "lagged.sender.effect" <- rowSums(as.matrix(g_pre[[1]]))
g[[2]] %v% "lagged.sender.effect" <- rowSums(as.matrix(g_pre[[2]]))
g[[3]] %v% "lagged.sender.effect" <- rowSums(as.matrix(g_pre[[3]]))

g[[1]] %v% "lagged.receiver.effect" <- colSums(as.matrix(g_pre[[1]]))
g[[2]] %v% "lagged.receiver.effect" <- colSums(as.matrix(g_pre[[2]]))
g[[3]] %v% "lagged.receiver.effect" <- colSums(as.matrix(g_pre[[3]]))


## lagged reciprocity and autoregression
g_delrecip <- g_autoregression <- g_innovation <- list()
for (i in 1:3) {
  temp <- as.matrix(g_pre[[i]])
  
  ## sufficient statistics (h_ds) for edge innovation is = Sig_ij (1- N_{ij} at t-1) * N_{ij} at t
  ## this represents cases where there was no tie at t-1 yet there's new tie at t between the (i,j)
  ## change statistics can be defined as:
  ## delta = h_ds(+) - h_ds(-), where we simply plugg in 1 for N_{ij} at t in case of h_ds(+), and 0 for h_ds(-)
  ## therfore, delta_ij = (1- N_{ij} at t-1) * 1 - (1- N_{ij} at t-1) * 0
  ## delta_ij = (1- N_{ij} at t-1) => 1 minus previous tie
  g_innovation[[i]] <- 1 - temp  
  
  ## sufficient statistics for edge autoregression is = Sig_ij (N_{ij} at t-1) * N_{ij} at t 
  ## which represent cases where ties in t-1 still exists in t
  ## change statistics delta_ij = h_ds(+) - h_ds(-)
  ## delta_ij = (N_{ij} at t-1) * 1 - (N_{ij} at t-1) * 0
  ## delta_ij = N_{ij} at t-1 => previous tie
  ## temp[temp == 0] <- -1
  g_autoregression[[i]] <- temp
  
  g_delrecip[[i]] <- t(as.matrix(g_pre[[i]]))
}

## delayed transitivity and delayed cyclic closure 
g_lagtransitivity <- g_lagcyclic <- list()
g_lag_shared_activity <- g_lag_shared_popularity <- list()
for (i in 1:3) {
  temp <- as.matrix(g_pre[[i]]) %*% as.matrix(g_pre[[i]])
  diag(temp) <- 0
  g_lagtransitivity[[i]] <- temp
  
  temp <- t(as.matrix(g_pre[[i]])) %*% t(as.matrix(g_pre[[i]]))
  diag(temp) <- 0
  g_lagcyclic[[i]] <- temp
  
  temp <- as.matrix(g_pre[[i]]) %*% t(as.matrix(g_pre[[i]]))
  diag(temp) <- 0
  g_lag_shared_activity[[i]] <- temp
  
  temp <- t(as.matrix(g_pre[[i]])) %*% as.matrix(g_pre[[i]])
  diag(temp) <- 0
  g_lag_shared_popularity[[i]] <- temp
}

# asymmetrical.expertise <- lapply(g, make_asymmetrical_adj, "expertise")
# asymmetrical.interest <- lapply(g, make_asymmetrical_adj, "interest")
# 
# alter_more_expertise <- lapply(asymmetrical.expertise, function(x) {
#   mat <- 1 * (x > 0)
#   diag(mat) <- 0
#   mat
# }
# )
# 
# alter_more_interested <- lapply(asymmetrical.interest, function(x) {
#   mat <- 1 * (x > 0)
#   diag(mat) <- 0
#   mat
# })


# ## source and sink nodes
# for (i in 1:3) {
# out <- data.table(indegree = degree(g[[i]], gmode = "digraph", cmode = "indegree"), 
#                   outdegree = degree(g[[i]], gmode = "digraph", cmode = "outdegree"))
# out[, sink := .(indegree > 0 & outdegree == 0)]
# out[, source := .(indegree == 0 & outdegree > 0)]
# g[[i]] %v% "sink" <- out[, sink]
# g[[i]] %v% "source" <- out[, source]
# }

# ## factor analysis for homophily variable groups
# dat.factor <- data.table(pol.ideo.W1 = dat[vids, as.numeric(pv258)], 
#                          pol.ideo.W2 = dat[vids, as.numeric(kv49)],
#                          pol.ideo.W3 = dat[vids, as.numeric(hv104)],
#                          
#                          can.pref.W1 = dat[vids, as.numeric(canpref1)],
#                          can.pref.W2 = dat[vids, as.numeric(canpref2)],
#                          can.pref.W3 = dat[vids, as.numeric(canpref3)],
#                          
#                          liberal.stance.W1 = apply(dat[vids, c("pv299", "pv303"), with = F], 1, mean),
#                          liberal.stance.W2 = apply(dat[vids, c("kv39", "kv43"), with = F], 1, mean),
#                          liberal.stance.W3 = apply(dat[vids, c("hv84", "hv88"), with = F], 1, mean),
#                            
#                          conserv.stance.W1 = apply(dat[vids, c("pv300", "pv304"), with = F], 1, mean),
#                          conserv.stance.W2 = apply(dat[vids, c("kv40", "kv44"), with = F], 1, mean),
#                          conserv.stance.W3 = apply(dat[vids, c("hv85", "hv89"), with = F], 1, mean))
# 
# # Determine Number of Factors to Extract
# library(nFactors)
# ev <- eigen(cor(dat.factor)) # get eigenvalues
# ap <- parallel(subject=nrow(dat.factor),var=ncol(dat.factor),
#                rep=100,cent=.05)
# nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
# plotnScree(nS)  ## three-factor solution appears to be enough
# 
# library(psych)
# fit <- factanal(dat.factor, 3, rotation = "varimax")
# print(fit, digits=2, cutoff=.3, sort=TRUE)

# rm(dat.factor, ev, ap, nS, fit)

## the pattern suggests that ideology and candidate preference would load on a single dimension,
## so we only look at the candidate preference homophily. Policy stance is loaded on a seperate dimension.

rm(dat2, consistency.motivation, hedomic.motivation, social.motivation,
   net, net2, temp, temp.test, evaludative.criteria.diff, policy.pref.diff,
   understanding.motivation, criteria.background, criteria.competence, g_pre, i, lengthn)

 
