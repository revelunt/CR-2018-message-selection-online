
options(scipen = 999)
setwd("~/Dropbox/(17) 2017 Spring/network QAP Korean election")
rm(list=ls())
library(foreign)
library(plyr)

if(!("haven" %in% installed.packages()[,"Package"])) install.packages("haven")
if(!("data.table" %in% installed.packages()[,"Package"])) install.packages("data.table")
library(haven)
library(data.table)
require(igraph)
require(sna)
require(car)

RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

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
## Wave 2 survey: Dec 4th to 6th (??), and Wave 3 survey: Dec 17th to 19th

g <- array(NA, dim = c(lengthn, lengthn, 3))

net <- read.csv("Dat/Reading_1113-1126_Participants(N=341)_Count(N=160836).csv")
net2 <- read.csv("Dat/Reading_1127-1219_Participants(N=341)_Count(N=160836).csv")
net2 <- data.frame(reading.time = net2$Reading.Time, reader.id = net2$Reader.Id, poster.id = net2$Poster.Id)
net <- rbind(net, net2)
setDT(net)
net[, reading.date := as.Date(reading.time, format = "%Y-%m-%d %H:%M:%S")]


## Wave 1 network from 11/27 to 11/29 (survey administered at Nov 27 to 29)
g1 <- net[reading.date %between% c("2012-11-27", "2012-11-29"),]
g1 <- data.frame(g1[,2], g1[,3]) ## reader: source, poster: target
setDT(g1); g1[, count :=1]
g1[, sum(count), by = c("poster.id", "reader.id")][, mean(V1)] ## 2.487684
g1 <- graph.data.frame(g1, directed = TRUE, vertices = 1:341)
g1 <- induced_subgraph(g1, vids = vids)
g1 <- as.matrix(as_adj(g1))
g[,,1] <- sna::event2dichot(g1, method = "absolute", thresh = 0.5)


## Wave 2 network from 12/11 to 12/13 (survey administered at Dec 11th to 13th?)
g2 <- net[reading.date %between% c("2012-12-11", "2012-12-13"),]
g2 <- data.frame(g2[,2], g2[,3]) ## reader: source, poster: target
setDT(g2); g2[, count :=1]
g2[, sum(count), by = c("poster.id", "reader.id")][, mean(V1)] ## 2.901684
g2 <- induced_subgraph(graph.data.frame(g2, directed = TRUE, vertices = 1:341), vids = vids)
g2 <- as.matrix(as_adj(g2))
g[,,2] <- sna::event2dichot(g2, method = "absolute", thresh = 0.5)


## Wave 3 network from 12/17 to 12/19 (survey administered at Dec 21th to 23th)
g3 <- net[reading.date %between% c("2012-12-17", "2012-12-19"),]
g3 <- data.frame(g3[,2], g3[,3]) ## reader: source, poster: target
setDT(g3); g3[, count :=1]
g3[, sum(count), by = c("poster.id", "reader.id")][, mean(V1)] ## 3.290696
g3 <- induced_subgraph(graph.data.frame(g3, directed = TRUE, vertices = 1:341), vids = vids)
g3 <- as.matrix(as_adj(g3))
g[,,3] <- sna::event2dichot(g3, method = "absolute", thresh = 0.5)

## graphs in one place
require(sna)
coordxy <- gplot(as.network(g[,,1]))
par(mfrow = c(1, 3))
gplot(g[,,1], xlab = 'online forum message selection t1', coord = coordxy)
gplot(g[,,2], xlab = 'online forum message selection t2', coord = coordxy)
gplot(g[,,3], xlab = 'online forum message selection t3', coord = coordxy) 
par(mfrow = c(1, 1))

##------------------------------------------##
## create a set of covariates for each wave ##
##------------------------------------------##

require(RSiena)
## CANDIDATE IMAGE
# candidate image (Park), W1 to W3
candidate.image.Park <- cbind(dat[vids, pv194:pv208][, (Mean = rowMeans(.SD)), by = vids][,V1],
                              dat[vids, kv7:kv21][, (Mean = rowMeans(.SD)), by = vids][,V1],
                              dat[vids, hv32:hv46][, (Mean = rowMeans(.SD)), by = vids][,V1])

# candidate image (Moon), W1 to W3
candidate.image.Moon <- cbind(dat[vids, pv209:pv223][, (Mean = rowMeans(.SD)), by = vids][,V1],
                              dat[vids, kv22:kv36][, (Mean = rowMeans(.SD)), by = vids][,V1],
                              dat[vids, hv47:hv61][, (Mean = rowMeans(.SD)), by = vids][,V1])

candidate.image.Park <- varCovar(candidate.image.Park)
candidate.image.Moon <- varCovar(candidate.image.Moon)


## CANDIDATE PREFERENCE
candidate.preference <- cbind(dat[vids, as.numeric(canpref1)], ## 0 = Park, 1 = Moon
                              dat[vids, as.numeric(canpref2)],
                              dat[vids, as.numeric(canpref3)])

candidate.preference <- varCovar(candidate.preference)


## CANDIDATE PREFERENCE BASED ON FEELING THERMOMETER (Park minus Moon)
cand.pref.thermo <- cbind(dat[vids, as.numeric(pv254 - pv255)], 
                          dat[vids, as.numeric(kv37 - kv38)],
                          dat[vids, as.numeric(hv62 - hv63)])
cand.pref.thermo <- varCovar(cand.pref.thermo)

## issue preference
## pv299 / kv39 / hv84: prefer big goverment and regulations
## pv303 / kv43 / hv88: prefer soft diplomatic stance with North Korea

## pv300 / kv40 / hv85: prefer small goverment and less regulations
## pv304 / kv44 / hv89: prefer hard solution with North Korea

liberal.issue.stance <- cbind(apply(dat[vids, c("pv299", "pv303"), with = F], 1, mean),
                              apply(dat[vids, c("kv39", "kv43"), with = F], 1, mean),
                              apply(dat[vids, c("hv84", "hv88"), with = F], 1, mean))

conserv.issue.stance <- cbind(apply(dat[vids, c("pv300", "pv304"), with = F], 1, mean),
                              apply(dat[vids, c("kv40", "kv44"), with = F], 1, mean),
                              apply(dat[vids, c("hv85", "hv89"), with = F], 1, mean))

liberal.issue.stance <- varCovar(liberal.issue.stance)
conserv.issue.stance <- varCovar(conserv.issue.stance)


## additionally make a list of dyadic ED matrix
policy.pref.diff <- array(NA, dim = c(lengthn, lengthn, 2))
# policy.pref.diff[,,1] <- sqrt(outer(liberal.issue.stance[,1], liberal.issue.stance[,1], "-")^2 + 
#                                 outer(conserv.issue.stance[,1], conserv.issue.stance[,1], "-")^2)
policy.pref.diff[,,1] <- sqrt(outer(liberal.issue.stance[,2], liberal.issue.stance[,2], "-")^2 + 
                                outer(conserv.issue.stance[,2], conserv.issue.stance[,2], "-")^2)
policy.pref.diff[,,2] <- sqrt(outer(liberal.issue.stance[,3], liberal.issue.stance[,3], "-")^2 + 
                                outer(conserv.issue.stance[,3], conserv.issue.stance[,3], "-")^2)

policy.pref.diff <- varDyadCovar(policy.pref.diff)

## CRITERIA IMPORTANCE
# criteria importance - competence (1 = "not at all important" vs 7 = "extremely important")
# there were no measurement in Wave2, so we average W1 and W3 measurements
# and treat the attributes to be invariant across measurement waves
criteria.competence <- dat[vids, c("pv267", "pv268", "pv269", "pv270", "hv19", "hv20", "hv21", "hv22"), with = F][ ,
                           (Mean = rowMeans(.SD)), by = vids][,V1]
criteria.competence <- coCovar(criteria.competence)

# criteria importance - background (1 = "not at all important" vs 7 = "extremely important")
# there were no measurement in Wave2, so we average W1 and W3 measurements
# and treat the attributes to be invariant across measurement waves
criteria.background <- dat[vids, c("pv271", "pv272", "pv273", "pv274", "pv275", "hv23", "hv24", "hv25", "hv26", "hv27"), 
                           with = F][ ,(Mean = rowMeans(.SD)), by = vids][,V1]
criteria.background <- coCovar(criteria.background)

## additionally make a list of dyadic ED matrix
evaludative.criteria.diff <- sqrt(outer(criteria.competence, criteria.competence, "-")^2 + 
                                         outer(criteria.background, criteria.background, "-")^2) 
evaludative.criteria.diff <- coDyadCovar(evaludative.criteria.diff)

## KNOWLEDGE AND INTEREST
## knowledge batteries: pv177 pv178 pv179 pv180 pv181 pv182 pv183 pv184 pv185 pv185_1
# there were no measurement in Wave 2 and Wave 3, so we treat the attributes to be invariant across measurement waves
knowledge <- rowSums(dat[vids,.(KN1 = recode(pv177, "2 = 1; else = 0"),
                                KN2 = recode(pv178, "1 = 1; else = 0"),
                                KN3 = recode(pv179, "2 = 1; else = 0"),
                                KN4 = recode(pv180, "2 = 1; else = 0"),
                                KN5 = recode(pv181, "1 = 1; else = 0"),
                                KN6 = recode(pv182, "3 = 1; else = 0"),
                                KN7 = recode(pv183, "4 = 1; else = 0"),
                                KN8 = recode(pv184, "5 = 1; else = 0"), 
                                KN9 = recode(pv185, "2 = 1; else = 0"),
                                KN0 = recode(pv185_1, "4 = 1; else = 0"))])
knowledge <- coCovar(knowledge)

## interest baterries: pv165 & pv166
# there were no measurement in Wave 2 and Wave 3, so we treat the attributes to be invariant across measurement waves
interest <- coCovar(dat[vids, pv165:pv166][, rowMeans(.SD), by = vids][,V1])

## news media use

## remove "NaN" in data -- W1
dat[is.na(pv311), pv311 := 0 ]
dat[is.na(pv313), pv313 := 0 ]
dat[is.na(pv317), pv317 := 0 ]

## add with hours, and creaet index
temp.test <- dat[vids, .(internet.news.use = (60*pv310 + pv311)/60,
                         newspaper.use = (60*pv312 + pv313)/60, 
                         tv.news.use = (60*pv316 + pv317)/60)]

## remove "NaN" in data -- W2
dat[is.na(kv194), kv194 := 0 ]
dat[is.na(kv196), kv196 := 0 ]
dat[is.na(kv200), kv200 := 0 ]

## add with hours, and creaet index
temp.test2 <- dat[vids, .(internet.news.use = (60*kv193 + kv194)/60,
                         newspaper.use = (60*kv195 + kv196)/60, 
                         tv.news.use = (60*kv199 + kv200)/60)]

## remove "NaN" in data -- W3
dat[is.na(hv253), hv253 := 0 ]
dat[is.na(hv255), hv255 := 0 ]
dat[is.na(hv259), hv259 := 0 ]

## add with hours, and creaet index
temp.test3 <- dat[vids, .(internet.news.use = (60*hv252 + hv253)/60,
                         newspaper.use = (60*hv254 + hv255)/60, 
                         tv.news.use = (60*hv258 + hv259)/60)]

media.use.freq <- varCovar(cbind(apply(temp.test, 1, mean),
                                 apply(temp.test2, 1, mean),
                                 apply(temp.test3, 1, mean)))


## offline discussion frequency regarding politics (1 = "never" vs 7 = "always")
talk.freq <- varCovar(cbind(dat[vids, as.numeric(pv322)],
                            dat[vids, as.numeric(kv217)],
                            dat[vids, as.numeric(hv276)]))
 

## political ideology (1 = "extremely liberal" to 7 = "extremely conservative")
pol.ideology <- varCovar(cbind(dat[vids, as.numeric(pv258)],
                               dat[vids, as.numeric(kv49)],
                               dat[vids, as.numeric(hv24)]))

## opinionated personality
opinionated <- varCovar(cbind(dat[vids, as.numeric(pv45)],
                              dat[vids, as.numeric(kv82)],
                              dat[vids, as.numeric(hv137)]))

## submissive personality
submissive <- varCovar(cbind(dat[vids, as.numeric(pv47)],
                             dat[vids, as.numeric(kv84)],
                             dat[vids, as.numeric(hv139)]))

## motivation for using online forum
consistency.motivation <- dat[vids, .(as.numeric(pv18),
                                      as.numeric(pv19),
                                      as.numeric(pv20),
                                      as.numeric(pv21),
                                      as.numeric(pv23),
                                      as.numeric(pv24))]

consistency.motivation <- coCovar(apply(consistency.motivation, 1, mean))
understanding.motivation <- coCovar(apply(dat[vids, pv13:pv16], 1, mean))
hedomic.motivation <- coCovar(apply(dat[vids, pv27:pv29], 1, mean))


## demographics (time-invariant) covariates
  age <- coCovar(dat[vids, as.numeric(age)/10]) ## age in ten years
  gender <- coCovar(dat[vids, as.numeric(sex) - 1]) ## 1 = female, 0 = male
  edu <- coCovar(dat[vids, as.numeric(edu)]) ## education level (1 = "less than elemantry" vs. 9 = "more than postgraduate")


## set dependent variables
  discussion.net <- sienaDependent(g)
  candidate.preference <- sienaDependent(as.matrix(candidate.preference), type="behavior")
  cand.pref.thermo <- sienaDependent(as.matrix(cand.pref.thermo), type="behavior")
  
  mydata <- RSiena::sienaDataCreate(discussion.net, candidate.preference, evaludative.criteria.diff,
                            age, gender, edu,
                            liberal.issue.stance, conserv.issue.stance, 
                            knowledge, interest, media.use.freq, 
                            talk.freq, pol.ideology, opinionated, submissive, consistency.motivation, 
                            understanding.motivation, hedomic.motivation)
  
  save(mydata, file = "RSIENA_test.Rdata")
  
  setwd("~/Dropbox/(17) 2017 Spring/network QAP Korean election")
  load("RSIENA_test.Rdata")
  
## get effect object
myeff <- getEffects(mydata)

# we first produce a data description which is available now:
print01Report(mydata, modelname ="init3.txt")

# then check the range of effect you could include in the model;
effectsDocumentation(myeff) # See Manual section 12 for a detailed information 


## Define the algorithm settings:
myalgorithm <- sienaAlgorithmCreate(useStdInits = F, projname = "test.july05-Mac.txt", 
                                    diagonalize = 0.2, doubleAveraging = 0,
                                    nsub = 5, n2start = 1000,
                                    seed = 43256423)

# add baseline control effects for network evolution: age, gender, edu status, and pol.ideology
myeff <- includeEffects(myeff, RateX, interaction1 = "consistency.motivation", type = "rate")
# myeff <- includeEffects(myeff, effFrom, name = "cand.pref.thermo", interaction1 = "interest")
myeff <- includeEffects(myeff, RateX, interaction1 = "understanding.motivation", type = "rate")
# myeff <- includeEffects(myeff, effFrom, name = "cand.pref.thermo", interaction1 = "edu")

# myeff <- includeEffects(myeff, diffX, interaction1 = "pol.ideology") ## simialr ideology (less difference)

# add some more effects
myeff <- includeEffects(myeff, gwespBB, gwespFF, inPop, outAct)
# myeff <- includeEffects(myeff, nbrDist2, inStructEq) # no. of actors at distance 2: inverse of network closure  # Control for Structural equivalence with respect to indegree 
# myeff <- includeEffects(myeff, cycle3, fix = T, test = T, include = T)

## selection by candidate preference
myeff <- includeEffects(myeff, egoX, altX, sameX,
               interaction1 = "candidate.preference")

myeff <- includeEffects(myeff, egoX, alterX, interaction1 = "consistency.motivation")
myeff <- includeEffects(myeff, egoX, alterX, interaction1 = "understanding.motivation")

myeff <- includeEffects(myeff, egoX, altX, sameX,
                        interaction1 = "gender" )

## influence on candidate preference from network
myeff <- includeEffects(myeff, name = "candidate.preference",
                        totAlt, interaction1 = "discussion.net")
myeff <- includeEffects(myeff, name = "candidate.preference",
                        totAltW, interaction1 = "discussion.net", interaction2 = "evaludative.criteria.diff")


# myeff <- includeEffects(myeff, name = "candidate.preference",
#                         effFrom, interaction1 = "liberal.issue.stance" )
# myeff <- includeEffects(myeff, name = "candidate.preference",
#                         effFrom, interaction1 = "conserv.issue.stance" )
myeff <- includeEffects(myeff, name = "candidate.preference",
                        effFrom, interaction1 = "pol.ideology" )

myeff <- includeEffects(myeff, X, interaction1 = "evaludative.criteria.diff")
myeff <- includeEffects(myeff, X, interaction1 = "policy.pref.diff")
# myeff <- includeEffects(myeff, simX, type = "eval", interaction1 = "pol.ideology")
# myeff <- includeEffects(myeff, altX, type = "eval", interaction1 = "opinionated")
# myeff <- includeEffects(myeff, egoX, type = "eval", interaction1 = "submissive")
# myeff <- includeEffects(myeff, egoX, type = "eval", interaction1 = "consistency.motivation")
# myeff <- includeEffects(myeff, egoX, type = "eval", interaction1 = "understanding.motivation")
# myeff <- includeEffects(myeff, egoX, type = "eval", interaction1 = "hedomic.motivation")


 


## The function siena07 fits the specified model to the data
Model1 <- RSiena::siena07(myalgorithm, data = mydata, effects = myeff, useCluster = T, 
                  nbrNodes = 10, batch = F, clusterType = "FORK", returnDeps = T) 	

## first, we should create some functions to caculate network statistics:
TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
  require(sna)
  require(network)
  x <- networkExtraction(i, data, sims, wave, groupName, varName)
  tc <- sna::triad.census(x)[1,levls]
  # triad names are transferred automatically
  tc
}

GeodesicDistribution <- function (i, data, sims, period, groupName,
                                  varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  require(sna)
  a <- sna::geodist(x)$gdist
  if (cumulative)
  {
    gdi <- sapply(levls, function(i){ sum(a<=i) })
  }
  else
  {
    gdi <- sapply(levls, function(i){ sum(a==i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}

## Then now run gof procedure   
gof1 <- sienaGOF(Model1,verbose=TRUE,varName="discussion.net", IndegreeDistribution)
gof2 <- sienaGOF(Model1,verbose=TRUE,varName="discussion.net", OutdegreeDistribution)
gof3 <- sienaGOF(Model1,verbose=TRUE,varName="discussion.net", TriadCensus)
gof4 <- sienaGOF(Model1,verbose=TRUE,varName="discussion.net", GeodesicDistribution)

plot(gof1)
plot(gof2)
plot(gof3)
plot(gof4)

## gof suggests we need to include indegree and outdegree related effects

# ## using btergm package function
# gof.using.btergm <- btergm::gof(Model1, period = NULL, parallel = "multicore", ncpus = 4, 
#                         statistics = c(esp, deg, ideg, geodesic, rocpr, walktrap.modularity), 
#                         varName = "discussion.net", nsim = 400, verbose = TRUE)
# 
# plot(gof.using.btergm)

myeff <- updateTheta(myeff, Model1)
myeff <- includeEffects(myeff, outPop, outAct, fix=F, test=F, include=T)
myeff <- includeEffects(myeff, gwespFF, fix=F, test=F, include=T)


Model1a <- siena07(myalgorithm, data=mydata, effects=myeff, useCluster=T,
                   nbrNodes = 12, batch=F, returnDeps=T, clusterType = "FORK", prevAns=Model1) 	

## calculate p-value from the estimates and SEs:
options(scipen=999)
siena.table(Model1a, type="html", sig=TRUE)

## add covariate-based social selection main effects:
## race (H1) and gender (H2) homophily
myeff <- updateTheta(myeff,Model1a)


## re-estimate model
Model2 <- siena07(myalgorithm,data=mydata,effects=myeff,useCluster=T,nbrNodes=2,batch=F,returnDeps=T)	
siena.table(Model2, type="html", sig=TRUE)	
