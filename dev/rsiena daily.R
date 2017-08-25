


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

## remove "NaN" in data -- W1
dat[is.na(pv311), pv311 := 0 ]
dat[is.na(pv313), pv313 := 0 ]
dat[is.na(pv317), pv317 := 0 ]
## remove "NaN" in data -- W2
dat[is.na(kv194), kv194 := 0 ]
dat[is.na(kv196), kv196 := 0 ]
dat[is.na(kv200), kv200 := 0 ]
## remove "NaN" in data -- W3
dat[is.na(hv253), hv253 := 0 ]
dat[is.na(hv255), hv255 := 0 ]
dat[is.na(hv259), hv259 := 0 ]

## motivation for using online forum
consistency.motivation <- dat[vids, .(as.numeric(pv18),
                                      as.numeric(pv19),
                                      as.numeric(pv20),
                                      as.numeric(pv21),
                                      as.numeric(pv23),
                                      as.numeric(pv24))]
understanding.motivation <- dat[vids, pv13:pv16]
hedomic.motivation <- dat[vids, pv27:pv29]

##---------------------------------##
## create a dependent network list ##
##---------------------------------##

net <- read.csv("Dat/Reading_1113-1126_Participants(N=341)_Count(N=160836).csv")
net2 <- read.csv("Dat/Reading_1127-1219_Participants(N=341)_Count(N=160836).csv")
net2 <- data.frame(reading.time = net2$Reading.Time, reader.id = net2$Reader.Id, poster.id = net2$Poster.Id)
net <- rbind(net, net2)
setDT(net)
net[, reading.date := as.Date(reading.time, format = "%Y-%m-%d %H:%M:%S")]

date.range <- unique(sort(net[, reading.date]))

g <- array(NA, dim = c(lengthn, lengthn, length(date.range)))

for (i in 1:length(date.range)) {
  
  temp <- net[reading.date %in% date.range[i],]
  temp <- data.frame(temp[,2], temp[,3])
  #nodelist <- unique(sort(c(g[[i]]$reader.id, g[[i]]$poster.id)))
  #nodelist <- nodelist[nodelist %in% vids]
  temp <- graph.data.frame(temp, directed = TRUE, vertices = 1:341)
  temp <- induced_subgraph(temp, vids = vids)
  temp <- as.matrix(as_adj(temp))
  temp <- event2dichot(temp, thresh = gden(temp))
  g[,,i] <- temp
}
  
##------------------------------------------##
## create a set of covariates for each wave ##
##------------------------------------------##

require(RSiena)

## define helper function
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

## candidate image Park
candidate.image.Park <- cbind(
  rep.col(dat[vids, pv194:pv208][, (Mean = rowMeans(.SD)), by = vids][,V1], 7),
  rep.col(dat[vids, kv7:kv21][, (Mean = rowMeans(.SD)), by = vids][,V1], 14),
  rep.col(dat[vids, hv32:hv46][, (Mean = rowMeans(.SD)), by = vids][,V1], 6)
)

## candidate image Moon
candidate.image.Moon <- cbind(
  rep.col(dat[vids, pv209:pv223][, (Mean = rowMeans(.SD)), by = vids][,V1], 7),
  rep.col(dat[vids, kv22:kv36][, (Mean = rowMeans(.SD)), by = vids][,V1], 14),
  rep.col(dat[vids, hv47:hv61][, (Mean = rowMeans(.SD)), by = vids][,V1], 6)
)

candidate.image.Park <- varCovar(candidate.image.Park)
candidate.image.Moon <- varCovar(candidate.image.Moon)

## candidate.preference
candidate.preference <- cbind(
  rep.col(dat[vids, as.numeric(canpref1)], 7),
  rep.col(dat[vids, as.numeric(canpref2)], 14),
  rep.col(dat[vids, as.numeric(canpref3)], 6)
)

candidate.preference <- varCovar(candidate.preference)

## CANDIDATE PREFERENCE BASED ON FEELING THERMOMETER (Park minus Moon)
cand.pref.thermo <- cbind(
  rep.col(dat[vids, as.numeric(pv254 - pv255)], 7), 
  rep.col(dat[vids, as.numeric(kv37 - kv38)], 14),
  rep.col(dat[vids, as.numeric(hv62 - hv63)], 6)
)
cand.pref.thermo <- (100 + cand.pref.thermo)/2
cand.pref.thermo <- varCovar(cand.pref.thermo)

## issue preference
## pv299 / kv39 / hv84: prefer big goverment and regulations
## pv303 / kv43 / hv88: prefer soft diplomatic stance with North Korea

## pv300 / kv40 / hv85: prefer small goverment and less regulations
## pv304 / kv44 / hv89: prefer hard solution with North Korea

liberal.issue.stance <- cbind(
  rep.col(apply(dat[vids, c("pv299", "pv303"), with = F], 1, mean), 7),
  rep.col(apply(dat[vids, c("kv39", "kv43"), with = F], 1, mean), 14),
  rep.col(apply(dat[vids, c("hv84", "hv88"), with = F], 1, mean), 6)
)

conserv.issue.stance <- cbind(
  rep.col(apply(dat[vids, c("pv300", "pv304"), with = F], 1, mean), 7),
  rep.col(apply(dat[vids, c("kv40", "kv44"), with = F], 1, mean), 14),
  rep.col(apply(dat[vids, c("hv85", "hv89"), with = F], 1, mean), 6)
)

liberal.issue.stance <- varCovar(liberal.issue.stance)
conserv.issue.stance <- varCovar(conserv.issue.stance)


## CRITERIA IMPORTANCE
# criteria importance - competence (1 = "not at all important" vs 7 = "extremely important")
# there were no measurement in Wave2, so we average W1 and W3 measurements
# and treat the attributes to be invariant across measurement waves
criteria.competence <- dat[vids, c("pv267", "pv268", "pv269", "pv270", "hv19", "hv20", "hv21", "hv22"), 
                           with = F][ , (Mean = rowMeans(.SD)), by = vids][,V1]
criteria.competence <- coCovar(criteria.competence)

# criteria importance - background (1 = "not at all important" vs 7 = "extremely important")
# there were no measurement in Wave2, so we average W1 and W3 measurements
# and treat the attributes to be invariant across measurement waves
criteria.background <- dat[vids, c("pv271", "pv272", "pv273", "pv274", "pv275", "hv23", "hv24", "hv25", "hv26", "hv27"), 
                           with = F][ , (Mean = rowMeans(.SD)), by = vids][,V1]
criteria.background <- coCovar(criteria.background)


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

mydata <- RSiena::sienaDataCreate(discussion.net, cand.pref.thermo,
                                  candidate.image.Park, candidate.image.Moon, 
                                  liberal.issue.stance, conserv.issue.stance, 
                                  criteria.competence, criteria.background, 
                                  knowledge, interest, age, gender, edu,
                                  consistency.motivation, understanding.motivation, hedomic.motivation)



## get effect object
myeff <- getEffects(mydata)

# we first produce a data description which is available now:
print01Report(mydata, modelname ="init.txt")

# then check the range of effect you could include in the model;
effectsDocumentation(myeff) # See Manual section 12 for a detailed information 


## Define the algorithm settings:
myalgorithm <- sienaAlgorithmCreate(useStdInits = F, projname = "test.txt", 
                                    diagonalize = 0.2, doubleAveraging = 0,
                                    seed = 43256423)

myeff <- includeEffects(myeff, gwespBB, gwespFB, gwespBF, inPop, outAct)
myeff <- includeEffects(myeff, egoX, altX, simX,
                        interaction1 = "cand.pref.thermo")
myeff <- includeEffects(myeff, name = "cand.pref.thermo",
                        avAlt, interaction1 = "discussion.net")

model1 <- RSiena::siena07(myalgorithm, data = mydata, effects = myeff, useCluster = T, 
                          nbrNodes = 10, batch = F, clusterType = "FORK", returnDeps = T) 	








  if (i <= 7) {


    g[[i]] %v% "liberal.issue.stance" <- apply(dat[vids, c("pv299", "pv303"), with = F], 1, mean)
    g[[i]] %v% "conserv.issue.stance" <- apply(dat[vids, c("pv300", "pv304"), with = F], 1, mean)
    temp.test <- dat[vids, .(internet.news.use = (60*pv310 + pv311)/60,
                             newspaper.use = (60*pv312 + pv313)/60, 
                             tv.news.use = (60*pv316 + pv317)/60)]
    g[[i]] %v% "media.use.freq" <- apply(temp.test, 1, mean)
    g[[i]] %v% "talk.freq" <- dat[vids, as.numeric(pv322)]
  } else if (i <= 21) {
    g[[i]] %v% "candidate.image.Park" <- dat[vids, kv7:kv21][, (Mean = rowMeans(.SD)), by = vids][,V1]
    g[[i]] %v% "candidate.image.Moon" <- dat[vids, kv22:kv36][, (Mean = rowMeans(.SD)), by = vids][,V1]
    g[[i]] %v% "candidate.preference" <- dat[vids, as.numeric(canpref2)]
    g[[i]] %v% "liberal.issue.stance" <- apply(dat[vids, c("kv39", "kv43"), with = F], 1, mean)
    g[[i]] %v% "conserv.issue.stance" <- apply(dat[vids, c("kv40", "kv44"), with = F], 1, mean)
    temp.test <- dat[vids, .(internet.news.use = (60*kv193 + kv194)/60,
                             newspaper.use = (60*kv195 + kv196)/60, 
                             tv.news.use = (60*kv199 + kv200)/60)]
    g[[i]] %v% "media.use.freq" <- apply(temp.test, 1, mean)
    g[[i]] %v% "talk.freq" <- dat[vids, as.numeric(kv217)]
  } else {
    g[[i]] %v% "candidate.image.Park" <- dat[vids, hv32:hv46][, (Mean = rowMeans(.SD)), by = vids][,V1]
    g[[i]] %v% "candidate.image.Moon" <- dat[vids, hv47:hv61][, (Mean = rowMeans(.SD)), by = vids][,V1]
    g[[i]] %v% "candidate.preference" <- dat[vids, as.numeric(canpref3)]
    g[[i]] %v% "liberal.issue.stance" <- apply(dat[vids, c("hv84", "hv88"), with = F], 1, mean)
    g[[i]] %v% "conserv.issue.stance" <- apply(dat[vids, c("hv85", "hv89"), with = F], 1, mean)
    temp.test <- dat[vids, .(internet.news.use = (60*hv252 + hv253)/60,
                             newspaper.use = (60*hv254 + hv255)/60, 
                             tv.news.use = (60*hv258 + hv259)/60)]
    g[[i]] %v% "media.use.freq" <- apply(temp.test, 1, mean)
    g[[i]] %v% "talk.freq" <- dat[vids, as.numeric(hv276)]
  }
  
  g[[i]] %v% "criteria.competence" <- dat[vids, c("pv267", "pv268", "pv269", "pv270", 
                                     "hv19", "hv20", "hv21", "hv22"), with = F][ , (Mean = rowMeans(.SD)), by = vids][,V1]
  g[[i]] %v% "criteria.background" <- dat[vids, c("pv271", "pv272", "pv273", "pv274", 
                                     "pv275", "hv23", "hv24", "hv25", "hv26", "hv27"), with = F][ , (Mean = rowMeans(.SD)), by = vids][,V1]

  g[[i]] %v% "knowledge" <- rowSums(dat[vids, .(KN1 = recode(pv177, "2 = 1; else = 0"),
                                                KN2 = recode(pv178, "1 = 1; else = 0"),
                                                KN3 = recode(pv179, "2 = 1; else = 0"),
                                                KN4 = recode(pv180, "2 = 1; else = 0"),
                                                KN5 = recode(pv181, "1 = 1; else = 0"),
                                                KN6 = recode(pv182, "3 = 1; else = 0"),
                                                KN7 = recode(pv183, "4 = 1; else = 0"),
                                                KN8 = recode(pv184, "5 = 1; else = 0"), 
                                                KN9 = recode(pv185, "2 = 1; else = 0"),
                                                KN0 = recode(pv185_1, "4 = 1; else = 0"))])
  
  g[[i]] %v% "interest" <- dat[vids, pv165:pv166][, rowMeans(.SD), by = vids][,V1] 
  g[[i]] %v% "expertise" <- apply(cbind(scale(g[[i]] %v% "knowledge", scale = T), scale(g[[i]] %v% "interest", scale = T)), 1, mean)
  g[[i]] %v% "internal.efficacy" <- dat[vids, pv126:pv129][, rowMeans(.SD), by = vids][,V1]
  g[[i]] %v% "consistency.motivation" <- apply(consistency.motivation, 1, mean)
  g[[i]] %v% "understanding.motivation" <- apply(understanding.motivation, 1, mean)
  g[[i]] %v% "hedomic.motivation" <- apply(hedomic.motivation, 1, mean)
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
  
  if (i == 1) { 
    
    g[[i]] %v% "lagged.sender.effect" <- rowSums(as.matrix(g[[i]]))
    g[[i]] %v% "lagged.receiver.effect" <- colSums(as.matrix(g[[i]]))
    g[[i]] %n% "edge.autoregression" <- as.matrix(g[[i]])
    g[[i]] %n% "lagged.reciprocity" <- as.matrix(g[[i]])
    g[[i]] %n% "lag.transitivity" <- as.matrix(g[[i]])
    g[[i]] %n% "lag.cyclic" <- as.matrix(g[[i]])
    g[[i]] %n% "lag.shared.activity" <- as.matrix(g[[i]])
    g[[i]] %n% "lag.shared.popularity" <- as.matrix(g[[i]])
    
  } else {
    temp <- as.matrix(g[[(i-1)]])
    row.sum <- rowSums(as.matrix(temp))
    row.sum[is.na(row.sum)] <- 0
    col.sum <- rowSums(as.matrix(temp))
    col.sum[is.na(col.sum)] <- 0
    g[[i]] %v% "lagged.sender.effect" <- row.sum
    g[[i]] %v% "lagged.receiver.effect" <- col.sum
    
    g[[i]] %n% "edge.autoregression" <- temp
    g[[i]] %n% "lagged.reciprocity" <- t(temp)
    g[[i]] %n% "lag.transitivity" <- temp %*% temp
    g[[i]] %n% "lag.cyclic" <- t(temp) %*% t(temp)
    g[[i]] %n% "lag.shared.activity" <- temp %*% t(temp)
    g[[i]] %n% "lag.shared.popularity" <- t(temp) %*% temp
    }

}


require(RSiena)

candidate.image.Park <- sapply(g, function(i) i %v% "candidate.image.Park", simplify = TRUE)

























rm(dat, dat2, consistency.motivation, hedomic.motivation, 
   net, net2, temp, temp.test, understanding.motivation, g_pre, i, lengthn, vids)

evaludative.criteria.sim <- 1 / (1 + sqrt(outer(g[[2]] %v% "criteria.competence", g[[2]] %v% "criteria.competence", "-")^2 + 
                                            outer(g[[2]] %v% "criteria.background", g[[2]] %v% "criteria.background", "-")^2))

policy.pref.sim <- 1 / (1 + sqrt(outer(g[[2]] %v% "liberal.issue.stance", g[[2]] %v% "liberal.issue.stance", "-")^2 + 
                                                outer(g[[2]] %v% "conserv.issue.stance", g[[2]] %v% "conserv.issue.stance", "-")^2))

net <- g[2:27]

final.model.daily.r <- btergm(net ~ edges + ## intercept
                        
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
                        #nodeifactor("candidate.preference") + nodeofactor("candidate.preference") + 
                        
                        ## individual, motivation factor
                        nodeicov("consistency.motivation") + nodeocov("consistency.motivation") + 
                        nodeicov("understanding.motivation") + nodeocov("understanding.motivation") + 
                        nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") + 
                        
                        ## dyadic, consistency
                        nodematch("candidate.preference", diff = T) + 
                        edgecov(policy.pref.sim) +
                        
                        ## dyadic, understanding
                        edgecov(evaludative.criteria.sim) +
                        
                        ## endogenous and lagged structural, control
                        isolates + 
                        mutual + 
                        edgecov("edge.autoregression") + 
                        gwdsp(decay = 1, fixed = T) + 
                        
                        ## lagged structural, control
                        edgecov("lagged.reciprocity") + 
                        edgecov("lag.transitivity") + ## lagged gwesp_OTP
                        edgecov("lag.cyclic") + ## lagged gwesp_ITP
                        edgecov("lag.shared.activity") + ## lagged gwesp_OSP
                        edgecov("lag.shared.popularity") + ## lagged gwesp_ISP
                        nodeocov("lagged.sender.effect") + 
                        nodeicov("lagged.receiver.effect") + 
                        
                        ## endogenous structural
                          dgwesp(decay = 2, fixed = T, type = "OTP") + 
                          dgwesp(decay = 2, fixed = T, type = "ITP") + 
                          dgwesp(decay = 2, fixed = T, type = "OSP") + 
                          dgwesp(decay = 2, fixed = T, type = "ISP") + 
                          
                          gwodegree(decay = 1.5, fixed = T) + 
                          gwidegree(decay = 2, fixed = T),
                      
                      verbose = T, R = 1000, ncpus = 6, parallel = "snow")

save(final.model.daily.r, file = "final.model.daily.r.Rdata"); summary(final.model.daily.r)


        
#                                         Estimate    2.5%   97.5%
# edges                                 -1.9827692 -2.7032 -0.9338
# nodeicov.age                           0.0164424 -0.0043  0.0331
# nodeocov.age                           0.0721016  0.0228  0.1214
# nodeifactor.gender.1                  -0.1005868 -0.1412 -0.0476
# nodeofactor.gender.1                   0.0707016 -0.0309  0.1831
# nodematch.gender                       0.0256096 -0.0135  0.0714
# nodeicov.edu                          -0.0441187 -0.0704 -0.0170
# nodeocov.edu                          -0.0598658 -0.1224  0.0039
# nodeifactor.region_origin2.1          -0.0753013 -0.1212 -0.0279
# nodeofactor.region_origin2.1           0.0166835 -0.0940  0.1170
# nodematch.region_origin2               0.0193086 -0.0229  0.0555
# nodeicov.talk.freq                     0.0554640  0.0276  0.0732
# nodeocov.talk.freq                    -0.0367416 -0.0867  0.0064
# nodeicov.media.use.freq               -0.0138156 -0.0296  0.0045
# nodeocov.media.use.freq                0.0122535 -0.0106  0.0381
# nodeicov.internal.efficacy            -0.0393500 -0.0560 -0.0229
# nodeocov.internal.efficacy             0.0863540  0.0327  0.1437
# nodeifactor.candidate.preference.1     0.0024812 -0.0445  0.0426
# nodeofactor.candidate.preference.1     0.0950211  0.0054  0.1944
# nodeicov.consistency.motivation        0.0542013  0.0269  0.0823
# nodeocov.consistency.motivation        0.0336518 -0.0094  0.0731
# nodeicov.understanding.motivation     -0.0216884 -0.0542  0.0116
# nodeocov.understanding.motivation      0.0478254 -0.0068  0.1053
# nodeicov.hedomic.motivation            0.0054623 -0.0106  0.0214
# nodeocov.hedomic.motivation           -0.0272339 -0.0722  0.0167
# nodematch.candidate.preference         0.0086403 -0.0297  0.0497
# edgecov.policy.pref.sim[[i]]          -0.0187218 -0.1603  0.1169
# edgecov.evaludative.criteria.sim[[i]]  0.2288562  0.0492  0.3914
# isolates                               1.2605245  0.9949  1.5017
# mutual                                 0.7666475  0.6283  0.8925
# edgecov.edge.autoregression            0.3682703  0.2593  0.4866
# gwdsp.fixed.1                          0.0079292 -0.0002  0.0141
# edgecov.lagged.reciprocity             0.0332737 -0.1507  0.2393
# edgecov.lag.transitivity               0.0664708  0.0458  0.0924
# edgecov.lag.cyclic                     0.0098839 -0.0253  0.0473
# edgecov.lag.shared.activity           -0.0584387 -0.0762 -0.0438
# edgecov.lag.shared.popularity         -0.0406270 -0.0604 -0.0205
# nodeocov.lagged.sender.effect          0.0265771  0.0217  0.0324
# nodeicov.lagged.receiver.effect        0.0005426 -0.0044  0.0062
# gwesp.OTP.fixed.2                      0.1782701  0.0922  0.2283
# gwesp.ITP.fixed.2                     -0.0865717 -0.1063 -0.0639
# gwesp.OSP.fixed.2                      0.0527281  0.0009  0.1256
# gwesp.ISP.fixed.2                      0.0312077 -0.0013  0.0765
# gwodegree                             -3.3884909 -3.9281 -2.9586
# gwidegree                             -3.1833144 -3.7842 -2.7307         
         
gof.fit.r <- gof(final.model.daily.r, statistics = c(dsp, odeg, ideg, esp,
                                                     triad.directed, geodesic, rocpr, walktrap.modularity),
                 MCMC.burnin = 20000, nsim = 100, parallel = "snow", ncpus = 4); plot(gof.fit.r, mfrow = FALSE)
plot(gof.fit.r)

## comparison with the main model (vs daily model)
load("~/Dropbox/GitHub/Korean2012ElectionProject/R_results/btergm.results.Aug 2nd.Rdata")
screenreg(list(final.model, final.model.daily.r), single.row = T, digits = 3, leading.zero = F,
          custom.model.names = c("Main reported model", "Daily Model"),
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
                                "Multiple path closure (GWESP-OTP, 3/2)", "Multiple cyclic closure (GWESP-ITP, 3/2)", 
                                "Multiple activity closure (GWESP-OSP, 3/2)", "Multiple popularity closure (GWESP-ISP, 2)",
                                "Activity spread (GW-outdegree, 2/1.5)", "Popularity spread (GW-indegree, 3/2)",
                                "Previous communication", "Delayed reciprocity", 
                                "Delayed transitivity closure", "Delayed cyclic closure", 
                                "Delayed activity closure", "Delayed popularity closure",
                                "Multiple path closure (GWESP-OTP, 3/2)", "Multiple cyclic closure (GWESP-ITP, 3/2)", 
                                "Multiple activity closure (GWESP-OSP, 3/2)"))

# =========================================================================================================
#                                                   Main reported model                 Daily Model                   
# ---------------------------------------------------------------------------------------------------------
# Edges (Intercept)                               -1.890 [-2.932;  -.304] *       -1.983 [-2.703;  -.934] *
# Age (in-ties)                                     .001 [ -.020;   .035]           .016 [ -.004;   .033]  
# Age (out-ties)                                    .052 [ -.192;   .093]           .072 [  .023;   .121] *
# Female (in-ties)                                  .005 [ -.036;   .071]          -.101 [ -.141;  -.048] *
# Female (out-ties)                                 .014 [ -.348;   .335]           .071 [ -.031;   .183]  
# Gender homophily                                  .044 [  .015;   .086] *         .026 [ -.013;   .071]  
# Education (in-ties)                              -.011 [ -.039;   .019]          -.044 [ -.070;  -.017] *
# Education (out-ties)                              .016 [ -.015;   .091]          -.060 [ -.122;   .004]  
# Regional origin = Seoul (in-ties)                -.084 [ -.157;   .044]          -.075 [ -.121;  -.028] *
# Regional origin = Seoul (out-ties)               -.125 [ -.598;   .350]           .017 [ -.094;   .117]  
# Regional homophily (Seoul)                        .017 [ -.014;   .080]           .019 [ -.023;   .056]  
# Talk freq (in-ties)                               .046 [  .002;   .049] *         .055 [  .028;   .073] *
# Talk freq (out-ties)                              .014 [ -.143;   .161]          -.037 [ -.087;   .006]  
# Media use (in-ties)                              -.011 [ -.019;   .024]          -.014 [ -.030;   .004]  
# Media use (out-ties)                              .033 [ -.017;   .287]           .012 [ -.011;   .038]  
# Internal efficacy (in-ties)                      -.013 [ -.058;   .055]          -.039 [ -.056;  -.023] *
# Internal efficacy (out-ties)                      .024 [ -.102;   .128]           .086 [  .033;   .144] *
# Candidate pref = Moon (in-ties)                   .003 [ -.008;   .092]           .002 [ -.045;   .043]  
# Candidate pref = Moon (out-ties)                  .013 [ -.123;   .131]           .095 [  .005;   .194] *
# Consistency motivation (in-ties)                  .034 [ -.021;   .113]           .054 [  .027;   .082] *
# Consistency motivation (out-ties)                 .025 [ -.112;   .077]           .034 [ -.009;   .073]  
# Understanding motivation (in-ties)               -.052 [ -.103;   .022]          -.022 [ -.054;   .012]  
# Understanding motivation (out-ties)               .028 [  .005;   .087] *         .048 [ -.007;   .105]  
# Hedonic motivation (in-ties)                     -.012 [ -.038;   .001]           .005 [ -.011;   .021]  
# Hedonic motivation (out-ties)                     .102 [  .087;   .133] *        -.027 [ -.072;   .017]  
# Same candidate pref                              -.032 [ -.079;   .047]           .009 [ -.030;   .050]  
# Similar policy pref                              -.108 [ -.212;   .042]          -.019 [ -.160;   .117]  
# Similar evaluative criteria                       .407 [  .207;   .415] *         .229 [  .049;   .391] *
# Isolates                                         1.019 [  .793;  1.264] *        1.261 [  .995;  1.502] *
# Reciprocity                                       .769 [  .507;  1.068] *         .767 [  .628;   .892] *
# Previous communication                            .222 [  .192;   .253] *         .368 [  .259;   .487] *
# Multiple two-paths (GWDSP, 1)                     .003 [ -.007;   .009]           .008 [ -.000;   .014]  
# Delayed reciprocity                               .074 [ -.073;   .344]           .033 [ -.151;   .239]  
# Delayed transitivity closure                      .034 [  .020;   .055] *         .066 [  .046;   .092] *
# Delayed cyclic closure                            .034 [  .008;   .057] *         .010 [ -.025;   .047]  
# Delayed activity closure                         -.056 [ -.067;  -.035] *        -.058 [ -.076;  -.044] *
# Delayed popularity closure                       -.059 [ -.110;  -.034] *        -.041 [ -.060;  -.021] *
# Persistent sender (out-tie)                       .019 [  .010;   .029] *         .027 [  .022;   .032] *
# Persistent receiver (in-ties)                     .023 [  .018;   .038] *         .001 [ -.004;   .006]  
# Multiple path closure (GWESP-OTP, 3/2)            .058 [ -.053;   .125]           .178 [  .092;   .228] *
# Multiple cyclic closure (GWESP-ITP, 3/2)         -.066 [ -.080;  -.060] *        -.087 [ -.106;  -.064] *
# Multiple activity closure (GWESP-OSP, 3/2)        .036 [  .033;   .053] *         .053 [  .001;   .126] *
# Multiple popularity closure (GWESP-ISP, 2)        .115 [  .082;   .232] *         .031 [ -.001;   .077]  
# Activity spread (GW-outdegree, 2/1.5)           -4.350 [-4.557; -3.994] *       -3.388 [-3.928; -2.959] *
# Popularity spread (GW-indegree, 3/2)            -4.049 [-5.342; -3.259] *       -3.183 [-3.784; -2.731] *
#   ---------------------------------------------------------------------------------------------------------
#   Num. obs.                                   291096                         2522832                       
# =========================================================================================================
#  * 0 outside the confidence interval


## predicted prob
ep <- edgeprob(final.model.daily.r)
for (i in 1:ncol(ep)) {
  if (grepl("((edge)|(dyad))cov", colnames(ep)[i])) {
    colnames(ep)[i] <- substr(colnames(ep)[i], 1, nchar(colnames(ep)[i]) - 5)
  }
}

date.range.labels <- date.range[-1]
date.range.labels <- sapply(date.range.labels, function(i) {
  i <- gsub("2012-", "", i)
  i <- sub("^11-", "Nov-", i)
  i <- sub("^12-", "Dec-", i)
  i
})


ggplot(data = ep, aes(x = t, y = probability, fill = factor(nodematch.candidate.preference))) + theme_bw() + 
  scale_colour_grey(start = 0.8, end = 0.2, guide = guide_legend(reverse=TRUE)) + 
  #theme(legend.justification=c(1,0), legend.position=c(0.9,0.1), axis.text.x = element_text(face = "bold", angle = 45)) +
  theme(axis.text.x = element_text(face = "bold", angle = 45)) +
  scale_x_continuous(breaks = seq(1,26,1), labels = date.range.labels) + 
  stat_smooth(fullrange = TRUE) + labs(colour = "Candidate preference")

ggplot(data = ep, aes(x = t, y = probability, colour = factor(nodematch.candidate.preference))) + theme_bw() + 
  scale_colour_grey(start = 0.8, end = 0.2, guide = guide_legend(reverse=TRUE)) + 
  theme(legend.justification=c(1,0), legend.position=c(0.9,0.1), axis.text.x = element_text(face = "bold", angle = 45)) +
  scale_x_continuous(breaks = seq(1,26,1), labels = date.range.labels) + 
  #stat_smooth(method = "auto", fullrange = TRUE) + labs(colour = "Candidate preference")
  geom_line(stat = "summary", fun.y = "mean", size = 1.5)


## possible interaction model?
same_candidate_preference  <- lapply(1:length(net), function(i) {
  temp1.t <- ergmMPLE(net[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

time_trends <- lapply(1:length(net), function(i) {
  dim <- dim(as.matrix(net[[i]]))
  outmat <- matrix(i - 1, nrow = dim[1], ncol = dim[2])
  diag(outmat) <- 0
  outmat
  })

debate <- lapply(1:length(net), function(i) {
  dim <- dim(as.matrix(net[[i]]))
  if (i %in% c(11,17,23)) {
    outmat <- matrix(1, nrow = dim[1], ncol = dim[2])
  } else {
    outmat <- matrix(0, nrow = dim[1], ncol = dim[2])
  }
  diag(outmat) <- 0
  outmat
})

time.X.same.candidate.preference <- lapply(1:length(net), function(i) {
  outmat <- time_trends[[i]] * same_candidate_preference[[i]]
  diag(outmat) <- 0
  outmat
})

debate.X.same.candidate.preference <- lapply(1:length(net), function(i) {
  outmat <- debate[[i]] * same_candidate_preference[[i]]
  diag(outmat) <- 0
  outmat
})

final.model.daily.r2 <- btergm(net ~ edges + ## intercept
                                
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
                                nodematch("candidate.preference", diff = F) + 
                                edgecov(policy.pref.sim) +
                                
                                ## dyadic, understanding
                                edgecov(evaludative.criteria.sim) +
                                
                                ## endogenous and lagged structural, control
                                isolates + 
                                mutual + 
                                edgecov("edge.autoregression") + 
                                gwdsp(decay = 1, fixed = T) + 
                                
                                ## lagged structural, control
                                edgecov("lagged.reciprocity") + 
                                edgecov("lag.transitivity") + ## lagged gwesp_OTP
                                edgecov("lag.cyclic") + ## lagged gwesp_ITP
                                edgecov("lag.shared.activity") + ## lagged gwesp_OSP
                                edgecov("lag.shared.popularity") + ## lagged gwesp_ISP
                                nodeocov("lagged.sender.effect") + 
                                nodeicov("lagged.receiver.effect") + 
                                
                                ## endogenous structural
                                dgwesp(decay = 2, fixed = T, type = "OTP") + 
                                dgwesp(decay = 2, fixed = T, type = "ITP") + 
                                dgwesp(decay = 2, fixed = T, type = "OSP") + 
                                dgwesp(decay = 2, fixed = T, type = "ISP") + 
                                
                                gwodegree(decay = 1.5, fixed = T) + 
                                gwidegree(decay = 2, fixed = T) + 
                                 
                                ## interaction
                                edgecov(time_trends) +
                                #edgecov(debate) + 
                                edgecov(time.X.same.candidate.preference)  
                                #edgecov(debate.X.same.candidate.preference)
                                 ,
                              
                              verbose = T, R = 1000, ncpus = 6, parallel = "snow")

## appears to be not significant, although trending to a right direction

# nodematch.candidate.preference                -0.04584520 -0.1246  0.0243
# edgecov.time_trends[[i]]                      -0.00093223 -0.0162  0.0137
# edgecov.time.X.same.candidate.preference[[i]]  0.00433447 -0.0008  0.0101

