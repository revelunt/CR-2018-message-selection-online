


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

## pre-wave: Nov 13 to Nov 26; Wave 1 survey: Nov 27 to Nov 29, 
## Wave 2 survey: Dec 11th to 13th, and Wave 3 survey: Dec 21th to 23th

g <- list()

net <- read.csv("Dat/Reading_1113-1126_Participants(N=341)_Count(N=160836).csv")
net2 <- read.csv("Dat/Reading_1127-1219_Participants(N=341)_Count(N=160836).csv")
net2 <- data.frame(reading.time = net2$Reading.Time, reader.id = net2$Reader.Id, poster.id = net2$Poster.Id)
net <- rbind(net, net2)
setDT(net)
net[, reading.date := as.Date(reading.time, format = "%Y-%m-%d %H:%M:%S")]

date.range <- unique(sort(net[, reading.date]))

thresholds <- sapply(1:length(date.range), function(i) {
  g[[i]] <- net[reading.date %in% date.range[i],]
  g[[i]] <- data.frame(g[[i]][,2], g[[i]][,3])
  setDT(g[[i]]); g[[i]][, count :=1]
  g[[i]][, sum(count), by = c("poster.id", "reader.id")][poster.id %in% vids & reader.id %in% vids, mean(V1)] 
})


for (i in 1:length(date.range)) {
  
  g[[i]] <- net[reading.date %in% date.range[i],]
  g[[i]] <- data.frame(g[[i]][,2], g[[i]][,3])
  #nodelist <- unique(sort(c(g[[i]]$reader.id, g[[i]]$poster.id)))
  #nodelist <- nodelist[nodelist %in% vids]
  g[[i]] <- graph.data.frame(g[[i]], directed = TRUE, vertices = 1:341)
  g[[i]] <- induced_subgraph(g[[i]], vids = vids)
  g[[i]] <- as.matrix(as_adj(g[[i]]))
  #g[[i]] <- g[[i]][, ]
  #rownames(g[[i]]) <- colnames(g[[i]]) <- nodelist
  #g[[i]] <- sna::event2dichot(g[[i]], method = "absolute", thresh = thresholds[i])
  g[[i]] <- as.network(g[[i]])

##------------------------------------------##
## create a set of covariates for each wave ##
##------------------------------------------##

  if (i <= 7) {
    g[[i]] %v% "candidate.image.Park" <- dat[vids, pv194:pv208][, (Mean = rowMeans(.SD)), by = vids]
    g[[i]] %v% "candidate.image.Moon" <- dat[vids, pv209:pv223][, (Mean = rowMeans(.SD)), by = vids]
    g[[i]] %v% "candidate.preference" <- dat[vids, as.numeric(canpref1)]
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


rm(dat, dat2, consistency.motivation, hedomic.motivation, 
   net, net2, temp, temp.test, understanding.motivation, g_pre, i, lengthn, vids)

evaludative.criteria.sim <- 1 / (1 + sqrt(outer(g[[2]] %v% "criteria.competence", g[[2]] %v% "criteria.competence", "-")^2 + 
                                            outer(g[[2]] %v% "criteria.background", g[[2]] %v% "criteria.background", "-")^2))

policy.pref.sim <- 1 / (1 + sqrt(outer(g[[2]] %v% "liberal.issue.stance", g[[2]] %v% "liberal.issue.stance", "-")^2 + 
                                                outer(g[[2]] %v% "conserv.issue.stance", g[[2]] %v% "conserv.issue.stance", "-")^2))

net <- g[5:27]

final.model.daily <- btergm(net ~ edges + ## intercept
                        
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
                        #gwdsp(decay = 1, fixed = T) + 
                        
                        ## lagged structural, control
                        edgecov("lagged.reciprocity") + 
                        edgecov("lag.transitivity") + ## lagged gwesp_OTP
                        edgecov("lag.cyclic") + ## lagged gwesp_ITP
                        edgecov("lag.shared.activity") + ## lagged gwesp_OSP
                        edgecov("lag.shared.popularity") + ## lagged gwesp_ISP
                        nodeocov("lagged.sender.effect") + 
                        nodeicov("lagged.receiver.effect") + 
                        
                        ## endogenous structural
                        dgwesp(decay = 3, fixed = T, type = "OTP") + ## 3 or 1.5 understanding
                        dgwesp(decay = 3, fixed = T, type = "ITP") + ## 3 or 1.5 understanding
                        dgwesp(decay = 3, fixed = T, type = "OSP") + ## 3 or 1.5 consistency
                        dgwesp(decay = 2, fixed = T, type = "ISP") + ## 3 or 1.5 consistency
                        
                        gwodegree(decay = 2, fixed = T) + ## hedonic
                        gwidegree(decay = 3, fixed = T), ## hedonic 
                      
                      verbose = T, R = 1000, ncpus = 8, parallel = "snow")

save(final.model.daily, file = "final.model.daily.Rdata"); summary(final.model.daily)

gof.fit <- gof(final.model.r, statistics = c(dsp, odeg, ideg, desp_OTP, desp_ITP, desp_OSP, desp_ISP,
                                             geodesic, triad.directed, rocpr, walktrap.modularity),
               nsim = 100, ncpus = 4); plot(gof.fit, mfrow = FALSE)
