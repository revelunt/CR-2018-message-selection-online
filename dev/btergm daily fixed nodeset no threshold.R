


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

net <- g[2:27]

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
                        dgwesp(decay = 3, fixed = T, type = "OTP") + ## 3 or 1.5 understanding
                        dgwesp(decay = 3, fixed = T, type = "ITP") + ## 3 or 1.5 understanding
                        dgwesp(decay = 3, fixed = T, type = "OSP") + ## 3 or 1.5 consistency
                        dgwesp(decay = 2, fixed = T, type = "ISP") + ## 3 or 1.5 consistency
                        
                        gwodegree(decay = 2, fixed = T) + ## hedonic
                        gwidegree(decay = 3, fixed = T), ## hedonic 
                      
                      verbose = T, R = 1000, ncpus = 6, parallel = "snow")

save(final.model.daily, file = "final.model.daily.Rdata"); summary(final.model.daily)

# Estimates and 95% confidence intervals:
#                                        Estimate    2.5%   97.5%
# edges                                 -1.2380323 -1.8028 -0.6396
# nodeicov.age                          -0.0223248 -0.0360 -0.0095
# nodeocov.age                           0.0294965 -0.0022  0.0639
# nodeifactor.gender.1                  -0.0366996 -0.0623 -0.0008
# nodeofactor.gender.1                  -0.0433784 -0.1049 -0.0028
# nodematch.gender                       0.0180177 -0.0025  0.0405
# nodeicov.edu                          -0.0187091 -0.0382 -0.0003
# nodeocov.edu                          -0.0233530 -0.0604  0.0038
# nodeifactor.region_origin2.1          -0.0774311 -0.1068 -0.0584
# nodeofactor.region_origin2.1           0.0982357  0.0322  0.1556
# nodematch.region_origin2               0.0148128 -0.0122  0.0417
# nodeicov.talk.freq                     0.0259718  0.0150  0.0372
# nodeocov.talk.freq                    -0.0126926 -0.0368  0.0120
# nodeicov.media.use.freq               -0.0076965 -0.0138  0.0014
# nodeocov.media.use.freq               -0.0043384 -0.0135  0.0064
# nodeicov.internal.efficacy            -0.0129056 -0.0205 -0.0046
# nodeocov.internal.efficacy             0.0489630  0.0269  0.0778
# nodeifactor.candidate.preference.1    -0.0336298 -0.0533 -0.0044
# nodeofactor.candidate.preference.1    -0.0146417 -0.0549  0.0354
# nodeicov.consistency.motivation        0.0173820  0.0023  0.0365
# nodeocov.consistency.motivation        0.0005258 -0.0311  0.0299
# nodeicov.understanding.motivation     -0.0267010 -0.0464 -0.0092
# nodeocov.understanding.motivation      0.0216079 -0.0162  0.0625
# nodeicov.hedomic.motivation           -0.0059270 -0.0164  0.0042
# nodeocov.hedomic.motivation           -0.0253844 -0.0483 -0.0061
# nodematch.candidate.preference         0.0396690  0.0197  0.0571
# edgecov.policy.pref.sim[[i]]           0.0708359 -0.0116  0.1427
# edgecov.evaludative.criteria.sim[[i]]  0.0938535  0.0174  0.1760
# isolates                               1.3107670  1.0507  1.5637
# mutual                                 0.8480975  0.7593  0.9737
# edgecov.edge.autoregression            0.2214859  0.1708  0.2729
# gwdsp.fixed.1                          0.0022051 -0.0010  0.0051
# edgecov.lagged.reciprocity            -0.0245904 -0.0796  0.0246
# edgecov.lag.transitivity               0.0306166  0.0199  0.0409
# edgecov.lag.cyclic                     0.0020324 -0.0041  0.0094
# edgecov.lag.shared.activity           -0.0278205 -0.0345 -0.0183
# edgecov.lag.shared.popularity         -0.0091721 -0.0159 -0.0010
# nodeocov.lagged.sender.effect          0.0165368  0.0142  0.0187
# nodeicov.lagged.receiver.effect        0.0016719 -0.0001  0.0030
# gwesp.OTP.fixed.3                      0.0826133  0.0661  0.1011
# gwesp.ITP.fixed.3                     -0.0598310 -0.0672 -0.0533
# gwesp.OSP.fixed.3                      0.0166841  0.0082  0.0262
# gwesp.ISP.fixed.2                      0.0810855  0.0590  0.1071
# gwodegree                             -2.8814324 -3.2286 -2.5983
# gwidegree                             -3.7791752 -4.1319 -3.4644

gof.fit <- gof(final.model.daily, statistics = c(dsp, odeg, ideg, desp_OTP, desp_ITP, desp_OSP, desp_ISP,
                                             geodesic, triad.directed, rocpr, walktrap.modularity),
               nsim = 100, ncpus = 4, parallel = "snow"); plot(gof.fit, mfrow = FALSE)
plot(gof.fit)

date <- character(0)
  for (i in (1:length(ep$t))) {
    k <- ep$t[i]
    date[i] <- date.range.labels[k]
  }
ep$date <- date

date.range.labels <- substr(date.range.labels, 6, 10)
date.range.labels <- sub("^11-", "Nov-", date.range.labels)
date.range.labels <- sub("^12-", "Dec-", date.range.labels)

ggplot(data = ep, aes(x = t, y = probability, colour = factor(nodematch.candidate.preference))) + theme_bw() + 
  scale_colour_grey(start = 0.8, end = 0.2, guide = guide_legend(reverse=TRUE)) + 
  #theme(legend.justification=c(1,0), legend.position=c(0.9,0.1), axis.text.x = element_text(face = "bold", angle = 45)) +
  theme(axis.text.x = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(1,26,1), labels = date.range.labels) + 
  geom_line(stat = "summary", fun.y = mean, size = 1.2) + 
  # stat_summary(geom = "ribbon", alpha = 0.1, #fill = interaction(factor(nodematch.candidate.preference)),
  #             fun.ymin = function(z) quantile(z, 0.025),
  #             fun.ymax = function(z) quantile(z, 0.975)) + 
  labs(colour = "Candidate preference")

ggplot(data = ep, aes(x = t, y = probability, colour = factor(nodematch.candidate.preference))) + theme_bw() + 
  scale_colour_grey(start = 0.8, end = 0.2, guide = guide_legend(reverse=TRUE)) + 
  theme(legend.justification=c(1,0), legend.position=c(0.9,0.1), axis.text.x = element_text(face = "bold", angle = 45)) +
  scale_x_continuous(breaks = seq(1,26,1), labels = date.range.labels) + 
  #stat_smooth(method = "auto", fullrange = TRUE) + labs(colour = "Candidate preference")
  geom_line(stat = "summary", fun.y = "median", size = 1.5)
