
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
g[[1]][, sum(count), by = c("poster.id", "reader.id")][, mean(V1)] 
g[[1]] <- graph.data.frame(g[[1]], directed = TRUE, vertices = 1:341)
g[[1]] <- induced_subgraph(g[[1]], vids = vids)
g[[1]] <- as.matrix(as_adj(g[[1]]))

## Wave 2 network from 12/11 to 12/13 (survey administered at Dec 11th to 13th?)
g[[2]] <- net[reading.date %between% c("2012-12-11", "2012-12-13"),]
g[[2]] <- data.frame(g[[2]][,2], g[[2]][,3]) ## reader: source, poster: target
setDT(g[[2]]); g[[2]][, count :=1]
g[[2]][, sum(count), by = c("poster.id", "reader.id")][, mean(V1)] 
g[[2]] <- induced_subgraph(graph.data.frame(g[[2]], directed = TRUE, vertices = 1:341), vids = vids)
g[[2]] <- as.matrix(as_adj(g[[2]]))

## Wave 3 network from 12/17 to 12/19 (*** survey administered at Dec 21th to 23th)
g[[3]] <- net[reading.date %between% c("2012-12-17", "2012-12-19"),]
g[[3]] <- data.frame(g[[3]][,2], g[[3]][,3]) ## reader: source, poster: target
setDT(g[[3]]); g[[3]][, count :=1]
g[[3]][, sum(count), by = c("poster.id", "reader.id")][, mean(V1)] 
g[[3]] <- induced_subgraph(graph.data.frame(g[[3]], directed = TRUE, vertices = 1:341), vids = vids)
g[[3]] <- as.matrix(as_adj(g[[3]]))


## previous communication patterns
## Pre-wave (Wave 0) network from Nov 23 to Nov 26
g_pre[[1]] <- net[reading.date %between% c("2012-11-23", "2012-11-26"),]
g_pre[[1]] <- data.frame(g_pre[[1]][,2], g_pre[[1]][,3]) ## reader: source, poster: target
setDT(g_pre[[1]]); g_pre[[1]][, count :=1]
g_pre[[1]][, sum(count), by = c("poster.id", "reader.id")][, mean(V1)] 
g_pre[[1]] <- graph.data.frame(g_pre[[1]], directed = TRUE, vertices = 1:341)
g_pre[[1]] <- induced_subgraph(g_pre[[1]], vids = vids)
g_pre[[1]] <- as.matrix(as_adj(g_pre[[1]]))

## between Wave 1 and Wave 2
## network from Dec 05 to Dec 10
g_pre[[2]] <- net[reading.date %between% c("2012-12-05", "2012-12-10"),]
g_pre[[2]] <- data.frame(g_pre[[2]][,2], g_pre[[2]][,3]) ## reader: source, poster: target
setDT(g_pre[[2]]); g_pre[[2]][, count :=1]
g_pre[[2]][, sum(count), by = c("poster.id", "reader.id")][, mean(V1)] 
g_pre[[2]] <- graph.data.frame(g_pre[[2]], directed = TRUE, vertices = 1:341)
g_pre[[2]] <- induced_subgraph(g_pre[[2]], vids = vids)
g_pre[[2]] <- as.matrix(as_adj(g_pre[[2]]))

## between Wave 2 and Wave 3
## network from Dec 14 to Dec 16
g_pre[[3]] <- net[reading.date %between% c("2012-12-14", "2012-12-16"),]
g_pre[[3]] <- data.frame(g_pre[[3]][,2], g_pre[[3]][,3]) ## reader: source, poster: target
setDT(g_pre[[3]]); g_pre[[3]][, count :=1]
g_pre[[3]][, sum(count), by = c("poster.id", "reader.id")][, mean(V1)] 
g_pre[[3]] <- graph.data.frame(g_pre[[3]], directed = TRUE, vertices = 1:341)
g_pre[[3]] <- induced_subgraph(g_pre[[3]], vids = vids)
g_pre[[3]] <- as.matrix(as_adj(g_pre[[3]]))

##------------------------------------------##
## create a set of covariates for each wave ##
##------------------------------------------##

## CANDIDATE IMAGE
# candidate image (Park), W1 to W3
covariate_data <- data.table(candidate.image.Park.W1 = dat[vids, pv194:pv208][, (Mean = rowMeans(.SD)), by = vids][,V1], 
                             candidate.image.Park.W2 =  dat[vids, kv7:kv21][, (Mean = rowMeans(.SD)), by = vids][,V1],
                             candidate.image.Park.W3 = dat[vids, hv32:hv46][, (Mean = rowMeans(.SD)), by = vids][,V1],
                             
                             candidate.image.Moon.W1 = dat[vids, pv209:pv223][, (Mean = rowMeans(.SD)), by = vids][,V1],
                             candidate.image.Moon.W2 = dat[vids, kv22:kv36][, (Mean = rowMeans(.SD)), by = vids][,V1],
                             candidate.image.Moon.W3 = dat[vids, hv47:hv61][, (Mean = rowMeans(.SD)), by = vids][,V1],
                             
                             candidate.preference.W1 = dat[vids, as.numeric(canpref1)],
                             candidate.preference.W2 = dat[vids, as.numeric(canpref2)],
                             candidate.preference.W3 = dat[vids, as.numeric(canpref3)],
                             
                             knowledge = rowSums(dat[vids, .(KN1 = recode(pv177, "2 = 1; else = 0"),
                                                             KN2 = recode(pv178, "1 = 1; else = 0"),
                                                             KN3 = recode(pv179, "2 = 1; else = 0"),
                                                             KN4 = recode(pv180, "2 = 1; else = 0"),
                                                             KN5 = recode(pv181, "1 = 1; else = 0"),
                                                             KN6 = recode(pv182, "3 = 1; else = 0"),
                                                             KN7 = recode(pv183, "4 = 1; else = 0"),
                                                             KN8 = recode(pv184, "5 = 1; else = 0"), 
                                                             KN9 = recode(pv185, "2 = 1; else = 0"),
                                                             KN0 = recode(pv185_1, "4 = 1; else = 0"))]),
                             
                            interest = dat[vids, pv165:pv166][, rowMeans(.SD), by = vids][,V1],
                            
                            efficacy.W1 = dat[vids, .(8 - pv163, 8 - pv164, pv165, pv167)][, rowMeans(.SD), by = vids][,V1],
                            efficacy.W2 = dat[vids, .(8 - kv177, 8 - kv178, kv179, kv181)][, rowMeans(.SD), by = vids][,V1],
                            efficacy.W3 = dat[vids, .(8 - hv232, 8 - hv233, hv238, hv239)][, rowMeans(.SD), by = vids][,V1],
                            
                            ideology.W1 = dat[vids, as.numeric(pv258)],
                            ideology.W2 = dat[vids, as.numeric(kv49)],
                            ideology.W3 = dat[vids, as.numeric(hv24)],
                            
                            consistency.motivation = apply(dat[vids, .(as.numeric(pv18),
                                                                       as.numeric(pv19),
                                                                       as.numeric(pv20),
                                                                       as.numeric(pv21),
                                                                       as.numeric(pv23),
                                                                       as.numeric(pv24))], 1, mean),
                            
                            understanding.motivation = apply(dat[vids, pv13:pv16], 1, mean),
                            
                            hedomic.motivation = apply(dat[vids, pv27:pv29], 1, mean),

                            age = dat[vids, as.numeric(age)/10],
                            gender = dat[vids, as.numeric(sex) - 1],
                            edu = dat[vids, as.numeric(edu)],
                            
                            region = dat[vids, recode(as.numeric(region1), "1 = 1; 
                                                 2 = 2; 14 = 2; 7 = 2;
                                                      3 = 3; 13 = 3;
                                                      4 = 4; 8 = 4;
                                                      5 = 5; 11:12 = 5; 
                                                      6 = 6; 9:10 = 6;
                                                      15 = 7")]
)
rownames(covariate_data) <- paste0("node_", as.character(vids))

# ## lagged reciprocity and autoregression
# g_delrecip <- g_autoregression <- list()
# for (i in 1:3) {
#   temp <- as.matrix(g_pre[[i]])
#   temp[temp == 0] <- -1
#   g_autoregression[[i]] <- temp
#   g_delrecip[[i]] <- t(as.matrix(g_pre[[i]]))
# }
# 
# ## delayed transitivity and delayed cyclic closure 
# g_lagtransitivity <- g_lagcyclic <- list()
# for (i in 1:3) {
#   temp <- as.matrix(g_pre[[i]]) %*% as.matrix(g_pre[[i]])
#   diag(temp) <- 0
#   g_lagtransitivity[[i]] <- temp
#   
#   temp <- t(as.matrix(g_pre[[i]])) %*% t(as.matrix(g_pre[[i]]))
#   diag(temp) <- 0
#   g_lagcyclic[[i]] <- temp
# }


## additionally make a list of dyadic ED matrix
liberal.issue.stance <- apply(dat[vids, c("pv299", "pv303"), with = F], 1, mean)
conserv.issue.stance <- apply(dat[vids, c("pv300", "pv304"), with = F], 1, mean)

policy.pref.diff <- sqrt(outer(liberal.issue.stance, liberal.issue.stance, "-")^2 + 
                                outer(conserv.issue.stance, conserv.issue.stance, "-")^2)
policy.pref.sim <- 1 / (1 + policy.pref.diff)
## CRITERIA IMPORTANCE
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
## additionally make a list of dyadic ED matrix
evaludative.criteria.diff <- sqrt(outer(criteria.competence, criteria.competence, "-")^2 + 
                                         outer(criteria.background, criteria.background, "-")^2) 
evaludative.criteria.sim <- 1 / (1 + evaludative.criteria.diff)



## GERGM time 1

net <- as.matrix(g[[1]])
net_pre <- as.matrix(g_pre[[1]])
rownames(net) <- colnames(net) <- paste0("node_", as.character(vids))
g_autoregression <- as.matrix(g_pre[[1]])
g_delrecip <- as.matrix(t(g_pre[[1]]))
g_lagtransitivity <- as.matrix(as.matrix(g_pre[[1]]) %*% as.matrix(g_pre[[1]]))

test.1 <- gergm(net ~ edges + mutual(alpha = 1) + ## downweighting 
                  netcov(g_autoregression) + netcov(g_delrecip) + netcov(g_lagtransitivity) + 
                  receiver("gender") + sender("gender") + nodematch("gender") + 
                  receiver("edu") + sender("edu") +
                  receiver("region") + sender("region") + nodematch("region") + 
                  receiver("candidate.preference.W1") + sender("candidate.preference.W1") + 
                  nodematch("candidate.preference.W1") + 
                  
                  netcov(evaludative.criteria.sim) + netcov(policy.pref.sim) + netcov(net_pre) + 
                  
                  ctriads(alpha = 1) + ttriads(alpha = 1) + 
                  out2stars(alpha = 1) + in2stars(alpha = 1), 
                
                covariate_data = covariate_data, weighted_MPLE = TRUE, downweight_statistics_together = FALSE,
                parallel = TRUE, parallel_statistic_calculation = TRUE, cores = 10, 
                number_of_networks_to_simulate = 400000,
                thin = 1/100,
                proposal_variance = 0.05,
                MCMC_burnin = 200000,
                seed = 456,
                convergence_tolerance = 0.8,
                hyperparameter_optimization = TRUE,
                target_accept_rate = 0.25,
                theta_grid_optimization_list = list(grid_steps = 2,
                                                    step_size = 0.1,
                                                    cores = 10,
                                                    iteration_fraction = 1))

















, ## instead of calling from covariate data, you can store them as network attributes
                number_of_networks_to_simulate = 500, thin = 1, proposal_variance = 0.05,
                MCMC_burnin = 10000, seed = 456, convergence_tolerance = 0.5)




  
  dgwdsp(decay = 1, fixed = T, type = "ITP") +
  dgwdsp(decay = 1, fixed = T, type = "OSP") +
  dgwdsp(decay = 1, fixed = T, type = "ISP") +
  
  dgwesp(decay = 1.5, fixed = T, type = "ITP") +
  dgwesp(decay = 1.5, fixed = T, type = "OSP") +
  dgwesp(decay = 1.5, fixed = T, type = "ISP") +
  gwodegree(decay = 2.5, fixed = T) + gwidegree(decay = 3, fixed = T) +
  
  nodeicov("consistency.motivation") + nodeocov("consistency.motivation") +
  nodeicov("understanding.motivation") + nodeocov("understanding.motivation") +
  nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") +
  nodeicov("candidate.preference") + nodeocov("candidate.preference") +
  nodematch("candidate.preference") + edgecov(policy.pref.sim.3) +
  edgecov(evaludative.criteria.sim.3)
