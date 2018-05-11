
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
rm(list = ls())
source("dev/btergm helper-functions.R")
source("dev/btergm (1) data prep.R")

RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")


## -------------------------------- ##
## Robostness check (Response memo) ##
## -------------------------------- ##

require(sna)

## New table 2
xmat <- list()
for (i in 1:3) {
  mat2 <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor
  dimnames(mat2) <- NULL
  xmat[[i]] <- mat2[,,1] ## X_ij is 1 if i and j has same preference, otherwise zero
}

candidate.pref.bivariate <- mclapply(1:3, function(i) {
  out <- netlogit(g[[i]], xmat[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
                  nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)

policy.pref.sim.bivariate <- mclapply(1:3, function(i) {
  out <- netlogit(g[[i]], policy.pref.sim[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
                  nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)

xmat <- list()
for (i in 1:3) {
  mat2 <- ergmMPLE(g[[i]] ~ absdiff("ideology"), output = "array")$predictor
  dimnames(mat2) <- NULL
  xmat[[i]] <- mat2[,,1] ## X_ij is 1 if i and j has same preference, otherwise zero
}

ideology.sim.bivariate <- mclapply(1:3, function(i) {
  out <- netlogit(g[[i]], xmat[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
                  nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)

evaludative.criteria.sim.bivariate <- mclapply(1:3, function(i) {
  out <- netlogit(g[[i]], evaludative.criteria.sim[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
                  nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)





## Table S4
## popularity-spread, predicted by same candidate pref var.
ymat <- xmat <- list()
for (i in 1:3) {
  mat <- ergmMPLE(g[[i]] ~ gwidegree(decay = 3, fixed = T), output = "array")$predictor
  dimnames(mat) <- NULL
  mat <- mat[,,1]
  ymat[[i]] <- mat
  
  mat2 <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor
  dimnames(mat2) <- NULL
  xmat[[i]] <- mat2[,,1]
}

candidate.pref.gwidegree.bivariate <- mclapply(1:3, function(i) {
  out <- netlm(ymat[[i]], xmat[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
                  nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)

## activity-spread, predicted by same candidate pref.
ymat <- list()
for (i in 1:3) {
  mat <- ergmMPLE(g[[i]] ~ gwodegree(decay = 2, fixed = T), output = "array")$predictor
  dimnames(mat) <- NULL
  mat <- mat[,,1]
  ymat[[i]] <- mat
}

candidate.pref.gwodegree.bivariate <- mclapply(1:3, function(i) {
  out <- netlm(ymat[[i]], xmat[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
               nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)

## reciprocity, predicted by same candidate pref.
ymat <- list()
for (i in 1:3) {
  mat <- ergmMPLE(g[[i]] ~ mutual, output = "array")$predictor
  dimnames(mat) <- NULL
  mat <- mat[,,1]
  ymat[[i]] <- mat
}

candidate.pref.mutual.bivariate <- mclapply(1:3, function(i) {
  out <- netlogit(ymat[[i]], xmat[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
               nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)


# gwidegree, predicted by policy preference similarity
ymat <- xmat <- list()
for (i in 1:3) {
  mat <- ergmMPLE(g[[i]] ~ gwidegree(decay = 3, fixed = T), output = "array")$predictor
  dimnames(mat) <- NULL
  mat <- mat[,,1]
  ymat[[i]] <- mat
  
  mat2 <- ergmMPLE(g[[i]] ~  edgecov(policy.pref.sim[[i]]), output = "array")$predictor
  dimnames(mat2) <- NULL
  xmat[[i]] <- mat2[,,1]
}

policy.pref.sim.gwidegree.bivariate <- mclapply(1:3, function(i) {
  out <- netlm(ymat[[i]], xmat[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
               nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)


## activity-spread, predicted by policy preference similarity
ymat <- list()
for (i in 1:3) {
  mat <- ergmMPLE(g[[i]] ~ gwodegree(decay = 2, fixed = T), output = "array")$predictor
  dimnames(mat) <- NULL
  mat <- mat[,,1]
  ymat[[i]] <- mat
}

policy.pref.sim.gwodegree.bivariate <- mclapply(1:3, function(i) {
  out <- netlm(ymat[[i]], xmat[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
               nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)


## reciprocity, predicted by policy preference similarity
ymat <- list()
for (i in 1:3) {
  mat <- ergmMPLE(g[[i]] ~ mutual, output = "array")$predictor
  dimnames(mat) <- NULL
  mat <- mat[,,1]
  ymat[[i]] <- mat
}

policy.pref.sim.mutual.bivariate <- mclapply(1:3, function(i) {
  out <- netlogit(ymat[[i]], xmat[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
                  nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)


## popularity-spread, predicted by ideological self-placement homophily.
ymat <- xmat <- list()
for (i in 1:3) {
  mat <- ergmMPLE(g[[i]] ~ gwidegree(decay = 3, fixed = T), output = "array")$predictor
  dimnames(mat) <- NULL
  mat <- mat[,,1]
  ymat[[i]] <- mat
  
  mat2 <- ergmMPLE(g[[i]] ~ absdiff("ideology"), output = "array")$predictor
  dimnames(mat2) <- NULL
  xmat[[i]] <- mat2[,,1]
}

ideological.homo.gwidegree.bivariate <- mclapply(1:3, function(i) {
  out <- netlm(ymat[[i]], xmat[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
                  nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)

## activity-spread, predicted by ideological self-placement homophily.
ymat <- list()
for (i in 1:3) {
  mat <- ergmMPLE(g[[i]] ~ gwodegree(decay = 2, fixed = T), output = "array")$predictor
  dimnames(mat) <- NULL
  mat <- mat[,,1]
  ymat[[i]] <- mat
}

ideological.homo.gwodegree.bivariate <- mclapply(1:3, function(i) {
  out <- netlm(ymat[[i]], xmat[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
               nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)

## reciprocity, predicted by ideological self-placement homophily.
ymat <- list()
for (i in 1:3) {
  mat <- ergmMPLE(g[[i]] ~ mutual, output = "array")$predictor
  dimnames(mat) <- NULL
  mat <- mat[,,1]
  ymat[[i]] <- mat
}

ideological.homo.mutual.bivariate <- mclapply(1:3, function(i) {
  out <- netlogit(ymat[[i]], xmat[[i]], intercept = TRUE, mode = "digraph", diag = FALSE,
                  nullhyp = "qapspp",  test.statistic = "beta", tol = 1e-7, reps = 1000)
  out
}, mc.cores = 3)




## ---------------------------------------- ##
## Number check (Quantities reported in ms) ##
## ---------------------------------------- ##

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
