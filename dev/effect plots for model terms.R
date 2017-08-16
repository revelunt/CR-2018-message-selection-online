

## automatically setting working directories
setwd("~/Dropbox/GitHub/Korean2012ElectionProject")
options(scipen = 999)
require(ergm)
require(btergm)
require(texreg)
require(parallel)

# setwd("~/Dropbox/GitHub/Korean2012ElectionProject")
source("dev/btergm helper-functions.R")
source("dev/btergm (1) data prep.R")

RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

load("~/Dropbox/GitHub/Korean2012ElectionProject/R_results/btergm.results.Aug 2nd.Rdata")
require(btergm)

summary(final.model, type = "bca")


ep <- edgeprob(final.model)

## clear up '[[i]]' in the data frame
for (i in 1:ncol(ep)) {
  if (grepl("((edge)|(dyad))cov", colnames(ep)[i])) {
    colnames(ep)[i] <- substr(colnames(ep)[i], 1, nchar(colnames(ep)[i]) - 5)
  }
}

indegree.dist <- lapply(1:3, function(i) sna::degree(g[[i]], cmode = "indegree"))
outdegree.dist <- lapply(1:3, function(i) sna::degree(g[[i]], cmode = "outdegree"))

setDT(ep)

## plot probability as a function of indegree/outdegree distribution
names(indegree.dist[[1]]) <- 1:312
names(indegree.dist[[2]]) <- 1:312
names(indegree.dist[[3]]) <- 1:312
names(outdegree.dist[[1]]) <- 1:312
names(outdegree.dist[[2]]) <- 1:312
names(outdegree.dist[[3]]) <- 1:312

ep$indegree[ep$t == 1] <- indegree.dist[[1]][match(ep$j[ep$t == 1], 1:312)]
ep$indegree[ep$t == 2] <- indegree.dist[[2]][match(ep$j[ep$t == 2], 1:312)]
ep$indegree[ep$t == 3] <- indegree.dist[[3]][match(ep$j[ep$t == 3], 1:312)]
ep$outdegree[ep$t == 1] <- outdegree.dist[[1]][match(ep$j[ep$t == 1], 1:312)]
ep$outdegree[ep$t == 2] <- outdegree.dist[[2]][match(ep$j[ep$t == 2], 1:312)]
ep$outdegree[ep$t == 3] <- outdegree.dist[[3]][match(ep$j[ep$t == 3], 1:312)]

## indegree effects
p.in <- ggplot(data = ep[ep$tie == 1, ], aes(x = indegree, y = probability)) + theme_bw() + 
  #scale_colour_grey(start = 0.8, end = 0.2, guide = guide_legend(reverse=TRUE)) + 
  theme(legend.justification=c(1,0), legend.position=c(0.9,0.1)) +
  stat_summary(fun.y = "median", colour = "grey80", geom = "point") +
  geom_quantile(quantiles = 0.5, formula = y ~ poly(x, 2), colour = "black", size = 1) 
  #facet_wrap( ~ t, labeller = as_labeller(c('1' = 'Time 1', '2' = 'Time 2', '3' = 'Time 3'))) #label_both)

## outdegree effects
p.out <- ggplot(data = ep[ep$tie == 1, ], aes(x = outdegree, y = probability)) + theme_bw() + 
  #scale_colour_grey(start = 0.8, end = 0.2, guide = guide_legend(reverse=TRUE)) + 
  theme(legend.justification=c(1,0), legend.position=c(0.9,0.1)) +
  stat_summary(fun.y = "median", colour = "grey80", geom = "point") +
  geom_quantile(quantiles = 0.5, formula = y ~ poly(x, 2), colour = "black", size = 1) 
  #facet_wrap( ~ t, labeller = as_labeller(c('1' = 'Time 1', '2' = 'Time 2', '3' = 'Time 3'))) #label_both)

require(gridExtra)
grid.arrange(p.in, p.out, nrow = 1, ncol = 2)


## evaluative criteria similarity
ggplot(data = ep, aes(x = edgecov.evaludative.criteria.sim, y = probability)) + theme_bw() + 
  #scale_colour_grey(start = 0.8, end = 0.2, guide = guide_legend(reverse=TRUE)) + 
  theme(legend.justification=c(1,0), legend.position=c(0.9,0.1)) +
  #stat_summary(fun.y = "mean", colour = "grey80", geom = "point") +
  geom_smooth(method = "lm", fullrange = T, colour = "black", size = 1) 


# define statistic for bootstrapping
samplemedian <- function(x, d) {
  return(median(x[d]))
}

# get extreme quantiles
low <- sapply(evaludative.criteria.sim, quantile, 0.1) ## 0.3058186
high <- sapply(evaludative.criteria.sim, quantile, 0.9) ## 0.7017544

# sample probabilities (with replacement) 
low.sam <- ep[edgecov.evaludative.criteria.sim <= 0.3058186, probability]
high.sam <- ep[edgecov.evaludative.criteria.sim >= 0.7017544, probability]

low.sam.bs <- boot(low.sam, samplemedian, R = 10000)
low.sam.bs <- c(median(low.sam.bs$t[, 1]), boot.ci(low.sam.bs, type = "bca")$bca[4:5])
