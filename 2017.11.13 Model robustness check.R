
## MODEL robustness check
options(scipen = 999)
require(ergm)
require(btergm)
# setwd("~/Dropbox/(17) 2017 Spring/network QAP Korean election")
source("dev/btergm helper-functions.R")

## -------------------------------------- ##
## imputed candidate preference at Wave 1 ##
## ---------------------------------------##

options(scipen = 999)

if(!("Amelia" %in% installed.packages()[,"Package"])) install.packages("Amelia")
require(Amelia)
require(ergm)
require(btergm)
require(texreg)
require(parallel)

# setwd("~/Dropbox/GitHub/Korean2012ElectionProject")
source("dev/btergm helper-functions.R")

## prepare data without subsetting nodes with missing covariates at W1
source_lines("dev/btergm (1) data prep.R", 1:20)
source_lines("dev/btergm (1) data prep.R", 25:489)

## a total of 341 cases are included in this analysis (including 22 missing covariate cases)
## check the dimension of matrix
dim(as.matrix(g[[1]]))

data_to_imp <- data.frame(
  ids = rep(network::network.vertex.names(g[[1]]), 3),
  waves = rep(1:3, each = 341),
  candidate.image.Park = c(dat[vids, pv194:pv208][, (Mean = rowMeans(.SD)), by = vids][,V1],
                           dat[vids, kv7:kv21][, (Mean = rowMeans(.SD)), by = vids][,V1],
                           dat[vids, hv32:hv46][, (Mean = rowMeans(.SD)), by = vids][,V1]),
  candidate.image.Moon = c(dat[vids, pv209:pv223][, (Mean = rowMeans(.SD)), by = vids][,V1],
                           dat[vids, kv22:kv36][, (Mean = rowMeans(.SD)), by = vids][,V1],
                           dat[vids, hv47:hv61][, (Mean = rowMeans(.SD)), by = vids][,V1]),
  candidate.preference = c(g[[1]] %v% "candidate.preference", g[[2]] %v% "candidate.preference", g[[3]] %v% "candidate.preference"),
  liberal.issue.stance = c(g[[1]] %v% "liberal.issue.stance", g[[2]] %v% "liberal.issue.stance", g[[3]] %v% "liberal.issue.stance"),
  conserv.issue.stance = c(g[[1]] %v% "conserv.issue.stance", g[[2]] %v% "conserv.issue.stance", g[[3]] %v% "conserv.issue.stance"),
  internal.efficacy = c(g[[1]] %v% "internal.efficacy", g[[2]] %v% "internal.efficacy", g[[3]] %v% "internal.efficacy"),
  media.use.freq = c(g[[1]] %v% "media.use.freq", g[[2]] %v% "media.use.freq", g[[3]] %v% "media.use.freq"),
  talk.freq = c(g[[1]] %v% "talk.freq", g[[2]] %v% "talk.freq", g[[3]] %v% "talk.freq")
)

## generate 10 imputed datasets using Amelia function, setting candidate.preference as nominal variable
set.seed(12345)
dat.imputed <- amelia(x = data_to_imp, m = 5, cs = "ids", ts = "waves", noms = "candidate.preference")

## check imputation patterns
imputation.pattern <- cbind(as.numeric(network.vertex.names(g[[1]])),
                            (g[[1]] %v% "candidate.preference"),
                            sapply(dat.imputed$imputations, function(i) i$candidate.preference[1:341]))
colnames(imputation.pattern) <- c("ids", "original values", paste("imp", 1:5))
imputation.pattern[!complete.cases(imputation.pattern),]

## check correlations across waves for time-varying variables
wide.dat <- reshape(data_to_imp, direction = 'wide', idvar = 'ids', timevar = 'waves')
setDT(wide.dat)

vars.to.check <- c("candidate.image.Park", "candidate.image.Moon", "candidate.preference",
                   "liberal.issue.stance", "conserv.issue.stance", "internal.efficacy",
                   "media.use.freq", "talk.freq")

test.cor.summary <- matrix(NA, ncol = 3, nrow = 8)
rownames(test.cor.summary) <- vars.to.check
colnames(test.cor.summary) <- c("mean", "min", "max")

vars.to.check <- paste(rep(vars.to.check, each = 3), 1:3, sep = ".")

for (i in 1:9) {
  k <- 3 * i
  index <- (k-2):k
  test.cor <- cor(wide.dat[, vars.to.check[index], with = F], use = "complete.obs")
  test.cor <- test.cor[lower.tri(test.cor)]
  test.cor.summary[i, 1] <- mean(test.cor)
  test.cor.summary[i, 2] <- min(test.cor)
  test.cor.summary[i, 3] <- max(test.cor)
}
print(test.cor.summary)
apply(test.cor.summary[1:5,], 2, range)

## assign imputed values to a origianl dataset
g_imp <- vector("list", 5)

for (i in 1:5) {
  g_imp[[i]] <- g
  g_imp[[i]][[1]] %v% "candidate.preference" <-  dat.imputed$imputations[[i]]$candidate.preference[1:341]
}

## Estimate Btergm with imputation ##
## prepare estimation
load("R_results/btergm.results.Aug 2nd.Rdata")
formula.btergm <- final.model@formula

estimate_btergm_with_imputed_data <- function(g, formula.btergm, R = 200) {

  model <- btergm::btergm(formula.btergm, R = R, parallel = "snow", ncpus = 4)
  coef <- model@coef
  boot <- model@boot

  return(list(model = model, coef = coef, boot = boot))
}

## estimate btergm model with imputated data
imputation.btergm.model <- lapply(1:5, function(i) estimate_btergm_with_imputed_data(g_imp[[i]], formula.btergm, R = 200))

pooled.btergm.model <- imputation.btergm.model[[1]]$model
pooled.btergm.model@coef <- apply(sapply(1:5, function(i) coef(imputation.btergm.model[[i]]$model)), 1, mean)
pooled.btergm.model@boot$t0 <- pooled.btergm.model@coef
pooled.btergm.model@boot$t <- Reduce("rbind", lapply(1:5, function(i) imputation.btergm.model[[i]]$model@boot$t))
pooled.btergm.model@boot$R <- pooled.btergm.model@R <- 1000

save(pooled.btergm.model, file = "R_results/5. pooled.btergm.model.Rdata")

## -------------------- ##
## no thresholded model ##
## ---------------------##

source("dev/btergm (1) data prep no threshhold.R")

RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

final.model.nothreshold <- btergm(
  g ~ edges + ## intercept

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

    ## individual, motivation factor
    nodeicov("consistency.motivation") + nodeocov("consistency.motivation") +
    nodeicov("understanding.motivation") + nodeocov("understanding.motivation") +
    nodeicov("hedomic.motivation") + nodeocov("hedomic.motivation") +

    ## dyadic, consistency
    nodeifactor("candidate.preference") + nodeofactor("candidate.preference") +
    nodematch("candidate.preference") +
    edgecov(policy.pref.sim) +
    edgecov(evaludative.criteria.sim) +

    ## endogenous and lagged structural, control
    isolates + mutual +
    edgecov(g_autoregression) +
    gwdsp(decay = 1.5, fixed = T) +

    ## lagged structural, control
    edgecov(g_delrecip) +
    edgecov(g_lagtransitivity) + ## lagged gwesp_OTP
    edgecov(g_lagcyclic) + ## lagged gwesp_ITP
    edgecov(g_lag_shared_activity) + ## lagged gwesp_OSP
    edgecov(g_lag_shared_popularity) + ## lagged gwesp_ISP
    nodeocov("lagged.sender.effect") +
    nodeicov("lagged.receiver.effect") +


    ## endogenous structural
    dgwesp(decay = 3, fixed = T, type = "OTP") +
    dgwesp(decay = 3, fixed = T, type = "ITP") +
    dgwesp(decay = 3, fixed = T, type = "OSP") +
    dgwesp(decay = 2, fixed = T, type = "ISP") +

    gwodegree(decay = 3.5, fixed = T) + ## hedonic
    gwidegree(decay = 3, fixed = T), ## hedonic
  R = 1000, parallel = "multicore", ncpus = parallel::detectCores())

save(final.model.nothreshold, file = "R_results/6. final.model.nothreshold.Rdata")

gof.statistics <- c(dsp, odeg, ideg, esp, desp_OTP, desp_ITP, desp_OSP, desp_ISP,
                    geodesic, triad.directed, rocpr, walktrap.modularity)

final.model.nothreshold.gof <- gof(final.model.nothreshold, nsim = 300,
                                   statistics = gof.statistics, parallel = "multicore", ncpus = 4)

plot(final.model.nothreshold.gof, mfrow = F, xlim = 40)


## ------------------------------------ ##
## Daily slice model (instead of 3-way) ##
## -------------------------------------##

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

## create a dependent network list ##

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
  g[[i]] <- sna::event2dichot(g[[i]], method = "absolute", thresh = thresholds[i])
  g[[i]] <- as.network(g[[i]])


  ## create a set of covariates for each wave ##
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
                                dgwesp(decay = 3, fixed = T, type = "OTP") +
                                dgwesp(decay = 3, fixed = T, type = "ITP") +
                                dgwesp(decay = 3, fixed = T, type = "OSP") +
                                dgwesp(decay = 2, fixed = T, type = "ISP") +

                                gwodegree(decay = 1.5, fixed = T) +
                                gwidegree(decay = 2, fixed = T),

                              verbose = T, R = 1000, ncpus = parallel::detectCores(), parallel = "snow")

save(final.model.daily, file = "R_results/7. final.model.daily.Rdata")




## --------------------------- ##
## MRQAP model instead of ERGM ##
## --------------------------- ##

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
## internal efficacy, in ties
predictor.matrix[[23]] <- matrix(rep(dat[vids, pv126:pv129][, rowMeans(.SD), by = vids][,V1], lengthn), nrow = lengthn, byrow = T)
predictor.matrix[[24]] <- matrix(rep(dat[vids, pv126:pv129][, rowMeans(.SD), by = vids][,V1], lengthn), nrow = lengthn, byrow = F)

## regional origin, in-tie
## 1 = Seoul, 2 = Busan, Ulsan & Kungnam, 3 = Tague and Kungbuk
## 4 = Inchun, kunggi & Kangwon,
## 5 = Kwangju & Junnam/buck, 6 = Daejun & Chungnam/buk
## 7 = Jeuju
region_origin <- dat[vids, recode(as.numeric(region1), "1 = 1; else = 0")]
predictor.matrix[[25]] <- matrix(rep(region_origin, lengthn), nrow = lengthn, byrow = T)
## regional origin, out tie
predictor.matrix[[26]] <- matrix(rep(region_origin, lengthn), nrow = lengthn, byrow = F)
## regional homophily
predictor.matrix[[27]] <- 1 - abs(outer(region_origin, region_origin, "-"))
## lagged effects
net2 <- read.csv("Dat/Reading_1113-1126_Participants(N=341)_Count(N=160836).csv")
net2 <- igraph::graph.data.frame(net2[, c("reader.id", "poster.id")], directed = TRUE, vertices = 1:341)
net2 <- igraph::induced_subgraph(net2, vids = vids)
predictor.matrix[[28]] <- as.matrix(igraph::as_adj(net2)) ## previous communication
## lagged transitivity
predictor.matrix[[29]] <- t(as.matrix(igraph::as_adj(net2)))
## mutual
predictor.matrix[[30]] <- t(net) ## transpose of Y

library(texreg)
source("dev/netlm.multicore.R")
MRQAP.model <- netlm.multicore(net, predictor.matrix, intercept = TRUE, mode = "digraph", diag = FALSE,
                               nullhyp = "qapspp", reps = 1000)
save(MRQAP.model, file = "8. MRQAP.model.Rdata")

## comparison of the estimated models (for robustness check) with the main model reported in the ms:
require(texreg)

ci.final.model <- summary(final.model, type = "bca")
ci.pooled.btergm.model <- summary(pooled.btergm.model, type = "bca")
ci.final.model.daily <- summary(final.model.daily, type = "bca")
ci.final.model.nothreshold <- summary(final.model.nothreshold, type = "bca")
ci.mrqap <- apply(MRQAP.model$dist, 2, quantile, c(0.025,0.975))

htmlreg(list(final.model, pooled.btergm.model, final.model.daily, final.model.nothreshold, MRQAP.model),
          leading.zero = F, single.row = T, digits = 3,
          custom.model.names = c("Main model", "Multiple imputation", "Daily", "No threshold", "MRQAP"),
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
                                "Multiple two-paths (GWDSP)",
                                "Delayed reciprocity",
                                "Delayed transitivity closure", "Delayed cyclic closure",
                                "Delayed activity closure", "Delayed popularity closure",
                                "Persistent sender (out-tie)", "Persistent receiver (in-ties)",
                                "Multiple path closure (GWESP-OTP)", "Multiple cyclic closure (GWESP-ITP)",
                                "Multiple activity closure (GWESP-OSP)", "Multiple popularity closure (GWESP-ISP)",
                                "Activity spread (GW-outdegree)", "Popularity spread (GW-indegree)",
                                "Previous communication", "Delayed reciprocity",
                                "Delayed transitivity closure", "Delayed cyclic closure",
                                "Delayed activity closure", "Delayed popularity closure",
                                "Multiple two-paths (GWDSP)",
                                "Edges (Intercept)",
                                "Consistency motivation (in-ties)", "Consistency motivation (out-ties)",
                                "Understanding motivation (in-ties)", "Understanding motivation (out-ties)",
                                "Hedonic motivation (in-ties)", "Hedonic motivation (out-ties)",
                                "Candidate pref = Moon (in-ties)", "Candidate pref = Moon (out-ties)",
                                "Same candidate pref", "Similar policy pref", "Similar evaluative criteria",
                                "Age (in-ties)", "Age (out-ties)",
                                "Female (in-ties)", "Female (out-ties)", "Gender homophily",
                                "Education (in-ties)", "Education (out-ties)",
                                "Talk freq (in-ties)", "Talk freq (out-ties)",
                                "Media use (in-ties)", "Media use (out-ties)",
                                "Internal efficacy (in-ties)", "Internal efficacy (out-ties)",
                                "Regional origin = Seoul (in-ties)",
                                "Regional origin = Seoul (out-ties)",
                                "Regional homophily (Seoul)",
                                "Previous communication",
                                "Delayed reciprocity",
                                "Reciprocity"),
        override.ci.low = list(ci.final.model[,2], ci.pooled.btergm.model[,2], ci.final.model.daily[,2],
                               ci.final.model.nothreshold[,2], NA),
        override.ci.up = list(ci.final.model[,3], ci.pooled.btergm.model[,3], ci.final.model.daily[,3],
                              ci.final.model.nothreshold[,3], NA),
        file = "robustness check.table.doc")


