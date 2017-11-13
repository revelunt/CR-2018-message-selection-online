
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

##---------------------------------##
## create a dataset for imputation ##
##---------------------------------##

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

##---------------------------------##
## Estimate Btergm with imputation ##
##---------------------------------##

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

## comparison of the pooled model with main model:
require(texreg)
screenreg(list(final.model, pooled.btergm.model), leading.zero = F, single.row = T, digits = 3)

save(pooled.btergm.model, file = "pooled.btergm.model.robustnesscheck.Rdata")
