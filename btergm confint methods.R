
## replication code for error in handling models with "little variation" models

library("statnet")
set.seed(5)

networks <- list()
for(i in 1:10){            # create 10 random networks with 10 actors
  mat <- matrix(rbinom(100, 1, .25), nrow = 10, ncol = 10)
  diag(mat) <- 0           # loops are excluded
  nw <- network(mat)       # create network object
  networks[[i]] <- nw      # add network to the list
}

covariates <- list()
for (i in 1:10) {          # create 10 matrices as covariate
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  covariates[[i]] <- mat   # add matrix to the list
}

fit <- btergm(networks ~ edges + istar(2) +
                edgecov(covariates), R = 100)

summary(fit)               # show estimation results


## add timecov

fit2 <- btergm(networks ~ edges + istar(2) + edgecov(covariates) +
                 timecov(transform = function(t) t) + 
                 timecov(covariates, transform = function(t) t) + 
                 timecov(covariates, transform = function(t) t^2), R = 100)

summary(fit2)











setMethod(f = "confint", signature = "btergm", definition = function(object, 
                                                                     parm, level = 0.95, type = "perc", invlogit = FALSE, na.rm = TRUE, ...) {
  cf <- coef(object, invlogit = invlogit)
  pnames <- names(cf)
  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }
  n.orig <- nrow(object@boot$t)
  n.ret <- nrow(object@boot$t[complete.cases(object@boot$t), ])
  perc <- 100 * (n.orig - n.ret) / n.orig
  if (n.orig != n.ret) {
    warning(paste0("Too little variation in the model. ", n.orig - n.ret, 
                   " replications (", perc, "%) are dropped from CI estimation."))
  }
  
  if (invlogit == TRUE) {
    object@boot$t <- apply(object@boot$t, 1:2, function(x) 1 / (1 + exp(-x)))
    object@boot$t0 <- sapply(object@boot$t0, function(x) 1 / (1 + exp(-x)))
  }
  if (type == "perc") {
    type2 <- "percent"
  } else if (type == "norm") {
    type2 <- "normal"
  } else if (type == "basic") {
    type2 <- "basic"
  } else if (type == "stud") {
    type2 <- "student"
  } else if (type == "bca") {
    type2 <- "bca"
  } else {
    stop(paste("'type' not supported. Use 'perc', 'bca', 'norm', 'basic',", 
               "or 'stud'."))
  }
  ci <- sapply(1:length(cf), function(x) {
    b <- boot::boot.ci(object@boot, conf = level, type = type, index = x, na.rm = T)
    b[[type2]][4:5]
  })
  ci <- cbind(cf, t(ci))
  if (class(ci) == "numeric") {
    ci.nam <- names(ci)
    ci <- matrix(ci, nrow = 1)
    colnames(ci) <- ci.nam
    rownames(ci) <- names(cf)
  }
  ci <- ci[parm, ]
  if (class(ci) != "matrix") {
    ci <- matrix(ci, ncol = 3)
    rownames(ci) <- parm
  }
  label1 <- paste0(100 * (1 - level) / 2, "%")
  label2 <- paste0(100 * (1 - (1 - level) / 2), "%")
  colnames(ci) <- c("Estimate", label1, label2)
  return(ci)
}
)

