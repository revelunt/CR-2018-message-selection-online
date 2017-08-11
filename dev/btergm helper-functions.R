
library(pbapply)

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}


#' compute and compare fitted probabilities from btergm fit objects
#'
#' extracting coefficients and creating null hypothesis vector
#' is done within function, but you must provide the specific model term
#' (idealy a dyadic level predictor) to test the effect
#' e.g., null_terms=c("nodematch.partyid.1","nodematch.partyid.3")
#' this sets nodematch.partyid term in the null hypothesis vector equals to zero
#' n_sample == no. of random samples to use per time step, within each chapter

compute_dyadic_probabilities_btergm <- function(model, null_terms = c("nodematch.partyid"), 
                                                null_value = null_value,  n_sample = 25) {
  
  n_time <- length(g)
  null <- coef(model)
  null[names(null) %in% null_terms] <- null_value
  
  library(parallel)
  cl <- makeCluster(3L)
  clusterExport(cl = cl, varlist = c("n_time", "null", "model", "n_sample"), envir = environment())
  clusterExport(cl = cl, varlist = c("g", "g_autoregression", "g_delrecip", "g_lagtransitivity", 
                                     "g_lagcyclic", "g_lag_shared_activity",
                                     "g_lag_shared_popularity","policy.pref.sim", "evaludative.criteria.sim"))
  probs <- pblapply(seq_len(n_time), function(time) {
    
    pn <- g[[time]]
    d <- dim(as.matrix(pn))
    size <- d[1] * d[2]
    nw <- matrix(1:size, nrow = d[1], ncol = d[2])
    nw <- nw[lower.tri(nw)]
    if (n_sample > length(nw)) {
      n_samps <- length(nw)
    } else {
      n_samps <- n_sample
    }
    nw <- sample(nw, n_samps, replace = TRUE)
    DF <- data.frame(sample = seq_len(n_samps), time = rep(time, n_samps),
                     prob.est.00 = NA, prob.est.01 = NA, prob.est.11 = NA,
                     prob.null.00 = NA, prob.null.01 = NA, prob.null.11 = NA)
    
    for (n in 1:n_samps) {
      dyad <- arrayInd(nw[n], d)
      i <- dyad[1, 1]
      j <- dyad[1, 2]
      int.est <- btergm::interpret(model, type = "dyad", i = i, j = j, t = time)[[1]]
      int.null <- btergm::interpret(model, coefficients = null, type = "dyad",
                                    i = i, j = j, t = time)[[1]]
      DF$prob.est.00[n] <- int.est[1, 1]
      DF$prob.est.11[n] <- int.est[2, 2]
      DF$prob.est.01[n] <- (int.est[1, 2] + int.est[2, 1]) / 2
      DF$prob.null.00[n] <- int.null[1, 1]
      DF$prob.null.11[n] <- int.null[2, 2]
      DF$prob.null.01[n] <- (int.null[1, 2] + int.null[2, 1]) / 2
    }
    return(DF)
  }, cl = cl)
  stopCluster(cl)
  do.call(rbind, probs)
}

## calculate the prob difference based on computed dyadic probs
calc_prob_diffs <- function(prob_average)
{
  # differences of est. and null hyp:
  # if diff is significantly positive in, for instance, reciprocated tie type,
  # then model coeff. predicts more reciprocated ties than null
  # and such difference is significant..
  probabilities <- matrix(NA,nrow=9,ncol=1)
  
  prob.diff.00 <- prob_average$prob.est.00 - prob_average$prob.null.00
  prob.diff.01 <- prob_average$prob.est.01 - prob_average$prob.null.01
  prob.diff.11 <- prob_average$prob.est.11 - prob_average$prob.null.11
  
  probabilities[1, 1] <- mean(prob.diff.00)   # mean estimated 00 tie prob
  probabilities[2, 1] <- mean(prob.diff.01)   # mean estimated 01 tie prob
  probabilities[3, 1] <- mean(prob.diff.11)   # mean estimated 11 tie prob
  
  ci.00 <- t.test(prob.diff.00, conf.level = 0.99)$conf.int
  ci.01 <- t.test(prob.diff.01, conf.level = 0.99)$conf.int
  ci.11 <- t.test(prob.diff.11, conf.level = 0.99)$conf.int
  
  probabilities[4, 1] <- ci.00[1]              # lower 00 conf. interval
  probabilities[5, 1] <- ci.01[1]              # lower 01 conf. interval
  probabilities[6, 1] <- ci.11[1]              # lower 11 conf. interval
  probabilities[7, 1] <- ci.00[2]              # upper 00 conf. interval
  probabilities[8, 1] <- ci.01[2]              # upper 01 conf. interval
  probabilities[9, 1] <- ci.11[2]              # upper 11 conf. interval
  
  rownames(probabilities) <- c("1.no_tie","2.asymmetric","3.reciprocated",
                               "1.ci_l","2.ci_l","3.ci_l",
                               "1.ci_h","2.ci_h","3.ci_h")
  return(probabilities)
}

## function to add error bar in the plot
error.bar <- function(x, y, upper, lower, length=0.1,...) {
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}


## multicore-support for netlm function
netlm.multicore <- function (y, x, intercept = TRUE, mode = "digraph", diag = FALSE, 
                             nullhyp = c("qap", "qapspp", "qapy", "qapx", "qapallx", "cugtie", "cugden", "cuguman", "classical"), 
                             test.statistic = c("t-value", "beta"), tol = 0.0000001, reps = 1000, 
                             mc.cores = parallel::detectCores()) 
{
  gettval <- function(x, y, tol) {
    xqr <- qr(x, tol = tol)
    coef <- qr.coef(xqr, y)
    resid <- qr.resid(xqr, y)
    rank <- xqr$rank
    n <- length(y)
    rdf <- n - rank
    resvar <- sum(resid^2)/rdf
    cvm <- chol2inv(xqr$qr)
    se <- sqrt(diag(cvm) * resvar)
    coef/se
  }
  gfit <- function(glist, mode, diag, tol, rety, tstat) {
    y <- gvectorize(glist[[1]], mode = mode, diag = diag, 
                    censor.as.na = TRUE)
    x <- vector()
    for (i in 2:length(glist)) x <- cbind(x, gvectorize(glist[[i]], 
                                                        mode = mode, diag = diag, censor.as.na = TRUE))
    if (!is.matrix(x)) 
      x <- matrix(x, ncol = 1)
    mis <- is.na(y) | apply(is.na(x), 1, any)
    if (!rety) {
      if (tstat == "beta") 
        qr.solve(x[!mis, ], y[!mis], tol = tol)
      else if (tstat == "t-value") {
        gettval(x[!mis, ], y[!mis], tol = tol)
      }
    }
    else {
      list(qr(x[!mis, ], tol = tol), y[!mis])
    }
  }
  y <- as.sociomatrix.sna(y)
  x <- as.sociomatrix.sna(x)
  if (is.list(y) || ((length(dim(y)) > 2) && (dim(y)[1] > 1))) 
    stop("y must be a single graph in netlm.")
  if (length(dim(y)) > 2) 
    y <- y[1, , ]
  if (is.list(x) || (dim(x)[2] != dim(y)[2])) 
    stop("Homogeneous graph orders required in netlm.")
  nx <- stackcount(x) + intercept
  n <- dim(y)[2]
  g <- list(y)
  if (intercept) 
    g[[2]] <- matrix(1, n, n)
  if (nx - intercept == 1) 
    g[[2 + intercept]] <- x
  else for (i in 1:(nx - intercept)) g[[i + 1 + intercept]] <- x[i, 
                                                                 , ]
  if (any(sapply(lapply(g, is.na), any))) 
    warning("Missing data supplied to netlm; this may pose problems for certain null hypotheses.  Hope you know what you're doing....")
  fit.base <- gfit(g, mode = mode, diag = diag, tol = tol, 
                   rety = TRUE)
  fit <- list()
  fit$coefficients <- qr.coef(fit.base[[1]], fit.base[[2]])
  fit$fitted.values <- qr.fitted(fit.base[[1]], fit.base[[2]])
  fit$residuals <- qr.resid(fit.base[[1]], fit.base[[2]])
  fit$qr <- fit.base[[1]]
  fit$rank <- fit.base[[1]]$rank
  fit$n <- length(fit.base[[2]])
  fit$df.residual <- fit$n - fit$rank
  tstat <- match.arg(test.statistic)
  if (tstat == "beta") 
    fit$tstat <- fit$coefficients
  else if (tstat == "t-value") 
    fit$tstat <- fit$coefficients/sqrt(diag(chol2inv(fit$qr$qr)) * 
                                         sum(fit$residuals^2)/(fit$n - fit$rank))
  nullhyp <- match.arg(nullhyp)
  if ((nullhyp %in% c("qap", "qapspp")) && (nx == 1)) 
    nullhyp <- "qapy"
  if (nullhyp == "classical") {
    resvar <- sum(fit$residuals^2)/fit$df.residual
    cvm <- chol2inv(fit$qr$qr)
    se <- sqrt(diag(cvm) * resvar)
    tval <- fit$coefficients/se
    fit$dist <- NULL
    fit$pleeq <- pt(tval, fit$df.residual)
    fit$pgreq <- pt(tval, fit$df.residual, lower.tail = FALSE)
    fit$pgreqabs <- 2 * pt(abs(tval), fit$df.residual, lower.tail = FALSE)
  }
  else if (nullhyp %in% c("cugtie", "cugden", "cuguman")) {
    repdist <- matrix(0, reps, nx)
    
    for (i in 1:nx) {
      gr <- g
      temp <- parallel::mclapply(1:reps, function(j) {
        
        gr[[i + 1]] <- switch(nullhyp, cugtie = rgraph(n, 
                                                       mode = mode, diag = diag, replace = FALSE, 
                                                       tielist = g[[i + 1]]), cugden = rgraph(n, tprob = gden(g[[i + 
                                                                                                                   1]], mode = mode, diag = diag), mode = mode, 
                                                                                              diag = diag), cuguman = (function(dc, n) {
                                                                                                rguman(1, n, mut = dc[1], asym = dc[2], null = dc[3], 
                                                                                                       method = "exact")
                                                                                              })(dyad.census(g[[i + 1]]), n))
        
        temp <- gfit(gr, mode = mode, diag = diag, tol = tol, rety = FALSE, tstat = tstat)[i]
        return(temp)
      }, mc.cores = mc.cores)
      repdist[, i] <- unlist(temp)
    }
    
    fit$dist <- repdist
    fit$pleeq <- apply(sweep(fit$dist, 2, fit$tstat, "<="), 
                       2, mean)
    fit$pgreq <- apply(sweep(fit$dist, 2, fit$tstat, ">="), 
                       2, mean)
    fit$pgreqabs <- apply(sweep(abs(fit$dist), 2, abs(fit$tstat), 
                                ">="), 2, mean)
  }
  else if (nullhyp == "qapy") {
    repdist <- matrix(0, reps, nx)
    gr <- g
    
    temp <- parallel::mclapply(1:reps, function(i) {
      gr[[1]] <- rmperm(g[[1]])
      temp <- gfit(gr, mode = mode, diag = diag, 
                   tol = tol, rety = FALSE, tstat = tstat)
      temp
    }, mc.cores = mc.cores)
    
    for (i in 1:reps) { repdist[i, ] <- temp[[i]]}
    
    fit$dist <- repdist
    fit$pleeq <- apply(sweep(fit$dist, 2, fit$tstat, "<="), 
                       2, mean)
    fit$pgreq <- apply(sweep(fit$dist, 2, fit$tstat, ">="), 
                       2, mean)
    fit$pgreqabs <- apply(sweep(abs(fit$dist), 2, abs(fit$tstat), 
                                ">="), 2, mean)
  }
  else if (nullhyp == "qapx") {
    repdist <- matrix(0, reps, nx)
    for (i in 1:nx) {
      gr <- g
      temp <- parallel::mclapply(1:reps, function(j) {
        gr[[i + 1]] <- rmperm(gr[[i + 1]])
        temp <- gfit(gr, mode = mode, diag = diag, tol = tol, rety = FALSE, tstat = tstat)[i]
        temp
        
      }, mc.cores = mc.cores)
      
      repdist[, i] <- unlist(temp)
    }
    
    fit$dist <- repdist
    fit$pleeq <- apply(sweep(fit$dist, 2, fit$tstat, "<="), 
                       2, mean)
    fit$pgreq <- apply(sweep(fit$dist, 2, fit$tstat, ">="), 
                       2, mean)
    fit$pgreqabs <- apply(sweep(abs(fit$dist), 2, abs(fit$tstat), 
                                ">="), 2, mean)
  }
  else if (nullhyp == "qapallx") {
    repdist <- matrix(0, reps, nx)
    gr <- g
    temp <- parallel::mclapply(1:reps, function(i) {
      for (j in 1:nx) gr[[1 + j]] <- rmperm(g[[1 + j]])
      temp <- gfit(gr, mode = mode, diag = diag, tol = tol, rety = FALSE, tstat = tstat)
      temp
    }, mc.cores = mc.cores)
    
    for (i in 1:reps) repdist[i,] <- temp[[i]]
    
    fit$dist <- repdist
    fit$pleeq <- apply(sweep(fit$dist, 2, fit$tstat, "<="), 
                       2, mean)
    fit$pgreq <- apply(sweep(fit$dist, 2, fit$tstat, ">="), 
                       2, mean)
    fit$pgreqabs <- apply(sweep(abs(fit$dist), 2, abs(fit$tstat), 
                                ">="), 2, mean)
  }
  else if ((nullhyp == "qap") || (nullhyp == "qapspp")) {
    xsel <- matrix(TRUE, n, n)
    if (!diag) 
      diag(xsel) <- FALSE
    if (mode == "graph") 
      xsel[upper.tri(xsel)] <- FALSE
    repdist <- matrix(0, reps, nx)
    for (i in 1:nx) {
      xfit <- gfit(g[1 + c(i, (1:nx)[-i])], mode = mode, 
                   diag = diag, tol = tol, rety = TRUE, tstat = tstat)
      xres <- g[[1 + i]]
      xres[xsel] <- qr.resid(xfit[[1]], xfit[[2]])
      if (mode == "graph") 
        xres[upper.tri(xres)] <- t(xres)[upper.tri(xres)]
      
      
      temp <- parallel::mclapply(1:reps, function(j) gfit(c(g[-(1 + i)], list(rmperm(xres))), mode = mode, diag = diag, 
                                                          tol = tol, rety = FALSE, tstat = tstat)[nx], mc.cores = mc.cores)
      
      repdist[, i] <- unlist(temp)
      
    }
    fit$dist <- repdist
    fit$pleeq <- apply(sweep(fit$dist, 2, fit$tstat, "<="), 
                       2, mean)
    fit$pgreq <- apply(sweep(fit$dist, 2, fit$tstat, ">="), 
                       2, mean)
    fit$pgreqabs <- apply(sweep(abs(fit$dist), 2, abs(fit$tstat), 
                                ">="), 2, mean)
  }
  fit$nullhyp <- nullhyp
  fit$names <- paste("x", 1:(nx - intercept), sep = "")
  if (intercept) 
    fit$names <- c("(intercept)", fit$names)
  fit$intercept <- intercept
  class(fit) <- "netlm"
  fit
}


## find mode of a vector
Mode <- function(x, na.rm = FALSE, NaN.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  if(NaN.rm) {
    x = x[!is.nan(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


## texreg extract function for class "netlm"
extract.netlm <- function(model, include.fstatistic = TRUE, include.rsquared = TRUE, include.adjr = TRUE, 
            include.nobs = TRUE, ...) 
  {
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    
    if (model$intercept) {
      mss <- sum((fitted(model) - mean(fitted(model)))^2)
    } else {
      mss <- sum(fitted(model)^2)
    }
    rss <- sum(resid(model)^2)
    qn <- NROW(model$qr$qr)
    df.int <- model$intercept
    rdf <- qn - model$rank
    resvar <- rss/rdf
    fstatistic <- c(value = (mss/(model$rank - df.int))/resvar, numdf = model$rank - 
                      df.int, dendf = rdf)
    r.squared <- mss/(mss + rss)
    adj.r.squared <- 1 - (1 - r.squared) * ((qn - df.int)/rdf)
    

  if (include.fstatistic == TRUE) {
      gof <- c(gof, fstatistic[[1]])
      gof.names <- c(gof.names, "F statistic")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.rsquared == TRUE) {
      gof <- c(gof, r.squared)
      gof.names <- c(gof.names, "Multiple R-squared")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.adjr == TRUE) {
      gof <- c(gof, adj.r.squared)
      gof.names <- c(gof.names, "Adj. R-squared")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.nobs == TRUE) {
      gof <- c(gof, model$n)
      gof.names <- c(gof.names, "Num. obs.")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    
    cf <- model$coefficients
    pv <- model$pgreqabs
    nm <- model$names
    
    if (is.null(model$dist)) {
      cvm <- chol2inv(model$qr$qr)
      se <- sqrt(diag(cvm))
    } else {
      se <- rep(NaN, length(cf))
    }
    
    tr <- createTexreg(coef.names = nm, coef = cf, se = se, pvalues = pv, 
                       gof.names = gof.names, gof = gof, gof.decimal = gof.decimal)
    return(tr)
  }
  

## calculate asymmetry matrix for a covariate
make_asymmetrical_adj <- function(net, var)
{
  n <- net$gal$n
  val <- network::get.vertex.attribute(net, var)
  mat <- matrix(val, n, n, byrow = FALSE)
  mat <- mat - t(mat)
  diag(mat) <- 0
  mat - mean(mat)
}



## gof functions

#' TITLE
#'
#' DETAIL
#' @param mat an adjacency matrix
#' @return vector of statistics
#' @import ergm
#' @export
ddsp_OTP <- function(mat)
{
  lim <- nrow(mat) - 2
  d <- summary(mat ~ ddsp(0:lim, type = "OTP"))
  d[1] <- nrow(mat) * (nrow(mat) - 1) + d[1]
  names(d) <- 0:(length(d) - 1)
  attributes(d)$label <- "Directed dyad-wise shared partners of type OTP"
  return(d)
}

#' TITLE
#'
#' DETAIL
#' @param mat an adjacency matrix
#' @return vector of statistics
#' @import ergm
#' @export
ddsp_ITP <- function(mat)
{
  lim <- nrow(mat) - 2
  d <- summary(mat ~ ddsp(0:lim, type = "ITP"))
  d[1] <- nrow(mat) * (nrow(mat) - 1) + d[1]
  names(d) <- 0:(length(d) - 1)
  attributes(d)$label <- "Directed dyad-wise shared partners of type ITP"
  return(d)
}

#' TITLE
#'
#' DETAIL
#' @param mat an adjacency matrix
#' @return vector of statistics
#' @import ergm
#' @export
ddsp_OSP <- function(mat)
{
  lim <- nrow(mat) - 2
  d <- summary(mat ~ ddsp(0:lim, type = "OSP"))
  d[1] <- nrow(mat) * (nrow(mat) - 1) + d[1]
  names(d) <- 0:(length(d) - 1)
  attributes(d)$label <- "Directed dyad-wise shared partners of type OSP"
  return(d)
}

#' TITLE
#'
#' DETAIL
#' @param mat an adjacency matrix
#' @return vector of statistics
#' @import ergm
#' @export
ddsp_ISP <- function(mat)
{
  lim <- nrow(mat) - 2
  d <- summary(mat ~ ddsp(0:lim, type = "ISP"))
  d[1] <- nrow(mat) * (nrow(mat) - 1) + d[1]
  names(d) <- 0:(length(d) - 1)
  attributes(d)$label <- "Directed dyad-wise shared partners of type ISP"
  return(d)
}

#' TITLE
#'
#' DETAIL
#' @param mat an adjacency matrix
#' @return vector of statistics
#' @import ergm
#' @export
desp_OTP <- function(mat)
{
  lim <- nrow(mat) - 2
  d <- summary(mat ~ ddsp(0:lim, type = "OTP"))
  d[1] <- nrow(mat) * (nrow(mat) - 1) + d[1]
  names(d) <- 0:(length(d) - 1)
  attributes(d)$label <- "Directed edge-wise shared partners of type OTP"
  return(d)
}

#' TITLE
#'
#' DETAIL
#' @param mat an adjacency matrix
#' @return vector of statistics
#' @import ergm
#' @export
desp_ITP <- function(mat)
{
  lim <- nrow(mat) - 2
  d <- summary(mat ~ ddsp(0:lim, type = "ITP"))
  d[1] <- nrow(mat) * (nrow(mat) - 1) + d[1]
  names(d) <- 0:(length(d) - 1)
  attributes(d)$label <- "Directed edge-wise shared partners of type ITP"
  return(d)
}

#' TITLE
#'
#' DETAIL
#' @param mat an adjacency matrix
#' @return vector of statistics
#' @import ergm
#' @export
desp_OSP <- function(mat)
{
  lim <- nrow(mat) - 2
  d <- summary(mat ~ ddsp(0:lim, type = "OSP"))
  d[1] <- nrow(mat) * (nrow(mat) - 1) + d[1]
  names(d) <- 0:(length(d) - 1)
  attributes(d)$label <- "Directed edge-wise shared partners of type OSP"
  return(d)
}

#' TITLE
#'
#' DETAIL
#' @param mat an adjacency matrix
#' @return vector of statistics
#' @import ergm
#' @export
desp_ISP <- function(mat)
{
  lim <- nrow(mat) - 2
  d <- summary(mat ~ ddsp(0:lim, type = "ISP"))
  d[1] <- nrow(mat) * (nrow(mat) - 1) + d[1]
  names(d) <- 0:(length(d) - 1)
  attributes(d)$label <- "Directed edge-wise shared partners of type ISP"
  return(d)
}

# function to tag whether given value (x) is in between of lower and upper limits
is.between <- function(x, lower, upper) {x > lower & x < upper}

# function to make standardized values from raw scale
z.var <- function(var) {
  mean <- mean(var, na.rm = T)
  sd <- sd(var, na.rm = T)
  z.var <- (var - mean)/sd
  z.var
}


## function to bypass the warnings when requesting the summary of models in which number of bootstraps are dropped due to insufficient variation


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

## function to recover percentage CI and bias-corrected CIs
require(boot)

perc.ci <- function(t, conf = 0.95, hinv = function(t) t) {
  alpha <- (1 + c(-conf, conf))/2
  qq <- boot:::norm.inter(t, alpha)
  out <- cbind(conf, matrix(qq[, 1L], ncol = 2L), matrix(hinv(qq[,2]), ncol = 2L))
  out[,4:5]
}

bca.ci <- function(dat, conf = 0.95, index = 1, t0 = mean(dat, na.rm = T),  t = NULL, L = NULL,
                   h = function(t) t, hdot = function(t) 1, hinv = function(t) t, ...) {
  t.o <- t
  t <- dat
  
  t <- t[is.finite(t)]
  w <- qnorm(sum(t < t0)/length(t))
  if (!is.finite(w)) 
    stop("estimated adjustment 'w' is infinite")
  alpha <- (1 + c(-conf, conf))/2
  zalpha <- qnorm(alpha)
  if (is.null(L)) 
    L <- mean(dat, na.rm = T) - t0 ## bias
  a <- sum(L^3)/(6 * sum(L^2)^1.5)
  if (!is.finite(a)) 
    stop("estimated adjustment 'a' is NA")
  adj.alpha <- pnorm(w + (w + zalpha)/(1 - a * (w + zalpha)))
  qq <- boot:::norm.inter(t, adj.alpha)
  out <- cbind(conf, matrix(qq[, 1L], ncol = 2L), matrix(hinv(h(qq[, 2L])), ncol = 2L))
  out[4:5]
}
