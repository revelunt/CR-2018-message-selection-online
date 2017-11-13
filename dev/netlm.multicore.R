

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


# extension for qap objects
extract.qap <- function(model, include.F = TRUE, include.Rsq = TRUE,
                             include.adj.Rsq = TRUE, ...) {
  mss<-if(model$intercept)
    sum((fitted(model)-mean(fitted(model)))^2)
  else
    sum(fitted(model)^2)
  rss<-sum(resid(model)^2)
  qn<-NROW(model$qr$qr)
  df.int<-model$intercept
  rdf<-qn-model$rank
  resvar<-rss/rdf
  fstatistic<-c(value=(mss/(model$rank-df.int))/resvar,numdf=model$rank-df.int, dendf=rdf)
  r.squared<-mss/(mss+rss)
  adj.r.squared<-1-(1-r.squared)*((qn-df.int)/rdf)
  sigma<-sqrt(resvar)
  
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.F == TRUE) {
    gof <- c(gof, fstatistic[1],  fstatistic[2], fstatistic[3])
    gof.names <- c(gof.names, "F-statistic", "df1", "df2")
    gof.decimal <- c(gof.decimal, TRUE, TRUE, TRUE)
  }
  if (include.Rsq == TRUE) {
    gof <- c(gof, r.squared)
    gof.names <- c(gof.names, "Multiple R-squared")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.adj.Rsq == TRUE) {
    gof <- c(gof, adj.r.squared)
    gof.names <- c(gof.names, "Adjusted R-squared")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  cf <- model$coefficients
  pv <- model$pgreqabs
  nm <- c("(Intercept)", paste0("x", 1:(length(cf) - 1)))
  if (is.null(model$dist)) {  # "classical" fit (= simple logit model)
    cvm <- chol2inv(model$qr$qr)
    se <- sqrt(diag(cvm))
  } else {  # QAP, CUG etc.
    se <- rep(NaN, length(cf))  # not perfect; results in empty brackets!
  }
  
  tr <- createTexreg(
    coef.names = nm,
    coef = cf,
    se = se,
    pvalues = pv,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

setMethod("extract", signature = className("netlm", "sna"),
          definition = extract.qap)
