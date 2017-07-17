
library(pbapply)

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
                                     "g_lagcyclic", "policy.pref.sim", "evaludative.criteria.sim"))
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


error.bar <- function(x, y, upper, lower, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
