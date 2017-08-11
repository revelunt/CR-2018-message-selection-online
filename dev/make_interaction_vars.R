
## make a series of interaction variables as edgecovariates

same_candidate_preference  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

same_candidate_preference_Park  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference", diff = TRUE, keep = c(1)), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

same_candidate_preference_Moon  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodematch("candidate.preference", diff = TRUE, keep = c(2)), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

time_trends <- lapply(1:3, function(i) {
  dim <- dim(as.matrix(g[[i]]))
  outmat <- matrix(i - 1, nrow = dim[1], ncol = dim[2])
  diag(outmat) <- 0
  outmat
})

time.X.same.candidate.preference_Park <- lapply(1:3, function(i) {
  outmat <- time_trends[[i]] * same_candidate_preference_Park[[i]]
  diag(outmat) <- 0
  outmat
})

time.X.same.candidate.preference_Moon <- lapply(1:3, function(i) {
  outmat <- time_trends[[i]] * same_candidate_preference_Moon[[i]]
  diag(outmat) <- 0
  outmat
})

time.X.same.candidate.preference <- lapply(1:3, function(i) {
  outmat <- time_trends[[i]] * same_candidate_preference[[i]]
  diag(outmat) <- 0
  outmat
})

ocov.consistency  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodeocov("consistency.motivation"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

ocov.understanding  <- lapply(1:3, function(i) {
  temp1.t <- ergmMPLE(g[[i]] ~ nodeocov("understanding.motivation"), output = "array")$predictor[,,1]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

ocov.consistency.X.time <- lapply(1:3, function(i) {
  temp1.t <- ocov.consistency[[i]] * time_trends[[i]]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

ocov.understanding.X.time <- lapply(1:3, function(i) {
  temp1.t <- ocov.understanding[[i]] * time_trends[[i]]
  dimnames(temp1.t) <- NULL
  diag(temp1.t) <- 0
  temp1.t})

ocov.consistency.X.preference.Moon <- lapply(1:3, function(i) {
  temp1.t <- ocov.consistency[[i]] * same_candidate_preference_Moon[[i]]
  diag(temp1.t) <- 0
  temp1.t
})

ocov.understanding.X.preference.Moon <- lapply(1:3, function(i) {
  temp1.t <- ocov.understanding[[i]] * same_candidate_preference_Moon[[i]]
  diag(temp1.t) <- 0
  temp1.t
})

ocov.consistency.X.preference.Park <- lapply(1:3, function(i) {
  temp1.t <- ocov.consistency[[i]] * same_candidate_preference_Moon[[i]]
  diag(temp1.t) <- 0
  temp1.t
})

ocov.understanding.X.preference.Park <- lapply(1:3, function(i) {
  temp1.t <- ocov.understanding[[i]] * same_candidate_preference_Moon[[i]]
  diag(temp1.t) <- 0
  temp1.t
})

ocov.consistency.X.time.X.preference.Moon <- lapply(1:3, function(i) {
  temp1.t <- ocov.consistency[[i]] * same_candidate_preference_Park[[i]]
  diag(temp1.t) <- 0
  temp1.t
})

ocov.understanding.X.time.X.preference.Moon <- lapply(1:3, function(i) {
  temp1.t <- ocov.understanding[[i]] * same_candidate_preference_Park[[i]]
  diag(temp1.t) <- 0
  temp1.t
})

ocov.consistency.X.time.X.preference.Park <- lapply(1:3, function(i) {
  temp1.t <- ocov.consistency[[i]] * time_trends[[i]] * same_candidate_preference_Park[[i]]
  diag(temp1.t) <- 0
  temp1.t
})

ocov.understandingy.X.time.X.preference.Park <- lapply(1:3, function(i) {
  temp1.t <- ocov.understanding[[i]] * time_trends[[i]] * same_candidate_preference_Park[[i]]
  diag(temp1.t) <- 0
  temp1.t
})

