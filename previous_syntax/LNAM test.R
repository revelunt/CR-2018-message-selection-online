
## DV: relative candidate preference
relative.pref <- list(t1 = g[[1]] %v% "candidate.preference", 
                      t2 = g[[2]] %v% "candidate.preference", 
                      t3 = g[[3]] %v% "candidate.preference")
names(relative.pref[[1]]) <- g[[1]] %v% "vertex.names"
names(relative.pref[[2]]) <- g[[2]] %v% "vertex.names"
names(relative.pref[[3]]) <- g[[3]] %v% "vertex.names"

## covariates: online offline discussion, interest, knowledge, efficacy, ideology, participation, demographics

talk.freq <- list(t1 = g[[1]] %v% "talk.freq", t2 = g[[2]] %v% "talk.freq", t3 = g[[3]] %v% "talk.freq")
media.use <- list(t1 = g[[1]] %v% "media.use.freq", t2 = g[[2]] %v% "media.use.freq", t3 = g[[3]] %v% "media.use.freq")
interest <- list(t1 = g[[1]] %v% "interest", t2 = g[[2]] %v% "interest", t3 = g[[3]] %v% "interest")
efficacy <- list(t1 = g[[1]] %v% "internal.efficacy", t2 = g[[2]] %v% "internal.efficacy", t3 = g[[3]] %v% "internal.efficacy")
knowledge <- list(t1 = g[[1]] %v% "knowledge", t2 = g[[2]] %v% "knowledge", t3 = g[[3]] %v% "knowledge")
ideology <- list(t1 = g[[1]] %v% "pol.ideology", t2 = g[[2]] %v% "pol.ideology", t3 = g[[3]] %v% "pol.ideology")
age <- list(t1 = g[[1]] %v% "age", t2 = g[[2]] %v% "age", t3 = g[[3]] %v% "age" )
gender <- list(t1 = g[[1]] %v% "gender", t2 = g[[2]] %v% "gender", t3 = g[[3]] %v% "gender")
education <- list(t1 = g[[1]] %v% "edu", t2 = g[[2]] %v% "edu", t3 = g[[3]] %v% "edu")

model1 <- tnam(
  relative.pref ~ 
    # covariate(talk.freq, coefname = "discussion frequency") + 
    # covariate(media.use, coefname = "media use") +
    # covariate(interest, coefname = "pol interst") +
    # covariate(efficacy, coefname = "pol efficacy") +
    # covariate(knowledge, coefname = "knowledge") +
    # covariate(ideology, coefname = "pol ideology") +
    # covariate(age, coefname = "age in 10 years") +
    # covariate(gender, coefname = "being female") +
    # covariate(education, coefname = "education") +
    
    #covariate(relative.pref, lag = 1, exponent = 2) + ## autoregression

    netlag(relative.pref, g) + ## connected friend's influence
    netlag(relative.pref, g, pathdist = 2, decay = 1) + 
    netlag(relative.pref, g, lag = 1) + 
    
    degreedummy(g, deg = 0, reverse = TRUE), 
  re.time = TRUE, family = binomial(link = "logit")
)
summary(model1); screenreg(model1, digits = 4, leading.zero = T)
