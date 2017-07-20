
options(scipen = 999)
rm(list=ls())
setwd("~/Dropbox/(17) 2017 Spring/network QAP Korean election")
library(foreign)
library(plyr)

if (!("haven" %in% installed.packages()[,"Package"])) install.packages("haven")
if (!("data.table" %in% installed.packages()[,"Package"])) install.packages("data.table")
if (!("asnipe" %in% installed.packages()[, "Package"])) install.packages("asnipe")
library(haven)
library(data.table)
library(sna)
library(igraph)

## run custom functions
source("btergm helper-functions.R")

## load the dataset from SPSS and set as data.table object
dat <- haven::read_spss("Dat/Survey Data for Network Matrix Data.sav")
setDT(dat)

## copy the original data for later use (just in case...)
dat.original <- dat

## based on selective columns, remove all cases with NAs
dat <- na.omit(dat[, c('r_id', 'p_image', 'm_image', 'ide_self', 'evalcrit1', 'evalcrit2', 
                       'policy_c', 'policy_l', 'knowtotal', 'talk', 'interest', "female", "edu", "age", "opleader", "follower", "income")])

## this yeilds a total of 312 cases
lengthn <- dat[, .N]
lengthn

# VARIABLE 1 : canddiate IMAGE
# p_image - Park (elected) ppls evaluation of each candidate based on their political image
# m_image - Moon evaluation
# construct into euclidean distance (2 vbls into 1)

## assuming two dimensions, dist of A(x1, y1) and B(x2, y2) is
## sqrt( (x1 - x2)^ + (y1 - y2)^ )

candidate.image <- dat[, sqrt(outer(p_image, p_image, "-")^2 + outer(m_image, m_image, "-")^2)] 
hist(candidate.image) ## represent dyadic evaluation (euclidean) difference between Xij

# candidate.image.park.diff <- dat[, abs(outer(p_image, p_image, "-"))] 
# candidate.image.moon.diff <- dat[, abs(outer(m_image, m_image, "-"))] 
# ## graph correlation is 0.25 (with p-value < .001) -- maybe entered jountly? 
candidate.image2 <- dat[, abs(outer(p_image - m_image, p_image - m_image, "-"))]

# evaluative criteria 
# evalcrit1 - competence
# evalcrit2 - background
evaludative.criteria <- dat[, sqrt(outer(evalcrit1, evalcrit1, "-")^2 + outer(evalcrit2, evalcrit2, "-")^2)]
hist(evaludative.criteria) ## represent dyadic evaluative criteria (euclidean) difference between Xij

## absolute difference of [abs(evalcrit)]
evaludative.criteria2 <- dat[, - abs(abs(outer(evalcrit1, evalcrit1, "-")) - abs(outer(evalcrit2, evalcrit2, "-")))]

# ide_self - lower score liberal, higher conservative
## recode to ide_self.r
dat[, ide_self.r := car::recode(ide_self, "'Very Conservative' = 7; 'Conservative' = 6;
                              'Somewhat Conservative' = 5; 'Middle' = 4;
                              'Somewhat Liberal' = 3; 'Liberal' = 2;
                              'Very Liberal' = 1", as.numeric.result = T)]
table(dat[, ide_self.r])

ideology.diff <- dat[, abs(outer(ide_self.r, ide_self.r, "-"))]  ## "+" means more dissimilarity
hist(ideology.diff)



# Policy preference Euclidean (2 vbles to 1)
# policy_c : 2 survey Qs: how much you agree with certain Qs - more conservative stance on issue
# policy_l: how much you agree with certain Qs - people take more liberal stance on issue
# we may just make single difference measure (policy_total) between policy_c and policy_l, and construct dyadic difference out of policy_total
policy.pref <- dat[, abs(outer(policy_c - policy_l, policy_c - policy_l, "-"))]
hist(policy.pref) ## represent dyadic policy stance distance between Xij

# previous communication
prev.comm <- read.csv("Dat/Reading_1113-1126_Participants(N=341)_Count(N=160836).csv")

source <- prev.comm[,2]  ## reader (who select)
target <- prev.comm[,3]  ## poster (who selected)
prev.comm <- data.frame(source, target) ## reader selecting poster's messages
g <- induced_subgraph(graph.data.frame(prev.comm, directed = TRUE,  vertices = 1:341),  vids = dat$r_id)

# average message selection instance
mean(graph.strength(g, mode = 'out'))
prev.comm <- get.adjacency(g, sparse = F)


# controls
# knowtotal: pol knowledge, talk: talk freq, interest, female, edu, age, opleader and follower
# just construct mat

# talk: need to do a littl transformation

# create talk.r variable
dat[, talk.r := as.numeric(substr(talk, 1,1))]

control.2 <- process.node.vars(dat, vars = c("female", "edu", "age", "income", "talk.r", "interest", "follower"), type = "ego")
control.3 <- process.node.vars(dat, vars = c("knowtotal", "opleader"), type = "alter")

## RESPONSE MATRIX
g <- read.csv("Dat/Reading_1127-1219_Participants(N=341)_Count(N=160836).csv")
source_reader <- g[,2]
target_poster <- g[,1]
g <- data.frame(source_reader, target_poster)

g <- induced_subgraph(graph.data.frame(g, directed = TRUE, vertices = 1:341), vids = dat$r_id)

mean(graph.strength(g, mode = 'in'))
g <- get.adjacency(g, sparse = FALSE)


##------------------------------------##
#   MR QAP using muticore processing   #
##------------------------------------##

predictor.matrices <- c(list(prev.comm), control.2, control.3, 
                        list(candidate.image2, evaludative.criteria, ideology.diff, policy.pref)) ## 14 predictors
names(predictor.matrices) <- c("prev.comm", "female.ego", "edu.ego", "age.ego", "income.ego", "talkfreq.ego", "interest.ego", "submissive.ego",
                               "knowledge.alter", "opionated.alter", "candidate.image.diff", 
                               "evaluative.criteria.diff", "ideology.diff", "policy.pref.diff")


female.ego <- control.2[[1]]
edu.ego <- control.2[[2]]
age.ego <- control.2[[3]]
income.ego <- control.2[[4]]
talkfreq <- control.2[[5]]
interest.ego <- control.2[[6]]
submissive.ego <- control.2[[7]]
knowledge.alter <- control.3[[1]]
opionated.alter <- control.3[[2]]

RNGkind("L'Ecuyer-CMRG")
set.seed(12345, "L'Ecuyer")

## this takes a good amount of time, but should be faster than simple looping (for loop) that previous function relies on..
nl2 <- mrqap.dsp.multicore(g ~ prev.comm + female.ego + edu.ego + age.ego + income.ego + talkfreq + interest.ego + submissive.ego +
                           knowledge.alter + opionated.alter + candidate.image2 + evaludative.criteria + 
                           ideology.diff + policy.pref, 
                         intercept = TRUE, directed = "directed", diagonal = FALSE, test.statistic = "t-value", 
                         tol = 1e-07, randomisations = 1000)
nl2$names <- c("Intercept", "prev.comm", "female.ego", "edu.ego", "age.ego", "income.ego",
               "talkfreq.ego", "interest.ego", "submissive.ego", "knowledge.alter", "opionated.alter",
               "candidate.image.diff", "evaludative.criteria.diff", "ideology.diff", "policy.pref")

print(nl2)

# interaction.eval.candimg <- evaludative.criteria * candidate.image2
# 
# nl3 <- mrqap.dsp.multicore(g ~ prev.comm + female.ego + edu.ego + age.ego + income.ego + talkfreq + interest.ego + submissive.ego +
#                              knowledge.alter + opionated.alter + candidate.image2 + evaludative.criteria + 
#                              ideology.diff + policy.pref + 
#                              interaction.eval.candimg, intercept = TRUE, directed = "directed", diagonal = FALSE, test.statistic = "t-value", 
#                            tol = 1e-07, randomisations = 1000) ## not sig

interaction.eval.know.alter <- evaludative.criteria * knowledge.alter

nl4 <- netlm.multicore(g ~ prev.comm + female.ego + edu.ego + age.ego + income.ego + talkfreq + interest.ego + submissive.ego +
                             knowledge.alter + opionated.alter + candidate.image2 + evaludative.criteria + 
                             ideology.diff + policy.pref + 
                             interaction.eval.know.alter, intercept = TRUE, directed = "directed", diagonal = FALSE, test.statistic = "t-value", 
                           tol = 1e-07, randomisations = 1000)

print(nl4)





###################
#QAP correlations
###################
q12 <- qaptest(predictor.matrices, gcor, g1=1, g2=2)
q13 <- qaptest(predictor.matrices, gcor, g1=1, g2=3)
q14 <- qaptest(predictor.matrices, gcor, g1=1, g2=4)
q15 <- qaptest(predictor.matrices, gcor, g1=1, g2=5)
q16 <- qaptest(predictor.matrices, gcor, g1=1, g2=6)
q17 <- qaptest(predictor.matrices, gcor, g1=1, g2=7)
q18 <- qaptest(predictor.matrices, gcor, g1=1, g2=8)

q23 <- qaptest(response_matrices, gcor, g1=2, g2=3)
q24 <- qaptest(response_matrices, gcor, g1=2, g2=4)
q25 <- qaptest(response_matrices, gcor, g1=2, g2=5)
q26 <- qaptest(response_matrices, gcor, g1=2, g2=6)
q27 <- qaptest(response_matrices, gcor, g1=2, g2=7)
q28 <- qaptest(response_matrices, gcor, g1=2, g2=8)

q34 <- qaptest(response_matrices, gcor, g1=3, g2=4)
q35 <- qaptest(response_matrices, gcor, g1=3, g2=5)
q36 <- qaptest(response_matrices, gcor, g1=3, g2=6)
q37 <- qaptest(response_matrices, gcor, g1=3, g2=7)
q38 <- qaptest(response_matrices, gcor, g1=3, g2=8)

q45 <- qaptest(response_matrices, gcor, g1=4, g2=5)
q46 <- qaptest(response_matrices, gcor, g1=4, g2=6)
q47 <- qaptest(response_matrices, gcor, g1=4, g2=7)
q48 <- qaptest(response_matrices, gcor, g1=4, g2=8)

q56 <- qaptest(response_matrices, gcor, g1=5, g2=6)
q57 <- qaptest(response_matrices, gcor, g1=5, g2=7)
q58 <- qaptest(response_matrices, gcor, g1=5, g2=8)

q67 <- qaptest(response_matrices, gcor, g1=6, g2=7)
q68 <- qaptest(response_matrices, gcor, g1=6, g2=8)

summary(q12)
summary(q13)
summary(q14)
summary(q15)
summary(q16)

summary(q17)
summary(q18)
summary(q23)
summary(q24)
summary(q25)
summary(q26)
summary(q27)
summary(q28)
summary(q34)
summary(q35)
summary(q36)
summary(q37)
summary(q38)
summary(q45)
summary(q46)
summary(q47)
summary(q48)
summary(q56)
summary(q57)
summary(q58)
summary(q67)
summary(q68)
summary(q78)
