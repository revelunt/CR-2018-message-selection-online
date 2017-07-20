
library(foreign)

library(plyr)
dat<-read.spss("Survey Data for Network Matrix Data.sav", to.data.frame=TRUE)

dat2<- data.frame(dat$r_id, dat$p_image, dat$m_image, dat$ide_self, dat$evalcrit1, dat$evalcrit2, dat$policy_c, dat$policy_l,
                  dat$knowtotal, dat$talk, dat$interest)
dat<- na.omit(dat2)
colnames(dat) <- c('r_id', 'p_image', 'm_image', 'ide_self', 'evalcrit1', 'evalcrit2', 'policy_c', 'policy_l', 'knowtotal',
                    'talk', 'interest')
##################################3
#   VARIABLE 1 : IMAGE
#p_image - park (elected) ppls evaluation of each candidate based on their political image
#m_image - moon
#construct into euclidean distance (2 vbls into 1)

r_id<-dat$r_id

p_image<-dat$p_image

df <- data.frame(r_id, p_image)

#absolute difference between each pair
p_imagemat <- with(df, abs(outer(df$p_image, df$p_image, "-")))
head(data.frame(p_imagemat))

#m_image
m_image<-dat$m_image

df<-data.frame(r_id, m_image)

m_imagemat <- with(df, abs(outer(df$m_image, df$m_image, "-")))


#Euclidean distance

v1_image<- -abs(p_imagemat - m_imagemat)
v1_image<-as.matrix(v1_image)

########################################################################
#6 
#evalcrit1 - how we evaluate candidates - what facotrs -personal qualifications
#evalcrit2 - background homophily

#evalcrit1
evalcrit1<-dat$evalcrit1

df<-data.frame(r_id, evalcrit1)
evalcrit1mat<-with(df, abs(outer(df$evalcrit1, df$evalcrit1, "-")))

#evalcrit2
evalcrit2<-dat$evalcrit2

df<-data.frame(r_id, evalcrit2)
evalcrit2mat<-with(df, abs(outer(df$evalcrit2, df$evalcrit2, "-")))


v6_evalcrit<-as.matrix( -abs(evalcrit1mat - evalcrit2mat))
############################################
#3
#ide_self - lower score liberal, higher conservative
#ide_fam
#ide_friends
#can create Euclidean using the three variables


ide_self<-dat$ide_self

ide_self<-revalue(ide_self, c("Very Conservative" = 7, "Conservative" = 6,
                              "Somewhat Conservative" = 5, "Middle" = 4,
                              "Somewhat Liberal" = 3, "Liberal" = 2,
                              "Very Liberal" = 1))
ide_self<-as.numeric(ide_self)
df<-data.frame(r_id, ide_self)
ide_self <- table(r_id, ide_self)
ide_selfmat<- - with(df, abs(outer(df$ide_self, df$ide_self, "-")))
v4_ideself<-ide_selfmat
################################################

#5 Policy Euclidean (2 vbles to 1)
#policy_c : 2 survey Qs: how much you agree with certain Qs - more conservative stance on issue

#policy_l: how much you agree with certain Qs - people take more liberal stance on issue

policy_c<-dat$policy_c

df<-data.frame(r_id, policy_c)
policy_cmat<-with(df, abs(outer(df$policy_c, df$policy_c, "-")))

#policy_l

policy_l<-dat$policy_l

df<-data.frame(r_id, policy_l)
policy_lmat<-with(df, abs(outer(df$policy_l, df$policy_l, "-")))

v5_policy<- as.matrix(  -abs(policy_cmat - policy_lmat) )

#################################################
#c12
#previous communication
prev_comm<-read.csv("Reading_1113-1126_Participants(N=341)_Count(N=160836).csv")

source<- prev_comm[,2]
target<- prev_comm[,3]

prev_comm<- data.frame(source, target)
library(igraph)
nodes = 1:341

g<-graph.data.frame(prev_comm, directed = TRUE,  vertices = nodes)

g2<- induced_subgraph(g, vids = dat$r_id)

#average
mean(graph.strength(g2, mode = 'out'))

y_matrix<- get.adjacency(g2, sparse =F)
c12_prevcomm<-data.frame(y_matrix)
table(rowSums(c12_prevcomm))

###################################
#c1 control
#knowtotal: pol knowledge
#just construct mat

knowtotal<-dat$knowtotal
df<-data.frame(r_id, knowtotal)

c1_knowtotal<- - with(df, abs(outer(df$knowtotal, df$knowtotal, "-")))
##############################################################
#c6 control
# talk: need to do a littl transformation

talk<-dat$talk
talk<-as.numeric(substr(talk, 1,1))
df<-data.frame(r_id, talk)
c6_talk<- - with(df, abs(outer(df$talk, df$talk, "-")))
##########################################################
#c5 control
#interest
interest<-dat$interest
df<-data.frame(r_id, interest)
c5_interest<- - with(df, abs(outer(df$interest, df$interest, "-")))
##########################################


###y

y_edge<-read.csv("Reading_1127-1219_Participants(N=341)_Count(N=160836).csv")
source_reader<-y_edge[,2]
target_poster<- y_edge[,1]
y_edge_df<-data.frame(source_reader, target_poster)

node= 1:341

g<-graph.data.frame(y_edge_df, directed = TRUE, vertices = node)

g2 <- induced_subgraph(g, vids = dat$r_id)
mean(graph.strength(g2, mode = 'in'))
y_mat<-get.adjacency(g2, sparse = FALSE)
y_df<- data.frame(y_mat)
y<-stack(y_df )
y<-y[,1]


##########################################
#     #MR QAP - Really slow
#   See: http://webcache.googleusercontent.com/search?q=cache:http://sna.stanford.edu/sna_R_labs/output/lab_7/console_output.txt
#
#####################################################################33
c12_prevcomm_m<- as.matrix(c12_prevcomm)
c1_knowtotal_m<- as.matrix(c1_knowtotal)
c6_talk_m<- as.matrix(c6_talk)
c5_interest_m<-as.matrix(c5_interest)
v1_image_m<- as.matrix(v1_image)
v6_evalcrit_m<- as.matrix(v6_evalcrit)
v4_ideself_m<- as.matrix(v4_ideself)
v5_policy_m <- as.matrix(v5_policy)

library(sna)

#control model
response_matrices2<-array(NA, c(4,312,312))
response_matrices2[1,,]<- c12_prevcomm_m
response_matrices2[2,,]<- c1_knowtotal_m
response_matrices2[3,,]<- c6_talk_m
response_matrices2[4,,]<- c5_interest_m
library("sna")
nl2<-netlm(y_mat, response_matrices2,  nullhyp = c("qapspp"))
print(nl2)

#full model
response_matrices<-array(NA, c(8,341,341))
response_matrices[1,,]<- c12_prevcomm_m
response_matrices[2,,]<- c1_knowtotal_m
response_matrices[3,,]<- c6_talk_m
response_matrices[4,,]<- c5_interest_m
response_matrices[5,,]<- v1_image_m
response_matrices[6,,]<- v6_evalcrit_m
response_matrices[7,,]<- v4_ideself_m
response_matrices[8,,]<- v5_policy_m



library("sna")
nl<-netlm(y_mat, response_matrices, nullhyp = c("qapspp"))
print(nl)

###################
#QAP correlations
###################
q12 <- qaptest(response_matrices, gcor, g1=1, g2=2)
q13 <- qaptest(response_matrices, gcor, g1=1, g2=3)
q14 <- qaptest(response_matrices, gcor, g1=1, g2=4)
q15 <- qaptest(response_matrices, gcor, g1=1, g2=5)
q16 <- qaptest(response_matrices, gcor, g1=1, g2=6)
q17 <- qaptest(response_matrices, gcor, g1=1, g2=7)
q18 <- qaptest(response_matrices, gcor, g1=1, g2=8)

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
