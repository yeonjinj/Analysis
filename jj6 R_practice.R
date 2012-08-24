nqs <- 4 # number of questions per item, replace with actual number
nitems <- 27 # number of items, replace with actual number
t0 <- readLines("jj6.txt") # replace with name of data
ns <- length(t0) # number of subjects (found automatically)
t00 <- unlist(lapply(t0,function(x) unlist(strsplit(x," "))[1:((nqs+1)*2*nitems+2)]))
t1 <- t(matrix(as.numeric(t00),(nqs+1)*2*nitems+2,ns))
a1 <- array(as.matrix(t1[,-c(1:3)]),c(ns,2*(nqs+1),nitems)) # Ss, resps, items
times <- apply(a1[,(nqs+3):(2*(nqs+1)),],c(1,3),max) # use fastest half for each S:
mtimes <- log(apply(apply(times,1,sort)[1:floor(nitems/2),],2,mean))
# omits <- which(mtimes<3.5) # for omitting bad subjects, replace the time
# t1 <- t1[-omits,]
# ns <- nrow(t1)
# a1 <- a1[-omits,,]
age <- t1[,1]
sex <- t1[,2] # 1=male, 0=female
Group <- t1[,3] # 1=victim, 0=dictator

## var PABCD=[[25,10,3,6,0], [25,10,0,6,0], [25,10,3,10,0],
##  [50,10,3,6,0], [50,10,0,6,0], [50,10,3,10,0],
##  [75,10,3,6,0], [75,10,0,6,0], [75,10,3,10,0]]
# P=probability of "winning" gamble
# A/B=outcomes for dic/vic if gamble won, C/D if lost
# no gamble is 3 each; A always 10; D always 0

## Case=2*Math.floor(itm/9)+Group // Group=1 for victim, 0 for dictator
## P=PABCD[itm % 9][0]
## A=PABCD[itm % 9][1]+" million"
## B=PABCD[itm % 9][2]+" million"
## C=PABCD[itm % 9][3]+" million"
## D=PABCD[itm % 9][4]+" million"

Item <- a1[,1,] # order of the item
Comp1 <- a1[,2,] # compensation if first outcome of gamble
Comp2 <- a1[,3,] # compensation if second outcome of gamble
Just <- a1[,4,]-3 # would compensation make gamble justifiable?
Penalty <- a1[,5,] # penalty for choosing gamble
Case <- rep(1:3,c(9,9,9))
P <- rep(c(25,25,25,50,50,50,75,75,75),3)
B <- rep(c(3,0,3),9)  #R's compensation depends more on R's outcomes. 
C <- rep(c(6,6,10),9) #R's compensation depends more on D's outcomes.
B1 <-rep(c(3,0,NA),9) #R's compensation distinguishes more bw [10,3* 6,0] than [10,0* 6,0].
C1 <-rep(c(6,NA,10),9) #R's compensation distinguishes more bw [10,3 10,0*] than [10,3 6,0*].
H <- rep(c(3,NA,3),9) 


# corrections based on individual outliers
Comp1[28,9] <- 5
Comp1[11,c(1,13)] <- 3.5
Comp2[47,25] <- 1
# Comp2[25,6] ????
Pen[25,6] <- 1
Comp1 <- t(apply(Comp1,1, function(x) {x/max(x)}))
Comp2 <- t(apply(Comp2,1, function(x) {x/max(x)}))
Pen <- t(apply(Penalty,1, function(x) {x/max(x)}))

Pen[is.nan(Pen)] <- 0


##t tests
#t.test(ifelse(Group==0,Comp1-Comp2,NA),na.rm=T) #R's compensation depended on the success of gamble, higher if lost than won (p=0.02)
#t.test(ifelse(Group==1,Comp1-Comp2,NA),na.rm=T) #D's compensation didn't depend on the success of gamble (p=0.67)
#t.test(ifelse(Group==0,Pen[,3]-Pen[,2]+Pen[,1],NA),na.rm=T,t.test(ifelse(Group==1,Pen[,3]-Pen[,2]+Pen[,1],NA),na.rm=T) 
) 

#means
# round(as.table(with(d1,by(Comp2,list(Group=Group,B=B),mean,na.rm=T))),2)
# round(as.table(with(d1,by(Comp1,list(Group=Group,B=B),mean,na.rm=T))),2)
# round(as.table(with(d1,by(Comp1,list(Group=Group,C=C),mean,na.rm=T))),2)
# round(as.table(with(d1,by(Comp2,list(Group=Group,C=C),mean,na.rm=T))),2)



##lmer
# long form
library(lme4)
d1 <- data.frame(S=gl(ns,1,ns*nitems),Case=rep(Case,rep(ns,27)),
                 B=rep(B,rep(ns,27)),B1=rep(B1,rep(ns,27)),
                 C=rep(C,rep(ns,27)),C1=rep(C1,rep(ns,27)),
                 P=rep(P,rep(ns,27)),H=rep(H,rep(ns,27)),
                 Comp1=c(Comp1),Comp2=c(Comp2),Just=c(Just),Pen=c(Pen),
                 Group=rep(Group,nitems),sex=rep(sex,nitems),age=rep(age,nitems))

lmer(Pen ~ Group*C + P + Case + (1|S),d1) #  Group and C pos and sig. When dictators can gain more, higher penalty Since B was not a sig predictor, 
lm1 <- lmer(Comp1 ~ Group*B + P + Case + (1|S),d1) # R cares more about R outcome
lm2 <- lmer(Comp2 ~ Group*C + P + Case + (1|S),d1) # R cares more about D outcome. t.test confirms this
lm3 <- lmer(Comp1 ~ Group*B + P*Group + Case + (1|S),d1) # no group*P interaction
lm4 <- lmer(Comp2 ~ Group*C + P*Group + Case + (1|S),d1) # no group*P interaction
lm1a <- lmer(Comp1 ~ Group*B1 + Case + (1|S),d1) # R distinguishes more bw [10,3* 6,0] vs [10,0* 6,0]
lm1b <- lmer(Comp1 ~ Group*C1 + Case + (1|S),d1) # R distinguishes more bw [10,3 10,0*] vs [10,3 6,0*].
lm5 <- lmer(Just ~ Group + Case + (1|S),d1) # D more so than R thinks compensation makes gambling justifiable
lm6 <- lmer(Pen ~ Group + P + Case + (1|S),d1) # R gives higher penalty. Penalty doesn't depend on probabilities for both R and D

