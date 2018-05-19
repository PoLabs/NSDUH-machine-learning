setwd()
NSDUH2004 <- read.table(file = 'NSDUH2004.tsv', header = TRUE)
NSDUH2005 <- read.table(file = 'NSDUH2005.tsv', header = TRUE)
NSDUH2006 <- read.table(file = 'NSDUH2006.tsv', header = TRUE)
NSDUH2007 <- read.table(file = 'NSDUH2007.tsv', header = TRUE)
NSDUH2008 <- read.table(file = 'NSDUH2008.tsv', header = TRUE)
NSDUH2009 <- read.table(file = 'NSDUH2009.tsv', header = TRUE)
NSDUH2010 <- read.table(file = 'NSDUH2010.tsv', header = TRUE)
NSDUH2011 <- read.table(file = 'NSDUH2011.tsv', header = TRUE)
NSDUH2012 <- read.table(file = 'NSDUH2012.tsv', header = TRUE)
NSDUH2013 <- read.table(file = 'NSDUH2013.tsv', header = TRUE)
NSDUH2014 <- read.table(file = 'NSDUH2014.tsv', header = TRUE)

library(car)
library(gdata)
library(Matrix)
library(dplyr)
allNSDUH <- dplyr::bind_rows(NSDUH2004, NSDUH2005, NSDUH2006, NSDUH2007, NSDUH2008, NSDUH2009, NSDUH2010, NSDUH2011, NSDUH2012, NSDUH2013, NSDUH2014)
rm(NSDUH2004, NSDUH2005, NSDUH2006, NSDUH2007, NSDUH2008, NSDUH2009, NSDUH2010, NSDUH2011, NSDUH2012, NSDUH2013, NSDUH2014)


#goal: identify past year abuse from variables, exclude DEP/AB dx variables, possibly exclude criteria?
#identify most predictive variables via ML
#use predictive variables to train model
#cross validate with internal
#external validate with new NSDUH years

#A. extract useful variables and add to aiNSDUH2 df, recode NA's
#B. normalize cont variables
#C. one-hot-pot binary cat variables

allNSDUH$ABODOP <- 0
allNSDUH$ABODOP[allNSDUH$ABUSEHER == 1 | allNSDUH$ABUSEANL == 1 | allNSDUH$DEPNDANL == 1 | allNSDUH$DEPNDHER == 1] <- 1

realIR.df <- allNSDUH[,grep("IR", colnames(allNSDUH))]
realIR.df <- realIR.df[, !names(realIR.df) %in% c("AUALCHIR", "CIGIRTBL", "DSTTIRE", "FIRSTPOS", "MHALCHIR", "TSTHIRE", "WRKHIRE")] 
realIR.df <- as.data.frame(realIR.df)

#1. Drug use 
#2. Imputed sub use ??? remove II
rcIR.df <- realIR.df[,1:23] #recency 
rcIR.df <- apply(rcIR.df, 2, function(x) {x <- recode(x, "9=NA"); x})
aiNSDUH2 <- as.data.frame(rcIR.df)
yrIR.df <- realIR.df[,24:36] #past yr
yrIR.df <- apply(yrIR.df, 2, function(y) {y <- recode(y, "991=0; 993=0"); y})
aiNSDUH2 <- cbind(aiNSDUH2, yrIR.df)
moIR.df <- realIR.df[,37:48] #past mo
moIR.df <- apply(moIR.df, 2, function(z) {z <- recode(z, "91=0; 93=0"); z})
aiNSDUH2 <- cbind(aiNSDUH2, moIR.df)
timeIR.df <- realIR.df[,49:92] #age first use, year: 991>NA, 9999>NA
#check make sure 1991 not now NA
timeIR.df <- apply(timeIR.df, 2, function(a) {a <- recode(a, "991=NA; 9999=NA"); a})
#PriorMo.df<- as.data.frame(PriorDrug.df[,grep("MLU", colnames(PriorDrug.df))])
timeIR.df <- as.data.frame(timeIR.df[,-grep("YFU", colnames(timeIR.df))])
timeIR.df <- as.data.frame(timeIR.df)
aiNSDUH2 <- cbind(aiNSDUH2, timeIR.df)
rm(timeIR.df, moIR.df, yrIR.df, rcIR.df, realIR.df)
#0. Demo: CATAG3 (not AGE2), NOMARR2 (94,97,99>NA), SERVICE (94,98,99), MILSTAT 97,98,99, HEALTH (94,97), IRSEX, IRMARIT (99), IREDUC2 , NEWRACE2
Demo.df <- allNSDUH[c("CATAG3", "NOMARR2", "SERVICE", "MILSTAT", "HEALTH", "IRSEX", "IRMARIT", "IREDUC2", "NEWRACE2")]
Demo.df <- apply(Demo.df, 2, function(l) {l <- recode(l, "94=NA; 97=NA; 98=NA; 99=NA; 85=NA"); l})
Demo.df <- as.data.frame(Demo.df)
aiNSDUH2 <- cbind(aiNSDUH2, Demo.df)
#3. Risk/avail: both should be 94,97,98>NA (all have RK) #1057-1077
RiskAv.df <- allNSDUH[,1057:1077]
RiskAv.df$RKFQDBLT[RiskAv.df$RKFQDBLT == 5] <- NA
RiskAv.df <- apply(RiskAv.df, 2, function(b) {b <- recode(b, "94=NA; 97=NA; 98=NA"); b})
aiNSDUH2 <- cbind(aiNSDUH2, RiskAv.df)
#4. Spc topics: 94,97,98>NA.df <- as.data.frame(allNSDUH[,grep(?????)])   #1408-1432
SpcTopicA.df <- allNSDUH[c(1408, 1410:1427)]
SpcTopicA.df$BKOTH[SpcTopicA.df$BKOTH == 5] <- 1
SpcTopicA.df <- apply(SpcTopicA.df, 2, function(c) {c <- recode(c, "3=1; 99=2; 89=2; 85=NA; 94=NA; 97=NA; 98=NA; 81=2; 91=2"); c})
SpcTopicA.df <- as.data.frame(SpcTopicA.df)
aiNSDUH2 <- cbind(aiNSDUH2, SpcTopicA.df)
SpcTopicB.df <- allNSDUH[c(1428:1432)]
SpcTopicB.df <- apply(SpcTopicB.df, 2, function(o) {o <- recode(o, "81=NA; 85=NA; 89=2; 91=NA; 94=NA; 97=NA; 98=NA; 99=NA"); o})
SpcTopicB.df <- as.data.frame(SpcTopicB.df)
aiNSDUH2 <- cbind(aiNSDUH2, SpcTopicB.df)
#continous: 994, 997, 998, 999=0?, # times booked
allNSDUH$NOBOOKY[allNSDUH$NOBOOKY == 994 | allNSDUH$NOBOOKY == 997 | allNSDUH$NOBOOKY == 998] <- NA
allNSDUH$NOBOOKY[allNSDUH$NOBOOKY ==  999] <- 0
aiNSDUH2$NOBOOKY <- allNSDUH$NOBOOKY
rm(SpcTopicA.df, SpcTopicB.df, RiskAv.df, Demo.df)
#5. Mj purchases
#6. Prioir sub use: use/month, age, year (MRJYRBFR/MRJMLU, MRJAGLST, MRJYLU)
PriorDrug.df <- allNSDUH[c(1494:1562)]
PriorDrug.df$MRJYRBFR[PriorDrug.df$MRJYRBFR == 94 | PriorDrug.df$MRJYRBFR == 97 | PriorDrug.df$MRJYRBFR == 98] <- NA
PriorDrug.df$MRJYRBFR[PriorDrug.df$MRJYRBFR == 91] <- 2
aiNSDUH2$MRJYRBFR <- PriorDrug.df$MRJYRBFR
PriorAge.df <- as.data.frame(PriorDrug.df[,grep("AGLST", colnames(PriorDrug.df))])
PriorAge.df <- apply(PriorAge.df, 2, function(d) {d <- recode(d, "985=NA; 989=NA; 991=NA; 993=NA; 994=NA; 997=NA; 998=NA; 999=NA"); d})
aiNSDUH2 <- cbind(aiNSDUH2, PriorAge.df)
#use timeline
PriorTimeA.df <- allNSDUH[c(1558:1560)]
PriorTimeA.df <- apply(PriorTimeA.df, 2, function(g) {g <- recode(g, "3=1; 4=2; 11=1; 12=2; 89=NA; 85=NA; 91=NA; 94=NA; 97=NA; 98=NA; 99=NA"); g})
aiNSDUH2 <- cbind(aiNSDUH2, PriorTimeA.df)
PriorTimeB.df <- allNSDUH[c(1561, 1562)]
PriorTimeB.df <- apply(PriorTimeB.df, 2, function(h) {h <- recode(h, "4=1; 5=2; 6=3; 11=1; 12=2; 13=3; 85=NA; 89=NA; 91=NA; 94=NA; 97=NA; 98=NA; 99=NA"); h})
aiNSDUH2 <- cbind(aiNSDUH2, PriorTimeB.df)
#7. Sub tx
#8. Preg + hc
PHCcat.df <- allNSDUH[c(1852, 1854, 1856:1859)]
PHCcat.df <- apply(PHCcat.df, 2, function(i) {i <- recode(i, "89=NA; 94=NA; 97=NA; 98=NA; 99=NA"); i})
PHCcont.df <- allNSDUH[c(1853, 1855)]
PHCcont.df <- apply(PHCcont.df, 2, function(j) {j <- recode(j, "994=NA; 997=NA; 998=NA; 993=NA; 999=NA"); j}) 
aiNSDUH2 <- cbind(aiNSDUH2, PHCcat.df)
aiNSDUH2 <- cbind(aiNSDUH2, PHCcont.df)
#9. Adult MH
#10. Social/hood
allNSDUH$SNMOV5Y2[allNSDUH$SNMOV5Y2 == 985 | allNSDUH$SNMOV5Y2 == 989 | allNSDUH$SNMOV5Y2 == 994 | allNSDUH$SNMOV5Y2 == 997 | allNSDUH$SNMOV5Y2 == 998 | allNSDUH$SNMOV5Y2 == 999] <- NA
aiNSDUH2$SNMOV5Y2 <- allNSDUH$SNMOV5Y2
SocEnv.df <- allNSDUH[c(2002:2011, 2013:2020)]
SocEnv.df <- apply(SocEnv.df, 2, function(k) {k <- recode(k, "85=NA; 89=NA; 94=NA; 97=NA; 98=NA; 99=NA"); k})
aiNSDUH2 <- cbind(aiNSDUH2, SocEnv.df)
#11. Youth exp - remove: YETLKNON, YETLKPAR, YETLKBGF, YETLKOTA, YETLKSOP
#12. Adult MDD
#Consumption of alcohol 
#13. Household: IRFAMSZ2, IRKIDFA2, IRHH65_2, IRKI17_2, IRHHSIZ2, NRCH17_2 (.toNA), IFATHER, IMOTHER, EDFAM18 (98>NA\)
HH.df <- allNSDUH[c(2544, 2546, 2542, 2540, 2538, 2537, 2536, 2535, 2534)]
HH.df$NRCH17_2[HH.df$NRCH17_2 == -9] <- NA
#xtabs(~HH.df$NRCH17_2)
HH.df$EDFAM18[HH.df$EDFAM18 == 98] <- NA
aiNSDUH2 <- cbind(aiNSDUH2, HH.df)
#14. Income/insurance, recode: GOVTPROG, income poverty, irinsur4, othins
IncInsur.df <- allNSDUH[c(2623, 2624, 2625, 2629, 2631)]
aiNSDUH2 <- cbind(aiNSDUH2, IncInsur.df)
rm(HH.df, IncInsur.df, PHCcat.df, PHCcont.df, PriorAge.df, PriorDrug.df, PriorTimeA.df, PriorTimeB.df, SocEnv.df)
#15. Employment/work: JBSTATR2
#cat: WRKHAVJB, WRKEDYR, WRKJOBS2, WRKUNEMP, cutWRKBZCY2, LOCSIZE, WORKBLAH, WORKDAYS, DRGPLCY, USALCTST, USDRGTST, TSTHIRE, TSTRAND, FIRSTPOS      94, 97, 98, 99 
EmpCat.df <- allNSDUH[c(2683, 2680, 2673, 2674, 2679, 2681, 2682, 2687, 2690, 2689, 2688, 2691, 2695, 2696, 2697, 2698, 2699)]
EmpCat.df$WRKSLFEM[EmpCat.df$WRKSLFEM == 3 | EmpCat.df$WRKSLFEM == 5] <- 1
EmpCat.df <- apply(EmpCat.df, 2, function(m) {m <- recode(m, "85=NA; 94=NA; 97=NA; 98=NA; 99=NA"); m})
aiNSDUH2 <- cbind(aiNSDUH2, EmpCat.df)
aiNSDUH2 <-  aiNSDUH2[ , -which(names(aiNSDUH2) %in% c("WRKBZCY2"))]
allNSDUH$WRKHRSW2[allNSDUH$WRKHRSW2 == 994 | allNSDUH$WRKHRSW2 == 997 | allNSDUH$WRKHRSW2 == 998] <- NA
allNSDUH$WRKHRSW2[allNSDUH$WRKHRSW2 == 999] <- 0
aiNSDUH2$WRKHRSW2 <- allNSDUH$WRKHRSW2
#16. Geography
aiNSDUH2$PDEN00 <- allNSDUH$PDEN00
rm(EmpCat.df)
CatVars.df <- aiNSDUH2[ , which(names(aiNSDUH2) %in% c("MILSTAT", "IRMARIT", "NEWRACE", "BKOTH", "USEALCG", "USEMJCG", "USEALMJ", "USEACM", "USENEXT", "DOMOSTYR", "TYPEPBLM", "IFATHER", "IMOTHER", "JBSTATR", "FIRSTPOS", "PDEN"))]
#convert CAT to binary
CatVars.df$NEWRACE <- CatVars.df$NEWRACE2
CatVars.df$NEWRACE2 <- NULL
CatVars.df$JBSTATR <- CatVars.df$JBSTATR2
CatVars.df$JBSTATR2 <- NULL
CatVars.df$WRKBZCY <- CatVars.df$WRKBZCY2
CatVars.df$WRKBZCY2 <- NULL
CatVars.df$PDEN <- CatVars.df$PDEN00
CatVars.df$PDEN00 <- NULL
for(t in unique(CatVars.df$MILSTAT)) {CatVars.df[paste("MILSTAT",t,sep="")] <- ifelse(CatVars.df$MILSTAT==t,1,0)}
for(t in unique(CatVars.df$IRMARIT)) {CatVars.df[paste("IRMARIT",t,sep="")] <- ifelse(CatVars.df$IRMARIT==t,1,0)}
for(t in unique(CatVars.df$NEWRACE)) {CatVars.df[paste("NEWRACE",t,sep="")] <- ifelse(CatVars.df$NEWRACE==t,1,0)}
for(t in unique(CatVars.df$BKOTH)) {CatVars.df[paste("BKOTH",t,sep="")] <- ifelse(CatVars.df$BKOTH==t,1,0)}
for(t in unique(CatVars.df$USEALCG)) {CatVars.df[paste("USEALCG",t,sep="")] <- ifelse(CatVars.df$USEALCG==t,1,0)}
for(t in unique(CatVars.df$USEMJCG)) {CatVars.df[paste("USEMJCG",t,sep="")] <- ifelse(CatVars.df$USEMJCG==t,1,0)}
for(t in unique(CatVars.df$USEALMJ)) {CatVars.df[paste("USEALMJ",t,sep="")] <- ifelse(CatVars.df$USEALMJ==t,1,0)}
for(t in unique(CatVars.df$USEACM)) {CatVars.df[paste("USEACM",t,sep="")] <- ifelse(CatVars.df$USEACM==t,1,0)}
for(t in unique(CatVars.df$USENEXT)) {CatVars.df[paste("USENEXT",t,sep="")] <- ifelse(CatVars.df$USENEXT==t,1,0)}
for(t in unique(CatVars.df$DOMOSTYR)) {CatVars.df[paste("DOMOSTYR",t,sep="")] <- ifelse(CatVars.df$DOMOSTYR==t,1,0)}
for(t in unique(CatVars.df$TYPEPBLM)) {CatVars.df[paste("TYPEPBLM",t,sep="")] <- ifelse(CatVars.df$TYPEPBLM==t,1,0)}
for(t in unique(CatVars.df$IFATHER)) {CatVars.df[paste("IFATHER",t,sep="")] <- ifelse(CatVars.df$IFATHER==t,1,0)}
for(t in unique(CatVars.df$IMOTHER)) {CatVars.df[paste("IMOTHER",t,sep="")] <- ifelse(CatVars.df$IMOTHER==t,1,0)}
for(t in unique(CatVars.df$JBSTATR)) {CatVars.df[paste("JBSTATR",t,sep="")] <- ifelse(CatVars.df$JBSTATR==t,1,0)}
for(t in unique(CatVars.df$FIRSTPOS)) {CatVars.df[paste("FIRSTPOS",t,sep="")] <- ifelse(CatVars.df$FIRSTPOS==t,1,0)}
for(t in unique(CatVars.df$PDEN)) {CatVars.df[paste("PDEN",t,sep="")] <- ifelse(CatVars.df$PDEN==t,1,0)}
CatVars.df <- CatVars.df[ , -which(names(CatVars.df) %in% c("MILSTAT", "IRMARIT", "NEWRACE", "BKOTH", "USEALCG", "USEMJCG", "USEALMJ", "USEACM", "USENEXT", "DOMOSTYR", "TYPEPBLM", "IFATHER", "IMOTHER", "JBSTATR", "FIRSTPOS", "PDEN"))]
aiNSDUH2 <- cbind(aiNSDUH2, CatVars.df)
aiNSDUH2 <-  aiNSDUH2[ , -which(names(aiNSDUH2) %in% c("MILSTAT", "IRMARIT", "NEWRACE", "BKOTH", "USEALCG", "USEMJCG", "USEALMJ", "USEACM", "USENEXT", "DOMOSTYR", "TYPEPBLM", "IFATHER", "IMOTHER", "JBSTATR2", "FIRSTPOS", "PDEN00", "MILSTATNA", "IRMARITNA", "NEWRACENA", "BKOTHNA", "USEALCGNA", "USEMJCGNA", "USEALMJNA", "USEACMNA", "USENEXTNA", "DOMOSTYRNA", "TYPEPBLMNA", "IFATHERNA", "IMOTHERNA", "JBSTATR2NA", "FIRSTPOSNA", "PDEN00NA"))]
aiNSDUH2$IRCDUAGE[aiNSDUH2$IRCDUAGE == 993] <- NA
rm(CatVars.df, t)
#convert ABODOP to 0's
allNSDUH$ABODOP[is.na(allNSDUH$ABODOP)] <- 0
#write.csv(aiNSDUH2, file="aiNSDUH2e.csv")


###########################################
#remove NA
#############################################
aiNSDUHscale <- aiNSDUH2
aiNSDUHscale <- as.data.frame(scale(aiNSDUHna, center = mins, scale = maxs - mins))



#############################
#Elastic Net regularization - thins variables
#############################
install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)
set.seed(42)
NSDUHmatrix <- data.matrix(aiNSDUHscale)
respVector <- allNSDUH[['ABODOP']]
class(respVector)

fit <- glmnet(NSDUHmatrix, respVector)
plot(fit, label=TRUE)
print(fit)

cvfit <- cv.glmnet(NSDUHmatrix, respVector, family="binomial", standardize=FALSE)
plot(cvfit)
coef(cvfit, s = "lambda.1se")
coef(cvfit, s = "lambda.min")
print(cvfit)

#sweep range of alpha values from 0-1? <1 for less strict regualization 

#extract top 25 variables
TopVars.df <- aiNSDUHscale[ , which(names(aiNSDUHscale) %in% c("SNYSTOLE", "IRTRNFY", "SEDAGLST", "IRANLAGE", "RKFQRSKY", "OXYAGLST", "PROBATON", "RKDIFHER", "IRCRKRC", "SNRLGSVC", "WRKSLFEM", "IRHERRC", "DRGPLCY", "WORKDAYS", "IRCIGFM", "IRCOCRC", "CIGAGLST", "IRSEDAGE", "HEALTH", "MRJAGLST", "IFATHER1", "BKOTH1", "ANLAGLST", "IREDUC2", "SNRLFRND"))]
TopVars.df$ABODOP <- allNSDUH[['ABODOP']]




install.packages("tictoc")
library(tictoc)
######################
#Neuralnet time
######################
#aiNSDUHoutcome <- apply(aiNSDUHoutcome, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
index <- sample(1:nrow(TopVars.df),round(0.75*nrow(TopVars.df)))
train <- TopVars.df[index,]
test <- TopVars.df[-index,]

#if at all necessary, one hidden layer is enough for a vast numbers of applications.
#number neurons between the input layer size and the output layer size, usually 2/3rd the input size 
#a single output since we are doing log regression.install.packages("neuralnet")install.packages("grid")install.packages("MASS")
library(grid)
library(MASS)
library(neuralnet)
n <- names(train)
f <- as.formula(paste("ABODOP ~", paste(n[!n %in% "ABODOP"], collapse = " + ")))
tic()
nn <- neuralnet(f,data=train,hidden=c(12,6),linear.output=F)
toc()

plot(nn)

#Predicting medv using the neural network
#Now we can try to predict medv values for the test set, calculate MSE. net will output a normalized prediction
#need to scale it back to make comparison (or simple prediction).
#$pr.nn <- compute(nn,test[,1:13])
#pr.nn_ <- pr.nn$net.result*(max(data$ABODOP)-min(data$ABODOP))+min(data$ABODOP)
#test.r <- (test$ABODOP)*(max(data$ABODOP)-min(data$ABODOP))+min(data$ABODOP)
#MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test)

## test the resulting output
temp_test <- test[,-which(names(test) %in% c("ABODOP"))]
net.results <- compute(nn, temp_test)
results <- data.frame(actual = test$ABODOP, prediction=net.results$net.result)
results[100:115, ]
results$prediction <- round(results$prediction)
results[100:115, ]

net.results_ <- net.results$net.result*(max(TopVars.df$ABODOP)-min(TopVars.df$ABODOP))+min(TopVars.df$ABODOP)
test_ <- (test$ABODOP)*(max(TopVars.df$ABODOP)-min(TopVars.df$ABODOP))+min(TopVars.df$ABODOP)
MSE.nn <- sum((test_ - net.results_)^2)/nrow(test)

#linear model
train.r <- TopVars.df[index,]
test.r <- TopVars.df[-index,]
lm.fit <- glm(ABODOP~., data=train.r)
summary(lm.fit)
pr.lm <- predict(lm.fit,test.r)
MSE.lm <- sum((pr.lm - test.r$ABODOP)^2)/nrow(test.r)

#AI v linear model
print(paste(MSE.lm,MSE.nn))


#################
#CROSS VALIDATION
#################
#cv for linear model
library(boot)
set.seed(200)
lm.fit <- glm(medv~., data=data)
cv.glm(data, lm.fit, k=10)$delta[1]

#Neuralnet CV (plyr is for status bar)
set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(TopVars.df),round(0.9*nrow(TopVars.df)))
  train.cv <- TopVars.df[index,]
  test.cv <- TopVars.df[-index,]
  
  nn.cv <- neuralnet(f,data=train.cv,hidden=c(12,6),linear.output=F)
  
  temp_test.cv <- test.cv[,-which(names(test.cv) %in% c("ABODOP"))]
  net.results.cv <- compute(nn.cv, temp_test.cv)
  net.results.cv <- net.results.cv$net.result*(max(TopVars.df$ABODOP)-min(TopVars.df$ABODOP))+min(TopVars.df$ABODOP)
  
  test.cv.r <- (test.cv$ABODOP)*(max(TopVars.df$ABODOP)-min(TopVars.df$ABODOP))+min(TopVars.df$ABODOP)
  
  cv.error[i] <- sum((test.cv.r - net.results.cv)^2)/nrow(test.cv)
  pbar$step()
}





#Results
mean(cv.error)
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)


temp_test.cv <- test.cv[,-which(names(test.cv) %in% c("ABODOP"))]
net.results.cv <- compute(nn.cv, temp_test.cv)
results.cv <- data.frame(actual = test.cv$ABODOP, prediction=net.results.cv$net.result)


net.results.cv_ <- net.results.cv$net.result*(max(TopVars.df$ABODOP)-min(TopVars.df$ABODOP))+min(TopVars.df$ABODOP)
test_ <- (test$ABODOP)*(max(TopVars.df$ABODOP)-min(TopVars.df$ABODOP))+min(TopVars.df$ABODOP)
MSE.nn <- sum((test_ - net.results_)^2)/nrow(test)
























#XGboost
install.packages('vtreat')
install.packages('xgboost')
install.packages('sigr')
install.packages('WVPlots')
library("vtreat")
library("xgboost")
library("sigr")
library("WVPlots")

outcome <- 'ABUSEILL'
target <- 1
CatVars <- c('CATAG3', 'MILSTAT', 'IRMARIT', 'NEWRACE', 'BKOTH', 'BKOTHOF2', 'USEALCG', 'USEMJCG', 'USEALMJ', 'USEACM', 'USENEXT', 'DOMOSTYR', 'TYPEPBLM', 'IFATHER', 'IMOTHER', 'JBSTATR2', 'WRKBZCY2', 'FIRSTPOS', 'WRKIDST2', 'WRKOCUY2', 'PDEN00')

#for(v in CatVars) {
#  aiNSDUHoutcome[[v]] <- as.factor(aiNSDUHoutcome[[v]])
#}
vars <- colnames(aiNSDUHoutcome)
dTrain <- aiNSDUHoutcome

set.seed(4623762)
crossValPlan <- vtreat::kWayStratifiedY(nrow(aiNSDUHoutcome), 10, aiNSDUHoutcome, aiNSDUHoutcome[[outcome]])
evaluateModelingProcedure <- function(xMatrix, outcomeV, crossValPlan) {
  preds <- rep(NA_real_, nrow(xMatrix))
  for(ci in crossValPlan) {
    nrounds <- 1000
    cv <- xgb.cv(data= xMatrix[ci$train, ],
                 label = outcomeV[ci$train],
                 objective = 'binary:logistic',
                 nrounds = nrounds,
                 verbose = 0,
                 nfold = 5)
    #nrounds  <- which.min(cv$evaluation_log$test_rmse_mean) # regression
    nrounds  <- which.min(cv$evaluation_log$test_error_mean) # classification
    model <- xgboost(data= xMatrix[ci$train, ],
                     label= outcomeV[ci$train],
                     objective= 'binary:logistic',
                     nrounds= nrounds,
                     verbose= 0)
    preds[ci$app] <-  predict(model, xMatrix[ci$app, ])
  }
  preds
}
#vtreat variable encoding   201-206
set.seed(4623762)
tplan <- vtreat::designTreatmentsZ(dTrain, vars, minFraction = 0, verbose = FALSE)
#restrict to common var types
sf <- tplan$scoreFrame
newvars <- sf$varName[sf$code %in% c("lev", "clean","isBAD")]
trainVtreat <- as.matrix(vtreat::prepare(tplan, dTrain, varRestriction=newvars))

print(dim(trainVtreat))
print(colnames(trainVtreat))
#many 'is BAD' variables - remove or something?
aiNSDUHoutcome$ABUSEILL <- as.numeric(as.character(aiNSDUHoutcome$ABUSEILL))

#FREEZE/HANG
dTrain$predVtreatZ <- evaluateModelingProcedure(trainVtreat,
                                                dTrain[[outcome]]==target,
                                                crossValPlan)
sigr::permTestAUC(dTrain, 
                  'predVtreatZ',
                  outcome, target)



#nn
#vtreat variable encoding   201-206
outcome <- 'ABUSEILL_clean'
target <- 1
CatVars <- c('CATAG3', 'MILSTAT', 'IRMARIT', 'NEWRACE', 'BKOTH', 'BKOTHOF2', 'USEALCG', 'USEMJCG', 'USEALMJ', 'USEACM', 'USENEXT', 'DOMOSTYR', 'TYPEPBLM', 'IFATHER', 'IMOTHER', 'JBSTATR2', 'WRKBZCY2', 'FIRSTPOS', 'WRKIDST2', 'WRKOCUY2', 'PDEN00')
vars <- colnames(aiNSDUHoutcome)

set.seed(4623762)
tplan <- vtreat::designTreatmentsZ(aiNSDUHoutcome, vars, minFraction = 0, verbose = FALSE)
#restrict to common var types
sf <- tplan$scoreFrame
newvars <- sf$varName[sf$code %in% c("lev", "clean","isBAD")]
trainVtreat <- as.matrix(vtreat::prepare(tplan, aiNSDUHoutcome, varRestriction=newvars))

traindf <- as.data.frame(trainVtreat)


#Neural Net
#randomly splitting the data into a train and a test set
#then we fit a linear regression model and test it on the test set. 
index <- sample(1:nrow(scaleTrain),round(0.75*nrow(scaleTrain)))
train <- scaleTrain[index,]
test <- scaleTrain[-index,]
lm.fit <- glm(ABUSEILL~., data=scaleTrain)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$ABUSEILL)^2)/nrow(test)

#The sample(x,size) function simply outputs a vector of the specified size of randomly selected samples from the vector x. 
#By default the sampling is without replacement: index is essentially a random vector of indeces. Since we are dealing with a
#regression problem, we are going to use the mean squared error (MSE) as a measure of how much our predictions are far away from the real data.

#It is good practice to normalize your data before training a neural network.
maxs <- apply(traindf, 2, max)
mins <- apply(traindf, 2, min)
scaleTrain <- as.data.frame(scale(traindf, center = mins, scale = maxs - mins))

train_ <- scaleTrain[index,]
test_ <- scaleTrain[-index,]


