install.packages(effect)
library(effects)
library(reshape2)
library(ggplot2)
library(tibble)
source("http://peterhaschke.com/Code/multiplot.R")
par(mfrow=c(1,1))
# Crossvalidation group A:
Atrain <- read.csv('../input/Atrain.csv',head=TRUE,sep=",")
Atest <- read.csv('../input/Atest.csv',head=TRUE,sep=",")
# Crossvalidation group B:
Btrain <- read.csv('../input/Btrain.csv',head=TRUE,sep=",")
Btest <- read.csv('../input/Btest.csv',head=TRUE,sep=",")
# Crossvalidation group C:
Ctrain <- read.csv('../input/Ctrain.csv',head=TRUE,sep=",")
Ctest <- read.csv('../input/Ctest.csv',head=TRUE,sep=",")
# Crossvalidation group D:
Dtrain <- read.csv('../input/Dtrain.csv',head=TRUE,sep=",")
Dtest <- read.csv('../input/Dtest.csv',head=TRUE,sep=",")
# Crossvalidation group E:
Etrain <- read.csv('../input/Etrain.csv',head=TRUE,sep=",")
Etest <- read.csv('../input/Etest.csv',head=TRUE,sep=",")

popular <- read.csv('../input/popular.csv',head=TRUE,sep=",")
unpopular <- read.csv('../input/unpopular.csv',head=TRUE,sep=",")
complete <- read.csv('../input/complete.csv',head=TRUE,sep=",")

# I'm copying complete to data as for some plots i'll have to transform the 1 or 0's in the column 'popular' to respectively "popular" and "unpopular". Though for the logistic regression model for example we need to keep the values 1 and 0.
data <- complete 
        
### Comparing continuous features, popular with unpopular

# Comparing the mean and it's diffences. 
popularfeatures <- popular[,c(6,7,9,11,12,14,15,16,17,18)]
popularmeans <- colMeans(popularfeatures)
popularmeans
unpopularfeatures <- unpopular[,c(6,7,9,11,12,14,15,16,17,18)]
unpopularmeans <- colMeans(unpopularfeatures)
unpopularmeans
completefeatures <- complete[,c(5,6,8,10,11,13,14,15,16,17)]
completefeatures
completemax <- apply(completefeatures, 2, function(x) max(x, na.rm = TRUE))
completemax

# As the values of the features differ in scale we normalized the differences by the max value of that feature. This normalized difference helps us understand what make the difference between a popular song and a unpopular song. Also it gives us an indication of what might be a telling feature to predict if songs are going to be popular or unpopular. 
difference <- abs(popularmeans-unpopularmeans)
plot(difference)
normdifference <- abs(popularmeans-unpopularmeans)/abs(completemax)
plot(normdifference)
means <- cbind(data.frame(popularmeans,unpopularmeans))
means



# Transform 1 into popular and 0 into unpopular. The factor instead of the 1 or 0 is needed for the plotting of the grouped boxplots.
complete$popular <- factor(complete$popular,levels = c(0,1),labels = c("unpopular", "popular")) 


# Put the features that scale from 0 to 1 into one figure
zeroOne <- complete[,c(5,6,10,11,13,14,18)]
stacked.zeroOne = melt(zeroOne, id=c('danceability','energy','speechiness','acousticness','liveliness','valence'),id.vars="popular")
p <- ggplot(data = stacked.zeroOne, aes(x=variable, y=value)) + geom_boxplot(aes(fill=popular))
p + facet_wrap( ~ variable, scales="free")

# Loudness
loudness <- complete[,c(18,8)]
stacked.loudness = melt(loudness, id=c('loudness'),id.vars="popular")
q <- ggplot(data = stacked.loudness, aes(x=variable, y=value)) + geom_boxplot(aes(fill=popular)) + facet_wrap( ~ variable, scales="free")

# instrumetalness
instrumetalness <- complete[,c(18,12)]
stacked.instrumetalness = melt(instrumetalness, id=c('instrumentalness'),id.vars="popular")
r <- ggplot(data = stacked.instrumetalness, aes(x=variable, y=value)) + geom_boxplot(aes(fill=popular)) 
r + facet_wrap( ~ variable, scales="free")
# Instrumentalness has too many missing values and the distribution doesn't make any sense. We consider this value worthless.

# tempo
tempo <- complete[,c(18,15)]
stacked.tempo = melt(tempo, id=c('tempo'),id.vars="popular")
s <- ggplot(data = stacked.tempo, aes(x=variable, y=value)) + geom_boxplot(aes(fill=popular))  + facet_wrap( ~ variable, scales="free")

# duration
duration <- complete[,c(18,16)]
stacked.duration = melt(duration, id=c('tempo'),id.vars="popular")
t <- ggplot(data = stacked.duration, aes(x=variable, y=value)) + geom_boxplot(aes(fill=popular)) + facet_wrap( ~ variable, scales="free")

p
q
r 
s
t

multiplot(q,r,s,t,cols=2)



# time signature
time <- complete[,c(18,16)]
stacked.time = melt(time, id=c('tempo'),id.vars="popular")
u <- ggplot(data = stacked.time, aes(x=variable, y=value)) + geom_boxplot(aes(fill=popular)) + facet_wrap( ~ variable, scales="free")
u
### Logistic regression models
model <- glm(popular ~ danceability, data=data, family=binomial(link="logit"))
fit <- as.data.frame(effect('danceability', model, xlevels = 100))
plot(data$valence, data$popular)
plot(data$danceability,complete$energy)
lines(fit$danceability ,fit$fit)
lines(fit$valence, fit$fit)




### Training/Test Set A
modeldanceability <- glm(popular ~ danceability, data=Atrain, family=binomial(link="logit"))
modelenergy <- glm(popular ~ energy, data=Atrain, family=binomial(link="logit"))
modelloudness <- glm(popular ~ loudness, data=Atrain, family=binomial(link="logit"))
modelspeechiness <- glm(popular ~ speechiness, data=Atrain, family=binomial(link="logit"))
modelacousticness <- glm(popular ~ acousticness, data=Atrain, family=binomial(link="logit"))
modelliveness <- glm(popular ~ liveness, data=Atrain, family=binomial(link="logit"))
modelvalence <- glm(popular ~ valence, data=Atrain, family=binomial(link="logit"))
modeltempo <- glm(popular ~ tempo, data=Atrain, family=binomial(link="logit"))
modelduration_ms <- glm(popular ~ duration_ms, data=Atrain, family=binomial(link="logit"))
modeltime_signature <- glm(popular ~ time_signature, data=Atrain, family=binomial(link="logit"))

resultsdanceability <- predict(modeldanceability,newdata=subset(Atest,select=c(5)),type='response')
resultsenergy <- predict(modelenergy,newdata=subset(Atest,select=c(6)),type='response')
resultsloudness <- predict(modelloudness,newdata=subset(Atest,select=c(8)),type='response')
resultsspeechiness <- predict(modelspeechiness,newdata=subset(Atest,select=c(10)),type='response')
resultsacousticness <- predict(modelacousticness,newdata=subset(Atest,select=c(11)),type='response')
resultsliveness <- predict(modelliveness,newdata=subset(Atest,select=c(13)),type='response')
resultsvalence <- predict(modelvalence,newdata=subset(Atest,select=c(14)),type='response')
resultstempo <- predict(modeltempo,newdata=subset(Atest,select=c(15)),type='response')
resultsduration_ms <- predict(modelduration_ms,newdata=subset(Atest,select=c(16)),type='response')
resultstime_signature <- predict(modeltime_signature,newdata=subset(Atest,select=c(17)),type='response')

ggplot(Atest, aes(x=speechiness, y=popular)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) # 0.4


### Training/Test Set B
modeldanceability <- glm(popular ~ danceability, data=Btrain, family=binomial(link="logit"))
modelenergy <- glm(popular ~ energy, data=Btrain, family=binomial(link="logit"))
modelloudness <- glm(popular ~ loudness, data=Btrain, family=binomial(link="logit"))
modelspeechiness <- glm(popular ~ speechiness, data=Btrain, family=binomial(link="logit"))
modelacousticness <- glm(popular ~ acousticness, data=Btrain, family=binomial(link="logit"))
modelliveness <- glm(popular ~ liveness, data=Btrain, family=binomial(link="logit"))
modelvalence <- glm(popular ~ valence, data=Btrain, family=binomial(link="logit"))
modeltempo <- glm(popular ~ tempo, data=Btrain, family=binomial(link="logit"))
modelduration_ms <- glm(popular ~ duration_ms, data=Btrain, family=binomial(link="logit"))
modeltime_signature <- glm(popular ~ time_signature, data=Btrain, family=binomial(link="logit"))

resultsdanceability <- predict(modeldanceability,newdata=subset(Btest,select=c(5)),type='response')
resultsenergy <- predict(modelenergy,newdata=subset(Btest,select=c(6)),type='response')
resultsloudness <- predict(modelloudness,newdata=subset(Btest,select=c(8)),type='response')
resultsspeechiness <- predict(modelspeechiness,newdata=subset(Btest,select=c(10)),type='response')
resultsacousticness <- predict(modelacousticness,newdata=subset(Btest,select=c(11)),type='response')
resultsliveness <- predict(modelliveness,newdata=subset(Btest,select=c(13)),type='response')
resultsvalence <- predict(modelvalence,newdata=subset(Btest,select=c(14)),type='response')
resultstempo <- predict(modeltempo,newdata=subset(Btest,select=c(15)),type='response')
resultsduration_ms <- predict(modelduration_ms,newdata=subset(Btest,select=c(16)),type='response')
resultstime_signature <- predict(modeltime_signature,newdata=subset(Btest,select=c(17)),type='response')


ggplot(Btest, aes(x=danceability, y=popular)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) #0.725

summary(modeldanceability)


### Training/Test Set C
modeldanceability <- glm(popular ~ danceability, data=Ctrain, family=binomial(link="logit"))
modelenergy <- glm(popular ~ energy, data=Ctrain, family=binomial(link="logit"))
modelloudness <- glm(popular ~ loudness, data=Ctrain, family=binomial(link="logit"))
modelspeechiness <- glm(popular ~ speechiness, data=Ctrain, family=binomial(link="logit"))
modelacousticness <- glm(popular ~ acousticness, data=Ctrain, family=binomial(link="logit"))
modelliveness <- glm(popular ~ liveness, data=Ctrain, family=binomial(link="logit"))
modelvalence <- glm(popular ~ valence, data=Ctrain, family=binomial(link="logit"))
modeltempo <- glm(popular ~ tempo, data=Ctrain, family=binomial(link="logit"))
modelduration_ms <- glm(popular ~ duration_ms, data=Ctrain, family=binomial(link="logit"))
modeltime_signature <- glm(popular ~ time_signature, data=Ctrain, family=binomial(link="logit"))

resultsdanceability <- predict(modeldanceability,newdata=subset(Ctrain,select=c(5)),type='response')
resultsenergy <- predict(modelenergy,newdata=subset(Ctrain,select=c(6)),type='response')
resultsloudness <- predict(modelloudness,newdata=subset(Ctrain,select=c(8)),type='response')
resultsspeechiness <- predict(modelspeechiness,newdata=subset(Ctrain,select=c(10)),type='response')
resultsacousticness <- predict(modelacousticness,newdata=subset(Ctrain,select=c(11)),type='response')
resultsliveness <- predict(modelliveness,newdata=subset(Ctrain,select=c(13)),type='response')
resultsvalence <- predict(modelvalence,newdata=subset(Ctrain,select=c(14)),type='response')
resultstempo <- predict(modeltempo,newdata=subset(Ctrain,select=c(15)),type='response')
resultsduration_ms <- predict(modelduration_ms,newdata=subset(Ctrain,select=c(16)),type='response')
resultstime_signature <- predict(modeltime_signature,newdata=subset(Ctrain,select=c(17)),type='response')

### Training/Test Set D
modeldanceability <- glm(popular ~ danceability, data=Dtrain, family=binomial(link="logit"))
modelenergy <- glm(popular ~ energy, data=Dtrain, family=binomial(link="logit"))
modelloudness <- glm(popular ~ loudness, data=Dtrain, family=binomial(link="logit"))
modelspeechiness <- glm(popular ~ speechiness, data=Dtrain, family=binomial(link="logit"))
modelacousticness <- glm(popular ~ acousticness, data=Dtrain, family=binomial(link="logit"))
modelliveness <- glm(popular ~ liveness, data=Dtrain, family=binomial(link="logit"))
modelvalence <- glm(popular ~ valence, data=Dtrain, family=binomial(link="logit"))
modeltempo <- glm(popular ~ tempo, data=Dtrain, family=binomial(link="logit"))
modelduration_ms <- glm(popular ~ duration_ms, data=Dtrain, family=binomial(link="logit"))
modeltime_signature <- glm(popular ~ time_signature, data=Dtrain, family=binomial(link="logit"))

resultsdanceability <- predict(modeldanceability,newdata=subset(Dtrain,select=c(5)),type='response')
resultsenergy <- predict(modelenergy,newdata=subset(Dtrain,select=c(6)),type='response')
resultsloudness <- predict(modelloudness,newdata=subset(Dtrain,select=c(8)),type='response')
resultsspeechiness <- predict(modelspeechiness,newdata=subset(Dtrain,select=c(10)),type='response')
resultsacousticness <- predict(modelacousticness,newdata=subset(Dtrain,select=c(11)),type='response')
resultsliveness <- predict(modelliveness,newdata=subset(Dtrain,select=c(13)),type='response')
resultsvalence <- predict(modelvalence,newdata=subset(Dtrain,select=c(14)),type='response')
resultstempo <- predict(modeltempo,newdata=subset(Dtrain,select=c(15)),type='response')
resultsduration_ms <- predict(modelduration_ms,newdata=subset(Dtrain,select=c(16)),type='response')
resultstime_signature <- predict(modeltime_signature,newdata=subset(Dtrain,select=c(17)),type='response')

### Training/Test Set E
modeldanceability <- glm(popular ~ danceability, data=Etrain, family=binomial(link="logit"))
modelenergy <- glm(popular ~ energy, data=Etrain, family=binomial(link="logit"))
modelloudness <- glm(popular ~ loudness, data=Etrain, family=binomial(link="logit"))
modelspeechiness <- glm(popular ~ speechiness, data=Etrain, family=binomial(link="logit"))
modelacousticness <- glm(popular ~ acousticness, data=Etrain, family=binomial(link="logit"))
modelliveness <- glm(popular ~ liveness, data=Etrain, family=binomial(link="logit"))
modelvalence <- glm(popular ~ valence, data=Etrain, family=binomial(link="logit"))
modeltempo <- glm(popular ~ tempo, data=Etrain, family=binomial(link="logit"))
modelduration_ms <- glm(popular ~ duration_ms, data=Etrain, family=binomial(link="logit"))
modeltime_signature <- glm(popular ~ time_signature, data=Etrain, family=binomial(link="logit"))

resultsdanceability <- predict(modeldanceability,newdata=subset(Etrain,select=c(5)),type='response')
resultsenergy <- predict(modelenergy,newdata=subset(Etrain,select=c(6)),type='response')
resultsloudness <- predict(modelloudness,newdata=subset(Etrain,select=c(8)),type='response')
resultsspeechiness <- predict(modelspeechiness,newdata=subset(Etrain,select=c(10)),type='response')
resultsacousticness <- predict(modelacousticness,newdata=subset(Etrain,select=c(11)),type='response')
resultsliveness <- predict(modelliveness,newdata=subset(Etrain,select=c(13)),type='response')
resultsvalence <- predict(modelvalence,newdata=subset(Etrain,select=c(14)),type='response')
resultstempo <- predict(modeltempo,newdata=subset(Etrain,select=c(15)),type='response')
resultsduration_ms <- predict(modelduration_ms,newdata=subset(Etrain,select=c(16)),type='response')
resultstime_signature <- predict(modeltime_signature,newdata=subset(Etrain,select=c(17)),type='response')

vector <- c()
results <- list(resultsdanceability, 
                resultsenergy,resultsloudness,resultsspeechiness,
                resultsacousticness,resultsliveness,resultsvalence,resultstempo,resultsduration_ms,resultstime_signature)
a <- 1
for(result in results){
  
  print(a)
  result
  result <- ifelse(result > 0.5,1,0)
  misClasificError <- mean(result != Etest$popular)
  print(paste(colnames(Etest[numbers[a]]),1-misClasificError))
  vector[a] <- 1-misClasificError
  a <- a+1
}
vector
df.precision$Etest <- vector
df.precision




models <- list(modeldanceability, 
  modelenergy,modelloudness,modelspeechiness,
  modelacousticness,modelliveness,modelvalence,modeltempo,modelduration_ms,modeltime_signature)
numbers = c(5,6,8,10,11,13,14,15,16,17)

fit <- as.data.frame(effect('valence', model, xlevels = 100))


plot(complete$valence,complete$popular,xlab="Valence",ylab="Probability of popular") # plot with body size on x-axis and survival (0 or 1) on y-axis
g=glm(complete$popular~complete$valence,family=binomial(link="logit"),complete) # run a logistic regression model (in this case, generalized linear model with logit link). see ?glm
summary(g)

ggplot(complete, aes(x=valence, y=popular)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)



par(mfrow=c(3,3)) ## plots of the features to the popularity
plot(complete$danceability,complete$popular, main="Danceability vs Popularity")
plot(complete$energy,complete$popular, main="energy vs Popularity")
plot(complete$loudness,complete$popular, main="loudness vs Popularity")
plot(complete$speechiness,complete$popular, main="speechiness vs Popularity")
plot(complete$acousticness,complete$popular, main="Danceability vs Popularity")
plot(complete$liveness,complete$popular, main="liveness vs Popularity")
plot(complete$valence,complete$popular, main="valence vs Popularity")
plot(complete$tempo,complete$popular, main="tempo vs Popularity")
plot(complete$duration_ms,complete$popular, main="duration_ms vs Popularity")


p1 <- ggplot(popular, aes(duration_ms)) + geom_area(stat="bin")
p2 <- ggplot(unpopular, aes(duration_ms)) + geom_area(stat="bin")
p3 <- ggplot(popular, aes(duration_ms)) + geom_area(stat="bin")
p4 <-ggplot(unpopular, aes(duration_ms)) + geom_area(stat="bin")
multiplot(p1,p2,p3,p4,col=2)


library(ggplot2)
boxplot(popDanc,popEner)
idtDancEnerPopu <- Atrain[,c(1,5,6,18)]

df.melt = melt(complete, id = c('popular', 'danceability','energy'),id.vars='popular')
df.melt
plot(popular[,6])
Atrain[1,2]


AtrainDancEner <- Atrain[,c(5,6,18)] # make a new data frame of column 5,6 and 18
AtrainDancEner[1,]

model <- glm(popular ~.,family = binomial(link='logit'),data = AtrainDancEner)

summary(model)


# In this case we just consider the training and test set of A. For the set A the single input precision scores are the highest for Energy, loudness, accousticness, liveness and valence (all .575). For this reason we try to combine the model these features to get see if we can get better predictions with the multiple input logistic regression models.

## AccLiv Atest - Accousticness and Liveness have both a more or less similar distributions an densities for the popular and unpopular songs. 
# In both cases, 1) the mean of the popular is slightly lower than the mean of the unpopular
# 2) the variance is smaller for the popular songs
# 3) the distribution is skewed towards the lower end
# With these indications we expected that accousticness and liveness might give a good precision score in a multiple input linear regression model. It ended up being the highest precision model we could find for logistic regression models.
AtrainAccLiv <- Atrain[,c(11,13,18)]
modelAccLiv <- glm(popular ~.,family = binomial(link='logit'),data = AtrainAccLiv)
resultsAccLiv <- predict(modelAccLiv,newdata=Atest,type='response')

resultsAccLiv <- ifelse(resultsAccLiv > 0.5,1,0)
misClasificErrorAccLiv <- mean(resultsAccLiv != Atest$popular)
print(paste('AccLiv',1-misClasificErrorAccLiv)) #0.725

ggplot(Atest, aes(x=acousticness+liveness, y=popular)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) # 0.725

## AccLiv Btest
# As high the precision was for the A data sets, for the B data sets ther results of this model were marginal. The reason for this differences in the model probably have a lot to do with the fact that the data set is very small. The results of the data set are not very precise.
BtrainAccLiv <- Btrain[,c(11,13,18)]
modelAccLiv <- glm(popular ~.,family = binomial(link='logit'),data = BtrainAccLiv)
resultsAccLiv <- predict(modelAccLiv,newdata=Btest,type='response')

resultsAccLiv <- ifelse(resultsAccLiv > 0.5,1,0)
misClasificErrorAccLiv <- mean(resultsAccLiv != Btest$popular)
print(paste('AccLiv',1-misClasificErrorAccLiv)) #0.55

## EneLouVal 
# With the same reasoning as before we expected that energy, valence and loudness would make a good model together. The results were quite bad. We think the reason for this is the fact that the scale of the loudness feature is from -17 to 0. Therefore it might nog be very compatible with the energy and valence.
AtrainEneLouVal <- Atrain[,c(6,8,14,18)]     
modelEneLouVal <- glm(popular ~.,family = binomial(link='logit'),data = AtrainEneLouVal)
resultsEneLouVal <- predict(modelEneLouVal,newdata=Atest,type='response')

resultsEneLouVal <- ifelse(resultsEneLouVal > 0.5,1,0)
misClasificErrorEneLouVal <- mean(resultsEneLouVal != Atest$popular)
print(paste('EneLouVal',1-misClasificErrorEneLouVal)) #0.525

## EneVal
AtrainEneVal <- Atrain[,c(6,14,18)]     
modelEneVal <- glm(popular ~.,family = binomial(link='logit'),data = AtrainEneVal)
resultsEneVal <- predict(modelEneVal,newdata=Atest,type='response')

resultsEneVal <- ifelse(resultsEneVal > 0.5,1,0)
misClasificErrorEneVal <- mean(resultsEneVal != Atest$popular)
print(paste('EneVal',1-misClasificErrorEneVal)) #0.55

## DanEneVal
AtrainDanEneVal <- Atrain[,c(5,6,14,18)]     
modelDanEneVal <- glm(popular ~.,family = binomial(link='logit'),data = AtrainDanEneVal)
resultsDanEneVal <- predict(modelDanEneVal,newdata=Atest,type='response')

resultsDanEneVal <- ifelse(resultsDanEneVal > 0.5,1,0)
misClasificErrorDanEneVal <- mean(resultsDanEneVal != Atest$popular)
print(paste('DanEneVal',1-misClasificErrorDanEneVal)) #0.55

## DanEne
AtrainDanEne <- Atrain[,c(5,6,18)]     
modelDanEne <- glm(popular ~.,family = binomial(link='logit'),data = AtrainDanEne)
resultsDanEne <- predict(modelDanEne,newdata=Atest,type='response')

resultsDanEne <- ifelse(resultsDanEne > 0.5,1,0)
misClasificErrorDanEne <- mean(resultsDanEne != Atest$popular)
print(paste('DanEne',1-misClasificErrorDanEne)) #0.5

## AccLivVal

AtrainAccLivVal <- Atrain[,c(11,13,14,18)]
modelAccLivVal <- glm(popular ~.,family = binomial(link='logit'),data = AtrainAccLivVal)
resultsAccLivVal <- predict(modelAccLivVal,newdata=Atest,type='response')

resultsAccLivVal <- ifelse(resultsAccLivVal > 0.5,1,0)
misClasificErrorAccLivVal <- mean(resultsAccLivVal != Atest$popular)
print(paste('AccLivVal',1-misClasificErrorAccLivVal)) #0.675

## EneAcc
AtrainEneAcc <- Atrain[,c(6,11,18)]     
modelEneAcc <- glm(popular ~.,family = binomial(link='logit'),data = AtrainEneAcc)
resultsEneAcc <- predict(modelEneAcc,newdata=Atest,type='response')

resultsEneAcc <- ifelse(resultsEneAcc > 0.5,1,0)
misClasificErrorEneAcc <- mean(resultsEneAcc != Atest$popular)
print(paste('EneAcc',1-misClasificErrorEneAcc)) #0.575

## DanVal
AtrainDanVal <- Atrain[,c(5,14,18)]     
modelDanVal <- glm(popular ~.,family = binomial(link='logit'),data = AtrainDanVal)
resultsDanVal <- predict(modelDanVal,newdata=Atest,type='response')

resultsDanVal <- ifelse(resultsDanVal > 0.5,1,0)
misClasificErrorDanVal <- mean(resultsDanVal != Atest$popular)
print(paste('DanVal',1-misClasificErrorDanVal)) #0.525

## DanAccLiv 
AtrainDanAccLiv <- Atrain[,c(5,11,13,18)]
modelDanAccLiv <- glm(popular ~.,family = binomial(link='logit'),data = AtrainDanAccLiv)
resultsDanAccLiv <- predict(modelDanAccLiv,newdata=Atest,type='response')

resultsDanAccLiv <- ifelse(resultsDanAccLiv > 0.5,1,0)
misClasificErrorDanAccLiv <- mean(resultsDanAccLiv != Atest$popular)
print(paste('DanAccLiv',1-misClasificErrorDanAccLiv)) #0.625

ggplot(Atest, aes(x=danceability+acousticness+liveness, y=popular)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) 


mat <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
              ncol=5, nrow=10,
              dimnames=list((NULL), c("Atest", "Btest", "Ctest", "Dtest", "Etest")))

df.precision<- as.data.frame(mat)
df
vector
df[,1] <- vector
df.precision
df.meanprecision <- as.data.frame(rowMeans(df.precision))
df.precision <- column_to_rownames(df.precision,var="names")

### mean precisions
colMeans(df.precision)
rowMeans(df.precision)


