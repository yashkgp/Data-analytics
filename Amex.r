
setwd("C:/Users/yashj/Desktop/Amex")
train=fread("train.csv")
library("data.table", lib.loc="~/R/win-library/3.3")
train=fread("train.csv")
summary(train)
summary(train$mvar1)
str(train$mvar1)
train$mvar1=as.factor(train$mvar1)
str(train$mvar1)
train$mvar1=NULL
train$mvar52=train$mvar16
train$mvar52=train$mvar16+train$mvar17+train$mvar18+train$mvar19
train$mvar53=train$mvar20+train$mvar21+train$mvar22+train$mvar23
train$mvar54=train$mvar24+train$mvar25+train$mvar26+train$mvar27
train$mvar55=train$mvar28+train$mvar29+train$mvar30+train$mvar31
train$mvar56=train$mvar32+train$mvar33+train$mvar34+train$mvar35
train$mvar57=train$mvar36+train$mvar37+train$mvar38+train$mvar39
for (x in 1:40000){}
for (x in 1:40000){
if(train$mvar3[x]==0){
next }
train$mvar3[x]=NULL
}
train$mvar3[x]=na
train$mvar3[x]=is.na(train$mvar3[x])
str(train)
for (x in 1:40000){
+         if(train$mvar3[x]==0){
+                 next }
+             train$mvar3[x]=as.na(train$mvar3[x])
}
for (x in 1:40000){
if(train$mvar3[x]==0){
next }
train$mvar3[x]=as.na(train$mvar3[x])
}
for (x in 1:40000){
if(train$mvar3[x]==0){
next }
train$mvar3[x]=is.na(train$mvar3[x])
}
str(train)
train1=fread("train1.csv")
train1=fread("train1.csv")
str(train1)
library("mice", lib.loc="~/R/win-library/3.3")
md.pattern(train1)
train1$mvar12=as.factor(train1$mvar12)
summary(train1)
train1$mvar52=train1$mvar16+train1$mvar17+train1$mvar18+train1$mvar19
train1$mvar53=train1$mvar20+train1$mvar21+train1$mvar22+train1$mvar23
train1$mvar54=train1$mvar24+train1$mvar25+train1$mvar26+train1$mvar27
train1$mvar55=train1$mvar28+train1$mvar29+train1$mvar30+train1$mvar31
train1$mvar56=train1$mvar32+train1$mvar33+train1$mvar34+train1$mvar35
train1$mvar57=train1$mvar36+train1$mvar37+train1$mvar38+train1$mvar39
train1$mvar10=as.factor(train$mvar10)
md.pattern(train1)
mice_data=mice(train1,m=5,maxit=50,meth='pmm',seed=500)
mice_data=mice(train1$mvar3,m=5,maxit=50,meth='pmm',seed=500)
summary(train1)
mvar12=train1$mvar12
train1$mvar12=NULL
mice_data=mice(train1,m=5,maxit=50,meth='pmm',seed=500)
for(i in 1:40000{
train1$mvar3[i][is.na(train1$mvar3[i])] <- round(mean(train1$mvar3[i], na.rm = TRUE))
}
for(i in 1:40000){
train1$mvar3[i][is.na(train1$mvar3[i])] <- round(mean(train1$mvar3[i], na.rm = TRUE))
}
str(train1)
train1$mvar12=mvar12
library("caTools", lib.loc="~/R/win-library/3.3")
detach("package:caTools", unload=TRUE)
library("caTools", lib.loc="~/R/win-library/3.3")
t=fread("train1.csv")
train1$mvar3=t$mvar3
cm_key=train1$cm_key
train1$cm_key=NULL
library("caret", lib.loc="~/R/win-library/3.3")
library("randomForest", lib.loc="~/R/win-library/3.3")
library("e1071", lib.loc="~/R/win-library/3.3")
set.seed(1234)
train1$mvar58=train1$mvar49+2*train1$mvar50+3*train1$mvar51
str(train1)
train1$mvar58=as.factor(train1$mvar58)
split <- sample.split(train1$mvar58, SplitRatio = 0.8)
tr <- subset(train1, split == T)
cv <- subset(train1, split == F)
set.seed(4)
rf.model <- randomForest(mvar58 ~ ., data = tr, ntree = 100, nodesize = 50)
str(train1)
summary(train1)
train1$mvar12=NULL
set.seed(4)
rf.model <- randomForest(mvar58 ~ ., data = tr, ntree = 100, nodesize = 50)
mice_data=mice(train1)
install.packages("Hmisc")
library("Hmisc", lib.loc="~/R/win-library/3.3")
impute(train1$mvar3, mean)
impute(train1$mvar3, 0)
train1=as.data.frame(train1)
impute(train1$mvar3, 0)
train1$mvar3=as.interger(train1$mvar3)
train1$mvar3=as.integer(train1$mvar3)
impute(train1$mvar3, 0)
impute(train1$mvar3, '0')
train1$mvar3[is.na(train1$mvar3)] <- mean(train1$mvar3, na.rm = T)
train1$mvar9[is.na(train1$mvar9)] <- mean(train1$mvar9, na.rm = T)
set.seed(4)
rf.model <- randomForest(mvar58 ~ ., data = train1, ntree = 100, nodesize = 50)
rf.predict <- predict(rf.model, cv)
print(rf.cm <- confusionMatrix(rf.predict, cv$label))
print(rf.cm <- confusionMatrix(rf.predict, cv$mvar58))
set.seed(4)
rf.model <- randomForest(mvar58 ~ ., data = train1, ntree = 110, nodesize = 50)
split <- sample.split(train1$mvar58, SplitRatio = 0.8)
tr <- subset(train1, split == T)
cv <- subset(train1, split == F)
summary(rf.model)
rf.predict <- predict(rf.model, newdata=cv)
print(rf.cm <- confusionMatrix(rf.predict, cv$label))
print(rf.cm <- confusionMatrix(rf.predict, cv$mvar58))
train=fread("train1.csv")
train1$mvar52=train1$mvar16+train1$mvar17+train1$mvar18+train1$mvar19
train$mvar52=train$mvar16+train$mvar17+train$mvar18+train$mvar19
train$mvar53=train$mvar20+train$mvar21+train$mvar22+train$mvar23
train$mvar54=train$mvar24+train$mvar25+train$mvar26+train$mvar27
train$mvar58=train$mvar28+train$mvar29+train$mvar30+train$mvar31
train$mvar58=NULL
train$mvar55=train$mvar28+train$mvar29+train$mvar30+train$mvar31
train$mvar56=train$mvar32+train$mvar33+train$mvar34+train$mvar35
train$mvar57=train$mvar36+train$mvar37+train$mvar38+train$mvar39
train$mvar58=train$mvar49+2*train$mvar50+3*train$mvar51
train$mvar16=NULL
train$mvar17=NULL
train$mvar18=NULL
train$mvar19=NULL
train$mvar20=NULL
train$mvar21=NULL
train$mvar22=NULL
train$mvar23=NULL
train$mvar24=NULL
train$mvar25=NULL
train$mvar26=NULL
train$mvar27=NULL
train$mvar28=NULL
train$mvar29=NULL
train$mvar30=NULL
train$mvar31=NULL
train$mvar32=NULL
train$mvar33=NULL
train$mvar34=NULL
train$mvar35=NULL
train$mvar36=NULL
train$mvar37=NULL
train$mvar38=NULL
train$mvar39=NULL
train$mvar49=NULL
train$mvar50=NULL
train$mvar51=NULL
train$mvar12=NULL
str(train)
train$cm_key=NULL
imputed_Data <- mice(train, m=5, maxit = 50, method = 'pmm', seed = 500)
completeData <- complete(imputed_Data,2)
str(completeData)
str(train1)
train1$mvar49=NULL
train1$mvar50=NULL
train1$mvar51=NULL
train1$mvar3=train$mvar3
train1$mvar9=train$mvar9
train1$mvar12=mvar12
str(train1)
set.seed(4)
rf.model <- randomForest(mvar58 ~ ., data = train1, ntree = 110, nodesize = 50)
train1$mvar12=NULL
set.seed(4)
rf.model <- randomForest(mvar58 ~ ., data = train1, ntree = 110, nodesize = 50)
summary(train1)
train1$mvar3=completeData$mvar3
train1$mvar9=completeData$mvar9
summary(train1)
set.seed(4)
rf.model <- randomForest(mvar58 ~ ., data = train1, ntree = 110, nodesize = 50)
rf.predict <- predict(rf.model, newdata=cv)
print(rf.cm <- confusionMatrix(rf.predict, cv$mvar58))
mvar46=train1$mvar46
mvar47=train1$mvar47
mvar48=train1$mvar48
train1$mvar46=NULL
train1$mvar47=NULL
train1$mvar48=NULL
set.seed(4)
rf.model <- randomForest(mvar58 ~ ., data = train1, ntree = 100, nodesize = 50)
rf.predict <- predict(rf.model, newdata=cv)
print(rf.cm <- confusionMatrix(rf.predict, cv$mvar58))
summary(cv)
train1$mvar59=mvar46+2*mvar47
train1$mvar59=mvar46+2*mvar47+3*mvar48
cor(train1$58,train$mvar57)
cor(train1$mvar58,train$mvar57)
summary(train1)
train1$mvar59=as.factor(train1$mvar59)
summary(train1)
write.csv(train1,"train_mice.csv")
#f
train1=fread("train_mice.csv")
summary(train1)
imputed_Data2 <- mice(mvar12, m=5, maxit = 50, method = 'polyreg', seed = 500)
summary(mvar12)
str(mvar12)
mvar12[mvar12==""]=NA
summary(mvar12)
train1$mvar12=mvar12
imputed_Data <- mice(train1, m=5, maxit = 30, method = 'polyreg', seed = 500)
ss
summary(train)
train=train1
train$mvar12=mvar12
summary(train)
train$mvar16=NULL
train$mvar17=NULL
train$mvar18=NULL
train$mvar19=NULL
train$mvar20=NULL
train$mvar21=NULL
train$mvar22=NULL
train$mvar23=NULL
train$mvar24=NULL
train$mvar25=NULL
train$mvar26=NULL
train$mvar27=NULL
train$mvar28=NULL
train$mvar29=NULL
train$mvar30=NULL
train$mvar31=NULL
train$mvar32=NULL
train$mvar33=NULL
train$mvar34=NULL
train$mvar35=NULL
train$mvar38=NULL
train$mvar36=NULL
train$mvar37=NULL
train$mvar39=NULL
train$v1=NULL
train$V1=NULL
imputed_Data <- mice(train1, m=3, maxit = 30, method = 'polyreg', seed = 500)
s
ss
a
a
train$mvar2=NULL
train$mvar5=NULL
train$mvar14=NULL
train$mvar52=NULL
train$mvar53=NULL
train$mvar54=NULL
train$mvar55=NULL
train$mvar56=NULL
train$mvar40=NULL
train$mvar41=NULL
train$mvar42=NULL
imputed_Data <- mice(train, m=1, maxit = 10, method = 'polyreg', seed = 500)
completeData <- complete(imputed_Data,1)
summary(completeData)
train1$mvar12=completeData$mvar12
save.image("C:/Users/yashj/Desktop/Amex/Mid1.RData")
set.seed(4)
rf.model <- randomForest(mvar58 ~ ., data = train1, ntree = 100, nodesize = 50)
summary(train1)
mvar58=train1$mvar58
mvar59=train1$mvar59
train1$mvar58=NULL
train1$mvar59=NULL
train1$mvar60=as.factor(train1$mvar60)
set.seed(4)
rf.model <- randomForest(mvar58 ~ ., data = train1, ntree = 100, nodesize = 50)
summary(train1)
set.seed(4)
rf.model <- randomForest(mvar60 ~ ., data = train1, ntree = 100, nodesize = 50)
rf.predict <- predict(rf.model, newdata=cv)
summary(cv)
set.seed(1234)
split <- sample.split(train1$mvar60, SplitRatio = 0.8)
train3 <- subset(train1$mvar60, split == T)
cv <- subset(digit, split == F)
set.seed(1234)
split <- sample.split(train1$mvar60, SplitRatio = 0.8)
train3 <- subset(train1$mvar60, split == T)
cv <- subset(train1$mvar60, split == F)
rf.predict <- predict(rf.model, newdata=cv)
summary(train1)
train1$V1=NULL
rf.predict <- predict(rf.model, newdata=cv)
set.seed(4)
rf.model <- randomForest(mvar60 ~ ., data = train1, ntree = 100, nodesize = 50)
cv$V1=NULL
summary(cv)
set.seed(1234)
split <- sample.split(train1$mvar60, SplitRatio = 0.8)
train3 <- subset(train1$mvar60, split == T)
cv <- subset(train1$mvar60, split == F)
summary(cv)
summary(cv )
set.seed(1234)
split <- sample.split(train1$mvar60, SplitRatio = 0.8)
train3 <- subset(train1, split == T)
cv <- subset(train1, split == F)
summary(cv)
rf.predict <- predict(rf.model, newdata=cv)
print(rf.cm <- confusionMatrix(rf.predict, cv$mvar58))
print(rf.cm <- confusionMatrix(rf.predict, cv$mvar60))
leader=fread("leader.csv")
summary(leader)
library("xgboost", lib.loc="~/R/win-library/3.3")
library("caret", lib.loc="~/R/win-library/3.3")
library("data.table", lib.loc="~/R/win-library/3.3")
train3 <- as.data.frame(lapply(train3, as.numeric))
cv <- as.data.frame(lapply(cv, as.numeric))
data.train3 <- xgb.DMatrix(data = data.matrix(train3[, 1:ncol(train3)]), mvar60 = train3$mvar60)
data.cv <- xgb.DMatrix(data = data.matrix(cv[, 1:ncol(cv)]), mvar60 = cv$mvar60)
mvar_cv60=cv$mvar60
mvar_train60=train3$mvar60
cv$mvar60=NULL
train3$mvar60=NULL
cv$mvar60=mvar_cv60
train3$mvar60=mvar_train60
data.train3 <- xgb.DMatrix(data = data.matrix(train3[, (1:ncol(train3)-1)]), mvar60 = train3$mvar60)
data.cv <- xgb.DMatrix(data = data.matrix(cv[, 1:ncol(cv)-1]), mvar60 = cv$mvar60)
data.train3 <- xgb.DMatrix(data = data.matrix(train3[, (1:ncol(train3)-1)]), label = train3$mvar60)
data.cv <- xgb.DMatrix(data = data.matrix(cv[, 1:ncol(cv)-1]), label= cv$mvar60)
watchlist <- list(train  = data.train, test = data.cv)
watchlist <- list(train  = data.train3, test = data.cv)
parameters <- list(
# General Parameters
booster            = "gbtree",          # default = "gbtree"
silent             = 0,                 # default = 0
# Booster Parameters
eta                = 0.3,               # default = 0.3, range: [0,1]
gamma              = 0,                 # default = 0,   range: [0,∞]
max_depth          = 6,                 # default = 6,   range: [1,∞]
min_child_weight   = 1,                 # default = 1,   range: [0,∞]
subsample          = 1,                 # default = 1,   range: (0,1]
colsample_bytree   = 1,                 # default = 1,   range: (0,1]
colsample_bylevel  = 1,                 # default = 1,   range: (0,1]
lambda             = 1,                 # default = 1
alpha              = 0,                 # default = 0
# Task Parameters
objective          = "multi:softmax",   # default = "reg:linear"
eval_metric        = "merror",
num_class          = 10,
seed               = 1234				# reproducability seed
)
xgb.model <- xgb.train(parameters, data.train3, nrounds = 9, watchlist)
xgb.predict <- predict(xgb.model, data.cv)
print(xgb.cm <- confusionMatrix(xgb.predict, cv$label))
xgb.predict <- predict(xgb.model, data.cv)
print(xgb.cm <- confusionMatrix(xgb.predict, cv$mvar60))
xgb.model <- xgb.train(parameters, data.train3, nrounds = 20, watchlist)
xgb.predict <- predict(xgb.model, data.cv)
print(xgb.cm <- confusionMatrix(xgb.predict, cv$mvar60))
summary(leader)
train1$mvar61=(train1$mvar43+2*train1$mvar44+3*train1$mvar45)/(train1$mvar40+2*train1$mvar41+3*train1$mvar42)
set.seed(1234)
split <- sample.split(train1$mvar60, SplitRatio = 0.8)
train3 <- subset(train1, split == T)
cv <- subset(train1, split == F)
set.seed(4)
rf.model <- randomForest(mvar60 ~ ., data = train1, ntree = 100, nodesize = 50)
sumarry(train1)
summary(train1)
train1$mvar61[train1$mvar61==NA]=0
set.seed(4)
rf.model <- randomForest(mvar60 ~ ., data = train1, ntree = 100, nodesize = 50)
summary(train1)
savehistory("C:/Users/yashj/Desktop/Amex/history.Rhistory")
