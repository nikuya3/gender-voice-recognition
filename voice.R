### Voice Recognition for Data Science ###
##########################################

### Load Packages
#################
rm(list=ls())
library(readr)
library(MASS)
library(caret)
voice <- read_csv("voice.csv")
summary(voice)

### bring data in shape
#######################
voice$sex <- voice$label
voice$sex[voice$label == "female"] <- 0
voice$sex[voice$label == "male"] <- 1 
voice$sex <- as.factor(voice$sex)
voice$label <- NULL

### Compute Logit Model on all attributes
#########################################
logit1 = glm(sex ~ ., family = binomial(link = "logit"), data = voice)
summary(logit1)
# lets find the ideal attributes for Logit Model.
step <- stepAIC(logit1, direction="both")
### Display ideal attributes
step$anova
### Compute suggested Logit Model
logit2 = glm(sex ~ Q25 + Q75 + kurt + sp.ent + sfm + meanfun + minfun + 
               modindx, family = binomial(link = "logit"), data = voice)
summary(logit2)
# Intercept seems not necessary so, the model is computed without
logit3 = glm(sex ~ 0 + Q25 + Q75 + kurt + sp.ent + sfm + meanfun + minfun + 
               modindx, family = binomial(link = "logit"), data = voice)
summary(logit3)
# The AIC of model3 has decrease a little compared to model2

### Model Evaluation
####################
# split data into subsets -> trainingset(.8) and testset(.2)
x <- createDataPartition(voice$sex, p=0.80, list=FALSE)
training <- voice[x,]
testing <- voice[-x,]
mod_fit <- train(sex ~ 
           0 + Q25 + Q75 + kurt + sp.ent + sfm + meanfun + minfun + modindx,
           data=training, method="glm", family="binomial")
### Get an idear aabout accuracy of prediction
predict(mod_fit, newdata=testing)
#predict(mod_fit, newdata=testing, type = "prob")
pred = predict(mod_fit, newdata=testing)
confusionMatrix(data=pred, testing$sex)

### K-Fold Cross Validation
###########################
ctrl <- trainControl(method = "repeatedcv", number = 15, savePredictions = TRUE)
mod_fit <- train(sex ~ 
           0 + Q25 + Q75 + kurt + sp.ent + sfm + meanfun + minfun + modindx,
           data=voice, method="glm", family="binomial",
           trControl = ctrl, tuneLength = 8)
pred = predict(mod_fit, newdata=testing)
confusionMatrix(data=pred, testing$sex)


