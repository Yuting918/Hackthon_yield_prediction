# DATA
train<- read.table("training.csv", sep=",", header = T)
test <- read.table("testing.csv", sep=",", header = T)

## remove NA
train <- train[complete.cases(train), ]

# Modeling
library(doParallel)
library(caret)
registerDoParallel(cores = 6)
# detectCores() 8 cores for the computer

## RandomForest
system.time(
  m_rf <- train(Yield ~ . - fid - SR_L - NR_L, data = train,
                method = "rf", importance = T,
                trControl = trainControl(method = 'none'), metric = "RMSE" ) # method=rf random forest
)
pred_rf <- predict(m_rf)
cor(train$Yield, pred_rf)**2
# [1] 0.9502894

## Regulized Random Forest Method
grid <- data.frame(mtry=seq(2,20,by=2))
trcontrol <- trainControl(method = "cv",p = 0.7,search = "grid")
system.time(
  m_rf <- train(Yield ~ . - fid - SR_L - NR_L, data = train,
                method = "RRF", importance = T,
                trControl = trcontrol, metric = "Rsquared") # method=rf random forest
)
pred_rf <- predict(m_rf)
cor(train$Yield, pred_rf)**2
# [1] 0.9515566

grid <- data.frame(mtry=seq(2,20,by=1))
trcontrol <- trainControl(method = "cv",p = 0.7,search = "grid")
system.time(
  m_rf <- train(Yield ~ . - fid - SR_L - NR_L, data = train,
                method = "RRF", importance = T,
                trControl = trcontrol, metric = "Rsquared") # method=rf random forest
)
pred_rf <- predict(m_rf)
cor(train$Yield, pred_rf)**2
# [1] 0.9525071

grid <- data.frame(mtry=seq(2,20,by=2))
trcontrol <- trainControl(method = "cv",p = 0.5,search = "grid")
system.time(
  m_rf <- train(Yield ~ . - fid - SR_L - NR_L, data = train,
                method = "RRF", importance = T,
                trControl = trcontrol, metric = "Rsquared") # method=rf random forest
)
pred_rf <- predict(m_rf)
cor(train$Yield, pred_rf)**2
# [1] 0.9514461

train$Pred_Y <- pred_rf # happy outcome for the training dataset
test$Yield <- predict(m_rf, newdata = test)
write.csv(test[,c("fid","Yield")], file = "submission.csv", row.names = FALSE)
train$Pred_Y <- pred_rf