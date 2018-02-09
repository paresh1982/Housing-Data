glimpse(dataset_resale_1)
#dataset_resale_1$resale_price <- dataset_resale_1$resale_price/1000

train <- dataset_resale_1 %>% filter(year %in% c(2000:2015))
test <- dataset_resale_1 %>% filter(year %in% c(2016, 2017))


dim(train)
dim(test)


install.packages("h2o")
library(h2o)

#launch the H2O cluster
localH2O <- h2o.init(nthreads = -1)

#transfer the data from R to h2o instance.
#data to h2o cluster
train.h2o <- as.h2o(train[, -1])
test.h2o <- as.h2o(test[, -c(1,19)])

colnames(train.h2o)
colnames(test.h2o)

#dependent variable (resale_price)
y.dep <- 18

#independent variables
x.indep <- c(1:17)

#Random Forest in H2O
#Random Forest
system.time(
  rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, 
                                    ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122)
)

h2o.performance(rforest.model)

#check variable importance
h2o.varimp(rforest.model)

#making predictions on unseen data
system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))
pred_rf <- predict.rforest$predict

# Calculate the RMSE of the predictions
test %>% mutate(residual = resale_price - pred_rf) %>% summarise(rmse = sqrt(mean(residual^2)))
test %>% mutate(resi.sq = (resale_price - pred_rf)^2, tot.sq = (resale_price - mean(resale_price))^2) %>% 
  summarise(sqr.R = 1-(sum(resi.sq)/sum(tot.sq)))
# base data----rmse 88009.45, sqr.R 0.626596
# base data----rmse 96815.82, sqr.R 0.5481306
plot(pred_rf, test$resale_price)

#GBM
system.time(
  gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122)
)

h2o.performance (gbm.model)

#making prediction and writing submission file
predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))
pred_gbm <- predict.gbm$predict

# Calculate the RMSE of the predictions
test %>% mutate(residual = resale_price - pred_gbm) %>% summarise(rmse = sqrt(mean(residual^2)))
test %>% mutate(resi.sq = (resale_price - pred_gbm)^2, tot.sq = (resale_price - mean(resale_price))^2) %>% 
  summarise(sqr.R = 1-(sum(resi.sq)/sum(tot.sq)))
# base data-----rmse 46866.72 sqr.R 0.8941113
# worked data-----rmse 46.74852 sqr.R 0.8946448

plot(pred_gbm, test$resale_price)


#deep learning models
system.time(
  dlearning.model <- h2o.deeplearning(y = y.dep,
                                      x = x.indep,
                                      training_frame = train.h2o,
                                      epoch = 60,
                                      hidden = c(100,100),
                                      activation = "Rectifier",
                                      seed = 1122
  )
)

h2o.performance(dlearning.model)

predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
pred_dl2 <- predict.dl2$predict

# Calculate the RMSE of the predictions
test %>% mutate(residual = resale_price - pred_dl2) %>% summarise(rmse = sqrt(mean(residual^2)))
test %>% mutate(resi.sq = (resale_price - pred_dl2)^2, tot.sq = (resale_price - mean(resale_price))^2) %>% 
  summarise(sqr.R = 1-(sum(resi.sq)/sum(tot.sq)))
caret::RMSE(pred_dl2, test$resale_price)
#base rmse 44517.58, sqr.R 0.9044604
#base rmse 42.73143, sqr.R 0.91197323
#base rmse 37.94937, sqr.R 0.9305728
