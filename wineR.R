head(wine)
summary(wine)
dataset<-wine
str(dataset)
#Transformar variável Cultivars em factor
levels(dataset$Cultivars)
dataset$Cultivars<-factor(dataset$Cultivars)
levels(dataset$Cultivars)
#normalizar dados
normalize <- function(x) {
  + return((x-min(x))/(max(x)-min(x)))}
dataset_x_norm <- as.data.frame(lapply(dataset[,2:14], normalize))
#Juntar coluna Cultivars aos dados normalizados
dataset_norm <- cbind(dataset_x_norm, dataset$Cultivars)
summary(dataset_norm)
set.seed(123)
split <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.8, 0.2))
training_set <- dataset[split == 1, ]
test_set <- dataset[split == 2, ]
nrow(training_set)
nrow(test_set)
library(class)
library("caret")
wineTrain <- training_set[,-1]
wineTest <- test_set[,-1]
wineTrain_label <- training_set[,1 , drop = TRUE]
wineTest_label <- test_set[,1 , drop = TRUE]
#y_pred = knn(train = training_set[,-1],test = test_set[,-1],cl = training_set[,1],k = 5)
set.seed(123)
wineknns<- knn(train = wineTrain, test=wineTest, cl=wineTrain_label, k=13)
wineknns
#matriz de confusão
table(wineknns, test_set$Cultivars)

model <- train(dataset_x_norm, dataset$Cultivars, method = "knn",trControl = trainControl("cv", number = 5),preProcess=NULL,tuneLength = 20)
plot(model)
model$bestTune

acuracia <- 100 * sum(wineTest_label  == wineknns)/NROW(wineTest_label)