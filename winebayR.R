winebay<-wine
winebay$Cultivars<-factor(winebay$Cultivars)
winebay$Cultivars
str(winebay$Cultivars)
install.packages("GGally")
library('Ggally')
ggpairs(winebay)
set.seed(123)
split <- sample(2, nrow(winebay), replace = TRUE, prob = c(0.8,
                                                           0.2))
training_set <- winebay[split == 1, ]
test_set <- winebay[split == 2, ]
nrow(training_set)
nrow(test_set)
library(klaR)

nb_model <- NaiveBayes(Cultivars ~.,data = training_set,usekernel = TRUE,fL = 1)
nb_model
model<-NaiveBayes(Cultivars~.data = training_set,fL = 1,prob=TRUE)
model
y_pred <- predict(nb_model, data = test_set, type = "class")
y_pred
y_pred <- y_pred$class
y_pred <- factor(y_pred , levels = c("1", "2","3"))
library(caret)
confusionMatrix(y_pred, training_set$Cultivars)
length(training_set$Cultivars)

