winetree<-wine
View(winetree)
str(winetree)
#verificar se existem valores omissos
sum(is.na(winetree))
#hist(winetree$, main= "Wine Quality", col= "blue")
winetree$Cultivars<-factor(winetree$Cultivars)
levels(winetree$Cultivars)
#normalizar dados
normalize <- function(x) {
  + return((x-min(x))/(max(x)-min(x)))}
winetree_x_norm <- as.data.frame(lapply(winetree[,2:14], normalize))
#Juntar coluna Cultivars aos dados normalizados
winetree_norm <- cbind(winetree_x_norm, winetree$Cultivars)
summary(winetree_norm)

#divisão do dataset
set.seed(123)
split <- sample(2, nrow(winetree), replace = TRUE, prob = c(0.8, 0.2))
training_set <- winetree[split == 1, ]
test_set <- winetree[split == 2, ]
nrow(training_set)
nrow(test_set)
library(rpart)
arvore <- rpart(Cultivars ~., training_set )
arvore
plot(arvore)
text(arvore, pretty = 0)

y_pred <- predict(arvore, newdata = test_set, type = "class")
y_pred <- factor(y_pred , levels = c("1", "2","3"))
table(y_pred, test_set$Cultivars)
library(caret)
confusionMatrix(y_pred, test_set$Cultivars)

#prunning

nova_arvore <- prune(arvore, cp=0.1)
nova_arvore
y_pred_prune <- predict(nova_arvore, newdata = test_set, type ="class")
y_pred_prune <- factor(y_pred_prune , levels = c("1", "2","3"))
confusionMatrix(y_pred_prune, test_set$Cultivars)

library(ipred)
set.seed(123)
bag_arvore <- bagging(Cultivars ~., training_set, nbagg = 100)
y_pred_bag <- predict(bag_arvore, newdata = test_set, type ="class")
y_pred_bag <- factor(y_pred_bag , levels = c("1", "2","3"))
confusionMatrix(y_pred_bag, test_set$Cultivars)