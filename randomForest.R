library(randomForest)

head(eda)

eda$binN <- as.factor(eda$binN)
df <- subset(eda, select = c(2, 6:23, 26)) #input = BW, CP, AAs
#df <- subset(eda, select = c(7:23, 26)
table(df$binN)

#train, test dataset
library(tidymodels)

df %>% initial_split(prop = 0.7) -> df_pre
df_pre %>% training() ->train
df_pre %>% testing()  ->test

#Random Forest
poline <- randomForest(binN~., data = train, proximity = TRUE)
print(poline)

#Prediction & Confusion Matrix – train data
p1 <- predict(poline, train)
confusionMatrix(p1, train$binN) #accuracy 97%

#Prediction & Confusion Matrix – test data
p2 <- predict(poline, test)
confusionMatrix(p2, test$binN) #accuracy  82%

plot(poline)

#Variable Importance
#MeanDecreaseGini(MDG) -> Gini importance score
varImpPlot(poline,
           sort = T,
           n.var = 20,
           main = "Top 20 - Variable Importance")

#data saving
save(train,file = "trainData.rda" )
save(test,file = "testData.rda" )

#----use rf model----

testData <- df[430,]

poline %>% predict(testData)


