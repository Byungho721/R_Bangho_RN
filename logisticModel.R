#----Logistic model----
library(caTools)
library(ROCR)

eda <- read.csv(file = "binDataFcr.csv")
load(file = "trainData.rda")
load(file = "testData.rda")

df <- subset(eda, select = c(2, 6:23, 26)) #input = sidMetLys, CP, AAs
#df <- subset(eda, select = c(7:23, 26)
table(df$binN)

#Logistic model using train, test
logistic_model <- glm(binN~sidMetLys, data = train, family = "binomial")
logistic_model %>% summary()

#model test
predict_reg <- predict(logistic_model, newdata = test, type = "response")
predict_reg <- ifelse(predict_reg >0.5, 1, 0)
predict_reg

## Evaluating model accuracy
table(test$binN, predict_reg)
missing_classerr <- mean(predict_reg != test$binN)
print(paste('Accuracy =', 1 - missing_classerr))

#coef
coef(logistic_model) -> c
as.data.frame(c) ->c
c[1,1]-> intercept
c[2,1]-> slope

exp(coef(logistic_model)["sidMetLys"])
confint(logistic_model, parm = "sidMetLys") 
exp(confint(logistic_model, parm = "sidMetLys"))

plot(df$sidMetLys, df$binN, xlab = "sidMetLys")
x=seq(min(df$sidMetLys), max(df$sidMetLys), 0.1)
lines(x,(1/(1+(1/exp(slope*x+intercept)))), type="l", col="red")
