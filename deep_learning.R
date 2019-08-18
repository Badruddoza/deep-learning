## Numeric example
# install.packages('neuralnet')
library('neuralnet')
set.seed(123)
# generates 50 observations without replacement over the range -2 to +2
attribute = as.data.frame(sample(seq(-2, 2, length=50), 50, replace=FALSE), ncol=1)
response = attribute^2
data = cbind(attribute, response)
colnames(data) = c('attribute', 'response')
head(data, 10)
plot(data, pch=20, col=2)
# plot to see
x = data$attribute[order(data$attribute)]
y = data$response[order(data$attribute)]
lines(x, y, col=8, lty=3, lwd=2)
fit = neuralnet(response~attribute, data=data, hidden=c(3, 3), threshold =0.01, act.fct="logistic")
plot(fit)
testdata = as.matrix(sample(seq(-2, 2, length =10), 10, replace=FALSE), ncol =1)
pred = compute(fit, testdata)
cor(pred$net.result, testdata^2)
result = cbind(testdata, pred$net.result, testdata^2)
colnames(result) = c('Attribute', 'Prediction', 'Actual')
round(result, 4)
# prepare data for plot 
x = result[,"Attribute"][order(result[,"Attribute"])]
y_act = result[,"Actual"][order(result[,"Attribute"])]
y_pred = result[,"Prediction"][order(result[,"Attribute"])]
par(mfrow=c(1,2))
# plot actual data
plot(x, y_act, pch=20, col=2, xlab='Attribute', ylab="Actual")
lines(x, y_act, col=8, lty=3, lwd=2)
# plot predict data
plot(x, y_pred, pch=20, col=1, xlab='Attribute', ylab="Predict")
lines(x, y_pred, col=8, lty=3, lwd=2)

## Classification
#install.packages('neuralnet')
library('neuralnet')
# 2 hidden layers with 3 and 4 nodes respectively
hidd = c(3, 4)
indexesTrain = sample(1:150, 70)
indexesValidate = setdiff(1:150, indexesTrain)
train = iris[indexesTrain,]
# classes labeling
train$setosa = c(train$Species == "setosa")
train$versicolor = c(train$Species == "versicolor")
train$virginica = c(train$Species == "virginica")
fit = neuralnet(setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, train, hidden=hidd, lifesign="full")
predict = compute(fit, iris[1:4])
maxidx = function(arr) {
  return(which(arr == max(arr)))
}
idx = apply(predict$net.result, c(1), maxidx)
prediction = c('setosa', 'versicolor', 'virginica')[idx]
table(prediction, iris$Species)
#install.packages('nnet')
library(nnet)
targets = class.ind(iris$Species)
fit2 = nnet(train[, -5], targets[indexesTrain,], size=10, softmax=TRUE)
predict(irisANN, irisdata[irisValData,-5], type="class")