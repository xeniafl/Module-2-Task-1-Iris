IrisDataset <- read.csv('C:/Users/user/Downloads/R Tutorial Data/R Tutorial Data Sets/iris.csv')

attributes(IrisDataset)

summary(IrisDataset)

str(IrisDataset)

library(ggplot2)

boxplot(IrisDataset$Sepal.Length, IrisDataset$Sepal.Width, IrisDataset$Petal.Length, IrisDataset$Petal.Width, names = c("sepal lenght", "sepal width", "petal length", "petal width"))

ggplot(IrisDataset, aes(x = IrisDataset$Sepal.Width, y = IrisDataset$Sepal.Length, color = IrisDataset$Species)) + geom_point() + xlim(0, 8) + ylim(0,8)
ggplot(IrisDataset, aes(x = IrisDataset$Petal.Width, y = IrisDataset$Petal.Length, color = IrisDataset$Species)) + geom_point() + xlim(0, 8) + ylim(0,8)

ggplot(IrisDataset, aes(x = IrisDataset$Sepal.Width, y = IrisDataset$Petal.Width, color = IrisDataset$Species)) + geom_point() + xlim(0, 8) + ylim(0,8)
ggplot(IrisDataset, aes(x = IrisDataset$Sepal.Length, y = IrisDataset$Petal.Length, color = IrisDataset$Species)) + geom_point() + xlim(0, 8) + ylim(0,8)


set.seed(123)
trainSize <- round(nrow(IrisDataset) * 0.8)
testSize <- nrow(IrisDataset) - trainSize
training_indices <- sample(seq_len(nrow(IrisDataset)), size = trainSize)
trainSet <- IrisDataset[training_indices, ]
testSet <- IrisDataset[-training_indices, ]
set.seed(405)
trainSet <- IrisDataset[training_indices, ]
testSet <- IrisDataset[-training_indices, ]

LinearModel <- lm(Petal.Length ~ Petal.Width, trainSet) 
summary(LinearModel)

prediction <- predict(LinearModel, testSet)

plot(IrisDataset$'Petal.Length' ~ IrisDataset$'Petal.Width' )
abline(lm(IrisDataset$'Petal.Length' ~ IrisDataset$'Petal.Width'))

prediction_error <- (testSet$Petal.Length - prediction)
tabla_final_iris <- cbind(testSet, prediction, prediction_error)

sum(abs(prediction_error))/120
sqrt(sum(prediction_error^2)/120)

ggplot(tabla_final_iris, aes(x = tabla_final_iris$Petal.Width, y = tabla_final_iris$prediction, color = tabla_final_iris$Species)) + geom_point() + xlim(0, 8) + ylim(0,8)
ggplot(tabla_final_iris, aes(x = tabla_final_iris$Petal.Length, y = tabla_final_iris$prediction, color = tabla_final_iris$Species)) + geom_point() + xlim(0, 8) + ylim(0,8) + geom_abline()

