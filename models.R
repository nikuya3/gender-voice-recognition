library(caret)

set <- read.csv('voice.csv')
set_size <- nrow(set)
training_size <- round(set_size * 0.8)
test_size <- set_size - training_size
trials <- 20
output <- 'output.txt'
accuracies <- list()
fastMethods <- c('rpart', 'knn')
allMethods <- c('rpart', 'knn', 'glm', 'C5.0', 'ctree', 'lda', 'svmLinear', 'rf')
for (trial in 1:trials) {
  print(paste0('Trial ', trial))
  training_set_indices <- sample(set_size, training_size)
  test_set_indices <- setdiff(1:set_size, training_set_indices)
  for (method in allMethods) {
    print(paste0('Using method ', method))
    model <- train(label ~ ., data = set[training_set_indices,], method = method)
    set.test <- set[test_set_indices,]
    prediction <- predict(model, set.test)
    accuracy <- confusionMatrix(prediction, set.test$label)$overall['Accuracy']
    print(accuracy)
    accuracies[[method]][[trial]] <- accuracy
  }
}

for (method in names(accuracies)) {
  accuracies[[method]] <- mean(accuracies[[method]])
}

sink(output)
print(accuracies)
sink()
