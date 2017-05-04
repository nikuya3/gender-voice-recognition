library(warbleR)
library(caret)
imgPattern <- "tiff"
wavs <- list.files(pattern="wav$")
detected <- autodetec(flist = wavs, it = imgPattern)
sigDetected <- sig2noise(detected, mar = 0.04)
selection <- sigDetected[ave(-sigDetected$SNR, FUN = rank) <= 5, ]
params <- specan(selection)
control <- trainControl(method="cv", number=12)
model.forest <- train(label ~ ., data = file, method = "rf", metric = "Accuracy", trControl = control)
model.forest