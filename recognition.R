library(fftw)
library(seewave)
library(tuneR)
library(caret)

humanFrequency <- 280

analyzeWav <- function(file, start = 0, end = 20) {
  wave <- file
  tuneWave <- readWave(file.path(getwd(), wave), from = start, to = end, units = "seconds")
  waveSpec <- spec(tuneWave, f = tuneWave@samp.rate, plot = F)
  analysis <- specprop(waveSpec, f = tuneWave@samp.rate, flim = c(0, humanFrequency / 1000), plot = F)
  
  meanfreq <- analysis$mean / 1000
  sd <- analysis$sd / 1000
  median <- analysis$median / 1000
  Q25 <- analysis$Q25 / 1000
  Q75 <- analysis$Q75 / 1000
  IQR <- analysis$IQR / 1000
  skew <- analysis$skewness
  kurt <- analysis$kurtosis
  sp.ent <- analysis$sh
  sfm <- analysis$sfm
  mode <- analysis$mode / 1000
  centroid <- analysis$cent / 1000
  
  fundamental <- fund(tuneWave, f = tuneWave@samp.rate, ovlp = 50, threshold = 5, wl = 2048,
                      ylim = c(0, humanFrequency / 1000), fmax = humanFrequency, plot = F)
  
  meanfun <- mean(fundamental[, 'y'], na.rm = T)
  minfun <- min(fundamental[, 'y'], na.rm = T)
  maxfun <- max(fundamental[, 'y'], na.rm = T)
  
  b <- c(0, 22)
  dom <- dfreq(tuneWave, f = tuneWave@samp.rate, wl = 2048, ylim = c(0, humanFrequency / 1000),
               ovlp = 0, threshold = 5, bandpass = b * 1000, fftw = T, plot = F)[, 2]
  
  meandom <- mean(dom, na.rm = TRUE)
  mindom <- min(dom, na.rm = TRUE)
  maxdom <- max(dom, na.rm = TRUE)
  dfrange <- (maxdom - mindom)
  duration <- (end - start)
  
  changes <- vector()
  for(d in which(!is.na(dom))) {
    change <- abs(dom[d] - dom[d + 1])
    changes <- append(changes, change)
  }
  if(mindom == maxdom) modindx <- 0 else modindx <- mean(changes, na.rm = T) / dfrange
  
  obj <- data.frame(duration, meanfreq, sd, median, Q25, Q75, IQR, skew, kurt, sp.ent, sfm, mode, 
                    centroid, meanfun, minfun, maxfun, meandom, mindom, maxdom, dfrange, modindx)
  names(obj) <- c("duration", "meanfreq", "sd", "median", "Q25", "Q75", "IQR", "skew", "kurt", "sp.ent", 
                  "sfm","mode", "centroid", "meanfun", "minfun", "maxfun", "meandom", "mindom", "maxdom",
                  "dfrange", "modindx")
  obj
}

erwin <- analyzeWav('erwin.wav')
control <- trainControl(method = "cv", number = 12)
#model.forest <- train(label ~ ., data = file, method = "rf", metric = "Accuracy", trControl = control)
model.forest <- readRDS('model.forest.rds')
predict(model.forest, obj)

### -- ###

imgPattern <- "tiff"
#wavs <- list.files(pattern="wav$")
detected <- autodetec(flist = wavs, it = imgPattern)
sigDetected <- sig2noise(detected, mar = 0.04)
selection <- sigDetected[ave(-sigDetected$SNR, FUN = rank) <= 5, ]
params <- specan(selection)
file <- read.csv('voice.csv')
model.forest <- train(label ~ ., data = file, method = "rf", metric = "Accuracy", trControl = control)
predict(model.forest, obj)