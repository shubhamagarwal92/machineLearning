library(caret)
library(h2o)
require(dplyr)
# h2o.shutdown()
localH2O = h2o.init(nthreads=-2)

dataDir = "path-to-dir"
load(paste0(dataDir,"train.rda"))
load(paste0(dataDir,"valid.rda"))
load(paste0(dataDir,"test.rda"))

set.seed(1234)

trainH2o = as.h2o(trainSampled)
validationH2o = as.h2o(validationSampled)

system.time(
rf = h2o.randomForest(x=2:20,y=1,
                      importance =T,
                      training_frame = trainH2o,
                      mtries = 3,ntrees = 100)
)
h2o.confusionMatrix(rf)

h2o.performance(rf)
h2o.varimp(rf)

predict.reg <- as.data.frame(h2o.predict(rf, validationH2o))
