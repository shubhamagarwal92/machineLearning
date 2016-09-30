library(caret)
library(h2o)
require(dplyr)

# h2o.shutdown()
# Sys.setenv("OPENBLAS_MAIN_FREE"=1)
localH2O <- h2o.init(nthread=16, max_mem_size="32g")

dataDir = "path-to-dir"
load(paste0(dataDir,"train.rda"))
load(paste0(dataDir,"validation.rda"))
load(paste0(dataDir,"test.rda"))

modelDir = paste0(dataDir,"models/")
infoStr = ""


# If need to remove some features and convert labels to 0 and 1
removeFeatures = function(dataFrame){
  dataFrame$feature1 = NULL
  dataFrame$feature2 = NULL
  dataFrame$action = as.factor(as.character(ifelse(dataFrame$label=='label1','1','0')))
  dataFrame
}

test = removeFeatures(test)
validation = removeFeatures(validation)
j = 2
fileName = paste0(dataDir,"trainingRatio",j,".rda")
load(fileName)
training = removeFeatures(training)
col = ncol(training)
train = training
rm(training)
max_depth	= 15
mtries = 3
ntrees = 100

valid = validation
model = h2o.randomForest(model_id = paste0("model0"),
                         x=c(2:col),y = 1,training_frame=as.h2o(train),validation_frame = as.h2o(valid),
                         max_depth = max_depth,mtries = mtries,ntrees = ntrees,stopping_metric = "AUC"
)
h2o.saveModel(model,path = modelDir,force = FALSE)
rm(model)

save.image(paste0(modelDir,"model.RData"))

# Get test results
getTestResults = function(model,test,resultFrame,testType,confList,criteria){
  predictFrame <- as.data.frame(h2o.predict(model,as.h2o(test[,2:ncol(test)])))
  if(criteria == 'f2')
  {threshold = model@model$validation_metrics@metrics$max_criteria_and_metric_scores[2,2]
  } else if(criteria == 'f0point5'){
    threshold = model@model$validation_metrics@metrics$max_criteria_and_metric_scores[3,2]
  }
  else{
    threshold = model@model$validation_metrics@metrics$max_criteria_and_metric_scores[1,2]
  }
  pred <- factor( ifelse(predictFrame[, 'p1'] >= threshold, '1', '0') )
  y = test[,1]
  confMatrix<-confusionMatrix(pred,y, positive="1")
  confType = paste(testType,criteria,sep="_")
  confIndex = paste(model@model_id,confType,sep = "_")
  confList[[confIndex]] = t(confMatrix$table)
  precision <- posPredValue(pred, y,positive="1")
  recall 	<- sensitivity(pred, y,positive="1")
  F1 <- (2 * precision * recall) / (precision + recall)
  beta =2 
  F2 = (beta*beta + 1)*precision*recall / (beta*beta*precision + recall)
  results = data.frame(precision,recall,F1,F2)
  results$type = testType
  resultFrame = rbind(resultFrame,results)
  return(list(resultFrame,confList))
}	
getCriteriaResults = function(modelDir,model_id,test,criteria,overAllResult,confList){
  modelName = paste0(modelDir,model_id)
  model = h2o.loadModel(modelName)
  results = data.frame()
  temp = getTestResults(model,test,results,"test",confList,criteria)
  results = temp[[1]]
  results$maxCriteria = criteria
  results$model_id = model_id
  overAllResult = rbind(overAllResult,results)
  temp[[1]] = overAllResult
  return(temp)
}
results = data.frame()
confList = list()
resultsList = getCriteriaResults(modelDir,"model0",test,"f2",results,confList)
resultsList = getCriteriaResults(modelDir,"model0",test,"f1",resultsList[[1]],resultsList[[2]])
resultsList = getCriteriaResults(modelDir,"model0",test,"f0point5",resultsList[[1]],resultsList[[2]])
results = resultsList[[1]]
confList = resultsList[[2]]
confMatrix  = do.call("rbind",confList)
