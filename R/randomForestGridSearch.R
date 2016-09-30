library(caret)
library(h2o)
require(dplyr)
# h2o.shutdown()
Sys.setenv("OPENBLAS_MAIN_FREE"=1)
localH2O <- h2o.init(nthread=16, max_mem_size="32g")

dataDir = "path-to-dir"
load(paste0(dataDir,"train.rda"))
load(paste0(dataDir,"validation.rda"))
load(paste0(dataDir,"test.rda"))

modelDir = paste0(dataDir,"models/")
infoStr = ""

removeFeatures = function(dataFrame){
  dataFrame$feature1 = NULL
  dataFrame$feature2 = NULL
  dataFrame$action = as.factor(as.character(ifelse(dataFrame$label=='label1','1','0')))
  dataFrame
}

test = removeFeatures(test)
validation = removeFeatures(validation)

for(j in 1:8) {
  fileName = paste0(dataDir,"trainingRatio",j,".rda")
  load(fileName)
  training = removeFeatures(training)
  col = ncol(training)
  training = as.h2o(training)
  grid_space <- list()
  # \\ToDo - search a much bigger space
  grid_space$ntrees <- c(500,1000,1500)
  grid_space$max_depth <- c(5,10,15)
  grid_space$mtries <- c(3,4,5,6,7)
  grid_space$sample_rate <- c(0.25,0.5,1)
  
  #Testing grid space use
  # grid_space <- list()
  # grid_space$ntrees <- c(500)
  # grid_space$max_depth <- c(5,10)

  ptm = proc.time()[3]
  grid_id = paste0("grid_rf",j)
  h20.rf2 <- h2o.grid(grid_id = grid_id,"randomForest",x=c(2:col),y = 1,training_frame=training,validation_frame = as.h2o(validation), hyper_params = grid_space, stopping_metric = "AUC")
  info = paste("Time taken to model:", round(proc.time()[3]-ptm,2),"seconds")
  print(info)
  infoStr = c(infoStr,info)
  aucTable <- h2o.getGrid(grid_id = grid_id, sort_by = "auc", decreasing = TRUE)
  aucFrame = as.data.frame(aucTable@summary_table)
  f1Table <- h2o.getGrid(grid_id = grid_id, sort_by = "f1", decreasing = TRUE)
  f1Frame = as.data.frame(f1Table@summary_table)
  f2Table <- h2o.getGrid(grid_id = grid_id, sort_by = "f2", decreasing = TRUE)
  f2Frame = as.data.frame(f2Table@summary_table)
  f0.5Table <- h2o.getGrid(grid_id = grid_id, sort_by = "f0point5", decreasing = TRUE)
  f0.5Frame = as.data.frame(f0.5Table@summary_table)
  gridFrame = inner_join(f2Frame,f1Frame)
  gridFrame = inner_join(gridFrame,aucFrame)
  gridFrame = inner_join(gridFrame,f0.5Frame)
  rm(f1Frame,f2Frame,aucFrame,aucTable,f1Table,f2Table,f0.5Frame)
  gridFrame = gridFrame[order(gridFrame$f0point5),]
  gridName = paste0("grid",j)
  assign(gridName,gridFrame)
  save(list = paste0(gridName),file=paste0(modelDir,gridName,".rda"))
  rm(gridFrame)
  #Save the models
  len <- length(h20.rf2@model_ids)
  for(k in 1:len) {
    h2o.saveModel(h2o.getModel(h20.rf2@model_ids[[k]]),path = modelDir,force = FALSE)
  }
}
rm(col,grid_id,j,k,len,training)

# When only models saved but no grids
# system(paste0("Rscript ",paste(scriptName,dataDir)))
# source(scriptName)
# gridTotal = getGridValues(modelDir)

# Choosing the best model and predicting
gridTotal = rbind(grid1,grid2)
gridTotal = rbind(gridTotal,grid3)
gridTotal = rbind(gridTotal,grid4)
gridTotal = rbind(gridTotal,grid5)
gridTotal = rbind(gridTotal,grid6)
gridTotal = rbind(gridTotal,grid7)
gridTotal = rbind(gridTotal,grid8)
# gridTotal = rbind(gridTotal,grid9)
save(gridTotal,file=paste0(modelDir,"gridTotal.rda"))

rm(grid1,grid2,grid3,grid4,grid5,grid6,grid7,grid8)
save.image(paste0(modelDir,"grid.RData"))

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

save.image(paste0(modelDir,"grid.RData"))

write.table(results,sep=",",file = paste0(modelDir,"results.csv"),row.names = FALSE)
write.table(confMatrix,file = paste0(modelDir,"confMatrix.csv"))
write.table(gridTotal,sep=",",file = paste0(modelDir,"gridTotal.csv"),row.names = FALSE)

