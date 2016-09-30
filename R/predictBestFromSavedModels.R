library(h2o)
library(dplyr)

getGridValues = function(modelDir){
  files = list.files(modelDir)
  gridTotal = data.frame()
  for(i in 1:length(files)){
    ptm = proc.time()[3]
    modelName = paste0(modelDir,files[i])
    model = h2o.loadModel(modelName)
    ntrees = model@parameters$ntrees
    max_depth = model@parameters$max_depth
    mtries = model@parameters$mtries
    sample_rate = model@parameters$sample_rate
    model_id = model@parameters$model_id
    auc = model@model$validation_metrics@metrics$AUC
    f1 = model@model$validation_metrics@metrics$max_criteria_and_metric_scores[1,3]
    f2 = model@model$validation_metrics@metrics$max_criteria_and_metric_scores[2,3]
    hideByLike = as.numeric(as.character(substr(model_id,8,8)))
    tempGrid = data.frame(ntrees,max_depth,mtries,sample_rate,model_id,f2,f1,auc,hideByLike)
    gridTotal = rbind(gridTotal,tempGrid)
    info = paste("Time taken to laod:", round(proc.time()[3]-ptm,2),"seconds")
    print(info)
    
  }
  gridTotal
}




