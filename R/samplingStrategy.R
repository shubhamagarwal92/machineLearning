require(dplyr)
require(sampling)

#Initialize h2o
# Sys.setenv("OPENBLAS_MAIN_FREE"=1)
# localH2O <- h2o.init(nthread=16, max_mem_size="32g")
#Load total Data
dataDir = "path-to-dir"
load(paste0(dataDir,"train.rda"))
#Set seed so as to replicate random sampling 
set.seed(1)
#Parameters initialization
# dataSize = 75000
trainDays = 40
valDays = 10
testDays = 10
trainStartDate = as.Date(min(totalData$timestamp))
trainEndDate = trainStartDate + trainDays
valStartDate = trainEndDate
valEndDate = valStartDate + valDays
testStartDate = valEndDate
testEndDate = testStartDate + testDays
actionLowerValue = 20


#Any strategy to split totalData into train test validation
trainingCorpus = totalData %>% filter(timestamp>as.POSIXct(trainStartDate) & timestamp<as.POSIXct(trainEndDate))
validationCorpus = totalData %>% filter(timestamp>as.POSIXct(valStartDate) & timestamp<as.POSIXct(valEndDate))
testCorpus = totalData %>% filter(timestamp>as.POSIXct(testStartDate) & timestamp<as.POSIXct(testEndDate))

save(trainingCorpus,file = paste0(dataDir,"train.rda"))
save(validationCorpus,file = paste0(dataDir,"validation.rda"))
save(testCorpus,file = paste0(dataDir,"test.rda"))

createAndSaveDownsampledTrainingData = function(train,ratio,femaleActionsTraining){
  for(i in ratio){
    label1Sampled = train %>% filter(label == 'label1')
    label2Sampled = train %>% filter(action == 'label2')  %>% sample_n(size=(dim(label1Sampled)[1]*weight), replace=FALSE)
    
    training = rbind(label1Sampled,label2Sampled)
    fileName = paste0(dataDir,"trainingRatio",i,".rda")
    print(length(unique(training$user1)))
    print(nrow(training))
    save(training, file = fileName)
    print(i)
  }
}
createAndSaveDownsampledTrainingData(train,ratio=c(1:8))
