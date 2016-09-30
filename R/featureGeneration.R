require(dplyr)
# require(ggplot2)
# require(reshape2)
dataDir = "path-to-dir"

load(paste0(dataDir,"totalData.RData"))
# load("totalData.rda")

# table(totalData$classLabel)
getQueryData <- function (query) {
  mydb = dbConnect(MySQL(), user='user', password='pwd', dbname='dbname', host='hostId')
  rs = suppressWarnings(dbSendQuery(mydb, query))
  data = fetch(rs, n=-1)
  suppressWarnings(dbDisconnect(mydb))
  data
}


totalData$label = as.factor(totalData$label)
totalData$numericFeature = as.numeric(totalData$numericFeature)
totalData$factorFeature = as.factor(totalData$factorFeature)
totalData$timestamp <- as.POSIXct(totalData$timestamp)
totalData$daysFeature = as.numeric(floor(difftime(totalData$timestamp,as.POSIXct(totalData$anotherTimestamp),units = "days")))
# Other data cleaning steps

# save(totalData,file="/mnt/data/totalData.rda")

#Preparing train and test data. Train=2/3 of data, Test=1/3 of data.
# train = totalData[1:floor(nrow(totalData)*2/3),]
# test =  totalData[(nrow(train)+1):nrow(totalData),]

totalData$timestamp = as.POSIXct(totalData$timestamp)
save(totalData,file=paste0(dataDir,"totalData.rda"))

# 
getSampledData = function(train,weight){
  label1Sampled = train %>% filter(label == 'label1')
  label2Sampled = train %>% filter(action == 'label2')  %>% sample_n(size=(dim(label1Sampled)[1]*weight), replace=FALSE)
  trainSampled = rbind(label1Sampled,label2Sampled)
  trainSampled <- trainSampled[sample(nrow(trainSampled)),]
}
# 
trainSampled = getSampledData(train,1)
testSampled = getSampledData(test,1)

write.table(trainSampled,file = "trainSampled.csv",sep="\t",row.names = FALSE)
write.table(testSampled,file = "testSampled.csv",sep="\t",row.names = FALSE)
