#separate stars, galaxies, quasars

#you will probably need these later
library(ggplot2)
library(data.table)
library(kernlab)
#change this to your own local repo
#outside of all functions so will be global

root_dir="/Users/ThoughtWorks/Documents/DataCenter/Learning/analytics_summit/TW-summit"
data_dir=root_dir

#your functions below

read.astro.raw<-function(){
   #a simple reader for the astro data-set (SDSS Survey)
   file=paste(data_dir,'/','sdss.csv',sep='')
   #read the csv file into a data.frame
   data=read.csv(file)
   #make it a data.table object
   data=data.table(data)
   return(data)   
}

read.astro<-function(){
   #a simple reader for the astro data-set (SDSS Survey)
   file=paste(data_dir,'/','sdss.csv',sep='')
   #read the csv file into a data.frame
   data=read.csv(file)
   #convert to a data.table, a more powerful data structure
   data=data.table(data)
   #keep only rows with specClass = 1,2,3 which is star,galaxy,quasar
   data=data[specClass %in% c(1,2,3),]
   #replace specClass number with a 'factor' 
   spec.classes=factor(c('star','galaxy','quasar'))
   data[,objtype:=spec.classes[specClass]]
   #drop these columns
   data[,specClass:=NULL]
   data[,objid:=NULL]
   #function defined below
   add.colors(data)
   return(data)
}

add.colors<-function(data){
   #add the colors, note this is side effecting because data.table
   #is passed by reference unlike data.frame
   data[,ug:=u-g]
   data[,gr:=g-r]
   data[,ri:=r-i]
   data[,iz:=i-z]
}

score.my.classifyer<-function(predicted.class,true.class) {
   #calculate the precision, recall and F-score for 
   #predicted and true binary classifications
  
   
   #check to make sure there are logicals TRUE and FALSE
   #stopifnot(class(predicted.class)=="logical")
   
}

test.logistic<-function(){
   num=300
   d1=data.table(x=rnorm(num),y=rnorm(num),cl="quasar")
   d2=data.table(x=rnorm(num)+2,y=rnorm(num)+2,cl="not_quasar")
   d=rbind(d1,d2)
   p<-ggplot(d,aes(x,y,colour=cl))+geom_point()
   print(p)
   mod=glm(class~x+y,data=d)
   print(mod)
}

test.log<-function(d=read.astro()){
   d[,quasar:=objtype == "quasar"]
   mod=glm(formula=quasar~ug+gr,data=d)   
   print(mod)
   d[,logit:=predict(mod,d)]
   d[,predictor:=exp(logit)/(1+exp(logit))]
   d[,cat:=predict(mod,d,type="response")]
   return(d)
}

extract_sample = function(percentage.of.population, data){
  samplePercent = 0.5
  number.of.samples = samplePercent*nrow(data.with.needed.columns)
  return(data.with.needed.columns[sample(1:nrow(data.with.needed.columns), number.of.samples),])
}

score.the.classifier = function(data){
  precision = nrow(data[prediction == objtype,])/nrow(data)
  recall = nrow(data[prediction == 'quasar',])/nrow(data[objtype == 'quasar',])
  f.measure = (2*precision*recall)/(precision+recall)
  return(c(precision,recall,f.measure))
}

data = read.astro()

#take out the columns for training
keeping.columns = c("ug","gr","ri","iz","objtype")
data.with.needed.columns = subset(data,select=keeping.columns)

samplePercent = 0.5
extract_sample(samplePercent, data.with.needed.columns)

#split the sample int training and validation sets
training.data.size = 0.5 * nrow(sample.data)
training.data = head(sample.data,n = training.data.size)
validation.data.size = 0.5 * nrow(sample.data)
validation.data = tail(sample.data, n = validation.data.size)

#machine learning algorithm
rbf = rbfdot(sigma=0.5)
svm.object = ksvm(objtype~.,data=training.data,kernel=rbf)

#score the classifier
columns.for.scoring = c("prediction","objtype")

training.data[,prediction:=predict(svm.object,training.data)]
training.data.for.scoring = subset(training.data,select=columns.for.scoring)
training.data.score = score.the.classifier(training.data.for.scoring)
cat("Training data", "F-score", training.data.score[3], "precision", training.data.score[1], "recall", training.data.score[2], '\n')
validation.data[,prediction:=predict(svm.object,validation.data)]
validation.data.for.scoring = subset(validation.data,select=columns.for.scoring)
validation.data.score = score.the.classifier(validation.data.for.scoring)
cat("Validation data", "F-score", validation.data.score[3], "precision", validation.data.score[1], "recall", validation.data.score[2], '\n')