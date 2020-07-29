#Libraries Used
library(survey)
library(cluster)
library(fpc)
library(matrixStats)

#read in the file, making sure all blanks are read as NA
questions=read.csv("formatted.csv", header=T, na.strings=c("","NA"))
str(study)
dim(study)
names(questions)

#Shows the amount of missing data
aggr(questions)

#removes the junk considered to be NA
questions$Diet.Switched[questions$Diet.Switched=="Donâ???Tt know"] = NA
questions$Diet.Switched=factor(questions$Diet.Switched)

#columns to run the cluster on
cols=c(2,8)

#check to make sure there are or are not NA values
is.na(questions[,cols])

#Run KMeans (five clusters as there are at most 5 times switched)
set.seed(10)
clusters=kmeans(na.omit(questions[,cols]),5)

#looks at the cluster created
clusters$centers
clusters$size
table(na.omit(questions$Diet.Switched), clusters$cluster)
study=na.omit(questions[,cols])
attach(study)
#plots of the clusters
clusplot(na.omit(questions),clusters$cluster, color=FALSE, col.study=2, labels=5, main="Clusters of Age and Times Switched")
plot(na.omit(questions[c("Diet.Switched","Age")]), col=clusters$cluster)
names(questions)
plotcluster(study,clusters$cluster,pch=clusters$cluster)

#Summary/Descriptive Stats
test=data.matrix(na.omit(questions))
summary(test)
mean=colMeans(test)
med=colMedians(test)
std=colMads(test)
var=colVars(test)
summ=rbind(mean,med,std,var)
summ

#Dendogram Cluster
set.seed(23)
questionsdag=agnes(study,diss=FALSE,metric="euclidian")
plot(questionsdag, main="Dendogram")
groups=cutree(questionsdag,k=5)
rect.hclust(questionsdag, k=5, border="red")
questionsdag$ac

plot(study$Age,study$Diet.Switched)

test=diana(study)
rect.hclust(test, k=5, border="red")
grouped=cutree(as.hclust(test),k=5)
plot(grouped)
test$dc
plot(test)

groups
questionsdag$ac

head(groups)

#combine the cluster association as a column in questions survey data
length(study[,1])
clusters$cluster

completeData=data.frame(na.omit(questions), clusters$cluster)

#Elminates all groups that are not group 3 or group 5

cluster3=completeData[+ grep(3, completeData$clusters.cluster),]
cluster5=completeData[+ grep(5, completeData$clusters.cluster),]

#Running regressions

summary(lm(Age~Diet.Switched+B12.deficiency+Iron.deficiency+Protein.deficiency, data=cluster3))
summary(lm(Age~Diet.Switched+B12.deficiency+Iron.deficiency+Protein.deficiency, data=cluster5))

summary(lm(Age~B12.deficiency+Iron.deficiency+Protein.deficiency, data=cluster3))
summary(lm(Age~B12.deficiency+Iron.deficiency+Protein.deficiency, data=cluster5))

#Summary stats on 2 cluster datasets

clusters3=data.matrix(cluster3)
mean=colMeans(clusters3)
med=colMedians(clusters3)
std=colMads(clusters3)
var=colVars(clusters3)
summ=rbind(mean,med,std,var)
summ

clusters5=data.matrix(cluster5)
mean=colMeans(clusters5)
med=colMedians(clusters5)
std=colMads(clusters5)
var=colVars(clusters5)
summ=rbind(mean,med,std,var)
summ

