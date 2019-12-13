mydata <- read.csv(file.choose())
View(mydata)

help("princomp")

View(mydata[-1])

data <- mydata[-1]
attach(data)
cor(data)
pcaObj<-princomp(mydata[-1],cor = TRUE,Alcohol=TRUE,covmat = NULL)


summary(pcaObj)
str(pcaObj)
loadings(pcaObj)

plot(pcaObj)
biplot(pcaObj)

pcaObj$loadings[,1.3]

mydata<-cbind(mydata,pcaObj$loadings[,1:3])
View(mydata)

clus_data<-mydata[,8:10]
norm_clus<-scale(clus_data) 
dist1<-dist(norm_clus,method = "euclidean")

fit1<-hclust(dist1,method = "complete")

plot(fit1) 

groups<-cutree(fit1,5) 
membership_1<-as.matrix(groups) 

View(membership_1)
final1<-cbind(membership_1,mydata) 
View(final1)
View(aggregate(final1[,-c(2,9:11)],by=list(membership_1),FUN=mean)


write.csv(final1,file="universities_clustered.csv",row.names = F,col.names = F)
getwd()
