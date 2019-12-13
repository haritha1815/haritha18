install.packages(plyr)
library(plyr)
airlines <- read.csv(file.choose())

x <-  runif(50) # generating 50 random numbers

y <-  runif(50) # generating 50 random numbers 

airlines <- cbind(x,y) 

plot(airlines)

plot(airlines, type="n")
text(airlines, rownames(airlines))

km <- kmeans(airlines,4) #kmeans clustering
str(km)

install.packages("animation")
library(animation)

km <- kmeans.ani(airlines, 4)
km$centers

max_centers=12
# selecting K for kmeans clustering using kselection
install.packages("kselection")
library(kselection)
airlines()

View(airlines)
k <- kselection(iris[,-5], parallel = TRUE, k_threshold = 0.9, )

install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
k <- kselection(iris[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)

normalized_data <- scale(mydata[,2:7])
fit <- kmeans(normalized_data, 4) 
str(fit)
final2<- data.frame(mydata, fit$cluster) 
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(mydata[,2:7], by=list(fit$cluster), FUN=mean)

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:8) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")


install.packages("cluster")
library(cluster)
xds <- rbind(cbind(rnorm(5000, 0, 8), rnorm(5000, 0, 8)), cbind(rnorm(5000, 50, 8), rnorm(5000, 50, 8)))
xcl <- clara(xds, 2, sample = 100)
clusplot(xcl)

#Partitioning around medoids
xpm <- pam(xds, 2)
clusplot(xpm)

normalized_data <- scale(mydata[,2:7]) 
d <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")

plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=3) # cut tree into 3 clusters

rect.hclust(fit, k=3, border="red")

membership<-as.matrix(groups)

final <- data.frame(mydata, membership)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]


write.csv(final1, file="final.csv")

getwd()
