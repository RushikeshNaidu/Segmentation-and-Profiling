MyData <- read.csv(file="C:/Users/Admin/Desktop/MIS584/Wholesale customers data-1.csv", header=TRUE, sep=",")
head(MyData)
summary(MyData)
cleandata <- MyData
cleandata <-  na.omit(cleandata)
cleandata$Channel <- NULL
cleandata$Region <- NULL
cleandata <- scale(cleandata)
within_sum_of_squares <- (nrow(cleandata)-1)*sum(apply(cleandata,2,var))
for (i in 2:15) within_sum_of_squares[i] <- sum(kmeans(cleandata, 
                                     centers=i)$withinss)
plot(1:15,within_sum_of_squares, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#This does not give a strong elbow, but the graph shows that there is no significant drop after 6 clusters.. 
fit <- kmeans(cleandata, 6) # fit the model
aggregate(cleandata,by=list(fit$cluster),FUN=mean) # getting cluster means
cleandata <- data.frame(cleandata, fit$cluster) #appending cluster 
library(cluster)
clusplot(cleandata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
sampled <- sample(1:dim(cleandata)[1], 50)
Sample_final <- cleandata[sampled,]
Sample_final$Region <- NULL
Sample_final$Channel <- NULL
d <- dist(Sample_final, method = "euclidean") 
fit <- hclust(d, method="ward") 
plot(fit) 
groups <- cutree(fit, k=6) # cut tree into 6 clusters
rect.hclust(fit, k=6, border="red") # draw dendogram with red borders around the 6 clusters
