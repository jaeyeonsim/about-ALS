# choose ALSFRS score when Delta=0
ALSFRS_df_firsttime <- ALSFRS_df[ALSFRS_df$Delta==0,c(1:12)]
head(ALSFRS_df_firsttime, n=10)
# PCA
pr.ALSFRS <- prcomp(ALSFRS_df_firsttime[,c(2:11)], scale = TRUE, center = TRUE)
summary(pr.ALSFRS)
biplot(pr.ALSFRS)
# variance explained
pr.var <- pr.ALSFRS$sdev^2
pve <- pr.var/sum(pr.var)
plot(pve, xlab="number of PC", ylab="variance explained by PC", ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="number of PC", ylab="variance explained by PC", ylim=c(0,1), type="b")
# kmeans clustering
km.ALSFRS <- kmeans(pr.ALSFRS$x[,c(1,2)], centers= 3, nstart=30)
plot(pr.ALSFRS$x[,c(1,2)], col=km.ALSFRS$cluster, xlab="PC1", ylab="PC2")
# select number of kmeans clustering : maybe i=3 would be ok..?
wss<-0
for(i in 1:15){
km.alsfrs <- kmeans(pr.ALSFRS$x[,c(1,2)], centers=i, nstart=30)  
wss[i] <- km.alsfrs$tot.withinss
}
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="within group sum of squares")
# hierchical clustering
ALSFRS.hclust <- hclust(d=dist(pr.ALSFRS$x[,1:2], method = "euclidean"), method = "complete")
ALSFRS.hclust.clusters <- cutree(ALSFRS.hclust, k=3)
plot(ALSFRS.hclust)
# compare k-means to hierchical clustering
table(km.ALSFRS$cluster, ALSFRS.hclust.clusters)
# compare k-means to hierchical clustering in graph
plot(pr.ALSFRS$x[,c(1,2)], col=ALSFRS.hclust.clusters, xlab="", ylab="")