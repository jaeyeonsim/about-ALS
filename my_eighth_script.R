# extract patients' data which are recorded more than or equal to 5 times
alsfrs_abc <- filter(mutate(group_by(ALSFRS_df, subject_id), "n"=n()), n>=5)
Cluster <- select(ALSFRS_df_firsttime2, subject_id, Cluster1)
CLUSTERASSIGN <- merge(alsfrs_abc, Cluster, by="subject_id")
# make 2 column which adds some questions' score(based on principal component)
CLUSTER_1<- mutate(CLUSTERASSIGN[CLUSTERASSIGN[,"Cluster1"]==1,], "alsfrsPC1"=Q4+Q5+Q6+Q7+Q8+Q9, "alsfrsPC2"=Q1+Q2+Q3+Q10)
CLUSTER_2<- mutate(CLUSTERASSIGN[CLUSTERASSIGN[,"Cluster1"]==2,], "alsfrsPC1"=Q4+Q5+Q6+Q7+Q8+Q9, "alsfrsPC2"=Q1+Q2+Q3+Q10)
CLUSTER_3<- mutate(CLUSTERASSIGN[CLUSTERASSIGN[,"Cluster1"]==3,], "alsfrsPC1"=Q4+Q5+Q6+Q7+Q8+Q9, "alsfrsPC2"=Q1+Q2+Q3+Q10)

CLUSTER_1 <- CLUSTER_1[order(CLUSTER_1$subject_id,CLUSTER_1$Delta),]
CLUSTER_2 <- CLUSTER_2[order(CLUSTER_2$subject_id,CLUSTER_2$Delta),]
CLUSTER_3 <- CLUSTER_3[order(CLUSTER_3$subject_id,CLUSTER_3$Delta),]
rownames(CLUSTER_1) <- c(1:8366)
rownames(CLUSTER_2) <- c(1:11362)
rownames(CLUSTER_3) <- c(1:24024)
CLUSTER_1 <- mutate(CLUSTER_1, "number" = as.numeric(factor(CLUSTER_1$subject_id)))
CLUSTER_2 <- mutate(CLUSTER_2, "number" = as.numeric(factor(CLUSTER_2$subject_id)))
CLUSTER_3 <- mutate(CLUSTER_3, "number" = as.numeric(factor(CLUSTER_3$subject_id)))
# plotting each patients' alsfrs score
plot(CLUSTER_1[which(CLUSTER_1$number==4),]$alsfrsPC1, CLUSTER_1[which(CLUSTER_1$number==4),]$alsfrsPC2, type="o", xlim= c(0,24), ylim=c(0,16), xlab="Q-Limb", ylab="Q-Bulbar")
plot(CLUSTER_2[which(CLUSTER_2$number==3),]$alsfrsPC1, CLUSTER_2[which(CLUSTER_2$number==3),]$alsfrsPC2, type="o", xlim= c(0,24), ylim=c(0,16), xlab="Q-Limb", ylab="Q-Bulbar")
plot(CLUSTER_3[which(CLUSTER_3$number==6),]$alsfrsPC1, CLUSTER_3[which(CLUSTER_3$number==6),]$alsfrsPC2, type="o", xlim= c(0,24), ylim=c(0,16),xlab="Q-Limb", ylab="Q-Bulbar")
# extract information(delta=min and delta=max) for each patients to calculate difference between them
cluster_1 <- CLUSTER_1 %>% group_by(subject_id) %>% slice(c(1, n()))
cluster_2 <- CLUSTER_2 %>% group_by(subject_id) %>% slice(c(1, n()))
cluster_3 <- CLUSTER_3 %>% group_by(subject_id) %>% slice(c(1, n()))
x <- cluster_1 %>% group_by(subject_id) %>% mutate(diff1=first(alsfrsPC1)-last(alsfrsPC1), diff2=first(alsfrsPC2)-last(alsfrsPC2))
y <- cluster_2 %>% group_by(subject_id) %>% mutate(diff1=first(alsfrsPC1)-last(alsfrsPC1), diff2=first(alsfrsPC2)-last(alsfrsPC2))
z <- cluster_3 %>% group_by(subject_id) %>% mutate(diff1=first(alsfrsPC1)-last(alsfrsPC1), diff2=first(alsfrsPC2)-last(alsfrsPC2))

plot(jitter(x$diff1), jitter(x$diff2), type="p", main="cluster1", xlab="Q-Bulbar(first-last)", ylab="Q-Limb(first-last)")
plot(jitter(y$diff1), jitter(y$diff2), xlab="difference(first-last of alsfrsPC1)", ylab="difference(first-last of alsfrsPC2)", type="p", main="cluster2")
plot(jitter(z$diff1), jitter(z$diff2), xlab="difference(first-last of alsfrsPC1)", ylab="difference(first-last of alsfrsPC2)", type="p", main="cluster3")
#혹시나 해서 clustering해봤는데 이건 아닌듯..
km.Alsfrs <- kmeans(x[,c("diff1", "diff2")], centers=4, nstart=30)
plot(x[,c("diff1", "diff2")], col=km.Alsfrs$cluster)