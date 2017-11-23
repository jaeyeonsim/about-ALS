# Death와 Cluster1간의 비교(일단 deathdata 합치고나서 시행)
ALSFRS_df_firsttime3<- merge(x=ALSFRS_df_firsttime2, y=DeathData,by="subject_id", all.x=TRUE)
levels(ALSFRS_df_firsttime3$Subject_Died) <- c("No","Yes","censoring")
for(i in 1:length(ALSFRS_df_firsttime3$subject_id)){
  if(is.na(ALSFRS_df_firsttime3$Subject_Died[i]))
    ALSFRS_df_firsttime3$Subject_Died[i] = "censoring"
}
head(ALSFRS_df_firsttime3)
table(ALSFRS_df_firsttime3$Cluster1, ALSFRS_df_firsttime3$Subject_Died)
mosaicplot(Subject_Died~Cluster1, data=ALSFRS_df_firsttime3)
chisq.test(ALSFRS_df_firsttime3$Cluster1, ALSFRS_df_firsttime3$Subject_Died, correct=FALSE)
## 각 cluster별로 mean구하고싶은데, 그냥 직접 계산함.(0.13, 0.33, 0.15)
# Onsettime~Cluster1간의 비교-Median
hist(ALSFRS_df_firsttime3[ALSFRS_df_firsttime3$Cluster1==1,]$Onset_Delta, breaks=20, right=FALSE)
hist(ALSFRS_df_firsttime3[ALSFRS_df_firsttime3$Cluster1==2,]$Onset_Delta, breaks=20, right=FALSE)
hist(ALSFRS_df_firsttime3[ALSFRS_df_firsttime3$Cluster1==3,]$Onset_Delta, breaks=20, right=FALSE)
GROUP_A <- group_by(ALSFRS_df_firsttime3, Cluster1)
SUMMARISE_A <- summarise(GROUP_A, Median=abs(median(Onset_Delta, na.rm=TRUE)))
SUMMARISE_A
#anova
sapply(ALSFRS_df_firsttime3, class)
aov(Onsetsite~Cluster1, data=ALSFRS_df_firsttime3)
#Mean
GROUP_B <- group_by(ALSFRS_df_firsttime3, Cluster1)
SUMMARISE_B <- summarise(GROUP_B, Mean=abs(mean(Onset_Delta, na.rm=TRUE)))
SUMMARISE_B
#anova 못하겠음..