#install survival packages
library("survival")
#sort the data by time(total 2856patients)
ALSFRS_df_firsttime3 <- ALSFRS_df_firsttime3[order(ALSFRS_df_firsttime3$time),]
#create graph of length of time that each subject was in the study
with(ALSFRS_df_firsttime3, plot(time, type="h"))
#descriptive statistics
summary(ALSFRS_df_firsttime3)
# create life table survival object for cluster
myfit1 <- survfit(Surv(time, event)~1, data=ALSFRS_df_firsttime3)
plot(myfit1, xlab="time", ylab="survival probability")
table(ALSFRS_df_firsttime3$Cluster1)
myfit2 <- survfit(Surv(time, event)~ALSFRS_df_firsttime3$Cluster1, data=ALSFRS_df_firsttime3)
plot(myfit2, col=c("green", "black", "red"), xlab="time", ylab="survival probability")
legend("topright", c("cluster1", "cluster2", "cluster3"), col=c("green", "black", "red"), lty=1)
#see difference between clusters are significant
survdiff(Surv(time, event)~ALSFRS_df_firsttime3$Cluster1, data=ALSFRS_df_firsttime3)
