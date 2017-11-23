# factor가 2개만 가질 수 있도록 바꿈(원래 3개였음)
levels(ALSFRS_df_firsttime3$sex)
ALSFRS_df_firsttime3$sex <- factor(ALSFRS_df_firsttime3$sex)

#cox proportional hazard ratio
coxph(Surv(time, event)~ALSFRS_df_firsttime3$Cluster1+ALSFRS_df_firsttime3$sex+ALSFRS_df_firsttime3$Onset_Delta+ALSFRS_df_firsttime3$age+ALSFRS_df_firsttime3$Onsetsite, data=ALSFRS_df_firsttime3)

coxph(Surv(time, event)~ALSFRS_df_firsttime3$Cluster1, data=ALSFRS_df_firsttime3)
ALSFRS_df_firsttime3$Cluster1<-C(ALSFRS_df_firsttime3$Cluster1, base=1)
