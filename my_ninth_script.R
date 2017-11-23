ALSFRS_df_firsttime2["time"] <- NA
ALSFRS_df_firsttime2["event"] <- NA
# create new column "maxdelta" in ALSFRS_df
ALSFRS_df <- ALSFRS_df %>% group_by(subject_id) %>% mutate(maxdelta = max(Delta))
ALSFRS_df_q <- ALSFRS_df %>% group_by(subject_id) %>% filter(Delta==0)
# merge maxdelta with ALSFRS_df_firsttime2
ALSFRS_df_firsttime2 <- merge(x=ALSFRS_df_firsttime2, y=ALSFRS_df_q[,c("subject_id", "maxdelta")], all.x=TRUE)
# merge deathdata with ALSFRS_df_firsttime2
ALSFRS_df_firsttime2 <- merge(x=ALSFRS_df_firsttime2, y=DeathData, all.x=TRUE)
# time, event
for(i in 1:length(ALSFRS_df_firsttime2$subject_id))
  {
  if(!is.na(ALSFRS_df_firsttime2$Death_Days[i])==TRUE)
    ALSFRS_df_firsttime2$time[i]<- ALSFRS_df_firsttime2$Death_Days[i]
else
  ALSFRS_df_firsttime2$time[i] <- ALSFRS_df_firsttime2$maxdelta[i]
}


for(i in 1:length(ALSFRS_df_firsttime2$subject_id))
{
  if(!is.na(ALSFRS_df_firsttime2$Subject_Died[i])==TRUE&ALSFRS_df_firsttime2$Subject_Died[i]=="Yes")
    ALSFRS_df_firsttime2$event[i] <- 1
  else{
    ALSFRS_df_firsttime2$event[i] <- 0
  }
}
#filter rows which have both event and time described
ALSFRS_df_firsttime3 <- ALSFRS_df_firsttime2 %>% filter(!is.na(ALSFRS_df_firsttime2$time)&!is.na(ALSFRS_df_firsttime2$event))