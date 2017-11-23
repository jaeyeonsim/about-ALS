# histogram - Delta
hist(ALSFRS_df$Delta, breaks=20, right = FALSE)
ALSFRS_df_new <- filter(ALSFRS_df, Delta<1000)
hist(ALSFRS_df_new$Delta, breaks= 20, right=FALSE)
#
ALSFRS_df_one <- subset(ALSFRS_df, subset=Delta<50)
ALSFRS_df_two <- subset(ALSFRS_df, subset=Delta>200 & Delta<300)
ALSFRS_df_three <- subset(ALSFRS_df, subset= Delta>400 & Delta <600)

ALSFRS_df_ONE <- aggregate(ALSFRS_df_one, by=list(ALSFRS_df_one$subject_id), FUN=min)
ALSFRS_df_TWO <- aggregate(ALSFRS_df_two, by=list(ALSFRS_df_two$subject_id), FUN=min)
ALSFRS_df_THREE <- aggregate(ALSFRS_df_three, by=list(ALSFRS_df_three$subject_id), FUN=max)

length(ALSFRS_df_ONE$subject_id)
length(ALSFRS_df_TWO$subject_id)
length(ALSFRS_df_THREE$subject_id)

