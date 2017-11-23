# ALSFRS_total 구하기
for(i in 1:length(ALSFRS_df$subject_id)){
  ALSFRS_df$ALSFRS_total[i] = sum(ALSFRS_df$Q1[i],ALSFRS_df$Q2[i],ALSFRS_df$Q3[i],ALSFRS_df$Q4[i],ALSFRS_df$Q5[i],ALSFRS_df$Q6[i],ALSFRS_df$Q7[i],ALSFRS_df$Q8[i],ALSFRS_df$Q9[i],ALSFRS_df$Q10[i])
}
head(ALSFRS_df)
# slope구하기
for(i in 1:length(ALSFRS_df_subject_id)){
  ALSFRS_df$
}