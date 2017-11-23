ALSFRS <- read.csv("C:/Temp/Alsfrs.CSV")
# combine Q5a, Q5b -> Q5
ALSFRS_df_1 <- ALSFRS[,c(6,7)]
ALSFRS_df_1$Q5 <- ifelse(is.na(ALSFRS_df_1[[1]]), ALSFRS_df_1[[2]], ALSFRS_df_1[[1]])
head(ALSFRS_df_1, n=100)
# check if NA still exist
sum(is.na(ALSFRS_df_1$Q5))
# combine Q10, Dyspnea
ALSFRS_df_2 <- ALSFRS[,c(12,16)]
ALSFRS_df_2$Q10 <- ifelse(is.na(ALSFRS_df_2[[1]]), ALSFRS_df_2[[2]], ALSFRS_df_2[[1]]) 
head(ALSFRS_df_2, n=100)
# check if NA still exist
sum(is.na(ALSFRS_df_2$Q10))
# combine ALSFRS_Total ALSFRS_R_total

# creat new dataframe (variable : subject_id, Q1~Q10, Delta, Total score)
ALSFRS_df_notcompleted <- data.frame(ALSFRS[,c(1:5)], ALSFRS_df_1[,c(3)], ALSFRS[,c(8:11)], ALSFRS_df_2[,c(3)], ALSFRS[,c(13)])
colnames(ALSFRS_df_notcompleted) <- c("subject_id", "Q1","Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Delta")
head(ALSFRS_df_notcompleted, n=10)
# erase row which contains NA
ALSFRS_df <- ALSFRS_df_notcompleted[complete.cases(ALSFRS_df_notcompleted),]
# maxdelta column 새로 만들어주기
ALSFRS_df <- ALSFRS_df %>% group_by(subject_id) %>% mutate(maxdelta = max(Delta))


sum(is.na(ALSFRS_df))
length(levels(factor(ALSFRS$subject_id)))
