# delta = 0(0~30) 일때와 delta = 360(330~390)일때 ALSFRS Q_bulbar와 Q_limb의 변화량을 보려고 함.

# 1. delta가 0과 360일때가 모두 존재하는 환자들을 추출.(delta가 0, 360에 제일 가까운 값을 각 환자당 한개씩 뽑음)

library(dplyr)

ALSFRS_zeromonth_pre <- ALSFRS_df %>% filter(0<=Delta, Delta<=30)
ALSFRS_zeromonth <- ALSFRS_zeromonth_pre %>% group_by(subject_id) %>% filter(Delta==min(Delta-0))

colnames(ALSFRS_zeromonth)[12] <- "Delta_zero"



ALSFRS_twelvemonth_pre <- ALSFRS_df %>% filter(330<=Delta, Delta<=390)
ALSFRS_twelvemonth_pre$Delta <- ALSFRS_twelvemonth_pre$Delta-360
ALSFRS_twelvemonth <- ALSFRS_twelvemonth_pre %>% group_by(subject_id) %>% filter(Delta==min(abs(Delta-0)))
ALSFRS_twelvemonth$Delta <- ALSFRS_twelvemonth$Delta + 360

colnames(ALSFRS_twelvemonth)[12] <- "Delta_twelve"


# 2. Q_Bulbar(Q1,2,3,10을 합한 값), Q_Limb(Q4,5,6,7,8,9을 합한 값) 점수를 계산하고, 한 데이터프레임에 합침. 

ALSFRS_zeromonth["Q_Bulbar_zero"] <- NA
ALSFRS_zeromonth["Q_Limb_zero"]<- NA

ALSFRS_zeromonth$Q_Bulbar_zero <- ALSFRS_zeromonth$Q1 + ALSFRS_zeromonth$Q2 + ALSFRS_zeromonth$Q3 + ALSFRS_zeromonth$Q10
ALSFRS_zeromonth$Q_Limb_zero <- ALSFRS_zeromonth$Q4 + ALSFRS_zeromonth$Q5 + ALSFRS_zeromonth$Q6 + ALSFRS_zeromonth$Q7 + ALSFRS_zeromonth$Q8 + ALSFRS_zeromonth$Q9

ALSFRS_twelvemonth["Q_Bulbar_twelve"] <- NA
ALSFRS_twelvemonth["Q_Limb_twelve"] <- NA

ALSFRS_twelvemonth$Q_Bulbar_twelve <- ALSFRS_twelvemonth$Q1 + ALSFRS_twelvemonth$Q2 + ALSFRS_twelvemonth$Q3 + ALSFRS_twelvemonth$Q10
ALSFRS_twelvemonth$Q_Limb_twelve <- ALSFRS_twelvemonth$Q4 + ALSFRS_twelvemonth$Q5 + ALSFRS_twelvemonth$Q6 + ALSFRS_twelvemonth$Q7 + ALSFRS_twelvemonth$Q8 + ALSFRS_twelvemonth$Q9


ALSFRS_zerotwelvemonth <- merge(x=ALSFRS_zeromonth, y=ALSFRS_twelvemonth[,c(1,12,14,15)], by='subject_id')


# 3. ALSFRS_zerotwelvemonth에다 다른 변수들을 추가.

ALSFRS_zerotwelvemonth <- merge(x=ALSFRS_zerotwelvemonth, y=alshistory_sub, by='subject_id')
ALSFRS_zerotwelvemonth <- merge(x=ALSFRS_zerotwelvemonth, y=DeathData, by='subject_id', all.x=TRUE)
ALSFRS_zerotwelvemonth <- merge(x=ALSFRS_zerotwelvemonth, y=DEMOGRAPHICS, by='subject_id')


# 4. ALSFRS_zerotwelvemonth에 있는 환자들마다 time과 event 구하기. time = death / censoring되기까지의 시간. event = 1은 death을 의미, event=0은 censoring을 의미

ALSFRS_zerotwelvemonth["time"] <- NA

for (i in 1:length(ALSFRS_zerotwelvemonth$subject_id))
     {
       if(!is.na(ALSFRS_zerotwelvemonth$Death_Days[i]))
         ALSFRS_zerotwelvemonth$time[i] <- ALSFRS_zerotwelvemonth$Death_Days[i]
        else
         ALSFRS_zerotwelvemonth$time[i] <- ALSFRS_zerotwelvemonth$maxdelta[i]
}


ALSFRS_zerotwelvemonth["event"] <- NA

for( i in 1:length(ALSFRS_zerotwelvemonth$subject_id))
{
  if(!is.na(ALSFRS_zerotwelvemonth$Death_Days[i]))
    ALSFRS_zerotwelvemonth$event[i] <- 1
  else
    ALSFRS_zerotwelvemonth$event[i] <- 0
}


# 5. Onsetsite = Limb인 환자에서 상대적인 진행속도(Bulbar versus Limb) 구하기

ALSFRS_zerotwelvemonth_limb <- filter(ALSFRS_zerotwelvemonth, Onsetsite=="Limb")

ALSFRS_zerotwelvemonth_limb$n <- 3/2 * (ALSFRS_zerotwelvemonth_limb$Q_Bulbar_zero-ALSFRS_zerotwelvemonth_limb$Q_Bulbar_twelve)/(ALSFRS_zerotwelvemonth_limb$Q_Limb_twelve-ALSFRS_zerotwelvemonth_limb$Q_Limb_zero) + 1

# 5-1. n = missing value의 처리(Q_bulbar와 Q_limb의 점수가 모두 변화하지 않은 사람들)

ALSFRS_zerotwelvemonth_limb <- filter(ALSFRS_zerotwelvemonth_limb, !is.na(ALSFRS_zerotwelvemonth_limb$n))

# 5-2. n = Inf의 처리()

# 5-3. n의 분포 확인

plot(density(ALSFRS_zerotwelvemonth_limb$n), main = "density plot of n", xlab = "n", ylab = "density")

# 5-4. n(상대적 속도비)에 따라 아형 분류하기 - summary나 mean으로 분포 확인

summary(ALSFRS_zerotwelvemonth_limb$n)
mean(ALSFRS_zerotwelvemonth_limb$n<0.4)
mean(ALSFRS_zerotwelvemonth_limb$n>=1)

# 6. 각 그룹별로 생존율 비교하기

ALSFRS_zerotwelvemonth_limb["group"] <- NA

for(i in 1 : length(ALSFRS_zerotwelvemonth_limb$subject_id))
{
  if(ALSFRS_zerotwelvemonth_limb$n[i]<0.4)
    ALSFRS_zerotwelvemonth_limb$group[i] <- 1
  else if(ALSFRS_zerotwelvemonth_limb$n[i]>=1)
    ALSFRS_zerotwelvemonth_limb$group[i] <- 3
  else
    ALSFRS_zerotwelvemonth_limb$group[i] <- 2
    }

ALSFRS_zerotwelvemonth_limb %>% group_by(group) %>% summarise(time=mean(time), event=mean(event))

# 7. Onsetsite = Bulbar인 환자에서 상대적인 진행속도(Limb versus Bulbar) 구하기

ALSFRS_zerotwelvemonth_Bulbar <- filter(ALSFRS_zerotwelvemonth, Onsetsite=="Bulbar")

ALSFRS_zerotwelvemonth_Bulbar$n <- 2/3 * (ALSFRS_zerotwelvemonth_Bulbar$Q_Limb_zero-ALSFRS_zerotwelvemonth_Bulbar$Q_Limb_twelve)/(ALSFRS_zerotwelvemonth_Bulbar$Q_Bulbar_twelve-ALSFRS_zerotwelvemonth_Bulbar$Q_Bulbar_zero)  + 1
