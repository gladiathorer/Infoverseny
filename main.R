# Title     : Adatok előkészítése
# Objective : Adatok előkészítése a neurális hálóhoz
# Created by: Szigeti Péter
# Created on: 4/29/2021
library(survival)
library(tidyr)



df <- read.csv('data.csv')
train1 <- subset(df,select = c(bcr_patient_uuid,PFS_time_months,OS_time_months,age_at_diagnosis,size_documented,lymph_node_positive_count_documented))
train1 <- na.omit(train1,na.action='replace',fill = 0)
train <- (subset(train1,select = -bcr_patient_uuid))/max(subset(train1,select = -bcr_patient_uuid))
train['bcr_patient_uuid'] = train1['bcr_patient_uuid']

df <- subset(df,select = -c(bcr_patient_uuid,PFS_time_months,OS_time_months,age_at_diagnosis,size_documented,lymph_node_positive_count_documented))
#plot(survfit(Surv(PFS_time_months,PFS_event) ~ 1, data = df))
for (col in colnames(df))
{
  for (el in unique(df[col]))
  {
    train[paste(col,el,sep='_')] <- 0.01
  }
}
for (i in seq(nrow(df)))
{
  for (col in colnames(df))
  {
    train[i,paste(col,df[i,col],sep = '_')] <- 0.99
  }

}
train <- na.omit(train,na.action='omit')

write.csv(train,"results.csv")

print('Done!')