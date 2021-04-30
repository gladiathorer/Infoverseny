# Title     : Adatok előkészítése
# Objective : Adatok előkészítése a neurális hálóhoz
# Created by: Szigeti Péter
# Created on: 4/29/2021

main <- function(x){
  #beolvasás
  df <- read.csv('data.csv')
  #a folytonos adatokból egy neuront készítek ezért külön veszem őket
  train1 <- subset(df,select = c(bcr_patient_uuid,PFS_time_months,OS_time_months,age_at_diagnosis,size_documented,lymph_node_positive_count_documented))
  train1 <- na.omit(train1,na.action='replace',fill = 0)
  #maximum normalizálás
  train <- (subset(train1,select = -bcr_patient_uuid))/max(subset(train1,select = -bcr_patient_uuid))
  #megjelöljük a beteget
  train['bcr_patient_uuid'] <- train1['bcr_patient_uuid']

  df <- subset(df,select = -c(bcr_patient_uuid,PFS_time_months,OS_time_months,age_at_diagnosis,size_documented,lymph_node_positive_count_documented))
  #a diszkrét értékeket külön-külön neuronokba fogom rakni egy igen vagy nem értékkel(0.99 vagy 0.01)
  #pl a 'pathologic_T_T4' és 'pathologic_T_T4b' két külön neuronba kerül
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
  #azokat a sorokat amikbe valamiért maradt nan érték, kitörlöm, mivel zavarja a neurális hálót
  train <- na.omit(train,na.action='omit')

  write.csv(train,paste('results_', x, sep = ''))

  print('Done!')
}
#training dataset
main('train.csv')
main(readline(prompt="Enter path for testing data: "))