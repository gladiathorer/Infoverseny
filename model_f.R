# Title     : Feedforward Neural Network
# Objective : Legyen kész
# Created by: szpet
# Created on: 4/30/2021
library(keras)

df <- read.csv('results_train.csv')
df_t <- read.csv(paste('results_', readline(prompt="Enter path for testing data: "), sep = ''))
df <- subset(df,select = -c(hormone_therapy_0,adjuvant_chemotherapy_0))
df_t <- subset(df_t,select = -c(hormone_therapy_0,adjuvant_chemotherapy_0))
original <- read.csv('data.csv')
train_x <- data.matrix(subset(df,select=-c(PFS_time_months,bcr_patient_uuid,X)))
train_y <- data.matrix(subset(df,select=c(PFS_time_months)))
patient_ids <- subset(df_t,select = bcr_patient_uuid)
patient_ids
test_x <- data.matrix(subset(df_t,select=-c(PFS_time_months,bcr_patient_uuid,X)))
test_y <- data.matrix(subset(df_t,select=c(PFS_time_months)))

model <- keras_model_sequential() %>%

  # network architecture
  layer_dense(units = 40, activation = "sigmoid", input_shape = ncol(train_x)) %>%
  layer_dense(units = 40, activation = "sigmoid") %>%
  layer_dense(units = 1) %>%

  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )

# train
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 20,
  batch_size = 5,
  validation_split = .2,
  verbose = FALSE
)

learn
#az eredeti kemoterápiás értékből(igen/nem) becsült DFS
output_before <- model %>% predict(test_x)*max(original['OS_time_months'])
for (i in seq(nrow(df_t))){
  if (test_x[,'adjuvant_chemotherapy_1'][i] == 0.99)
  {
    test_x[,'adjuvant_chemotherapy_1'][i] = 0.01
  }
  else
  {
    test_x[,'adjuvant_chemotherapy_1'][i] = 0.99
  }
}
#a megfordított kemoterápiás értékből(igen/nem) becsült DFS
output_after <- model %>% predict(test_x)*max(original['OS_time_months'])
dff <- data.frame(patient_ids,test_y*max(original['OS_time_months']),output_before,output_after)
#az 'output.csv'-be írjuk ki
write.csv(dff,'output.csv')
