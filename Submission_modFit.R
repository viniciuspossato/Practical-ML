# QUIZ - WEEK 4 - PRACTICAL MACHINE LEARNING

library(caret); library(tidyverse); library(lubridate)

# LOADING THE DATASETS

training <- read.csv2("pml-training.csv",
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = FALSE)


testing <- read.csv2("pml-testing.csv",
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = FALSE)

# ANALYZING

check_NA <- training %>%
  summarise_all(
    .funs = list(
      ~sum(is.na(.))
    )) %>% mutate_all(
      .funs = ~ifelse(. >19200,"BAD","GOOD") # Looking for NA
    ) 
 
training <-
  training[,which(check_NA=="GOOD")] # 93 useful covariates

check_NULL <- training %>%
  summarise_all(
    .funs = list(
      ~sum(.=="")
    )
  ) %>%
  mutate_all(
    .funs = list(
      ~ifelse(. > 19000,"BAD","GOOD")
    )
  ) # Looking for NULL values

training <-
  training[,which(check_NULL=="GOOD")] # 60 useful covariates

# EXPLORATORY ANALYSIS

training <- training[,-1] # The first column is not necessary!

training$classe <-
  as.factor(training$classe) # Categorical variable (pos. 59)

training$user_name <-
  as.factor(training$user_name) # Categorical Variable (pos. 1)

training$cvtd_timestamp <-
  as.POSIXct(training$cvtd_timestamp,
             format = "%d/%m/%Y %H:%M") # DATE-TIME Variable (pos. 4)

training <- training %>%
  mutate_at(
    c(6:58),.funs = as.numeric
  ) # Changing the character variables into numeric

nearZeroVar(training[-c(1,2,3,4,5,59)],saveMetrics = TRUE) %>% 
  View() # Verifying the number of unique values

M <- corr_matrix <-
  cor(training[,-c(1:5,59)]) # Matriz de correlação

diag(M) <- 0 # Giving zero where the correlation was equal to 1

vars_highCorr <-
  colnames(M)[which(abs(M) > 0.85,arr.ind = T)[,2] %>% 
                unique()] # 12 covariates with more than 85% of correlation

# Selecting the most useful covariates

training <- 
  training[,setdiff(names(training),vars_highCorr)]

# Testing dataset

testing <- 
  testing[,intersect(names(testing), names(training))]

testing$user_name <-
  as.factor(testing$user_name) 

testing$cvtd_timestamp <-
  as.POSIXct(testing$cvtd_timestamp,
             format = "%d/%m/%Y %H:%M") 

testing <- testing %>%
  mutate_at(
    c(6:46),.funs = as.numeric
  )

# Fitting a MODEL

modFit_sprf <-
  train(classe ~.,
        data = training,
        method = "rf",
        preProcess = c("center","scale"),
        trControl = trainControl(method = "cv", number = 10)
        )

pred <- predict(
  modFit_sprf,
  testing
  #type = "prob"
)

# SALVANDO O MODELO AO FINAL DO PROCESSO

saveRDS(modFit_sprf,file = "modFit_sprf.rds")

# CARREGANDO O MODELO SALVO

testando <-
  readRDS("modFit_sprf.rds")


