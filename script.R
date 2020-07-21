#############################

if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(data.table)) install.packages("data.table")
if(!require(klaR)) install.packages("klaR")

#############################
#DATA CLEANING
#############################

# Here we take every brazilian states dataset and take the variables we want:
# (id, notification date, first symptome date, comorbitites, sex, state, age, evolution)
# We also filter for the cases that had a definitive ending, cure or death

#list with all the file names, one file for each brazilian state
states <- c('ac', 'al', 'am', 'ap', 'ba', 'ce', 'df', 'es', 'go', 'ma', 'mg', 'ms', 'mt', 'pi',
            'pr', 'rj', 'rn', 'ro', 'rr', 'rs', 'sc', 'se', 'sp', 'to')#states abreviation, as on the file names

files <- str_c('data\\dados-',states,'.csv')#concatenate the states with the text that makes the filenames

clean_dataset <- data.frame('id' = '1', 
                            'dataNotificacao' = '2020-03-24T03:00:00.000Z', 
                            'dataInicioSintomas' = '2020-03-24T03:00:00.000Z', 
                            'condicoes'='null', 
                            'sexo'='Feminino',
                            'estado'='TOCANTINS', 
                            'idade'=21, 
                            'evolucaoCaso'='Cura')
#open a clean dataset so we can fill it with the files
#randon data, will remove id == '1', it's just to establish the variable names

#for loop that import, clean and add the clean file into the dataset
for (file in files) {
  #import
  df <- read.csv2(file, stringsAsFactors = FALSE)
  
  #clean
  df <- df %>% 
    filter(evolucaoCaso == 'Cura' | evolucaoCaso == 'Óbito') %>%
    mutate(id = ÿid,
           idade = parse_number(idade)) %>%
    select(id, dataNotificacao, dataInicioSintomas, condicoes, sexo, estado, idade, evolucaoCaso)
  
  #add to the dataset
  clean_dataset <- bind_rows(clean_dataset, df)
}

#this my take a while to run, I will put the clean dataset as a .csv on GitHub, make things easier
#the raw files can be found on https://opendatasus.saude.gov.br/dataset/casos-nacionais in case you want to
#due to size and number of files i won't be adding then on GitHub

rm(df, file, files, states) #removing dataframes I won't use
clean_dataset <- filter(clean_dataset, id != '1') #removing the randon case I put in

#translating the variable names
names(clean_dataset) <- c('id', 'notification_date', 'first_symptom_date', 'conditions', 'sex', 'state', 'age', 'case_evolution')

#detecting the more significant conditions, making it in a tidyer format 
clean_dataset <- clean_dataset %>%
  mutate(diabetes = if_else(str_detect(clean_dataset$conditions, 'Diabetes'), 'yes', 'no'),
         cardiac = if_else(str_detect(clean_dataset$conditions, pattern = c('cardíacas', 'Cardoacas')), 'yes', 'no'),
         respiratory = if_else(str_detect(clean_dataset$conditions, 'respiratórias'), 'yes', 'no'),
         renal = if_else(str_detect(clean_dataset$conditions, 'renais'), 'yes', 'no'),
         immunosuppression = if_else(str_detect(clean_dataset$conditions, 'Imunossupressão'), 'yes', 'no'),
         pregnant = if_else(str_detect(clean_dataset$conditions, 'Gestante'), 'yes', 'no'),
         chromosomal_abnormality = if_else(str_detect(clean_dataset$conditions, 'cromossômicas'), 'yes', 'no'))

#a general comorbidities collum if any of the ones above is true, so will be this one
clean_dataset <- clean_dataset %>%
  mutate(comorbidities = if_else(diabetes == 'yes'|cardiac == 'yes'|respiratory == 'yes'|renal == 'yes'|
                                immunosuppression == 'yes'|pregnant == 'yes'|chromosomal_abnormality =='yes', 'yes', 'no'))
#remove the conditions collum since it was replaced for a tidyer format
clean_dataset$conditions <- NULL

#We can note that there is a couple weird ages, so I added a new filter to take only age <= 110
clean_dataset <- filter(clean_dataset, age <= 110)

#translating a couple variables to english
clean_dataset$sex <- str_replace_all(clean_dataset$sex, 'Feminino', 'female')
clean_dataset$sex <- str_replace_all(clean_dataset$sex, 'Masculino', 'male')
clean_dataset$sex <- str_replace_all(clean_dataset$sex, 'Indefinido', 'null')

clean_dataset$case_evolution <- str_replace_all(clean_dataset$case_evolution, 'Cura', 'cure')
clean_dataset$case_evolution <- str_replace_all(clean_dataset$case_evolution, 'Óbito', 'decease')

write.csv(clean_dataset, file = 'data\\clean_dataset.csv')#export the clean dataset so I can push it into GitHub

#############################
#DOWNLOAD, UNZIP AND LOAD clean_dataset

url <- "https://raw.githubusercontent.com/rodrigues-pedro/COVID-19/master/data/clean_dataset.zip"
temp <- tempfile()
download.file(url, temp, mode="wb")
unzip(temp, "clean_dataset.csv")
clean_dataset <- read.csv("clean_dataset.csv")
unlink(temp)

#change level order on the case_evolution since we're more concerned with the decease cases.
clean_dataset$case_evolution <- factor(clean_dataset$case_evolution, levels = c('decease', 'cure'))

#removing the columm added when writing the csv
clean_dataset$X <- NULL

#############################
#DATA EXPLORATION
#############################

##BASE STATS
sum(clean_dataset$case_evolution == 'decease') #Total of deaths in the dataset
mean(clean_dataset$case_evolution == 'cure') #Percentage of cures in the dataset
# Class is totally imbalanced

##AGE
clean_dataset %>% ggplot(aes(case_evolution, age, fill = case_evolution)) +
  geom_boxplot(alpha = 0.2)
#now we note that the median age amongst the deceased cases is quite higher then the cured ones

##SEX
clean_dataset %>% ggplot(aes(case_evolution, fill = sex)) +
  geom_bar(stat = 'count', position = 'fill') +
  ylab('percentage')
#slight prevalence of male other than females in the deceased group, not sure if statistically significant
#deeper analysis later

##STATE
clean_dataset %>% group_by(state) %>% summarise(deceased = mean(case_evolution == 'decease'),
                                                cured = mean(case_evolution == 'cure')) %>% view()
#little to no variation in between states, don't think is influential

##COMORBIDITIES
clean_dataset %>% ggplot(aes(case_evolution, fill = comorbidities)) +
  geom_bar(stat = 'count', position = 'fill') +
  ylab('percentage')
#here we note that the presence of comorbidities in general is much higher amongst the deceased cases

clean_dataset %>% group_by(case_evolution) %>%
  summarise(comorbidities = mean(comorbidities == 'yes'),
            diabetes = mean(diabetes == 'yes'),
            cardiac = mean(cardiac == 'yes'),
            respiratory = mean(respiratory == 'yes'),
            renal = mean(renal == 'yes'),
            immunosuppression = mean(immunosuppression == 'yes'),
            chromosomal_abnormality = mean(chromosomal_abnormality == 'yes'),
            pregnant = mean(pregnant == 'yes'))
#a little summary on the presence of the analysed comorbidities

#############################
#MACHINE LEARNING
#############################

##MODEL 1

# validation set will be 10% of clean_dataset data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
validation_index <- createDataPartition(y = clean_dataset$case_evolution, times = 1, p = 0.1, list = FALSE)
remaining <- clean_dataset[-validation_index,]
validation <- clean_dataset[validation_index,]

# test set will be 10% of remaining data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use set.seed(1)
test_index <- createDataPartition(y = remaining$case_evolution, times = 1, p = 0.1, list = FALSE)
train <- clean_dataset[-test_index,]
test <- clean_dataset[test_index,]

#Since the goal is to predict the most vunerable cases, we are focusing on specifity for this algorithm
#which is a potencial problem, since the number of deceased cases is very low compared to the cured ones

#Here we fit a model to predict the probability of death, by age, as a linear model
fit <- train %>%
  mutate(y = ifelse(case_evolution == 'decease', 1, 0)) %>%
  lm(y ~ age, data = .)
#27.8 Case study: is it a 2 or a 7?#
p_hat <- predict(fit, newdata = test)
#conditional probabilities acording to age only

#now we calculate conditional probabilities acording to the other variables
#fatalitie probabilitie considering that there is one of the comorbidities
p_diabetes <- train %>% filter(diabetes == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_cardiac <- train %>% filter(cardiac == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_respiratory <- train %>% filter(respiratory == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_renal <- train %>% filter(renal == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_immuno <- train %>% filter(immunosuppression == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_pregnant <- train %>% filter(pregnant == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_chrom <- train %>% filter(chromosomal_abnormality == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)

#now we write a model that takes in consideration every one of this calculations:
p <- seq(0,0,length.out = 35735)

for (i in 1:length(p_hat)){
  if(test$diabetes[i] == 'yes'){p[i] = p[i] + p_diabetes} #if there is diabetes, we add the p_diabetes to the general probability 
  if(test$cardiac[i] == 'yes'){p[i] = p[i] + p_cardiac} #and so on for the other comobidities
  if(test$respiratory[i] == 'yes'){p[i] = p[i] + p_respiratory}
  if(test$renal[i] == 'yes'){p[i] = p[i] + p_renal}
  if(test$immunosuppression[i] == 'yes'){p[i] = p[i] + p_immuno}
  if(test$pregnant[i] == 'yes'){p[i] = p[i] + p_pregnant}
  if(test$chromosomal_abnormality[i] == 'yes'){p[i] = p[i] + p_chrom}
  p[i] = p[i] + p_hat[i]
}

#now, having the general probabilities we can tune for the optimal cuttof value using the balanced accuracy as measure
#using balanced accuracy because sensitivity is very important, and due to the unbalenced of the dataset, it's generally quite low with high accuracy
ps <- seq(0,1,0.01)

accuracy <- sapply(ps, function(ps){
  y_hat <- factor(ifelse(p > ps, 'decease', 'cure'))
  b_a <- confusionMatrix(y_hat, test$case_evolution)$byClass[['Balanced Accuracy']]
  b_a  
  })

best_p <- ps[which.max(accuracy)]

##MODEL 1 - FINAL
#now we can retrain everything using the remaining dataset but using the tuned values to check on the validation set

#Here we fit a model to predict the probability of death, by age, as a linear model
fit_fin <- remaining %>%
  mutate(y = ifelse(case_evolution == 'decease', 1, 0)) %>%
  lm(y ~ age, data = .)

p_hat_fin <- predict(fit_fin, newdata = validation)
#conditional probabilities acording to age only

#now we calculate conditional probabilities acording to the other variables
#fatalitie probabilitie considering that there is one of the comorbidities
p_diabetes_fin <- remaining %>% filter(diabetes == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_cardiac_fin <- remaining %>% filter(cardiac == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_respiratory_fin <- remaining %>% filter(respiratory == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_renal_fin <- remaining %>% filter(renal == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_immuno_fin <- remaining %>% filter(immunosuppression == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_pregnant_fin <- remaining %>% filter(pregnant == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)
p_chrom_fin <- remaining %>% filter(chromosomal_abnormality == 'yes') %>% summarise(mean(case_evolution == 'decease')) %>% pull(.)

#now we write a model that takes in consideration every one of this calculations:
p_fin <- seq(0,0,length.out = 39705)

for (i in 1:length(p_hat_fin)){
  if(validation$diabetes[i] == 'yes'){p_fin[i] = p_fin[i] + p_diabetes_fin} #if there is diabetes, we add the p_diabetes to the general probability 
  if(validation$cardiac[i] == 'yes'){p_fin[i] = p_fin[i] + p_cardiac_fin} #and so on for the other comobidities
  if(validation$respiratory[i] == 'yes'){p_fin[i] = p_fin[i] + p_respiratory_fin}
  if(validation$renal[i] == 'yes'){p_fin[i] = p_fin[i] + p_renal_fin}
  if(validation$immunosuppression[i] == 'yes'){p_fin[i] = p_fin[i] + p_immuno_fin}
  if(validation$pregnant[i] == 'yes'){p_fin[i] = p_fin[i] + p_pregnant_fin}
  if(validation$chromosomal_abnormality[i] == 'yes'){p_fin[i] = p_fin[i] + p_chrom_fin}
  p_fin[i] = p_fin[i] + p_hat_fin[i]
}

y_hat_fin <- factor(ifelse(p_fin > best_p, 'decease', 'cure'))
confusionMatrix(y_hat_fin, validation$case_evolution)
#since we couldn't tune for the best value of p we lost a little bit of accuracy here

##MODEL 2

#Now we are going to make a smaller and less imbalanced dataset, since, due to its size, we can't quite do much

train <- remaining %>% filter(case_evolution == 'decease')
#take all the deceases

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use set.seed(1)
temp <- sample_n(remaining %>% filter(case_evolution == 'cure'), size = nrow(train))
#take the same number of the deceases, but only cures

train <- add_row(train, temp)
#add the two together

#now we repeat the process with validation set to create test set:

test <- validation %>% filter(case_evolution == 'decease')
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use set.seed(1)
temp <- sample_n(remaining %>% filter(case_evolution == 'cure'), size = nrow(test))
test <- add_row(test, temp)

rm(temp) #remove tem set

#train glm model and predict on test, take the balanced accuracy
glm <- train(case_evolution ~ age + diabetes + cardiac + renal + respiratory + immunosuppression + pregnant + chromosomal_abnormality,
             data = train, method = 'glm')
glm_pred <- predict(glm, test)
results <- data.frame('model' = "Generalized Linear Model", 
                      'balanced_acc' = confusionMatrix(glm_pred, test$case_evolution)$byClass[['Balanced Accuracy']])

#train knn and predict on test, take the balanced accuracy 
knn <- train(case_evolution ~ age + diabetes + cardiac + renal + respiratory + immunosuppression + pregnant + chromosomal_abnormality,
             data = train, method = 'knn')
knn_pred <- predict(knn, test)
results <- add_row(results, data.frame('model' = "k-Nearest Neighbors", 
                      'balanced_acc' = confusionMatrix(knn_pred, test$case_evolution)$byClass[['Balanced Accuracy']]))

#train rpart and predict on test, take the balanced accuracy
rpart <- train(case_evolution ~ age + diabetes + cardiac + renal + respiratory + immunosuppression + pregnant + chromosomal_abnormality,
             data = train, method = 'rpart')
rpart_pred <- predict(rpart, test)
results <- add_row(results, data.frame('model' = "Decision Tree", 
                                       'balanced_acc' = confusionMatrix(rpart_pred, test$case_evolution)$byClass[['Balanced Accuracy']]))

#Ensemble:
ens1 <- if_else(glm_pred == 'cure' & knn_pred == 'cure' & rpart_pred == 'cure',
                'cure', 'decease')
#ensemble by only if all three say its a 'cure'
ens1 <- factor(ens1, levels = levels(test$case_evolution))
results <- add_row(results, data.frame('model' = "Ensemble - 1", 
                                       'balanced_acc' = confusionMatrix(ens1, test$case_evolution)$byClass[['Balanced Accuracy']]))

ens2 <- if_else(glm_pred == 'cure' & knn_pred == 'cure' |
                  glm_pred == 'cure' & rpart_pred == 'cure' |
                  rpart_pred == 'cure' & knn_pred == 'cure' ,
                'cure', 'decease')
#ensemble by marjority
ens2 <- factor(ens2, levels = levels(test$case_evolution))
results <- add_row(results, data.frame('model' = "Ensemble - 2", 
                                       'balanced_acc' = confusionMatrix(ens2, test$case_evolution)$byClass[['Balanced Accuracy']]))

results
#as seen on results, the best balanced accuracy between the ensembles was achieved on the first ensemble, 
#which will be the final model used on the validation set:

##MODEL 2 - FINAL

#make the final predictions on the validation set
glm_pred_fin <- predict(glm, validation)
knn_pred_fin <- predict(knn, validation)
rpart_pred_fin <- predict(rpart, validation)

ens <- if_else(glm_pred_fin == 'cure' & knn_pred_fin == 'cure' & rpart_pred_fin == 'cure',
                'cure', 'decease')

ens <- factor(ens, levels = levels(test$case_evolution))
confusionMatrix(ens, validation$case_evolution)

#Comparing to model 1 - final we improoved a little in regards to balanced accuracy, 
#and in sensitivity, which, considering the nature of the data, is more important than the overall accuracy.
#even though it wasn't the improovment I was expecting I still consider it valid, and satisfied me.