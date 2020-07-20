if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

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

write.csv(clean_dataset, file = 'data\\clean_dataset.csv')#export the clean dataset so I can push it into GitHub

