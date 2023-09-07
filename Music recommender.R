#Tidyverse is an R programming package that helps to transform and better present data. It assists with data import, tidying, manipulation, and data visualization. 
library(tidyverse)
polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

#number of entries of rankings is lesser as the top 5 songs of each person can repeat and are hence considered only once in the rankings dataset
polls     
rankings   #points is 2(5n1+4n2...n5)
# The pipe operator, written as %>%. It takes the output of one function and passes it into another function as an argument. 
# This block of code will show which year has how many songs in the dataset
polls %>%
  count(year) %>%
  mutate(decade = floor(year/10)*10) %>%  
  #(year/10)*10 is taken to reduce the scale to decade wise classification
  #mutate() adds new variables and preserves existing ones
  #floor() returns the largest integer that is smaller than or equal to value passed to it as argument
  mutate(decade = as.factor(decade)) %>%
  #Factor in R is a variable used to categorize and store the data
  ggplot(aes(x = year, y = n, fill = decade)) + geom_col()

# This block of code will show which decade has how many songs in the dataset
polls %>%
  count(year) %>%
  mutate(decade = floor(year/10)*10) %>%
  mutate(decade = as.factor(decade)) %>%
  ggplot(aes(x = decade, y = n, fill = decade)) + geom_col()

# This shows how many songs each artist has in this dataset in descending order
polls %>%
  count(artist, sort = TRUE)

# Tells us how many songs of the artists have received a particular rank
rankings %>%
  select(artist, n, n1, n2, n3, n4, n5) %>%
  group_by(artist) %>%
  summarise_all(sum)

# Scatter plot of the number of songs that have rank 1 and rank 5
rankings %>%
  select(artist, n, n1, n2, n3, n4, n5) %>%
  group_by(artist) %>%
  summarise_all(sum) %>%
  filter(!str_detect(artist, "ft.")) %>%
  #The filter() function is used to subset a data frame, retaining all rows that satisfy your conditions.
  ggplot(aes(x=n1, y=n5)) + geom_jitter()

# Arranges the list in descending order of number of mentions of an artists on rank 1
rankings %>%
  select(artist, n, n1, n2, n3, n4, n5) %>%
  group_by(artist) %>%
  summarise_all(sum) %>%
  filter(!str_detect(artist, "ft.")) %>%
  arrange(desc(n1))%>%
  slice(1:5)

# Arranges the list in descending order of number of mentions of an artists on rank 5
rankings %>%
  select(artist, n, n1, n2, n3, n4, n5) %>%
  group_by(artist) %>%
  summarise_all(sum) %>%
  filter(!str_detect(artist, "ft.")) %>%
  arrange(desc(n5))%>%
  slice(1:5)

# 15%  of top songs were voted by countries except US
# less chance of home town/country bias
polls%>%
  count(title, critic_country, name = "song_nom")%>%
  add_count(title, name = "number_of_countries")%>%
  filter(number_of_countries == 1 & critic_country!="US")%>%
  nrow()/nrow(polls)


# Describes number of critics votes for each song from a particular country
polls%>%
  count(title, critic_country, name = "song_nom_country")%>%
  add_count(title, name = "number_of_countries")%>% 
  filter(number_of_countries != 1)%>%
  select(-number_of_countries)%>%
  pivot_wider(names_from = "critic_country", values_from = "song_nom_country")

library(recommenderlab)

#creating matrix from existing dataset for training model
rap_matrix <- polls%>%
  select(critic_name, title)%>%
  mutate(n=1)%>%
  arrange(title)%>%
  pivot_wider(names_from = "title", values_from = "n", values_fill = list(n = 0))%>%
  #pivot_wider increasing the number of columns and decreasing the number of rows.
  select(-critic_name)%>%
  as.matrix()%>%
  as("binaryRatingMatrix")


# train = 0.8 means fraction of dataset used for training
training_schema <- evaluationScheme(rap_matrix, method = "split", train = 0.8 , given = -1)
training_schema

UBCF_Model <- evaluate(training_schema, method = "UBCF", type = "topNList",  n = 5)
IBCF_Model <- evaluate(training_schema, method = "IBCF", type = "topNList",  n = 5) 

UBCF_Model %>% avg()

IBCF_Model %>% avg()
#this gives the results of confusion matrix.
#TPR tells how many correct positive results occur among all positive samples
#FPR tells how many  incorrect positive results occur among all negative samples

tune_engines <- function(schema, parameters){
  
  UBCF_Model <- evaluate(schema, method = "UBCF", type = "topNList",  n = 5, param = list(nn = parameters))
  IBCF_Model <- evaluate(training_schema, method = "IBCF", type = "topNList",  n = 5, param = list(k = parameters)) 
  
  UBCF_Model %>% 
    avg %>% 
    as_tibble() %>% 
    mutate(model = "UBCF")%>%
    rbind(IBCF_Model %>% 
            avg %>% 
            as_tibble() %>% 
            mutate(model = "IBCF"))%>%
    return()
  
}

tune_grid <- tibble(parameters = c(3, 5, 10,  15, 20, 25))



# Use 5 nearest neighbours
history <- tune_grid  %>% 
  mutate(results = map(parameters, ~tune_engines(training_schema, .x))) %>% 
  unnest()

history %>% 
  ggplot(aes(x = parameters, y = TPR, fill = model, label = parameters)) + geom_col(position = "dodge") + geom_text(aes(x = parameters, y = TPR))



# Final Model=
UBCF_Final_Model <- Recommender(getData(training_schema, "train"), "UBCF", param = list(nn = 5))

UBCF_Final_Model

predictions <-predict(UBCF_Final_Model, getData(training_schema, "known"), type = "topNList")
#calcPredictionAccuracy(predictions, getData(training_schema, "unknown"), given = -1)


rec_engine <- Recommender(rap_matrix, "UBCF", param =list(nn=5))
rec_engine

polls%>% filter(str_detect(artist, "Drake"))%>%distinct(title)%>%arrange(title)

aditya_songs <- polls %>%
  select(title)%>%
  distinct()%>%
  arrange(title)%>%
  filter(title %in% c("All of The Lights", "Alright", "Bitch Don’t Kill My Vibe", "m.A.A.d. city", "changes"))%>%
  rbind(polls%>%select(title)%>%distinct())%>%
  count(title)%>%
  mutate(n = n-1)%>%
  pivot_wider(names_from = "title", values_from = "n", values_fill = list(n = 0))%>%
  as.matrix()%>%
  as("binaryRatingMatrix")

predict(rec_engine, aditya_songs)%>%as("list")%>%as.data.frame()



#tune_engines(training_schema, parameters = 10)