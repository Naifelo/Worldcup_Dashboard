library(tidyr)
library(dplyr)
library(purrr)

# Read the data
statsbomb <- readRDS("~/statsbomb.rds")

# All teams
team_name <- unique(statsbomb$Home)

# All players
player_name <- statsbomb %>%
  unnest() %>%
  select(player.name,team.name) %>%
  unique() %>% na.omit()

# All types
type_name <- statsbomb %>%
  unnest() %>%
  pull(type.name) %>%
  unique() %>% na.omit()


# Variables to be calculated
variables_<-names(statsbomb$data[[1]])

variables_to_count <- variables_[c(12,30,32,34,35,37,41,45,47,51,52,53,54,74,76,79,85,89,91,92)]

variables_to_count <- set_names(variables_to_count)

variables_to_sum <- variables_[27]


# Function to calculate the avg
n_types <- function(x){
  x %>% 
    group_by(team.name, player.name, type.name) %>% 
    summarise(n_types = length(type.name)) %>% 
    na.omit()
}

# Calculate the averag type per game.
aggdata_n <- statsbomb %>%
  mutate(by_types = map(data, n_types))


# variables_to_count
n_types_2 <- function(x, group_var){
  var <- rlang::ensym(group_var)
  x %>% 
    group_by(team.name, player.name, !!var) %>% 
    summarise(n_types = length(!!var))  %>% 
    na.omit()
}

# Calculate the averag type per game.
aggdata_n<- aggdata_n %>%
  mutate(by_vairables = map(data, ~pmap(list(group_var = variables_to_count), n_types_2, x = .))) 

# Function to calculate the avg pass.length
passes_length <- function(x){
  x %>% 
  group_by(team.name, player.name) %>% 
  summarise(passes_length = sum(pass.length, na.rm = T)) %>% 
    na.omit()
}

# Calculate the passes per game.
aggdata_n <- aggdata_n %>%
  mutate(by_passes_length = map(data, passes_length))

###########################################################################################


# calculate the average per player
by_vairables<- aggdata_n  %>%
  select(by_vairables) %>%
  unnest() %>% unnest() %>%
  gather(key,value, -team.name, -player.name,-n_types) %>% na.omit() %>%
  group_by(player.name, key, value) %>% 
  summarise(average_vairables = mean(n_types) %>% round(2))  %>% ungroup()



# calculate the average per player
by_passes_length<- aggdata_n  %>%
  select(by_passes_length) %>%
  unnest()  %>% 
  group_by(player.name) %>% 
  summarise(average_passes = mean(passes_length) %>% round(2))  
  
  
  
# calculate the average per player
by_types<- aggdata_n  %>%
    select(by_types) %>%
    unnest() %>% 
    group_by(player.name, type.name) %>% 
    summarise(average_types = mean(n_types) %>% round(2))   %>% ungroup()
  
  

all_data <- by_vairables %>% left_join(by_passes_length, by= "player.name") %>% left_join(by_types, by= "player.name")  
  
all_data %>% View()  
  
  
all_data %>%
  mutate(var = stringr::str_c(key, value,sep = "_")) %>%
  select(-key,-value) %>%
  saveRDS("data/all_data.rds")

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


