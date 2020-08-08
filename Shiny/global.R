library(shiny)
library(shinydashboard)
library(googleVis)
library(dplyr)
library(tibble)
library(DT)
library(plotly)
library(leaflet)
library(tidyverse)
library(shinythemes)

# state_stat=state.x77 %>% 
#   as.data.frame() %>% 
#   rownames_to_column('state.name')

#public school score data
scores <- read_csv('scores.csv')
scores <- na.omit(scores) %>% 
  as.data.frame() 

scores_total <- scores %>% 
  mutate(sat_total = avg_score_math + avg_score_reading + 
           avg_score_writing)


#nyc data
nyc <- read_csv('nyc.csv')

nyc <- nyc %>% 
  dplyr::rename(borough = population,
                population = population_1)

nyc <- nyc %>% 
  separate(long_lat, into = c('longitude', 'latitude'), 
           sep = ',')

nyc <- nyc %>% 
  dplyr::mutate(household_income = as.numeric(gsub('[$,]', '', 
                                                   household_income)))

# Total data
full <- left_join(scores, nyc, by = 'zip') %>% 
  select(-city, -building_code, -id) %>% 
  dplyr::mutate(household_income = as.numeric(gsub('[$,]', '', household_income)))

full <- full %>% 
  select(-borough.y,
         borough = borough.x,
         school_long = longitude.x,
         school_lat = latitude.x,
         zip_lat = longitude.y,
         zip_long = latitude.y) %>% 
  na.omit(full)


#Private school enrollment
year <- c('2015-2016', '2016-2017', '2017-2018', '2018-2019', '2019-2020')
enroll <-c(72214, 73078, 72698, 72976, 73021)

priv <- as.data.frame(cbind(year, enroll))
priv = priv %>% 
  mutate(enroll = as.numeric(enroll))

#Charter Schools
charter <- read_csv('operating_charters.csv')

chart_count =charter %>%
  dplyr::mutate(year_open = `Year Opened`)

chart_count = chart_count %>% 
  dplyr::group_by(year_open) %>% 
  summarize(count = n())

char_borough <- charter %>% 
  dplyr::group_by(Borough) %>% 
  summarise(count = n()) %>% 
  dplyr::arrange(desc(count))
char_borough

#score comparison
comp <- read_csv('pub_priv.csv') %>% 
  select(c(1,2,3,4))
head(comp)

colnames(comp) <- c('type', 'Reading', 'Math', 'Writing')

#map label

label <- paste(scores$school_name, 'Address:', scores$address, ', Zip:', scores$zip,
               ', Borough', scores$borough, ', Enrollment:', scores$enrollment,
               ', SAT Math:', scores$avg_score_math,
               ', SAT Reading:', scores$avg_score_reading, ', SAT Writing:', 
               scores$avg_score_writing, ', Percent Tested:', scores$percent_tested)
