library(tidyverse)
library(tidycensus)
library(plotly)
library(leaflet)
library(sf)

scores <- read_csv('scores.csv')
scores <- na.omit(scores)

plot.new()
#boxplot of scores
scores %>%
  plot_ly(y = ~avg_score_math, type = 'box', name = 'Math', 
          text = ~school_name) %>% 
  add_trace(y = ~avg_score_reading, type = 'box', name = 'Reading', 
            text = ~school_name) %>% 
  add_trace(y = ~avg_score_writing, type = 'box', name = 'Writing', 
            text = ~school_name) %>% 
  layout(yaxis = list(title = ''), title = 'SAT Scores In NYC Public High Schools', 
         showlegend = F, colors = 'set1')

#scatterplot of scores
scores %>% 
  plot_ly(x = ~avg_score_math, y = ~avg_score_reading, 
          text = ~school_name, jitter = .2, colors = 'set1') %>% 
          layout(title = 'Math And Reading Scores By School', xaxis = list(title = 'Avg Math SAT Score'), 
         yaxis = list(title = 'Avg Reading SAT Score'))


#map of schools
label <- paste(scores$school_name, 'Address:', scores$address, ', Zip:', scores$zip,
               ', Borough', scores$borough, ', Enrollment:', scores$enrollment,
               ', SAT Math:', scores$avg_score_math,
               ', SAT Reading:', scores$avg_score_reading, ', SAT Writing:', 
               scores$avg_score_writing, ', Percent Tested:', scores$percent_tested)

leaflet(scores) %>% 
  addTiles() %>% 
  addMarkers(~scores$longitude, ~scores$latitude, 
             label = scores$school_name,
             popup = label)  
  

#Denisty Chat Of Scores - Add Dropdown menu for specific categories
scores_total <- scores %>% 
  mutate(sat_total = avg_score_math + avg_score_reading + 
           avg_score_writing)
  

scores_total %>% 
  ggplot(aes(x = sat_total)) +
  stat_density(aes(color=borough), geom="line",position="identity") +
  labs(title='Density of Avg SAT Score By Borough', 
         x='Score', 
         y='Density') +
  scale_colour_brewer(palette='Set1') +
  theme_bw()

#Income by Zip

########### FORMAT INCOME DATA #######################
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
########################################################

# visualizing scores by zipcode using gather
scores %>%
  dplyr::mutate(zip = as.character(zip)) %>% 
  dplyr::group_by(zip) %>% 
  dplyr::summarise(average_math = mean(avg_score_math),
                   average_reading = mean(avg_score_reading),
                   average_writing = mean(avg_score_writing)) %>% 
  dplyr::arrange(desc(average_math)) %>%
  top_n(10) %>%
  plot_ly(x = ~zip, y = ~average_math, type = 'bar', 
          name = 'Math', colors = 'set1') %>% 
  add_trace(y = ~average_reading, name = 'Reading') %>%
  # add_trace(y = ~average_writing, name = 'Writing') %>%
  layout(xaxis = list(title = 'Zip', tickangle = -45), 
         yaxis = list(title = 'Score'), barmode = 'group',
         categoryorder = 'array', categoryarray = scores[order(~average_math)])
  
  
  # gather(key = 'test', value = 'score', c(2,3,4)) %>% 
  # plot_ly(x = ~zip, y = ~score, type = 'bar')
  #top_n(20) #make this  top and bottom

#Income By Zip
nyc %>%
  na.omit(nyc) %>% 
  dplyr::mutate(zip = as.character(zip)) %>%
  dplyr::mutate(household_income = as.numeric(gsub('[$,]', '', household_income))) %>% 
  dplyr::arrange(desc(household_income)) %>% 
  top_n(20) %>% 
  plot_ly(x = ~zip, y = ~household_income, type = 'bar', 
          name = 'Income', colors = 'set1') %>% 
  layout(title = 'Household Income By Zip', xaxis = list(title = 'Zip', tickangle = -45), 
         yaxis = list(title = 'Household Income'))

  
############### JOIN TO INCLUDE INCOME ###########

full <- left_join(scores, nyc, by = 'zip') %>% 
  select(-city, -building_code, -id) %>% 
  dplyr::mutate(household_income = as.numeric(gsub('[$,]', '', household_income)))

str(full)

#Same scatter chart as above (x=math, y=reading) but with hh_income as size
full %>% 
  plot_ly(x = ~avg_score_math, y = ~avg_score_reading, 
          text = ~school_name, type = 'scatter', jitter = .3, 
          colors = 'set1', marker = list(size = ~household_income/5000, opacity = .5)) %>% 
          layout(title = 'Scores By School', xaxis = list(title = 'Avg Math SAT Score'), 
                 yaxis = list(title = 'Avg Reading SAT Score'))





