##### Description: BAE590 Module 8 homework
##### Author: Kanjana Laosuntisuk
##### Date created: Oct 19, 2019
##### Last modified: Oct 19, 2019

# clear workspace and load packages
rm(list=ls(all=TRUE))

library(tidyverse)

##### National Park Visits
##### question 1: Which national park had the highest number of visitors in 2016?

# get data
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")

# inspect the dataset
str(park_visits)
head(park_visits)
tail(park_visits)
summary(park_visits)

# filter national park data from 2016
park_visits %>%
  filter(unit_type == "National Park") %>%
  filter(year == "2016") -> park_visits_2016 
View(park_visits_2016)

# make park names shorter for a plot
park_visits_2016$unit_name <- str_replace(park_visits_2016$unit_name, "National Park", "")
park_visits_2016$unit_name <- str_replace(park_visits_2016$unit_name, 
                                          " of American Samoa", "American Samoa")
park_visits_2016$unit_name <- str_replace(park_visits_2016$unit_name, 
                                          "Wolf Trap  for the Performing Arts", "Wolf Trap")

# remove duplicated data of Denali National preserve
park_visits_2016 <- park_visits_2016[-49, ]
View(park_visits_2016)

# change unit_code to factor
park_visits_2016$unit_code <- factor(park_visits_2016$unit_code)
str(park_visits_2016)

# Make a bar chart
park_visits_2016 %>%
  mutate(unit_code = fct_reorder(unit_code, visitors)) %>%
  ggplot(mapping = aes(x = unit_code, y = visitors, fill = unit_name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Visitors of U.S. National Parks in 2016",
       y = "Number of Visitors",
       x = "Park Code Abbreviation",
       caption = "Source: dataisplural/data.world.") +
  scale_fill_discrete(name = "Park Name")

# save a plot
ggsave("National_park_visitors_2016.pdf", width = 10, height = 8, units = "in")



##### question 2: According to top 5 most-visited national parks in 2016, how does the number of visitors to these parks change over time?

# filter GRSM, GRCA, YOSE, ROMO, ZION and remove "Total" in year
park_visits %>%
  filter(unit_code == "GRSM" | unit_code == "GRCA" | unit_code == "YOSE" | 
           unit_code == "ROMO" | unit_code == "ZION") %>%
  filter(year != "Total") -> park_visits_top5 
View(park_visits_top5)

#change year to numeric 
str(park_visits_top5)
park_visits_top5$year <- as.numeric(park_visits_top5$year)
str(park_visits_top5)

# Make a line plot
ggplot(park_visits_top5, mapping = aes(x = year, y = visitors, color = unit_code)) +
  geom_line(show.legend = TRUE, size = 2) +
  labs(title = "Number of Visitors to Top 5 U.S. National Parks",
       y = "Number of Visitors",
       x = "Year",
       caption = "Source: dataisplural/data.world.") +
  scale_color_brewer(palette = "Set2", 
                     name = "Park Name", 
                     breaks = c("GRSM", "GRCA", "YOSE", "ROMO", "ZION"),
                     labels = c("Great Smoky Mountains", "Grand Canyon", 
                                "Yosemite", "Rocky Mountain", "Zion")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

# save a plot
ggsave("Top5_National_park_visitors.pdf", width = 10, height = 8, units = "in")
