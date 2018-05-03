library(dplyr)
library(tidyr)
library(ggplot2)

shootings <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-school-shootings/master/school-shootings-data.csv",
                      dec = ",")
shootings$enrollment <- as.numeric(gsub(',', '', shootings$enrollment))

?read.csv

names(shootings)

shootings.year <- shootings %>% group_by(year) %>% 
  summarize(n = n(), kill = sum(killed), inj = sum(injured)) %>% 
  mutate(cumn = cumsum(n)) %>%
  mutate(cumkill = cumsum(kill)) %>% 
  mutate(cuminj = cumsum(inj))

shootings.year %>% ggplot(aes(x=year, y=kill)) + geom_col()
shootings.year %>% ggplot(aes(x=year, y=cumkill)) + geom_line()


shootings.year %>% select(-c(kill, inj, n)) %>% gather(measure, value, -year) %>% 
  ggplot(aes(x = year, y=value)) + geom_line(aes(color=measure)) +
  theme_minimal()

shootings.state <- shootings %>% group_by(state) %>% 
  summarize(n = n(), kill = sum(killed), inj = sum(injured)) %>% 
  arrange(desc(n))


shootings %>% ggplot(aes(x=year, y=state)) + 
  geom_point(aes(size=casualties, color=race_ethnicity_shooter1), 
             position = position_jitter())








