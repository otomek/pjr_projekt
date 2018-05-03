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


shootings.race <- shootings %>% group_by(race_ethnicity_shooter1) %>% 
  summarize(n = n(), kill = sum(killed), inj = sum(injured)) %>% 
  arrange(desc(n))

median(shootings$age_shooter1, na.rm = TRUE)

table(shootings$race_ethnicity_shooter1, shootings$shooting_type)


shootings.type <- shootings %>% group_by(shooting_type) %>% 
  summarize(n = n(), kill = sum(killed), inj = sum(injured)) %>% 
  arrange(desc(n))

table(shootings$race_ethnicity_shooter2)
shootings$age_shooter2

table(shootings$resource_officer)

shootings.officer <- shootings %>% group_by(resource_officer) %>% 
  summarize(n = n(), kill = sum(killed), inj = sum(injured)) %>% 
  arrange(desc(n))

table(shootings$weapon)


shootings %>% filter(casualties > 10) %>% select(casualties, weapon)

table(shootings$weapon_source)


shootings %>% 
  filter(shooting_type %in% c('targeted', 'indiscriminate', 'accidental')) %>% 
  ggplot(aes(x=shooting_type, y=age_shooter1)) +
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom='point', color='red', size=3, shape=17)

shootings.filter.type <- shootings %>% 
  filter(shooting_type %in% c('targeted', 'indiscriminate', 'accidental'))

summary(aov(age_shooter1 ~ shooting_type, shootings.filter.type))


shootings %>% 
  filter(shooting_type %in% c('targeted', 'indiscriminate', 'accidental')) %>% 
  ggplot(aes(x=shooting_type, y=casualties)) +
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom='point', color='red', size=3, shape=17)

summary(aov(age_shooter1 ~ injured, shootings.filter.type))

summary(as.factor(shootings$shooter_deceased1))
table(shootings$deceased_notes1)
table(shootings$shooter_relationship1)


shootings %>% ggplot(aes(x=as.factor(ulocale))) + geom_bar()


