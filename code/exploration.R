library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)



####### obrobka danych #######

### wczytuje dane ####
shootings <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-school-shootings/master/school-shootings-data.csv")

### sprawdzam i poprawiam typy ####
str(shootings)
# jest 50 zmiennych, niektore z nich powinny miec inny typ

# enrollment i white zostaly potraktowane jako factory,
# poniewaz w niektorych rekordach maja przecinki oddzielajace zcesci tysieczne
# usunmt je i zamienmy na wartosci numeryczne
# funkcja gsub wyszukuje wzorca i zamienia na inny
# (w tym wypadku wyszukam przecinkow i zamienie je na pusty ciag znakow)
shootings$enrollment <- as.numeric(gsub(',', '', shootings$enrollment))
shootings$white <- as.numeric(gsub(',', '', shootings$white))

# faktorami sa tez daty, zamienie je na znaki, a potem utworze dodatkowe 
# kolumny z miesiacem, dniem i rokiem osobno, kolumny z rokiem sie
# pozbede, bo juz taka jest
shootings <- shootings %>% 
  separate(date, into=c("month", "day", "year2"), sep='/', remove = FALSE) %>% 
  select(-year2)


####### analiza czasowa #######
# sprawdzam, czy mozna zaobserwowac jakies wzorce zwiazane z czasem

### grupowanie po roku ####
# sprawdze:
# - liczbe zdarzen
# - sume zabitych
# - sume rannych
# - przecietny wiek strzelajacego
shootings.year <- shootings %>% group_by(year) %>% 
  summarize(n = n(), kill = sum(killed), inj = sum(injured),
            mean.age = mean(age_shooter1, na.rm=TRUE))


### liczba zdarzen ####
shootings.year %>% ggplot(aes(x=as.factor(year), y=n)) + 
  geom_col(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Number of shootings in each year') + 
  labs(x = '', y = '')

### liczba zabitych ####
shootings.year %>% ggplot(aes(x=as.factor(year), y=kill)) + 
  geom_col(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Number of killed in each year') + 
  labs(x = '', y = '')

### liczba rannych ####
shootings.year %>% ggplot(aes(x=as.factor(year), y=inj)) + 
  geom_col(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Number of injured in each year') + 
  labs(x = '', y = '')

### sredni wiek ####
shootings.year %>% ggplot(aes(x=as.factor(year), y=mean.age)) + 
  geom_col(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Average age of shooter in each year') + 
  labs(x = '', y = '')
# nie widac zadnej zaleznosci, a sprawdzmy jeszcze raz wiek, ale
# za pomoca wykresow pudelkowych


### wiek ####
shootings %>% ggplot(aes(x=as.factor(year), y=age_shooter1)) + 
  geom_boxplot(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Age of shooter in each year') + 
  labs(x = '', y = '')
# takze niczego nie widac, choc rozklady w wiekszosci sa prawoskosne


### grupowanie po dniu tygodnia ####
# sprawdze:
# - liczbe zdarzen
# - sume zabitych
# - sume rannych

# ale najpierw ustalam porzadek dni, bo domyslny byl zly
shootings$day_of_week <- factor(shootings$day_of_week, 
                                levels=c('Monday', 'Tuesday',
                                         'Wednesday', 'Thursday',
                                         'Friday'))
# grupuje
shootings.weekday <- shootings %>% group_by(day_of_week) %>% 
  summarize(n = n(), kill = sum(killed), inj = sum(injured))

# wykres
shootings.weekday %>% ggplot(aes(x=day_of_week, y=n)) + 
  geom_col(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Number of shootings by weekday') + 
  labs(x = '', y = '')
# raczej pierwsza polowa tygodnia


####### analiza przestrzenna #######
# sprawdzam, czy mozna zaobserwowac jakies wzorce zwiazane z miejscem
# a konkretnie - czy ktorys stan przewaza

### tworze nowa ramke z grupowaniem po stanie ####
# tym razem sprawdzam:
# - liczbe zdarzen
# - sume zabitych
# - sume rannych
shootings.state <- shootings %>% group_by(state) %>% 
  summarize(n = n(), kill = sum(killed), inj = sum(injured))


### liczba zdarzen ####
shootings.state %>% ggplot(aes(x=state, y=n)) + 
  geom_col(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Number of shootings in each state') + 
  labs(x = '', y = '') + coord_flip() +
  scale_x_discrete(limits = rev(levels(shootings.state$state)))
  # ostatnia linijka zeby zmienic kolejnosc stanow na alfabetyczna

# zdecydowanie przoduje California, potem Floryda i Texas
# ale to sa tez najliczniejsze stany, wiec moze nie ma co sie dziwic
# natomiast z wykresu wynika, ze Pennsylvania raz zostala wprowadzona 
# ze spacja na koncu

### suma zabitych ####
shootings.state %>% ggplot(aes(x=state, y=kill)) + 
  geom_col(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Number of killed in each state') + 
  labs(x = '', y = '') + coord_flip() +
  scale_x_discrete(limits = rev(levels(shootings.state$state)))
# tutaj Connecticut i Floryda przoduja
# pewnie tam mialy miejsce najtragiczniejsze strzelaniny 
# sprawdzmy to inaczej

### liczba zabitych ####
# wykres punktowy z lekkim szumem losowym w poziomie,
# dokladne wartosci nie sa potrzebne, a tak bedzie lepiej widac
shootings %>% ggplot(aes(x=state, y=killed)) + 
  geom_jitter(width=0, size=2, alpha=0.5) + 
  theme_minimal() + ggtitle('Number of killed in each shooting') + 
  labs(x = '', y = '') + coord_flip() +
  scale_x_discrete(limits = rev(levels(shootings.state$state)))

### jeszcze raz liczba zabitych ####
# to samo co wyzej, ale dodaje rok do najtragiczniejszych strzelanin
shootings %>% ggplot(aes(x=state, y=killed)) + 
  geom_jitter(width=0, size=2, alpha=0.5) + 
  theme_minimal() + ggtitle('Number of killed in each shooting') + 
  labs(x = '', y = '') + coord_flip() +
  scale_x_discrete(limits = rev(levels(shootings.state$state))) +
  geom_label_repel(aes(label=ifelse(shootings$killed>4, shootings$year, '')))
                             

### suma rannych ####
shootings.state %>% ggplot(aes(x=state, y=inj)) + 
  geom_col(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Number of injured in each state') + 
  labs(x = '', y = '') + coord_flip() +
  scale_x_discrete(limits = rev(levels(shootings.state$state)))
# tutaj zdecydowanie California
# znow sprawdzmy to inaczej

### liczba rannych ####
shootings %>% ggplot(aes(x=state, y=injured)) + 
  geom_jitter(width=0, size=2, alpha=0.5) + 
  theme_minimal() + ggtitle('Number of injured in each shooting') + 
  labs(x = '', y = '') + coord_flip() +
  scale_x_discrete(limits = rev(levels(shootings.state$state))) +
  geom_label_repel(aes(label=ifelse(shootings$injured>9, shootings$year, '')))




####### wiek, plec, motywy #######


### histogram wieku ####
shootings %>% ggplot(aes(x = age_shooter1)) + 
  geom_histogram(boundary=0, fill='darkgrey', color='black', binwidth = 2) +
  geom_vline(xintercept = median(shootings$age_shooter1, na.rm=TRUE),
             color= 'red', lty=2, size=1.5) + 
  theme_minimal() + ggtitle('Distribution of shooters\' age') + 
  labs(x = '', y = '') + 
  annotate(geom='text', x=20, y=45, color = 'red', size=5,
           label=paste0('Median = ',median(shootings$age_shooter1, 
                                           na.rm=TRUE)))

### plec napastnikow ####
# wykres
shootings %>% ggplot(aes(x=gender_shooter1)) + 
  geom_bar(color='black', fill='darkgrey', width=0.3) +
  theme_minimal() + ggtitle('Gender of shooters') + 
  labs(x = '', y = '')


# tabelka
shootings %>% group_by(gender_shooter1) %>% 
  summarise(n = n())
# tabelka bedzie lepsza
# zdecydowana przwaga mezczyzn


### motywy napastnikow ####

# sprawdzmy jak to wyglada liczbowo
sort(table(shootings$shooting_type), decreasing = TRUE)
# celowe (z checia zabicia) zdecydowanie przewazaja, 
# oprocz tego jeszcze czesto zdarzaja sie indiscriminate 
# (strzelanie bez konkretnych celow) i przypadkowe
# te trzy typy wezme na tapet


# liczba zabitych
shootings %>% 
  filter(shooting_type %in% c('targeted', 'indiscriminate', 'accidental')) %>% 
  ggplot(aes(x=shooting_type, y=killed)) + 
  geom_boxplot(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Number of killed') + 
  labs(x = '', y = '')


# liczba rannych
shootings %>% 
  filter(shooting_type %in% c('targeted', 'indiscriminate', 'accidental')) %>% 
  ggplot(aes(x=shooting_type, y=injured)) + 
  geom_boxplot(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Number of injured') + 
  labs(x = '', y = '')


# liczba ofiar ogolem
shootings %>% 
  filter(shooting_type %in% c('targeted', 'indiscriminate', 'accidental')) %>% 
  ggplot(aes(x=shooting_type, y=casualties)) + 
  geom_boxplot(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Number of casulaties (killed + injured)') + 
  labs(x = '', y = '') 
# najgorsze sa strzelaniny bez okreslonych celow

# sprawdzam srednia liczbe ofiar
shootings %>% 
  filter(shooting_type %in% c('targeted', 'indiscriminate', 'accidental')) %>% 
  group_by(shooting_type) %>% 
  summarise(mean.casualties = mean(casualties, na.rm=TRUE))
# roznice sa bardzo duze

# sprawdzam statystyczna istotnosc roznicy w rozkladach
# uzywam testu Kruskala-Wallisa, zby to ocenic
# nowa pomocna ramka
shootings.filter.type <- shootings %>% 
  filter(shooting_type %in% c('targeted', 'indiscriminate', 'accidental')) 
# test
kruskal.test(casualties ~ shooting_type, shootings.filter.type)
# p-value <0.05, czyli mozna przyjac, ze te roznice rzeczywiscie wystepuja


# wiek strzelajacych
shootings %>% 
  filter(shooting_type %in% c('targeted', 'indiscriminate', 'accidental')) %>% 
  ggplot(aes(x=shooting_type, y=age_shooter1)) + 
  geom_boxplot(color='black', fill='darkgrey') + 
  theme_minimal() + ggtitle('Shooters\' age') + 
  labs(x = '', y = '')
# wydaje sie, ze istnieje zwiazek miedzy wiekiem a rodzajem strzelaniny
# te celowe sa przeprowadzane przez starszych, a te przypadkowe - przez
# mlodszych (choc mediany tego nie pokazuja, ale pudelka juz tak)

# sprawdzam srednia wieku
shootings %>% 
  filter(shooting_type %in% c('targeted', 'indiscriminate', 'accidental')) %>% 
  group_by(shooting_type) %>% 
  summarise(mean.age = mean(age_shooter1, na.rm=TRUE))
# srednie wyraznie rozne

# ponownie uzywam testu Kruskala-Wallisa, zeby sprawdzic
# statystyczna istotnosc roznic w rozkladach
# ale biorac pod uwage podobienstwo median, ten test raczej
# niczego nie pokaze
kruskal.test(age_shooter1 ~ shooting_type, shootings.filter.type)
# p-value 0.16 - za duze, zeby odrzucic H0




shootings.officer <- shootings %>% group_by(resource_officer) %>% 
  summarize(n = n(), kill = sum(killed), inj = sum(injured)) %>% 
  arrange(desc(n))

table(shootings$weapon)


shootings %>% filter(casualties > 10) %>% select(casualties, weapon)

table(shootings$weapon_source)




