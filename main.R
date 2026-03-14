library(tidyverse)
library(patchwork)
library(ggthemes)


data <- read.csv("ufo_sighting_data.csv") %>% 
  mutate(Date_time = mdy_hm(Date_time), 
         date_documented = mdy(date_documented),
         documantation_delay = round(difftime(date_documented, as.Date(Date_time), units = "days")),
         length_of_encounter_seconds = as.numeric(length_of_encounter_seconds),
         description = gsub("&#44", ",", description),
         description = gsub("&#33", "!", description),
         description = gsub("&#39", "'", description),
         description = gsub("&quot", '"', description),
         latitude = as.numeric(latitude),
         year = year(Date_time), 
         month = month(Date_time),
         weekday = wday(Date_time, label = TRUE, abbr = FALSE, week_start = 1),
         hour = hour(Date_time),
         ) %>% 
  filter(!is.na(latitude) & !is.na(length_of_encounter_seconds))




str(data)

#Sprawdzamy w którym kraju jest najwięcej zgłoszeń
         
data %>% 
  filter(!country == "") %>% 
  group_by(country) %>% 
  summarize(liczba_obserwacji=n()) %>% 
  arrange(desc(liczba_obserwacji))

ggplot(data, aes(x=country, fill = country)) +
  geom_bar()+
  scale_color_colorblind()

#Patrzymy który stan ma najwięcej
data %>% 
  filter(country == "us") %>% 
  group_by(state.province) %>% 
  summarize(liczba_obserwacji = n()) %>% 
  arrange(desc(liczba_obserwacji))  %>% 
  top_n(5)

# Miasto z największą ilością zaobserwowanych zjawisk
data %>% 
  group_by(city, country) %>% 
  summarize(liczba_obserwacji = n()) %>% 
  arrange(desc(liczba_obserwacji)) 




#Liczba zaobserwowanych zjawisk na przestrzeni lat

data1 <- data %>% 
  group_by(year) %>% 
  summarize(liczba_obserwacji = n())

ggplot(data1, aes(x=year, y=liczba_obserwacji))+
  geom_line()


#Który miesiąc jest najlepszy do obserwacji ufo

ggplot(data, aes(x=month))+
  geom_bar()+
  scale_x_continuous(breaks = c(1:12) )+
  theme_minimal()


#Która godzina jest najlepsza do obserwacji ufo
  
ggplot(data, aes(x=hour))+
  geom_bar()


#Jak zmieniał się rozkład kształtów ufo na przestrzeni lat

data$UFO_shape

data %>% 
  group_by(UFO_shape) %>% 
  summarize(Liczba_obserwacji = n()) %>% 
  arrange(desc(Liczba_obserwacji))


#Rozkład obserwacji zjawisk nja mapie

country_map = map_data("world")

ggplot() +
  geom_map(data = country_map, 
           map = country_map,aes(x = long, y = lat, map_id = region, group = group),
           fill = "white", color = "black", size = 0.1)+
            geom_point(data = data, aes(x=longitude, y = latitude), color = "red", size = 0.5, alpha = 0.5)

#Wykres wąsaty szerokości i długości geograficznej



ggplot(data, aes(y=longitude))+
  geom_boxplot(fill="green")+ggplot(data, aes(y=latitude))+
  geom_boxplot(fill="purple")

#Rozkład obserwacji w USA

usa_map = map_data("usa")

us_data <- data %>% 
  filter(country=="us")

ggplot()+
  geom_map(data = usa_map, 
       map=usa_map, aes(x = long, y = lat, map_id = region, group = group),
       fill = "white", color = "black", size = 0.1)+ 
  geom_point(data = us_data, aes(x=longitude, y = latitude), color = "red", size = 0.5, alpha = 0.5)+
  coord_quickmap()


#Podstawowe statystyki dla czasu trwania ufo

data %>% 
  group_by(UFO_shape) %>% 
  summarise(
    srednia = mean(length_of_encounter_seconds),
    mediana = median(length_of_encounter_seconds),
    odchyl = sd(length_of_encounter_seconds),
    min = min(length_of_encounter_seconds),
    max = max(length_of_encounter_seconds),
    liczba_obserwacji = n()
  ) %>% 
  arrange(desc(liczba_obserwacji))



#zmiana trendów kształtów

data2 <- data %>% 
  filter(UFO_shape != "unknown") %>% 
  group_by(UFO_shape) %>% 
  summarize(liczba_obserwacji = n()) %>% 
  arrange(desc(liczba_obserwacji)) %>% 
  top_n(5)

data3 <- data %>% 
  filter(UFO_shape %in% data2$UFO_shape) %>% 
  group_by(year, UFO_shape) %>% 
  summarize(liczba_obserwacji = n()) 

ggplot(data3, aes(x=year, y=liczba_obserwacji, color=UFO_shape))+
  geom_line()+
  xlim(1940, 2020)+
  theme_minimal()

ggplot(data3, aes(x=year, y=liczba_obserwacji, color=UFO_shape))+
  geom_line()+
  xlim(1990, 2020)+
  theme_minimal()


#Pierwsze zgłoszenie
data %>% 
  arrange(Date_time) %>% 
  slice(1)

#Ostatnie zgłoszenie
data %>% 
  arrange(desc(Date_time)) %>% 
  slice(1)


#w który dzień tygodnia najwięcej
data %>% 
  group_by(weekday) %>% 
  summarize(liczba_obserwacji = n())
  
#dane dla opoznienia

data %>% 
  filter(documantation_delay > 0) %>% 
  summarize(srednia = round(mean(documantation_delay)),
            min = min(documantation_delay),
            max = max(documantation_delay),
            odch = sd(documantation_delay),
            Q1 = quantile(documantation_delay, 0.25, na.rm = T),
            med = median(documantation_delay),
            Q3 = quantile(documantation_delay, 0.75, na.rm = T)
            )


#boxploty

data %>% 
  filter(UFO_shape %in% data2$UFO_shape) %>% 
  ggplot(aes(x=UFO_shape, y = length_of_encounter_seconds))+
  geom_boxplot() +
  scale_y_log10() +
  
  theme_minimal() 


```{r}
data %>% 
  filter(UFO_shape %in% data2$UFO_shape) %>% 
  ggplot(aes(x=UFO_shape, y = length_of_encounter_seconds))+
  geom_boxplot(fill="forestgreen") +
  scale_y_log10() +
  theme_classic() 
```
