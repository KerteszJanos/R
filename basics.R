#https://www.youtube.com/watch?v=eR-XRSKsuR4

library(tidyverse)

women %>%
  filter(height > 60) %>% 
  View()

starwars %>% 
  filter(height > 150 & mass < 200) %>%
  mutate(height_in_meters = height/100) %>% #add new col called hight_in_meters
  select(height_in_meters, mass) %>% #will show only the pararameters
  arrange(-mass) %>% 
  #View()
  plot()

View(msleep)
glimpse(msleep)  
head(msleep)

class(msleep$name) #returns the type of the col 
length(msleep) #returns col number
length(msleep$name) #returns row number

names(msleep) #returns the names of cols

unique(msleep$vore)
#NA means Not Avaiable (data)

missing <- !complete.cases(msleep) #returns a bool array that is true if a row is not compete

msleep[missing, ] #listing all the rows where is a missing value

#-----------------------------

starwars %>% 
  select(name, height, mass) #select(1:3)

starwars %>% 
  select(ends_with("color"))

starwars$hair_color <- as.factor(starwars$hair_color) #chaning the original hair color to a factor

starwars %>% 
  mutate(hair_color = as.character(hair_color)) %>% 
  glimpse()


#Chaning factor levels
df <- starwars

df$sex <- as.factor(df$sex)

levels(df$sex)

#change factor levels order
df <- df %>% 
  mutate(sex = factor(sex, levels = c("male", "female", "hermaphroditic", "none")))

levels(df$sex)


table(df$sex) #counts rows in the original "table"

#filter rows
starwars %>% 
  select(mass, sex) %>% 
  filter(mass < 55 & sex == "male")

#recode data
starwars %>% 
  select(sex) %>% 
  mutate(sex = recode(sex, "male" = "man", "female" = "woman"))

df


#Handle missing data
mean(starwars$height) #cant applie this func bc of the missing data

mean(starwars$height, na.rm = TRUE) #returns the avg height removing the NA values


#create or change a variable with mutate

#if_else function works like ?: in C#


#reshape data with pivot rider
install.packages("gapminder")
library(gapminder)

data <- select(gapminder, country, year, lifeExp) #could look like this: gapminder %>% select(...)
data

wide_data <- data %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

wide_data

#reshape data with pivot longer
long_data <- wide_data %>% 
  pivot_longer(2:13, names_to = "year", values_to = "lifeExp")

summary(msleep$awake)
msleep$awake %>% summary

msleep %>% 
  select(awake, sleep_total) %>% 
  summary()

msleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>% 
  summarise(Lower = min(sleep_total),
            Avarage = mean(sleep_total),
            Upper = max(sleep_total),
            Difference = max(sleep_total) - min(sleep_total)) %>% 
  arrange(Avarage)

#Create tables
