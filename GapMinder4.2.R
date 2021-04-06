library(dslabs)
library(tidyverse)
data(gapminder)
head(gapminder)

#transformations 4.2
#add dolars per day variable
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

#histogram of dollar per day
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth =1 , color = "black")

#repeat histogram with log2 scaled data
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, col= "black")

#repeat histogram with log2 scaled x-axis
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")

#Boxplot of GDP by region
#add dolars oer day variable 
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

#number of regions
length(levels(gapminder$region))

#boxplot of GDp by region in 1970 
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p +  geom_boxplot()

#rotate names on x-axis
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust= 1))

#the reorder function
#by default factor order is alphabetical
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)

#reorder factor by the category names
value <- c(10, 11, 12, 6, 4)
fac <- reorder (fac, value, FUN = mean)
levels (fac)

#Enhanced boxplot ordered  by median income, scaled and showing data
#reorder using the median income and color by continent
p <- gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% #reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) + #color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")
p

#log2 scale y-axis
p + scale_y_continuous(trans = "log2")

#add data points
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)


#Histogram of income in West versus developing world 
#dollars per day and past year already defined
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)
past_year <- 1970
#define Western countries 
west <- c("Western Europe", "northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

#facet by West vs dveloping countries
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color= "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(.~group)

#facet by West/developing and year
present_year <- 2010
gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

#Income distribution in West versus developing world, only countries with data
#define countries that have data available in both years
country_list_1 <- gapminder %>%
  filter(year ==past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

#make histogram including only countries with data available in both years
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>% #keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

#Boxplot of income in West vs Developing world, 1970 and 2010
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot()+
  theme(axis.text.x = element_text(angle= 90, hjust = 1)) +
  xlab ("") +
  scale_y_continuous(trans = "log2")
p + geom_boxplot (aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year~.)

#arrange matching boxplots next to each other, coloured by year
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))


#faceted smooth density plots
#smooth density plots - area under each curve adds  to 1
gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  group_by(group) %>%
  summarize(n = n()) %>%
  knitr ::kable()

#smooth density plots - variable counts on y-axis
p <- gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) +
  facet_grid(year ~.)

#add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia", 
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

#reorder factor levels 
gapminder <- gapminder %>%
  mutate(group = factor(group,levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

#must redefine 'p' with the new gapminder object
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")
# stacked denisty plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~.)

#Weighted stacked density plot 
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill= group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid (year ~.)