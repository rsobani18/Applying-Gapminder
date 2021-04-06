library(tidyverse)
library(dslabs)
data(gapminder)
head(gapminder)
#compare infant mortality in Sri Lanka and Turkey
gapminder %>%
   filter(year == 2015 & country %in% c("Sri Lanka","Turkey")) %>%
   select(country, infant_mortality)

#basic scatter plot of life expectancy versus fertility 
ds_theme_set() #set plot theme
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

#add colour as a continent
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

#facet by continent and year
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col= continent)) +
  geom_point() +
  facet_grid(continent ~ year)

#facet by year only
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col= continent)) +
  geom_point() +
  facet_grid(.~year)

#facet by year, plots wrapped into multiple rows
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")

gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)


#Time Series Plots
#Single time series: scatter plot of US fertility rate by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_point()

#line plot of US fertility rate by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

#Multiple time series: 
#line plot fertility time series for two countries - only one line(incorrect)
countries <- c("South Korea", "Germany")
gapminder %>%
  filter(country %in% countries) %>%
  ggplot(aes(year, fertility)) +
  geom_line()

#line plot fertility time series for two countries - one line per country
gapminder %>%
  filter(country %in% countries) %>%
  ggplot(aes(year, fertility, group = country)) +
  geom_line()

#fertility time series for two coutnries - lines coloured by country
gapminder %>%
  filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col= country)) +
  geom_line()

#adding text labels to plot
#life expectancy time series  - lines coloured by country and labelled - no legend
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line()
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme (legend.position = "none")