---
title: "Decades of Change: Tracking the Journey from Classrooms to Dreams - A Comparative Dashboard on the Out of School Rate (1900-2020)"
author: "Iarla Monaghan"
date: 04/10/2024
format: html
execute:
 echo: false
 warning: false
 message: false
--- 
# Introduction
This dashboard will tell a detailed story of how UNICEF indicators, specifically out-of-school rates of children during the 20th and 21st Centuries, have evolved. There will also be a comparison of the observation value to social and economic factors such as sex, population, and life expectancy. We aim to see the observation value reduced to zero by the end of this century. This visualization and analysis endeavor to illuminate the progress made so far and highlight the areas where more focused efforts are necessary to achieve our goal of universal education access.
---
```{r setup, include=FALSE}
library(tidyverse) 

```{r}
# Assuming the dataset is in a CSV file named 'unicef_indicator_1.csv'
unicef_data <- read_csv("unicef_indicator_1.csv")

```{r}
# Install packages if you haven't already
install.packages(c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata", "countrycode"))
```{r}
library(tidyverse)
library(sf)
library(rnaturalearth)
library(countrycode)

```{r}
unicef_data <- read_csv("unicef_indicator_1.csv")

```{r}
unicef_data <- unicef_data %>%
  mutate(country_code = countrycode(country, "country.name", "iso3c"))

```{r}
avg_data <- unicef_data %>%
  group_by(country_code) %>%
  summarise(avg_obs_value = mean(obs_value, na.rm = TRUE))

```{r}
world <- ne_countries(scale = "medium", returnclass = "sf")

world_avg_data <- world %>%
  left_join(avg_data, by = c("iso_a3" = "country_code"))

```{r plot-map, fig.width=10, fig.height=8}
ggplot(data = world_avg_data) +
  geom_sf(aes(fill = avg_obs_value), color = NA) +
  scale_fill_viridis_c(name = "Average Obs Value", na.value = "white") +
  labs(title = "Average UNICEF Indicator: Out-of-School Rates per Country",
fill = "Avg Obs Value") +
  theme_minimal()
```

# Caption  
**This map outlines the average observation value per country over the entire period being investigated. It can be seen that in the more economically developed countries, the observation value is lower and that in the less developed countries, the observation value is higher. By the end of this century, we need to see the observation value at zero so every child gets access to a proper education.**


```{r}
unicef_data <- read_csv("unicef_indicator_1.csv")

```{r}
average_obs_values <- unicef_data %>%
  group_by(sex) %>%
  summarize(Average_Obs_Value = mean(obs_value, na.rm = TRUE))

```{r}
ggplot(average_obs_values, aes(x = sex, y = Average_Obs_Value, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Observation Value by Sex",
       x = "Sex",
       y = "Average Observation Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")  # Optional: Adds color
```
# Caption
This Bar Chart provides an outline of the observation value per the two sexes over the entire period under review. As would be expected the female sex has a higher out-of-school rate due to the old traditional values that a woman's place is in the home as males would be the first choice for education. As previously mentioned we need to by the end of the century need to have an opportunity  of all females have a proper education. 

```{r}
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("ggnewscale", quietly = TRUE)) install.packages("ggnewscale")
if (!requireNamespace("treemapify", quietly = TRUE)) install.packages("treemapify")

```{r}
library(ggplot2)
library(ggnewscale)
library(treemapify)



# Read the dataset
```{r}
unicef_metadata <- read_csv("unicef_metadata.csv")

```{r}
unicef_metadata <-unicef_metadata %>%
  mutate(country_code = countrycode(country, "country.name", "iso3c"))

```{r}
average_population_per_country <- unicef_metadata %>%
  group_by(country_code) %>%
  summarise(Average_Population = mean(Population, na.rm = TRUE))

```{r}
world <- ne_countries(scale = "medium", returnclass = "sf")

world_avg_data <- world %>%
  left_join(average_population_per_country, by = c("iso_a3" = "country_code"))

```{r}
ggplot(average_population_per_country, aes(area = Average_Population, label = country_code, fill = Average_Population)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE) +
  scale_fill_viridis_c() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
  ggtitle("Average Population per Country")
```
# Caption
This treemap shows each country's average population over the entire period under review. As we have seen the less developed countries may have a high population but their out-of-school rate is still high. If we compare to Figure 1 the more economically developed countries have a high population and  a low indicator rate and low economically developed countries have high populations and alarmingly high indicator rates.

```{r}
unicef_metadata <- read.csv("unicef_metadata.csv")

```{r}
life_expectancy_trend <- unicef_metadata %>%
  select(year, Lifeexpectancy) %>%
  group_by(year) %>%
  summarise(AverageLifeExpectancy = mean(Lifeexpectancy, na.rm = TRUE))

```{r}
ggplot(life_expectancy_trend, aes(x = year, y = AverageLifeExpectancy)) +
  geom_line() +  # Creates the line plot
  geom_point() +  # Adds points to each data point
  labs(title = "Life Expectancy Over Time",
       x = "year",
       y = " Average Life Expectancy") +
  theme_minimal()  # Applies a minimalistic theme
```

# Caption
This time series shows the world's life
expectancy per year from 1960-2020. If we go
pack figure 1 the observation value is decreased in modern times and I believe this has led to a higher life expectancy in this period. If a child is more educated they are more likely to livelonger because their county has better healthservice.

```{r}
unicef_metadata <- read.csv("unicef_metadata.csv")

```{r}
ggplot(unicef_metadata, aes(x = country, y = GNI)) +
  geom_point() +  # Adds the points for the scatter plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +  # Rotates x-axis labels for readability
  labs(title = "GNI per Country",
       x = "country",
       y = "GNI") +
  theme_minimal()  # Applies a minimalistic theme
```

# Caption
This chart shows the GNI for 2015  per country over the period under review. If we go back to visualisation 1 we see that countries with very
low GNI have a very alarmingly higher
observation rate. The power of education in the growth of the economy is massive. Countries with high-class universities churn out graduates who demand higher wages which attracts foreign direct investment which increases government income which leads to better public services.
---
# Analysis
The goal of this dashboard was to show that the out-of-school rate of children has decreased as a whole across the world.
Yes, that is true on the raw data but there is a large amount of work to down in the less developed countries. If we go look
at Figure 1 the map shows that particularly on the African Continent there is still a large average out-of-school rate. In
Figure 2 we see that more females are not being given access to education which is a concern. Figures 3,4 + 5 are economic
and social factors that show the power of the indicator. The population in Figure 3 shows once again the developed v
developing countries concept we can see that the raw data is more alarming when we view the population of the high
out-of-school rate countries. The life expectancy figure is used to show the power of education in modern times. The final
GNI Visualisation is once again a sign of the power of education in the growth of an economy.
---
# Conclusion
In the end, the goal of this dashboard was to highlight both the power of education and the alarming number of children
who do not get access to quality education. It is my firm belief that education is key to economic and social success which
the figures and analysis of this dashboard have proven. "Mól an Oige agus tiocfaidh siad." This is an Irish language proverb
which states in English that if you praise the young they will follow which is apt for this indicator.
