# This adjusts plot images to an appealing size.
options(repr.plot.width = 6, repr.plot.height = 6)

# Loading packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Loading data
life_exp <- read.csv("/Users/gautham/Desktop/Projects/R/LifeExpectancyGapAnalysis/LifeExpectancyGapAnalysis-main/UNdata_Export_20230906_031548405.csv")

# Viewing first few rows.
head(life_exp)

# Subsetting data
sub_set_data <- life_exp  %>% 
  filter(Year == "2000-2005") %>% 
  select(Country.or.Area, Subgroup, Value) %>% 
  spread(Subgroup, Value)

# Viewing first few rows.
head(sub_set_data)

# Plotting male and female life expectancy.
ggplot(sub_set_data, aes(x = Male, y = Female)) + geom_point()

# Adding geom_abline and changing the scale of axes of the previous plots.
# Plotting male and female life expectancy
ggplot(sub_set_data, aes(x = Male, y = Female)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_x_continuous(limits = c(35,85)) +
  scale_y_continuous(limits = c(35,85))

# Adding labels to previous plot.
ggplot(sub_set_data, aes(x = Male, y = Female)) +
  geom_point(colour = "white", fill = "red", shape = 21, alpha = .50, size = 5) +
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_x_continuous(limits = c(35,85)) +
  scale_y_continuous(limits = c(35,85)) +
  labs(title = "Life Expectancy at Birth by Country",
       subtitle ="Period: 2000-2005 ",
       caption = "Source: United Nations Statistics Division(UNSD)",
       x = "Males",
       y = "Females")

# Subseting data to obtain countries of interest.
top_male <- sub_set_data %>%
              arrange(Male-Female) %>%
              head(3)
top_female <- sub_set_data %>% 
              arrange(Female-Male) %>% 
              head(3)

# Adding text to the previous plot to label countries of interest
ggplot(sub_set_data, aes(x = Male, y = Female)) +
  geom_point(colour = "white", fill = "red", shape = 21, alpha = .50, size = 5) +
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_x_continuous(limits = c(35,85)) +
  scale_y_continuous(limits = c(35,85)) +
  labs(title = "Life Expectancy at Birth by Country",
       subtitle ="Period: 2000-2005 ",
       caption = "Source: United Nations Statistics Division",
       x = "Males",
       y = "Females") +
  geom_text(data = top_male, size=3) +
  geom_text(data = top_female, size=3) +
  theme_bw()

# Subsetting, mutating and reshaping the life expectancy data.
sub_set_data2 <- life_expectancy %>% 
  filter(Year %in% c("1985-1990", "2000-2005")) %>% 
  mutate(Sub_Year = paste(Subgroup, Year, sep="_")) %>% 
  mutate(Sub_Year = gsub("-", "_", Sub_Year)) %>% 
  select(-Subgroup, -Year) %>% 
  spread(Sub_Year, Value) %>% 
  mutate(diff_Female = Female_2000_2005 - Female_1985_1990, 
         diff_Male   = Male_2000_2005 - Male_1985_1990)

# Viewing first few rows.
head(sub_set_data2)

# Scaling axis and adding labels.
ggplot(sub_set_data2, aes(x = diff_Male, y = diff_Female, label = Country.or.Area))+
  geom_point(colour = "white", fill = "red", shape = 21, alpha = .55, size = 5) +
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  scale_x_continuous(limits = c(-25,25))+
  scale_y_continuous(limits = c(-25,25))+
  labs(title="Life Expectancy at Birth by Country in Years",
       subtitle="Difference between 1985-1990 and 2000-2005",
       caption="Source: United Nations Statistics Division",
       x = "Males",
       y = "Females")+
  theme_bw()

# Adding an hline and vline to previous plots.
ggplot(sub_set_data2, aes(x = diff_Male, y = diff_Female, label = Country.or.Area))+
  geom_point(colour = "white", fill = "red", shape = 21, alpha = .55, size = 5) +
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  scale_x_continuous(limits = c(-25,25))+
  scale_y_continuous(limits = c(-25,25))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_vline(xintercept = 0, linetype = 2)+
  labs(title = "Life Expectancy at Birth by Country",
       subtitle = "Difference between 1985-1990 and 2000-2005.",
       caption = "Source: United Nations Statistics Division",
       x = "Males",
       y = "Females")+
  theme_bw()

# Subseting data to obtain countries of interest
top <- sub_set_data2 %>% 
         arrange(diff_Male+diff_Female) %>% 
         head(3)
bottom <- sub_set_data2 %>% 
            arrange(-(diff_Male+diff_Female)) %>% 
            head(3)

# Adding text to the previous plot to label countries of interest
ggplot(sub_set_data2, aes(x = diff_Male, y = diff_Female, label = Country.or.Area), guide = FALSE)+
  geom_point(colour = "white", fill = "red", shape = 21, alpha = .55, size = 5)+
  geom_abline(intercept = 0, slope = 1, linetype = 2)+
  scale_x_continuous(limits = c(-25,25))+
  scale_y_continuous(limits = c(-25,25))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_vline(xintercept = 0, linetype = 2)+
  labs(title = "Life Expectancy at Birth by Country",
       subtitle = "Difference between 1985-1990 and 2000-2005.",
       caption = "Source: United Nations Statistics Division",
       x = "Males",
       y = "Females")+
  geom_text(data = top, size = 3)+
  geom_text(data = bottom, size = 3)+
  theme_bw()

