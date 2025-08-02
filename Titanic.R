install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

titanic <- read_csv("train.csv")

head(titanic, 10)
str(titanic)
summary(titanic)

titanic <- titanic %>%
  mutate(Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age))

titanic$Embarked[is.na(titanic$Embarked)] <- "S"

SurvivalBySex <- titanic %>%
  group_by(Sex) %>%
  summarise(SurvivalRate = mean(Survived))

SurvivalByPclass <- titanic %>%
  group_by(Pclass) %>%
  summarise(SurvivalRate = mean(Survived))

titanic <- titanic %>% 
  mutate(AgeGroup = case_when(
    Age < 12 ~ "Child",
    Age < 18 ~ "Teen",
    Age < 40 ~ "Adult",
    Age < 60 ~ "Middle",
    TRUE ~ "Senior"
  ))

SurvivalBySex$SurvivalRate <- SurvivalBySex$SurvivalRate * 100
SurvivalByPclass$SurvivalRate <- SurvivalByPclass$SurvivalRate * 100
  

ggplot(SurvivalBySex, aes(x = Sex, y = SurvivalRate)) +
  geom_col(fill = "steelblue", color = "black", width = 0.5) +
  labs(title = "Passenger Proportion by Gender", y = "Percentage")

ggplot(SurvivalByPclass, aes(x = Pclass, y = SurvivalRate)) +
  geom_col(fill = "steelblue", color = "black", width = 0.5) +
  labs(title = "Passenger Proportion by Class", x = "Passenger Class", y = "Percentage")

ggplot(titanic, aes(x = Age, fill = factor(Survived))) + 
  geom_histogram(bins=30, alpha=0.6, position="identity") +
  labs(title = "Age distribution by Survival", fill = "Survived")



