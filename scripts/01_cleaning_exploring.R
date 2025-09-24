# Script 01: Cleaning & Exploring
# Author: Shelita Smith
# Date: September 2025
# Goals: cleaned, ML ready data set

# 0 Setup

raw_train <- read.csv("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Titanic\\data\\raw_data\\train.csv")
## 0.1 raw_train notes ####
  ### full name + titles in name, missing ages -w2d?, some decimal ages - round down to full age, missing cabin info, decimal fare, some missing embarked info; ticket number numletter mix
  ### outcome/ variable of interest: "Survived"
  ### name doesn't matter
raw_test <- read.csv("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Titanic\\data\\raw_data\\test.csv")
## 0.2 raw_test notes ####
  ### full names + title in name, "Survived" removed, missing ages, some decimal ages, 

# 1 Cleaning ####
cl_train <- clean_names(raw_train)

## summary stats
head(cl_train)
summary(cl_train)
## 177 missing ages; 80%/ 714 data points remaining if removed
## change fare to xx.x
## columns to remove: name, ticket #, cabin, age (replaced w/ age_months), sib_sp & parch (replaced with family size/ traveling alone)

cl_train <- cl_train %>%
  mutate(
    age_months = ifelse(!is.na(age), round(age * 12), NA),
    minor = ifelse(!is.na(age), age_months <= 228, NA),
    family_size = sib_sp + parch + 1,
    traveling_alone = (sib_sp == 0 & parch == 0),
    fare = round(fare, 1)
  ) %>%
  select(-name, -ticket, -cabin, -age, -sib_sp, -parch)

summary(cl_train)

# 2 Exploration ####
  ## statistical tests: any existing relationships: gender/survival(t-test), age/survival, minor/survival (t-tes), class/survival (ANOVA), fare/survival (make some categories(?)), embarked/survival (ANOVA),  family_size/survival (ANOVA), traveling_alone/survival (t-test)
  ### make age group categories: <19, 20-45, 46-64, 65 - 75, 76-80
  ### regression: age + gender/ survival, age + class/survival, gender +class/ survival, age + gender + class/ survival


  