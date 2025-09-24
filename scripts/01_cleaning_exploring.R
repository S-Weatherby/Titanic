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

### create tables directory/folder
if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables", recursive = TRUE)
}

write.csv(cl_train, "C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Titanic\\outputs\\tables\\cl_train.csv", row.names = FALSE)

# 2 Exploration on Train ####
  ## statistical tests: any existing relationships: gender/survival(t-test), age/survival, minor/survival (t-test), class/survival (ANOVA), fare/survival (make some categories(?)), embarked/survival (ANOVA), family_size/survival (ANOVA), traveling_alone/survival (t-test)
  ### make age group categories(?): <19, 20-45, 46-64, 65 - 75, 76-80 | 228, 229-540, 541-768, 769-900, 901-960
  ### make fare categories (?): 
  ### regression: age + gender/ survival, age + class/survival, gender +class/ survival, age + gender + class/ survival

## age groups
cl_train <- cl_train %>% 
  mutate(
    age_group = case_when(
      is.na(age_months) ~ NA_character_,
      age_months <= 228 ~ "minor",
      age_months >= 229 & age_months <= 540 ~ "young_adult", 
      age_months >= 541 & age_months <= 768 ~ "adult", 
      age_months >= 769 & age_months <= 900 ~ "mature_adult",
      age_months > 900 ~ "elder"
    )
  )
## 2.1 t-tests ####
### survival/ gender, minor, traveling_alone

gender_survival_test <- t.test(survived ~ sex, data = cl_train)

gender_test_results <- list(
  test_type = "t-test",
  variables = "survived ~ sex",
  t_statistic = gender_survival_test$statistic,
  p_value = gender_survival_test$p.value,
  female_survival_rate = gender_survival_test$estimate[1],
  male_survival_rate = gender_survival_test$estimate[2],
  ci_lower = gender_survival_test$conf.int[1],
  ci_upper = gender_survival_test$conf.int[2]
)

minor_survival_test <- t.test(survived ~ minor, data = cl_train)

minor_test_results <- list (
  test_type = "t-test",
  variables = "survived ~ minor",
  t_statistic = minor_survival_test$statistic,
  p_value = minor_survival_test$p.value,
  non_minor_survival_rate = minor_survival_test$estimate[1],  # FALSE 
  minor_survival_rate = minor_survival_test$estimate[2],      # TRUE 
  ci_lower = minor_survival_test$conf.int[1],
  ci_upper = minor_survival_test$conf.int[2]
)

traveling_alone_test_results <- list(
  test_type = "t-test", 
  variables = "survived ~ traveling_alone",
  t_statistic = traveling_alone_survival_test$statistic,
  p_value = traveling_alone_survival_test$p.value,
  with_family_survival_rate = traveling_alone_survival_test$estimate[1],  # FALSE 
  traveling_alone_survival_rate = traveling_alone_survival_test$estimate[2], # TRUE 
  ci_lower = traveling_alone_survival_test$conf.int[1],
  ci_upper = traveling_alone_survival_test$conf.int[2]
)

## 2.2 ANOVA tests ####
### survival/ class, embarked, family size

class_survival_anova <- aov(survived ~ pclass, data = cl_train)
class_summary <- summary(class_survival_anova)

class_test_results <- list(
  test_type = "ANOVA",
  variables = "survived ~ pclass", 
  f_statistic = class_summary[[1]][["F value"]][1],
  p_value = class_summary[[1]][["Pr(>F)"]][1],
  df = class_summary[[1]][["Df"]],
  sum_sq = class_summary[[1]][["Sum Sq"]]
)

embarked_survival_anova <- aov(survived ~ embarked, data = cl_train)
embarked_summary <- summary(embarked_survival_anova)

embarked_test_results <- list(
  test_type = "ANOVA",
  variables = "survived ~ embarked", 
  f_statistic = embarked_summary[[1]][["F value"]][1],
  p_value = embarked_summary[[1]][["Pr(>F)"]][1],
  df = embarked_summary[[1]][["Df"]],
  sum_sq = embarked_summary[[1]][["Sum Sq"]]
)

family_category_anova <- aov(survived ~ family_category, data = cl_train)
family_category_summary <- summary(family_category_anova)

family_category_test_results <- list(
  test_type = "ANOVA",
  variables = "survived ~ family_category", 
  f_statistic = family_category_summary[[1]][["F value"]][1],
  p_value = family_category_summary[[1]][["Pr(>F)"]][1],
  df = family_category_summary[[1]][["Df"]],
  sum_sq = family_category_summary[[1]][["Sum Sq"]]
)

age_group_survival_anova <- aov(survived ~ age_group, data = cl_train)
age_group_summary <- summary(age_group_survival_anova)

age_group_test_results <- list(
  test_type = "ANOVA",
  variables = "survived ~ age_group", 
  f_statistic = age_group_summary[[1]][["F value"]][1],
  p_value = age_group_summary[[1]][["Pr(>F)"]][1],
  df = age_group_summary[[1]][["Df"]],
  sum_sq = age_group_summary[[1]][["Sum Sq"]]
)

## 2.3 Test Results ####
### all tests results 
all_survival_test_results <- data.frame(
  Test = c("Gender", "Minor Status", "Traveling Alone", "Class", "Embarked", "Family Size", "Age Group"),
  Method = c("t-test", "t-test", "t-test", "ANOVA", "ANOVA", "ANOVA", "ANOVA"),
  Statistic = c(
    round(gender_survival_test$statistic, 3),
    round(minor_survival_test$statistic, 3), 
    round(traveling_alone_survival_test$statistic, 3),
    round(class_summary[[1]][["F value"]][1], 3),
    round(embarked_summary[[1]][["F value"]][1], 3),
    round(family_size_summary[[1]][["F value"]][1], 3),
    round(age_group_summary[[1]][["F value"]][1], 3)
  ),
  P_Value = c(
    gender_survival_test$p.value,
    minor_survival_test$p.value,
    traveling_alone_survival_test$p.value,
    class_summary[[1]][["Pr(>F)"]][1],
    embarked_summary[[1]][["Pr(>F)"]][1], 
    family_size_summary[[1]][["Pr(>F)"]][1],
    age_group_summary[[1]][["Pr(>F)"]][1]
  ),
  Significant = c(
    ifelse(gender_survival_test$p.value < 0.05, "Yes", "No"),
    ifelse(minor_survival_test$p.value < 0.05, "Yes", "No"),
    ifelse(traveling_alone_survival_test$p.value < 0.05, "Yes", "No"),
    ifelse(class_summary[[1]][["Pr(>F)"]][1] < 0.05, "Yes", "No"),
    ifelse(embarked_summary[[1]][["Pr(>F)"]][1] < 0.05, "Yes", "No"),
    ifelse(family_size_summary[[1]][["Pr(>F)"]][1] < 0.05, "Yes", "No"),
    ifelse(age_group_summary[[1]][["Pr(>F)"]][1] < 0.05, "Yes", "No")
  )
)

# Round p-values 
all_survival_test_results$P_Value <- ifelse(all_survival_test_results$P_Value < 0.001, "<0.001", 
                                  round(all_survival_test_results$P_Value, 4))

## 2.4 Interpretations ####

## family size wasn't significant; gonna try family categories

cl_train <- cl_train %>% 
  mutate(
    family_category = case_when(
      family_size == 1 ~ "alone",
      family_size %in% 2:4 ~ "small_family", 
      family_size > 4 ~ "large_family"
    )
  )

family_category_anova <- aov(survived ~ family_category, data = cl_train)
family_category_summary <- summary(family_category_anova)

family_category_test_results <- list(
  test_type = "ANOVA",
  variables = "survived ~ family_size", 
  f_statistic = family_category_summary[[1]][["F value"]][1],
  p_value = family_category_summary[[1]][["Pr(>F)"]][1],
  df = family_category_summary[[1]][["Df"]],
  sum_sq = family_category_summary[[1]][["Sum Sq"]]
)

all_survival_test_results <- data.frame(
  Test = c("Gender", "Minor Status", "Traveling Alone", "Class", "Embarked", "Family Size", "Age Group", "Family Category"),
  Method = c("t-test", "t-test", "t-test", "ANOVA", "ANOVA", "ANOVA", "ANOVA", "ANOVA"),
  Statistic = c(
    round(gender_survival_test$statistic, 3),
    round(minor_survival_test$statistic, 3), 
    round(traveling_alone_survival_test$statistic, 3),
    round(class_summary[[1]][["F value"]][1], 3),
    round(embarked_summary[[1]][["F value"]][1], 3),
    round(family_size_summary[[1]][["F value"]][1], 3),
    round(age_group_summary[[1]][["F value"]][1], 3),
    round(family_category_summary[[1]][["F value"]][1], 3)
  ),
  P_Value = c(
    gender_survival_test$p.value,
    minor_survival_test$p.value,
    traveling_alone_survival_test$p.value,
    class_summary[[1]][["Pr(>F)"]][1],
    embarked_summary[[1]][["Pr(>F)"]][1], 
    family_size_summary[[1]][["Pr(>F)"]][1],
    age_group_summary[[1]][["Pr(>F)"]][1],
    family_category_summary[[1]][["Pr(>F)"]][1]
  ),
  Significant = c(
    ifelse(gender_survival_test$p.value < 0.05, "Yes", "No"),
    ifelse(minor_survival_test$p.value < 0.05, "Yes", "No"),
    ifelse(traveling_alone_survival_test$p.value < 0.05, "Yes", "No"),
    ifelse(class_summary[[1]][["Pr(>F)"]][1] < 0.05, "Yes", "No"),
    ifelse(embarked_summary[[1]][["Pr(>F)"]][1] < 0.05, "Yes", "No"),
    ifelse(family_size_summary[[1]][["Pr(>F)"]][1] < 0.05, "Yes", "No"),
    ifelse(age_group_summary[[1]][["Pr(>F)"]][1] < 0.05, "Yes", "No"),
    ifelse(family_category_summary[[1]][["Pr(>F)"]][1] < 0.05, "Yes", "No")
  )
)

# Round p-values 
all_survival_test_results$P_Value <- ifelse(all_survival_test_results$P_Value < 0.001, "<0.001", 
                                            round(all_survival_test_results$P_Value, 4))

write.csv(all_survival_test_results, "C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Titanic\\outputs\\tables\\all_survival_tests_results.csv", row.names = FALSE)

### FAMILY CATEGORY SIGNIFICANT !!!!

# 3 Feature Engineering ####
## interaction variables: age + sex/ survival, age + class/survival, sex +class/ survival, age + sex + class/ survival
## cat vars to factors

cl_train <- cl_train %>%
  mutate(
    sex = as.factor(sex),
    embarked = as.factor(embarked),
    age_group = as.factor(age_group),
    family_category = as.factor(family_category),
    # interaction vars
    age_class = interaction(age_group, pclass),
    age_sex = interaction(age_group, sex),
    class_sex = interaction(pclass, sex),
    age_class_sex = interaction(age_group, pclass, sex),
    age_travel = interaction(age_group, traveling_alone),
    minor_sex = interaction(minor, sex),
    minor_class = interaction(minor, pclass)
)

# 4 Modeling ####
## simple log regressions, interactions log regressions, RF,

## 4.1 Simple Logistic Regression ####
simple_model <- glm(survived ~ sex + minor + traveling_alone + pclass + embarked + age_group, 
              family = binomial, data = cl_train)

summary(simple_model)
