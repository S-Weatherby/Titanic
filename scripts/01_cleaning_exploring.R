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

# 1 Cleaning & Exploring ####
cl_train <- clean_names(raw_train)

## summary stats
head(cl_train)
summary(cl_train)

## 177 missing ages; 80%/ 714 data points remaining if removed
## change fare to xx.x ??
## columns to remove: name, ticket #, cabin, age (replaced w/ age_months), sib_sp & parch (replaced with family size/ traveling alone)

summary(cl_train$embarked) # 2 NAs

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

## 2.1 Descriptive Stats ####
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

# Basic survival rate
overall_survival <- cl_train %>%
  summarise(
    total = n(),
    survived = sum(survived, na.rm = TRUE),
    survival_rate = round(survived/total * 100, 1)
  )

# Passenger class
survival_by_class <- cl_train %>%
  group_by(pclass) %>%
  summarise(
    total = n(),
    survived = sum(survived, na.rm = TRUE),
    survival_rate = round(survived/total * 100, 1),
    .groups = 'drop'
  )

# Sex
survival_by_sex <- cl_train %>%
  group_by(sex) %>%
  summarise(
    total = n(),
    survived = sum(survived, na.rm = TRUE),
    survival_rate = round(survived/total * 100, 1),
    .groups = 'drop'
  )

# Age group (excluding NAs)
survival_by_age <- cl_train %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(
    total = n(),
    survived = sum(survived, na.rm = TRUE),
    survival_rate = round(survived/total * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(survival_rate))

# Family size categories
survival_by_family <- cl_train %>%
  group_by(family_category) %>%
  summarise(
    total = n(),
    survived = sum(survived, na.rm = TRUE),
    survival_rate = round(survived/total * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(survival_rate))

# Cross-tabulations interaction effects
# Class + Sex (the big story)
survival_class_sex <- cl_train %>%
  group_by(pclass, sex) %>%
  summarise(
    total = n(),
    survived = sum(survived, na.rm = TRUE),
    survival_rate = round(survived/total * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(pclass, desc(survival_rate))

# Age group + Sex
survival_age_sex <- cl_train %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group, sex) %>%
  summarise(
    total = n(),
    survived = sum(survived, na.rm = TRUE),
    survival_rate = round(survived/total * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(age_group, desc(survival_rate))

# Results
overall_survival
survival_by_class
survival_by_sex
survival_by_age
survival_by_family
survival_class_sex
survival_age_sex

### 2.1.1 Interpretations ####
# 1. Overall survival: ~38%
# 2. Class matters: 1st class >> 2nd class > 3rd class  
# 3. Sex dominates: Women ~74% vs Men ~19%
# 4. Children had higher survival than adults
# 5. Small families did better than large families or solo travelers
# 6. The interaction between class and sex shows the strongest pattern

## 2.2 t-tests ####
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

## 2.3 ANOVA tests ####
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

## 2.4 Test Results ####
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

## 2.5 Interpretations ####

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
    pclass = as.factor(pclass),
    family_category = as.factor(family_category),
    traveling_alone = as.factor(traveling_alone),
    # interaction vars
    age_class = interaction(age_group, pclass),
    age_sex = interaction(age_group, sex),
    class_sex = interaction(pclass, sex),
    age_class_sex = interaction(age_group, pclass, sex),
    age_travel = interaction(age_group, traveling_alone),
    minor_sex = interaction(minor, sex),
    minor_class = interaction(minor, pclass),
    minor_class = interaction(minor,sex,pclass)
)

head(cl_train)
summary(cl_train)

# 4 Modeling ####
## simple log regression (4.2), interactions log regressions (4.4), RF (4.6), GBM(4.8)

## 4.1 Simple Logistic Regression ####
# simple_model <- glm(survived ~ sex + minor + traveling_alone + pclass + embarked + age_group, 
#               family = binomial, data = cl_train)
# 
# summary(simple_model)

### 4.1.1 Simple Model Eval ####
# simple_predictions <- predict(simple_model, type = "response")
# simple_auc <- auc(simple_model$y, simple_predictions)  # 177 age/agre_group NAs; model$y contains the actual outcomes used
# simple_predicted_class <- ifelse(simple_predictions > 0.5, 1, 0)
# simple_confusion <- table(Predicted = simple_predicted_class, Actual = simple_model$y)
# simple_accuracy <- mean(simple_predicted_class == simple_model$y)
# simple_aic <- AIC(simple_model)
# # rm(simple_predictions, simple_auc, simple_predicted_class, simple_confusion, simple_accuracy, simple_aic)

# ## 4.1.2 Simple Model Plot
# # DF
# simple_model_data <- tidy(simple_model, conf.int = TRUE)
# rm(simple_model_data)
# 
# # Plot
# simple_model_plot <- ggplot(simple_model_data, aes(x = estimate, y = term)) +
#   geom_point() +
#   geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   labs(title = "Simple Model Coefficients", 
#        x = "Coefficient Estimate", 
#        y = "Variables")
# 
# # Plot Save
# ggsave("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Titanic\\outputs\\plots\\simple_model_plot.png", 
#        plot = simple_model_plot, width = 8, height = 6)
# rm(simple_model_plot)

## 4.2 Cleaned Simple  Logistic Regression ####
### excluding minor due to minor/age_group multicollinearity
### excluding embarked due to 2 missing values causing perfect separation issues

### 4.2.1 Cleaned Simple Model ####
clean_simple_model <- glm(survived ~ sex + age_group + pclass + traveling_alone, 
                   family = binomial, data = cl_train)

summary(clean_simple_model)

### 4.2.1 Simple Model Eval ####
simple_predictions <- predict(clean_simple_model, type = "response")
simple_auc <- auc(clean_simple_model$y, simple_predictions)  # 177 age/agre_group NAs; model$y contains the actual outcomes used
simple_predicted_class <- ifelse(simple_predictions > 0.5, 1, 0)
simple_confusion <- table(Predicted = simple_predicted_class, Actual = clean_simple_model$y)
simple_accuracy <- mean(simple_predicted_class == clean_simple_model$y)
simple_aic <- AIC(clean_simple_model)

## 4.2.2 Clean Simple Model Plot
# DF
clean_simple_model_data <- tidy(clean_simple_model, conf.int = TRUE)

table(cl_train$survived, cl_train$age_group)

# Plot
clean_simple_model_plot <- ggplot(clean_simple_model_data, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Simple Model Coefficients", 
       x = "Coefficient Estimate", 
       y = "Variables")

print(clean_simple_model_plot)

# Plot Save
ggsave("C:\\Users\\sheli\\OneDrive\\Documents\\DA Practice GitHub\\Titanic\\outputs\\plots\\simple_model_plot.png", 
       plot = clean_simple_model_plot, width = 8, height = 6)

## 4.3 Simple Model Interpretations ####
## may need to create simple age grouping category: minor vs adult; currently like the breakout of age groups

## 4.4 Interactions Logistic Regression ####
### current interactions: ####
# cl_train <- cl_train %>%
# mutate(
#   sex = as.factor(sex),
#   embarked = as.factor(embarked),
#   age_group = as.factor(age_group),
#   family_category = as.factor(family_category),
#   # interaction vars
#   age_class = interaction(age_group, pclass),
#   age_sex = interaction(age_group, sex),
#   class_sex = interaction(pclass, sex),
#   age_class_sex = interaction(age_group, pclass, sex),
#   age_travel = interaction(age_group, traveling_alone),
#   minor_sex = interaction(minor, sex),
#   minor_class = interaction(minor, pclass),
#   minor_sex_class = interaction(minor,sex,pclass)

## gonna exclude minor as age_group captures minor status

## adding traveling_alone interactions
cl_train <- cl_train %>% 
  mutate(
    sex_travel = interaction(sex, traveling_alone),
    class_travel = interaction(pclass, traveling_alone)
)

### 4.3.1 Interactions Log Regression ####
## gonna exclude minor interactions as age_group captures minor status
interaction_model <- glm(survived ~ 
  sex + pclass + age_group + traveling_alone + age_sex + age_class + age_class_sex + class_sex + sex_travel + class_travel + age_travel,
                    family = binomial, data = cl_train)

### 4.3.2 Interactions Log Model Eval ####
interaction_predictions <- predict(interaction_model, type = "response")
interaction_auc <- auc(interaction_model$y, interaction_predictions)
interaction_predicted_class <- ifelse(interaction_predictions > 0.5, 1, 0)
interaction_confusion <- table(Predicted = interaction_predicted_class, Actual = interaction_model$y)
interaction_accuracy <- mean(interaction_predicted_class == interaction_model$y)
interaction_aic <- AIC(interaction_model)

### 4.3.3 Interactions Log Plot
interaction_model_data <- tidy(interaction_model, conf.int = TRUE)

interaction_model_plot <- ggplot(interaction_model_data, aes(x = estimate, y = term)) +
  geom_point() + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Interaction Model Coefficients")

ggsave("outputs/plots/interaction_model_plot.png", plot = interaction_model_plot)

## 4.5 Interactions Interpretations ####
### warnings: Warning messages:
  #1: glm.fit: algorithm did not converge 
  #2: glm.fit: fitted probabilities numerically 0 or 1 occurred #
## gonna use more simple/basc interactions: age/sex/class

### 4.5.1 Simple Interactions Log Model ####
simple_interaction_model <- glm(survived ~ 
                           sex + pclass + age_group + traveling_alone + age_sex + age_class + class_sex,
                           family = binomial, data = cl_train)

### 4.5.2 Simple Interactions Log Model Eval ####
simple_interaction_predictions <- predict(simple_interaction_model, type = "response")
simple_interaction_auc <- auc(simple_interaction_model$y, simple_interaction_predictions)
simple_interaction_predicted_class <- ifelse(simple_interaction_predictions > 0.5, 1, 0)
simple_interaction_confusion <- table(Predicted = simple_interaction_predicted_class, Actual = simple_interaction_model$y)
simple_interaction_accuracy <- mean(simple_interaction_predicted_class == simple_interaction_model$y)
simple_interaction_aic <- AIC(simple_interaction_model)

### 4.3.3 Simple Interactions Log Plot
simple_interaction_model_data <- tidy(simple_interaction_model, conf.int = TRUE)

simple_interaction_model_plot <- ggplot(simple_interaction_model_data, aes(x = estimate, y = term)) +
  geom_point() + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Interaction Model Coefficients")

ggsave("outputs/plots/simple_interaction_model_plot.png", plot = simple_interaction_model_plot)


