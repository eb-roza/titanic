# Titanic Data
# Last Edited: 12/10/2020

#####----- Load Libraries -----

library(tidyverse)

#####----- Read Files ----- 

data_train <- read.csv("C:\\Users\\edbro\\Documents\\kaggle_data\\titanic\\train.csv") %>% 
  janitor::clean_names()

data_test <- read.csv("C:\\Users\\edbro\\Documents\\kaggle_data\\titanic\\test.csv") %>% 
  janitor::clean_names()



#####----- Describe Data (Preliminary) ----

Hmisc::describe(data_train)

# 891 passengers spanning 3 classes and ranging in age from babies - 88.  
# ... Family sizes range from travelling alone to with 7 family members.
# ... Fares range from free to $500 and may make up for the missing cabin data.
# ... Passengers embarked from three cities, with most embarking from "S"
# ... There are roughly twice as many men on board as there are women.

# Missing Data
# - 177 missing ages
# - 687 missing cabins

# 342 associates survived (38%)
data_train %>% 
  group_by(survived) %>% 
  summarise(count = n()) %>% 
  mutate(total = sum(count),
         perc = count/total) %>% 
  ungroup()


#####----- Clean Data ----

data_train_cln <- data_train %>% 
  mutate(is_sibling = ifelse(sib_sp > 0, "Yes", "No" )) %>% 
  mutate(is_parent = ifelse(parch > 0, "Yes", "No" )) %>% 
  mutate(survived = as.factor(survived))

#####----- Summarise Data ----

# Of the passengers that survived, 68% are female (233) and 31% are male (109), 
# ... despite the fact that there are nearly twice as many male passengers than female.
data_train %>% 
  filter(survived == 1) %>% 
  group_by(sex) %>% 
  summarise(count = n()) %>% 
  mutate(total = sum(count),
         perc = count/total) %>% 
  ungroup()

data_train_cln %>% 
  ggplot(aes(x=sex, y=..count.., fill=as.factor(survived))) +
  geom_bar()


# SIBLINGS
# There doesn't appear to be a difference in survival rate for passengers traveling alone
# ... vs. travelling with siblings/spouses; however a higher rate of passengers traveling
# ... alone tend to be in third class.
data_train_cln %>% 
  ggplot(aes(x=is_sibling, y=..count.., fill=as.factor(survived))) +
  geom_bar() + 
  facet_wrap(~as.factor(pclass))

data_train_cln %>% 
  ggplot(aes(x=is_sibling, y=..count.., fill=as.factor(survived))) +
  geom_bar() 



# PARENTS/CHILDREN
# There doesn't appear to be a difference in survival rate for passengers traveling with children 
# ... or parents vs. travelling without.
# While family size is slightly larger in third class, most passengers travel alone or with one other
# ... parent/child, consistent across classes.

data_train_cln %>% 
  ggplot(aes(x=is_parent, y=..count.., fill=as.factor(survived))) +
  geom_bar() + 
  facet_wrap(~as.factor(pclass))

# Larger families in third class among females
# Does this impact their survival? Otherwise consistent
data_train_cln %>% 
  ggplot(aes(x=sex, y=..count.., fill=as.factor(parch))) +
  geom_bar(position = "fill") +
  facet_wrap(~pclass)


# CLASS
# Survival increases with class
data_train_cln %>% 
  ggplot(aes(x=pclass, y=..count.., fill=survived)) +
  geom_bar(position = "fill") 


# FARE
# Majority of fares were low among those that did not survive
data_train_cln %>% 
  ggplot(aes(x=fare, y=..count..)) +
  geom_density() +
  facet_wrap(~survived)




##### ----- FIT BINOMIAL MODEL -----

model <- glm(formula=survived~pclass + sex + fare,
             data=data_train, family = binomial)

summary(model)


test_pred <- predict(model,data_test, type="response")

test_pred <- ifelse(test_pred>0.5,1,0)

table(test_pred)


# Model Accuracy from Kaggle: 76%


##### ----- CREATE OUTPUT -----

output <- bind_cols(data_test, test_pred) %>% 
  select(1,12) %>% 
  rename(PassengerId = passenger_id) %>% 
  rename(Survived = 2)

write.csv(output, "C:\\Users\\edbro\\Documents\\gender_submission.csv")
write_excel_csv(output, "C:\\Users\\edbro\\Documents\\gender_submission.csv")
