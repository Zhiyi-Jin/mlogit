library(tidyverse)
library(glmnet)
library(kableExtra)
library(tidymodels)
library(stargazer)
library(essurvey)
library(nnet)
library(modelr)

ess_uk <- import_country(
  country = "United Kingdom",
  rounds = 8
)

#clean variables
ess_uk <- ess_uk %>% 
  mutate(prtvtbg = as_factor(prtvtbgb),
        party = fct_recode(prtvtbg, 
                           NULL = "Not applicable",
                           NULL = "Refusal",
                           NULL = "Don't know",
                           NULL = "No answer")) %>% 
  mutate(eisced = as_factor(eisced),
        educ = fct_recode(eisced, 
                          NULL = "Other",
                          NULL = "Refusal",
                          NULL = "Don't know",
                          NULL = "No answer")) %>%
  mutate(gndr = as_factor(gndr),
         gender = fct_recode(gndr, 
                             NULL = "No answer")) %>%
  mutate(impsafe = as_factor(impsafe),
          impsafe = fct_recode(impsafe, 
                               NULL = "Refusal",
                               NULL = "Don't know",
                               NULL = "No answer"))

#recode -prtvtbgb- as Conservative, Labour, Liberal Democrat, UK Independence Party, and others.
#recode -eisced- as -college- with No and Yes
ess_uk <- ess_uk %>% 
  mutate(party = fct_collapse(party,
                              others = c("Scottish National Party",
                                         "Plaid Cymru",
                                         "Green Party",
                                         "Other",
                                         "Ulster Unionist Party (nir)",
                                         "Democratic Unionist Party (nir)",
                                         "Sinn Féin (nir)",
                                         "Social Democratic and Labour Party (nir)",
                                         "Alliance Party (nir)",
                                         "Traditional Unionist (nir)",
                                         "Independent(s) (nir)",
                                         "Green Party (nir)",
                                         "People Before Profit Alliance (nir)",
                                         "Other (nir)"))) %>% 
  mutate(college = fct_collapse(educ,
                                No = c("ES-ISCED I , less than lower secondary",
                                       "ES-ISCED II, lower secondary",
                                       "ES-ISCED IIIb, lower tier upper secondary",
                                       "ES-ISCED IIIa, upper tier upper secondary",
                                       "ES-ISCED IV, advanced vocational, sub-degree",
                                       "Not possible to harmonise into ES-ISCED"),
                                Yes = c("ES-ISCED V1, lower tertiary education, BA level",
                                        "ES-ISCED V2, higher tertiary education, >= MA level"))) %>% 
  mutate(college = fct_relevel(college, "No"))
  
#fit models
m1 <- multinom(relevel(party, ref = "Labour") ~ gender + agea + college, data = ess_uk)
m2 <- multinom(relevel(party, ref = "Labour") ~ gender + agea + college + impsafe, data = ess_uk)

#present one table with both models and their estimated coefficients
stargazer(m1, m2, header = FALSE, type = "html", no.space = TRUE,
          title = "Party Choice in the UK. Multinomial logistic regression")

#generate data to predict
pred_dt <- expand_grid(impsafe = c("Very much like me", "Like me", "Somewhat like me", "A little like me", "Not like me", "Not like me at all"), college = c("Yes", "No"), gender = c("Male", "Female"), agea = median(ess_uk$agea, na.rm = TRUE))

#make predictions and add to the table
preds <- predict(m2, newdata = pred_dt, "probs") %>% 
  as_tibble() %>% 
  bind_cols(pred_dt)

#change to long format
preds_long <- preds %>% 
  pivot_longer(cols = c("Labour", "Conservative", "Liberal Democrat", "others", "UK Independence Party"),
               names_to = "party",
               values_to = "preds")

#present also a graph with the predicted probabilites for the second model
theme_set(theme_light())

ggplot(preds_long, aes(x = preds, y = impsafe, color = party)) +
  geom_point(alpha = 0.75) + 
  facet_wrap(college ~ gender) +
  labs(title = "Figure: Party Choice in the UK",
       y = "Importance of secure and safe surroundings",
       x = "Predicted probability vote choice") 

