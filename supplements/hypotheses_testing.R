setwd("D:/Documents/thesis R/separated_supplements") # change to the location of the data file
getwd()

if(!require(lmerTest)){install.packages("lmerTest")}
if(!require(car)){install.packages("car")}
if(!require(sjPlot)){install.packages("sjPlot")}
if(!require(dplyr)){install.packages("dplyr")}

library(lmerTest)
library(car)
library(sjPlot)
library(dplyr)

# Load complete dataset
Data <- read.csv("hyp_testing_data.csv", header = TRUE, na.strings = "N/A")
head(Data)

# Models in this R script are numbered the same way they are
# numbered in the text of the thesis (and tables, see below).

# Removal of observations (rows) where women do not observe 
# men while dancing (they are the receivers of the courtship 
# signal) - relevant for the first two hypotheses.
Data_h1_h2 <- Data[Data$women_obs != 0, ]

#####
# Hypothesis 1:
# In polygynous societies, the custom of men dancing is 
# present in nuptial rituals more often than in 
# non-polygynous societies.
#####

##
# Influence of polygyny on whether men dance in nuptial rituals.
##

model_1 <- glmer(men_dance ~ polygyny + (1|culture),
                 data = Data_h1_h2,
                 family = binomial(link = "logit"),
                 na.action = na.omit)
summary(model_1)

#####
# Hypothesis 2:
# In stratified polygynous societies, the custom of men 
# dancing is less frequent in nuptial rituals compared 
# to non-stratified polygynous societies.
#####

##
# Influence of social stratification on whether men dance in
# nuptial rituals.
##

model_2 <- glmer(men_dance ~ soc_strat + (1|culture),
                 data = Data_h1_h2,
                 family = binomial(link = "logit"),
                 na.action = na.omit)
summary(model_2)

##
# Model including polygyny as a fixed effect.
##

model_3 <- glmer(men_dance ~ soc_strat + polygyny + (1|culture),
                 data = Data_h1_h2,
                 family = binomial(link = "logit"),
                 na.action = na.omit)
summary(model_3)

vif(model_3)

# sjPlot for the three models relevant for first and 
# second hypotheses
tab_model(model_1, model_2, model_3, 
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "polygyny", 
                          "social stratification"),
          dv.labels = c("Model 1: men dance", 
                        "Model 2: men dance", 
                        "Model 3: men dance"),
          title = "Effects of polygyny (FE) and social stratification (FE) on whether men dance (culture = RE, grouping var.)")

#####
# Hypothesis 3:
# Men dance in nuptial rituals more when the degree of exogamy
# is higher in society.
#####

##
# Influence of the marriage organization on whether men dance
# in nuptial rituals.
##

model_4 <- glmer(men_dance ~ marriage_org + (1|culture),
                 data = Data,
                 family = binomial(link = "logit"),
                 na.action = na.omit)

summary(model_4)

# sjPlot for the third hypothesis model
tab_model(model_4, 
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "marriage organization"),
          dv.labels = c("Model 4: men dance"),
          title = "Effects of marriage organization (FE) on whether men dance (culture = RE, grouping var.)")

#####
# Hypothesis 4:
# The custom of women dancing as a part of nuptial rituals 
# is less frequent in cultures located in harsher environments.
#####

# Data with omitted observations (rows) where men do not 
# observe women while dancing. (Original is called "Data".)
Data_f <- Data[Data$men_obs != 0, ] # f stands for filtered

###
# Shapiro-Wilk test was performed to find out whether the 
# distribution of prec_pred (precipitation predictability)
# and temp_pred (temperature predictability) variables is 
# normal. This was done for both original data (Data) and 
# data with omitted values (Data_f).
shapiro.test(Data$prec_pred)
shapiro.test(Data_f$prec_pred)
shapiro.test(Data$temp_pred)
shapiro.test(Data_f$temp_pred)
# Results tell us that the null hypothesis saying the 
# distribution is not normal should be rejected.
# Therefore, the link = "probit" argument is used for the 
# models for fourth hypothesis.
###

# TEMPERATURE * PRECIPITATION MODEL (with data omission)
h4_interaction_a <- glmer(women_dance ~ prec_pred * temp_pred + (1|culture),
                          data = Data_f,
                          family = binomial(link = "probit"),
                          na.action = na.omit)
summary(h4_interaction_a)

# TEMPERATURE * PRECIPITATION MODEL (without data omission)
h4_interaction_b <- glmer(women_dance ~ prec_pred * temp_pred + (1|culture),
                          data = Data,
                          family = binomial(link = "probit"),
                          na.action = na.omit)
summary(h4_interaction_b)

# Variance inflation factor for both of the models
vif(h4_interaction_a)
vif(h4_interaction_b)

# sjPlot for models with interaction
tab_model(h4_interaction_a, h4_interaction_b, 
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "precipitation predictability", 
                          "temperature predictability",
                          "prec. pred. * temp. pred."),
          dv.labels = c("men dance (w/ data omission)", 
                        "men dance (w/o data ommision)"),
          title = "Models with interaction: effects of precipitation and temperature predictability (FEs) on whether men dance (culture = RE, grouping var.)")

# Calculation of correlation between the precipitation and 
# temperature predictability (with data omission).
cor.test(Data_f$prec_pred, Data_f$temp_pred,
         method = "pearson",
         alternative = "greater",
         conf.level = 0.95,
         na.rm = TRUE)

# Calculation of correlation between the precipitation and 
# temperature predictability (without data omission).
cor.test(Data$prec_pred, Data$temp_pred,
         method = "pearson",
         alternative = "greater",
         conf.level = 0.95,
         na.rm = TRUE)

#
# Models calculated with the data omission.
#

# TEMPERATURE + PRECIPITATION MODEL (with data omission)
model_5 <- glmer(women_dance ~ prec_pred + temp_pred + (1|culture),
                 data = Data_f,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_5)

vif(model_5)

# PRECIPITATION MODEL (with data omission)
model_6 <- glmer(women_dance ~ prec_pred + (1|culture),
                 data = Data_f,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_6)

# TEMPERATURE MODEL (with data omission)
model_7 <- glmer(women_dance ~ temp_pred + (1|culture),
                 data = Data_f,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_7)

#
# Models calculated without the data omission.
#

# TEMPERATURE + PRECIPITATION MODEL (without data omission)
model_8 <- glmer(women_dance ~ prec_pred + temp_pred + (1|culture),
                 data = Data,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_8)

vif(model_8)

# PRECIPITATION MODEL (without data omission)
model_9 <- glmer(women_dance ~ prec_pred + (1|culture),
                 data = Data,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_9)

# TEMPERATURE MODEL (without data omission)
model_10 <- glmer(women_dance ~ temp_pred + (1|culture),
                  data = Data,
                  family = binomial(link = "probit"),
                  na.action = na.omit)
summary(model_10)

# sjPlot for models 5-7; relevant for the fourth hypothesis
tab_model(model_5, model_6, model_7,
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          linebreak = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "precipitation predictability", 
                          "temperature predictability"),
          dv.labels = c("Model 5: women dance (w/ d. o.)", 
                        "Model 6: women dance (w/ d. o.)", 
                        "Model 7: women dance (w/ d. o.)"),
          title = "Effects of precipitation and temperature predictability (FEs) on whether women dance (culture = RE, grouping var.; with data omission)")

# sjPlot for models 8-10; relevant for the fourth hypothesis
tab_model(model_8, model_9, model_10,
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          linebreak = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "precipitation predictability", 
                          "temperature predictability"),
          dv.labels = c("Model 8: women dance (w/o d. o.)", 
                        "Model 9: women dance (w/o d. o.)", 
                        "Model 10: women dance (w/o d. o.)"),
          title = "Effects of precipitation and temperature predictability (FEs) on whether women dance (culture = RE, grouping var.; without data omission)")

