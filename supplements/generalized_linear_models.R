setwd("C:/path/working_directory") # change to the location of the data file
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

# Data alteration for models 1-3
Data_h1_h2 <- Data[Data$women_obs != 0, ]

# Data alteration for models 5-7
Data_f <- Data[Data$men_obs != 0, ]

#####
#
# Model 1 (GLMM) and model 1.1 (GLM)
#
#####

# Model 1 - generalized linear mixed effects model
model_1 <- glmer(men_dance ~ polygyny + (1|culture),
                 data = Data_h1_h2,
                 family = binomial(link = "logit"),
                 na.action = na.omit)
summary(model_1)

# Model 1.1 - generalized linear model
model_1.1 <- glm(men_dance ~ polygyny,
                 data = Data_h1_h2,
                 family = binomial(link = "logit"),
                 na.action = na.omit)
summary(model_1.1)

# sjPlot for model 1 and model 1.1
tab_model(model_1, model_1.1, 
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "polygyny"),
          dv.labels = c("Model 1: men dance", 
                        "Model 1.1: men dance"),
          title = "Comparison of model 1 (GLMM) and model 1.1 (GLM)")
# The last coefficient for model 1.1 is R² Tjur, the sjPlot displays it incorrectly.

#####
#
# Model 2 (GLMM) and model 2.1 (GLM)
#
#####

# Model 2 - generalized linear mixed effects model
model_2 <- glmer(men_dance ~ soc_strat + (1|culture),
                 data = Data_h1_h2,
                 family = binomial(link = "logit"),
                 na.action = na.omit)
summary(model_2)

# Model 2.1 - generalized linear model
model_2.1 <- glm(men_dance ~ soc_strat,
                 data = Data_h1_h2,
                 family = binomial(link = "logit"),
                 na.action = na.omit)
summary(model_2.1)

# sjPlot for model 2 and model 2.1
tab_model(model_2, model_2.1, 
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "social stratification"),
          dv.labels = c("Model 2: men dance", 
                        "Model 2.1: men dance"),
          title = "Comparison of model 2 (GLMM) and model 2.1 (GLM)")
# The last coefficient for model 2.1 is R² Tjur, the sjPlot displays it incorrectly.

#####
#
# Model 3 (GLMM) and model 3.1 (GLM)
#
#####

# Model 3 - generalized linear mixed effects model
model_3 <- glmer(men_dance ~ soc_strat + polygyny + (1|culture),
                 data = Data_h1_h2,
                 family = binomial(link = "logit"),
                 na.action = na.omit)
summary(model_3)

# Model 3.1 - generalized linear model
model_3.1 <- glm(men_dance ~ soc_strat + polygyny,
                 data = Data_h1_h2,
                 family = binomial(link = "logit"),
                 na.action = na.omit)
summary(model_3.1)

# sjPlot for model 3 and model 3.1
tab_model(model_3, model_3.1, 
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
          dv.labels = c("Model 3: men dance", 
                        "Model 3.1: men dance"),
          title = "Comparison of model 3 (GLMM) and model 3.1 (GLM)")
# The last coefficient for model 3.1 is R² Tjur, the sjPlot displays it incorrectly.

#####
#
# Model 4 (GLMM) and model 4.1 (GLM)
#
#####

# Model 4 - generalized linear mixed effects model
model_4 <- glmer(men_dance ~ marriage_org + (1|culture),
                 data = Data,
                 family = binomial(link = "logit"),
                 na.action = na.omit)
summary(model_4)

# Model 4.1 - generalized linear model
model_4.1 <- glm(men_dance ~ marriage_org,
                 data = Data,
                 family = binomial(link = "logit"),
                 na.action = na.omit)
summary(model_4.1)

# sjPlot for model 4 and model 4.1
tab_model(model_4, model_4.1, 
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "social stratification"),
          dv.labels = c("Model 4: men dance", 
                        "Model 4.1: men dance"),
          title = "Comparison of model 4 (GLMM) and model 4.1 (GLM)")
# The last coefficient for model 4.1 is R² Tjur, the sjPlot displays it incorrectly.

#####
#
# Model 5 (GLMM) and model 5.1 (GLM)
#
#####

# Model 5 - generalized linear mixed effects model
model_5 <- glmer(women_dance ~ prec_pred + temp_pred + (1|culture),
                 data = Data_f,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_5)

# Model 5.1 - generalized linear model
model_5.1 <- glm(women_dance ~ prec_pred + temp_pred,
                 data = Data_f,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_5.1)

# sjPlot for model 5 and model 5.1
tab_model(model_5, model_5.1, 
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
                          "temperature predictability"),
          dv.labels = c("Model 5: women dance", 
                        "Model 5.1: women dance"),
          title = "Comparison of model 5 (GLMM) and model 5.1 (GLM)")
# The last coefficient for model 5.1 is R² Nagelkerke, the sjPlot displays it incorrectly.

#####
#
# Model 6 (GLMM) and model 6.1 (GLM)
#
#####

# Model 6 - generalized linear mixed effects model
model_6 <- glmer(women_dance ~ prec_pred + (1|culture),
                 data = Data_f,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_6)

# Model 6.1 - generalized linear model
model_6.1 <- glm(women_dance ~ prec_pred,
                 data = Data_f,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_6.1)

# sjPlot for model 6 and model 6.1
tab_model(model_6, model_6.1, 
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "precipitation predictability"),
          dv.labels = c("Model 6: women dance", 
                        "Model 6.1: women dance"),
          title = "Comparison of model 6 (GLMM) and model 6.1 (GLM)")
# The last coefficient for model 6.1 is R² Nagelkerke, the sjPlot displays it incorrectly.

#####
#
# Model 7 (GLMM) and model 7.1 (GLM)
#
#####

# Model 7 - generalized linear mixed effects model
model_7 <- glmer(women_dance ~ temp_pred + (1|culture),
                 data = Data_f,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_7)

# Model 7.1 - generalized linear model
model_7.1 <- glm(women_dance ~ temp_pred,
                 data = Data_f,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_7.1)

# sjPlot for model 7 and model 7.1
tab_model(model_7, model_7.1, 
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "temperature predictability"),
          dv.labels = c("Model 7: women dance", 
                        "Model 7.1: women dance"),
          title = "Comparison of model 7 (GLMM) and model 7.1 (GLM)")
# The last coefficient for model 7.1 is R² Nagelkerke, the sjPlot displays it incorrectly.

#####
#
# Model 8 (GLMM) and model 8.1 (GLM)
#
#####

# Model 8 - generalized linear mixed effects model
model_8 <- glmer(women_dance ~ prec_pred + temp_pred + (1|culture),
                 data = Data,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_8)

# Model 8.1 - generalized linear model
model_8.1 <- glm(women_dance ~ prec_pred + temp_pred,
                 data = Data,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_8.1)

# sjPlot for model 8 and model 8.1
tab_model(model_8, model_8.1, 
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
                          "temperature predictability"),
          dv.labels = c("Model 8: women dance", 
                        "Model 8.1: women dance"),
          title = "Comparison of model 8 (GLMM) and model 8.1 (GLM)")
# The last coefficient for model 8.1 is R² Nagelkerke, the sjPlot displays it incorrectly.

#####
#
# Model 9 (GLMM) and model 9.1 (GLM)
#
#####

# Model 9 - generalized linear mixed effects model
model_9 <- glmer(women_dance ~ prec_pred + (1|culture),
                 data = Data,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_9)

# Model 9.1 - generalized linear model
model_9.1 <- glm(women_dance ~ prec_pred,
                 data = Data,
                 family = binomial(link = "probit"),
                 na.action = na.omit)
summary(model_9.1)

# sjPlot for model 9 and model 9.1
tab_model(model_9, model_9.1, 
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "precipitation predictability"),
          dv.labels = c("Model 9: women dance", 
                        "Model 9.1: women dance"),
          title = "Comparison of model 9 (GLMM) and model 9.1 (GLM)")
# The last coefficient for model 9.1 is R² Nagelkerke, the sjPlot displays it incorrectly.

#####
#
# Model 10 (GLMM) and model 10.1 (GLM)
#
#####

# Model 10 - generalized linear mixed effects model
model_10 <- glmer(women_dance ~ temp_pred + (1|culture),
                  data = Data,
                  family = binomial(link = "probit"),
                  na.action = na.omit)
summary(model_10)

# Model 10.1 - generalized linear model
model_10.1 <- glm(women_dance ~ temp_pred,
                  data = Data,
                  family = binomial(link = "probit"),
                  na.action = na.omit)
summary(model_10.1)

# sjPlot for model 10 and model 10.1
tab_model(model_10, model_10.1, 
          CSS = list(css.table = "+font-family: Cambria;"),
          p.style = "numeric",
          emph.p = TRUE,
          collapse.ci = FALSE,
          collapse.se = TRUE,
          string.est = "Estimate",
          string.ci = "Conf. Int.",
          string.p = "p",
          pred.labels = c("(Intercept)", 
                          "temperature predictability"),
          dv.labels = c("Model 10: women dance", 
                        "Model 10.1: women dance"),
          title = "Comparison of model 10 (GLMM) and model 10.1 (GLM)")
# The last coefficient for model 10.1 is R² Nagelkerke, the sjPlot displays it incorrectly.
