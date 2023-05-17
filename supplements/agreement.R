setwd("C:/path/working_directory") # change to the location of the data file
getwd()

AGR <- read.csv("agreement_data.csv", header = TRUE, na.string = "N/A")
head(AGR)

library(stats)

attach(AGR)

# Correlation between ordinal scale for document evaluation
# and the degree of agreement between coders (none, partial, full).

cor.test(doc_eval_scale, dance_pres_sc, 
         method = "kendall",
         alternative = "two.sided",
         conf.level = 0.95,
         na.rm = TRUE)

cor.test(doc_eval_scale, gender_sc, 
         method = "kendall",
         alternative = "two.sided",
         conf.level = 0.95,
         na.rm = TRUE)

cor.test(doc_eval_scale, observers_sc, 
         method = "kendall",
         alternative = "two.sided",
         conf.level = 0.95,
         na.rm = TRUE)

cor.test(doc_eval_scale, location_sc, 
         method = "kendall",
         alternative = "two.sided",
         conf.level = 0.95,
         na.rm = TRUE)

# Tau for all results is very low and p-values are high,
# it appears that there is no correlation between the 
# the agreement among coders (none, partial, full)
# and evaluation of documents (coded as average: 3, 3.4, 4, 
# good: 4.5, perfect: 5).
