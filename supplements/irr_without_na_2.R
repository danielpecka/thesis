setwd("C:/path/working_directory") # change to the location of the data file
getwd()

if(!require(dplyr)){install.packages("dplyr")}
if(!require(irr)){install.packages("irr")}

library(dplyr)
library(irr)

# This R script contains code for calculation of Light's kappa for two 
# variables (observers' and dancers' gender) that were coded from eHRAF. 
# Data for this analysis were modified from the original code and the
# results are recalculated. Specifically, values 2 and 3 for mentioned
# variables were merged into one. Reasoning is described in the
# text of the thesis. 

# Missing data are not included in the analysis.

# Load the data
IRR2 <- read.csv("irr_data_2.csv", header = TRUE)
head(IRR2)

#####
# Cohen's kappa for all coder pairs, var. dancer's 
# gender (gender)
#####

# 1st and 2nd coder
gender_1_2 <- IRR2 %>%
  select(gender_c1, gender_c2)
gender_1_2 <- gender_1_2[gender_1_2$gender_c1 != 99,]
gender_1_2 <- gender_1_2[gender_1_2$gender_c2 != 99,]

gender_a <- kappa2(gender_1_2,
                   weight = "unweighted")
gender_a

# 2nd and 3rd coder
gender_2_3 <- IRR2 %>%
  select(gender_c2, gender_c3)
gender_2_3 <- gender_2_3[gender_2_3$gender_c2 != 99,]
gender_2_3 <- gender_2_3[gender_2_3$gender_c3 != 99,]

gender_b <- kappa2(gender_2_3,
                   weight = "unweighted")
gender_b

# 3rd and 1st coder
gender_3_1 <- IRR2 %>%
  select(gender_c3, gender_c1)
gender_3_1 <- gender_3_1[gender_3_1$gender_c3 != 99,]
gender_3_1 <- gender_3_1[gender_3_1$gender_c1 != 99,]

gender_c <- kappa2(gender_3_1,
                   weight = "unweighted")
gender_c

# Kappa for all coder pairs, variable gender
gender_all <- c(gender_a$value, gender_b$value, gender_c$value)
gender_all

### Average for the entire variable according to Light (1971)
mean(gender_all) 

# Kappa for gender variable is ca. 0.738, which is according to
# Landis and Koch (1977) a substantial agreement between coders (0.41-0.60)
# according to Krippendorff (1980) results of following analysis should
# NOT be discounted as the value is above 0.67

#####
# Cohen's kappa for all coder pairs, var. observers's 
# gender (observers)
#####

# 1st and 2nd coder
observers_1_2 <- IRR2 %>%
  select(observers_c1, observers_c2)
observers_1_2 <- observers_1_2[observers_1_2$observers_c1 != 99,]
observers_1_2 <- observers_1_2[observers_1_2$observers_c2 != 99,]

observers_a <- kappa2(observers_1_2,
                      weight = "unweighted")
observers_a

# 2nd and 3rd coder
observers_2_3 <- IRR2 %>%
  select(observers_c2, observers_c3)
observers_2_3 <- observers_2_3[observers_2_3$observers_c2 != 99,]
observers_2_3 <- observers_2_3[observers_2_3$observers_c3 != 99,]

observers_b <- kappa2(observers_2_3,
                      weight = "unweighted")
observers_b

# 3rd and 1st coder
observers_3_1 <- IRR2 %>%
  select(observers_c3, observers_c1)
observers_3_1 <- observers_3_1[observers_3_1$observers_c3 != 99,]
observers_3_1 <- observers_3_1[observers_3_1$observers_c1 != 99,]

observers_c <- kappa2(observers_3_1,
                      weight = "unweighted")
observers_c

# Kappa for all coder pairs, variable observers
observers_all <- c(observers_a$value, observers_b$value, observers_c$value)
observers_all

### Average for the entire variable according to Light (1971)
mean(observers_all) 

# Kappa for observers variable is ca. 0.802, which is according to
# Landis and Koch (1977) a substantial agreement between coders (0.21-0.40)
# according to Krippendorff (1980) results of following analysis should
# NOT be discounted as the value is above 0.67
