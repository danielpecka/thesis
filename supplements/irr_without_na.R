setwd("C:/path/working_directory") # change to the location of the data file
getwd()

if(!require(dplyr)){install.packages("dplyr")}
if(!require(irr)){install.packages("irr")}

library(dplyr)
library(irr)

# This R script contains code for calculation of Light's kappa for variables,
# that were coded from eHRAF, in the form before data transformation. 

# Missing data are not included in the analysis.

# Load the data
IRR <- read.csv("irr_data.csv", header = TRUE)
head(IRR)

#####
# Cohen's kappa for all coder pairs, var. dance 
# presence (dance_pres)
#####

# 1st and 2nd coder
dance_pres_1_2 <- IRR %>%
  select(dance_pres_c1, dance_pres_c2)
dance_pres_1_2 <- dance_pres_1_2[dance_pres_1_2$dance_pres_c1 != 99,]
dance_pres_1_2 <- dance_pres_1_2[dance_pres_1_2$dance_pres_c2 != 99,]

dance_pres_a <- kappa2(dance_pres_1_2,
                       weight = "unweighted")
dance_pres_a

# 2nd and 3rd coder
dance_pres_2_3 <- IRR %>%
  select(dance_pres_c2, dance_pres_c3)
dance_pres_2_3 <- dance_pres_2_3[dance_pres_2_3$dance_pres_c2 != 99,]
dance_pres_2_3 <- dance_pres_2_3[dance_pres_2_3$dance_pres_c3 != 99,]

dance_pres_b <- kappa2(dance_pres_2_3,
                       weight = "unweighted")
dance_pres_b

# 3rd and 1st coder
dance_pres_3_1 <- IRR %>%
  select(dance_pres_c3, dance_pres_c1)
dance_pres_3_1 <- dance_pres_3_1[dance_pres_3_1$dance_pres_c3 != 99,]
dance_pres_3_1 <- dance_pres_3_1[dance_pres_3_1$dance_pres_c1 != 99,]

dance_pres_c <- kappa2(dance_pres_3_1,
                       weight = "unweighted")
dance_pres_c

# Kappa for all coder pairs, variable dance_pres
dance_pres_all <- c(dance_pres_a$value, dance_pres_b$value, dance_pres_c$value)
dance_pres_all

### Average for the entire variable according to Light (1971)
mean(dance_pres_all) 

# Kappa for dance_pres variable is ca. 0.464, which is according to
# Landis and Koch (1977) a moderate agreement between coders (0.41-0.60)
# according to Krippendorff (1980) results of following analysis should be 
# discounted as the value is below 0.67

#####
# Cohen's kappa for all coder pairs, var. dancer's 
# gender (gender)
#####

# 1st and 2nd coder
gender_1_2 <- IRR %>%
  select(gender_c1, gender_c2)
gender_1_2 <- gender_1_2[gender_1_2$gender_c1 != 99,]
gender_1_2 <- gender_1_2[gender_1_2$gender_c2 != 99,]

gender_a <- kappa2(gender_1_2,
                   weight = "unweighted")
gender_a

# 2nd and 3rd coder
gender_2_3 <- IRR %>%
  select(gender_c2, gender_c3)
gender_2_3 <- gender_2_3[gender_2_3$gender_c2 != 99,]
gender_2_3 <- gender_2_3[gender_2_3$gender_c3 != 99,]

gender_b <- kappa2(gender_2_3,
                   weight = "unweighted")
gender_b

# 3rd and 1st coder
gender_3_1 <- IRR %>%
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

# Kappa for gender variable is ca. 0.540, which is according to
# Landis and Koch (1977) a moderate agreement between coders (0.41-0.60)
# according to Krippendorff (1980) results of following analysis should be 
# discounted as the value is below 0.67

#####
# Cohen's kappa for all coder pairs, var. observers's 
# gender (observers)
#####

# 1st and 2nd coder
observers_1_2 <- IRR %>%
  select(observers_c1, observers_c2)
observers_1_2 <- observers_1_2[observers_1_2$observers_c1 != 99,]
observers_1_2 <- observers_1_2[observers_1_2$observers_c2 != 99,]

observers_a <- kappa2(observers_1_2,
                      weight = "unweighted")
observers_a

# 2nd and 3rd coder
observers_2_3 <- IRR %>%
  select(observers_c2, observers_c3)
observers_2_3 <- observers_2_3[observers_2_3$observers_c2 != 99,]
observers_2_3 <- observers_2_3[observers_2_3$observers_c3 != 99,]

observers_b <- kappa2(observers_2_3,
                      weight = "unweighted")
observers_b

# 3rd and 1st coder
observers_3_1 <- IRR %>%
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

# Kappa for observers variable is ca. 0.243, which is according to
# Landis and Koch (1977) a fair agreement between coders (0.21-0.40)
# according to Krippendorff (1980) results of following analysis should be 
# discounted as the value is below 0.67

#####
# Cohen's kappa for all coder pairs, var. location of families 
# (together or apart) when dance takes place (location)
#####

# 1st and 2nd coder
location_1_2 <- IRR %>%
  select(location_c1, location_c2)
location_1_2 <- location_1_2[location_1_2$location_c1 != 99,]
location_1_2 <- location_1_2[location_1_2$location_c2 != 99,]

location_a <- kappa2(location_1_2,
                     weight = "unweighted")
location_a

# 2nd and 3rd coder
location_2_3 <- IRR %>%
  select(location_c2, location_c3)
location_2_3 <- location_2_3[location_2_3$location_c2 != 99,]
location_2_3 <- location_2_3[location_2_3$location_c3 != 99,]

location_b <- kappa2(location_2_3,
                     weight = "unweighted")
location_b

# 3rd and 1st coder
location_3_1 <- IRR %>%
  select(location_c3, location_c1)
location_3_1 <- location_3_1[location_3_1$location_c3 != 99,]
location_3_1 <- location_3_1[location_3_1$location_c1 != 99,]

location_c <- kappa2(location_3_1,
                     weight = "unweighted")
location_c

# Kappa for all coder pairs, variable location
location_all <- c(location_a$value, location_b$value, location_c$value)
location_all

### Average for the entire variable according to Light (1971)
mean(location_all) 

# Kappa for location variable is ca. 0.326, which is according to
# Landis and Koch (1977) a fair agreement between coders (0.21-0.40)
# according to Krippendorff (1980) results of following analysis should be 
# discounted as the value is below 0.67
