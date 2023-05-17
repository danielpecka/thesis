setwd("C:/path/working_directory") # change to the location of the data file
getwd()

if(!require(stats)){install.packages("stats")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(gt)){install.packages("gt")}

library(stats)
library(dplyr)
library(ggplot2)
library(gt)

# This .R file contains code for all charts that are included
# in the thesis, excluding the tables summarizing models
# for the analysis which are part of the hypotheses_testing.R

# Load font and set theme (applies to all ggplot2 graphs)
windowsFonts(Cambria = windowsFont("Cambria"))
theme_set(theme_bw(base_family= "Cambria"))

##################################
##                              ##
#  Document evaluation bar chart #
##                              ##
##################################

DE_data <- read.csv("doc_eval_data.csv",
                    header = TRUE)
head(DE_data)

# Calculate the count of each evaluation category
tab_doc_eval_ex <- aggregate(pub_name ~ doc_eval,
                             data = DE_data,  
                             FUN = length)

# Rename the columns accordingly
names(tab_doc_eval_ex)[1] <- "Document_evaluation"
names(tab_doc_eval_ex)[2] <- "Count"

# Order the data by the percentage of each category
tab_doc_eval_ex <- arrange(tab_doc_eval_ex, desc(Count))
tab_doc_eval_ex # Display the table

# Calculate the percentage of each evaluation category
tab_doc_eval_pc <- aggregate(pub_name ~ doc_eval,
                             data = DE_data,  
                             FUN = function(x) length(x)/length(DE_data$pub_name)*100)

# Rename the columns accordingly
names(tab_doc_eval_pc)[1] <- "Document_evaluation"
names(tab_doc_eval_pc)[2] <- "Percentage"

# Order the data by the percentage of each category
tab_doc_eval_pc <- arrange(tab_doc_eval_pc, desc(Percentage))
tab_doc_eval_pc # Display the table

# Define the colors for each value
colors <- c("none" = "#a0a7aa",
            "3" = "#e1f4ff", 
            "3.4" = "#e1f4ff", 
            "4" = "#b8e6fe", 
            "4.5" = "#7cd0fe", 
            "5" = "#3fbafd")

# Generate a bar chart showing the percentages with the specified colors
bar_doc_eval <- ggplot(data = tab_doc_eval_pc,
                       aes(x = Document_evaluation,
                           y = Percentage,
                           fill = Document_evaluation)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(values = colors) +
  labs(title = "The percentage of each evaluation in the dataset",
       x = "Document evaluation", 
       y = "Percentage of evaluations") +
  theme(axis.text.x = element_text(hjust = 0.5, size = 11),
        axis.text.y = element_text(size = 11),
        panel.border = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "transparent"),
        legend.position = "none") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%"),
                y = Percentage + 2),
            family = "Cambria",
            size = 4,
            position = position_stack(vjust = 0.5))
bar_doc_eval

###########################################################
##                                                       ##
#   Distribution of cultures among subregions in the      #
#   dataset before omitting missing data for individual   #
#   models and subregions and regions table               #
##                                                       ##
###########################################################

# Load the data
Data <- read.csv("hyp_testing_data.csv",
                 header = TRUE,
                 na.strings = "N/A")
head(Data)

################################
# Regions and subregions table #
################################

# Create a table of subregions each with the region they belong to
reg_subreg <- unique(Data[, c("region", "subregion")])

# Rename the table columns
names(reg_subreg)[1] <- "Region"
names(reg_subreg)[2] <- "Subregion"
reg_subreg # Display the table

# Create a gt_table object in order to save the table
region_subregion <- gt(reg_subreg)

# Render the table with a title and export it
region_subregion |>
  tab_header(
    title = "Locations of represented cultures",
    subtitle = "Regions and subregions in the original dataset"
  ) %>%
  tab_options(table.font.names = "Cambria",
              column_labels.font.weight = "bold",
              heading.title.font.size = 18,
              heading.subtitle.font.size = 16,
              table.font.color = "black",
              table.font.size = 14)

###############################################################
# Bar chart containing the count and percentage of subregions #
# for comparison with the charts in following section         #
###############################################################

# Keep only one record for each culture (this applies to all charts bellow)
comp_Data <- distinct(Data, culture, .keep_all = TRUE)

# Create a table from complete data
comp_tab <- aggregate(culture ~ subregion,  
                      data = comp_Data,
                      FUN = length)

# Rename the columns accordingly
names(comp_tab)[1] <- "Subregion"
names(comp_tab)[2] <- "No_of_Cultures"

# Order the table by the number of cultures for each subregion
comp_tab <- arrange(comp_tab, desc(No_of_Cultures))

# Add a percentage column to the table
comp_tab$Pc_of_cultures <- comp_tab$No_of_Cultures / sum(comp_tab$No_of_Cultures) * 100
comp_tab # Display the table

# Define the colors for each value
comp_colors <- c("Western Africa" = "#3fbafd",
                 "Southeast Asia" = "#3fbafd",  
                 "Eastern Africa" = "#3fbafd", 
                 "Arctic and Subarctic" = "#3fbafd", 
                 "South Asia" = "#7cd0fe",
                 "Southern Africa" = "#b8e6fe", 
                 "Melanesia" = "#b8e6fe",
                 "Amazon and Orinoco" = "#b8e6fe", 
                 "Scandinavia" = "#e1f4ff",
                 "Polynesia" = "#e1f4ff", 
                 "Plains and Plateau" = "#e1f4ff", 
                 "Middle East" = "#e1f4ff", 
                 "Eastern South America" = "#e1f4ff", 
                 "East Asia" = "#e1f4ff", 
                 "Central Andes" = "#e1f4ff", 
                 "Central America" = "#e1f4ff", 
                 "Central Africa" = "#e1f4ff") 

# Generate a bar chart showing the count
comp_bar <- ggplot(data = comp_tab,
                   aes(x = factor(Subregion, levels = rev(unique(Subregion)[order(No_of_Cultures)])),
                       y = No_of_Cultures,
                       fill = Subregion)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0, 4)) +
  scale_fill_manual(values = comp_colors) + 
  labs(title = "Complete data: The count of cultures in each subregion",
       subtitle = "The number of cultures in each subregion and their percentage distribution in the sample",
       x = "Subregion", 
       y = "No. of cultures") +
  theme(axis.text.x = element_text(hjust = 1, size = 11, angle = 45),
        axis.text.y = element_text(size = 11),
        panel.border = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "transparent"),
        legend.position = "none") +
  geom_text(aes(label = paste0(round(Pc_of_cultures, 1), "%"),
                y = No_of_Cultures), 
            size = 4,
            family = "Cambria",
            position = position_stack(vjust = 0.5))
comp_bar

##############################################
##                                          ##
#   Distribution of cultures in subregions   #
#   for individual models                    #
##                                          ##
##############################################

###########
# Model 1 #
###########

# Recreate the data that was used in a model for hypothesis testing. 
M1 <- Data[Data$women_obs != 0, ]
M1 <- M1 %>%
  select(men_dance, polygyny, subregion, culture)
M1 <- na.omit(M1)
M1 <- distinct(M1, culture, .keep_all = TRUE)

# Create a table showing number of records in each subregion
M1_tab <- aggregate(culture ~ subregion,
                    data = M1,
                    FUN = length)

# Rename the columns accordingly
names(M1_tab)[1] <- "Subregion"
names(M1_tab)[2] <- "No_of_Cultures"

# Order the table by the number of cultures for each subregion
M1_tab <- arrange(M1_tab, desc(No_of_Cultures))

# Add a percentage column to the table
M1_tab$Pc_of_cultures <- M1_tab$No_of_Cultures / sum(M1_tab$No_of_Cultures) * 100
M1_tab # Display the table

# Define the colors for each value
M1_colors <- c("Western Africa" = "#3fbafd",
               "Eastern Africa" = "#3fbafd",
               "Southeast Asia" = "#7cd0fe",
               "Southern Africa" = "#b8e6fe",
               "South Asia" = "#b8e6fe",
               "Scandinavia" = "#e1f4ff",
               "Polynesia" = "#e1f4ff",
               "Middle East" = "#e1f4ff",
               "Eastern South America" = "#e1f4ff",
               "East Asia" = "#e1f4ff",
               "Central Andes" = "#e1f4ff",
               "Central America" = "#e1f4ff",
               "Central Africa" = "#e1f4ff",
               "Amazon and Orinoco" = "#e1f4ff")  

# Arctic and Subarctic, Plains and Plateau and Melanesia are entirely missing

# split the subtitle into two lines (its too long for export in smaller resolution)
m1_subtitle <- "The number of cultures in each represented subregion and their percentage \ndistribution in model 1"
m1_subtitle <- paste(strwrap(m1_subtitle, width = 80), collapse = "\n")

# Generate a bar chart showing the count
M1_bar <- ggplot(data = M1_tab,
                 aes(x = factor(Subregion, levels = rev(unique(Subregion)[order(No_of_Cultures)])),
                     y = No_of_Cultures,
                     fill = Subregion)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0, 4)) +
  scale_fill_manual(values = M1_colors) + 
  labs(title = "Model 1: The count of cultures in represented subregions",
       subtitle = m1_subtitle,
       x = "Subregion", 
       y = "No. of cultures") +
  theme(axis.text.x = element_text(hjust = 1, size = 11, angle = 45),
        axis.text.y = element_text(size = 11),
        panel.border = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "transparent"),
        legend.position = "none") +
  geom_text(aes(label = paste0(round(Pc_of_cultures, 1), "%"),
                y = No_of_Cultures), 
            size = 4,
            family = "Cambria",
            position = position_stack(vjust = 0.5))
M1_bar

###########
# Model 2 #
###########

# Recreate the data that was used in a model for hypothesis testing. 
M2 <- Data[Data$women_obs != 0, ]
M2 <- M2 %>%
  select(men_dance, soc_strat, subregion, culture)
M2 <- na.omit(M2)
M2 <- distinct(M2, culture, .keep_all = TRUE)

# Create a table showing number of records in each subregion
M2_tab <- aggregate(culture ~ subregion,
                    data = M2,
                    FUN = length)

# Rename the columns accordingly
names(M2_tab)[1] <- "Subregion"
names(M2_tab)[2] <- "No_of_Cultures"

# Order the table by the number of cultures for each subregion
M2_tab <- arrange(M2_tab, desc(No_of_Cultures))

# Add a percentage column to the table
M2_tab$Pc_of_cultures <- M2_tab$No_of_Cultures / sum(M2_tab$No_of_Cultures) * 100
M2_tab # Display the table

# Define the colors for each value
M2_colors <- c("Western Africa" = "#3fbafd",
               "Eastern Africa" = "#7cd0fe",
               "South Asia" = "#b8e6fe",
               "Southern Africa" = "#e1f4ff",
               "Southeast Asia" = "#e1f4ff",
               "Polynesia" = "#e1f4ff", 
               "Middle East" = "#e1f4ff",
               "East Asia" = "#e1f4ff", 
               "Central Andes" = "#e1f4ff",
               "Central Africa" = "#e1f4ff", 
               "Amazon and Orinoco" = "#e1f4ff")

# Central America, Scandinavia, Eastern South America, Arctic and Subarctic, 
# Plains and Plateau and Melanesia are entirely missing

# split the subtitle into two lines (its too long for export in smaller resolution)
m2_subtitle <- "The number of cultures in each represented subregion and their percentage \ndistribution in model 2"
m2_subtitle <- paste(strwrap(m2_subtitle, width = 80), collapse = "\n")

# Generate a bar chart showing the count
M2_bar <- ggplot(data = M2_tab,
                 aes(x = factor(Subregion, levels = rev(unique(Subregion)[order(No_of_Cultures)])),
                     y = No_of_Cultures,
                     fill = Subregion)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0, 4)) +
  scale_fill_manual(values = M2_colors) + 
  labs(title = "Model 2: The count of cultures in represented subregions",
       subtitle = m2_subtitle,
       x = "Subregion", 
       y = "No. of cultures") +
  theme(axis.text.x = element_text(hjust = 1, size = 11, angle = 45),
        axis.text.y = element_text(size = 11),
        panel.border = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "transparent"),
        legend.position = "none") +
  geom_text(aes(label = paste0(round(Pc_of_cultures, 1), "%"),
                y = No_of_Cultures), 
            size = 4,
            family = "Cambria",
            position = position_stack(vjust = 0.5))
M2_bar

###########
# Model 3 #
###########

# Recreate the data that was used in a model for hypothesis testing. 
M3 <- Data[Data$women_obs != 0, ]
M3 <- M3 %>%
  select(men_dance, polygyny, soc_strat, subregion, culture)
M3 <- na.omit(M3)
M3 <- distinct(M3, culture, .keep_all = TRUE)

# Create a table showing number of records in each subregion
M3_tab <- aggregate(culture ~ subregion,
                    data = M3,
                    FUN = length)

# Rename the columns accordingly
names(M3_tab)[1] <- "Subregion"
names(M3_tab)[2] <- "No_of_Cultures"

# Order the table by the number of cultures for each subregion
M3_tab <- arrange(M3_tab, desc(No_of_Cultures))

# Add a percentage column to the table
M3_tab$Pc_of_cultures <- M3_tab$No_of_Cultures / sum(M3_tab$No_of_Cultures) * 100
M3_tab # Display the table

# Define the colors for each value
M3_colors <- c("Western Africa" = "#3fbafd",
               "Eastern Africa" = "#7cd0fe",
               "South Asia" = "#b8e6fe",
               "Southern Africa" = "#e1f4ff",
               "Southeast Asia" = "#e1f4ff",
               "Polynesia" = "#e1f4ff",
               "Middle East" = "#e1f4ff",
               "East Asia" = "#e1f4ff",
               "Central Andes" = "#e1f4ff", 
               "Central Africa" = "#e1f4ff", 
               "Amazon and Orinoco" = "#e1f4ff")

# Central America, Scandinavia, Eastern South America, Arctic and Subarctic, 
# Plains and Plateau and Melanesia are entirely missing

# split the subtitle into two lines (its too long for export in smaller resolution)
m3_subtitle <- "The number of cultures in each represented subregion and their percentage \ndistribution in model 3"
m3_subtitle <- paste(strwrap(m3_subtitle, width = 80), collapse = "\n")

# Generate a bar chart showing the count
M3_bar <- ggplot(data = M3_tab,
                 aes(x = factor(Subregion, levels = rev(unique(Subregion)[order(No_of_Cultures)])),
                     y = No_of_Cultures,
                     fill = Subregion)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0, 4)) +
  scale_fill_manual(values = M3_colors) + 
  labs(title = "Model 3: The count of cultures in represented subregions",
       subtitle = m3_subtitle,
       x = "Subregion", 
       y = "No. of cultures") +
  theme(axis.text.x = element_text(hjust = 1, size = 11, angle = 45),
        axis.text.y = element_text(size = 11),
        panel.border = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "transparent"),
        legend.position = "none") +
  geom_text(aes(label = paste0(round(Pc_of_cultures, 1), "%"),
                y = No_of_Cultures), 
            size = 4,
            family = "Cambria",
            position = position_stack(vjust = 0.5))
M3_bar

###########
# Model 4 #
###########

# Recreate the data that was used in a model for hypothesis testing. 
M4 <- Data %>%
  select(men_dance, marriage_org, subregion, culture)
M4 <- na.omit(M4)
M4 <- distinct(M4, culture, .keep_all = TRUE)

# Create a table showing number of records in each subregion
M4_tab <- aggregate(culture ~ subregion,
                    data = M4,
                    FUN = length)

# Rename the columns accordingly
names(M4_tab)[1] <- "Subregion"
names(M4_tab)[2] <- "No_of_Cultures"

# Order the table by the number of cultures for each subregion
M4_tab <- arrange(M4_tab, desc(No_of_Cultures))

# Add a percentage column to the table
M4_tab$Pc_of_cultures <- M4_tab$No_of_Cultures / sum(M4_tab$No_of_Cultures) * 100
M4_tab # Display the table

# Define the colors for each value
M4_colors <- c("Western Africa" = "#3fbafd",
               "Eastern Africa" = "#3fbafd",
               "Southeast Asia" = "#7cd0fe",
               "Southern Africa" = "#b8e6fe",
               "South Asia" = "#b8e6fe",
               "Scandinavia" = "#e1f4ff",
               "Polynesia" = "#e1f4ff",
               "Middle East" = "#e1f4ff",
               "Eastern South America" = "#e1f4ff",
               "East Asia" = "#e1f4ff",
               "Central Andes" = "#e1f4ff",
               "Central America" = "#e1f4ff",
               "Central Africa" = "#e1f4ff",
               "Amazon and Orinoco" = "#e1f4ff")  

# Arctic and Subarctic, Plains and Plateau and Melanesia are entirely missing

# split the subtitle into two lines (its too long for export in smaller resolution)
m4_subtitle <- "The number of cultures in each represented subregion and their percentage \ndistribution in model 4"
m4_subtitle <- paste(strwrap(m4_subtitle, width = 80), collapse = "\n")

# Generate a bar chart showing the count
M4_bar <- ggplot(data = M4_tab,
                 aes(x = factor(Subregion, levels = rev(unique(Subregion)[order(No_of_Cultures)])),
                     y = No_of_Cultures,
                     fill = Subregion)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0, 4)) +
  scale_fill_manual(values = M4_colors) + 
  labs(title = "Model 4: The count of cultures in represented subregions",
       subtitle = m4_subtitle,
       x = "Subregion", 
       y = "No. of cultures") +
  theme(axis.text.x = element_text(hjust = 1, size = 11, angle = 45),
        axis.text.y = element_text(size = 11),
        panel.border = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "transparent"),
        legend.position = "none") +
  geom_text(aes(label = paste0(round(Pc_of_cultures, 1), "%"),
                y = No_of_Cultures), 
            size = 4,
            family = "Cambria",
            position = position_stack(vjust = 0.5))
M4_bar

#############
# Model 5-7 #
#############

# Recreate the data that was used in a model for hypothesis testing. 
M5_7 <- Data[Data$men_obs != 0, ]
M5_7 <- M5_7 %>%
  select(women_dance, prec_pred, temp_pred, subregion, culture)
M5_7 <- na.omit(M5_7)
M5_7 <- distinct(M5_7, culture, .keep_all = TRUE)

# Create a table showing number of records in each subregion
M5_7_tab <- aggregate(culture ~ subregion,
                      data = M5_7,
                      FUN = length)

# Rename the columns accordingly
names(M5_7_tab)[1] <- "Subregion"
names(M5_7_tab)[2] <- "No_of_Cultures"

# Order the table by the number of cultures for each subregion
M5_7_tab <- arrange(M5_7_tab, desc(No_of_Cultures))

# Add a percentage column to the table
M5_7_tab$Pc_of_cultures <- M5_7_tab$No_of_Cultures / sum(M5_7_tab$No_of_Cultures) * 100
M5_7_tab # Display the table

# Define the colors for each value
M5_7_colors <- c("Western Africa" = "#3fbafd",
                 "Eastern Africa" = "#3fbafd",
                 "Southeast Asia" = "#7cd0fe",  
                 "Southern Africa" = "#b8e6fe",
                 "South Asia" = "#b8e6fe",
                 "Amazon and Orinoco" = "#b8e6fe",
                 "Scandinavia" = "#e1f4ff",
                 "Polynesia" = "#e1f4ff",
                 "Middle East" = "#e1f4ff",
                 "Eastern South America" = "#e1f4ff",
                 "East Asia" = "#e1f4ff",
                 "Central Andes" = "#e1f4ff",
                 "Central America" = "#e1f4ff",
                 "Central Africa" = "#e1f4ff",
                 "Arctic and Subarctic" = "#e1f4ff")

# Plains and Plateau and Melanesia are entirely missing

# split the subtitle into two lines (its too long for export in smaller resolution)
m5_7_subtitle <- "The number of cultures in each represented subregion and their percentage distribution in models 5, 6, and 7"
m5_7_subtitle <- paste(strwrap(m5_7_subtitle, width = 80), collapse = "\n")

# Generate a bar chart showing the count
M5_7_bar <- ggplot(data = M5_7_tab,
                   aes(x = factor(Subregion, levels = rev(unique(Subregion)[order(No_of_Cultures)])),
                       y = No_of_Cultures,
                       fill = Subregion)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0, 4)) +
  scale_fill_manual(values = M5_7_colors) + 
  labs(title = "Models 5-7: The count of cultures in represented subregions",
       subtitle = m5_7_subtitle,
       x = "Subregion", 
       y = "No. of cultures") +
  theme(axis.text.x = element_text(hjust = 1, size = 11, angle = 45),
        axis.text.y = element_text(size = 11),
        panel.border = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "transparent"),
        legend.position = "none") +
  geom_text(aes(label = paste0(round(Pc_of_cultures, 1), "%"),
                y = No_of_Cultures), 
            size = 4,
            family = "Cambria",
            position = position_stack(vjust = 0.5))
M5_7_bar

##############
# Model 8-10 #
##############

# Recreate the data that was used in a model for hypothesis testing. 
M8_10 <- Data %>%
  select(women_dance, prec_pred, temp_pred, subregion, culture)
M8_10 <- na.omit(M8_10)
M8_10 <- distinct(M8_10, culture, .keep_all = TRUE)

# Create a table showing number of records in each subregion
M8_10_tab <- aggregate(culture ~ subregion,
                       data = M8_10,
                       FUN = length)

# Rename the columns accordingly
names(M8_10_tab)[1] <- "Subregion"
names(M8_10_tab)[2] <- "No_of_Cultures"

# Order the table by the number of cultures for each subregion
M8_10_tab <- arrange(M8_10_tab, desc(No_of_Cultures))

# Add a percentage column to the table
M8_10_tab$Pc_of_cultures <- M8_10_tab$No_of_Cultures / sum(M8_10_tab$No_of_Cultures) * 100
M8_10_tab # Display the table

# Define the colors for each value
M8_10_colors <- c("Western Africa" = "#3fbafd",
                  "Eastern Africa" = "#3fbafd",
                  "Southeast Asia" = "#7cd0fe",
                  "Southern Africa" = "#b8e6fe",
                  "South Asia" = "#b8e6fe",
                  "Amazon and Orinoco" = "#b8e6fe",
                  "Scandinavia" = "#e1f4ff",
                  "Polynesia" = "#e1f4ff",
                  "Middle East" = "#e1f4ff",
                  "Eastern South America" = "#e1f4ff",
                  "East Asia" = "#e1f4ff",
                  "Central Andes" = "#e1f4ff",
                  "Central America" = "#e1f4ff",
                  "Central Africa" = "#e1f4ff",
                  "Arctic and Subarctic" = "#e1f4ff")

# Plains and Plateau and Melanesia are entirely missing

# split the subtitle into two lines (its too long for export in smaller resolution)
m8_10_subtitle <- "The number of cultures in each represented subregion and their percentage distribution in models 8, 9, and 10"
m8_10_subtitle <- paste(strwrap(m8_10_subtitle, width = 80), collapse = "\n")

# Generate a bar chart showing the count
M8_10_bar <- ggplot(data = M8_10_tab,
                    aes(x = factor(Subregion, levels = rev(unique(Subregion)[order(No_of_Cultures)])),
                        y = No_of_Cultures,
                        fill = Subregion)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0, 4)) +
  scale_fill_manual(values = M8_10_colors) + 
  labs(title = "Models 8-10: The count of cultures in represented subregions",
       subtitle = m8_10_subtitle,
       x = "Subregion", 
       y = "No. of cultures") +
  theme(axis.text.x = element_text(hjust = 1, size = 11, angle = 45),
        axis.text.y = element_text(size = 11),
        panel.border = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "transparent"),
        legend.position = "none") +
  geom_text(aes(label = paste0(round(Pc_of_cultures, 1), "%"),
                y = No_of_Cultures), 
            size = 4,
            family = "Cambria",
            position = position_stack(vjust = 0.5))
M8_10_bar

