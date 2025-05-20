# Install key packages
install.packages("tidyverse")
install.packages("psych")
install.packages("readr")
install.packages("dplyr")
install.packages("Hmisc")
install.packages("REdaS")
install.packages("eRm")
install.packages("ltm")
install.packages("lavaan")
install.packages("semPlot")
install.packages("irr")
install.packages("plspm")
install.packages("seminr")
install.packages("qgraph")
install.packages("mice")
install.packages("ordinal")
install.packages("effectsize")

# Load key packages
library(tidyverse)
library(psych)
library(readr)
library(dplyr)
library(Hmisc)
library(grid)
library(REdaS)
library(eRm)
library(ltm)
library(lavaan)
library(semPlot)
library(irr)
library(ggplot2)
library(MASS)
library(seminr)
library(plspm)
library(qgraph)
library(mice)
library(ordinal)
library(effectsize)

# Import datafile
library(readr)
SSKD_Data_Cleaned_ <- read_csv("SSKD Data (Cleaned) - Full CFA.csv")
cleaned_data <- as_tibble(SSKD_Data_Cleaned_)
print(cleaned_data)

# descriptive analysis (Central Tendencies)
central_tendencies <- summary(cleaned_data)
print(central_tendencies)

# transform data into categorical/ordinal data
# demographic information
cleaned_data$Jantina <- factor(cleaned_data$Jantina, ordered = FALSE)
cleaned_data$Kumpulan <- factor(cleaned_data$`Program / Kumpulan`,ordered = FALSE)
cleaned_data$Program <- factor(cleaned_data$`Program Pengajian`, ordered = FALSE)
cleaned_data$Kelayakan <- factor(cleaned_data$`Kelayakan Akademik Tertinggi`, ordered = TRUE)

# rating item
SSKD_level <- c(1,2,3,4)
cleaned_data[,5:73] <- lapply(cleaned_data[,5:73], factor, levels = SSKD_level, ordered = TRUE)

# descriptive analysis (frequency analysis)
frequency_analysis <- summary(cleaned_data, na.rm = TRUE)
print(frequency_analysis)

# analyse SSKD_items data
SSKD_items <- subset(SSKD_Data_Cleaned_, select = 5:73)
summary(SSKD_items)

# Cronbach alpha analysis
cronbach_alpha_all <- alpha(SSKD_items)
print(cronbach_alpha_all)

# impute missing data
runif(1)
set.seed(123)  # or whatever seed you like
imputed_data <- mice(cleaned_data)
cleaned_data <- complete(imputed_data,) 

# set chart style
par(mfrow = c(1, 1))

# mutate into SSKD_composite data; 11 sub-domains
cleaned_data <- cleaned_data %>%
  mutate(across(5:73, as.numeric))
cleaned_data <- cleaned_data %>%
  mutate(composite_D111 = rowMeans(across(5:6), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D112 = rowMeans(across(7:8), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D113 = rowMeans(across(9:10), na.rm = TRUE)) 
cleaned_data <- cleaned_data %>%
  mutate(composite_D114 = rowMeans(across(11:12), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D121 = rowMeans(across(13:15), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D122 = rowMeans(across(16:17), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D131 = rowMeans(across(18:20), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D211 = rowMeans(across(21:22), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D212 = rowMeans(across(23:25), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D221 = rowMeans(across(26:28), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D231 = rowMeans(across(29:31), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D232 = rowMeans(across(32:34), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D241 = rowMeans(across(35:36), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D242 = rowMeans(across(37:39), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D243 = rowMeans(across(39:40), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D311 = rowMeans(across(41:43), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D321 = rowMeans(across(44:45), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D331 = rowMeans(across(46:47), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D332 = rowMeans(across(48:50), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D411 = rowMeans(across(51:54), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D421 = rowMeans(across(55:56), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D422 = rowMeans(across(57:58), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D423 = rowMeans(across(59:60), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D431 = rowMeans(across(61:62), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D432 = rowMeans(across(63:64), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D433 = rowMeans(across(65:67), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D434 = rowMeans(across(68:69), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D441 = rowMeans(across(70:71), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D442 = rowMeans(across(72:73), na.rm = TRUE))


# Performing CFA to verify model structure
model_1 <- '

D11 =~ composite_D111 + composite_D112 + composite_D113 + composite_D114
D12 =~ composite_D121 + composite_D122
D13 =~ 1*composite_D131
D21 =~ composite_D211 + composite_D212
D22 =~ 1*composite_D221
D23 =~ composite_D231 + composite_D232
D24 =~ composite_D241 + composite_D242 + composite_D243
D31 =~ 1*composite_D311
D32 =~ 1*composite_D321
D33 =~ composite_D331 + composite_D332
D41 =~ 1*composite_D411
D42 =~ composite_D421 + composite_D422 + composite_D423
D43 =~ composite_D431 + composite_D432 + composite_D433 + composite_D434
D44 =~ composite_D441 + composite_D442

D1 =~ D11 + D12 + D13
D2 =~ D21 + D22 + D23 + D24
D3 =~ D31 + D32 + D33
D4 =~ D41 + D42 + D43 + D44

'
fit <- cfa(model_1, data = cleaned_data, estimator = "WLSMV")  # WLSMV for ordinal data

summary(fit, fit.measures = TRUE, standardized = TRUE)
semPaths(fit, "std", whatLabels = "std", edge.label.cex = 0.8, layout = "tree")
modindices(fit, sort. = TRUE, minimum.value = 10)



# ANOVA demo X composite_overall
# overall x Program 
aov_overall_Program <- aov(composite_overall ~ Program, data = cleaned_data)

summary(aov_overall_Program)

eta_squared(aov_overall_Program)

# Pendidikan Seni Visual, m = 3.44
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D1 = mean(composite_overall, na.rm = TRUE))

# Run MANOVA; MANOVA is more reliable than SEM because PROGRAM is nominal rather than ordinal
manova_model <- manova(cbind(composite_D1, composite_D2, composite_D3, composite_D4) ~ Program, data = cleaned_data)

# Get multivariate test results (e.g., Pillai's trace, Wilks' Lambda)
summary(manova_model)

# Follow-up univariate ANOVAs
summary.aov(manova_model)
eta_squared(manova_model)

# Calculate mean of composite_D1 for each level of Program - Orientasi Ilmu
# Pendidikan Jasmani, m = 3.28
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D1 = mean(composite_D1, na.rm = TRUE))

# Calculate mean of composite_D2 for each level of Pengajian - Instruksional
# Pendidikan Seni Visual, m = 3.45
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D2 = mean(composite_D2, na.rm = TRUE))

# Calculate mean of composite_D3 for each level of Pengajian - Penglibatan Komuniti
# Pendidikan Seni VIsual, m = 3.52
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D3 = mean(composite_D3, na.rm = TRUE))

# Calculate mean of composite_D4 for each level of Pengajian - Kualiti Peribadi
# Pendidikan Seni VIsual, m = 3.54
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D4 = mean(composite_D4, na.rm = TRUE))

