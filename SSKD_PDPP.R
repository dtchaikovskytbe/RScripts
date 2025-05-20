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
SSKD_Data_Cleaned_ <- read_csv("C:/Users/dtcha/OneDrive - ipgm.edu.my/IPGKBA/Research and Innovation/SSKD (PDPP_KDC)/SSKD Data (Cleaned).csv")
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

# mutate into SSKD_composite data; SSKD
cleaned_data <- cleaned_data %>%
  mutate(across(5:73, as.numeric))
cleaned_data <- cleaned_data %>%
  mutate(composite_overall = rowMeans(across(5:73), na.rm = TRUE))

# mutate into SSKD_composite data; 4 domains
cleaned_data <- cleaned_data %>%
  mutate(composite_D1 = rowMeans(across(5:20), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D2 = rowMeans(across(21:40), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D3 = rowMeans(across(41:50), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D4 = rowMeans(across(51:73), na.rm = TRUE))

# mutate into SSKD_composite data; 11 sub-domains
cleaned_data <- cleaned_data %>%
  mutate(composite_D1.1 = rowMeans(across(5:12), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D1.2 = rowMeans(across(13:17), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D1.3 = rowMeans(across(18:20), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D2.1 = rowMeans(across(21:25), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D2.2 = rowMeans(across(26:28), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D2.3 = rowMeans(across(28:34), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D2.4 = rowMeans(across(35:40), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D3.1 = rowMeans(across(41:43), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D3.2 = rowMeans(across(44:45), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D3.3 = rowMeans(across(46:50), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D4.1 = rowMeans(across(51:54), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D4.2 = rowMeans(across(55:60), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D4.3 = rowMeans(across(61:69), na.rm = TRUE))
cleaned_data <- cleaned_data %>%
  mutate(composite_D4.4 = rowMeans(across(70:73), na.rm = TRUE))

summary(cleaned_data)

# analyse SSKD_items data
SSKD_items <- subset(cleaned_data, select = 5:73)

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

# Performing CFA to verify model structure
model_1 <- '
D1 =~ composite_D1.1 + composite_D1.2 + composite_D1.3
D2 =~ composite_D2.1 + composite_D2.2 + composite_D2.3 + composite_D2.4
D3 =~ composite_D3.1 + composite_D3.2 + composite_D3.3
D4 =~ composite_D4.1 + composite_D4.2 + composite_D4.3 + composite_D4.4
'
fit <- cfa(model_1, data = cleaned_data, estimator = "WLSMV")  # WLSMV for ordinal data

summary(fit, fit.measures = TRUE, standardized = TRUE)
semPaths(fit, "std", whatLabels = "std", edge.label.cex = 0.8, layout = "tree")
modindices(fit, sort. = TRUE, minimum.value = 10)

# Justification for the Use of Composite Variables
# In the current study, composite variables were employed in place of single-indicator latent constructs during confirmatory factor analysis (CFA). The use of single-indicator latent variables is generally discouraged in structural equation modeling (SEM) due to their limited capacity to account for measurement error and their susceptibility to estimation instability (Bollen, 1989; Kline, 2016). A single indicator fails to provide sufficient information about the underlying construct and does not allow for the estimation of internal consistency or construct validity. To address this limitation, observed variables that theoretically represented a unidimensional construct and demonstrated acceptable internal consistency (Cronbach’s α ≥ .80) were aggregated to form composite scores. This approach aligns with recommendations in the SEM literature, where composite variables are considered a pragmatic alternative when the inclusion of multiple indicators per latent construct is not feasible (Little et al., 2002; Hair et al., 2019). The use of composites simplifies the model, reduces parameter estimation complexity, and retains construct-level representation while mitigating the risks associated with under-identified or unstable latent structures. Accordingly, the decision to utilize composite variables in this study ensured parsimony, theoretical coherence, and statistical robustness in the measurement model.

#References
#Bollen, K. A. (1989). Structural equations with latent variables. Wiley.
#Hair, J. F., Black, W. C., Babin, B. J., & Anderson, R. E. (2019). Multivariate data analysis (8th ed.). Cengage Learning.
#Kline, R. B. (2016). Principles and practice of structural equation modeling (4th ed.). Guilford Press.
#Little, T. D., Cunningham, W. A., Shahar, G., & Widaman, K. F. (2002). To parcel or not to parcel: Exploring the question, weighing the merits. Structural Equation Modeling, 9(2), 151–173. https://doi.org/10.1207/S15328007SEM0902_1

#model 3,  
model_3 <- '
D1 ~~ D2
D1 ~~ D3
D1 ~~ D4
D2 ~~ D3
D2 ~~ D4
D3 ~~ D4

D1 =~ composite_D1.1 + composite_D1.2 + composite_D1.3
D2 =~ composite_D2.1 + composite_D2.2 + composite_D2.3 + composite_D2.4
D3 =~ composite_D3.1 + composite_D3.2 + composite_D3.3
D4 =~ composite_D4.1 + composite_D4.2 + composite_D4.3 + composite_D4.4

'
# fit model
fit_3 <- sem(model_3, data = cleaned_data)

# inspect result
summary(fit_3, standardized = TRUE, fit.measures = TRUE)

# check fit indices
fit_indices_3 <- fitmeasures(fit_3, c("CFI", "TLI", "RMSEA", "SRMR"))
print(fit_indices_3)  

# check modification indices
mod_indices_3 <- modindices(fit_3)
print(mod_indices_3)

# visualise model
semPaths(fit_3, 
         whatLabels = "est",
         sizeMan = 7, 
         sizeLat = 7, 
         edge.label.cex = 0.85,
         fade = TRUE, 
         layout = "tree2",
         style = "lisrel", 
         residuals = FALSE,
)

semPaths(fit_3, 
         whatLabels = "std",
         sizeMan = 7, 
         sizeLat = 7, 
         edge.label.cex = 0.85,
         fade = TRUE, 
         layout = "tree2",
         style = "lisrel", 
         residuals = FALSE,
)

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
# Pendidikan Jasmani, m = 3.28; Bahasa Inggeris, m = 2.86
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D1 = mean(composite_D1, na.rm = TRUE))

# Calculate mean of composite_D2 for each level of Pengajian - Instruksional
# Pendidikan Seni Visual, m = 3.45; Bahasa Inggeris, m = 2.96
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D2 = mean(composite_D2, na.rm = TRUE))

# Calculate mean of composite_D3 for each level of Pengajian - Penglibatan Komuniti
# Pendidikan Seni Visual, m = 3.52; Bahasa Inggeris, m = 2.99
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D3 = mean(composite_D3, na.rm = TRUE))

# Calculate mean of composite_D4 for each level of Pengajian - Kualiti Peribadi
# Pendidikan Seni Visual, m = 3.54, Bahasa INggeris, m = 3.12
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D4 = mean(composite_D4, na.rm = TRUE))

# Calculate mean of composite_D1.1 for each level of Program - Orientasi Ilmu
# Pendidikan Jasmani, m = 3.28
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D1.1 = mean(composite_D1.1, na.rm = TRUE))

# Calculate mean of composite_D1.2 for each level of Program - Orientasi Ilmu
# Pendidikan Jasmani, m = 3.25
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D1.2 = mean(composite_D1.2, na.rm = TRUE))

# Calculate mean of composite_D1.3 for each level of Program - Orientasi Ilmu
# Pendidikan Jasmani, m = 3.32
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D1.3 = mean(composite_D1.3, na.rm = TRUE))

# Calculate mean of composite_D2.1 for each level of Pengajian - Instruksional
# Pendidikan Seni Visual, m = 3.39
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D2.1 = mean(composite_D2.1, na.rm = TRUE))

# Calculate mean of composite_D2.2 for each level of Pengajian - Instruksional
# Pendidikan Seni Visual, m = 3.39
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D2.2 = mean(composite_D2.2, na.rm = TRUE))

# Calculate mean of composite_D2.3 for each level of Pengajian - Instruksional
# Pendidikan Seni Visual, m = 3.48
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D2.3 = mean(composite_D2.3, na.rm = TRUE))

# Calculate mean of composite_D2.4 for each level of Pengajian - Instruksional
# Pendidikan Seni Visual, m = 3.46
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D2.4 = mean(composite_D2.4, na.rm = TRUE))

# Calculate mean of composite_D3.1 for each level of Pengajian - Penglibatan Komuniti
# Pendidikan Seni Visual, m = 3.32
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D3.1 = mean(composite_D3.1, na.rm = TRUE))

# Calculate mean of composite_D3.2 for each level of Pengajian - Penglibatan Komuniti
# Pendidikan Seni Visual, m = 3.48
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D3.2 = mean(composite_D3.2, na.rm = TRUE))

# Calculate mean of composite_D3.3 for each level of Pengajian - Penglibatan Komuniti
# Pendidikan Seni Visual, m = 3.66
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D3.3 = mean(composite_D3.3, na.rm = TRUE))

# Calculate mean of composite_D4.1 for each level of Pengajian - Kualiti Peribadi
# Pendidikan Seni Visual, m = 3.49
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D4.1 = mean(composite_D4.1, na.rm = TRUE))

# Calculate mean of composite_D4.2 for each level of Pengajian - Kualiti Peribadi
# Pendidikan Seni Visual, m = 3.67
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D4.2 = mean(composite_D4.2, na.rm = TRUE))

# Calculate mean of composite_D4.3 for each level of Pengajian - Kualiti Peribadi
# Pendidikan Seni Visual, m = 3.41
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D4.3 = mean(composite_D4.3, na.rm = TRUE))

# Calculate mean of composite_D4.4 for each level of Pengajian - Kualiti Peribadi
# Pendidikan Seni Visual, m = 3.70
cleaned_data %>%
  group_by(Program) %>%
  summarise(mean_D4.4 = mean(composite_D4.4, na.rm = TRUE))
