####################################################
#
# PHS 2000B Problem Set 9 - Missing Data
#
####################################################

# Part II

####################################################

###########    Loading YRBS Dataset    ############

####################################################

### This code does NOT have to be modified ###

# Load YRBS data
if (!require("haven")) install.packages("haven")
library("haven")

yrbs2015 <- data.frame(read_sas("yrbs2015.sas7bdat"))

# Perform data cleaning
# Turn blanks into "NA" 
yrbs2015[yrbs2015 == ""] <- NA
# Convert some fields from character to numeric
convert <- colnames(yrbs2015[1:99])
yrbs2015[,convert] <- lapply(yrbs2015[,convert,drop=FALSE],as.numeric)


####################################################

###############    Question 1    ##################

####################################################

### This code does NOT have to be modified ###

# Subset to the 6 variables of interest
yrbs2015.2 <- subset(yrbs2015, select = c(Q25, Q27, Q1, Q2, Q82, QN18))

# Create a function to calculate the proportion of missing data
pMiss <- function(x){sum(is.na(x)) / length(x)*100}

# Apply the missing data function to the subsetted data
apply(yrbs2015.2, 2, pMiss)

# Plot the pattern of missing data
if (!require("VIM")) install.packages("VIM")
library("VIM")
aggr_plot <- aggr(yrbs2015.2, col=c('navyblue','red'), numbers = TRUE, sortVars = TRUE,
                  labels = names(data), cex.axis = 0.7, gap = 1, prop = c(TRUE, FALSE),
                  ylab = c("Histogram of missing data", "Pattern"), cex.numbers = 0.5)


####################################################

###############    Question 2    ##################

####################################################

# To look at the data for these questions you can use the table() functions below.  
# Q89. During the past 12 months, how would you describe your grades in school?
# A. Mostly A's
# B. Mostly B's
# C. Mostly C's
# D. Mostly D's
# E. Mostly F's
# F. None of these grades
# G. Not sure
table(yrbs2015$Q89)

# Q13. During the past 30 days, on how many days did you carry a weapon such as a gun, knife, or club?
# A. 0 days
# B. 1 day
# C. 2 or 3 days
# D. 4 or 5 days
# E. 6 or more days
table(yrbs2015$Q13)

# Feel free to use whichever method and/or package you prefer to answer this question
# For example, you could construct simple frequency tables or fit a logistic model


####################################################

###############    Question 3    ##################

####################################################

### This code does NOT have to be modified ###

# First, convert dichotomous variables to 0/1 instead of 1/2 for interpretability and modeling
yrbs2015$Q25[yrbs2015$Q25 == 2] <- 0
yrbs2015$Q27[yrbs2015$Q27 == 2] <- 0
yrbs2015$Q2[yrbs2015$Q2 == 2] <- 0
yrbs2015$QN18[yrbs2015$QN18 == 2] <- 0

# Fit the logistic model
CCmodel <- glm(Q27 ~ Q25 + Q1 + Q2 + Q82 + QN18,
               family = binomial(link = "logit"), 
               data = yrbs2015)
summary(CCmodel)

# Print the odds ratio for the effect of electronic bullying on suicidal ideation, along with its 95% CI
exp(cbind(OR = coef(CCmodel), confint(CCmodel)))


####################################################

###############    Question 4    ##################

####################################################

### This code does NOT have to be modified ###

# First, restrict to variables which will be used in the imputation model
to_keep <- c("Q25", "Q27", "Q1", "Q2", "Q82", "QN18", "Q89", "Q13") # UPDATE LIST OF VARIABLES
d <- yrbs2015[ , to_keep ]

# Call the MICE package
if (!require("mice")) install.packages("mice")
library(mice)

# Implement MICE with m=5 imputed datasets
imps <- mice(d, m=5)

# Check the predictor matrix used
imps$pred

# Check the default imputation method
imps$method


####################################################

###############    Question 5    ##################

####################################################

### This code does NOT have to be modified ###

# Extract the first imputed dataset
mouse <- mice::complete(imps, 1)

# Concatenate observed data with the first imputed dataset (for ease of analysis)
temp <- rbind(d, mouse)
temp$dataset <- c(rep( "Original", nrow(d) ),
                  rep( "Imputation m=1", nrow(mouse) ) )

# Display the marginal distributions in the observed vs. imputed data
library(dplyr)
library(tidyr)

temp |> 
  group_by(dataset) |>
  filter( !is.na(QN18) ) |>
  summarize( prop.yes = mean(QN18) )

# Display the conditional distributions in the observed vs. imputed data
# conditioning on a variable hypothesized to be predictive of missingness
temp |>
  group_by(dataset, Q13) |>      
  filter( !is.na(QN18) & !is.na(Q13) ) |>
  summarize( prop.yes = mean(QN18) )


temp |>
  group_by(dataset, Q89) |>       
  filter( !is.na(QN18) & !is.na(Q89) ) |> 
  summarize( prop.yes = mean(QN18) )


####################################################

###############    Question 6     ##################

####################################################

### This code does NOT have to be modified ###

# Fit analytic model within each of the m imputed datasets
impmods <- with(imps, glm(Q27 ~ Q25 + Q1 + Q2 + Q82 + QN18,
                           family = binomial(link = "logit") ) )
# Pool across 5 datasets              
pooled <- pool(impmods)

# Compute the pooled OR
str(pooled)   # This command helps us to see the structure of the object "pooled"
exp(pooled$pooled$estimate)


####################################################

###############    Question 7    ##################

####################################################

### This code does NOT have to be modified ###

pooled$pooled$r
pooled$pooled$lambda
