#Research question:Is there a significant difference between gingival crevicular fluid measured 3 months post treatment and 6 months post treatment?

#import data
library(haven)
data <- read_sav("C:/Users/ylyl/University of Michigan Dropbox/Yang Liu/OBGYN-Stats/Fellowship course/module9/Module_9_Data.sav")
#export data
library(rio)
export(data, "Module_9_Data.csv") 

# Check sample size
nrow(data)

# check if all participants have completed paired data
data$pair_complete <- complete.cases(data$avg_gcf3, data$avg_gcf6)
table(data$pair_complete)

#############optional###############################
# Keep only complete paired cases if any case has incomplete data
data <- data[complete.cases(data$avg_gcf3, data$avg_gcf6), ]

# Check sample size
nrow(data)
####################end##############################

# Create difference score
data$diff_gcf <- data$avg_gcf6 - data$avg_gcf3

# Descriptive statistics
summary(data$diff_gcf)
mean(data$diff_gcf)
sd(data$diff_gcf)

# Normality test of difference scores
shapiro.test(data$diff_gcf)

# Visual checks
hist(data$diff_gcf,
     main = "Histogram of Difference Scores",
     xlab = "avg_gcf6 - avg_gcf3")

qqnorm(data$diff_gcf,
       main = "Q-Q Plot of Difference Scores")
qqline(data$diff_gcf)

boxplot(data$diff_gcf,
        main = "Boxplot of Difference Scores",
        ylab = "avg_gcf6 - avg_gcf3")

# Paired t-test
t.test(data$avg_gcf3, data$avg_gcf6, paired = TRUE)

#######################################################################
# Research question: Do those with higher than the median average GCF at the three month visit stay in that categorization at the six month visit?

# check if all participants have completed paired data
data$pair_complete <- complete.cases(data$avg_gcf_medsplit3, data$avg_gcf_medsplit6)
table(data$pair_complete)

# Keep only complete paired cases
d <- data[complete.cases(data$avg_gcf_medsplit3, data$avg_gcf_medsplit6), ]

# Create crosstable
gcf_table <- table(
  Time3 = d$avg_gcf_medsplit3,
  Time6 = d$avg_gcf_medsplit6
)

# Display crosstable
gcf_table

# Add row and column totals
addmargins(gcf_table)

# McNemar's test
mcnemar.test(gcf_table)
