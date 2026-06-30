#research question: Do male and female faculty members have significantly different average salaries at BGSU? 

#import data
library(readr)
data <- read_csv("module_8_data.csv")

#check variables "male" and "salary", any missing data, outliers
table(data$male)
summary(data$salary)

#check data distribution
#histogram
hist(data$salary, breaks = 30)
#Normality test
by(data$salary, data$male, shapiro.test) #p<0.05

#create new variable salary_normal
set.seed(123)

normal_by_group <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  
  n <- sum(ok)
  
  # Convert original values to normal quantiles based on rank
  r <- rank(x[ok], ties.method = "random")
  z <- qnorm((r - 0.5) / n)
  
  # Rescale so the new variable has the same mean and SD as the original group
  z <- as.numeric(scale(z))
  out[ok] <- mean(x[ok]) + sd(x[ok]) * z
  
  return(out)
}

data$salary_normal <- ave(data$salary, data$male, FUN = normal_by_group)

# Optional: round to 2 decimals
data$salary_normal <- round(data$salary_normal, 2)
export(data, "module_8_data.csv")  

#Normality test
by(data$salary_normal, data$male, shapiro.test)

#Histogram
hist(data$salary_normal[data$male == 0], main = "Female Faculty Salaries", xlab = "Salary")
hist(data$salary_normal[data$male == 1], main = "Male Faculty Salaries", xlab = "Salary")

#Plot
qqnorm(data$salary_normal[data$male == 0])
qqline(data$salary_normal[data$male == 0])

qqnorm(data$salary_normal[data$male == 1])
qqline(data$salary_normal[data$male == 1])

#Homogeneity of Variance: F test
f_test <- var.test(salary_normal ~ male, data = data)
f_test
#p-value is less than 0.05, therefore we reject the null hypothesis and we give evidence our variance are heterogenous. 

#Welch two sample t-test
t.test(salary_normal ~ male, data = data)

#Independent two samples t-test
t.test(salary_normal ~ male, data = data, var.equal = TRUE)
#Welch t-test vs Independent two sample t-test:
#Welch t-test handles unequal variance well.

#######################ANOVA####################################################
#Research question: Do faculty members in 3 ranks (assistant, associate, and full professors) have significantly different average salaries at BGSU? 
  
# 1. Check normality by group
by(data$salary_normal, data$rank, shapiro.test)


# 2. Check variance equality
bartlett.test(salary_normal ~ rank, data = data)


# 3. One way ANOVA If normality and equal variance are reasonable

anova_model <- aov(salary_normal ~ rank, data = data)

summary(anova_model)

# 4. Bonferroni-adjusted post-hoc comparisons
pairwise.t.test(data$salary_normal, data$rank, p.adjust.method = "bonferroni")
