#Clear environment.
rm(list = ls())

#Set up workspace.
library(mice)
library(modelsummary)
library(webshot2)

#4. Import data.
wages <- read_csv("DScourseS24/ProblemSets/PS7/wages.csv")

#5. Remove missing values for hgc and tenure.
wages <- wages[!is.na(wages$hgc), ]
wages <- wages[!is.na(wages$tenure), ]

#6. Generate brief summary of data. 25% of logwage is missing.
datasummary_skim(wages)
sum <- datasummary_skim(wages, output = "sum.tex")
print(sum)

wages$missing <- ifelse(!is.na(wages$logwage), 0, 1)
wages$missing[is.na(wages$missing)] <- 1

datasummary_correlation(wages)
corr <- datasummary_correlation(wages, output = "corr.tex")
print(corr)

#7. Regression models
#Listwise deletion:
listdel <- wages[!is.na(wages$logwage), ]
listdel$tenure2 <- I(listdel$tenure^2)
listdel_model <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, data = listdel)
  
#Mean imputation:
lwmean <- mean(wages$logwage, na.rm = TRUE)
meanimp <- wages
meanimp$logwage <- ifelse(is.na(meanimp$logwage), lwmean, meanimp$logwage)
meanimp$tenure2 <- I(meanimp$tenure^2)
meanimp_model <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, data = meanimp)

#Predicted values:
predval <- wages
predval$tenure2 <- I(predval$tenure^2)
predval$logwage[is.na(predval$logwage)] <- predict(listdel_model, newdata = predval[is.na(predval$logwage), ])
predval_model <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, data = predval)

#Multiple imputation:
multimp <- mice(wages, m = 5, maxit = 5, printFlag = FALSE)
multimp_model <- with(multimp, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))

#Pool models:
mod <- list()
mod[['Listwise deletion']] <- listdel_model
mod[['Mean imputation']] <- meanimp_model
mod[['Predicted values']] <- predval_model
mod[['Multiple imputation']] <- multimp_model
mod[['Multiple imputation']] <- mice::pool(mod[['Multiple imputation']])

#Summarize:
modelsummary(mod)
output <- modelsummary(mod, stars = TRUE, output = "mod.tex")
print(output)




