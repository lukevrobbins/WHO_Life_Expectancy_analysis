# Cleaning Variables
# Loading libraries
library(tidyverse)
library(car)
library(MASS)
library(ggplot2)
library(dplyr)

# Define data set into variable called LED, short for Life Expectancy Data
LED <- read.csv("who_life_exp.csv")

# Most names are already cleaned and appropriate, but some can be improved
names(LED)[5] <- "life_expectancy"
names(LED)[6] <- "life_exp_at_60"
names(LED)[7] <- "adult_mort"
names(LED)[9] <- "age1_4mort"
names(LED)[12] <- "age5_19thinness"
names(LED)[13] <- "age5_19obesity"
names(LED)[18] <- "basic_water"
names(LED)[22] <- "gghe_d"
names(LED)[25] <- "une_infant_mort"
names(LED)[26] <- "une_life_expectancy"


# Change infant and age 1-4 mortality from decimal to percent
LED$infant_mort <- LED$infant_mort * 100
LED$age1_4mort <- LED$age1_4mort * 100

# Change adult mortality from 1 in 1000 to percent
LED$adult_mort <- LED$adult_mort / 10

# Change frequency of doctors from 1 in 10000 to percent
LED$doctors <- LED$doctors / 100

# Scale the GNI down by 1000 so that a value of 1 represents 1k dollars
LED$gni_capita <- LED$gni_capita / 1000



# Demographic
demoDF <- data.frame(LED$life_expectancy,
                     LED$region)


# Mortality
mortDF <- data.frame(LED$life_expectancy, 
                         LED$infant_mort, 
                         LED$adult_mort, 
                         LED$une_infant_mort)

# Public health
pubhDF <- data.frame(LED$life_expectancy,
                     LED$alcohol,
                     LED$bmi,
                     LED$age5_19thinness,
                     LED$age5_19obesity)

# Vaccine 
vacDF <- data.frame(LED$life_expectancy,
                    LED$hepatitis,
                    LED$measles,
                    LED$polio,
                    LED$diphtheria)

# Resources 
resoDF <- data.frame(LED$life_expectancy,
                     LED$basic_water,
                     LED$doctors,
                     LED$hospitals)

# Ecnomic
economicDF <- data.frame(LED$life_expectancy, 
                         LED$gni_capita, 
                         LED$gghe_d, 
                         LED$che_gdp, 
                         LED$une_poverty)

# Educational
eduDF <- data.frame(LED$life_expectancy, 
                    LED$une_edu_spend, 
                    LED$une_literacy, 
                    LED$une_school)

# Plotting categories  

library(reshape2)

# Economics
demo2 <- melt(demoDF[,1:2], id.vars='LED.life_expectancy')
ggplot(demo2) +
  geom_jitter(aes(value,LED.life_expectancy, colour="54D7E7"),) +
  geom_smooth(aes(value,LED.life_expectancy, colour="54D7E7"), method=lm, se=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(vjust = 1),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none"
        )+
  labs(title = "Regional Life Expectancy", y = "Life Expectancy", x = "Region")


mort2 <- melt(mortDF[,1:4], id.vars='LED.life_expectancy')
ggplot(mort2) +
  geom_jitter(aes(value,LED.life_expectancy, colour=variable),) +
  geom_smooth(aes(value,LED.life_expectancy, colour=variable), method=lm, se=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(vjust = 1),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Mortality vs. Life Expectancy", y = "Life Expectancy", x = "Mortality By Age")+
  facet_wrap(~variable, scales="free_x")

# Public health vs LE
pubh2 <- melt(pubhDF[,1:5], id.vars='LED.life_expectancy')
ggplot(pubh2) +
  geom_jitter(aes(value,LED.life_expectancy, colour=variable),) +
  geom_smooth(aes(value,LED.life_expectancy, colour=variable), method=lm, se=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(vjust = 1),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Public Health Variables", y = "Life Expectancy", x = "Health Variable Value")+
  facet_wrap(~variable, scales="free_x")

# Vaccination vs LE

vac2 <- melt(vacDF[,1:5], id.vars='LED.life_expectancy')
ggplot(vac2) +
  geom_jitter(aes(value,LED.life_expectancy, colour=variable),) +
  geom_smooth(aes(value,LED.life_expectancy, colour=variable), method=lm, se=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(vjust = 1),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Vaccination Variables", y = "Life Expectancy", x = "Vaccination Variable Value") +
  facet_wrap(~variable, scales="free_x")


# Resources vs LE

reso2 <- melt(resoDF[,1:4], id.vars='LED.life_expectancy')
ggplot(reso2) +
  geom_jitter(aes(value,LED.life_expectancy, colour=variable),) +
  geom_smooth(aes(value,LED.life_expectancy, colour=variable), method=lm, se=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(vjust = 1),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Resource Variables", y = "Life Expectancy", x = "Resource Variable Value") +
  facet_wrap(~variable, scales="free_x")


# Economic vs LE

eco2 <- melt(economicDF[,1:5], id.vars='LED.life_expectancy')
ggplot(eco2) +
  geom_jitter(aes(value,LED.life_expectancy, colour=variable),) +
  geom_smooth(aes(value,LED.life_expectancy, colour=variable), method=lm, se=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(vjust = 1),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Economic Variables", y = "Life Expectancy", x = "Economic Variable Value") +
  facet_wrap(~variable, scales="free_x")


#Education vs LE
edu2 <- melt(eduDF[,1:4], id.vars='LED.life_expectancy')
ggplot(edu2) +
  geom_jitter(aes(value,LED.life_expectancy, colour=variable),) +
  geom_smooth(aes(value,LED.life_expectancy, colour=variable), method=lm, se=FALSE) v +
  facet_wrap(~variable, scales="free_x")


# Creating a data frame with only the year 2012
LED12 <- LED %>% filter(year == 2012) %>% arrange(desc(life_expectancy))

# Univariate Analysis

# Measles vs LE

ggplot(LED12) +
  geom_jitter(aes(life_expectancy, measles), color = "blue")+
  theme(axis.title.x = element_text(vjust = -4),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Life Expectancy With Measles Vaccine (2012)", y = "Life Expectancy", x = "Percent of Population Vaccinated") +
  scale_x_continuous(limits=c(50, 90)) +
  scale_y_continuous(limits=c(40, 110)) +
  geom_smooth(aes(life_expectancy, measles), method=lm, se=FALSE)

# Basic Water vs LE
# Basic Water
ggplot(LED12) +
  geom_jitter(aes(life_expectancy, basic_water), color = "red")+
  theme(axis.title.x = element_text(vjust = -4),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Life Expectancy with Access to Water (2012)", y = "Life Expectancy", x = "Percentage of Pop. with Basic Water Access") +
  scale_x_continuous(limits=c(50, 90)) +
  scale_y_continuous(limits=c(40, 110)) +
  geom_smooth(aes(life_expectancy, basic_water), method=lm, se=FALSE, color = "red")



# GNI vs LE
# Basic Water
ggplot(LED12) +
  geom_jitter(aes(life_expectancy, gni_capita), color = "green")+
  theme(axis.title.x = element_text(vjust = -4),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Life Expectancy with Gross National Income (2012)", y = "Life Expectancy", x = "Gross National Income") +
  scale_x_continuous(limits=c(72, 85)) +
  scale_y_continuous(limits=c(40, 80)) +
  geom_smooth(aes(life_expectancy, gni_capita), method=lm, se=FALSE, color = "green")


# GGHE vs LE
# Basic Water
ggplot(LED12) +
  geom_jitter(aes(life_expectancy, gghe_d), color = "orange")+
  theme(axis.title.x = element_text(vjust = -4),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Life Expectancy with General Government Health Expenditure (2012)", y = "Life Expectancy", x = "Gov. Health Expend. as % of GDP") +
  geom_smooth(aes(life_expectancy, gghe_d), method=lm, se=FALSE, color = "orange")



# Residual analysis on first model
# Creating model
model <- lm(life_expectancy ~ 
                measles +
                basic_water +
                gni_capita +
                gghe_d,
                data = LED12)

summary(model)

# Checking for multicollinearity

vif(model)


# Summarizing residuals, non-standardized then standardized, studentized, and rstudentized.
# Summarize the actual residual values (not scaled)
summary(residuals(model))


# Summarize scaled residuals
summary(stdres(model))

summary(studres(model))

summary(rstudent(model))


# Drawing residual plots - standardized

# 99% cutoff
cor.99level <- 0.01 / 2
cor.99qt <- qt(cor.99level, 166 - 1, lower.tail = F)

# 95% cutoff
cor.95level <- 0.05 / 2
cor.95qt <- qt(cor.95level, 166 - 1, lower.tail = F)

# Plot standardized residuals
barplot(height = stdres(model),
        main = "Standardized Residuals", xlab = "Index", 
        ylab = "Standardized Resid", ylim = c(-5, 5)) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5)



# Studentized residuals
barplot(height = studres(model), 
        main = "Studentized Residuals", xlab = "Index", 
        ylab = "Studentized Resid", ylim = c(-5, 5)) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) + 
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5)



# R-student residuals
barplot(height = rstudent(model), 
        main = "R-Student Residuals", xlab = "Index", 
        ylab = "R-Student Resid", ylim = c(-5, 5)) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) + 
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5)


# Measure influence

modelInf <- influence.measures(model)
summary(modelInf)




# Influence plot

influenceIndexPlot(model)

# DFBETAS plot
dfbetasPlots(model)



# Histogram of studentized residuals

hist(studres(model), breaks = 14, freq = F, col = "cornflowerblue", 
     cex.axis = 1.1, cex.lab = 1.1, cex.main = 1.3) 




# Plot a QQ-plot

qqPlot(model)


# Residual plot using R-student residuals

residualPlot(model, type = "rstudent", quadratic = F, col = "cornflowerblue",
pch = 16, cex = 1.25, cex.axis = 1, cex.lab = 1)


# Residuals against the Regressor

residualPlots(model, type = "rstudent", fitted = F, quadratic = F,
             col = "cornflowerblue", 
             pch = 16, cex = 1.25, cex.axis = 1, cex.lab = 1)



# Determining transformations required
LED12.2 <- LED12[-c(35, 136, 139, 143, 145, 156, 171, 176, 179, 181, 182),]

# Graph the plot for alcohol
ggplot(LED12.2) +
  geom_jitter(aes(x = alcohol, y = life_expectancy), color = "blue") +
  theme(axis.title.x = element_text(vjust = -4),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Life Expectancy vs. Alcohol", y = "Life Expectancy", x = "Alcohol Consumption") +
  scale_x_continuous(limits=c(-1, 20)) +
  scale_y_continuous(limits=c(50, 87))

# Conclusion: No transformation needed



# Graph the plot for measles
ggplot(LED12.2) +
  geom_jitter(aes(x = measles, y = life_expectancy), color = "red") +
  theme(axis.title.x = element_text(vjust = -4),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Life Expectancy with Measles Vaccination", y = "Life Expectancy", x = "Measles")

# Conclusion: No transformation needed


# Graph the plot for basic water access
ggplot(LED12.2) +
  geom_jitter(aes(x = basic_water, y = life_expectancy), color = "dark blue") +
  theme(axis.title.x = element_text(vjust = -4),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Life Expectancy with Water Access", y = "Life Expectancy", x = "Water Access")

# Conclusion: No transformation needed





# Graph the plot for GNI per capita
ggplot(LED12.2) +
  geom_jitter(aes(x = gni_capita, y = life_expectancy), color = "orange") +
  theme(axis.title.x = element_text(vjust = -4),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Life Expectancy vs. GNI per Capita", y = "Life Expectancy", x = "GNI per Capita")

# Conclusion: Transformation preferred


# Graph the plot for GGHE
ggplot(LED12.2) +
  geom_jitter(aes(x = gghe_d, y = life_expectancy), color = "green") +
  theme(axis.title.x = element_text(vjust = -4),
        axis.title.y = element_text(vjust = 5),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "none")+
  labs(title = "Life Expectancy vs. Health Expend.", y = "Life Expectancy", x = "Gov. Health Expenditure")

# Comments:
# The distribution seems to have a slight curve to it, but given how the errors 
# were normally distributed with a constant variance for the majority of points,
# the transformation isn't quite necessary. The limited range of GGHE values
# makes it less necessary for a logarithmic-type transformation.
# 
# Conclusion: No transformation needed


# OVERALL CONCLUSION: We will transform the GNI per capita regressor by applying
# the natural log function to it. This will change the logarithmic distribution 
# of the points into a linear one. 



# Creating new transformed df
LEDtrans <- LED12
LEDtrans$ln_gni <- log(LEDtrans$gni_capita)




# Fitting new model

# Now fit the model, called trModel, with the newly transformed GNI variable
trModel <- lm(life_expectancy ~ 
                   alcohol +
                   measles +
                   basic_water +
                   ln_gni +
                   gghe_d,
                 data = LEDtrans)
# Summarize the model

summary(trModel)

# Multicollinearity measurements

# Show VIF's
vif(trModel)


# Standardized residuals
# 99% cutoff
cor.99level <- 0.01 / 2
cor.99qt <- qt(cor.99level, 155 - 1, lower.tail = F)
#
# 95% cutoff
cor.95level <- 0.05 / 2
cor.95qt <- qt(cor.95level, 155 - 1, lower.tail = F)
#
# Plot standardized residuals
barplot(height = stdres(trModel),
        main = "Standardized Residuals", xlab = "Index", 
        ylab = "Standardized Resid", ylim = c(-5, 5), cex.names = T) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5)
# 5 outliers for 99% range, 10 for 95%

# Studentized and R-student residuals
# Plot studentized residuals
barplot(height = studres(trModel), 
        main = "Studentized Residuals", xlab = "Index", 
        ylab = "Studentized Resid", ylim = c(-5, 5)) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) + 
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5)
# 6 outliers for 99% range, 10 for 95%
#
# Plot R-student residuals
barplot(height = rstudent(trModel), 
        main = "R-Student Residuals", xlab = "Index", 
        ylab = "R-Student Resid", ylim = c(-5, 5)) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) + 
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5)
# 6 outliers for 99% range, 10 for 95%

# Influence measurements
# Finding influence on Cook's D, hat values, DFFITS, DFBETAS, COVRATIO 
modelInf.tr <- influence.measures(trModel)
summary(modelInf.tr)

# Influence graphs
# Plot graphs of hat values, Studentized residuals, and Cook's D
influenceIndexPlot(trModel, vars=c("Cook", "Studentized", "hat"))

# DFBETAS
# Plot graphs of DFBETAS values
dfbetasPlots(trModel)

# Normal probability plot of Studentized residuals
hist(studres(trModel), breaks = 14, freq = F, col = "cornflowerblue", 
     cex.axis = 1.1, cex.lab = 1.1, cex.main = 1.3)

# QQPlot
# Plot a QQ-plot
qqPlot(trModel)


# Residual plots
# Residual plot using R-student residuals
residualPlot(trModel, type = "rstudent", quadratic = F, col = "cornflowerblue",
pch = 16, cex = 1.25, cex.axis = 1, cex.lab = 1)

# Residual plot against the Regressors using R-student residuals
residualPlots(trModel, type = "rstudent", fitted = F, quadratic = F,
             col = "cornflowerblue", 
             pch = 16, cex = 1.25, cex.axis = 1, cex.lab = 1)


# Removing outliers

which(studres(trModel) > cor.99qt) # Sample 143 has a large positive error
which(studres(trModel) < -cor.99qt) # Samples 145, 156, 176, 179, 181

# Create a data set with the intersection of the above results removed
LEDtrans.2 <- LEDtrans[-c(143, 145, 156, 176, 179, 181, 182), ]

# Fitting new model
# Now fit the model, called trModel, with the newly transformed GNI variable
trModel.2 <- lm(life_expectancy ~ 
                   alcohol +
                   measles +
                   basic_water +
                   ln_gni +
                   gghe_d,
                 data = LEDtrans.2)
# Summarize the model
summary(trModel.2)


# Plotting new model resid.
# Show VIF's
vif(trModel.2)
#
# Comments:
# Although slight, there is an overall decrease in multicollinearity

# Plot Residuals
#
# 99% cutoff
cor.99level <- 0.01 / 2
cor.99qt <- qt(cor.99level, 155 - 1, lower.tail = F)
#
# 95% cutoff
cor.95level <- 0.05 / 2
cor.95qt <- qt(cor.95level, 155 - 1, lower.tail = F)
#
# Plot standardized residuals
barplot(height = stdres(trModel.2),
        main = "Standardized Residuals", xlab = "Index", 
        ylab = "Standardized Resid", ylim = c(-5, 5), cex.names = T) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = 3, col = "Purple", lwd = 1.5) +
  abline(h = -3, col = "Purple", lwd = 1.5)
# 2 outliers for 99% range, 8 for 95%
#
# Plot studentized residuals
barplot(height = studres(trModel.2), 
        main = "Studentized Residuals", xlab = "Index", 
        ylab = "Studentized Resid", ylim = c(-5, 5)) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) + 
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = 3, col = "Purple", lwd = 1.5) +
  abline(h = -3, col = "Purple", lwd = 1.5)
# 2 outliers for 99% range, 9 for 95%
#
# Plot R-student residuals
barplot(height = rstudent(trModel.2), 
        main = "R-Student Residuals", xlab = "Index", 
        ylab = "R-Student Resid", ylim = c(-5, 5)) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) + 
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = 3, col = "Purple", lwd = 1.5) +
  abline(h = -3, col = "Purple", lwd = 1.5)
# 2 outliers for 99% range, 9 for 95%


# New model influence
# Finding influence on Cook's D, hat values, DFFITS, DFBETAS, COVRATIO 
modelInf.tr2 <- influence.measures(trModel.2)
summary(modelInf.tr2)

# Plot graphs of hat values, Studentized residuals, and Cook's D
influenceIndexPlot(trModel.2, vars=c("Cook", "Studentized", "hat"))
#

dfbetasPlots(trModel.2)


# Normal probability plot of Studentized residuals
hist(studres(trModel.2), breaks = 14, freq = F, col = "chocolate1", border = 1, 
     cex.axis = 1.1, cex.lab = 1.1, cex.main = 1.3, xlim = c(-3, 3), 
     main = "Normal Probability Plot of Transformed Model", 
     xlab = "Studentized Residuals t-values", 
     ylab = "Density")

# Plot a QQ-plot
qqPlot(trModel.2)
#


# Residual plots

residualPlot(trModel.2, type = "rstudent", quadratic = F, col = "cornflowerblue",
pch = 16, cex = 1.25, cex.axis = 1, cex.lab = 1)

# Residual plot against the Regressors using R-student residuals
residualPlots(trModel.2, type = "rstudent", fitted = F, quadratic = F,
             col = "darkgreen", 
             pch = 16, cex = 1.25, cex.axis = 1, cex.lab = 1)

# Further improvements of the model


# Removal of those countries
LEDtrans.3 <- LEDtrans.2 %>% 
  filter(country_code != "NAM") %>% 
  filter(country_code != "ZWE")

# Creation of model
trModel.3 <- lm(life_expectancy ~ 
                   alcohol +
                   measles +
                   basic_water +
                   ln_gni +
                   gghe_d,
                 data = LEDtrans.3)
# Summarize the model
summary(trModel.3)


# VIFs
# Show VIF's
vif(trModel.3)


# Plot Residuals

# 99% cutoff
cor.99level <- 0.01 / 2
cor.99qt <- qt(cor.99level, 155 - 1, lower.tail = F)

# 95% cutoff
cor.95level <- 0.05 / 2
cor.95qt <- qt(cor.95level, 155 - 1, lower.tail = F)

# Plot standardized residuals
barplot(height = stdres(trModel.3),
        main = "Standardized Residuals", xlab = "Index", 
        ylab = "Standardized Resid", ylim = c(-5, 5), cex.names = T) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = 3, col = "Purple", lwd = 1.5) +
  abline(h = -3, col = "Purple", lwd = 1.5)
# 0 outliers for 99% range, 8 for 95%

# Plot studentized residuals
barplot(height = studres(trModel.3), 
        main = "Studentized Residuals", xlab = "Index", 
        ylab = "Studentized Resid", ylim = c(-5, 5)) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) + 
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = 3, col = "Purple", lwd = 1.5) +
  abline(h = -3, col = "Purple", lwd = 1.5)
# 0 outliers for 99% range, 8 for 95%

# Plot R-student residuals
barplot(height = rstudent(trModel.3), 
        main = "R-Student Residuals", xlab = "Index", 
        ylab = "R-Student Resid", ylim = c(-5, 5)) + 
  abline(h = cor.99qt, col = "Red", lwd = 1.5) + 
  abline(h = -cor.99qt, col = "Red", lwd = 1.5) +
  abline(h = cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = -cor.95qt, col = "Blue", lwd = 1.5) +
  abline(h = 3, col = "Purple", lwd = 1.5) +
  abline(h = -3, col = "Purple", lwd = 1.5)
# 0 outliers for 99% range, 8 for 95%

# Measuring influence
# Finding influence on Cook's D, hat values, DFFITS, DFBETAS, COVRATIO 
modelInf.tr3 <- influence.measures(trModel.3)
summary(modelInf.tr3)


# Influence Index
# Plot graphs of hat values, Studentized residuals, and Cook's D
influenceIndexPlot(trModel.3, vars=c("Cook", "Studentized", "hat"))


# DFBETAS
dfbetasPlots(trModel.3)


# Probability plots of residuals
# Normal probability plot of Studentized residuals
hist(studres(trModel.3), breaks = 14, freq = F, col = "chocolate1", border = 1, 
     cex.axis = 1.1, cex.lab = 1.1, cex.main = 1.3, xlim = c(-3, 3), 
     main = "Normal Probability Plot of Transformed Model", 
     xlab = "Studentized Residuals t-values", 
     ylab = "Density")
#
# Comments:
# Similar trend to before where positive residuals follow the normality more 
# than the negative residuals, but plot is still acceptable

# Plot a QQ-plot
qqPlot(trModel.3)
#
# Comments:
# Similar QQ-plot, most points in the acceptability range, no signs of non-normal
# distributions

# Residual plot using R-student residuals
residualPlot(trModel.3, type = "rstudent", quadratic = F, col = "cornflowerblue",
pch = 16, cex = 1.25, cex.axis = 1, cex.lab = 1)
#
# Comments:
# Constant variance is from around ~-2.5 to ~2.5, variance relatively constant

# Residual plot against the Regressors using R-student residuals
residualPlots(trModel.3, type = "rstudent", fitted = F, quadratic = F,
             col = "darkgreen", 
             pch = 16, cex = 1.25, cex.axis = 1, cex.lab = 1)
#
# Comments:
# Same trends are seen, not much difference

