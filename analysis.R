setwd(choose.dir())
library(tidyverse)
library(caTools)
library(ggpubr)

cafos <- read.csv("cafos.csv")

#initial scatter plot
ggplot(cafos) + 
  geom_smooth(mapping = aes(x = totalanimalunits, y = Cases.Per.10000.Residents)) + 
  geom_point(mapping = aes(x = totalanimalunits, y = Cases.Per.10000.Residents, alpha = Population))

#doing a bivaritate regression to test signifigance
linear <-  lm(Cases.Per.10000.Residents~totalanimalunits, data = cafos)
summary(linear)

#testing to see if the data is distributed normally
#shapiro test to see if the data follow normal distribution. If it is greater than 0.5 than it does
shapiro.test(cafos$totalanimalunits) # p value is less than 0.05 so it is NOT normal distribution 
#it is <2.2e-16 so it is not normal distribution so can not use pearson correlation coefficient
#have to use spearman or kendall
shapiro.test(cafos$Cases.Per.10000.Residents) #p-value <2.2e-16
shapiro.test(cafos$MortalityRate)# p-value < 2.2e-16


#after the shapiro tests we know that our data is not normally distributed we have to use spearman
casesper10000result <- cor.test(cafos$totalanimalunits, cafos$Cases.Per.10000.Residents, method = "spearman") # R = 0.27
mortalityrateresult <- cor.test(cafos$totalanimalunits, cafos$MortalityRate, method = "spearman") # R = 0.19

#wanted to test pearson correlation coefficient just to see how different the correlations would be
cor(cafos$totalanimalunits, cafos$Cases.Per.10000.Residents, method = "pearson") #0.34
cor(cafos$totalanimalunits, cafos$MortalityRate, method = "pearson")#0.15


#creating the visulations of the correlations
ggscatter(cafos, x = "totalanimalunits", y = "Cases.Per.10000.Residents", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Animal Units (1,000 lbs)", ylab = "Cases Per 10,000 Per Zipcode", alpha = 0.1, color = "blue")

ggscatter(cafos, x = "totalanimalunits", y = "deathsovercases", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Animal Units (1,000 lbs)", ylab = "Deaths over Cases Per Zipcode ", alpha = 0.1, color = "blue")

ggscatter(cafos, x = "totalanimalunits", y = "MortalityRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Animal Units (1,000 lbs)", ylab = "Mortality Rate Per Zip Code", alpha = 0.1, color = "blue")


#I saw that there were 502 of zipcodes that had 0 large Cafos in them filtered all of those out. 
cafos3 <- filter(cafos, totalanimalunits != 0) 

#did the same test for dataset with zipcodes filtered out as the ones with all
linear <-  lm(Cases.Per.10000.Residents~totalanimalunits, data = cafos3)
summary(linear)

ggscatter(cafos3, x = "totalanimalunits", y = "Cases.Per.10000.Residents", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Animal Units (1,000 lbs)", ylab = "Cases Per 10,000 Per Zipcode", alpha = 0.1, color = "blue")
