setwd(choose.dir())
nccafos <- read.csv("nccafos2.csv")

library(tidyverse)

nccafos1 <- group_by(nccafos, Regulated.Activity) %>%
            summarise(n_farms = n())


#getting rid of stuff that has no counts of animals and stuff that is not right t
#type of animals
nccafos4 <- filter(nccafos, !is.na(Allowable.Count)) %>%
  filter(., !Regulated.Operation == "Other Animals") %>%
  filter(., !Allowable.Count == 0)


#list of all of the animal units multiples
animals <- sapply(nccafos4$Regulated.Activity, myfun)


#creating the animal units field 
nccafos5 <- mutate(nccafos4, animalunits = Allowable.Count * 
                     unlist(animals))

#getting the zip code data all nice
nccafos6 <- nccafos5 %>% 
  filter(., !Zip == "d")
zipcode <- substr(nccafos6$Zip, start = 0, stop = 5)
nccafos7 <- mutate(nccafos6, zipcode1 = unlist(zipcode))

nccafos10 <- mutate(nccafos7, zipcounty = paste(zipcode1, County.Name, sep = ""))


nccafos30 <- filter(nccafos10, animalunits >1000)
#grouping by zipcode/ and summing all of the animal units
#nccafos11 <- group_by(nccafos10, zipcounty) %>%
#  summarise(totalanimalunits = sum(animalunits))
# <- mutate(nccafos11, Zip.Code = gsub(
#  "\\D", "", zipcounty))
#nccafos13 <- mutate(nccafos12, County = gsub("\\d", "", zipcounty))
#nccafos14 <- select(nccafos13, -zipcounty)
#write.csv(nccafos14, "C:/Aaron/trees/nccafos/filename.csv")
#nccafos14$Zip.Code <- as.numeric(nccafos14$Zip.Code)


nccafos11 <- group_by(nccafos30, zipcode1) %>%
   summarise(totalanimalunits = sum(animalunits))

nccafos11$Zip.Code <- as.numeric(nccafos11$Zip.Code)

colnames(nccafos11)[which(names(nccafos11) == "zipcode1")] <- "Zip.Code"



#inputting the covid data
covid <- read.csv("nczipcode1.csv")
population <- read.csv("ncpopulation.csv")
colnames(covid)[which(names(covid) == "ZIP.Code")] <- "Zip.Code"
#zipandcounty <- read.csv("zipcounty1.csv")

#putting all the datasets into 1
combined40 <- population %>% 
  left_join(covid, by = "Zip.Code") %>%
  left_join(nccafos11, by = "Zip.Code")
  #left_join(zipandcounty, by = "Zip.Code") %>%
  #select(-County.x)
#changing column name
#colnames(combined)[which(names(combined) == "County.y")] <- "County"

#getting rid of all of the ones with no animal units
combined1 <- filter(combined, !is.na(totalanimalunits))

#saving this file so I can run t tests rory sent online and what not.
#csv(combined1, "C:/Aaron/trees/nccafos/combined1.csv")

#creating the visulaization
ggplot(combined1) + 
  geom_smooth(mapping = aes(x = totalanimalunits, y = Cases.Per.10000.Residents)) + 
  geom_point(mapping = aes(x = totalanimalunits, y = Cases.Per.10000.Residents))


#seeing if deaths over cases changes
combined1$Cases <- as.numeric(combined1$Cases)
combined2 <- mutate(combined1, deathsovercases = Deaths/Cases)

ggplot(combined2) + 
  geom_smooth(mapping = aes(x = totalanimalunits, y = deathsovercases))  +
  geom_point(mapping = aes(x = totalanimalunits, y = deathsovercases))


#getting the population in correct format
#write.csv(combined2, "C:/Aaron/trees/nccafos/combined2.csv")
combined3 <- read.csv("combined3.csv")
combined3$Population <- gsub("\\D", "", combined3$Population)
combined3$Population <- as.numeric(combined3$Population)

#graphs that have an alpha to tell apart populatio
ggplot(combined3) + 
  geom_smooth(mapping = aes(x = totalanimalunits, y = Cases.Per.10000.Residents)) + 
  geom_point(mapping = aes(x = totalanimalunits, y = Cases.Per.10000.Residents, alpha = Population))

ggplot(combined3) + 
  geom_smooth(mapping = aes(x = totalanimalunits, y = deathsovercases)) + 
  geom_point(mapping = aes(x = totalanimalunits, y = deathsovercases, alpha = Population))

#creating deaths over 10000 population numbers
combined4 <- mutate(combined3, MortalityRate = (Deaths*100)/Population)


ggplot(combined4) + 
  geom_smooth(mapping = aes(x = totalanimalunits, y = MortalityRate)) + 
  geom_point(mapping = aes(x = totalanimalunits, y = MortalityRate, alpha = Population))


t.test(combined4$Deaths, combined4$Population)

plot(density(combined4$totalanimalunits))

cor(combined4$totalanimalunits, combined4$Cases.Per.10000.Residents)
cor(combined4$totalanimalunits, combined4$MortalityRate)


install.packages("ggpubr")

library("ggpubr")
ggscatter(combined4, x = "totalanimalunits", y = "Cases.Per.10000.Residents", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Animal Units (1,000 lbs)", ylab = "Cases Per 10,000")





#getting ready for the t test things
caseslist <- gsub("\\D", "", combined$Cases)
caseslist1 <- as.numeric(caseslist)


#getting stuff ready for the t test
#write.csv(combined, "C:/Aaron/trees/nccafos/ttestcombined.csv")
ttest <- read.csv("ttestcombined1.csv")
poplist <- gsub("\\D", "", ttest$Population)
combined10 <- select(combined, -Population)
combined11 <- select(combined10, -Cases)
combined12 <- combined11 %>% mutate (Cases = unlist(caseslist1)) %>%
  mutate(Population = unlist(poplist))
#write.csv(combined12, "C:/Aaron/trees/nccafos/ttestcombine2.csv")
ttest10 <- read.csv("ttestcombine2.csv")

ttest20 <- ttest10 %>%
  mutate(deathsovercases = Deaths/Cases) %>%
  mutate(MortalityRate = (Deaths*100)/Population)

ttest30 <- filter(ttest20, !is.na(Deaths))


ttest21 <- filter(ttest20, is.na(totalanimalunits))
ttest22 <- filter(ttest20, !is.na(totalanimalunits))

write.csv(ttest21, "C:/Aaron/trees/nccafos/ttest21.csv")
write.csv(ttest22, "C:/Aaron/trees/nccafos/ttest22.csv")

write.csv(combined40, "C:/Aaron/trees/nccafoscorrect/restart.csv")

write.csv(combined40, "C:/Aaron/trees/nconlycafos/restart.csv")





multiplier <- function(x) {
  switch(x, 
         "Cattle - Beef Brood Cow" = 1.0,
         "Cattle - Beef Feeder" = 1.0,
         "Cattle - Beef Stocker Calf" = 0.6,
         "Cattle - Dairy Calf" = 0.6,
         "Cattle - Dairy Heifer"= 1.4, 	
         "Cattle - Dry Cow" = 1.0,
         "Cattle - Milk Cow" = 1.4,
         "Dry Poultry - Laying Chickens" = 0.03, 
         "Horses - Horses" = 2,  	
         "Horses - Other" = 2,	
         "Swine - Boar/Stud" = 0.4,  	
         "Swine - Farrow to Feeder" = 0.03, 	
         "Swine - Farrow to Finish" = 0.4, 	
         "Swine - Farrow to Wean" = 0.03, 	
         "Swine - Feeder to Finish" = 0.4,	
         "Swine - Gilts" = 0.4, 
         "Swine - Other" = 0.03, 	
         "Swine - Wean to Feeder" = 0.03, 	
         "Swine - Wean to Finish" = 0.4, 	
         "Wet Poultry - Layers" = 0.03,	
         "Wet Poultry - Non Laying Pullets" = 0.03,	
         "Wet Poultry - Other" = 0.03
)}


#nccafos1 <- nccafos[!duplicated(nccafos$Facility.Name),]