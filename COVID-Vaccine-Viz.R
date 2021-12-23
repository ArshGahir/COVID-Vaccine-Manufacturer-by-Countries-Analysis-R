
#Loading Required Packages

pkgs <- c("ggplot2","tidyverse", "scales", "plyr", "sjmisc")
install.packages(pkgs, repos = "http://cran.us.r-project.org")

library(plyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(sjmisc)



#Reading Data
covid <- read.csv("/Users/arshveergahir/Desktop/Semester 3/BDM 300/Project/country_vaccinations.csv")
income <- read.csv("/Users/arshveergahir/Desktop/Semester 3/BDM 300/Project/income_ranking.csv")

#Selecting last value of each group(Country) for all data
covid_totals <- covid %>%
                group_by(country)%>%
                do(tail(., 1))

#Replacing missing values with 0
covid_totals_clean <- covid_totals
covid_totals_clean[is.na(covid_totals_clean)] <- 0

covid_income_merged <- merge(covid_totals_clean, income, by ='country')

#Top Countries with most vaccinations

covid_income_merged %>%
  group_by(country, total_vaccinations)%>%
  arrange(desc(total_vaccinations))%>%
  head(10)%>%
  ggplot(aes(x = reorder(country, total_vaccinations), y = total_vaccinations, fill = IncomeGroup)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  scale_y_continuous(labels = comma) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 72, size = 15, hjust = 1), axis.text.y = element_text(size = 15))+
  labs(title = "Top Countries with Most Vaccinations", y = "Total Vaccinations", x = "Countries")

###Which countries have the most fully vaccinated people?
  
covid_income_merged %>%
  group_by(country, people_fully_vaccinated)%>%
  arrange(desc(people_fully_vaccinated))%>%
  head(10)%>%
  ggplot(aes(x = reorder(country, people_fully_vaccinated), y = people_fully_vaccinated, fill = IncomeGroup)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  scale_y_continuous(labels = comma) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 72, size = 15, hjust = 1), axis.text.y = element_text(size = 15))+
  labs(title = "Top Countries with Most Fully Vaccinated People", 
       y = "Total People", x = "Countries")


###Which vaccine is the most popular/in use

countries_using_AZ <- filter(covid_income_merged, grepl('Oxford/AstraZeneca', vaccines))
countries_using_Pf <- filter(covid_income_merged, grepl('Pfizer', vaccines))
countries_using_Sino <- filter(covid_income_merged, grepl('Sino', vaccines))     
countries_using_Sputnik <- filter(covid_income_merged, grepl('Sputnik', vaccines))
countries_using_Moderna <- filter(covid_income_merged, grepl('Moderna', vaccines))
countries_using_CoV <- filter(covid_income_merged, grepl('Covaxin', vaccines))
countries_using_JnJ <- filter(covid_income_merged, grepl('Johnson', vaccines))

#Creating dataframe from no. of rows
vaccine_country_popularity <- data.frame(vaccine = c("Oxford/AstraZeneca", "Pfizer/BioNTech", "Sino", "Sputnik V", "Moderna", "Covaxin", "Johnson&Johnson"),
                              num_countries_using = c(nrow(countries_using_AZ), nrow(countries_using_Pf), nrow(countries_using_Sino), 
                                                  nrow(countries_using_Sputnik), nrow(countries_using_Moderna), nrow(countries_using_CoV), nrow(countries_using_JnJ)))

vaccine_country_popularity %>%
  ggplot(aes(x = reorder(vaccine, -num_countries_using), y = num_countries_using, fill = vaccine)) +
  geom_bar(stat = "identity") +
  labs(title = "Vaccines by Popularity", x = "Vaccines/Manufacturers", y = "Number of Countries ") +
  theme(axis.text.x = element_text(angle = 72, size = 15, hjust = 1), , axis.text.y = element_text(size = 15))


covid_vc <- covid_income_merged %>%
  separate(col = vaccines, into = c('Vaccine_1', 'Vaccine_2', 'Vaccine_3', 'Vaccine_4', 'Vaccine_5'), sep = ",")

covid_vc%>%
  ggplot(aes(y = Vaccine_1, x = IncomeGroup)) +
  geom_point(stat = "identity")+
  theme(axis.text.x = element_text(angle = 72, size = 15, hjust = 1), , axis.text.y = element_text(size = 15)) +
  labs(title = "Vaccines by Country Income", x = "Income", y = "Vaccines")
  