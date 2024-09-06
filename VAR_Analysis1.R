library(tidyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(stargazer)
library(xtable)
library(knitr)
library(kableExtra)
library(xts)
library(rugarch)
library(zoo)

# ---------------------------------------

# Prepare Political Risk Series

series_pol_risk = read.csv('C:/Users/dadaset/Downloads/series_political_risk ts.csv', header = T)

#since the financial data is only since 2000 we drop the first two obs

series_pol_risk<-series_pol_risk[-c(1,2), ]   

# now create a obs for 2001 with mean

vectorr <- c(2001, -0.195, -1.545, -1.04)
names(vectorr)<-c('year','Libya','Nigeria','Venezuela')
series_pol_risk<-rbind(series_pol_risk,vectorr)

# Prepare Regulatory Quality

regulatory_index = read.csv('C:/Users/dadaset/Desktop/ts job/Regulatory Quality ts.csv', header = T)

# Prepare Energy Prices

energy_prices = read.csv('C:/Users/dadaset/Desktop/ts job/Energy prices.csv', header = T)

energy_prices$Dates <- str_replace_all(energy_prices$Dates, '/', '-')

dates.vector <-dplyr::pull(energy_prices, Dates)

class(dates.vector)

vector.date.format <-mdy(dates.vector)
class(vector.date.format)

energy_prices <- cbind(energy_prices, vector.date.format)

class(energy_prices$vector.date.format)

  energy_prices$Brent.Crude <- str_replace_all(energy_prices$Brent.Crude, ',', '.')
  energy_prices$WTI.Crude <- str_replace_all(energy_prices$WTI.Crude, ',', '.')
  energy_prices$Europe.Natural.Gas <- str_replace_all(energy_prices$Europe.Natural.Gas, ',', '.')
  energy_prices$Asia..Gas...JKM. <- str_replace_all(energy_prices$Asia..Gas...JKM., ',', '.')
  energy_prices$US.Gas.prices..Henry.Hub. <- str_replace_all(energy_prices$US.Gas.prices..Henry.Hub., ',', '.')
  energy_prices$Europe.Coal.Prices <- str_replace_all(energy_prices$Europe.Coal.Prices, ',', '.')
  energy_prices$Australia.Coal.Prices <- str_replace_all(energy_prices$Australia.Coal.Prices, ',', '.')
  
  energy_prices$Brent.Crude               <-as.numeric(energy_prices$Brent.Crude              ) 
  energy_prices$WTI.Crude                 <-as.numeric(energy_prices$WTI.Crude                )  
  energy_prices$Europe.Natural.Gas        <-as.numeric(energy_prices$Europe.Natural.Gas       ) 
  energy_prices$Asia..Gas...JKM.          <-as.numeric(energy_prices$Asia..Gas...JKM.         ) 
  energy_prices$US.Gas.prices..Henry.Hub. <-as.numeric(energy_prices$US.Gas.prices..Henry.Hub.) 
  energy_prices$Europe.Coal.Prices        <-as.numeric(energy_prices$Europe.Coal.Prices       ) 
  energy_prices$Australia.Coal.Prices     <-as.numeric(energy_prices$Australia.Coal.Prices    ) 
  
# remove row 37 beause date is 1900
  energy_prices <- energy_prices[-c(37), ]
# shortening the name of us gas prices 
  colnames(energy_prices)[6] <- "US Gas Prices"
  
  #generate summaries of the energy prices for latex tables
  
xtable(summary(energy_prices[,-c(1,9)]))
  

stargazer(summary(energy_prices))

kable(energy_prices, "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
  
# convert number columns to numeric

energy_prices$Dates <- str_replace_all(energy_prices$Dates, '/', '-')

energy_prices$Brent.Crude <- as.numeric(energy_prices$Brent.Crude)


# for now u made a date column with the dates, lets try to aggregate em

# Convert the Dates column to Date format
energy_prices <- energy_prices %>%
  mutate(Dates = as.Date(Dates, format = "%Y-%m-%d"))


#save energy prices table in csv

write.csv(energy_prices,"C:/Users/dadaset/Desktop/ts job/energy_prices_table.csv", row.names = FALSE)


# Creating pure TS Tables for each price

    # BRENT

BRENT_TS <-cbind(energy_prices$Dates, energy_prices$Brent.Crude) 

BRENT_TS <- as.data.frame(BRENT_TS)
BRENT_TS$V1 <- mdy(BRENT_TS$V1)
BRENT_TS$V2 <- as.numeric(BRENT_TS$V2)

BRENT_TS2 <- BRENT_TS
BRENT_TS2$year <-strftime(BRENT_TS2$V1, '%Y')

BRENT_AGGR <- aggregate(V2 ~ year,
                        BRENT_TS2,
                        FUN = mean)

      # WTI 
WTI_TS <-cbind(energy_prices$Dates, energy_prices$WTI.Crude) 

WTI_TS <- as.data.frame(WTI_TS)
WTI_TS$V1 <- mdy(WTI_TS$V1)
WTI_TS$V2 <- as.numeric(WTI_TS$V2)

WTI_TS2 <- WTI_TS
WTI_TS2$year <-strftime(WTI_TS2$V1, '%Y')

WTI_AGGR <- aggregate(V2 ~ year,
                        WTI_TS2,
                        FUN = mean)
    # US GAS PRICES

US_TS <-cbind(energy_prices$Dates, energy_prices$`US Gas Prices`) 

US_TS <- as.data.frame(US_TS)
US_TS$V1 <- mdy(US_TS$V1)
US_TS$V2 <- as.numeric(US_TS$V2)

US_TS2 <- US_TS
US_TS2$year <-strftime(US_TS2$V1, '%Y')

US_AGGR <- aggregate(V2 ~ year,
                      US_TS2,
                      FUN = mean)
  # EUROPE

EU_TS <-cbind(energy_prices$Dates, energy_prices$Europe.Natural.Gas) 

EU_TS <- as.data.frame(EU_TS)
EU_TS$V1 <- mdy(EU_TS$V1)
EU_TS$V2 <- as.numeric(EU_TS$V2)

EU_TS2 <- EU_TS
EU_TS2$year <-strftime(EU_TS2$V1, '%Y')

EU_AGGR <- aggregate(V2 ~ year,
                     EU_TS2,
                     FUN = mean)
  # ASIA

ASIA_TS <-cbind(energy_prices$Dates, energy_prices$Asia..Gas...JKM.) 

ASIA_TS <- as.data.frame(ASIA_TS)
ASIA_TS$V1 <- mdy(ASIA_TS$V1)
ASIA_TS$V2 <- as.numeric(ASIA_TS$V2)

ASIA_TS2 <- ASIA_TS
ASIA_TS2$year <-strftime(ASIA_TS2$V1, '%Y')

ASIA_AGGR <- aggregate(V2 ~ year,
                     ASIA_TS2,
                     FUN = mean)



  # EUROPEAN COAL

EU_COAL_TS <-cbind(energy_prices$Dates, energy_prices$Europe.Coal.Prices) 

EU_COAL_TS <- as.data.frame(EU_COAL_TS)
EU_COAL_TS$V1 <- mdy(EU_COAL_TS$V1)
EU_COAL_TS$V2 <- as.numeric(EU_COAL_TS$V2)

EU_COAL_TS2 <- EU_COAL_TS
EU_COAL_TS2$year <-strftime(EU_COAL_TS2$V1, '%Y')

EU_COAL_AGGR <- aggregate(V2 ~ year,
                       EU_COAL_TS2,
                       FUN = mean)



# AUST COAL

AUST_COAL <-cbind(energy_prices$Dates, energy_prices$Australia.Coal.Prices) 

AUST_COAL <- as.data.frame(AUST_COAL)
AUST_COAL$V1 <- mdy(AUST_COAL$V1)
AUST_COAL$V2 <- as.numeric(AUST_COAL$V2)

AUST_COAL2 <- AUST_COAL
AUST_COAL2$year <-strftime(AUST_COAL2$V1, '%Y')

AUST_COAL_AGGR <- aggregate(V2 ~ year,
                          AUST_COAL2,
                          FUN = mean)



### lets calculate the return and volatility of each variable, IT WORKED, WE CAN DO WITH THE OTHER PRICES LATER
#aus

AUST_COAL_AGGR_LOG <- AUST_COAL_AGGR %>%
  arrange(year) %>%
  mutate(across(V2, ~ log(. / lag(.)), .names = "return_{col}"))

# Calculate log returns for political risk (if applicable)



# MERGE AUS COAL PRICES WITH POLITICAL RISK

analysis_data <- merge(AUST_COAL_AGGR_LOG, series_pol_risk, by = "year")

#removing first line because first log of the prices is NA

analysis_data <- analysis_data[-1,]

analysis_data$Libya <- as.numeric(gsub(",", ".", analysis_data$Libya))

analysis_data$Nigeria <- as.numeric(gsub(",", ".", analysis_data$Nigeria))

analysis_data$Venezuela <- as.numeric(gsub(",", ".", analysis_data$Venezuela))

analysis_data <- analysis_data %>%
  mutate(log_return_political_risk_Lybia = log(Libya / lag(Libya)))

analysis_data <- analysis_data %>%
  mutate(log_return_political_risk_Nigeria = log(Libya / lag(Nigeria)))

analysis_data <- analysis_data %>%
  mutate(log_return_political_risk_Venezuela = log(Libya / lag(Venezuela)))




### lets forget about GARCH, we are using rolling volatilty now


# Rolling volatility for Australian coal prices
analysis_data$rolling_vol_coal <- rollapply(analysis_data$return_V2, width = 3, FUN = sd, fill = NA, align = "right")

# Rolling volatility for Political Risk Index
  analysis_data$rolling_vol_risk_Libya <- rollapply(analysis_data$log_return_political_risk_Lybia, width = 3, FUN = sd, fill = NA, align = "right")



ggplot(analysis_data, aes(x = year)) +
  geom_line(aes(y = rolling_vol_coal, color = "Coal Volatility")) +
  geom_line(aes(y = rolling_vol_risk_Libya, color = "Libya Political Risk")) +
  labs(title = "Rolling Volatility of Australian Coal Prices and Lybia Political Risk", x = "year", y = "Volatility") +
  theme_minimal()

#deixa pra la essa volatilidade vamo no arroz com feijao, lm


regression_model <- lm(rolling_vol_coal ~ rolling_vol_risk_Libya, data = analysis_data)
  
summary(regression_model)


stargazer(regression_model)


























