rm(list=ls())

# Austria all emissions categories -------------------------------------------

emissions_codes <- read.csv("Data/01 Source_data/IPCC 2006 Categories.csv") %>% 
  select(IPCC, IPCC_description)

data_full <- read.csv("Data/02 Intermediary_data/CO2DriversEU_dataset.csv") %>% 
  left_join(emissions_codes,
            by = c("category" = "IPCC")) %>% 
  filter(!is.na(lemissions_pc))

#emissions on same scale
ggplot(data = subset(data_full, country == "Austria")) +
  geom_line(mapping = aes(x = year, y = emissions_pc)) +
  facet_wrap(~ IPCC_description) +
  labs(title = "Emissions per capita for Austria",
       xlab = "Year",
       ylab = "Emissions per capita")

#emissions on individual scales
ggplot(data = subset(data_full, country == "Austria")) +
  geom_line(mapping = aes(x = year, y = emissions_pc)) +
  facet_wrap(~ IPCC_description, scale = "free") +
  labs(title = "Emissions per capita for Austria",
       xlab = "Year",
       ylab = "Emissions per capita")


