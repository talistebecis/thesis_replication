# Overall data creation TT ------------------------------------------------
rm(list=ls())

### data assembly

set.seed(123)

EU31 <- c("Austria", "Croatia", "Belgium", "Bulgaria", "Cyprus", 
          "Czech Republic", "Germany", "Denmark", "Spain", "Estonia", "Finland",
          "France", "United Kingdom", "Greece", "Hungary", "Ireland", "Italy",
          "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland",
          "Portugal", "Romania", "Slovak Republic", "Slovenia", "Sweden", 
          "Switzerland", "Iceland", "Norway")

#### Import and merge Data
gdp <- read.csv("Data/01 Source_data/WB_gdpconst.csv", skip = 3) %>%
  filter(Country.Name %in% EU31) %>%
  select(c(1,14:63)) %>%
  pivot_longer(2:51, names_to = "year") %>%
  transmute(country=Country.Name, year=as.numeric(str_remove(year, "X")),
            gdp=value)

pop <- read.csv("Data/01 Source_data/WB_totpop.csv", skip=3) %>%
  filter(Country.Name %in% EU31) %>%
  select(c(1,13:63)) %>%
  pivot_longer(5:52, names_to = "year") %>%
  transmute(country=Country.Name, year=as.numeric(str_remove(year, "X")),
            pop=value)

emissions_total <- read_excel("Data/01 Source_data/v50_CO2_excl_short-cycle_org_C_1970_2018.xls",
                              range = "v5.0_EM_CO2_IPCC2006!A10:BC3373") %>% 
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic", Name)) %>%
  filter(Name %in% EU31) %>% 
  pivot_longer(7:55, "year") %>%
  transmute(country=Name,
            year=as.numeric(str_remove(year, "X")),
            emissions=as.numeric(value),
            category=IPCC)

data <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(emissions_total, c("country", "year"))


#### Transform Variables
data$lgdp <- log(data$gdp)
data$lpop <- log(data$pop)
data$lemissions <- log(data$emissions)
data$const <- 1
data$lgdp_sq <- data$lgdp^2
data$emissions_pc <- data$emissions/data$pop
data$lemissions_pc <- log(data$emissions_pc)

#### Output
write.csv(data, here("Data/02 Intermediary_data/CO2DriversEU_dataset.csv"), row.names = F)

