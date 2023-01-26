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

energy.em <- read_excel("Data/01 Source_data/v50_CO2_excl_short-cycle_org_C_1970_2018.xls",
                        range = "v5.0_EM_CO2_IPCC2006!A10:BC2284") %>% 
  mutate(Name = ifelse(Name=="Slovakia", "Slovak Republic", Name)) %>%
  filter(Name %in% EU31, IPCC=="1.A.1.a") %>%
  pivot_longer(7:55, "year") %>%
  transmute(country=Name, year=as.numeric(str_remove(year, "X")),
            energy.emissions=as.numeric(value))

data_energy <- left_join(gdp, pop, c("country", "year")) %>%
  left_join(energy.em, c("country", "year"))


#### Transform Variables
data_energy$lgdp <- log(data_energy$gdp)
data_energy$lpop <- log(data_energy$pop)
data_energy$lenergy.emissions <- log(data_energy$energy.emissions)
data_energy$const <- 1


#### Level Variables
data_energy <- as.data.table(data_energy)
data_energy[, L1.lenergy.emissions:=c(NA, lenergy.emissions[-.N]), by="country"]
data_energy[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
data_energy[, L1.lpop:=c(NA, lpop[-.N]), by="country"]


#### Output
write.csv(data_energy, "Energy_DriversEU_dataset.csv", row.names = F)


### clean working directory
rm(gdp, pop, energy.em)


# Analysis ----------------------------------------------------------------

### analysis

set.seed(1230) 

ltitle="Results_Energy_Drivers_Analysis.txt"


# Prepare Data

data <- read.csv("Energy_DriversEU_dataset.csv")
data$lgdp_sq <- data$lgdp^2

data <- as.data.table(data)

data$energy.emissions_pc <- data$energy.emissions/data$pop
data$lenergy.emissions_pc <- log(data$energy.emissions_pc)
data[, L1.lenergy.emissions_pc:=c(NA, lenergy.emissions_pc[-.N]), by="country"]

# Group specification
EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")
EU16 <- c("Croatia", "Bulgaria", "Cyprus","Czech Republic", "Estonia", 
          "Hungary", "Lithuania", "Latvia", "Malta", "Poland", "Romania", 
          "Slovak Republic", "Slovenia", "Switzerland", "Iceland", 
          "Norway")
EU31 <- c(EU15, EU16)

# Heterogenous effects preparation
group.interactions <- c("lgdp_EU15", "lgdp_EU16", "lgdpsq_EU15", "lgdpsq_EU16",
                        "lpop_EU15", "lpop_EU16")
data$lgdp_EU15 <- data$lgdp * (data$country %in% EU15)
data$lgdp_EU16 <- data$lgdp * (data$country %in% EU16)
data$lgdpsq_EU15 <- data$lgdp_sq * (data$country %in% EU15)
data$lgdpsq_EU16 <- data$lgdp_sq * (data$country %in% EU16)
data$lpop_EU15 <- data$lpop * (data$country %in% EU15)
data$lpop_EU16 <- data$lpop * (data$country %in% EU16)


###### Analysis:

cat(
  paste0(
    "#################################################################### \n",
    "#                                                                  # \n",
    "#               CO2 DRIVERS EU (ENERGY) - ANALYSIS                 # \n",
    "#                                                                  # \n",
    "#################################################################### \n",
    "\n \n \n"),
  file = ltitle
)



# Analysis

for(group in 1:2){
  
  # Prepare sample and data
  sample <- list(EU15, EU31)[[group]]
  dat <- filter(data, country %in% sample, year>=1995)
  
  # Print Sample Header
  cat(
    paste0(
      "############################## \n",
      "#  SAMPLE = EU", length(sample), " \n",
      "############################## \n",
      "\n \n "),
    file = ltitle,
    append = T
  )
  
  for(p.value in c(.05, .01, .001)){
    
    # Break analysis:
    is <- isatpanel(
      data = dat,
      formula = ifelse(
        group == 1, "lenergy.emissions_pc ~ lgdp + lgdp_sq + lpop",
        paste0(
          "lenergy.emissions_pc ~ ", 
          paste(group.interactions, collapse = " + ")
        )
      ) %>% as.formula,
      index = c("country", "year"),
      effect = "twoways",
      iis = T,
      fesis = T, 
      t.pval=p.value
    )
    
    # Print analysis results
    cat(
      paste0(
        " \n ###########################", 
        " \n # p-value: ", p.value,
        " \n \n "), 
      file = ltitle, 
      append = T)
    
    sink(ltitle, append=T)
    print(is)
    sink()
    
    cat(" \n \n \n \n \n", 
        file = ltitle, 
        append = T)
  }
}

