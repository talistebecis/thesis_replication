# Analysis template -------------------------------------------------------

rm(list=ls())

#import data
data <- read.csv("Data/02 Intermediary_data/CO2DriversEU_dataset.csv")

#filter for variable of interest
emissions_code <- "1.A.1.a" # add category of interest
emissions_category <- "Main Activity Electricity and Heat Production" # add category of interest

data <- data %>% 
  filter(category == emissions_code) %>% 
  select(-category)

#add lags
data <- as.data.table(data)
data[, L1.lemissions:=c(NA, lemissions[-.N]), by="country"]
data[, L1.lgdp:=c(NA, lgdp[-.N]), by="country"]
data[, L1.lpop:=c(NA, lpop[-.N]), by="country"]
data[, L1.lemissions_pc:=c(NA, lemissions_pc[-.N]), by="country"]

#Group specification
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


# Analysis ----------------------------------------------------------------

set.seed(1230)
file_name <- paste0("Data/03 Output_data/",emissions_category)

###### Analysis:

cat(
  paste0(
    "#################################################################### \n",
    "#                                                                  # \n",
    "#                 CO2 DRIVERS EU - ANALYSIS                        # \n",
    "#                                                                  # \n",
    "#################################################################### \n",
    "\n \n \n"),
  file = file_name
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
    file = file_name,
    append = T
  )
  
  for(p.value in c(.05, .01, .001)){
    
    # Break analysis:
    is <- isatpanel(
      data = dat,
      formula = ifelse(
        group == 1, "lemissions_pc ~ lgdp + lgdp_sq + lpop",
        paste0(
          "lemissions_pc ~ ", 
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
      file = file_name, 
      append = T)
    
    sink(file_name, append=T)
    print(is)
    sink()
    
    cat(" \n \n \n \n \n", 
        file = file_name, 
        append = T)
  }
}
