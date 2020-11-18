
# PACKAGES ----------------------------------------------------------------

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("data.table")) install.packages("data.table")
if (!require("stringr")) install.packages("stringr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("dtplyr")) devtools::install_github("tidyverse/dtplyr")
if (!require("purrr")) devtools::install_github("purrr")
if (!require("utils")) devtools::install_github("utils")


# CALL DATA ---------------------------------------------------------------

unzip('data/cleaned_data.zip') # Github repo cannot have so big file --> I remove data/ships.csv before pushing on github


data <- fread('data/ships.csv') %>% 
  lazy_dt()


# GET FAMILIAR WITH DATA --------------------------------------------------

str(data)
glimpse(data)

view_data <- head(data,1000) %>% 
  as_tibble()

View(view_data)

data$parent %>%  
  select_if(is.character) %>% 
  map(unique)

colnames(data$parent)


# TIDY DATA ---------------------------------------------------------------

## set column names to lower_case
names(data$parent) <- str_to_lower(names(data$parent))
str(data)

## date format
data$parent$datetime <- ymd_hms(data$parent$datetime)
data$parent$date <- ymd(data$parent$date)

str(data)

## port column is dupplicated -- remove one
names(data$parent)
data$parent <- data$parent %>%
  select(-25) 

data_tibble <- data %>% 
  as_tibble()

str(data_tibble)


fwrite(data_tibble, "data/cleaned_data.csv")

# SAVE VESSEL TYPE AND NAME -----------------------------------------------

## the dataset contains more than 3M rows
## to guarantee better user experience, I will save the vessel type and name
## in a different csv that I will first load into the shiny app

vessel_dt <- data_tibble %>% 
  select(ship_type, shipname) %>% 
  unique()

fwrite(vessel_dt, "data/vessel_dt.csv")
