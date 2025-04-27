################### Set-up Chunk ########
if (!require('pacman', character.only = TRUE)){
  install.packages('pacman', dependancies = TRUE)
}
library(pacman)
p_load("jsonlite",
       "httr",
       "rvest",
       "tidyverse",
       "dplyr",
       "purrr",
       "readxl",
       'tesseract',
       'magick',
       'tidytext',
       'did2s',
       'tictoc',
       'stringi',
       'sf',
       'ggthemes',
       'rnaturalearth',
       'maps',
       'mapproj')
################## Determine Baseline Population 2010 ##################
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011/county1011')
data.2010 = read_csv('countyoutflow1011.csv')
in.data.2010 = read_csv('countyinflow1011.csv')

nonmigrants.data = data.2010|>
  filter(State_Code_Origin != '00',
         County_Code_Origin != '000',
         State_Code_Origin == State_Code_Dest,
         County_Code_Origin == County_Code_Dest)|>
  mutate(FIPS = paste(State_Code_Origin, County_Code_Origin, sep = ''))|>
  rename(nonmigrants = Exmpt_Num)|>
  select(FIPS, nonmigrants)

outmigrant.data = data.2010|>
  filter(State_Code_Origin != '00',
         County_Code_Origin != '000',
         State_Code_Dest == '96',
         County_Code_Dest == '000')|>
  mutate(FIPS = paste(State_Code_Origin, County_Code_Origin, sep = ''))|>
  rename(outmigrants = Exmpt_Num)|>
  select(FIPS, outmigrants)

inmigrants.data = in.data.2010|>
  filter(State_Code_Dest != '00',
         County_Code_Dest != '000',
         State_Code_Origin == '96',
         County_Code_Origin == '000')|>
  mutate(FIPS = paste(State_Code_Dest, County_Code_Dest, sep = ''))|>
  rename(inmigrants = Exmpt_Num)|>
  select(FIPS, inmigrants)

population.data = left_join(nonmigrants.data, outmigrant.data)
population.data = left_join(population.data, inmigrants.data)
population.data.2 = population.data|>
  mutate(population.2010 = nonmigrants - outmigrants + inmigrants)|>
  select(FIPS, population.2010)
############################ Create Lists of Inflow/Outflow Files #####################
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Post-2011')
file.list = list.files()

inflow.files = file.list[grepl(pattern = 'inflow', file.list)]
outflow.files = file.list[grepl(pattern = 'outflow', file.list)]
########################### Create code for Inflow Function #########################
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Post-2011')
file.name = inflow.files[10]
#Start of Function
year.value = paste('20', str_sub(file.name, start = 13, end = 14), sep = '')

inflow.data = read_csv(file.name)
inflow.data = inflow.data|>
  mutate(y1_statefips = str_pad(y1_statefips, width = 2, side = 'left', pad = '0'),
         y2_statefips = str_pad(y2_statefips, width = 2, side = 'left', pad = '0'),
         y1_countyfips = str_pad(y1_countyfips, width = 3, side = 'left', pad = '0'),
         y2_countyfips = str_pad(y2_countyfips, width = 3, side = 'left', pad = '0'))
inflow.data.2 = inflow.data|>
  mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''),
         other.county.FIPS = paste(y1_statefips, y1_countyfips, sep = ''))|>
  rename(num.returns.inflow = n1,
         num.exemptions.inflow = n2,
         income.inflow = agi,
         description = y1_countyname)|>
  mutate(year = year.value)|>
  select(FIPS, year, description, other.county.FIPS, num.returns.inflow, num.exemptions.inflow, income.inflow)
inflow.data.3 = inflow.data|>
  filter(y2_statefips == y1_statefips,
         y2_countyfips == y1_countyfips)|>
  mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''))|>
  rename(nonmigrants.inflow = n2)|>
  select(FIPS, nonmigrants.inflow)
inflow.data.combined = left_join(inflow.data.2, inflow.data.3)
########################## Create Code for Outflow Function #######################
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Post-2011')
file.name = outflow.files[10]
#Start of Function
year.value = paste('20', str_sub(file.name, start = 14, end = 15), sep = '')

outflow.data = read_csv(file.name)
outflow.data = outflow.data|>
  mutate(y1_statefips = str_pad(y1_statefips, width = 2, side = 'left', pad = '0'),
         y2_statefips = str_pad(y2_statefips, width = 2, side = 'left', pad = '0'),
         y1_countyfips = str_pad(y1_countyfips, width = 3, side = 'left', pad = '0'),
         y2_countyfips = str_pad(y2_countyfips, width = 3, side = 'left', pad = '0'))
outflow.data.2 = outflow.data|>
  mutate(y1_statefips = str_pad(y1_statefips, width = 2, side = 'left', pad = '0'),
         y2_statefips = str_pad(y2_statefips, width = 2, side = 'left', pad = '0'),
         y1_countyfips = str_pad(y1_countyfips, width = 3, side = 'left', pad = '0'),
         y2_countyfips = str_pad(y2_countyfips, width = 3, side = 'left', pad = '0'))|>
  filter(y2_statefips %in% c('96', '97','98'))|>
  mutate(FIPS = paste(y1_statefips, y1_countyfips, sep = ''),
         other.county.FIPS = paste(y2_statefips, y2_countyfips, sep = ''))|>
  rename(num.returns.outflow = n1,
         num.exemptions.outflow = n2,
         income.outflow = agi,
         description = y2_countyname)|>
  mutate(year = year.value)|>
  select(FIPS, year, description, other.county.FIPS, num.returns.outflow, num.exemptions.outflow, income.outflow)

outflow.data.3 = outflow.data|>
  filter(y2_statefips == y1_statefips,
         y2_countyfips == y1_countyfips)|>
  mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''))|>
  rename(nonmigrants.outflow = n2)|>
  select(FIPS, nonmigrants.outflow)
outflow.data.combined = left_join(outflow.data.2, outflow.data.3)
############################ Test Combining Functions ##############################
combined.data.test = full_join(inflow.data.combined, outflow.data.combined)
combined.data.test.2 = combined.data.test|>
  mutate(nonmigrants = ifelse(is.na(nonmigrants.inflow) == TRUE & is.na(nonmigrants.outflow) == FALSE, nonmigrants.outflow,
                              ifelse(is.na(nonmigrants.inflow) == FALSE & is.na(nonmigrants.outflow) == TRUE, nonmigrants.inflow,
                                     ifelse(is.na(nonmigrants.inflow) == TRUE & is.na(nonmigrants.outflow) == TRUE, NA, 
                                            ifelse(is.na(nonmigrants.inflow) == FALSE & is.na(nonmigrants.outflow) == FALSE, nonmigrants.inflow, NA)))))

############################ Main Chunk; Import Inflow and Outflow, Combine and Clean ############################
#Import Inflow Data
import.inflow = function(file.name){
  year.value = paste('20', str_sub(file.name, start = 13, end = 14), sep = '')
  
  inflow.data = read_csv(file.name)
  inflow.data = inflow.data|>
    mutate(y1_statefips = str_pad(y1_statefips, width = 2, side = 'left', pad = '0'),
           y2_statefips = str_pad(y2_statefips, width = 2, side = 'left', pad = '0'),
           y1_countyfips = str_pad(y1_countyfips, width = 3, side = 'left', pad = '0'),
           y2_countyfips = str_pad(y2_countyfips, width = 3, side = 'left', pad = '0'))
  inflow.data.2 = inflow.data|>
    mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''),
           other.county.FIPS = paste(y1_statefips, y1_countyfips, sep = ''))|>
    rename(num.returns.inflow = n1,
           num.exemptions.inflow = n2,
           income.inflow = agi,
           description = y1_countyname)|>
    mutate(year = year.value)|>
    select(FIPS, year, description, other.county.FIPS, num.returns.inflow, num.exemptions.inflow, income.inflow)
  
  inflow.data.3 = inflow.data|>
    filter(y2_statefips == y1_statefips,
           y2_countyfips == y1_countyfips)|>
    mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''))|>
    rename(nonmigrants.inflow = n2)|>
    select(FIPS, nonmigrants.inflow)
  inflow.data.combined = left_join(inflow.data.2, inflow.data.3)
  return(inflow.data.combined)
}
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Post-2011')
all.inflow = map_dfr(inflow.files, import.inflow)
#Import Outflow Data
import.outflow = function(file.name){
  year.value = paste('20', str_sub(file.name, start = 14, end = 15), sep = '')
  
  outflow.data = read_csv(file.name)
  outflow.data = outflow.data|>
    mutate(y1_statefips = str_pad(y1_statefips, width = 2, side = 'left', pad = '0'),
           y2_statefips = str_pad(y2_statefips, width = 2, side = 'left', pad = '0'),
           y1_countyfips = str_pad(y1_countyfips, width = 3, side = 'left', pad = '0'),
           y2_countyfips = str_pad(y2_countyfips, width = 3, side = 'left', pad = '0'))
  outflow.data.2 = outflow.data|>
    mutate(y1_statefips = str_pad(y1_statefips, width = 2, side = 'left', pad = '0'),
           y2_statefips = str_pad(y2_statefips, width = 2, side = 'left', pad = '0'),
           y1_countyfips = str_pad(y1_countyfips, width = 3, side = 'left', pad = '0'),
           y2_countyfips = str_pad(y2_countyfips, width = 3, side = 'left', pad = '0'))|>
    mutate(FIPS = paste(y1_statefips, y1_countyfips, sep = ''),
           other.county.FIPS = paste(y2_statefips, y2_countyfips, sep = ''))|>
    rename(num.returns.outflow = n1,
           num.exemptions.outflow = n2,
           income.outflow = agi,
           description = y2_countyname)|>
    mutate(year = year.value)|>
    select(FIPS, year, description, other.county.FIPS, num.returns.outflow, num.exemptions.outflow, income.outflow)
  
  outflow.data.3 = outflow.data|>
    filter(y2_statefips == y1_statefips,
           y2_countyfips == y1_countyfips)|>
    mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''))|>
    rename(nonmigrants.outflow = n2)|>
    select(FIPS, nonmigrants.outflow)
  outflow.data.combined = left_join(outflow.data.2, outflow.data.3)
  return(outflow.data.combined)
}
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Post-2011')
all.outflow = map_dfr(outflow.files, import.outflow)

combined.data = full_join(all.inflow, all.outflow)
combined.data.2 = combined.data|>
  mutate(num.exemptions.inflow = ifelse(num.exemptions.inflow == -1, 0, num.exemptions.inflow),
         num.exemptions.outflow = ifelse(num.exemptions.outflow == -1, 0, num.exemptions.outflow),
         income.inflow = ifelse(income.inflow == -1, 0, income.inflow),
         income.outflow = ifelse(income.outflow == -1, 0, income.outflow),
         num.returns.inflow = ifelse(num.returns.inflow == -1, 0, num.returns.inflow),
         num.returns.outflow = ifelse(num.returns.outflow == -1, 0, num.returns.outflow))|>
  mutate(num.exemptions.inflow = ifelse(is.na(num.exemptions.inflow) == TRUE, 0, num.exemptions.inflow),
         num.exemptions.outflow = ifelse(is.na(num.exemptions.outflow) == TRUE, 0, num.exemptions.outflow),
         income.inflow = ifelse(is.na(income.inflow) == TRUE, 0, income.inflow),
         income.outflow = ifelse(is.na(income.outflow) == TRUE, 0, income.outflow),
         num.returns.inflow = ifelse(is.na(num.returns.inflow) == TRUE, 0, num.returns.inflow),
         num.returns.outflow = ifelse(is.na(num.returns.outflow) == TRUE, 0, num.returns.outflow))|>
  mutate(net.returns = num.returns.inflow - num.returns.outflow,
         net.exemptions = num.exemptions.inflow - num.exemptions.outflow,
         net.income = income.inflow - income.outflow,
         nonmigrants = ifelse(is.na(nonmigrants.inflow) == TRUE & is.na(nonmigrants.outflow) == FALSE, nonmigrants.outflow,
                              ifelse(is.na(nonmigrants.inflow) == FALSE & is.na(nonmigrants.outflow) == TRUE, nonmigrants.inflow,
                                     ifelse(is.na(nonmigrants.inflow) == TRUE & is.na(nonmigrants.outflow) == TRUE, NA, 
                                            ifelse(is.na(nonmigrants.inflow) == FALSE & is.na(nonmigrants.outflow) == FALSE, nonmigrants.inflow, NA)))))|>
  filter(is.na(nonmigrants) == FALSE)|>
  select(!c(nonmigrants.inflow, nonmigrants.outflow))
combined.data.3 = left_join(combined.data.2, population.data.2)
################ Save Data Frame #############################
setwd('D:/Dropbox/Research Data/Cleaned Data/IRS Migration')
write_csv(combined.data.3, file = 'IRS_Migration_Data_2011-2021.csv')
