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
################### Import 1995 - 2003 ###################
setwd('D:/Github/IRS-Migration-Data/Pre-2011')
files = list.files()
files = files[grepl(pattern = 'countymigration',files)]
files = files[-c(1:4)]
read.data.1.inflow = function(dir.name){
  year.value = gsub('countymigration', '', dir.name)
  year.value = str_sub(start = 1, end = 4, year.value)
  setwd(dir.name)
  inflow.dir = list.files(pattern = "Inflow")
  setwd(inflow.dir)
  inflow.files.xls = list.files(pattern = '*.xls')
  read.data.files = function(file.name){
    data = read_xls(file.name)
    column.names = c('y2_statefips',
                     'y2_countyfips',
                     'y1_statefips',
                     'y1_countyfips',
                     'state.abbrieviation',
                     'description',
                     'num.returns.inflow',
                     'num.exemptions.inflow',
                     'income.inflow')
    colnames(data) = column.names
    data = data[-c(1,2,3,4,5,6,7),]
    return(data)
  }
  inflow.data = map_dfr(inflow.files.xls, read.data.files)
  inflow.data= inflow.data|>
    select(c(y2_statefips,
             y2_countyfips,
             y1_statefips,
             y1_countyfips,
             state.abbrieviation,
             description,
             num.returns.inflow,
             num.exemptions.inflow,
             income.inflow))
  inflow.data = inflow.data|>
    mutate(y1_statefips = str_pad(y1_statefips, width = 2, side = 'left', pad = '0'),
           y2_statefips = str_pad(y2_statefips, width = 2, side = 'left', pad = '0'),
           y1_countyfips = str_pad(y1_countyfips, width = 3, side = 'left', pad = '0'),
           y2_countyfips = str_pad(y2_countyfips, width = 3, side = 'left', pad = '0'))
  inflow.data.2 = inflow.data|>
    mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''),
           other.county.FIPS = paste(y1_statefips, y1_countyfips, sep = ''),
           num.returns.inflow = as.numeric(num.returns.inflow),
           num.exemptions.inflow = as.numeric(num.exemptions.inflow),
           income.inflow = as.numeric(income.inflow))|>
    mutate(year = year.value)|>
    select(FIPS, year, description, other.county.FIPS, num.returns.inflow, num.exemptions.inflow, income.inflow)
  inflow.data.3 = inflow.data|>
    filter(y2_statefips == y1_statefips,
           y2_countyfips == y1_countyfips)|>
    mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''))|>
    rename(nonmigrants.inflow = num.exemptions.inflow)|>
    select(FIPS, nonmigrants.inflow)
  inflow.data.combined = left_join(inflow.data.2, inflow.data.3)
  inflow.data.combined.2 = inflow.data.combined|>
    mutate(num.returns.inflow = ifelse(num.returns.inflow == 'd', 0, num.returns.inflow),
           num.exemptions.inflow = ifelse(num.exemptions.inflow == 'd', 0, num.exemptions.inflow),
           income.inflow = ifelse(income.inflow == 'd', 0, income.inflow))
  setwd('..')
  setwd('..')
  return(inflow.data.combined.2)
}
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
data.subset.1.inflow = map_dfr(files, read.data.1.inflow)

read.data.1.outflow = function(dir.name){
  year.value = gsub('countymigration', '', dir.name)
  year.value = str_sub(start = 1, end = 4, year.value)
  setwd(dir.name)
  outflow.dir = list.files(pattern = "Outflow")
  setwd(outflow.dir)
  outflow.files.xls = list.files(pattern = '*.xls')
  read.data.files = function(file.name){
    data = read_xls(file.name)
    column.names = c('y1_statefips',
                     'y1_countyfips',
                     'y2_statefips',
                     'y2_countyfips',
                     'state.abbrieviation',
                     'description',
                     'num.returns.outflow',
                     'num.exemptions.outflow',
                     'income.outflow')
    colnames(data) = column.names
    data = data[-c(1,2,3,4,5,6,7),]
    return(data)
  }
  outflow.data = map_dfr(outflow.files.xls, read.data.files)
  outflow.data = outflow.data|>
    mutate(y1_statefips = str_pad(y1_statefips, width = 2, side = 'left', pad = '0'),
           y2_statefips = str_pad(y2_statefips, width = 2, side = 'left', pad = '0'),
           y1_countyfips = str_pad(y1_countyfips, width = 3, side = 'left', pad = '0'),
           y2_countyfips = str_pad(y2_countyfips, width = 3, side = 'left', pad = '0'))
  outflow.data.2 = outflow.data|>
    mutate(FIPS = paste(y1_statefips, y1_countyfips, sep = ''),
           other.county.FIPS = paste(y2_statefips, y2_countyfips, sep = ''),
           num.returns.outflow = as.numeric(num.returns.outflow),
           num.exemptions.outflow = as.numeric(num.exemptions.outflow),
           income.outflow = as.numeric(income.outflow))|>
    mutate(year = year.value)|>
    select(FIPS, year, description, other.county.FIPS, num.returns.outflow, num.exemptions.outflow, income.outflow)
  outflow.data.3 = outflow.data|>
    filter(y2_statefips == y1_statefips,
           y2_countyfips == y1_countyfips)|>
    mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''))|>
    rename(nonmigrants.outflow = num.exemptions.outflow)|>
    select(FIPS, nonmigrants.outflow)
  outflow.data.2 = unique(outflow.data.2)
  outflow.data.3 = unique(outflow.data.3)
  outflow.data.combined = left_join(outflow.data.2, outflow.data.3)
  outflow.data.combined.2 = outflow.data.combined|>
    mutate(num.returns.outflow = ifelse(num.returns.outflow == 'd', 0, num.returns.outflow),
           num.exemptions.outflow = ifelse(num.exemptions.outflow == 'd', 0, num.exemptions.outflow),
           income.outflow = ifelse(income.outflow == 'd', 0, income.outflow))
  setwd('..')
  setwd('..')
  return(outflow.data.combined.2)
}
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
data.subset.1.outflow = map_dfr(files, read.data.1.outflow)

combined.data.subset.1 = full_join(data.subset.1.inflow, data.subset.1.outflow)

combined.data.subset.1.2 = combined.data.subset.1|>
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
################### Import 2004 - 2010 #########################
setwd('D:/Github/IRS-Migration-Data/Pre-2011')
files = list.files()
files = files[!grepl(pattern = 'countymigration', files)]
read.data.2.inflow = function(dir.name){
  year.value = str_sub(start = 7, end = 8, dir.name)
  year.value = paste('20',year.value,sep = '')
  setwd(dir.name)
  subdirectory = list.files(pattern = 'Inflow')
  setwd(subdirectory)
  inflow.files.xls = list.files(pattern = '*.xls', full.names = TRUE)
  read.data.files = function(file.name){
    data = read_xls(file.name)
    column.names = c('y2_statefips',
                     'y2_countyfips',
                     'y1_statefips',
                     'y1_countyfips',
                     'state.abbrieviation',
                     'description',
                     'num.returns.inflow',
                     'num.exemptions.inflow',
                     'income.inflow')
    colnames(data) = column.names
    data = data[-c(1,2,3,4,5,6,7),]
    return(data)
  }
  inflow.data = map_dfr(inflow.files.xls, read.data.files)
  inflow.data = inflow.data|>
    mutate(y1_statefips = str_pad(y1_statefips, width = 2, side = 'left', pad = '0'),
           y2_statefips = str_pad(y2_statefips, width = 2, side = 'left', pad = '0'),
           y1_countyfips = str_pad(y1_countyfips, width = 3, side = 'left', pad = '0'),
           y2_countyfips = str_pad(y2_countyfips, width = 3, side = 'left', pad = '0'))
  inflow.data.2 = inflow.data|>
    mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''),
           other.county.FIPS = paste(y1_statefips, y1_countyfips, sep = ''),
           num.returns.inflow = as.numeric(num.returns.inflow),
           num.exemptions.inflow = as.numeric(num.exemptions.inflow),
           income.inflow = as.numeric(income.inflow))|>
    mutate(year = year.value)|>
    select(FIPS, year, description, other.county.FIPS, num.returns.inflow, num.exemptions.inflow, income.inflow)
  inflow.data.3 = inflow.data|>
    filter(y2_statefips == y1_statefips,
           y2_countyfips == y1_countyfips)|>
    mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''))|>
    rename(nonmigrants.inflow = num.exemptions.inflow)|>
    select(FIPS, nonmigrants.inflow)
  inflow.data.combined = left_join(inflow.data.2, inflow.data.3)
  inflow.data.combined.2 = inflow.data.combined|>
    mutate(num.returns.inflow = ifelse(num.returns.inflow == 'd', 0, num.returns.inflow),
           num.exemptions.inflow = ifelse(num.exemptions.inflow == 'd', 0, num.exemptions.inflow),
           income.inflow = ifelse(income.inflow == 'd', 0, income.inflow))
  setwd('..')
  setwd('..')
  return(inflow.data.combined.2)
}
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
data.subset.2.inflow = map_dfr(files, read.data.2.inflow)

read.data.2.outflow = function(dir.name){
  year.value = str_sub(start = 7, end = 8, dir.name)
  year.value = paste('20',year.value,sep = '')
  setwd(dir.name)
  subdirectory = list.files(pattern = 'Outflow')
  setwd(subdirectory)
  outflow.files.xls = list.files(pattern = '*.xls', full.names = TRUE)
  read.data.files = function(file.name){
    data = read_xls(file.name)
    column.names = c('y1_statefips',
                     'y1_countyfips',
                     'y2_statefips',
                     'y2_countyfips',
                     'state.abbrieviation',
                     'description',
                     'num.returns.outflow',
                     'num.exemptions.outflow',
                     'income.outflow')
    colnames(data) = column.names
    data = data[-c(1,2,3,4,5,6,7),]
    return(data)
  }
  outflow.data = map_dfr(outflow.files.xls, read.data.files)
  outflow.data = outflow.data|>
    mutate(y1_statefips = str_pad(y1_statefips, width = 2, side = 'left', pad = '0'),
           y2_statefips = str_pad(y2_statefips, width = 2, side = 'left', pad = '0'),
           y1_countyfips = str_pad(y1_countyfips, width = 3, side = 'left', pad = '0'),
           y2_countyfips = str_pad(y2_countyfips, width = 3, side = 'left', pad = '0'))
  outflow.data.2 = outflow.data|>
    mutate(FIPS = paste(y1_statefips, y1_countyfips, sep = ''),
           other.county.FIPS = paste(y2_statefips, y2_countyfips, sep = ''),
           num.returns.outflow = as.numeric(num.returns.outflow),
           num.exemptions.outflow = as.numeric(num.exemptions.outflow),
           income.outflow = as.numeric(income.outflow))|>
    mutate(year = year.value)|>
    select(FIPS, year, description, other.county.FIPS, num.returns.outflow, num.exemptions.outflow, income.outflow)
  outflow.data.3 = outflow.data|>
    filter(y2_statefips == y1_statefips,
           y2_countyfips == y1_countyfips)|>
    mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''))|>
    rename(nonmigrants.outflow = num.exemptions.outflow)|>
    select(FIPS, nonmigrants.outflow)
  outflow.data.combined = left_join(outflow.data.2, outflow.data.3)
  outflow.data.combined.2 = outflow.data.combined|>
    mutate(num.returns.outflow = ifelse(num.returns.outflow == 'd', 0, num.returns.outflow),
           num.exemptions.outflow = ifelse(num.exemptions.outflow == 'd', 0, num.exemptions.outflow),
           income.outflow = ifelse(income.outflow == 'd', 0, income.outflow))
  setwd('..')
  setwd('..')
  return(outflow.data.combined.2)
}
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
data.subset.2.outflow = map_dfr(files, read.data.2.outflow)

combined.data.subset.2 = full_join(data.subset.2.inflow, data.subset.2.outflow)

combined.data.subset.2.2 = combined.data.subset.2|>
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
############################ 2011 - 2021; Import Inflow and Outflow, Combine and Clean #####################
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Post-2011')
file.list = list.files()

inflow.files = file.list[grepl(pattern = 'inflow', file.list)]
outflow.files = file.list[grepl(pattern = 'outflow', file.list)]
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
#################### Combine All Data into Master Data Set #####################
full.data = rbind(combined.data.subset.1.2, combined.data.subset.2.2, combined.data.2)
full.data = full.data|>
  arrange(FIPS, year)
################# Save Data Frame ##################
setwd('D:/Dropbox/Research Data/Cleaned Data/IRS Migration')
write_csv(full.data, file = 'IRS_Migration_Data_1995-2021.csv')
