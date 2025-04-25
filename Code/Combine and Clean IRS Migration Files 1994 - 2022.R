######################### Set-Up Chunk ################
tempdir()
dir.create(tempdir())


if (!require('pacman')){
  install.packages('pacman')
}
library(pacman)
p_load('sf',
       'tidyverse',
       'stringr',
       'ggplot2',
       'tools',
       'data.table',
       'readxl',
       'stargazer',
       'ivreg',
       'tryCatchLog')

############################### Create FIPS Vector###############
#setwd('D:/Dropbox/Research Data/Raw Data/Census')
#census = read_csv('co-est2020-alldata.csv')
#census.fips = census|>
#  filter(COUNTY != '000')|>
#  mutate(fips = paste(STATE, COUNTY, sep = ''))|>
#  select(fips)
############################### Import Inflow Data from Years 1994 - 2004 #################
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
files = list.files()
files = files[grepl(pattern = 'countymigration',files)]
files = files[-c(1,2)]
read.data = function(dir.name){
  year.value = gsub('countymigration', '', dir.name)
  year.value = str_sub(start = 7, end = 11, year.value)
  setwd(dir.name)
  inflow.dir = list.files(pattern = "Inflow")
  setwd(inflow.dir)
  inflow.files.xls = list.files(pattern = '*.xls')
  read.data.files = function(file.name){
    data = read_xls(file.name)
    column.names = c('statefips.immigrate',
                     'countyfips.immigrate',
                     'statefips.origin',
                     'countyfips.origin',
                     'state.abbrieviation',
                     'totals',
                     'returns.inflow',
                     'exemptions.inflow',
                     'grossincome.inflow')
    colnames(data) = column.names
    data = data[-c(1,2,3,4,5,6,7),]
    return(data)
  }
  data = map_dfr(inflow.files.xls, read.data.files)
  data = data|>
    mutate(year = year.value)
  setwd('..')
  setwd('..')
  return(data)
}
data.subset.1 = map_dfr(files, read.data)
data.subset.1.2 = data.subset.1|>
  select(!c(`...10`,`...11`,`...12`))
data.subset.1.2$state.abbrieviation = toupper(data.subset.1.2$state.abbrieviation)
################################ Import Inflow Data from Years 2005 - 2011 ##############
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
files = list.files()
files = files[!grepl(pattern = 'countymigration', files)]
read.data.2 = function(dir.name){
  year.value = str_sub(start = 9, end = 10, dir.name)
  year.value = paste('20',year.value,sep = '')
  setwd(dir.name)
  subdirectory = list.files(pattern = 'Inflow')
  setwd(subdirectory)
  inflow.files.xls = list.files(pattern = '*.xls', full.names = TRUE)
  read.data.files = function(file.name){
    data = read_xls(file.name)
    column.names = c('statefips.immigrate',
                     'countyfips.immigrate',
                     'statefips.origin',
                     'countyfips.origin',
                     'state.abbrieviation',
                     'totals',
                     'returns.inflow',
                     'exemptions.inflow',
                     'grossincome.inflow')
    colnames(data) = column.names
    data = data[-c(1,2,3,4,5,6,7),]
    return(data)
  }
  data = map_dfr(inflow.files.xls, read.data.files)
  data = data|>
    mutate(year = year.value)
  setwd('..')
  setwd('..')
  return(data)
}
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
data.subset.2 = map_dfr(files, read.data.2)
##################### Import Inflow Data from Years 2012 - 2022 #############
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Post-2011')
files = list.files(pattern = 'countyinflow')
read.data.3 = function(file.name){
  year.value = str_sub(file.name, start = 15, end = 16)
  year.value = paste('20',year.value,sep = '')
  data = read_csv(file.name)
  column.names = c('statefips.immigrate',
                   'countyfips.immigrate',
                   'statefips.origin',
                   'countyfips.origin',
                   'state.abbrieviation',
                   'totals',
                   'returns.inflow',
                   'exemptions.inflow',
                   'grossincome.inflow')
  column.numbers = c('returns.inflow',
                     'exemptions.inflow',
                     'grossincome.inflow')
  column.characters = c('statefips.immigrate',
                        'countyfips.immigrate',
                        'statefips.origin',
                        'countyfips.origin',
                        'state.abbrieviation',
                        'totals')
  colnames(data) = column.names
  data[column.numbers] = sapply(data[column.numbers], as.numeric)
  data[column.characters] = sapply(data[column.characters], as.character)
  data = data|>
    mutate(year = year.value)
  return(data)
}
data.subset.3 = map_dfr(files, read.data.3)
###################### Import Outflow data from 1994 - 2004 #############
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
files = list.files()
files = files[grepl(pattern = 'countymigration',files)]
files = files[-c(1,2)]
read.data.4 = function(dir.name){
  year.value = gsub('countymigration', '', dir.name)
  year.value = str_sub(start = 7, end = 11, year.value)
  setwd(dir.name)
  outflow.dir = list.files(pattern = "Outflow")
  setwd(outflow.dir)
  outflow.files.xls = list.files(pattern = '*.xls')
  read.data.files = function(file.name){
    data = read_xls(file.name)
    column.names = c('statefips.emigrate',
                     'countyfips.emigrate',
                     'statefips.destination',
                     'countyfips.destination',
                     'state.abbrieviation',
                     'totals',
                     'returns.outflow',
                     'exemptions.outflow',
                     'grossincome.outflow')
    colnames(data) = column.names
    data = data[-c(1,2,3,4,5,6,7),]
    return(data)
  }
  data = map_dfr(outflow.files.xls, read.data.files)
  data = data|>
    mutate(year = year.value)
  setwd('..')
  setwd('..')
  return(data)
}
data.subset.4 = map_dfr(files, read.data.4)
data.subset.4.2 = data.subset.4|>
  select(-c(`...10`,`...11`))
data.subset.4.2$state.abbrieviation = toupper(data.subset.4.2$state.abbrieviation)
######################## Import Outflow Data from 2005 - 2011 ###################
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
files = list.files()
files = files[!grepl(pattern = 'countymigration', files)]
read.data.5 = function(dir.name){
  year.value = str_sub(start = 9, end = 10, dir.name)
  year.value = paste('20',year.value,sep = '')
  setwd(dir.name)
  subdirectory = list.files(pattern = 'Outflow')
  setwd(subdirectory)
  outflow.files.xls = list.files(pattern = '*.xls', full.names = TRUE)
  read.data.files = function(file.name){
    data = read_xls(file.name)
    column.names = c('statefips.emigrate',
                     'countyfips.emigrate',
                     'statefips.destination',
                     'countyfips.destination',
                     'state.abbrieviation',
                     'totals',
                     'returns.outflow',
                     'exemptions.outflow',
                     'grossincome.outflow')
    colnames(data) = column.names
    data = data[-c(1,2,3,4,5,6,7),]
    return(data)
  }
  data = map_dfr(outflow.files.xls, read.data.files)
  data = data|>
    mutate(year = year.value)
  setwd('..')
  setwd('..')
  return(data)
}
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
data.subset.5 = map_dfr(files, read.data.5)
################## Import Outflow Data from 2012 - 2022 #####################
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Post-2011')
files = list.files(pattern = 'countyoutflow')
read.data.6 = function(file.name){
  year.value = str_sub(file.name, start = 16, end = 17)
  year.value = paste('20',year.value,sep = '')
  data = read_csv(file.name)
  column.names = c('statefips.emigrate',
                   'countyfips.emigrate',
                   'statefips.destination',
                   'countyfips.destination',
                   'state.abbrieviation',
                   'totals',
                   'returns.outflow',
                   'exemptions.outflow',
                   'grossincome.outflow')
  column.numbers = c('returns.outflow',
                     'exemptions.outflow',
                     'grossincome.outflow')
  column.characters = c('statefips.emigrate',
                        'countyfips.emigrate',
                        'statefips.destination',
                        'countyfips.destination',
                        'state.abbrieviation',
                        'totals')
  colnames(data) = column.names
  data[column.numbers] = sapply(data[column.numbers], as.numeric)
  data[column.characters] = sapply(data[column.characters], as.character)
  data = data|>
    mutate(year = year.value)
  return(data)
}
data.subset.6 = map_dfr(files, read.data.6)
##################### Combine all Data and Clean #########################
setwd('D:/Dropbox/Research Data/Cleaned Data')
data.1 = rbind(data.subset.1.2, data.subset.2, data.subset.3)
data.2 = rbind(data.subset.4.2, data.subset.5, data.subset.6)

data.1$countyfips.immigrate = str_pad(data.1$countyfips.immigrate, width = 3, side = 'left', pad = '0')
data.1$statefips.immigrate = str_pad(data.1$statefips.immigrate, width = 2, side = 'left', pad = '0')
data.1$countyfips.origin = str_pad(data.1$countyfips.origin, width = 3, side = 'left', pad = '0')
data.1$statefips.origin = str_pad(data.1$statefips.origin, width = 2, side = 'left', pad = '0')
data.1$returns.inflow = as.numeric(data.1$returns.inflow)
data.1$exemptions.inflow = as.numeric(data.1$exemptions.inflow)
data.1$grossincome.inflow = as.numeric(data.1$grossincome.inflow)

data.2$countyfips.emigrate = str_pad(data.2$countyfips.emigrate, width = 3, side = 'left', pad = '0')
data.2$statefips.emigrate = str_pad(data.2$statefips.emigrate, width = 2, side = 'left', pad = '0')
data.2$countyfips.destination = str_pad(data.2$countyfips.destination, width = 3, side = 'left', pad = '0')
data.2$statefips.destination = str_pad(data.2$statefips.destination, width = 2, side = 'left', pad = '0')
data.2$returns.outflow = as.numeric(data.2$returns.outflow)
data.2$exemptions.outflow = as.numeric(data.2$exemptions.outflow)
data.2$grossincome.outflow = as.numeric(data.2$grossincome.outflow)

data.1.2 = data.1|>
  mutate(fips = paste(statefips.immigrate, countyfips.immigrate, sep = ''))|>
  mutate(fips.origin = paste(statefips.origin, countyfips.origin, sep = ''))|>
  arrange(fips,year)

data.2.2 = data.2|>
  mutate(fips = paste(statefips.emigrate, countyfips.emigrate, sep = ''))|>
  mutate(fips.destination = paste(statefips.destination, countyfips.destination, sep = ''))|>
  arrange(fips, year)

combined.data = left_join(data.1.2, data.2.2)
combined.data.2 = combined.data|>
  select(!c(statefips.immigrate,
            statefips.emigrate,
            countyfips.immigrate,
            countyfips.emigrate,
            statefips.origin,
            statefips.destination,
            countyfips.origin,
            countyfips.destination))|>
  filter(fips.origin != 'NANA')|>
  select(fips, year, totals, fips.destination, returns.outflow, exemptions.outflow, grossincome.outflow, fips.origin, returns.inflow, exemptions.inflow, grossincome.inflow)|>
  arrange(fips, year)
################ Save Output #####################
setwd('D:/Dropbox/Research Data/Cleaned Data/IRS Migration')
write_csv(combined.data.2, file = 'IRS Migration Data 1994 - 2022.csv')
#################
