#This script is used to read migration statistics from the IRS into R, then combine and clean them into a single data set.
#As the same code is used multiple times and is only slightly changed from chunk to chunk, only the earliest example of the code will be annotated.
#Any differences between chunks should be annotated for clarity.
################### Set-up Chunk ########
#Automatically checks if required R packages are installed, then downloads, installs and loads them.
#
if (!require('pacman', character.only = TRUE)){
  install.packages('pacman', dependancies = TRUE)
}
library(pacman)
p_load("tidyverse",
       "dplyr",
       "purrr",
       "readxl",
       'tidytext',
       'stringi')
################### Import 1995 - 2003 ###################
#Identifies all data files in the pre-2011 folder which correspond to the years 1990 to 2003.  
#Data files between the years 1990 to 1994 are excluded due to difficulties equating the data in them to the rest of the data.
#In all cases where the working directory is set, make sure that the disk letter is correct.
setwd('D:/Github/IRS-Migration-Data/Pre-2011')
files = list.files()
files = files[grepl(pattern = 'countymigration',files)]
files = files[-c(1:4)]
#Creates a function to read the inflow data files.
read.data.1.inflow = function(dir.name){
  #Determines a year value using the name of the directory.  The first year listed is used to denote the year of the data.
  year.value = gsub('countymigration', '', dir.name)
  year.value = str_sub(start = 1, end = 4, year.value)
  #Automatically navigates to the sub-directory which contains the inflow files, then makes a list of all the files with the .xls file type in it.
  setwd(dir.name)
  inflow.dir = list.files(pattern = "Inflow")
  setwd(inflow.dir)
  inflow.files.xls = list.files(pattern = '*.xls')
  #Create a function which will be used to automate reading all the files in the sub-directory, then does some basic cleaning
  read.data.files = function(file.name){
    data = read_xls(file.name)
    #Sets the column names to match what the columns record
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
    #Redundant rows are dropped
    data = data[-c(1,2,3,4,5,6,7),]
    return(data)
  }
  #Automates reading all the inflow data into R, and creates a data frame containing all of them
  inflow.data = map_dfr(inflow.files.xls, read.data.files)
  #Ensure that only columns with data are included
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
  #Ensure that all FIPS codes are formatted correctly
  inflow.data = inflow.data|>
    mutate(y1_statefips = str_pad(y1_statefips, width = 2, side = 'left', pad = '0'),
           y2_statefips = str_pad(y2_statefips, width = 2, side = 'left', pad = '0'),
           y1_countyfips = str_pad(y1_countyfips, width = 3, side = 'left', pad = '0'),
           y2_countyfips = str_pad(y2_countyfips, width = 3, side = 'left', pad = '0'))
  #Pastes together the FIPS codes for the primary county as well as the origin county.
  #The origin county is named "other.county.FIPS" as the final data frame will contain one observation which records migration in and out from each specific county.
  #Also ensures that migration statistics are recorded properly as numeric variables.
  inflow.data.2 = inflow.data|>
    mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''),
           other.county.FIPS = paste(y1_statefips, y1_countyfips, sep = ''),
           num.returns.inflow = as.numeric(num.returns.inflow),
           num.exemptions.inflow = as.numeric(num.exemptions.inflow),
           income.inflow = as.numeric(income.inflow))|>
    mutate(year = year.value)|>
    select(FIPS, year, description, other.county.FIPS, num.returns.inflow, num.exemptions.inflow, income.inflow)
  #Creates a separate data frame which records the number of non-migrating individuals for each county.  
  inflow.data.3 = inflow.data|>
    filter(y2_statefips == y1_statefips,
           y2_countyfips == y1_countyfips)|>
    mutate(FIPS = paste(y2_statefips, y2_countyfips, sep = ''))|>
    rename(nonmigrants.inflow = num.exemptions.inflow)|>
    select(FIPS, nonmigrants.inflow)
  #Join the migration data frame with the non-migration data frame
  inflow.data.combined = left_join(inflow.data.2, inflow.data.3)
  #Recodes missing migration statistics as zero.
  inflow.data.combined.2 = inflow.data.combined|>
    mutate(num.returns.inflow = ifelse(num.returns.inflow == 'd', 0, num.returns.inflow),
           num.exemptions.inflow = ifelse(num.exemptions.inflow == 'd', 0, num.exemptions.inflow),
           income.inflow = ifelse(income.inflow == 'd', 0, income.inflow))
  #Navigates back to the starting working directory so the function can run again.
  setwd('..')
  setwd('..')
  return(inflow.data.combined.2)
}
#Makes sure the working directory is correct, then populates a data frame with the years 1995 - 2003, using the inflow data function.
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
data.subset.1.inflow = map_dfr(files, read.data.1.inflow)

#Defines a function to read the outflow data.
#Most of this function is identical to the previous function, except for outflow migration data.
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
#Makes sure the working directory is correct, then populates a data frame with the years 1995 - 2003
setwd('D:/Dropbox/Research Data/Raw Data/IRS Migration/Pre-2011')
data.subset.1.outflow = map_dfr(files, read.data.1.outflow)

#Join together the inflow and outflow data frames
#Note that NA values will be generated if inflow data is available but outflow data is not available for a specific combination of inflow and outflow data, and vice versa.
combined.data.subset.1 = full_join(data.subset.1.inflow, data.subset.1.outflow)

#Cleans the combined data set
#Note that missing values will be recoded as zero.
#Also note that the same recoding process is used for each chunk.
combined.data.subset.1.2 = combined.data.subset.1|>
  #Recodes missing observations (recorded as -1) as zero
  mutate(num.exemptions.inflow = ifelse(num.exemptions.inflow == -1, 0, num.exemptions.inflow),
         num.exemptions.outflow = ifelse(num.exemptions.outflow == -1, 0, num.exemptions.outflow),
         income.inflow = ifelse(income.inflow == -1, 0, income.inflow),
         income.outflow = ifelse(income.outflow == -1, 0, income.outflow),
         num.returns.inflow = ifelse(num.returns.inflow == -1, 0, num.returns.inflow),
         num.returns.outflow = ifelse(num.returns.outflow == -1, 0, num.returns.outflow))|>
  #Recodes missing observations (recorded as NA) as zero
  mutate(num.exemptions.inflow = ifelse(is.na(num.exemptions.inflow) == TRUE, 0, num.exemptions.inflow),
         num.exemptions.outflow = ifelse(is.na(num.exemptions.outflow) == TRUE, 0, num.exemptions.outflow),
         income.inflow = ifelse(is.na(income.inflow) == TRUE, 0, income.inflow),
         income.outflow = ifelse(is.na(income.outflow) == TRUE, 0, income.outflow),
         num.returns.inflow = ifelse(is.na(num.returns.inflow) == TRUE, 0, num.returns.inflow),
         num.returns.outflow = ifelse(is.na(num.returns.outflow) == TRUE, 0, num.returns.outflow))|>
  #Create net migration statistics
  mutate(net.returns = num.returns.inflow - num.returns.outflow,
         net.exemptions = num.exemptions.inflow - num.exemptions.outflow,
         net.income = income.inflow - income.outflow,
         #Determines the number of non-migrating individuals using either the outflow or inflow data.
         #If the number of non-migrants is recorded for outflow statistics but not inflow, it will use the outflow non-migrant statistic
         nonmigrants = ifelse(is.na(nonmigrants.inflow) == TRUE & is.na(nonmigrants.outflow) == FALSE, nonmigrants.outflow,
                              #If the number of non-migrants is recorded for inflow statistics but not outflow, it will use the inflow non-migrant statistic
                              ifelse(is.na(nonmigrants.inflow) == FALSE & is.na(nonmigrants.outflow) == TRUE, nonmigrants.inflow,
                                     #If the number of non-migrants is recorded for neither outflow statistics nor inflow, it will record non-migrants as NA
                                     #Note:  If the number of non-migrants is not recorded for either outflow or inflow data, that means that the data for that county is probably missing.
                                     ifelse(is.na(nonmigrants.inflow) == TRUE & is.na(nonmigrants.outflow) == TRUE, NA, 
                                            #If non-migrants is recorded for both inflow and outflow data, it will default to inflow
                                            ifelse(is.na(nonmigrants.inflow) == FALSE & is.na(nonmigrants.outflow) == FALSE, nonmigrants.inflow, NA)))))|>
  #Filters out all observations without non-migrant statistics
  filter(is.na(nonmigrants) == FALSE)|>
  select(!c(nonmigrants.inflow, nonmigrants.outflow))
################### Import 2004 - 2010 #########################
#The importation for the two following chunks are more or less identical to the previous chunk with a few differences which will be stated.
setwd('D:/Github/IRS-Migration-Data/Pre-2011')
files = list.files()
files = files[!grepl(pattern = 'countymigration', files)]
read.data.2.inflow = function(dir.name){
  #Year value generation is done by taking the first two number of the directory, which corresponds to the last two numbers of a year
  #Those two numbers are pasted with '20' to generate the full year value, i.e. 2004, 2005, etc.
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
#The data files for this chunk are not located in separate directories, so it will not go in and out of separate directories
inflow.files = file.list[grepl(pattern = 'inflow', file.list)]
outflow.files = file.list[grepl(pattern = 'outflow', file.list)]
#Import Inflow Data
import.inflow = function(file.name){
  #The year value is generated in a manner similar to the previous chunk
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
    #The values are recorded in the raw data files differently than the previous chunks
    #Additionally, the data files do not have redundant rows which need to be dropped
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
#Arranges observations by FIPS code and year
full.data = full.data|>
  arrange(FIPS, year)
################# Save Data Frame ##################
#Set the working directory to where you want the final data set to go, if you don't want it to go back into the Github folder.
setwd('D:/Dropbox/Research Data/Cleaned Data/IRS Migration')
#Feel free to rename it as well.
write_csv(full.data, file = 'IRS_Migration_Data_1995-2021.csv')
