
#LONGFIN SMELT DATA# 

#### FRONT MATTER ####
# AUTHOR: Vanessa Tobias
# AGENCY: CDFW

# Data provenance ####
# Bay Study data were provided by Kathy Hieb (CDFW) as a an Excel workbook file on November 22, 2016.
#  This version of the Bay Study dataset contains catch totals and CPUE summarized by tow.
#  It covers years 1980:2015.  The original Excel file contains a metadata tab with additional information.
#  There are several additional files with additional metadata (3 Word and 1 Powerpoint).  These datafiles 
#  should accompany this R code file in addition to the Excel file.

# Load Packages ####
library(readxl)  #read excel file
library(lubridate)  #work with dates
library(mgcv)  #GAMs
library(sm)
library(parallel)

#### READ IN DATA ####
mwt <- read_excel("I:/Data/MAST/Longfin MAST 2017/Original Data/1980-2015_LongfinSmelt_CatchCPUELFData_KH.xlsx", sheet=4)
ot <- read_excel("I:/Data/MAST/Longfin MAST 2017/Original Data/1980-2015_LongfinSmelt_CatchCPUELFData_KH.xlsx", sheet=3)

#### CLEAN UP DATA ####

# Make dataset names identical for merging ####
identical(names(mwt), names(ot))  #check if names are identical
names(mwt)[13:14] <- c("Sal", "Temp")
names(ot)[13:14] <- c("Sal", "Temp")
mwt$TowArea <- NA
ot$TowVolume <- NA
mwt$gear <- "mwt"
ot$gear <- "ot"
#We don't need them to be merged yet, but it will be nice later.


# Clean up names to play nicely with R ####
# catch0, catch1, catch2 = Age0 Catch, Age1 Catch, & Age2+ Catch
# cpue names are similar to catch names
# note that the position of tow area and towvolume are reversed between the mwt and ot datasets

names(mwt) <- c("year", "survey", "date", "station", "series",
                "bay", "chanshoal", "depth", "secchi", "time",
                "tide", "direction", "sal", "temp", "net",
                "tow", "towvolume", "alphacode", "catch0", "catch1", 
                "catch2", "catchtotal", "cpue0", "cpue1", "cpue2",
                "cpuetotal", "towarea", "gear")
names(ot) <- c("year", "survey", "date", "station", "series",
               "bay", "chanshoal", "depth", "secchi", "time",
               "tide", "direction", "sal", "temp", "net",
               "tow", "towarea", "alphacode", "catch0", "catch1", 
               "catch2", "catchtotal", "cpue0", "cpue1", "cpue2",
               "cpuetotal", "towvolume", "gear")

#### CREATE NEW VARIABLES ####
# Time Variables ####
mwt$doy <- yday(mwt$date)
ot$doy <- yday(ot$date)

mwt$month <- month(mwt$date)
ot$month <- month(ot$date)

# Location Variables ####
mwt$bay <- as.factor(mwt$bay)
ot$bay <- as.factor(ot$bay)

mwt$station <- as.factor(mwt$station)
ot$station <- as.factor(ot$station)

# Presence/Absence Variables ####
mwt$pa0 <- as.numeric(mwt$catch0 > 0)  #logical returns T/F; as.numeric(T)=1
mwt$pa1 <- as.numeric(mwt$catch1 > 0)
mwt$pa2 <- as.numeric(mwt$catch2 > 0)
ot$pa0 <- as.numeric(ot$catch0 > 0)  #logical returns T/F; as.numeric(T)=1
ot$pa1 <- as.numeric(ot$catch1 > 0)
ot$pa2 <- as.numeric(ot$catch2 > 0)

#### MERGE SEPARATE GEAR DATAFRAMES ####
both <- rbind(mwt, ot)